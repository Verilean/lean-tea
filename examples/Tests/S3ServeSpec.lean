import LeanTea
import LeanTea.Cloud.S3

/-! # s3_serve_spec — `s3_serve` against the repo's own S3 client

Spawns `s3_serve` on a temp dir and drives it with `LeanTea.Cloud.S3`
(SigV4 over curl): object round-trip, prefix listing, awkward keys,
and the refusals that make it safe — bad signature, unsigned request,
tampered body, wrong access key, `..` keys staying inside the bucket,
non-empty bucket delete. No Docker, no network beyond loopback. -/

open LeanTea LeanTea.LSpec LeanTea.Cloud

def port : Nat := 9123

def cfg : S3.Config := {
  endpoint := s!"http://127.0.0.1:{port}", region := "us-east-1", bucket := "spec-bucket",
  accessKey := "spec", secretKey := "spec-secret-123", pathStyle := true }

def hasSubstr (h n : String) : Bool := (h.splitOn n).length > 1

/-- Send a signed request with an optional replacement body; returns (status, body). -/
def sendRaw (r : S3.SignedRequest) (bodyOverride? : Option ByteArray := none) : IO (Nat × String) := do
  let body := bodyOverride?.getD r.payload
  let tmp := s!"/tmp/s3spec-{← IO.rand 0 0xffffff}.bin"
  let mut args := #["-sS", "--path-as-is", "-X", r.method, "-w", "\n___%{http_code}", r.url]
  for h in r.headers do args := args ++ #["-H", h]
  if !body.isEmpty then
    IO.FS.writeBinFile tmp body
    args := args ++ #["--data-binary", s!"@{tmp}"]
  let out ← IO.Process.output { cmd := "curl", args }
  if !body.isEmpty then try IO.FS.removeFile tmp catch _ => pure ()
  match out.stdout.splitOn "\n___" with
  | [b, c] => return (c.trimAscii.toString.toNat?.getD 0, b)
  | _ => return (0, out.stdout)

/-- `GET /` (ListBuckets) — the client only signs bucket paths, so
    sign this one by hand with the exported SigV4 pieces. -/
def signRoot : IO S3.SignedRequest := do
  let out ← IO.Process.output { cmd := "date", args := #["-u", "+%Y%m%dT%H%M%SZ %Y%m%d"] }
  let (iso, date) := match out.stdout.trimAscii.toString.splitOn " " with
    | [i, d] => (i, d) | _ => ("", "")
  let host := s!"127.0.0.1:{port}"
  let ph := S3.hexSha256 .empty
  let hs := [("host", host), ("x-amz-content-sha256", ph), ("x-amz-date", iso)]
  let scope := s!"{date}/{cfg.region}/s3/aws4_request"
  let canon := S3.canonicalRequest "GET" "/" "" hs ph
  let sig := LeanTea.Crypto.Hmac.sha256Hex (S3.signingKey cfg.secretKey date cfg.region "s3")
    (S3.stringToSign iso scope canon).toUTF8
  let auth := s!"AWS4-HMAC-SHA256 Credential={cfg.accessKey}/{scope}, SignedHeaders=host;x-amz-content-sha256;x-amz-date, Signature={sig}"
  return { method := "GET", url := s!"http://{host}/", payload := .empty,
           headers := (hs.map (fun (k, v) => s!"{k}: {v}")).toArray.push s!"Authorization: {auth}" }

def errOf (act : IO α) : IO String := do
  try let _ ← act; return "" catch e => return toString e

def spec (dataDir : System.FilePath) : IO LSpec := do
  let mk ← S3.signRequest cfg "PUT" ""
  let (mkStatus, _) ← sendRaw mk
  let _ ← S3.putObject cfg "docs/a.txt" "alpha".toUTF8 (contentType := "text/plain")
  let _ ← S3.putObject cfg "docs/b.txt" "beta".toUTF8
  let _ ← S3.putObject cfg "img/c.png" (ByteArray.mk #[0, 255, 1, 254])
  let weird := "../../escape me/ünï cødé+x.txt"
  let _ ← S3.putObject cfg weird "weird".toUTF8
  let a ← S3.getObject cfg "docs/a.txt"
  let bin ← S3.getObject cfg "img/c.png"
  let w ← S3.getObject cfg weird
  let all ← S3.listObjects cfg ""
  let docs ← S3.listObjects cfg "docs/"
  let headA ← S3.headObject cfg "docs/a.txt"
  let headMissing ← S3.headObject cfg "nope.txt"
  let missingErr ← errOf (S3.getObject cfg "nope.txt")
  -- refusals
  let badSecretErr ← errOf (S3.getObject { cfg with secretKey := "wrong" } "docs/a.txt")
  let badKeyErr ← errOf (S3.getObject { cfg with accessKey := "intruder" } "docs/a.txt")
  let unsigned ← IO.Process.output { cmd := "curl", args := #["-sS", "-o", "/dev/null", "-w", "%{http_code}",
    s!"http://127.0.0.1:{port}/spec-bucket/docs/a.txt"] }
  let signedA ← S3.signRequest cfg "PUT" "docs/a.txt" (payload := "alpha".toUTF8)
  let (tamperStatus, tamperBody) ← sendRaw signedA (some "EVIL!".toUTF8)
  let afterTamper ← S3.getObject cfg "docs/a.txt"
  let notEmptyDel ← S3.signRequest cfg "DELETE" ""
  let (delBucketStatus, delBucketBody) ← sendRaw notEmptyDel
  let (_, bucketsXml) ← sendRaw (← signRoot)
  -- every stored file sits directly inside the bucket dir
  let mut strays := 0
  for e in ← dataDir.readDir do
    if e.fileName != "spec-bucket" then strays := strays + 1
  let _ ← S3.deleteObject cfg "docs/b.txt"
  let afterDel ← S3.listObjects cfg "docs/"
  let health ← IO.Process.output { cmd := "curl", args := #["-sS", s!"http://127.0.0.1:{port}/health"] }
  return group "s3_serve" [
    group "round-trip" [
      it "create bucket → 200" (mkStatus == 200),
      it "text object" (String.fromUTF8! a == "alpha"),
      it "binary object byte-exact" (bin == ByteArray.mk #[0, 255, 1, 254]),
      it "awkward key (.., spaces, unicode, +)" (String.fromUTF8! w == "weird"),
      it "list all keys" (all.length == 4 && all.contains weird),
      it "list with prefix" (docs == ["docs/a.txt", "docs/b.txt"]),
      it "head existing / missing" (headA.isSome && headMissing.isNone),
      it "get missing → NoSuchKey" (hasSubstr missingErr "NoSuchKey"),
      it "delete then list" (afterDel == ["docs/a.txt"]),
      it "ListBuckets names the bucket" (hasSubstr bucketsXml "<Name>spec-bucket</Name>"),
      it "health is unauthenticated" (health.stdout == "ok")
    ],
    group "refusals" [
      it "wrong secret → SignatureDoesNotMatch" (hasSubstr badSecretErr "SignatureDoesNotMatch"),
      it "unknown access key → InvalidAccessKeyId" (hasSubstr badKeyErr "InvalidAccessKeyId"),
      it "unsigned request → 403" (unsigned.stdout == "403"),
      it s!"tampered body → 400 ({tamperStatus})" (tamperStatus == 400 && hasSubstr tamperBody "XAmzContentSHA256Mismatch"),
      it "tampered body not stored" (String.fromUTF8! afterTamper == "alpha"),
      it "delete non-empty bucket → 409" (delBucketStatus == 409 && hasSubstr delBucketBody "BucketNotEmpty"),
      it "no file escapes the bucket dir" (strays == 0)
    ]
  ]

def main : IO UInt32 := do
  let dir : System.FilePath := s!"/tmp/s3-serve-spec-{← IO.rand 0 0xffffff}"
  IO.FS.createDirAll dir
  let child ← IO.Process.spawn {
    cmd := "./.lake/build/bin/s3_serve",
    args := #["--port", toString port, "--data", dir.toString,
              "--access-key", cfg.accessKey, "--secret-key", cfg.secretKey],
    stdin := .null, stdout := .null, stderr := .null }
  let mut up := false
  for _ in [:50] do
    let r ← IO.Process.output { cmd := "curl", args := #["-s", s!"http://127.0.0.1:{port}/health"] }
    if r.stdout == "ok" then up := true; break
    IO.sleep 100
  let code ← try
      if !up then throw <| IO.userError "s3_serve did not come up"
      lspecIO (← spec dir)
    finally
      child.kill
      IO.FS.removeDirAll dir
  return if code == 0 then 0 else 1
