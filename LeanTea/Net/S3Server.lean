import LeanTea.Net.Http
import LeanTea.Cloud.S3
import LeanTea.Crypto.Hmac
import LeanTea.Crypto.Password
import Lean.Data.Json

/-! # LeanTea.Net.S3Server — a small S3-compatible object store

Enough of the S3 REST API for tests, local development and small
deployments, served by `LeanTea.Net.Server` and verified with the same
SigV4 code the client uses (`LeanTea.Cloud.S3`):

| request | S3 operation |
|---|---|
| `GET /` | ListBuckets |
| `PUT /b` · `HEAD /b` · `DELETE /b` | CreateBucket · HeadBucket · DeleteBucket |
| `GET /b?list-type=2&prefix=p` (or v1) | ListObjects(V2) |
| `PUT /b/k` · `GET /b/k` · `HEAD /b/k` · `DELETE /b/k` | Put/Get/Head/DeleteObject |
| `GET /health` (alias `/minio/health/live`) | liveness, unauthenticated |

Path-style addressing only. Every other request needs a valid
`AWS4-HMAC-SHA256` `Authorization` header for the configured key pair;
`x-amz-content-sha256` is checked against the body unless it is
`UNSIGNED-PAYLOAD`. Objects live on disk as
`<dataDir>/<bucket>/<hex(key)>.obj` (+ `.meta` JSON), so no key — `..`,
`/`, NUL — can ever name a path outside the bucket directory.

Not implemented: multipart upload, presigned URLs, versioning, ACLs,
virtual-hosted style, ranges, clock-skew rejection. Bodies are capped
by the server's 8 MB request limit. `HEAD` on an object answers with
`content-length: 0` (the body is never sent) and the real size in
`x-amz-meta-size`. -/

namespace LeanTea.Net.S3Server

open LeanTea.Net.Http
open LeanTea.Cloud
open LeanTea.Crypto
open Lean (Json)

structure Config where
  dataDir   : System.FilePath
  accessKey : String
  secretKey : String
  deriving Inhabited

/-! ## Encoding helpers -/

def hexEncode (bs : ByteArray) : String :=
  bs.foldl (fun acc b =>
    let d (n : Nat) := "0123456789abcdef".toList[n]!
    (acc.push (d (b.toNat / 16))).push (d (b.toNat % 16))) ""

def hexDecode (s : String) : Option ByteArray := do
  let nib (c : Char) : Option Nat :=
    if c.isDigit then some (c.toNat - '0'.toNat)
    else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10) else none
  let rec go : List Char → ByteArray → Option ByteArray
    | [], acc => some acc
    | a :: b :: rest, acc => do go rest (acc.push ((← nib a) * 16 + (← nib b)).toUInt8)
    | _, _ => none
  go s.toList .empty

/-- Percent-decode a URL path (unlike form decoding, `+` stays `+`). -/
def pathDecode (s : String) : String := Id.run do
  let cs := s.toList.toArray
  let hex (c : Char) : Option Nat :=
    if c.isDigit then some (c.toNat - '0'.toNat)
    else if 'a' ≤ c.toLower && c.toLower ≤ 'f' then some (c.toLower.toNat - 'a'.toNat + 10) else none
  let mut out : ByteArray := .empty
  let mut i := 0
  while i < cs.size do
    if cs[i]! == '%' && i + 2 < cs.size then
      if let (some h, some l) := (hex cs[i+1]!, hex cs[i+2]!) then
        out := out.push (h * 16 + l).toUInt8
        i := i + 3
        continue
    out := out ++ cs[i]!.toString.toUTF8
    i := i + 1
  return (String.fromUTF8? out).getD s

def xmlEscape (s : String) : String :=
  s.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;" |>.replace "\"" "&quot;"

/-- S3 bucket naming rules (lowercase, digits, `.`/`-`, 3–63 chars). -/
def validBucket (b : String) : Bool :=
  3 ≤ b.length && b.length ≤ 63 &&
  b.all (fun c => c.isLower || c.isDigit || c == '-' || c == '.') &&
  (b.get 0).isAlphanum && (b.back).isAlphanum

/-- Days since 1970-01-01 → (year, month, day), proleptic Gregorian. -/
def civilFromDays (z : Int) : Int × Nat × Nat :=
  let z := z + 719468
  let era := (if z ≥ 0 then z else z - 146096) / 146097
  let doe := z - era * 146097
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let y := yoe + era * 400
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let d := doy - (153 * mp + 2) / 5 + 1
  let m := if mp < 10 then mp + 3 else mp - 9
  (if m ≤ 2 then y + 1 else y, m.toNat, d.toNat)

def iso8601 (secs : Int) : String :=
  let (y, mo, d) := civilFromDays (secs / 86400)
  let s := (secs % 86400).toNat
  let p2 (n : Nat) := if n < 10 then s!"0{n}" else toString n
  s!"{y}-{p2 mo}-{p2 d}T{p2 (s / 3600)}:{p2 (s / 60 % 60)}:{p2 (s % 60)}.000Z"

/-! ## Responses -/

def xmlResp (status : Nat) (body : String) (extra : Array (String × String) := #[]) : Response :=
  { status, headers := #[("content-type", "application/xml")] ++ extra,
    body := ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++ body).toUTF8 }

def s3Error (status : Nat) (code msg : String) (resource : String := "") : Response :=
  xmlResp status s!"<Error><Code>{code}</Code><Message>{xmlEscape msg}</Message><Resource>{xmlEscape resource}</Resource></Error>"

/-! ## SigV4 verification -/

/-- `k=v` pairs sorted by key, as SigV4 wants (values kept as sent). -/
def canonicalQuery (q : String) : String :=
  if q.isEmpty then "" else
  let ps := (q.splitOn "&").filter (!·.isEmpty) |>.map (fun p =>
    match p.splitOn "=" with
    | [k] => (k, "")
    | k :: vs => (k, "=".intercalate vs)
    | [] => ("", ""))
  "&".intercalate ((ps.mergeSort (fun a b => a.1 ≤ b.1)).map (fun (k, v) => s!"{k}={v}"))

private def field (auth key : String) : Option String :=
  match auth.splitOn (key ++ "=") with
  | _ :: rest :: _ => some ((rest.splitOn ",").head!.trimAscii.toString)
  | _ => none

/-- `none` when the request is authentic, else the S3 error to send. -/
def verify (cfg : Config) (req : Request) : Option Response := Id.run do
  let some auth := req.header? "authorization"
    | return some (s3Error 403 "AccessDenied" "missing Authorization header")
  unless auth.startsWith "AWS4-HMAC-SHA256 " do
    return some (s3Error 400 "AuthorizationHeaderMalformed" "only AWS4-HMAC-SHA256 is supported")
  let (some cred, some signed, some sig) := (field auth "Credential", field auth "SignedHeaders", field auth "Signature")
    | return some (s3Error 400 "AuthorizationHeaderMalformed" "Credential/SignedHeaders/Signature required")
  let some amzDate := req.header? "x-amz-date"
    | return some (s3Error 403 "AccessDenied" "missing x-amz-date")
  let some payloadHash := req.header? "x-amz-content-sha256"
    | return some (s3Error 400 "InvalidRequest" "missing x-amz-content-sha256")
  match cred.splitOn "/" with
  | [ak, date, region, service, "aws4_request"] =>
    unless Password.constantTimeEq ak.toUTF8 cfg.accessKey.toUTF8 do
      return some (s3Error 403 "InvalidAccessKeyId" "unknown access key")
    if payloadHash != "UNSIGNED-PAYLOAD" && payloadHash != S3.hexSha256 req.body then
      return some (s3Error 400 "XAmzContentSHA256Mismatch" "body does not match x-amz-content-sha256")
    let names := signed.splitOn ";"
    unless names.contains "host" do
      return some (s3Error 403 "AccessDenied" "host must be signed")
    let headers := names.map (fun n => (n, (req.header? n).getD ""))
    let canon := S3.canonicalRequest req.method req.path (canonicalQuery req.query) headers payloadHash
    let scope := s!"{date}/{region}/{service}/aws4_request"
    let key := S3.signingKey cfg.secretKey date region service
    let want := Hmac.sha256Hex key (S3.stringToSign amzDate scope canon).toUTF8
    if Password.constantTimeEq want.toUTF8 sig.toUTF8 then return none
    return some (s3Error 403 "SignatureDoesNotMatch" "request signature does not match")
  | _ => return some (s3Error 400 "AuthorizationHeaderMalformed" "bad Credential scope")

/-! ## Storage -/

def bucketDir (cfg : Config) (b : String) : System.FilePath := cfg.dataDir / b
def objPath (cfg : Config) (b k : String) : System.FilePath := bucketDir cfg b / s!"{hexEncode k.toUTF8}.obj"
def metaPath (cfg : Config) (b k : String) : System.FilePath := bucketDir cfg b / s!"{hexEncode k.toUTF8}.meta"

structure ObjInfo where
  key   : String
  size  : Nat
  mtime : Int
  etag  : String

def etagOf (body : ByteArray) : String := "\"" ++ (S3.hexSha256 body).take 32 ++ "\""

def readMeta (cfg : Config) (b k : String) : IO Json := do
  try IO.ofExcept (Json.parse (← IO.FS.readFile (metaPath cfg b k))) catch _ => pure (Json.mkObj [])

def listObjs (cfg : Config) (b pfx : String) : IO (Array ObjInfo) := do
  let mut out := #[]
  for e in ← (bucketDir cfg b).readDir do
    let some hex := e.fileName.dropSuffix? ".obj" |>.map (·.toString) | continue
    let some bytes := hexDecode hex | continue
    let some key := String.fromUTF8? bytes | continue
    unless key.startsWith pfx do continue
    let md ← e.path.metadata
    let j ← readMeta cfg b key
    out := out.push { key, size := md.byteSize.toNat, mtime := md.modified.sec,
                      etag := (j.getObjValD "etag").getStr?.toOption.getD "\"\"" }
  return out.qsort (·.key < ·.key)

/-! ## Handler -/

def health : Response := { status := 200, headers := #[("content-type", "text/plain")], body := "ok".toUTF8 }

def handler (cfg : Config) : Handler := fun req => do
  if req.path == "/health" || req.path == "/minio/health/live" then return health
  if let some err := verify cfg req then return err
  let segs := (req.path.drop 1).toString.splitOn "/"
  let bucket := segs.head!
  let key := pathDecode ("/".intercalate segs.tail)
  let bdir := bucketDir cfg bucket
  -- ListBuckets
  if bucket.isEmpty then
    unless req.method == "GET" do return s3Error 405 "MethodNotAllowed" req.method
    IO.FS.createDirAll cfg.dataDir
    let mut xs := ""
    for e in ← cfg.dataDir.readDir do
      if ← e.path.isDir then
        xs := xs ++ s!"<Bucket><Name>{xmlEscape e.fileName}</Name><CreationDate>{iso8601 (← e.path.metadata).modified.sec}</CreationDate></Bucket>"
    return xmlResp 200 s!"<ListAllMyBucketsResult><Owner><ID>{cfg.accessKey}</ID></Owner><Buckets>{xs}</Buckets></ListAllMyBucketsResult>"
  unless validBucket bucket do return s3Error 400 "InvalidBucketName" bucket bucket
  let exists_ ← bdir.isDir
  -- Bucket-level
  if key.isEmpty && segs.length ≤ 2 then
    match req.method with
    | "PUT" =>
      IO.FS.createDirAll bdir
      return { status := 200, headers := #[("location", s!"/{bucket}")] }
    | "HEAD" => return { status := if exists_ then 200 else 404 }
    | "DELETE" =>
      unless exists_ do return s3Error 404 "NoSuchBucket" bucket bucket
      unless (← bdir.readDir).isEmpty do return s3Error 409 "BucketNotEmpty" bucket bucket
      IO.FS.removeDirAll bdir
      return { status := 204 }
    | "GET" =>
      unless exists_ do return s3Error 404 "NoSuchBucket" bucket bucket
      let q (k : String) : String :=
        ((req.query.splitOn "&").findSome? (fun p =>
          if p.startsWith (k ++ "=") then some (pathDecode (p.drop (k.length + 1)).toString) else none)).getD ""
      let pfx := q "prefix"
      let objs ← listObjs cfg bucket pfx
      let contents := objs.foldl (fun acc o =>
        acc ++ s!"<Contents><Key>{xmlEscape o.key}</Key><LastModified>{iso8601 o.mtime}</LastModified><ETag>{xmlEscape o.etag}</ETag><Size>{o.size}</Size><StorageClass>STANDARD</StorageClass></Contents>") ""
      return xmlResp 200 (s!"<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Name>{bucket}</Name><Prefix>{xmlEscape pfx}</Prefix>" ++
        s!"<KeyCount>{objs.size}</KeyCount><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated>{contents}</ListBucketResult>")
    | m => return s3Error 405 "MethodNotAllowed" m
  -- Object-level
  unless exists_ do return s3Error 404 "NoSuchBucket" bucket bucket
  let path := objPath cfg bucket key
  match req.method with
  | "PUT" =>
    let etag := etagOf req.body
    IO.FS.writeBinFile path req.body
    IO.FS.writeFile (metaPath cfg bucket key) (Json.mkObj [
      ("contentType", .str ((req.header? "content-type").getD "application/octet-stream")),
      ("etag", .str etag)]).compress
    return { status := 200, headers := #[("etag", etag)] }
  | "GET" | "HEAD" =>
    unless ← path.pathExists do
      return if req.method == "HEAD" then { status := 404 } else s3Error 404 "NoSuchKey" key s!"/{bucket}/{key}"
    let j ← readMeta cfg bucket key
    let md ← path.metadata
    let hdrs := #[("content-type", (j.getObjValD "contentType").getStr?.toOption.getD "application/octet-stream"),
                  ("etag", (j.getObjValD "etag").getStr?.toOption.getD "\"\""),
                  ("last-modified-iso", iso8601 md.modified.sec),
                  ("x-amz-meta-size", toString md.byteSize)]
    if req.method == "HEAD" then return { status := 200, headers := hdrs }
    return { status := 200, headers := hdrs, body := ← IO.FS.readBinFile path }
  | "DELETE" =>
    if ← path.pathExists then
      IO.FS.removeFile path
      try IO.FS.removeFile (metaPath cfg bucket key) catch _ => pure ()
    return { status := 204 }
  | m => return s3Error 405 "MethodNotAllowed" m

end LeanTea.Net.S3Server
