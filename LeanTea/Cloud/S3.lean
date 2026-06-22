import LeanTea.Auth
import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Hmac
import Lean.Data.Json

/-! # LeanTea.Cloud.S3 — AWS Signature V4 client for S3-compatible storage

One small client that talks to:

  * **AWS S3**           — `s3.<region>.amazonaws.com`
  * **MinIO**            — self-hosted S3-compatible
  * **Cloudflare R2**    — `<account>.r2.cloudflarestorage.com`
  * **Backblaze B2**     — `s3.<region>.backblazeb2.com`
  * **Google GCS**       — via the S3-compatible "interoperability" endpoint

HTTPS calls go through `curl(1)` because Lean's `Net.HttpClient`
doesn't speak TLS yet. SigV4 signing is implemented in pure Lean
using `LeanTea.Crypto.Sha256` + `LeanTea.Crypto.Hmac`.

```lean
let cfg : S3.Config := {
  endpoint := "https://s3.ap-northeast-1.amazonaws.com",
  region   := "ap-northeast-1",
  bucket   := "my-bucket",
  accessKey := "AKIA…",
  secretKey := "…",
  pathStyle := false       -- AWS uses virtual-hosted by default
}

let _ ← S3.putObject cfg "hello.txt" "hello, world!".toUTF8
                       (contentType := "text/plain")
let body ← S3.getObject cfg "hello.txt"
let _ ← S3.deleteObject cfg "hello.txt"
let keys ← S3.listObjects cfg ""
```

The SigV4 helpers are exposed (`signRequest`, `canonicalRequest`)
so callers can sign other S3 APIs (multipart upload, S3 Select, …)
without forking the module. -/

namespace LeanTea.Cloud.S3

open LeanTea.Auth (urlEncode)
open LeanTea.Crypto
open Lean (Json)

/-! ## Config -/

structure Config where
  /-- e.g. `https://s3.us-east-1.amazonaws.com` or
      `http://127.0.0.1:9000` for a MinIO CI service. -/
  endpoint  : String
  /-- AWS region (`us-east-1`, `ap-northeast-1`, …). For R2 use
      `auto`; for MinIO any non-empty string works. -/
  region    : String
  /-- The bucket. Encoded into the URL via virtual-hosted style by
      default; flip `pathStyle := true` for MinIO / older S3 / R2-with-IP. -/
  bucket    : String
  accessKey : String
  secretKey : String
  /-- Use `https://<endpoint>/<bucket>/<key>` (path-style) instead
      of `https://<bucket>.<endpoint>/<key>` (virtual-hosted style).
      Path-style is required for MinIO and recommended for any
      bucket whose name has dots. -/
  pathStyle : Bool := false
  /-- `s3` for S3 / MinIO / R2 / B2; `s3` is correct here regardless. -/
  service   : String := "s3"
  deriving Inhabited, Repr

/-- Pull a config from environment variables.

    `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_REGION`,
    `S3_ENDPOINT`, `S3_BUCKET`, `S3_PATH_STYLE` (`1`/`true` →
    pathStyle). Returns `none` when the access/secret/bucket trio
    is incomplete. -/
def Config.fromEnv : IO (Option Config) := do
  let access  ← IO.getEnv "AWS_ACCESS_KEY_ID"
  let secret  ← IO.getEnv "AWS_SECRET_ACCESS_KEY"
  let bucket  ← IO.getEnv "S3_BUCKET"
  let region  := (← IO.getEnv "AWS_REGION").getD "us-east-1"
  let endpoint:= (← IO.getEnv "S3_ENDPOINT").getD
                   s!"https://s3.{region}.amazonaws.com"
  let pathStyle := match ← IO.getEnv "S3_PATH_STYLE" with
    | some s => s.toLower == "1" || s.toLower == "true"
    | none   => false
  match access, secret, bucket with
  | some a, some s, some b =>
    return some { endpoint, region, bucket := b,
                  accessKey := a, secretKey := s, pathStyle }
  | _, _, _ => return none

/-! ## SigV4 helpers — exported so callers can sign extras. -/

private def hexLower (ba : ByteArray) : String :=
  Sha256.toHex ba

/-- Hex-encoded SHA-256 of `s` (UTF-8 bytes). -/
def hexSha256 (bs : ByteArray) : String :=
  Sha256.toHex (Sha256.hash bs)

/-- Build the SigV4 canonical request:

      METHOD\n
      CanonicalURI\n
      CanonicalQueryString\n
      CanonicalHeaders\n
      SignedHeaders\n
      HashedPayload

    See AWS docs § "Task 1: Create a canonical request". -/
def canonicalRequest
    (method : String) (uri : String) (query : String)
    (headers : List (String × String)) (payloadHash : String) : String :=
  let sortedHeaders := headers.mergeSort (fun a b => a.1 ≤ b.1)
  let canonicalHeaders := sortedHeaders.foldl
    (fun acc (k, v) => acc ++ s!"{k.toLower}:{v.trimAscii}\n") ""
  let signedHeaders :=
    String.intercalate ";" (sortedHeaders.map (·.1.toLower))
  s!"{method}\n{uri}\n{query}\n{canonicalHeaders}\n{signedHeaders}\n{payloadHash}"

/-- Build the SigV4 "string to sign":

      AWS4-HMAC-SHA256\n
      <amzDate>\n
      <credentialScope>\n
      <hex(SHA256(canonicalRequest))>

    `amzDate` is the basic-ISO-8601 form `YYYYMMDD'T'HHMMSS'Z'`. -/
def stringToSign (amzDate scope canonicalReq : String) : String :=
  s!"AWS4-HMAC-SHA256\n{amzDate}\n{scope}\n{hexSha256 canonicalReq.toUTF8}"

/-- Derive the SigV4 signing key:
      kSecret  = "AWS4" + secretKey
      kDate    = HMAC(kSecret,  date)            -- date = YYYYMMDD
      kRegion  = HMAC(kDate,    region)
      kService = HMAC(kRegion,  service)
      kSigning = HMAC(kService, "aws4_request")  -/
def signingKey (secret : String) (date region service : String) : ByteArray :=
  let k0 := ("AWS4" ++ secret).toUTF8
  let kDate    := Hmac.sha256 k0    date.toUTF8
  let kRegion  := Hmac.sha256 kDate region.toUTF8
  let kService := Hmac.sha256 kRegion service.toUTF8
  Hmac.sha256 kService "aws4_request".toUTF8

/-! ## Date helpers

We need the basic-ISO-8601 form `YYYYMMDDTHHMMSSZ` and the
`YYYYMMDD` scope-prefix. `IO.monoMsNow` doesn't give us calendar
time, so we shell to `date -u`. Pure-Lean calendar conversion is a
separate project. -/

private def utcNow : IO (String × String) := do
  let out ← IO.Process.output {
    cmd := "date", args := #["-u", "+%Y%m%dT%H%M%SZ %Y%m%d"] }
  if out.exitCode != 0 then
    throw <| IO.userError s!"S3: failed to read date: {out.stderr}"
  let parts := out.stdout.trimAscii.toString.splitOn " "
  match parts with
  | [iso, date] => return (iso, date)
  | _ => throw <| IO.userError s!"S3: bad date output `{out.stdout}`"

/-! ## URL composition -/

/-- Strip a leading `https://` or `http://` and any trailing `/`. -/
private def hostOf (endpoint : String) : String :=
  let s :=
    if endpoint.startsWith "https://" then (endpoint.drop 8).toString
    else if endpoint.startsWith "http://" then (endpoint.drop 7).toString
    else endpoint
  if s.endsWith "/" then (s.dropEnd 1).toString else s

private def schemeOf (endpoint : String) : String :=
  if endpoint.startsWith "https://" then "https://"
  else if endpoint.startsWith "http://" then "http://"
  else "https://"

/-- Build the (host, path) pair for a request. Path always starts
    with `/` and the object key is URL-encoded except for `/`. -/
private def hostAndPath (cfg : Config) (key : String) : String × String :=
  let host := hostOf cfg.endpoint
  if cfg.pathStyle then
    /- Path-style: host stays as endpoint, bucket goes into the
       path. Required for MinIO and ad-hoc test setups. -/
    (host, s!"/{cfg.bucket}/{urlEncodePath key}")
  else
    /- Virtual-hosted: bucket prefixed onto the host. -/
    (s!"{cfg.bucket}.{host}", s!"/{urlEncodePath key}")
where
  /-- URL-encode each path segment but leave `/` alone. -/
  urlEncodePath (s : String) : String :=
    String.intercalate "/" (s.splitOn "/" |>.map urlEncode)

/-! ## Sign one request -/

/-- The signed result. The caller passes `headers` to curl via `-H`
    flags; `payload` is what curl writes as the body (`--data-binary`). -/
structure SignedRequest where
  method  : String
  url     : String
  headers : Array String                  -- in `-H "k: v"` form for curl
  payload : ByteArray
  deriving Inhabited

/-- Sign an S3 request. Returns the curl-ready URL + headers. -/
def signRequest (cfg : Config) (method key : String)
    (query : String := "") (payload : ByteArray := .empty)
    (extraHeaders : List (String × String) := []) : IO SignedRequest := do
  let (iso, date) ← utcNow
  let (host, path) := hostAndPath cfg key
  let scheme := schemeOf cfg.endpoint
  let url := s!"{scheme}{host}{path}{if query.isEmpty then "" else "?" ++ query}"
  let payloadHash := hexSha256 payload
  let mut headers : List (String × String) := [
    ("host",                 host),
    ("x-amz-content-sha256", payloadHash),
    ("x-amz-date",           iso)
  ] ++ extraHeaders
  /- Sort + lowercase happens inside canonicalRequest. -/
  let canon := canonicalRequest method path query headers payloadHash
  let scope := s!"{date}/{cfg.region}/{cfg.service}/aws4_request"
  let toSign := stringToSign iso scope canon
  let sk := signingKey cfg.secretKey date cfg.region cfg.service
  let sig := Hmac.sha256Hex sk toSign.toUTF8
  let signedHdrs :=
    String.intercalate ";"
      ((headers.mergeSort (fun a b => a.1 ≤ b.1)).map (·.1.toLower))
  let auth :=
    s!"AWS4-HMAC-SHA256 Credential={cfg.accessKey}/{scope}, " ++
    s!"SignedHeaders={signedHdrs}, Signature={sig}"
  let curlHeaders : Array String :=
    headers.toArray.map (fun (k, v) => s!"{k}: {v}")
      |>.push s!"Authorization: {auth}"
  return { method, url, headers := curlHeaders, payload }

/-! ## curl wrapper -/

structure HttpResp where
  status : Nat
  body   : ByteArray
  /-- stdout from curl when something goes wrong (network, DNS). -/
  diag   : String := ""

private def curlSigned (r : SignedRequest) : IO HttpResp := do
  /- Write the payload (possibly binary) to a temp file so curl can
     `--data-binary @file`. Inline `-d` would mangle bytes. -/
  let tmpDir := (← IO.getEnv "TMPDIR").getD "/tmp"
  let bodyFile := s!"{tmpDir}/leantea-s3-body-{← IO.rand 0 0xffff_ffff}.bin"
  let outFile  := s!"{tmpDir}/leantea-s3-resp-{← IO.rand 0 0xffff_ffff}.bin"
  if !r.payload.isEmpty then
    IO.FS.writeBinFile bodyFile r.payload
  let mut args : Array String := #[
    "-sS", "--max-time", "30",
    "-X", r.method,
    "-w", "\n___STATUS:%{http_code}",
    "-o", outFile, r.url ]
  for h in r.headers do args := args ++ #["-H", h]
  if !r.payload.isEmpty then
    args := args ++ #["--data-binary", s!"@{bodyFile}"]
  let out ← IO.Process.output { cmd := "curl", args }
  /- Best-effort cleanup. -/
  if !r.payload.isEmpty then
    try IO.FS.removeFile bodyFile catch _ => pure ()
  let stdout := out.stdout
  let parts := stdout.splitOn "\n___STATUS:"
  let status :=
    match parts with
    | [_, codeS] => codeS.trimAscii.toString.toNat?.getD 0
    | _ => 0
  let body ←
    try IO.FS.readBinFile outFile
    catch _ => pure .empty
  try IO.FS.removeFile outFile catch _ => pure ()
  return { status, body, diag := stdout }

/-! ## High-level operations -/

/-- `PUT /bucket/key`. Returns the HTTP status (200 / 201 on success). -/
def putObject (cfg : Config) (key : String) (data : ByteArray)
    (contentType : String := "application/octet-stream") : IO Nat := do
  let extra : List (String × String) := [("content-type", contentType)]
  let req ← signRequest cfg "PUT" key (payload := data) (extraHeaders := extra)
  let resp ← curlSigned req
  if resp.status < 200 || resp.status >= 300 then
    throw <| IO.userError s!"S3 putObject {key}: HTTP {resp.status}\n{String.fromUTF8! resp.body}"
  return resp.status

/-- `GET /bucket/key`. Returns the raw bytes. -/
def getObject (cfg : Config) (key : String) : IO ByteArray := do
  let req ← signRequest cfg "GET" key
  let resp ← curlSigned req
  if resp.status != 200 then
    throw <| IO.userError s!"S3 getObject {key}: HTTP {resp.status}\n{String.fromUTF8! resp.body}"
  return resp.body

/-- `DELETE /bucket/key`. Returns the HTTP status. -/
def deleteObject (cfg : Config) (key : String) : IO Nat := do
  let req ← signRequest cfg "DELETE" key
  let resp ← curlSigned req
  if resp.status < 200 || resp.status >= 300 then
    throw <| IO.userError s!"S3 deleteObject {key}: HTTP {resp.status}\n{String.fromUTF8! resp.body}"
  return resp.status

/-- `HEAD /bucket/key`. Returns `some status` on 2xx, `none` on 404,
    throws on other errors. Useful for "does this object exist?". -/
def headObject (cfg : Config) (key : String) : IO (Option Nat) := do
  let req ← signRequest cfg "HEAD" key
  let resp ← curlSigned req
  if resp.status == 404 then return none
  if resp.status < 200 || resp.status >= 300 then
    throw <| IO.userError s!"S3 headObject {key}: HTTP {resp.status}"
  return some resp.status

/-- `GET /bucket?list-type=2&prefix=<prefix>` — list keys.

    Returns the **raw XML body** so the caller can XPath what they
    need; parsing every list-objects shape (ContinuationToken,
    Owner, ETag, …) is out of scope for v0.3. The companion
    `extractKeys` extracts just `<Key>…</Key>` for the common case. -/
def listObjectsRaw (cfg : Config) (keyPrefix : String := "") : IO String := do
  let query := s!"list-type=2&prefix={urlEncode keyPrefix}"
  let req ← signRequest cfg "GET" "" query
  let resp ← curlSigned req
  if resp.status != 200 then
    throw <| IO.userError s!"S3 listObjects: HTTP {resp.status}\n{String.fromUTF8! resp.body}"
  return String.fromUTF8! resp.body

/-- Extract `<Key>…</Key>` tags from a list-objects XML response. -/
partial def extractKeys (xml : String) : List String := Id.run do
  let mut acc : List String := []
  let mut rest : String := xml
  while true do
    match rest.splitOn "<Key>" with
    | _ :: tail :: _ =>
      match tail.splitOn "</Key>" with
      | key :: ks =>
        acc := key :: acc
        rest := String.intercalate "</Key>" ks
      | _ => break
    | _ => break
  return acc.reverse

/-- Convenience: parsed key list. Empty list on no matches; throws
    on non-2xx. -/
def listObjects (cfg : Config) (keyPrefix : String := "") : IO (List String) := do
  let xml ← listObjectsRaw cfg keyPrefix
  return extractKeys xml

end LeanTea.Cloud.S3
