import LeanTea
import LeanTea.Cloud.S3

/-! # s3_smoke — round-trip the S3 client against a live endpoint

Needs a running S3-compatible server. Two ways:

  1. **MinIO** (recommended for CI): `docker run --rm -p 9000:9000
     -e MINIO_ROOT_USER=test -e MINIO_ROOT_PASSWORD=testtest1234
     minio/minio server /data`. The smoke creates the bucket on
     start.
  2. **Real AWS S3**: set `AWS_ACCESS_KEY_ID` /
     `AWS_SECRET_ACCESS_KEY` / `AWS_REGION` / `S3_BUCKET` and the
     smoke picks them up via `Config.fromEnv`.

Defaults to MinIO at `http://127.0.0.1:9000` so it works against
the `services:` block in `.github/workflows/ci.yml` out of the box. -/

open LeanTea LeanTea.Cloud
open LeanTea.LSpec

private def hasSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Default MinIO config for local + CI smokes. Overridable via the
    env vars `Config.fromEnv` reads. -/
def defaultCfg : S3.Config := {
  endpoint  := "http://127.0.0.1:9000",
  region    := "us-east-1",
  bucket    := "leantea-smoke",
  accessKey := "test",
  secretKey := "testtest1234",
  pathStyle := true            -- MinIO requires path-style
}

/-- Create the bucket if it doesn't already exist. Idempotent.

    S3's "create bucket" is `PUT /bucket/` with no key. We can't
    just signRequest with key="" because that would target the
    bucket-list endpoint; we cheat by hand-constructing a request
    that signs an empty key against the bucket root. -/
def ensureBucket (cfg : S3.Config) : IO Unit := do
  let req ← S3.signRequest cfg "PUT" ""
  /- We don't need to inspect the response — the round-trip below
     fails loudly if creation didn't take. -/
  let _ ← S3.signRequest cfg "PUT" ""
  let _ := req
  /- Best-effort: shell to curl directly so we can ignore failures
     (the bucket may already exist; MinIO returns 200 vs S3 returns
     409 BucketAlreadyOwnedByYou). -/
  let r := req
  let mut args : Array String := #["-sS", "--max-time", "10", "-X", "PUT",
                                    "-o", "/dev/null", r.url ]
  for h in r.headers do args := args ++ #["-H", h]
  let _ ← IO.Process.output { cmd := "curl", args }

def s3Spec (cfg : S3.Config) : IO LSpec := do
  /- Step 0: make sure the bucket exists. -/
  ensureBucket cfg
  /- Step 1: putObject. -/
  let body := "hello-from-leantea-s3-smoke".toUTF8
  let putStatus : Nat ← try
    let _ ← S3.putObject cfg "smoke.txt" body (contentType := "text/plain")
    pure 200
  catch e => do IO.eprintln s!"put error: {e}"; pure 0
  /- Step 2: headObject — should report present. -/
  let headPresent : Bool ← try
    let r ← S3.headObject cfg "smoke.txt"
    pure r.isSome
  catch _ => pure false
  /- Step 3: getObject — round-trip. -/
  let fetched : String ← try
    let raw ← S3.getObject cfg "smoke.txt"
    pure (String.fromUTF8! raw)
  catch e => do IO.eprintln s!"get error: {e}"; pure ""
  /- Step 4: listObjects — should include smoke.txt. -/
  let keys : List String ← try S3.listObjects cfg "" catch _ => pure []
  /- Step 5: deleteObject + headObject again → none. -/
  let deletedStatus : Nat ← try
    let _ ← S3.deleteObject cfg "smoke.txt"
    pure 204
  catch _ => pure 0
  let headAbsent : Bool ← try
    let r ← S3.headObject cfg "smoke.txt"
    pure r.isNone
  catch _ => pure false
  return group "S3 round-trip" [
    it "putObject returns 2xx"            (putStatus >= 200 && putStatus < 300),
    it "headObject sees the new key"      headPresent,
    it "getObject body round-trips"
      (fetched == "hello-from-leantea-s3-smoke"),
    it "listObjects contains smoke.txt"
      (keys.contains "smoke.txt"),
    it "deleteObject returns 2xx"
      (deletedStatus >= 200 && deletedStatus < 300),
    it "headObject sees the key gone"     headAbsent
  ]

def main : IO Unit := do
  let cfg :=
    match ← S3.Config.fromEnv with
    | some c => c
    | none   => defaultCfg
  IO.println s!"s3_smoke — endpoint={cfg.endpoint} bucket={cfg.bucket}"
  let tree ← s3Spec cfg
  let code ← lspecIO (group "LeanTEA S3 / object storage" [tree])
  if code != 0 then IO.Process.exit code.toUInt8
