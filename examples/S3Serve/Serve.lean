import LeanTea
import LeanTea.Net.S3Server

/-! # s3_serve — S3-compatible object storage in Lean

Serves `LeanTea.Net.S3Server` (path-style, SigV4-authenticated) over a
directory. Drop-in for the subset of MinIO the repo's tests use:

```sh
./.lake/build/bin/s3_serve --port 9000 --data /tmp/s3 \
    --access-key test --secret-key testtest1234
curl http://127.0.0.1:9000/health        # → ok
```

Env overrides: `S3_SERVE_PORT`, `S3_SERVE_HOST`, `S3_SERVE_DATA`,
`S3_SERVE_ACCESS_KEY`, `S3_SERVE_SECRET_KEY`. Binds 127.0.0.1 by
default — put TLS in front before exposing it. -/

open LeanTea LeanTea.Net

private structure Args where
  port      : UInt16 := 9000
  host      : String := "127.0.0.1"
  data      : String := ".leantea-state/s3"
  accessKey : String := ""
  secretKey : String := ""

private partial def parseArgs : List String → Args → Args
  | "--port" :: v :: r, a       => parseArgs r { a with port := (v.toNat?.getD 9000).toUInt16 }
  | "--host" :: v :: r, a       => parseArgs r { a with host := v }
  | "--data" :: v :: r, a       => parseArgs r { a with data := v }
  | "--access-key" :: v :: r, a => parseArgs r { a with accessKey := v }
  | "--secret-key" :: v :: r, a => parseArgs r { a with secretKey := v }
  | _ :: r, a => parseArgs r a
  | [], a => a

def main (argv : List String) : IO UInt32 := do
  let mut a := parseArgs argv {}
  if let some v ← IO.getEnv "S3_SERVE_PORT" then a := { a with port := (v.toNat?.getD 9000).toUInt16 }
  if let some v ← IO.getEnv "S3_SERVE_HOST" then a := { a with host := v }
  if let some v ← IO.getEnv "S3_SERVE_DATA" then a := { a with data := v }
  if let some v ← IO.getEnv "S3_SERVE_ACCESS_KEY" then a := { a with accessKey := v }
  if let some v ← IO.getEnv "S3_SERVE_SECRET_KEY" then a := { a with secretKey := v }
  if a.accessKey.isEmpty || a.secretKey.isEmpty then
    IO.eprintln "s3_serve: --access-key and --secret-key are required"
    return 2
  IO.FS.createDirAll a.data
  let cfg : S3Server.Config := { dataDir := ← IO.FS.realPath a.data, accessKey := a.accessKey, secretKey := a.secretKey }
  IO.eprintln s!"s3_serve: http://{a.host}:{a.port}/ data={cfg.dataDir}"
  LeanTea.Net.Server.serve a.port a.host (S3Server.handler cfg)
  return 0
