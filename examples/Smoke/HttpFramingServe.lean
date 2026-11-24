import LeanTea

/-! # http_framing_serve — one HTTP backend, for `http_framing_spec`

```
http_framing_serve --backend server|concurrent|fast|reactor --port 18770 --timeout 1000
```

Every request is answered `200` with `path=<path> body=<n>` (`n` = body
bytes), so a test can tell which requests the server saw and how it
framed them. `--timeout` is the backend's `requestTimeoutMs`. -/

open LeanTea.Net.Http

def handler (req : Request) : IO Response :=
  pure (Response.text 200 s!"path={req.path} body={req.body.size}\n")

partial def parse (args : List String) (backend : String) (port timeout : Nat)
    : String × Nat × Nat :=
  match args with
  | "--backend" :: b :: r => parse r b port timeout
  | "--port" :: p :: r => parse r backend (p.toNat?.getD port) timeout
  | "--timeout" :: t :: r => parse r backend port (t.toNat?.getD timeout)
  | _ :: r => parse r backend port timeout
  | [] => (backend, port, timeout)

def main (args : List String) : IO UInt32 := do
  let (backend, port, timeout) := parse args "server" 18770 30000
  let p := port.toUInt16
  match backend with
  | "server" => LeanTea.Net.Server.serve p "127.0.0.1" handler (requestTimeoutMs := timeout)
  | "concurrent" => LeanTea.Net.Server.serveConcurrent p "127.0.0.1" handler (requestTimeoutMs := timeout)
  | "fast" => LeanTea.Net.FastServer.serve p 1 handler (requestTimeoutMs := timeout)
  | "reactor" => LeanTea.Net.ReactorServer.serve p 1 handler (requestTimeoutMs := timeout.toUInt32)
  | other => IO.eprintln s!"unknown backend {other}"; return 2
  return 0
