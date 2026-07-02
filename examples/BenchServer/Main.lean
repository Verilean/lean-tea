import LeanTea

/-! # examples/BenchServer/Main.lean — micro handler for the perf harness

Two routes so the harness can time each:

  * GET  /health    → `"OK"` — no allocation past the constant
                       Response, closest thing to a "framework
                       overhead only" measurement.
  * POST /echo      → the raw request body — exercises body read + write.
  * GET  /json      → a small `Response.json` with 5 fields — shows
                       codec cost.

The server exclusively uses `serveConcurrent`, which fans each
accepted connection out into an `IO.asTask`. Lean's task scheduler
uses `LEAN_NUM_THREADS` OS threads (defaults to core count) so
running the same binary with different values of that env var tells
us how the throughput scales as we hand it more cores.

Run: `./.lake/build/bin/bench_server --port 8080`
-/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

private def jsonPayload : Json := Json.mkObj [
  ("ok",       Json.bool true),
  ("service",  Json.str "lean-tea/bench"),
  ("build",    Json.str "release"),
  ("count",    Json.num 42),
  ("tags",     Json.arr #[Json.str "bench", Json.str "lean"])
]

private def handler (req : Request) : IO Response := do
  match req.path, req.method with
  | "/health", "GET"  => return Response.text 200 "OK"
  | "/json",   "GET"  => return Response.json 200 jsonPayload
  | "/echo",   "POST" =>
    return { status := 200,
             headers := #[("content-type", "application/octet-stream")],
             body := req.body }
  | _, _ => return Response.notFound

private structure Args where
  port : UInt16 := 8080
  host : String := "127.0.0.1"
  /-- `--fast N` picks the POSIX-native accept-worker server
      (`LeanTea.Net.FastServer`, SO_REUSEPORT-based, N workers).
      Zero — the default — keeps the libuv-backed `serveConcurrent`
      so we can bench both from the same binary. -/
  fastWorkers : Nat := 0
  /-- `--reactor` picks the epoll/kqueue non-blocking reactor server
      (`LeanTea.Net.ReactorServer`). Single event-loop thread. -/
  useReactor : Bool := false

private partial def parseArgs : List String → Args → Args
  | "--port" :: v :: rest, a => parseArgs rest { a with port := (v.toNat?.getD 8080).toUInt16 }
  | "--host" :: v :: rest, a => parseArgs rest { a with host := v }
  | "--fast" :: v :: rest, a => parseArgs rest { a with fastWorkers := v.toNat?.getD 1 }
  | "--reactor" :: rest,   a => parseArgs rest { a with useReactor := true }
  | _ :: rest,             a => parseArgs rest a
  | [],                    a => a

def main (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  IO.eprintln s!"bench_server on http://{a.host}:{a.port}/"
  IO.eprintln s!"  routes: GET /health · GET /json · POST /echo"
  let nt := (← IO.getEnv "LEAN_NUM_THREADS").getD "(default = ncpu)"
  IO.eprintln s!"  LEAN_NUM_THREADS = {nt}"
  -- CLI flags win over env vars — makes ad-hoc perf runs unambiguous.
  let backend : LeanTea.Net.Backend.Backend ←
    if a.useReactor then pure .reactor
    else if a.fastWorkers > 0 then pure (.fast a.fastWorkers)
    else LeanTea.Net.Backend.fromEnv (default := .libuv)
  IO.eprintln s!"  backend = {repr backend}"
  LeanTea.Net.Backend.serve backend a.port a.host handler
