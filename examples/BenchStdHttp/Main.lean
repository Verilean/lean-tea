import Std.Http
import Std.Http.Server
import Std.Async
import Std.Async.TCP
import Std.Net

/-! # examples/BenchStdHttp/Main.lean — reference: `Std.Http.Server` throughput

Same three routes (`GET /health`, `GET /json`, `POST /echo`) as
`bench_server`, but implemented on top of the `Std.Http.Server`
that ships with Lean 4.31 (`Std/Http/Server.lean`, Sofia Rodrigues).
The point is a like-for-like number so `docs/BENCHMARKS.md` can
show what a stock Std-only HTTP server delivers on the same box.

Prediction (from source reading): matches or trails our libuv
`LeanTea.Net.Server` at ~6-7 k RPS. It shares `Std.Async.TCP` for
socket I/O and layers additional per-connection bookkeeping
(`Std.Semaphore` for the connection limit, `Std.Mutex Nat` for the
active-conn counter, `Std.CancellationContext` for shutdown
coordination, full RFC-9112 `Std.Http.Protocol.H1` codec). -/

open Std Async Std.Http Std.Http.Server

/-- Read every remaining chunk off the streaming request body,
    concatenating into one ByteArray. `partial` because the loop
    terminates on the network side (EOF), not a Lean measure. -/
private partial def drainBody (body : Body.Stream) (acc : ByteArray := .empty)
    : Async ByteArray := do
  match ← body.recv with
  | some chunk => drainBody body (acc ++ chunk.data)
  | none       => return acc

private def jsonBody : String :=
  "{\"ok\":true,\"service\":\"lean-tea/bench\",\"build\":\"release\",\"count\":42,\"tags\":[\"bench\",\"lean\"]}"

private def onRequest (req : Request Body.Stream)
    : ContextAsync (Response Body.Any) := do
  let path := toString req.line.uri
  match req.line.method, path with
  | .get,  "/health" =>
    let r ← (Response.new.text "OK")
    return ↑r
  | .get,  "/json"   =>
    let r ← (Response.new.json jsonBody)
    return ↑r
  | .post, "/echo"   =>
    -- Std.Http.Server always hands the body as a stream; drain it,
    -- concatenate, and echo. This isn't the cheapest way to do echo
    -- but it's the natural one on this API. `partial` because the
    -- termination is on the network stream, not a Lean value.
    let bytes ← drainBody req.body
    let r ← (Response.new.fromBytes bytes)
    return ↑r
  | _, _ =>
    let r ← (Response.new.status .notFound |>.text "not found")
    return ↑r

private def handler : StatelessHandler where
  onRequest := onRequest

private structure Args where
  port : UInt16 := 8080

private partial def parseArgs : List String → Args → Args
  | "--port" :: v :: rest, a =>
    parseArgs rest { a with port := (v.toNat?.getD 8080).toUInt16 }
  | _ :: rest, a => parseArgs rest a
  | [],        a => a

def main (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  IO.eprintln s!"bench_std_http on http://0.0.0.0:{a.port}/"
  IO.eprintln s!"  routes: GET /health · GET /json · POST /echo"
  IO.eprintln s!"  backend = Std.Http.Server (Lean 4.31 stock)"
  let addr : Std.Net.SocketAddress :=
    .v4 { addr := ⟨#v[0, 0, 0, 0]⟩, port := a.port }
  Async.block do
    let server ← Std.Http.Server.serve addr handler
    server.waitShutdown
