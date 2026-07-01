import LeanTea.Net.Http
import Std.Async.TCP
import Std.Net

/-! # TCP server loop driving the HTTP handler

The accept loop is sequential and single-threaded. On each accepted
connection we now do HTTP/1.1 keep-alive: a connection can carry
many requests before the client (or server) decides to close it,
which saves 3-5 syscalls + one task-spawn per additional request.

Two accept-loop flavours:

  * `serve` / `serveLoop` — synchronous. The handler runs on the
    accept thread. Best throughput for CPU-bound tiny handlers
    (no task-spawn cost). Recommended default.
  * `serveConcurrent` / `serveLoopConcurrent` — each connection
    handed to an `IO.asTask`. Use when a single handler can block
    on an external resource (LLM turn, DB query) and you don't
    want to stall the rest of the server behind it.

The bench data in `docs/BENCHMARKS.md` shows why the sync variant
wins on trivial handlers — the task-spawn overhead exceeds the
handler cost. -/

namespace LeanTea.Net.Server

open LeanTea.Net.Http
open Std.Async
open Std.Net
open Std.Async.TCP

/-! ## Reading one HTTP/1.1 request off a keep-alive connection

`recvOneRequest` reads from `client` until it has *at least* one
complete request. Any bytes read past the current request end are
returned as `leftover` so the next iteration of the keep-alive loop
can pick them up without an extra `recv?` roundtrip. -/

private structure RecvResult where
  /-- Bytes for this request (headers + body, ending after the body). -/
  reqBytes : ByteArray
  /-- Bytes that arrived past this request end (pipelining tolerance
      and the "the next request's headers were in the same TCP
      segment" common case). -/
  leftover : ByteArray

private partial def recvUntilRequest (client : Socket.Client) (acc : ByteArray)
    : IO (Option RecvResult) := do
  match splitHeaders acc with
  | some (headersStr, bodySoFar) =>
    let lower := headersStr.toLower
    let cl := match lower.splitOn "content-length:" with
      | _ :: rest :: _ =>
        let v := (rest.takeWhile (· != '\r')).toString.trim
        v.toNat?.getD 0
      | _ => 0
    if bodySoFar.size ≥ cl then
      -- We have this request. Split: headers + first `cl` body bytes → reqBytes;
      -- the rest → leftover for the next iteration.
      let headBytes := headersStr.toUTF8
      let sep : ByteArray := ⟨#[0x0d, 0x0a, 0x0d, 0x0a]⟩
      let headEnd := headBytes.size + sep.size
      let reqEnd := headEnd + cl
      let reqBytes := acc.extract 0 reqEnd
      let leftover := acc.extract reqEnd acc.size
      return some { reqBytes, leftover }
    else
      let chunk ← (client.recv? 4096).block
      match chunk with
      | some b => recvUntilRequest client (acc ++ b)
      | none   => return none  -- EOF mid-request → give up
  | none =>
    let chunk ← (client.recv? 4096).block
    match chunk with
    | some b => recvUntilRequest client (acc ++ b)
    | none   =>
      -- No complete headers seen and connection closed → nothing to do.
      return none

/-- HTTP/1.1 default is keep-alive; HTTP/1.0 default is close; a
    `Connection: close` header on the request forces close. -/
private def wantsClose (req : Request) : Bool :=
  let conn := (req.header? "connection").getD ""
  let l := conn.toLower
  if l.trim == "close" then true
  else if req.version.startsWith "HTTP/1.0" && l != "keep-alive" then true
  else false

/-- Add `Connection: close|keep-alive` on the outgoing response. We
    don't touch other headers the handler set. -/
private def annotateConnection (resp : Response) (close : Bool) : Response :=
  let hName := "connection"
  let already := resp.headers.any (fun (n, _) => n.toLower == hName)
  if already then resp
  else
    let v := if close then "close" else "keep-alive"
    { resp with headers := resp.headers.push (hName, v) }

/-! ## Per-connection loop

`handleConn` now serves any number of requests on the same TCP
connection until the client (or the handler) opts to close it.
`leftover` carries bytes that arrived past the current request end
(HTTP pipelining or piggy-backed subsequent request in the same
segment), so we don't pay an extra `recv?` in the common case. -/

private partial def handleConnLoop (handler : Handler) (client : Socket.Client)
    (leftover : ByteArray) : IO Unit := do
  match ← recvUntilRequest client leftover with
  | none => (client.shutdown).block
  | some ⟨raw, next⟩ =>
    let body : ByteArray :=
      match baFindSeq raw CRLFCRLF with
      | some h => raw.extract (h + 4) raw.size
      | none   => .empty
    let (resp, close) ← match parseRequest raw body with
      | some req =>
        let c := wantsClose req
        let r ← try handler req
                catch e => pure (Response.serverError s!"handler: {e}")
        pure (r, c)
      | none => pure (Response.badRequest, true)
    let resp := annotateConnection resp close
    (client.send resp.toBytes).block
    if close then
      (client.shutdown).block
    else
      handleConnLoop handler client next

private def handleConn (handler : Handler) (client : Socket.Client) : IO Unit := do
  try
    handleConnLoop handler client .empty
  catch e =>
    IO.eprintln s!"conn error: {e}"

/-! ## Accept loops -/

partial def serveLoop (server : Socket.Server) (handler : Handler) : IO Unit := do
  let client ← (server.accept).block
  handleConn handler client
  serveLoop server handler

/-- Concurrent variant: each accepted connection is handled in its
    own `IO.asTask` so a long-running handler doesn't stall the rest
    of the server. Use this for apps where one request can block
    waiting on another (e.g. a chat UI that asks the user to approve
    a tool call via a separate API endpoint). The single-threaded
    `serveLoop` above is otherwise preferable — simpler lifetimes,
    no task-spawn overhead. See `docs/BENCHMARKS.md` for numbers. -/
partial def serveLoopConcurrent (server : Socket.Server) (handler : Handler)
    : IO Unit := do
  let client ← (server.accept).block
  let _ ← IO.asTask (handleConn handler client)
  serveLoopConcurrent server handler

/-- Parse a dotted IPv4 string ("127.0.0.1") into a `Std.Net.IPv4Addr`.
    Returns `0.0.0.0` if parsing fails. -/
def parseIPv4 (s : String) : IPv4Addr :=
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  match (s.splitOn ".").map (·.toNat?.getD 0) with
  | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
  | _ => zero

/-- Same as `serve`, but accept loop hands each connection to a
    concurrent task. Use when a handler can block on a user / external
    event that has to be resolved via another HTTP request. -/
def serveConcurrent (port : UInt16 := 8001) (host : String := "0.0.0.0")
    (handler : Handler) : IO Unit := do
  let server ← Socket.Server.mk
  let addr : SocketAddress := .v4 {
    addr := parseIPv4 host,
    port := port
  }
  server.bind addr
  -- Nagle off: tiny responses (health checks, JSON-RPC replies) shouldn't
  -- wait for a buffer to fill before hitting the wire.
  try server.noDelay catch _ => pure ()
  server.listen 128
  IO.eprintln s!"serving (concurrent, keep-alive) on http://{host}:{port}/"
  serveLoopConcurrent server handler

def serve (port : UInt16 := 8001) (host : String := "0.0.0.0")
    (handler : Handler) : IO Unit := do
  let server ← Socket.Server.mk
  let addr : SocketAddress := .v4 {
    addr := parseIPv4 host,
    port := port
  }
  server.bind addr
  try server.noDelay catch _ => pure ()
  server.listen 128
  IO.eprintln s!"serving (keep-alive) on http://{host}:{port}/"
  serveLoop server handler

end LeanTea.Net.Server
