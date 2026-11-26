import LeanTea.Net.Http
import LeanTea.Net.WebSocketServer
import LeanTea.Net.Transport
import LeanTea.Net.Unix
import Std.Async.TCP
import Std.Net
import Std.Async.Timer

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
handler cost.

`serveUnix` serves the same handler and WebSocket router on a Unix
domain socket, one dedicated thread per connection.

Before the WebSocket router and the handler, an optional `Takeover`
can claim a request and answer on the connection itself, for as long
as it likes (`LeanTea.Auth.Proxy` relays upstream responses and
WebSocket traffic this way). The connection closes when it returns. -/

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

/-- What reading one request ended with. -/
private inductive Recv where
  | request (r : RecvResult)
  /-- The request cannot be framed safely or is over a limit: answer
      with this status and close (the rest of the stream is not HTTP we
      can trust). -/
  | refuse (status : Nat)
  /-- EOF, a read error or the deadline: close without an answer. -/
  | closed

/-- Header section cap (64 KB) and total-request cap (8 MB). Without
    these a client can pin unbounded memory (giant Content-Length, or
    headers that never terminate). Mirrors the FFI backends' limits. -/
private def maxHeaderBytes : Nat := 64 * 1024
private def maxRequestBytes : Nat := 8 * 1024 * 1024

/-- Read until `acc` holds one whole request. The deadline is absolute:
    a client that trickles bytes cannot hold the connection past it. -/
private partial def recvUntilRequest (client : Transport) (acc : ByteArray) (deadline : Nat)
    : IO Recv := do
  match splitHeaders acc with
  | some (headersStr, bodySoFar) =>
    let headBytes := headersStr.toUTF8
    if headBytes.size + 4 > maxHeaderBytes then return .refuse 431
    match requestBodyLength headersStr with
    | .error status => return .refuse status
    | .ok cl =>
    if headBytes.size + 4 + cl > maxRequestBytes then return .refuse 413
    if bodySoFar.size ≥ cl then
      -- We have this request. Split: headers + first `cl` body bytes → reqBytes;
      -- the rest → leftover for the next iteration.
      let reqEnd := headBytes.size + 4 + cl
      return .request { reqBytes := acc.extract 0 reqEnd, leftover := acc.extract reqEnd acc.size }
    else
      match ← client.recv deadline with
      | some b => recvUntilRequest client (acc ++ b) deadline
      | none   => return .closed  -- EOF or deadline mid-request → give up
  | none =>
    -- Bound the header wait so a client that never sends "\r\n\r\n"
    -- can't grow `acc` without limit (slowloris).
    if acc.size > maxHeaderBytes then return .refuse 431
    match ← client.recv deadline with
    | some b => recvUntilRequest client (acc ++ b) deadline
    | none   => return .closed

/-- HTTP/1.1 default is keep-alive; HTTP/1.0 default is close; a
    `Connection: close` header on the request forces close. -/
private def wantsClose (req : Request) : Bool :=
  let conn := (req.header? "connection").getD ""
  let l := conn.toLower
  if l.trimAscii.toString == "close" then true
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

/-- A route that answers on the connection itself: given the request, it
    may return a function that gets the connection and the bytes already
    read past the request, writes the whole answer (or relays a stream or
    tunnel) and returns. The server then closes the connection. `none`
    leaves the request to the WebSocket router and the handler. -/
abbrev Takeover := Request → IO (Option (Transport → ByteArray → IO Unit))

def Takeover.none : Takeover := fun _ => pure Option.none

private partial def handleConnLoop (handler : Handler) (ws : WebSocketServer.Router)
    (takeover : Takeover) (client : Transport) (leftover : ByteArray) (requestTimeoutMs : Nat)
    : IO Unit := do
  -- 0 means the default, as in `ReactorServer` (not "close at once").
  let deadline := (← IO.monoMsNow) + (if requestTimeoutMs == 0 then 30000 else requestTimeoutMs)
  match ← recvUntilRequest client leftover deadline with
  | .closed => pure ()
  | .refuse status =>
    client.send (annotateConnection (Response.framingError status) true).toBytes
  | .request ⟨raw, next⟩ =>
    let body : ByteArray :=
      match baFindSeq raw CRLFCRLF with
      | some h => raw.extract (h + 4) raw.size
      | none   => .empty
    let req? := parseRequest raw body
    let taken? ← match req? with
      | some req => try takeover req
                    catch e => pure (some fun c _ => c.send
                      (annotateConnection (Response.serverError s!"takeover: {e}") true).toBytes)
      | none => pure none
    if let some answer := taken? then
      return ← answer client next
    -- WebSocket upgrade: the router decides; the session then owns the
    -- connection until it ends (no more HTTP on this socket).
    let upgrade? ← match req? with
      | some req =>
        if WebSocketServer.isUpgrade req then
          match ← (try ws req catch e => pure (some (.error (Response.serverError s!"ws router: {e}")))) with
          | some r => pure (some (req, r))
          | none => pure none
        else pure none
      | none => pure none
    match upgrade? with
    | some (req, .ok session) => WebSocketServer.run client req next session
    | some (_, .error resp) =>
      client.send (annotateConnection resp true).toBytes
    | none =>
    let (resp, close) ← match req? with
      | some req =>
        let c := wantsClose req
        let r ← try handler req
                catch e => pure (Response.serverError s!"handler: {e}")
        pure (r, c)
      | none => pure (Response.badRequest, true)
    let resp := annotateConnection resp close
    client.send resp.toBytes
    unless close do
      handleConnLoop handler ws takeover client next requestTimeoutMs

private def handleConn (handler : Handler) (ws : WebSocketServer.Router) (takeover : Takeover)
    (client : Transport) (requestTimeoutMs : Nat) : IO Unit := do
  try
    handleConnLoop handler ws takeover client .empty requestTimeoutMs
  catch e =>
    IO.eprintln s!"conn error: {e}"
  finally
    client.close

/-! ## Accept loops -/

partial def serveLoop (server : Socket.Server) (handler : Handler)
    (ws : WebSocketServer.Router := WebSocketServer.Router.none)
    (requestTimeoutMs : Nat := 30000) (takeover : Takeover := Takeover.none) : IO Unit := do
  let client ← (server.accept).block
  handleConn handler ws takeover (← Transport.ofTcp client) requestTimeoutMs
  serveLoop server handler ws requestTimeoutMs takeover

/-- Concurrent variant: each accepted connection is handled in its
    own `IO.asTask` so a long-running handler doesn't stall the rest
    of the server. Use this for apps where one request can block
    waiting on another (e.g. a chat UI that asks the user to approve
    a tool call via a separate API endpoint). The single-threaded
    `serveLoop` above is otherwise preferable — simpler lifetimes,
    no task-spawn overhead. See `docs/BENCHMARKS.md` for numbers. -/
partial def serveLoopConcurrent (server : Socket.Server) (handler : Handler)
    (ws : WebSocketServer.Router := WebSocketServer.Router.none)
    (requestTimeoutMs : Nat := 30000) (takeover : Takeover := Takeover.none) : IO Unit := do
  let client ← (server.accept).block
  let conn ← Transport.ofTcp client
  let _ ← IO.asTask (handleConn handler ws takeover conn requestTimeoutMs)
  serveLoopConcurrent server handler ws requestTimeoutMs takeover

/-- A dotted IPv4 address (four decimal octets, each 0–255), or
    `localhost` as 127.0.0.1. `none` for anything else — the caller must
    not guess, since falling back to 0.0.0.0 listens on every interface. -/
def parseIPv4? (s : String) : Option IPv4Addr :=
  if s == "localhost" then some ⟨#v[127, 0, 0, 1]⟩ else
  match s.splitOn "." with
  | [a, b, c, d] =>
    let oct := fun (t : String) => if !t.isEmpty && t.length ≤ 3 && t.all Char.isDigit then
        t.toNat?.bind (fun n => if n ≤ 255 then some n.toUInt8 else none) else none
    match oct a, oct b, oct c, oct d with
    | some a, some b, some c, some d => some ⟨#v[a, b, c, d]⟩
    | _, _, _, _ => none
  | _ => none

/-- `parseIPv4?`, or an error naming the host. -/
def listenAddress (host : String) : IO IPv4Addr :=
  match parseIPv4? host with
  | some a => pure a
  | none => throw <| IO.userError s!"serve: {host} is not an IPv4 address (use e.g. 127.0.0.1, 0.0.0.0 or localhost)"

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
    (handler : Handler) (ws : WebSocketServer.Router := WebSocketServer.Router.none)
    (requestTimeoutMs : Nat := 30000) (takeover : Takeover := Takeover.none) : IO Unit := do
  let server ← Socket.Server.mk
  let addr : SocketAddress := .v4 {
    addr := (← listenAddress host),
    port := port
  }
  server.bind addr
  -- Nagle off: tiny responses (health checks, JSON-RPC replies) shouldn't
  -- wait for a buffer to fill before hitting the wire.
  try server.noDelay catch _ => pure ()
  server.listen 128
  IO.eprintln s!"serving (concurrent, keep-alive) on http://{host}:{port}/"
  serveLoopConcurrent server handler ws requestTimeoutMs takeover

/-- Sequential accept loop. A WebSocket session (`ws`) holds the loop for
    its whole lifetime, so servers with WebSockets want `serveConcurrent`.

    `requestTimeoutMs` (0 = the default 30 s) bounds how long one request
    may take to arrive, counted from when the server starts waiting for
    it (so it also closes idle keep-alive connections). Trickling bytes
    does not extend it. It does not bound sending the response: on this
    loop a client that stops reading a large response, like one that
    stalls its request for up to the timeout, holds every other client.
    `serveConcurrent` does not have that problem. -/
def serve (port : UInt16 := 8001) (host : String := "0.0.0.0")
    (handler : Handler) (ws : WebSocketServer.Router := WebSocketServer.Router.none)
    (requestTimeoutMs : Nat := 30000) (takeover : Takeover := Takeover.none) : IO Unit := do
  let server ← Socket.Server.mk
  let addr : SocketAddress := .v4 {
    addr := (← listenAddress host),
    port := port
  }
  server.bind addr
  try server.noDelay catch _ => pure ()
  server.listen 128
  IO.eprintln s!"serving (keep-alive) on http://{host}:{port}/"
  serveLoop server handler ws requestTimeoutMs takeover

/-- `serveConcurrent` on a Unix domain socket at `path`, created with file
    mode `mode` (who may connect is decided by the file's owner, group and
    mode, and by the directory it is in). A stale socket at `path` is
    replaced; any other file there is an error. Each connection runs on
    its own dedicated thread.

    `requestTimeoutMs` (0 = the default 30 s) bounds how long each request
    may take to arrive, as on the TCP servers. -/
partial def serveUnix (path : String) (mode : UInt32 := 0o660)
    (handler : Handler) (ws : WebSocketServer.Router := WebSocketServer.Router.none)
    (requestTimeoutMs : Nat := 30000) (takeover : Takeover := Takeover.none) : IO Unit := do
  let listener ← Unix.listen path mode
  IO.eprintln s!"serving (concurrent, keep-alive) on unix:{path}"
  let rec loop : IO Unit := do
    match ← (Unix.accept listener).toBaseIO with
    | .ok fd =>
      let conn ← Transport.ofUnix fd
      let _ ← IO.asTask (prio := .dedicated) (handleConn handler ws takeover conn requestTimeoutMs)
    | .error e =>
      -- Out of descriptors and the like: wait a little rather than spin.
      IO.eprintln s!"unix accept: {e}"
      IO.sleep 100
    loop
  -- `accept` blocks its thread, so the loop runs on one of its own.
  let t ← IO.asTask (prio := .dedicated) loop
  match t.get with
  | .ok () => pure ()
  | .error e => throw e

end LeanTea.Net.Server
