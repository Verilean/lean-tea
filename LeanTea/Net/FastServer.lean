import LeanTea.Net.Http

/-! # LeanTea.Net.FastServer — POSIX-native HTTP/1.1 with SO_REUSEPORT

An alternative to `LeanTea.Net.Server` (which is `Std.Async.TCP` on
top of libuv). This one calls `socket(2)` / `accept(2)` / `recv(2)` /
`send(2)` directly through `c/leantea_fastnet.c`, then dispatches to
N accept-worker threads bound to the same port via `SO_REUSEPORT`.

## Why bypass `Std.Async.TCP`?

`Std.Async.TCP` returns an `AsyncTask` for every recv/send. Each
`.block` on that task hops through libuv → task wake → Lean fiber.
Under sustained keep-alive load our benchmarks put that hop at
100-500 µs — a hard ceiling around 6-7 k RPS regardless of
`LEAN_NUM_THREADS`.

The blocking model here is simpler and faster:

1. `bindReusePort` on each worker returns its own listener fd, and
   the kernel round-robins accept()s across them.
2. Each worker parks in the kernel on `accept()`, wakes on a client,
   handles it inline (parse → handler → serialize → send), and loops.
3. `LEAN_NUM_THREADS` controls the actual accept-loop count.

The trade: connections are pinned to whichever worker accepted them.
Long-blocking handlers can starve one worker's queue while others
sit idle. That's fine for the stateless request/response pattern the
`Handler` type already assumes. -/

namespace LeanTea.Net.FastServer

open LeanTea.Net.Http

/-! ## POSIX socket primitives — thin `@[extern]` bindings. -/

/-- Create a listener socket with `SO_REUSEADDR + SO_REUSEPORT + TCP_NODELAY`,
    bind to `INADDR_ANY:port`, and `listen(1024)`. Returns the fd. -/
@[extern "lean_ft_bind_reuseport"]
opaque bindReusePort (port : UInt16) : IO UInt32

/-- Block until one client connects, returning its fd. `TCP_NODELAY`
    is applied to the accepted socket too (the listener option doesn't
    propagate on macOS). -/
@[extern "lean_ft_accept_one"]
opaque acceptOne (listener : UInt32) : IO UInt32

/-- One blocking `recv`. Returns whatever the kernel hands over up to
    `max` bytes; empty ByteArray means EOF. -/
@[extern "lean_ft_recv_bytes"]
opaque recvBytes (fd : UInt32) (max : UInt32) : IO ByteArray

/-- Write-all: retries on partial write and EINTR. -/
@[extern "lean_ft_send_bytes"]
opaque sendBytes (fd : UInt32) (bytes : ByteArray) : IO Unit

/-- Half-close the write side. Lets the client read the final response
    bytes before the connection tears down. -/
@[extern "lean_ft_shutdown"]
opaque shutdownFd (fd : UInt32) : IO Unit

/-- Release the descriptor. -/
@[extern "lean_ft_close"]
opaque closeFd (fd : UInt32) : IO Unit

/-! ## Per-connection loop

`readOneRequest` reads until `\r\n\r\n` + Content-Length bytes are in
hand; then we split off the current request, keep the tail as
`leftover` for the next iteration, and call the handler. Same shape
as `Std.Async.TCP` server, just without the async hop. -/

private partial def readOneRequest (fd : UInt32) (acc : ByteArray)
    : IO (Option (ByteArray × ByteArray)) := do
  match splitHeaders acc with
  | some (headersStr, bodySoFar) =>
    let lower := headersStr.toLower
    let cl := match lower.splitOn "content-length:" with
      | _ :: rest :: _ =>
        let v := (rest.takeWhile (· != '\r')).toString.trim
        v.toNat?.getD 0
      | _ => 0
    if bodySoFar.size ≥ cl then
      let headBytes := headersStr.toUTF8
      let sep : ByteArray := ⟨#[0x0d, 0x0a, 0x0d, 0x0a]⟩
      let headEnd := headBytes.size + sep.size
      let reqEnd := headEnd + cl
      return some (acc.extract 0 reqEnd, acc.extract reqEnd acc.size)
    else
      let chunk ← recvBytes fd 8192
      if chunk.size == 0 then return none
      readOneRequest fd (acc ++ chunk)
  | none =>
    let chunk ← recvBytes fd 8192
    if chunk.size == 0 then return none
    readOneRequest fd (acc ++ chunk)

private def wantsClose (req : Request) : Bool :=
  let conn := (req.header? "connection").getD ""
  let l := conn.toLower
  if l.trim == "close" then true
  else if req.version.startsWith "HTTP/1.0" && l != "keep-alive" then true
  else false

private def annotateConnection (resp : Response) (close : Bool) : Response :=
  let already := resp.headers.any (fun (n, _) => n.toLower == "connection")
  if already then resp
  else
    let v := if close then "close" else "keep-alive"
    { resp with headers := resp.headers.push ("connection", v) }

private partial def handleConnLoop (handler : Handler) (fd : UInt32)
    (leftover : ByteArray) : IO Unit := do
  match ← readOneRequest fd leftover with
  | none => shutdownFd fd
  | some (raw, next) =>
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
    sendBytes fd resp.toBytes
    if close then shutdownFd fd
    else handleConnLoop handler fd next

private def handleConn (handler : Handler) (fd : UInt32) : IO Unit := do
  try
    handleConnLoop handler fd .empty
  catch _ => pure ()
  closeFd fd

/-! ## Accept-worker loops

Two design points behind the loop shape below:

1. Each accepted connection is handed off to a fresh `IO.asTask` so
   the accept worker can immediately return to `accept()`. If we
   handled the connection inline, keep-alive would pin one connection
   per worker and any concurrency past `workers` would deadlock in
   the OS accept queue.
2. The handler task uses the C blocking recv/send — no libuv hop. It
   does spend a worker thread while parked in the kernel, but that's
   also true of the async version; the difference is we avoid
   ~100-500 µs of scheduler overhead per syscall. -/

private partial def acceptLoop (listener : UInt32) (handler : Handler) : IO Unit := do
  let client ← acceptOne listener
  let _ ← IO.asTask (handleConn handler client)
  acceptLoop listener handler

/-- Serve `handler` on `port` with `workers` accept threads, each
    bound to the port via `SO_REUSEPORT`. The main thread parks on
    the first worker; the rest run as `IO.asTask`s.

    `workers` defaults to `1` — bump it (usually to `LEAN_NUM_THREADS`
    or physical core count) for real load. Above ~core count you get
    diminishing returns as the kernel-level accept queue is already
    saturated. -/
def serve (port : UInt16 := 8001) (workers : Nat := 1) (handler : Handler)
    : IO Unit := do
  IO.eprintln s!"fastserving on http://0.0.0.0:{port}/ (workers={workers})"
  -- Spawn (workers - 1) background workers, then run one on this thread.
  let tail : Nat := if workers == 0 then 0 else workers - 1
  for _ in [0:tail] do
    let _ ← IO.asTask do
      let listener ← bindReusePort port
      acceptLoop listener handler
  let listener ← bindReusePort port
  acceptLoop listener handler

end LeanTea.Net.FastServer
