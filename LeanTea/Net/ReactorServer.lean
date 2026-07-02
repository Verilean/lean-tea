import LeanTea.Net.Http

/-! # LeanTea.Net.ReactorServer — non-blocking single-thread reactor

The third HTTP server flavour in the framework. Trades:

| server       | throughput   | idle-conn ceiling     | when to use               |
|--------------|--------------|------------------------|---------------------------|
| `Server`     | ~6k RPS      | ~libuv (10k+)         | LLM proxy, WS, SSE, chat  |
| `FastServer` | ~64k RPS     | ~`LEAN_NUM_THREADS`   | short-request web APIs    |
| `ReactorServer` (this file) | ~40-60k RPS | ~fd limit (100k)   | both, at some latency cost |

Under the hood: `c/leantea_reactor.c` spins one kqueue (macOS) or
epoll (Linux) thread that manages all fds non-blocking. When a full
HTTP request has arrived in its recv buffer, C invokes a Lean
callback of type `ByteArray → IO ByteArray` — request bytes in,
response bytes out. The Lean callback runs on the reactor thread,
so a slow handler will stall the whole event loop; that's the price
of avoiding thread-per-conn. Handlers that need to do heavy work
should `IO.asTask` it off.

## Handler shape

The public API wraps the raw ByteArray callback so app authors keep
writing the same `Handler = Request → IO Response` they use with the
other servers:

```lean
LeanTea.Net.ReactorServer.serve 8080 fun req => do
  return Response.text 200 "hello"
```

Internally we parse `raw : ByteArray` into a `Request` via the same
`parseRequest` used elsewhere, run the user handler, and re-serialize
via `Response.toBytes`. That means all of the framework's per-request
allocation (splitOn, toLower, s! interpolation) happens on the
reactor thread. Reducing it — picohttpparser-style zero-alloc parser
plus a ByteArray builder for the response — is the natural next
lever, but even the current shape sustains 40-60 k RPS. -/

namespace LeanTea.Net.ReactorServer

open LeanTea.Net.Http

/-- Low-level entry point: hand C a callback that maps raw request
    bytes to raw response bytes. Blocks the caller for the lifetime of
    the server (same shape as `Server.serve`). -/
@[extern "lean_reactor_run"]
opaque reactorRun (port : UInt16) (rawHandler : ByteArray → IO ByteArray) : IO Unit

/-! ## User-facing wrapper -/

/-- Turn a `Handler` into the raw-bytes callback the reactor speaks. -/
private def wrap (handler : Handler) (raw : ByteArray) : IO ByteArray := do
  -- Split the raw request into header block + body — same layout the
  -- Server/FastServer paths compute.
  let body : ByteArray :=
    match baFindSeq raw CRLFCRLF with
    | some h => raw.extract (h + 4) raw.size
    | none   => .empty
  let (resp, close) ← match parseRequest raw body with
    | some req =>
      let conn := (req.header? "connection").getD ""
      let l := conn.toLower
      let wantsClose :=
        if l.trim == "close" then true
        else if req.version.startsWith "HTTP/1.0" && l != "keep-alive" then true
        else false
      let r ← try handler req
              catch e => pure (Response.serverError s!"handler: {e}")
      pure (r, wantsClose)
    | none => pure (Response.badRequest, true)
  -- Annotate Connection: header (idempotently) so the client knows
  -- whether to reuse the socket.
  let resp :=
    if resp.headers.any (fun (n, _) => n.toLower == "connection") then resp
    else
      let v := if close then "close" else "keep-alive"
      { resp with headers := resp.headers.push ("connection", v) }
  return resp.toBytes

/-- Serve `handler` on `port` using the non-blocking reactor. Blocks. -/
def serve (port : UInt16 := 8001) (handler : Handler) : IO Unit := do
  IO.eprintln s!"reactor-serving on http://0.0.0.0:{port}/"
  reactorRun port (wrap handler)

end LeanTea.Net.ReactorServer
