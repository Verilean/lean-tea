import LeanTea.Net.Http
import Std.Http.Server
import Std.Async
import Std.Net

/-! # TCP server loop driving the HTTP handler

Previously a hand-rolled HTTP/1.1 accept loop on top of
`Std.Internal.Async.TCP`. 4.31 ships a real `Std.Http.Server` with
graceful shutdown, connection limits, `100-continue` handling, and
proper async lifecycle — strictly more than what we wrote. We now
sit on top of it.

The public API stays:

```
LeanTea.Net.Server.serve port host handler
```

…where `handler : LeanTea.Net.Http.Request → IO LeanTea.Net.Http.Response`
keeps the original shape. The conversion between our
`Request`/`Response` (raw bytes, lowercased header array) and Std's
strongly-typed `Std.Http.{Request, Response}` happens in this
file. Anything more advanced (streaming bodies, Std's
`ContextAsync`-aware handlers) belongs at the Std layer; this
module is just the shim. -/

namespace LeanTea.Net.Server

open LeanTea.Net.Http
open Std.Async
open Std.Net
open Std.Http
open Std.Http.Server

/-! ## Request adapter: Std → our `Request` -/

private partial def drainStream (s : Body.Stream) (acc : ByteArray) : Async ByteArray := do
  match ← s.recv with
  | none       => return acc
  | some chunk => drainStream s (acc ++ chunk.data)

/-- Std's `URI.Query.toString` emits a leading `?`. Our public
    `Request.query` carries the raw `k=v&k=v` body without it
    (matching how the previous hand-rolled parser populated the
    field), so strip the prefix here. -/
private def queryStrip (s : String) : String :=
  if s.startsWith "?" then (s.drop 1).toString else s

private def pathAndQueryOf (target : RequestTarget) : String × String :=
  match target with
  | .originForm path query =>
    let p : String := toString path
    let q : String := match query with
                      | some q => queryStrip (toString q)
                      | none   => ""
    (p, q)
  | .absoluteForm uri =>
    let p : String := toString uri.path
    let q : String := queryStrip (toString uri.query)
    (p, q)
  | .authorityForm a => (toString a, "")
  | .asteriskForm    => ("*", "")

/-- Std's `Header.Name.toString` is the canonical PascalCase form;
    our existing parser populated header names as lowercase, and
    `Request.header?` compares against `name.toLower`. Preserve that
    invariant by reading `n.value` (the underlying lowercased
    representation) directly. -/
private def headersToPairs (h : Headers) : Array (String × String) :=
  h.toArray.map fun (n, v) => (n.value, v.value)

/-- Convert a Std request (with stream body) into our raw record.
    Drains the body up front so handlers see a complete `ByteArray`. -/
def fromStd (req : Std.Http.Request Body.Stream) : Async LeanTea.Net.Http.Request := do
  let (path, query) := pathAndQueryOf req.line.uri
  let body ← drainStream req.body .empty
  return {
    method  := toString req.line.method,
    path,
    query,
    headers := headersToPairs req.line.headers,
    body
  }

/-! ## Response adapter: our `Response` → Std -/

private def statusOfNat (n : Nat) : Status :=
  (Status.ofCode none n.toUInt16).getD .ok

private def headersFromPairs (pairs : Array (String × String)) : Headers := Id.run do
  let mut h := Headers.empty
  for (k, v) in pairs do
    h := h.insert! k v
  return h

/-- Convert our raw response into a Std response with a `Body.Full`
    payload (the bytes the handler computed). -/
def toStd (resp : LeanTea.Net.Http.Response) : Async (Std.Http.Response Body.Any) := do
  let head : Response.Head := {
    status  := statusOfNat resp.status,
    version := .v11,
    headers := headersFromPairs resp.headers
  }
  let full ← Body.Full.ofByteArray resp.body
  return { line := head, body := .ofBody full, extensions := .empty }

/-! ## Handler bridge -/

private structure LegacyHandler where
  inner : LeanTea.Net.Http.Handler

instance : Std.Http.Server.Handler LegacyHandler where
  ResponseBody := Body.Any
  onRequest h req := do
    let our ← fromStd req
    /- `IO → ContextAsync` is a MonadLift instance in Std.Async,
       so we can simply `← h.inner our` without an explicit lift. -/
    let resp ← h.inner our
    toStd resp

/-! ## IPv4 helper -/

def parseIPv4 (s : String) : IPv4Addr :=
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  match (s.splitOn ".").map (·.toNat?.getD 0) with
  | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
  | _ => zero

/-- Run an HTTP server on `port` / `host` with the given
    LeanTEA-shaped handler. Blocks until the underlying
    `Std.Http.Server` is shut down. -/
def serve (port : UInt16 := 8001) (host : String := "0.0.0.0")
    (handler : Handler) : IO Unit := do
  let addr : SocketAddress := .v4 {
    addr := parseIPv4 host,
    port := port
  }
  IO.eprintln s!"serving on http://{host}:{port}/"
  Async.block do
    let server ← Std.Http.Server.serve addr (LegacyHandler.mk handler) {}
    server.waitShutdown

end LeanTea.Net.Server
