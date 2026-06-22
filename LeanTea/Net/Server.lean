import LeanTea.Net.Http
import Std.Async.TCP
import Std.Net

/-! # TCP server loop driving the HTTP handler

Sequential accept loop — one connection at a time. -/

namespace LeanTea.Net.Server

open LeanTea.Net.Http
open Std.Async
open Std.Net
open Std.Async.TCP

private partial def recvAll (client : Socket.Client) (acc : ByteArray) : IO ByteArray := do
  match splitHeaders acc with
  | some (headersStr, bodySoFar) =>
    -- Derive content-length from headers (case-insensitive lookup).
    let lower := headersStr.toLower
    let cl := match lower.splitOn "content-length:" with
      | _ :: rest :: _ =>
        let v := (rest.takeWhile (· != '\r')).toString.trim
        v.toNat?.getD 0
      | _ => 0
    if bodySoFar.size ≥ cl then
      return acc
    let chunk ← (client.recv? 4096).block
    match chunk with
    | some b => recvAll client (acc ++ b)
    | none   => return acc
  | none =>
    let chunk ← (client.recv? 4096).block
    match chunk with
    | some b => recvAll client (acc ++ b)
    | none   => return acc

private def handleConn (handler : Handler) (client : Socket.Client) : IO Unit := do
  try
    let raw ← recvAll client .empty
    let body : ByteArray :=
      match baFindSeq raw CRLFCRLF with
      | some h => raw.extract (h + 4) raw.size
      | none   => .empty
    let resp ← match parseRequest raw body with
      | some req =>
        try
          handler req
        catch e =>
          pure (Response.serverError s!"handler: {e}")
      | none => pure Response.badRequest
    let bytes := resp.toBytes
    (client.send bytes).block
    (client.shutdown).block
  catch e =>
    IO.eprintln s!"conn error: {e}"

partial def serveLoop (server : Socket.Server) (handler : Handler) : IO Unit := do
  let client ← (server.accept).block
  handleConn handler client
  serveLoop server handler

/-- Parse a dotted IPv4 string ("127.0.0.1") into a `Std.Net.IPv4Addr`.
    Returns `0.0.0.0` if parsing fails. -/
def parseIPv4 (s : String) : IPv4Addr :=
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  match (s.splitOn ".").map (·.toNat?.getD 0) with
  | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
  | _ => zero

def serve (port : UInt16 := 8001) (host : String := "0.0.0.0")
    (handler : Handler) : IO Unit := do
  let server ← Socket.Server.mk
  let addr : SocketAddress := .v4 {
    addr := parseIPv4 host,
    port := port
  }
  server.bind addr
  server.listen 64
  IO.eprintln s!"serving on http://{host}:{port}/"
  serveLoop server handler

end LeanTea.Net.Server
