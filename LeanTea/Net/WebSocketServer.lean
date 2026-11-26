import Std.Sync.Mutex
import LeanTea.Net.Http
import LeanTea.Net.Transport
import LeanTea.Net.WebSocketFrame

/-! # LeanTea.Net.WebSocketServer — RFC 6455 server side

`LeanTea.Net.Server.serve` / `serveConcurrent` take an optional
`ws : Router`. For each request carrying `Upgrade: websocket` the router
decides:

* `none` — not a WebSocket route; the request goes to the HTTP handler.
* `some (.error resp)` — refuse (e.g. 401/403) with an ordinary response.
* `some (.ok session)` — the server answers `101 Switching Protocols` and
  runs `session conn` on the connection until it returns or the peer
  closes.

```lean
def ws : WebSocketServer.Router := fun req => pure <|
  if req.path == "/echo" then some (.ok fun conn => do
    while true do
      match ← conn.recv with
      | some (.text s) => conn.sendText s
      | some (.binary b) => conn.sendBinary b
      | none => break)
  else none

serveConcurrent 8080 "127.0.0.1" handler (ws := ws)
```

What it handles: text and binary messages, fragmented messages
(continuations are reassembled), ping → pong, the close handshake,
masked-frame enforcement, UTF-8 validation of text, and a message size
cap. Not handled: extensions (permessage-deflate) and subprotocol
negotiation (the session can read `Sec-WebSocket-Protocol` from the
request but the 101 does not echo one).

`send*` may be called from several tasks at once (a reader task and a
pusher task, typically); frames never interleave.

Sessions are long-lived, so use `serveConcurrent`: with the sequential
`serve` loop a session blocks every other connection. Only the default
(libuv) server and `serveUnix` support upgrades; the C `fast`/`reactor`
backends do not. -/

namespace LeanTea.Net.WebSocketServer

open LeanTea.Net.Http
open LeanTea.Net.WebSocketFrame

inductive Message where
  | text (s : String)
  | binary (b : ByteArray)
  deriving Inhabited

structure Conn where
  /-- The connection (TCP or Unix socket). -/
  io         : Transport
  /-- Bytes received but not yet decoded. -/
  buf        : IO.Ref ByteArray
  sendLock   : Std.Mutex Unit
  closed     : IO.Ref Bool
  /-- Largest message (after reassembly) accepted from the peer. -/
  maxMessage : Nat
  /-- The upgrade request (path, query, headers, cookies). -/
  request    : Request

abbrev Session := Conn → IO Unit
abbrev Router := Request → IO (Option (Except Response Session))

/-- A router that never upgrades. -/
def Router.none : Router := fun _ => pure Option.none

/-- The host part (`host[:port]`) of an `Origin` such as
    `https://app.example:8443`, lowercased. -/
def originAuthority (origin : String) : String :=
  match origin.splitOn "://" with
  | [_, rest] => ((rest.splitOn "/").head?.getD "").toLower
  | _ => ""

/-- Refuse (403) an upgrade whose `Origin` is not in `origins`. Browsers
    always send `Origin` on a WebSocket handshake and attach the user's
    cookies even when another site opens the socket, so without this a
    page anywhere can talk to the socket as the signed-in user
    (cross-site WebSocket hijacking). Requests without `Origin` (not a
    browser) pass. -/
def Router.allowOrigins (origins : List String) (r : Router) : Router := fun req =>
  match req.header? "origin" with
  | some o => if origins.contains o then r req
              else pure (some (.error (Response.text 403 "websocket: origin not allowed\n")))
  | none => r req

/-- Like `allowOrigins`, with "the page's own origin" as the list: the
    `Origin` host must equal the request's `Host`.

    With `requireHttps`, the origin must also be `https://`: otherwise
    a network attacker could serve a page on `http://` the same host and
    open the socket with the user's cookies. Behind a proxy that rewrites
    `Host`, use `allowOrigins` with the public origin instead. -/
def Router.sameOrigin (r : Router) (requireHttps : Bool := false) : Router := fun req =>
  match req.header? "origin" with
  | some o =>
    let schemeOk := !requireHttps || o.toLower.startsWith "https://"
    if schemeOk && originAuthority o == ((req.header? "host").getD "").toLower &&
        !(originAuthority o).isEmpty then r req
    else pure (some (.error (Response.text 403 "websocket: cross-origin upgrade refused\n")))
  | none => r req

namespace Conn

private def sendFrame (c : Conn) (opcode : UInt8) (payload : ByteArray) : IO Unit :=
  c.sendLock.atomically do
    c.io.send (encode opcode payload)

def sendText (c : Conn) (s : String) : IO Unit := c.sendFrame opText s.toUTF8
def sendBinary (c : Conn) (b : ByteArray) : IO Unit := c.sendFrame opBinary b
def ping (c : Conn) (payload : ByteArray := .empty) : IO Unit := c.sendFrame opPing payload

def send (c : Conn) : Message → IO Unit
  | .text s => c.sendText s
  | .binary b => c.sendBinary b

/-- Start the close handshake (idempotent). -/
def close (c : Conn) (code : Nat := 1000) (reason : String := "") : IO Unit := do
  if ← c.closed.modifyGet (fun was => (was, true)) then return
  try c.sendFrame opClose (closePayload code reason) catch _ => pure ()

def isClosed (c : Conn) : IO Bool := c.closed.get

/-- Next frame, refilling from the socket. `none` = EOF. -/
private partial def nextFrame (c : Conn) : IO (Except String (Option Frame)) := do
  match decode (← c.buf.get) c.maxMessage with
  | .frame f rest => c.buf.set rest; return .ok (some f)
  | .error e => return .error e
  | .needMore =>
    match ← c.io.recv 0 with
    | none => return .ok none
    | some chunk => c.buf.modify (· ++ chunk); nextFrame c

/-- Receive the next complete message. Answers pings, completes the
    close handshake and returns `none` once the connection is closed
    (by either side). Protocol violations close with the right code. -/
partial def recv (c : Conn) : IO (Option Message) := do
  let fail (code : Nat) (why : String) : IO (Option Message) := do
    c.close code why
    return none
  let rec loop (partOp : Option UInt8) (acc : ByteArray) : IO (Option Message) := do
    match ← c.nextFrame with
    | .error e => fail 1002 e
    | .ok none => c.closed.set true; return none
    | .ok (some f) =>
      if !f.masked then return ← fail 1002 "client frames must be masked"
      if f.opcode == opPing then
        unless ← c.isClosed do
          try c.sendFrame opPong f.payload catch _ => pure ()
        loop partOp acc
      else if f.opcode == opPong then loop partOp acc
      else if f.opcode == opClose then
        -- Echo the status code back, then stop.
        let code := if f.payload.size ≥ 2 then f.payload[0]!.toNat * 256 + f.payload[1]!.toNat else 1000
        c.close code
        return none
      else
        let (op, data) ← match partOp, f.opcode with
          | none, 0x1 | none, 0x2 => pure (f.opcode, f.payload)
          | some op, 0x0 => pure (op, acc ++ f.payload)
          | none, 0x0 => return ← fail 1002 "continuation without a message"
          | some _, _ => return ← fail 1002 "new message inside a fragmented one"
          | none, _ => return ← fail 1003 "unsupported opcode"
        if data.size > c.maxMessage then return ← fail 1009 "message too large"
        if !f.fin then loop (some op) data
        else if op == opText then
          match String.fromUTF8? data with
          | some s => return some (.text s)
          | none => fail 1007 "text message is not valid UTF-8"
        else return some (.binary data)
  if ← c.isClosed then return none
  loop none .empty

end Conn

/-- Does any `name` header (repeated headers form one list, RFC 7230
    §3.2.2) contain `token`, case-insensitively? -/
private def headerHas (req : Request) (name token : String) : Bool :=
  req.headers.any fun (k, v) =>
    k.toLower == name && (v.toLower.splitOn ",").any (fun t => t.trimAscii.toString == token)

/-- Does this request ask for a WebSocket upgrade? -/
def isUpgrade (req : Request) : Bool :=
  req.method == "GET" && headerHas req "upgrade" "websocket" && headerHas req "connection" "upgrade"

/-- The raw `101 Switching Protocols` head, or why the handshake is invalid. -/
def handshake (req : Request) : Except String ByteArray := do
  unless req.header? "sec-websocket-version" == some "13" do
    throw "unsupported Sec-WebSocket-Version (need 13)"
  let some key := req.header? "sec-websocket-key" | throw "missing Sec-WebSocket-Key"
  let key := key.trimAscii.toString
  match LeanTea.Crypto.Base64.decode key with
  | some bs => unless bs.size == 16 do throw "Sec-WebSocket-Key must be 16 bytes"
  | none => throw "Sec-WebSocket-Key is not base64"
  return ("HTTP/1.1 101 Switching Protocols\r\n" ++
          "Upgrade: websocket\r\n" ++
          "Connection: Upgrade\r\n" ++
          s!"Sec-WebSocket-Accept: {acceptKey key}\r\n\r\n").toUTF8

/-- Complete the upgrade on `io` and run `session`. `leftover` holds
    bytes that arrived after the request head (early frames). Always
    closes the connection afterwards. -/
def run (io : Transport) (req : Request) (leftover : ByteArray) (session : Session)
    (maxMessage : Nat := 16 * 1024 * 1024) : IO Unit := do
  match handshake req with
  | .error e =>
    io.send (Response.toBytes (Response.text 400 s!"websocket: {e}\n"))
  | .ok head =>
    io.send head
    let conn : Conn := { io, request := req, maxMessage,
                         buf := ← IO.mkRef leftover, sendLock := ← Std.Mutex.new (),
                         closed := ← IO.mkRef false }
    try session conn
    catch e => IO.eprintln s!"websocket session error: {e}"
    conn.close 1000
  io.close

end LeanTea.Net.WebSocketServer
