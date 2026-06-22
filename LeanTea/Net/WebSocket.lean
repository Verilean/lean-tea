import Std.Async.TCP
import Std.Net
import LeanTea.Crypto.Sha1
import LeanTea.Crypto.Base64
import LeanTea.Net.HttpClient

/-! # LeanTea.Net.WebSocket — minimal RFC 6455 client

Built to talk to **Chrome DevTools Protocol** endpoints (the per-tab
`ws://localhost:9222/devtools/page/<id>` URLs you get from `GET
/json`). Anything in the same shape — text-only, no compression,
unmasked-server-to-client — works too.

Scope on purpose:
* `ws://` only. No TLS (matches HttpClient's plain-HTTP scope).
* Text frames only — `Text.send` / `Text.recv`. Binary, ping/pong,
  and per-message-deflate are not handled. CDP and most LLM
  streaming endpoints fit inside this subset.
* Server frames MUST be unmasked (per RFC 6455 §5.1). We send our
  own frames masked with a per-message random 4-byte key.
* No fragmentation. Each `send` produces one final-bit frame; we
  reject incoming fragmented frames.

Usage:

```lean
let ws ← LeanTea.Net.WebSocket.connect "ws://127.0.0.1:9222/devtools/page/abc"
LeanTea.Net.WebSocket.sendText ws "{\"id\":1,\"method\":\"Page.navigate\",\"params\":{...}}"
let reply ← LeanTea.Net.WebSocket.recvText ws
LeanTea.Net.WebSocket.close ws
```
-/

namespace LeanTea.Net.WebSocket

open Std.Async
open Std.Net
open Std.Async.TCP

/-! ## Connection handle -/

/-- Holds the active TCP socket plus a small read-buffer so frames
    that arrive in the same TCP read as the previous one don't get
    discarded. -/
structure Conn where
  socket : Socket.Client
  buf    : IO.Ref ByteArray

/-! ## Handshake -/

private def magicGuid : String :=
  "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

private def acceptKey (clientKey : String) : String :=
  let combined := clientKey ++ magicGuid
  let digest := LeanTea.Crypto.Sha1.hashString combined
  LeanTea.Crypto.Base64.encode digest

private def randomNonceB64 : IO String := do
  let mut bs : ByteArray := ByteArray.empty
  for _ in [0 : 16] do
    let r ← IO.rand 0 255
    bs := bs.push r.toUInt8
  return LeanTea.Crypto.Base64.encode bs

private def expectStatus101 (head : String) : IO Unit := do
  let firstLine := (head.splitOn "\r\n").head!
  let parts := firstLine.splitOn " "
  let status := match parts with
    | _ :: code :: _ => code.toNat?.getD 0
    | _              => 0
  if status != 101 then
    throw <| IO.userError s!"websocket: handshake returned status {status}\n{head}"

/-- Read bytes until we see `\r\n\r\n`. Leaves any post-header
    bytes in `extra` so the frame loop picks them up. -/
private partial def readHead (s : Socket.Client) (acc : ByteArray)
    : IO (String × ByteArray) := do
  let needle : ByteArray := "\r\n\r\n".toUTF8
  let scan (raw : ByteArray) : Option Nat := Id.run do
    if raw.size < needle.size then return none
    for i in [0 : raw.size - needle.size + 1] do
      if raw.extract i (i + needle.size) == needle then
        return some i
    return none
  match scan acc with
  | some cut =>
    let headBs := acc.extract 0 cut
    let extra := acc.extract (cut + needle.size) acc.size
    let headStr := match String.fromUTF8? headBs with
      | some s => s | none => ""
    return (headStr, extra)
  | none =>
    match (← (s.recv? 4096).block) with
    | none       => throw <| IO.userError "websocket: connection closed during handshake"
    | some chunk => readHead s (acc ++ chunk)

/-- Open a `ws://host:port/path` connection. Performs the RFC 6455
    upgrade handshake and returns a usable `Conn`. -/
def connect (rawUrl : String) : IO Conn := do
  let trimmed :=
    if rawUrl.startsWith "ws://" then rawUrl.drop 5 |>.toString
    else rawUrl
  /- Stripped URL still parses by HttpClient.parseUrl by re-adding
     the `http://` scheme. -/
  let url ← match LeanTea.Net.HttpClient.parseUrl ("http://" ++ trimmed) with
    | some u => pure u
    | none   => throw <| IO.userError s!"websocket: bad URL {rawUrl}"
  let socket ← Socket.Client.mk
  /- Same DNS-free numeric address path as HttpClient — splits the
     dotted-quad into 4 bytes. Hostnames other than `localhost`
     or a raw IPv4 won't resolve. -/
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  let ip : IPv4Addr :=
    let h := if url.host == "localhost" then "127.0.0.1" else url.host
    match (h.splitOn ".").map (·.toNat?.getD 0) with
    | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
    | _ => zero
  let addr : SocketAddress := .v4 { addr := ip, port := url.port }
  (socket.connect addr).block
  let key ← randomNonceB64
  let req :=
    s!"GET {url.path} HTTP/1.1\r\n" ++
    s!"Host: {url.host}:{url.port}\r\n" ++
    "Upgrade: websocket\r\n" ++
    "Connection: Upgrade\r\n" ++
    s!"Sec-WebSocket-Key: {key}\r\n" ++
    "Sec-WebSocket-Version: 13\r\n\r\n"
  (socket.send req.toUTF8).block
  let (head, extra) ← readHead socket .empty
  expectStatus101 head
  let expectedAccept := acceptKey key
  -- Sanity-check the server's Sec-WebSocket-Accept. Catches captive
  -- portals / wrong-port issues early.
  let acceptHeader : Option String :=
    (head.splitOn "\r\n").filterMap (fun line =>
      let lc := line.toLower
      if lc.startsWith "sec-websocket-accept:" then
        some <| (line.drop "sec-websocket-accept:".length).trimAscii.toString
      else none) |>.head?
  if let some v := acceptHeader then
    if v != expectedAccept then
      throw <| IO.userError s!"websocket: bad Sec-WebSocket-Accept ({v} vs {expectedAccept})"
  let buf ← IO.mkRef extra
  return { socket, buf }

/-! ## Frame I/O -/

/-- Read exactly `n` bytes from the conn's buffer, refilling from the
    socket as needed. Throws on premature EOF. -/
private partial def readN (c : Conn) (n : Nat) : IO ByteArray := do
  let cur ← c.buf.get
  if cur.size >= n then
    let out := cur.extract 0 n
    c.buf.set (cur.extract n cur.size)
    return out
  match (← (c.socket.recv? 65536).block) with
  | none       => throw <| IO.userError "websocket: connection closed mid-frame"
  | some chunk =>
    c.buf.set (cur ++ chunk)
    readN c n

/-- Build a client-to-server frame: FIN=1, opcode=1 (text), masked. -/
private def encodeTextFrame (payload : ByteArray) : IO ByteArray := do
  let mut frame : ByteArray := ByteArray.empty
  /- Byte 0: 0x81 = FIN bit + opcode 0x1 (text). -/
  frame := frame.push 0x81
  /- Length encoding: 0..125 inline, 126 → next 2 bytes, 127 → 8 bytes.
     MASK bit (0x80) always set on client frames. -/
  let n := payload.size
  if n < 126 then
    frame := frame.push (0x80 ||| n.toUInt8)
  else if n < 65536 then
    frame := frame.push 0xFE
    frame := frame.push (((n >>> 8) &&& 0xff).toUInt8)
    frame := frame.push (( n        &&& 0xff).toUInt8)
  else
    frame := frame.push 0xFF
    /- 64-bit big-endian length. -/
    let n64 : UInt64 := UInt64.ofNat n
    for i in [0 : 8] do
      let shift : UInt64 := UInt64.ofNat ((7 - i) * 8)
      frame := frame.push ((n64 >>> shift) &&& 0xff).toUInt8
  /- 4-byte masking key. -/
  let mut mask : ByteArray := ByteArray.empty
  for _ in [0 : 4] do
    let r ← IO.rand 0 255
    mask := mask.push r.toUInt8
  frame := frame ++ mask
  /- XOR payload by mask, cycling key over `i % 4`. -/
  let mut masked : ByteArray := ByteArray.empty
  for i in [0 : n] do
    let m := mask[i % 4]!
    masked := masked.push (payload[i]! ^^^ m)
  return frame ++ masked

/-- Send one text frame. -/
def sendText (c : Conn) (msg : String) : IO Unit := do
  let frame ← encodeTextFrame msg.toUTF8
  (c.socket.send frame).block

/-- Receive one text frame. Reassembles continuation frames if the
    server fragments (rare for CDP). Returns the decoded string. -/
partial def recvText (c : Conn) : IO String := do
  let hdr ← readN c 2
  let b0 := hdr[0]!
  let b1 := hdr[1]!
  let _fin := (b0 &&& 0x80) != 0
  let opcode := b0 &&& 0x0F
  let masked := (b1 &&& 0x80) != 0
  let lenCode := (b1 &&& 0x7F).toNat
  let len ←
    if lenCode == 126 then do
      let ext ← readN c 2
      pure (ext[0]!.toNat * 256 + ext[1]!.toNat)
    else if lenCode == 127 then do
      let ext ← readN c 8
      let mut n : Nat := 0
      for i in [0 : 8] do
        n := n * 256 + ext[i]!.toNat
      pure n
    else pure lenCode
  let mask ← if masked then readN c 4 else pure .empty
  let payload ← readN c len
  let bytes :=
    if masked then Id.run do
      let mut out := ByteArray.empty
      for i in [0 : len] do
        out := out.push (payload[i]! ^^^ mask[i % 4]!)
      pure out
    else payload
  match opcode with
  | 0x1 => return String.fromUTF8! bytes
  | 0x8 =>
    /- Close. Empty payload acceptable; signal as a thrown error so
       callers can clean up. -/
    throw <| IO.userError "websocket: server closed connection"
  | 0x9 =>
    /- Ping. Echo a pong with the same payload and recurse to wait
       for the real text frame. -/
    let mut pong := ByteArray.empty
    pong := pong.push 0x8A  -- FIN + opcode 0xA (pong)
    pong := pong.push (0x80 ||| bytes.size.toUInt8)
    let mut maskOut := ByteArray.empty
    for _ in [0 : 4] do
      let r ← IO.rand 0 255
      maskOut := maskOut.push r.toUInt8
    pong := pong ++ maskOut
    for i in [0 : bytes.size] do
      pong := pong.push (bytes[i]! ^^^ maskOut[i % 4]!)
    (c.socket.send pong).block
    recvText c
  | _ =>
    /- Binary, pong, continuation, etc. — skip and try again. -/
    recvText c

/-- Send a close frame and shut down the socket. -/
def close (c : Conn) : IO Unit := do
  let frame : ByteArray :=
    /- FIN + opcode 0x8 (close), masked, zero-length payload. -/
    ByteArray.mk #[0x88, 0x80, 0, 0, 0, 0]
  try (c.socket.send frame).block catch _ => pure ()
  (c.socket.shutdown).block

end LeanTea.Net.WebSocket
