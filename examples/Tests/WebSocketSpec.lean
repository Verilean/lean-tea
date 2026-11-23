import LeanTea

/-! # websocket_spec

Pure checks of the RFC 6455 frame codec and handshake, then a real
server (`ws_echo_serve`, spawned as a subprocess — same-process server +
client share one libuv loop) driven by the `LeanTea.Net.WebSocket`
client and by hand-built frames. -/

open LeanTea.LSpec
open LeanTea.Net.Http
open LeanTea.Net.WebSocketFrame
open Std.Async

namespace WebSocketSpec

def bytes (xs : List Nat) : ByteArray := ⟨xs.toArray.map (·.toUInt8)⟩

def roundTrip (op : UInt8) (p : ByteArray) (mask : Option ByteArray) : Bool :=
  match decode (encode op p (mask := mask)) with
  | .frame f rest => f.opcode == op && f.payload == p && f.masked == mask.isSome && rest.size == 0
  | _ => false

def req (headers : Array (String × String)) : Request :=
  { method := "GET", path := "/echo", query := "", body := .empty, headers }

def goodHeaders : Array (String × String) :=
  #[("upgrade", "websocket"), ("connection", "keep-alive, Upgrade"),
    ("sec-websocket-version", "13"), ("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==")]

def big : ByteArray := ⟨(Array.range 70000).map (fun i => (i % 251).toUInt8)⟩

def pureSpecs : LSpec := group "codec + handshake" [
  it "acceptKey matches RFC 6455 §1.3" (acceptKey "dGhlIHNhbXBsZSBub25jZQ==" == "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="),
  it "round-trips short unmasked frames" (roundTrip opText "hi".toUTF8 none),
  it "round-trips masked frames" (roundTrip opBinary (bytes [1, 2, 3, 250]) (some (bytes [9, 8, 7, 6]))),
  it "uses the 16-bit length form" (roundTrip opText ⟨Array.replicate 300 65⟩ (some (bytes [1, 2, 3, 4]))),
  it "uses the 64-bit length form" (roundTrip opBinary big none),
  it "RFC 6455 §5.7 masked \"Hello\" decodes"
    (match decode (bytes [0x81, 0x85, 0x37, 0xfa, 0x21, 0x3d, 0x7f, 0x9f, 0x4d, 0x51, 0x58]) with
     | .frame f _ => f.payload == "Hello".toUTF8 && f.masked | _ => false),
  it "partial frames ask for more"
    (match decode ((encode opText "hello".toUTF8).extract 0 4) with | .needMore => true | _ => false),
  it "keeps bytes after the frame"
    (match decode (encode opText "a".toUTF8 ++ encode opText "b".toUTF8) with
     | .frame f rest => f.payload == "a".toUTF8 && rest == encode opText "b".toUTF8 | _ => false),
  it "rejects oversized control frames"
    (match decode (encode opPing ⟨Array.replicate 126 0⟩) with | .error _ => true | _ => false),
  it "rejects frames above the size cap"
    (match decode (encode opBinary big) 1000 with | .error _ => true | _ => false),
  it "rejects reserved bits"
    (match decode (bytes [0xC1, 0x00]) with | .error _ => true | _ => false),
  it "detects upgrade requests (case-insensitive token lists)"
    (LeanTea.Net.WebSocketServer.isUpgrade (req goodHeaders)),
  it "plain GET is not an upgrade" (!LeanTea.Net.WebSocketServer.isUpgrade (req #[])),
  it "handshake answers 101 with the accept key"
    (match LeanTea.Net.WebSocketServer.handshake (req goodHeaders) with
     | .ok head => let s := String.fromUTF8! head
                   (s.splitOn "101 Switching Protocols").length > 1 &&
                   (s.splitOn "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=").length > 1
     | .error _ => false),
  it "handshake rejects a wrong version"
    (match LeanTea.Net.WebSocketServer.handshake (req (goodHeaders.map fun (k, v) =>
        if k == "sec-websocket-version" then (k, "8") else (k, v))) with
     | .error _ => true | .ok _ => false)
]

/-! ## Raw frame helpers on top of the client's socket -/

open LeanTea.Net.WebSocket in
partial def recvFrame (c : Conn) : IO Frame := do
  match decode (← c.buf.get) with
  | .frame f rest => c.buf.set rest; pure f
  | .error e => throw (IO.userError e)
  | .needMore =>
    match ← (c.socket.recv? 65536).block with
    | some chunk => c.buf.modify (· ++ chunk); recvFrame c
    | none => throw (IO.userError "eof")

def mask : ByteArray := bytes [0x11, 0x22, 0x33, 0x44]

def sendRaw (c : LeanTea.Net.WebSocket.Conn) (op : UInt8) (p : ByteArray) (fin := true) (masked := true) : IO Unit :=
  (c.socket.send (encode op p fin (if masked then some mask else none))).block

def httpStatus (port : Nat) (path : String) (headers : Array (String × String)) : IO Nat := do
  let some u := LeanTea.Net.HttpClient.parseUrl s!"http://127.0.0.1:{port}{path}" | return 0
  return (← LeanTea.Net.HttpClient.request "GET" u (headers := headers)).status

def liveSpecs (port : Nat) : IO LSpec := do
  let url (p : String) := s!"ws://127.0.0.1:{port}{p}"
  let c ← LeanTea.Net.WebSocket.connect (url "/echo") #[("Origin", "http://example.test")]
  LeanTea.Net.WebSocket.sendText c "hello"
  let r1 ← LeanTea.Net.WebSocket.recvText c
  LeanTea.Net.WebSocket.sendText c "日本語 ✓"
  let r2 ← LeanTea.Net.WebSocket.recvText c
  let longMsg := String.ofList (List.replicate 70000 'x')
  LeanTea.Net.WebSocket.sendText c longMsg
  let r3 ← LeanTea.Net.WebSocket.recvText c
  -- Fragmented message with a ping in between (RFC 6455 §5.4).
  sendRaw c opText "frag-".toUTF8 (fin := false)
  sendRaw c opPing "p!".toUTF8
  sendRaw c opContinuation "ment".toUTF8
  let pong ← recvFrame c
  let frag ← recvFrame c
  -- Binary echo.
  sendRaw c opBinary (bytes [0, 255, 7])
  let bin ← recvFrame c
  -- Unmasked client frame → protocol error close (1002).
  sendRaw c opText "bad".toUTF8 (masked := false)
  let closeF ← recvFrame c
  -- Server-initiated close after a message.
  let b ← LeanTea.Net.WebSocket.connect (url "/bye")
  let bye ← LeanTea.Net.WebSocket.recvText b
  let byeClosed ← try let _ ← LeanTea.Net.WebSocket.recvText b; pure false catch _ => pure true
  -- Several clients at once.
  let clients ← (List.range 5).mapM fun _ => LeanTea.Net.WebSocket.connect (url "/echo")
  for (cl, i) in clients.zipIdx do LeanTea.Net.WebSocket.sendText cl s!"n{i}"
  let replies ← clients.mapM LeanTea.Net.WebSocket.recvText
  for cl in clients do LeanTea.Net.WebSocket.close cl
  let wsHeaders : Array (String × String) :=
    #[("Upgrade", "websocket"), ("Connection", "Upgrade"), ("Sec-WebSocket-Version", "13"),
      ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")]
  let denied ← httpStatus port "/deny" wsHeaders
  let badVersion ← httpStatus port "/echo" (wsHeaders.map fun (k, v) =>
    if k == "Sec-WebSocket-Version" then (k, "8") else (k, v))
  let plain ← httpStatus port "/" #[]
  let unrouted ← httpStatus port "/nows" wsHeaders
  pure <| group "live server" [
    it "echoes text" (r1 == "hello"),
    it "echoes UTF-8" (r2 == "日本語 ✓"),
    it "echoes a 70 kB message (64-bit length)" (r3 == longMsg),
    it "answers a ping inside a fragmented message" (pong.opcode == opPong && pong.payload == "p!".toUTF8),
    it "reassembles fragments" (frag.opcode == opText && frag.payload == "frag-ment".toUTF8),
    it "server frames are unmasked" (!frag.masked),
    it "echoes binary" (bin.opcode == opBinary && bin.payload == bytes [0, 255, 7]),
    it "closes with 1002 on an unmasked client frame"
      (closeF.opcode == opClose && closeF.payload.size ≥ 2 &&
       closeF.payload[0]!.toNat * 256 + closeF.payload[1]!.toNat == 1002),
    it "server-initiated close is delivered after the last message" (bye == "bye" && byeClosed),
    it "serves several clients concurrently" (replies == (List.range 5).map (s!"n{·}")),
    it "router can refuse an upgrade (403)" (denied == 403),
    it "bad handshake gets 400" (badVersion == 400),
    it "plain HTTP still works on the same port" (plain == 200),
    it "unrouted upgrade falls through to the HTTP handler" (unrouted == 200)
  ]

end WebSocketSpec

def main : IO UInt32 := do
  let port := 18766
  let child ← IO.Process.spawn {
    cmd := "./.lake/build/bin/ws_echo_serve", args := #["--port", toString port],
    stdin := .null, stdout := .null, stderr := .null }
  let mut up := false
  for _ in [:40] do
    if (← (try WebSocketSpec.httpStatus port "/" #[] catch _ => pure 0)) == 200 then up := true; break
    IO.sleep 100
  if !up then
    IO.eprintln "ws_echo_serve did not start (build it: lake build ws_echo_serve)"
    child.kill
    return 1
  let live ← try WebSocketSpec.liveSpecs port finally child.kill
  lspecIO (group "websocket" [WebSocketSpec.pureSpecs, live])
