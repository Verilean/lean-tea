import LeanTea.Crypto.Sha1
import LeanTea.Crypto.Base64

/-! # LeanTea.Net.WebSocketFrame — RFC 6455 frame codec (pure)

Shared by the WebSocket server (`LeanTea.Net.WebSocketServer`). Pure
functions over `ByteArray`, so the wire format is unit-testable without
sockets:

* `encode` builds one frame, optionally masked (clients must mask,
  servers must not — RFC 6455 §5.1).
* `decode` parses one frame from the front of a buffer and returns the
  unconsumed rest, or `needMore` when the buffer is incomplete.
* `acceptKey` computes `Sec-WebSocket-Accept` for a `Sec-WebSocket-Key`. -/

namespace LeanTea.Net.WebSocketFrame

/-- Opcodes (RFC 6455 §5.2). -/
def opContinuation : UInt8 := 0x0
def opText         : UInt8 := 0x1
def opBinary       : UInt8 := 0x2
def opClose        : UInt8 := 0x8
def opPing         : UInt8 := 0x9
def opPong         : UInt8 := 0xA

def isControl (op : UInt8) : Bool := op ≥ 0x8

structure Frame where
  fin     : Bool := true
  opcode  : UInt8
  /-- Already unmasked. -/
  payload : ByteArray
  /-- Whether the frame arrived masked (servers require it of clients). -/
  masked  : Bool := false
  deriving Inhabited

inductive Decoded where
  | frame (f : Frame) (rest : ByteArray)
  | needMore
  | error (msg : String)

private def guid : String := "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

/-- `Sec-WebSocket-Accept` value for a client's `Sec-WebSocket-Key`. -/
def acceptKey (clientKey : String) : String :=
  LeanTea.Crypto.Base64.encode (LeanTea.Crypto.Sha1.hashString (clientKey ++ guid))

private def xorMask (payload mask : ByteArray) : ByteArray := Id.run do
  let mut out := ByteArray.emptyWithCapacity payload.size
  for i in [0 : payload.size] do
    out := out.push (payload[i]! ^^^ mask[i % 4]!)
  return out

/-- Encode one frame. `mask` must be 4 bytes when given. -/
def encode (opcode : UInt8) (payload : ByteArray) (fin : Bool := true)
    (mask : Option ByteArray := none) : ByteArray := Id.run do
  let mut out := ByteArray.emptyWithCapacity (payload.size + 14)
  out := out.push ((if fin then 0x80 else 0) ||| (opcode &&& 0x0F))
  let maskBit : UInt8 := if mask.isSome then 0x80 else 0
  let n := payload.size
  if n < 126 then
    out := out.push (maskBit ||| n.toUInt8)
  else if n < 65536 then
    out := out.push (maskBit ||| 126)
    out := out.push (n / 256).toUInt8
    out := out.push (n % 256).toUInt8
  else
    out := out.push (maskBit ||| 127)
    for i in [0 : 8] do
      out := out.push ((n / (256 ^ (7 - i))) % 256).toUInt8
  match mask with
  | some m => return (out ++ m) ++ xorMask payload m
  | none   => return out ++ payload

/-- Decode one frame from the front of `buf`. Payloads larger than
    `maxPayload` are rejected (before they are buffered in full). -/
def decode (buf : ByteArray) (maxPayload : Nat := 16 * 1024 * 1024) : Decoded := Id.run do
  if buf.size < 2 then return .needMore
  let b0 := buf[0]!
  let b1 := buf[1]!
  if b0 &&& 0x70 != 0 then return .error "reserved bits set (no extensions negotiated)"
  let fin := b0 &&& 0x80 != 0
  let opcode := b0 &&& 0x0F
  let masked := b1 &&& 0x80 != 0
  let code := (b1 &&& 0x7F).toNat
  let (len, off) ←
    if code < 126 then pure (code, 2)
    else if code == 126 then
      if buf.size < 4 then return .needMore
      pure (buf[2]!.toNat * 256 + buf[3]!.toNat, 4)
    else
      if buf.size < 10 then return .needMore
      let mut n := 0
      for i in [0 : 8] do n := n * 256 + buf[2 + i]!.toNat
      pure (n, 10)
  if isControl opcode && (len > 125 || !fin) then
    return .error "control frames must be final and at most 125 bytes"
  if len > maxPayload then return .error s!"frame too large ({len} bytes)"
  let maskEnd := off + (if masked then 4 else 0)
  if buf.size < maskEnd + len then return .needMore
  let raw := buf.extract maskEnd (maskEnd + len)
  let payload := if masked then xorMask raw (buf.extract off maskEnd) else raw
  return .frame { fin, opcode, payload, masked } (buf.extract (maskEnd + len) buf.size)

/-- Close-frame payload: 2-byte status code + UTF-8 reason. -/
def closePayload (code : Nat) (reason : String := "") : ByteArray :=
  (ByteArray.mk #[(code / 256).toUInt8, (code % 256).toUInt8]) ++ (reason.toUTF8.extract 0 123)

end LeanTea.Net.WebSocketFrame
