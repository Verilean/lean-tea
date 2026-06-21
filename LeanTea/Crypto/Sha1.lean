/-! # LeanTea.Crypto.Sha1 — pure-Lean SHA-1 (FIPS 180-4)

Same style as `Sha256.lean`. SHA-1 is cryptographically broken for
collision-resistant signatures, but the WebSocket handshake (RFC
6455 §1.3) requires it specifically to derive `Sec-WebSocket-Accept`
from the client's `Sec-WebSocket-Key`. No general crypto use is
intended.

API:

```lean
LeanTea.Crypto.Sha1.hash (data : ByteArray) : ByteArray  -- 20 bytes
LeanTea.Crypto.Sha1.hashString (s : String) : ByteArray
```
-/

namespace LeanTea.Crypto.Sha1

/-! ## 32-bit helpers -/

@[inline] private def rotl (x : UInt32) (n : UInt32) : UInt32 :=
  x.shiftLeft n ||| x.shiftRight (32 - n)

/-! ## Block processing -/

/-- Build the 80-word schedule from a single 64-byte block. -/
private def schedule (block : ByteArray) (off : Nat) : Array UInt32 := Id.run do
  let mut w : Array UInt32 := Array.mkEmpty 80
  for i in [0 : 16] do
    let b := off + i * 4
    let v : UInt32 :=
      (block[b]!.toUInt32 <<< 24) |||
      (block[b+1]!.toUInt32 <<< 16) |||
      (block[b+2]!.toUInt32 <<< 8) |||
      block[b+3]!.toUInt32
    w := w.push v
  for i in [16 : 80] do
    let v := rotl (w[i-3]! ^^^ w[i-8]! ^^^ w[i-14]! ^^^ w[i-16]!) 1
    w := w.push v
  return w

/-- Round constants per FIPS 180-4 §5.1.1. -/
@[inline] private def kAt (t : Nat) : UInt32 :=
  if t < 20 then 0x5A827999
  else if t < 40 then 0x6ED9EBA1
  else if t < 60 then 0x8F1BBCDC
  else 0xCA62C1D6

/-- Round function f varies by quarter of the 80 rounds. -/
@[inline] private def fAt (t : Nat) (b c d : UInt32) : UInt32 :=
  if t < 20 then (b &&& c) ||| ((~~~ b) &&& d)
  else if t < 40 then b ^^^ c ^^^ d
  else if t < 60 then (b &&& c) ||| (b &&& d) ||| (c &&& d)
  else b ^^^ c ^^^ d

private def processBlock (h : Array UInt32) (block : ByteArray) (off : Nat) : Array UInt32 := Id.run do
  let w := schedule block off
  let mut a := h[0]!
  let mut b := h[1]!
  let mut c := h[2]!
  let mut d := h[3]!
  let mut e := h[4]!
  for t in [0 : 80] do
    let temp := (rotl a 5) + (fAt t b c d) + e + (kAt t) + w[t]!
    e := d
    d := c
    c := rotl b 30
    b := a
    a := temp
  return #[h[0]! + a, h[1]! + b, h[2]! + c, h[3]! + d, h[4]! + e]

/-! ## Padding + driver -/

private def padMessage (data : ByteArray) : ByteArray := Id.run do
  let lenBits : UInt64 := (UInt64.ofNat data.size) * 8
  let mut buf := data.push 0x80
  -- Pad with 0x00 until size ≡ 56 (mod 64), leaving 8 bytes for length.
  while buf.size % 64 != 56 do
    buf := buf.push 0
  -- 64-bit big-endian length.
  for i in [0 : 8] do
    let shift : UInt64 := UInt64.ofNat ((7 - i) * 8)
    buf := buf.push ((lenBits >>> shift) &&& 0xff).toUInt8
  return buf

def hash (data : ByteArray) : ByteArray := Id.run do
  let padded := padMessage data
  let mut h : Array UInt32 := #[
    0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0
  ]
  let blocks := padded.size / 64
  for i in [0 : blocks] do
    h := processBlock h padded (i * 64)
  -- Pack the five hash words big-endian into a 20-byte digest.
  let mut out : ByteArray := ByteArray.empty
  for w in h do
    out := out.push ((w >>> 24) &&& 0xff).toUInt8
    out := out.push ((w >>> 16) &&& 0xff).toUInt8
    out := out.push ((w >>> 8)  &&& 0xff).toUInt8
    out := out.push ( w         &&& 0xff).toUInt8
  return out

def hashString (s : String) : ByteArray := hash s.toUTF8

end LeanTea.Crypto.Sha1
