/-! # LeanTea.Crypto.Sha256 — pure-Lean SHA-256 (FIPS 180-4)

Reference implementation good enough for password-hashing iteration
loops and JWT HS256 signing. Throughput is well below OpenSSL — fine
for tens of thousands of PBKDF2 rounds, not for hashing large files.

API:

```lean
LeanTea.Crypto.Sha256.hash (data : ByteArray) : ByteArray  -- 32 bytes
LeanTea.Crypto.Sha256.hashString (s : String) : ByteArray
```

The block-compression loop is unrolled into Lean's tail-recursive
`Nat.fold` style so the algorithm stays in pure value-level Lean. -/

namespace LeanTea.Crypto.Sha256

/-! ## 32-bit arithmetic helpers (operate inside UInt32) -/

@[inline] private def rotr (x : UInt32) (n : UInt32) : UInt32 :=
  x.shiftRight n ||| x.shiftLeft (32 - n)

@[inline] private def shr (x : UInt32) (n : UInt32) : UInt32 :=
  x.shiftRight n

@[inline] private def ch (x y z : UInt32) : UInt32 :=
  (x &&& y) ^^^ ((~~~x) &&& z)

@[inline] private def maj (x y z : UInt32) : UInt32 :=
  (x &&& y) ^^^ (x &&& z) ^^^ (y &&& z)

@[inline] private def bigSigma0 (x : UInt32) : UInt32 :=
  rotr x 2 ^^^ rotr x 13 ^^^ rotr x 22

@[inline] private def bigSigma1 (x : UInt32) : UInt32 :=
  rotr x 6 ^^^ rotr x 11 ^^^ rotr x 25

@[inline] private def smallSigma0 (x : UInt32) : UInt32 :=
  rotr x 7 ^^^ rotr x 18 ^^^ shr x 3

@[inline] private def smallSigma1 (x : UInt32) : UInt32 :=
  rotr x 17 ^^^ rotr x 19 ^^^ shr x 10

/-! ## Constants -/

private def K : Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
  0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
  0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
  0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
  0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
  0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
]

private def H0 : Array UInt32 := #[
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
]

/-! ## Padding (FIPS 180-4 §5.1.1) -/

/-- Append 0x80, zero-pad to multiple of 64 bytes minus 8, then
    big-endian 64-bit length in bits. -/
private def pad (msg : ByteArray) : ByteArray := Id.run do
  let l := msg.size
  let bitLen : UInt64 := (l.toUInt64) * 8
  -- After 0x80 we need (l + 1) ≡ 56 (mod 64).
  let rem := (l + 1) % 64
  let zeros := if rem ≤ 56 then 56 - rem else 64 + 56 - rem
  let mut out := msg
  out := out.push 0x80
  for _ in [:zeros] do
    out := out.push 0x00
  -- Append big-endian 64-bit bit length.
  for i in [:8] do
    let shift := (7 - i.toUInt64) * 8
    let byte := ((bitLen.shiftRight shift) &&& 0xff).toUInt8
    out := out.push byte
  return out

/-! ## Compression of one 512-bit block -/

@[inline] private def be32 (bs : ByteArray) (off : Nat) : UInt32 :=
  (bs.get! off).toUInt32 <<< 24 |||
  (bs.get! (off + 1)).toUInt32 <<< 16 |||
  (bs.get! (off + 2)).toUInt32 <<< 8 |||
  (bs.get! (off + 3)).toUInt32

private def expandSchedule (block : ByteArray) (off : Nat) : Array UInt32 := Id.run do
  let mut w : Array UInt32 := Array.mkEmpty 64
  for i in [:16] do
    w := w.push (be32 block (off + i * 4))
  for i in [16:64] do
    let s0 := smallSigma0 w[i - 15]!
    let s1 := smallSigma1 w[i - 2]!
    let v  := w[i - 16]! + s0 + w[i - 7]! + s1
    w := w.push v
  return w

private def compressBlock (H : Array UInt32) (block : ByteArray) (off : Nat)
    : Array UInt32 := Id.run do
  let w := expandSchedule block off
  let mut a := H[0]!
  let mut b := H[1]!
  let mut c := H[2]!
  let mut d := H[3]!
  let mut e := H[4]!
  let mut f := H[5]!
  let mut g := H[6]!
  let mut h := H[7]!
  for i in [:64] do
    let t1 := h + bigSigma1 e + ch e f g + K[i]! + w[i]!
    let t2 := bigSigma0 a + maj a b c
    h := g
    g := f
    f := e
    e := d + t1
    d := c
    c := b
    b := a
    a := t1 + t2
  return #[H[0]! + a, H[1]! + b, H[2]! + c, H[3]! + d,
           H[4]! + e, H[5]! + f, H[6]! + g, H[7]! + h]

/-! ## Public hash -/

/-- 32-byte SHA-256 digest of `data`. -/
def hash (data : ByteArray) : ByteArray := Id.run do
  let padded := pad data
  let mut H := H0
  let blocks := padded.size / 64
  for i in [:blocks] do
    H := compressBlock H padded (i * 64)
  -- Big-endian serialise H to 32 bytes.
  let mut out : ByteArray := .empty
  for i in [:8] do
    let v := H[i]!
    out := out.push ((v.shiftRight 24) &&& 0xff).toUInt8
    out := out.push ((v.shiftRight 16) &&& 0xff).toUInt8
    out := out.push ((v.shiftRight  8) &&& 0xff).toUInt8
    out := out.push ( v               &&& 0xff).toUInt8
  return out

def hashString (s : String) : ByteArray := hash s.toUTF8

/-! ## Hex helpers (mirrored from `LeanTea.Auth.hex` so callers don't
    have to drag the Auth module in just for formatting). -/

def toHex (ba : ByteArray) : String := Id.run do
  let digits := "0123456789abcdef".toList.toArray
  let mut s := ""
  for i in [:ba.size] do
    let b := ba.get! i
    s := s.push digits[(b.toNat / 16)]!
    s := s.push digits[(b.toNat % 16)]!
  return s

def hashHex (s : String) : String := toHex (hashString s)

end LeanTea.Crypto.Sha256
