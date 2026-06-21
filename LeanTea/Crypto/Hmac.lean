import LeanTea.Crypto.Sha256

/-! # LeanTea.Crypto.Hmac — HMAC-SHA256 (RFC 2104)

Composes our pure-Lean SHA-256 into a keyed-MAC. Used by:

  * `LeanTea.Crypto.Pbkdf2` — password hashing
  * `LeanTea.Crypto.Jwt`    — HS256 token signing -/

namespace LeanTea.Crypto.Hmac

open LeanTea.Crypto

private def blockSize : Nat := 64    -- SHA-256 block size
private def outSize   : Nat := 32    -- SHA-256 digest size

/-- Step 1: derive a `blockSize`-length key by hashing-if-too-long
    then right-zero-padding. -/
private def normaliseKey (key : ByteArray) : ByteArray := Id.run do
  let k := if key.size > blockSize then Sha256.hash key else key
  let mut out := k
  for _ in [k.size : blockSize] do
    out := out.push 0x00
  return out

/-- XOR every byte of `bs` with `b`. -/
private def xorWith (bs : ByteArray) (b : UInt8) : ByteArray := Id.run do
  let mut out : ByteArray := .empty
  for i in [:bs.size] do
    out := out.push ((bs.get! i) ^^^ b)
  return out

/-- 32-byte HMAC-SHA256(key, msg). -/
def sha256 (key msg : ByteArray) : ByteArray :=
  let k     := normaliseKey key
  let oPad  := xorWith k 0x5c
  let iPad  := xorWith k 0x36
  let inner := Sha256.hash (iPad ++ msg)
  Sha256.hash (oPad ++ inner)

def sha256Hex (key msg : ByteArray) : String :=
  Sha256.toHex (sha256 key msg)

end LeanTea.Crypto.Hmac
