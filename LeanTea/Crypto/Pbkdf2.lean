import LeanTea.Crypto.Hmac
import LeanTea.Crypto.Sha256

/-! # LeanTea.Crypto.Pbkdf2 — PBKDF2-HMAC-SHA256 (RFC 8018)

Stretches a password into a long key by iterating HMAC many times.
The cost factor (`iterations`) is what makes brute-forcing leaked
hashes expensive — pick at least 100k for human passwords in 2026.

Uses our pure-Lean HMAC-SHA256 which is dramatically slower than
OpenSSL. For a CI-friendly default we recommend 10k–50k iterations;
production should override per workload. -/

namespace LeanTea.Crypto.Pbkdf2

open LeanTea.Crypto

private def outBlock : Nat := 32   -- HMAC-SHA256 output

/-- Big-endian 32-bit serialise. -/
private def be32 (n : UInt32) : ByteArray := Id.run do
  let mut out : ByteArray := .empty
  out := out.push ((n.shiftRight 24) &&& 0xff).toUInt8
  out := out.push ((n.shiftRight 16) &&& 0xff).toUInt8
  out := out.push ((n.shiftRight  8) &&& 0xff).toUInt8
  out := out.push ( n               &&& 0xff).toUInt8
  return out

/-- XOR two equal-length byte arrays. -/
private def xorBA (a b : ByteArray) : ByteArray := Id.run do
  let mut out : ByteArray := .empty
  for i in [:a.size] do
    out := out.push ((a.get! i) ^^^ (b.get! i))
  return out

/-- F(P, S, c, i) of RFC 8018 §5.2 — one PBKDF2 block. -/
private def fBlock (pw salt : ByteArray) (iter : Nat) (idx : UInt32)
    : ByteArray := Id.run do
  let u1     := Hmac.sha256 pw (salt ++ be32 idx)
  let mut u  := u1
  let mut t  := u1
  for _ in [1:iter] do
    u := Hmac.sha256 pw u
    t := xorBA t u
  return t

/-- `derive password salt iterations keyLen` — keyLen bytes. -/
def derive (password salt : ByteArray) (iterations : Nat) (keyLen : Nat)
    : ByteArray := Id.run do
  let blocks := (keyLen + outBlock - 1) / outBlock
  let mut out : ByteArray := .empty
  for i in [:blocks] do
    out := out ++ fBlock password salt iterations (i.toUInt32 + 1)
  return out.extract 0 keyLen

end LeanTea.Crypto.Pbkdf2
