import LeanTea.Crypto.Pbkdf2
import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Base64

/-! # LeanTea.Crypto.Password — hash + verify user passwords

Format string (Django-compatible):

```
pbkdf2_sha256$<iterations>$<saltB64>$<hashB64>
```

* `pbkdf2_sha256` — algorithm tag, future-proofs migration
* `<iterations>`  — cost factor, baked into the hash so it can be
  raised over time without invalidating old rows
* `<saltB64>`     — base64 of 16 random bytes
* `<hashB64>`     — base64 of 32-byte PBKDF2 output

The constant-time `==` on `String` in Lean is a value-equality on the
underlying `List Char`, so an attacker timing a `verify` call sees
work proportional to the matching prefix. For login flows that matters
— so we compare byte-by-byte against a sentinel that processes all 32
bytes regardless. See `constantTimeEq`. -/

namespace LeanTea.Crypto.Password

open LeanTea.Crypto

/-! ## Constants. Iteration count is conservative for our pure-Lean
    PBKDF2 — production should raise it to 100k+ if hardware allows. -/

def defaultIterations : Nat := 20000
def saltLen : Nat := 16
def hashLen : Nat := 32

/-! ## Randomness for salts -/

/-- 16 random bytes from `/dev/urandom`, with `IO.rand` fallback so
    the test suite still works on platforms without that special. -/
def randomSalt : IO ByteArray := do
  let result : IO ByteArray := do
    let h ← IO.FS.Handle.mk "/dev/urandom" .read
    h.read saltLen.toUSize
  let bytes ← result.catchExceptions (fun _ => pure .empty)
  if bytes.size == saltLen then return bytes
  let mut out : ByteArray := .empty
  for _ in [:saltLen] do
    let n ← IO.rand 0 255
    out := out.push n.toUInt8
  return out

/-! ## Constant-time equality

JS / Lean string `==` short-circuits on the first mismatch, leaking
bytes-prefix matched via timing. Compare byte arrays via an OR
accumulator instead. -/

def constantTimeEq (a b : ByteArray) : Bool := Id.run do
  if a.size != b.size then return false
  let mut acc : UInt8 := 0
  for i in [:a.size] do
    acc := acc ||| ((a.get! i) ^^^ (b.get! i))
  return acc == 0

/-! ## Hash + verify -/

/-- Produce a Django-style `pbkdf2_sha256$N$salt$hash` string. -/
def hash (password : String) (iterations : Nat := defaultIterations)
    : IO String := do
  let salt ← randomSalt
  let h := Pbkdf2.derive password.toUTF8 salt iterations hashLen
  return s!"pbkdf2_sha256${iterations}${Base64.encode salt}${Base64.encode h}"

/-- Parse the stored hash, re-derive with the candidate password,
    constant-time compare. Returns `false` for malformed inputs. -/
def verify (password stored : String) : Bool := Id.run do
  let parts := stored.splitOn "$"
  match parts with
  | [tag, iterS, saltB64, hashB64] =>
    if tag != "pbkdf2_sha256" then return false
    match iterS.toNat?, Base64.decode saltB64, Base64.decode hashB64 with
    | some iter, some salt, some expected =>
      let derived := Pbkdf2.derive password.toUTF8 salt iter expected.size
      constantTimeEq derived expected
    | _, _, _ => false
  | _ => false

/-! ## Hash rotation

Storage holds an algorithm tag + iteration count, so raising the cost
factor is just "if verify succeeds, re-hash with the new cost and
update the row." Callers do:

```lean
if verify cleartext stored then
  if needsRehash stored newIterations then
    let fresh ← hash cleartext newIterations
    -- write `fresh` back to the DB
```
-/

def needsRehash (stored : String) (target : Nat := defaultIterations) : Bool :=
  match stored.splitOn "$" with
  | [_, iterS, _, _] =>
    match iterS.toNat? with
    | some iter => iter < target
    | none      => true
  | _ => true

end LeanTea.Crypto.Password
