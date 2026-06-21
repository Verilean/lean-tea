/-! # LeanTea.Crypto.Base64 — RFC 4648 base64 + base64url

Two variants:
  * `encode` / `decode` — standard alphabet (`+ /`), padded with `=`
  * `encodeUrl` / `decodeUrl` — URL-safe alphabet (`- _`), no padding
    (matches what JWT / WebAuthn use)

The encoder is `Id.run`-shaped, the decoder rejects malformed input
(returns `none`) rather than silently coercing. -/

namespace LeanTea.Crypto.Base64

private def stdAlphabet : String :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

private def urlAlphabet : String :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

private def encodeWith (alphabet : String) (padded : Bool)
    (bs : ByteArray) : String := Id.run do
  let abc := alphabet.toList.toArray
  let mut out := ""
  let mut i := 0
  while i + 2 < bs.size do
    let b0 := (bs.get! i).toNat
    let b1 := (bs.get! (i + 1)).toNat
    let b2 := (bs.get! (i + 2)).toNat
    out := out.push abc[(b0 >>> 2) &&& 0x3f]!
    out := out.push abc[((b0 <<< 4) ||| (b1 >>> 4)) &&& 0x3f]!
    out := out.push abc[((b1 <<< 2) ||| (b2 >>> 6)) &&& 0x3f]!
    out := out.push abc[b2 &&& 0x3f]!
    i := i + 3
  let rem := bs.size - i
  if rem == 1 then
    let b0 := (bs.get! i).toNat
    out := out.push abc[(b0 >>> 2) &&& 0x3f]!
    out := out.push abc[(b0 <<< 4) &&& 0x3f]!
    if padded then out := out.push '=' |>.push '='
  else if rem == 2 then
    let b0 := (bs.get! i).toNat
    let b1 := (bs.get! (i + 1)).toNat
    out := out.push abc[(b0 >>> 2) &&& 0x3f]!
    out := out.push abc[((b0 <<< 4) ||| (b1 >>> 4)) &&& 0x3f]!
    out := out.push abc[(b1 <<< 2) &&& 0x3f]!
    if padded then out := out.push '='
  return out

def encode    (bs : ByteArray) : String := encodeWith stdAlphabet true  bs
def encodeUrl (bs : ByteArray) : String := encodeWith urlAlphabet false bs

/-! ## Decode — reject unknown chars. -/

private def charIdx (alphabet : String) (c : Char) : Option Nat :=
  let pos := alphabet.toList.zipIdx.find? (·.fst == c)
  pos.map (·.snd)

private def decodeWith (alphabet : String) (s : String) : Option ByteArray := Id.run do
  -- Strip padding and any whitespace.
  let chars : List Char := s.toList.filter
    (fun c => c != '=' && c != '\n' && c != '\r' && c != ' ' && c != '\t')
  let mut out : ByteArray := .empty
  let mut buf : Nat := 0
  let mut bits : Nat := 0
  for c in chars do
    match charIdx alphabet c with
    | none   => return none
    | some v =>
      buf := (buf <<< 6) ||| v
      bits := bits + 6
      if bits ≥ 8 then
        let shift := bits - 8
        let byte := (buf >>> shift) &&& 0xff
        out := out.push byte.toUInt8
        buf := buf &&& ((1 <<< shift) - 1)
        bits := shift
  return some out

def decode    (s : String) : Option ByteArray := decodeWith stdAlphabet s
def decodeUrl (s : String) : Option ByteArray := decodeWith urlAlphabet s

end LeanTea.Crypto.Base64
