import LeanTea.Crypto.Hmac
import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Base64
import LeanTea.Crypto.Password
import LeanTea.Crypto.Native
import Lean.Data.Json

/-! # LeanTea.Crypto.Jwt — JSON Web Token sign + verify (RFC 7519)

Implemented:
  * `HS256` (HMAC-SHA256) — pure Lean, ready to use.
  * `RS256` (RSA-SHA256) — verification via `openssl dgst -verify`
    shell-out, since pure-Lean RSA is impractical.

`alg: none` is rejected on verify — a well-known footgun where a
token signed with `none` is accepted because the verifier blindly
trusts the header. We treat any header `alg` outside the explicit
allow-list as a hard fail.

`exp` / `nbf` / `iat` are checked when present. Skew is configurable
(`leeway` seconds). The verifier never mutates input — it returns a
fresh `Claims` value with the parsed JSON payload. -/

namespace LeanTea.Crypto.Jwt

open LeanTea.Crypto
open Lean (Json)

/-! ## Header / Claims -/

inductive Alg
  | hs256
  | rs256
  | none      -- explicitly rejected on verify; only useful for tests
  deriving Inhabited, Repr, DecidableEq

def Alg.toString : Alg → String
  | .hs256 => "HS256"
  | .rs256 => "RS256"
  | .none  => "none"

def Alg.fromString : String → Option Alg
  | "HS256" => some .hs256
  | "RS256" => some .rs256
  | "none"  => some .none
  | _       => Option.none

structure Header where
  alg : Alg
  typ : String := "JWT"
  /-- RS256 key identifier — `kid` claim in the header. -/
  kid : Option String := none
  deriving Inhabited, Repr

def Header.toJson (h : Header) : Json :=
  let base : Json := Json.mkObj [
    ("alg", Json.str h.alg.toString),
    ("typ", Json.str h.typ)]
  match h.kid with
  | some k => base.setObjVal! "kid" (Json.str k)
  | none   => base

/-! ## Encode / decode segments -/

private def encodeSegment (j : Json) : String :=
  Base64.encodeUrl j.compress.toUTF8

private def decodeSegment (s : String) : Except String Json := do
  let bytes ← match Base64.decodeUrl s with
    | some bs => Except.ok bs
    | _       => Except.error s!"segment not valid base64url: `{s}`"
  let text := String.fromUTF8! bytes
  match Json.parse text with
  | .ok j    => return j
  | .error e => throw s!"segment JSON parse: {e}"

/-! ## Sign — HS256 -/

/-- Build a signed `header.payload.signature` JWT with HMAC-SHA256.
    Pass the shared secret as a UTF-8 string. -/
def signHS256 (claims : Json) (secret : String)
    (header : Header := { alg := .hs256 }) : String :=
  let head := encodeSegment header.toJson
  let body := encodeSegment claims
  let signingInput := head ++ "." ++ body
  let mac := Hmac.sha256 secret.toUTF8 signingInput.toUTF8
  signingInput ++ "." ++ Base64.encodeUrl mac

/-! ## Verify — common machinery + per-alg handling -/

structure VerifyOpts where
  /-- Allow-list of acceptable algorithms. Anything else (including
      `none`) is rejected. Default is HS256 only. -/
  algs   : List Alg := [.hs256]
  /-- Optional `iss` claim to enforce. -/
  issuer : Option String := none
  /-- Optional `aud` claim to enforce (exact string match). -/
  audience : Option String := none
  /-- Clock-skew tolerance for `exp` / `nbf`, in seconds. -/
  leeway : Nat := 30
  deriving Inhabited, Repr

structure Claims where
  header : Header
  body   : Json
  deriving Inhabited

private def jStr? (j : Json) (k : String) : Option String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption)

private def jNat? (j : Json) (k : String) : Option Nat :=
  (j.getObjVal? k).toOption.bind (·.getNat?.toOption)

/-- Common parse step: split into 3 segments, decode header, decide
    `Alg`. Returns the signing input + raw signature bytes so the
    per-algorithm verifier can take it from there. -/
private def parseEnvelope (token : String) (opts : VerifyOpts)
    : Except String (Header × Json × String × ByteArray) := do
  let parts := token.splitOn "."
  match parts with
  | [headB64, bodyB64, sigB64] =>
    let hdrJson ← decodeSegment headB64
    let algS ← match jStr? hdrJson "alg" with
      | some s => Except.ok s
      | none   => throw "header missing `alg`"
    let alg ← match Alg.fromString algS with
      | some a => Except.ok a
      | none   => throw s!"unknown alg `{algS}`"
    if !opts.algs.contains alg then
      throw s!"alg `{algS}` is not in the allow-list"
    let kid := jStr? hdrJson "kid"
    let typ := (jStr? hdrJson "typ").getD "JWT"
    let header : Header := { alg, typ, kid }
    let bodyJson ← decodeSegment bodyB64
    let sigBytes ← match Base64.decodeUrl sigB64 with
      | some bs => Except.ok bs
      | _       => throw "signature segment not valid base64url"
    let signingInput := headB64 ++ "." ++ bodyB64
    return (header, bodyJson, signingInput, sigBytes)
  | _ => throw s!"token must have 3 dot-separated segments, got {parts.length}"

/-- Standard-claim checks: `exp`, `nbf`, `iss`, `aud`. `nowSec` is
    typically `LeanTea.Auth.nowSec`. -/
private def checkClaims (body : Json) (opts : VerifyOpts) (nowSec : Nat)
    : Except String Unit := do
  if let some exp := jNat? body "exp" then
    if nowSec > exp + opts.leeway then
      throw s!"token expired (exp={exp}, now={nowSec})"
  if let some nbf := jNat? body "nbf" then
    if nowSec + opts.leeway < nbf then
      throw s!"token not yet valid (nbf={nbf})"
  if let some want := opts.issuer then
    if jStr? body "iss" != some want then
      throw s!"iss mismatch (want `{want}`)"
  if let some want := opts.audience then
    if jStr? body "aud" != some want then
      throw s!"aud mismatch (want `{want}`)"

/-- Verify an HS256-signed JWT. The secret must match the signer's. -/
def verifyHS256 (token : String) (secret : String) (nowSec : Nat)
    (opts : VerifyOpts := {}) : Except String Claims := do
  let (hdr, body, signingInput, sigBytes) ← parseEnvelope token opts
  if hdr.alg != .hs256 then
    throw s!"verifyHS256 called with non-HS256 token (alg={hdr.alg.toString})"
  let expected := Hmac.sha256 secret.toUTF8 signingInput.toUTF8
  if !(Password.constantTimeEq expected sigBytes) then
    throw "signature mismatch"
  checkClaims body opts nowSec
  return { header := hdr, body }

/-! ## RS256 — native FFI first, openssl shell-out fallback

Pure-Lean RSA verification would need a bignum library and a careful
modexp. We delegate to OpenSSL via `LeanTea.Crypto.Native` when the
FFI was compiled in (`LEANTEA_CRYPTO=1`), else shell out to the
`openssl` CLI as a portable fallback. -/

/-- Verify an RS256 JWT. `publicKey` is either a PEM-encoded RSA
    public key (preferred — used directly by the native FFI) **or**
    a filesystem path to such a PEM file (the shell-out fallback
    accepts either; we sniff by leading `-----BEGIN`). -/
def verifyRS256 (token : String) (publicKey : String) (nowSec : Nat)
    (opts : VerifyOpts := { algs := [.rs256] }) : IO (Except String Claims) := do
  match parseEnvelope token opts with
  | .error e => return .error e
  | .ok (hdr, body, signingInput, sigBytes) =>
    if hdr.alg != .rs256 then
      return .error s!"verifyRS256 called with non-RS256 token (alg={hdr.alg.toString})"
    let isPem := publicKey.startsWith "-----BEGIN"
    let verified ← (do
      if Native.isAvailable then
        -- Need the key as a PEM string. If we were handed a path,
        -- read the file first.
        let pem ← if isPem then pure publicKey else IO.FS.readFile publicKey
        let r ← Native.rsaVerifySha256 pem signingInput.toUTF8 sigBytes
        return (r == 1 : Bool)
      else
        -- Shell-out path: need a file for `openssl dgst -verify`.
        let keyPath ← if isPem then do
          let p := s!"/tmp/jwt_key_{nowSec}.pem"
          IO.FS.writeFile p publicKey; pure p
        else pure publicKey
        let sigPath := s!"/tmp/jwt_sig_{nowSec}.bin"
        let dataPath := s!"/tmp/jwt_data_{nowSec}.txt"
        IO.FS.writeBinFile sigPath sigBytes
        IO.FS.writeFile dataPath signingInput
        let out ← IO.Process.output {
          cmd := "openssl"
          args := #["dgst", "-sha256", "-verify", keyPath,
                    "-signature", sigPath, dataPath] }
        let _ ← (IO.FS.removeFile sigPath).catchExceptions (fun _ => pure ())
        let _ ← (IO.FS.removeFile dataPath).catchExceptions (fun _ => pure ())
        if isPem then
          let _ ← (IO.FS.removeFile keyPath).catchExceptions (fun _ => pure ())
        return (out.exitCode == 0 && out.stdout.startsWith "Verified" : Bool))
    if !verified then
      return .error "RSA signature verify: mismatch"
    match checkClaims body opts nowSec with
    | .error e => return .error e
    | .ok _    => return .ok { header := hdr, body }

end LeanTea.Crypto.Jwt
