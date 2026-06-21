import LeanTea.Auth
import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Base64
import LeanTea.Crypto.Password
import LeanTea.Crypto.Native
import Lean.Data.Json

/-! # LeanTea.Auth.Passkey — WebAuthn / FIDO2 server logic

Covers the *relying-party* side of WebAuthn — what a server has to
do during registration + authentication. The browser does the
cryptographic heavy lifting; the RP only has to:

  * Mint random challenges + store them in a session
  * Verify `clientDataJSON` is well-formed and matches the challenge
  * Verify the `authenticatorData` flags (UV / UP) + RP-ID hash
  * Verify the assertion signature with the stored public key

ECDSA-P256 (the dominant COSE alg `-7`) signature verification is
delegated to `openssl pkeyutl -verify`. CBOR / COSE parsing is
limited to the minimum needed to lift the public key out of the
attestation object.

## Quick flow

Registration:

```
client → server  POST /webauthn/register/begin
server → client  challenge + rp/user info
client (browser) navigator.credentials.create(...)
client → server  POST /webauthn/register/finish  attestationObject + clientDataJSON
server          verifyRegistration  →  store {credId, publicKey}
```

Authentication:

```
client → server  POST /webauthn/login/begin {handle}
server → client  challenge + allowCredentials [credId]
client (browser) navigator.credentials.get(...)
client → server  POST /webauthn/login/finish  authenticatorData + clientDataJSON + signature
server          verifyAuthentication  →  issue session
```
-/

namespace LeanTea.Auth.Passkey

open LeanTea.Auth
open LeanTea.Crypto
open Lean (Json)

/-! ## Stored credential -/

structure StoredCredential where
  /-- base64url-encoded credential ID. Matches what the browser sends. -/
  credId       : String
  /-- PEM-formatted EC public key (P-256). We export to PEM during
      registration so verifyAuthentication can hand it to openssl. -/
  publicKeyPem : String
  /-- Monotonic signature counter. WebAuthn requires it strictly
      increases between authentications; otherwise the credential
      may have been cloned. -/
  signCount    : UInt32 := 0
  /-- RP-ID at registration. Future logins must match. -/
  rpId         : String
  deriving Inhabited, Repr

/-! ## Challenge management

Challenges are 32 random bytes, base64url-encoded. The server stores
them keyed by session token and consumes (deletes) them when the
client finishes. -/

def newChallenge : IO String := do
  let h ← IO.FS.Handle.mk "/dev/urandom" .read
  let bytes ← h.read (32 : USize)
  return Base64.encodeUrl bytes

/-! ## clientDataJSON parsing

The browser hands the RP a UTF-8 JSON blob shaped like:

```
{ "type": "webauthn.create" | "webauthn.get",
  "challenge": "<base64url challenge>",
  "origin": "https://app.example.com",
  "crossOrigin": false }
```

We don't blindly trust the JSON; we re-validate every field. -/

structure ClientData where
  ctype     : String   -- "webauthn.create" or "webauthn.get"
  challenge : String   -- base64url
  origin    : String
  raw       : ByteArray
  deriving Inhabited

private def jStr (j : Json) (k : String) : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD ""

def parseClientData (bytes : ByteArray) : Except String ClientData := do
  let text := String.fromUTF8! bytes
  match Json.parse text with
  | .error e => throw s!"clientDataJSON parse: {e}"
  | .ok j    =>
    return { ctype     := jStr j "type"
           , challenge := jStr j "challenge"
           , origin    := jStr j "origin"
           , raw       := bytes }

/-! ## CBOR sniff for COSE key

A full CBOR decoder is overkill. The attestationObject is a CBOR
map with three top-level keys: `fmt`, `attStmt`, `authData`. We hop
the authData bytes out, then within authData jump past the AAGUID
to find the credentialPublicKey COSE map.

For now we use the **`openssl ec -text -in attestation.cose -inform DER`**
shell-out path to convert COSE EC2 → PEM. -/

/-- Skim the attestation object for the credId (raw) and the credPubKey
    (PEM-converted by `openssl`). Hands back a `StoredCredential`
    ready to insert into the user's credentials table. -/
def extractCredential (attestationObj : ByteArray) (rpId : String)
    : IO (Except String StoredCredential) := do
  -- This is a stub for the COSE → PEM conversion. Production:
  --   * Decode CBOR top-level map to find `authData`.
  --   * Authdata: 32-byte rpIdHash | 1-byte flags | 4-byte signCount |
  --              | AAGUID(16) | credIdLen(2) | credId | credPubKey(COSE).
  --   * Hand credPubKey (raw CBOR bytes) to a tiny converter that
  --     emits PEM via `openssl`.
  --
  -- For demonstration we accept the data as-is and let the caller
  -- pre-process via a Node-side helper (which most WebAuthn SDKs
  -- already provide), so this Lean code stays small.
  IO.FS.writeBinFile "/tmp/passkey_attest.cbor" attestationObj
  -- Production: shell out to `cose-key-to-pem` or similar.
  -- Here we surface that the caller must preprocess.
  return .error "Passkey.extractCredential: CBOR/COSE preprocessing required (use a JS helper)."

/-! ## Verify registration -/

structure RegisterRequest where
  attestationObject : ByteArray
  clientDataJSON    : ByteArray
  /-- Original challenge the RP issued, retrieved from session. -/
  expectedChallenge : String
  expectedOrigin    : String
  expectedRpId      : String
  deriving Inhabited

def verifyRegistration (_ : RegisterRequest) : IO (Except String StoredCredential) := do
  -- Top-level: parse clientDataJSON, confirm `type == "webauthn.create"`,
  -- challenge / origin match, then unpack the attestationObject and
  -- emit a StoredCredential. The crypto details flow through
  -- `extractCredential` above.
  --
  -- Documented stub — full impl is ~200 LOC and would dwarf the rest
  -- of this module. See `extractCredential`.
  return .error "Passkey.verifyRegistration: not implemented (see extractCredential)."

/-! ## Verify authentication -/

structure AuthenticateRequest where
  authenticatorData : ByteArray
  clientDataJSON    : ByteArray
  signature         : ByteArray
  expectedChallenge : String
  expectedOrigin    : String
  expectedRpId      : String
  /-- Credential the user-handle resolved to. -/
  stored            : StoredCredential
  deriving Inhabited

/-- Verify an assertion signature with `openssl pkeyutl`. The signed
    bytes per WebAuthn spec are `authenticatorData || SHA-256(clientDataJSON)`.

    Returns the updated `signCount` so the caller can persist it. -/
def verifyAuthentication (req : AuthenticateRequest)
    : IO (Except String UInt32) := do
  -- 1. clientDataJSON shape
  match parseClientData req.clientDataJSON with
  | .error e => return .error e
  | .ok cd =>
    if cd.ctype != "webauthn.get" then
      return .error s!"clientData type != webauthn.get (got `{cd.ctype}`)"
    if cd.challenge != req.expectedChallenge then
      return .error "challenge mismatch"
    if cd.origin != req.expectedOrigin then
      return .error s!"origin mismatch (got `{cd.origin}`)"
    -- 2. authenticatorData RP-ID hash + signCount
    let ad := req.authenticatorData
    if ad.size < 37 then
      return .error s!"authenticatorData too short ({ad.size} bytes)"
    let rpIdHash := ad.extract 0 32
    let expectedRpHash := Sha256.hashString req.expectedRpId
    if !Password.constantTimeEq rpIdHash expectedRpHash then
      return .error "RP-ID hash mismatch"
    -- Counter is bytes 33..36, big-endian.
    let counter : UInt32 :=
      (ad.get! 33).toUInt32 <<< 24 |||
      (ad.get! 34).toUInt32 <<< 16 |||
      (ad.get! 35).toUInt32 <<< 8 |||
      (ad.get! 36).toUInt32
    if counter ≤ req.stored.signCount && counter != 0 then
      return .error s!"signCount didn't advance ({counter} ≤ {req.stored.signCount}) — possible cloned authenticator"
    -- 3. ECDSA P-256 signature verify. Prefer the native libcrypto
    --    path (no fork) when LEANTEA_CRYPTO=1; fall back to openssl
    --    CLI shell-out so this still works in stub builds.
    let signedBytes := ad ++ Sha256.hash req.clientDataJSON
    let verified ← (do
      if Native.isAvailable then
        let r ← Native.ecdsaP256VerifySha256 req.stored.publicKeyPem
                  signedBytes req.signature
        return (r == 1 : Bool)
      else
        let pkPath  := "/tmp/passkey_pub.pem"
        let sigPath := "/tmp/passkey_sig.bin"
        let dataPath := "/tmp/passkey_signed.bin"
        IO.FS.writeFile pkPath req.stored.publicKeyPem
        IO.FS.writeBinFile sigPath req.signature
        IO.FS.writeBinFile dataPath signedBytes
        let out ← IO.Process.output {
          cmd := "openssl"
          args := #["dgst", "-sha256", "-verify", pkPath,
                    "-signature", sigPath, dataPath] }
        let _ ← (IO.FS.removeFile pkPath).catchExceptions (fun _ => pure ())
        let _ ← (IO.FS.removeFile sigPath).catchExceptions (fun _ => pure ())
        let _ ← (IO.FS.removeFile dataPath).catchExceptions (fun _ => pure ())
        return (out.exitCode == 0 && out.stdout.startsWith "Verified" : Bool))
    if !verified then
      return .error "signature verify failed"
    return .ok counter

end LeanTea.Auth.Passkey
