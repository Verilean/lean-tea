/-! # LeanTea.Crypto.Native — `@[extern]` decls for `c/leantea_crypto.c`

Mirrors the surface of the pure-Lean modules but routes through
OpenSSL libcrypto when the `LEANTEA_CRYPTO=1` build flag is set.
When the flag is off the C wrapper compiles to stubs that throw
`"Crypto FFI not compiled in"` on first use — so the link still
succeeds and callers can check `available` before delegating.

The pure-Lean modules (`Crypto.Sha256`, `Crypto.Hmac`,
`Crypto.Pbkdf2`) remain the in-process fallback. Production callers
that need real throughput use the native primitives here; tests
and CI can stay on the pure-Lean path. -/

namespace LeanTea.Crypto.Native

/-- `1` when the C wrapper was built with `LEANTEA_CRYPTO=1`,
    `0` otherwise. Used to gate fallbacks. -/
@[extern "leantea_crypto_available"]
opaque available : Unit → UInt8

/-- 32-byte SHA-256 digest. -/
@[extern "leantea_crypto_sha256"]
opaque sha256 (data : @& ByteArray) : IO ByteArray

/-- 32-byte HMAC-SHA256(key, msg). -/
@[extern "leantea_crypto_hmac_sha256"]
opaque hmacSha256 (key msg : @& ByteArray) : IO ByteArray

/-- PBKDF2-HMAC-SHA256(password, salt, iterations, keyLen). -/
@[extern "leantea_crypto_pbkdf2_sha256"]
opaque pbkdf2Sha256 (password salt : @& ByteArray)
    (iterations : UInt32) (keyLen : USize) : IO ByteArray

/-- Verify an RSA-SHA256 signature. Returns 1 on success, 0 on
    signature mismatch, throws on bad PEM / library error. -/
@[extern "leantea_crypto_rsa_verify_sha256"]
opaque rsaVerifySha256 (pemPubKey : @& String)
    (data sig : @& ByteArray) : IO UInt8

/-- Verify an ECDSA P-256 + SHA-256 signature. Same return shape as
    `rsaVerifySha256`. WebAuthn / FIDO2 leaf authenticators use this. -/
@[extern "leantea_crypto_ecdsa_p256_verify_sha256"]
opaque ecdsaP256VerifySha256 (pemPubKey : @& String)
    (data sig : @& ByteArray) : IO UInt8

/-! ## Convenience: choose-and-execute -/

/-- `true` when the native backend is available at runtime. -/
def isAvailable : Bool := available () == 1

end LeanTea.Crypto.Native
