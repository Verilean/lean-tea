import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Hmac
import LeanTea.Crypto.Pbkdf2
import LeanTea.Crypto.Base64
import LeanTea.Crypto.Password
import LeanTea.Crypto.Jwt
import LeanTea.Crypto.Native
import LeanTea.Auth.Saml
import LeanTea.Auth.Security
import Lean.Data.Json

/-! # crypto_smoke — round-trip the crypto primitives against
known answer values from RFC / NIST test vectors. -/

open LeanTea.Crypto

def expect (label : String) (actual expected : String) : IO Unit := do
  if actual == expected then
    IO.println s!"  ✓ {label} = {actual}"
  else
    IO.println s!"  ✗ {label}"
    IO.println s!"      expected: {expected}"
    IO.println s!"      got:      {actual}"

def main : IO Unit := do
  IO.println "── SHA-256 (FIPS 180-4 known vectors) ─────────────────────"
  expect "empty"
    (Sha256.hashHex "")
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  expect "abc"
    (Sha256.hashHex "abc")
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  -- 56-byte boundary in the padding code (FIPS test).
  expect "long"
    (Sha256.hashHex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"

  IO.println ""
  IO.println "── HMAC-SHA256 (RFC 4231 test case 1) ─────────────────────"
  let k1 : ByteArray := Id.run do
    let mut b : ByteArray := .empty
    for _ in [:20] do b := b.push 0x0b
    return b
  expect "rfc4231-tc1"
    (Hmac.sha256Hex k1 "Hi There".toUTF8)
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"

  IO.println ""
  IO.println "── PBKDF2-HMAC-SHA256 (RFC 6070-shape vector) ─────────────"
  let dk := Pbkdf2.derive "password".toUTF8 "salt".toUTF8 1 32
  expect "iter=1, dkLen=32"
    (Sha256.toHex dk)
    "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"

  IO.println ""
  IO.println "── Base64 round-trips ─────────────────────────────────────"
  let bs : ByteArray := "any carnal pleasure.".toUTF8
  expect "encode"
    (Base64.encode bs)
    "YW55IGNhcm5hbCBwbGVhc3VyZS4="
  let decoded := (Base64.decode "YW55IGNhcm5hbCBwbGVhc3VyZS4=").getD .empty
  expect "decode"
    (String.fromUTF8! decoded)
    "any carnal pleasure."
  -- URL-safe variant skips padding.
  expect "encodeUrl"
    (Base64.encodeUrl "any+carnal/pleasure?".toUTF8)
    "YW55K2Nhcm5hbC9wbGVhc3VyZT8"

  IO.println ""
  IO.println "── Password hash/verify (PBKDF2 storage format) ───────────"
  -- Use a low iteration count for the smoke run; production should
  -- use `Password.defaultIterations` or higher.
  let stored ← Password.hash "correct horse battery staple" 1000
  IO.println s!"  stored: {stored.take 80}…"
  if Password.verify "correct horse battery staple" stored then
    IO.println "  ✓ verify accepts the matching password"
  else
    IO.println "  ✗ verify rejected the matching password"
  if !(Password.verify "wrong" stored) then
    IO.println "  ✓ verify rejects a wrong password"
  else
    IO.println "  ✗ verify accepted a wrong password"
  if Password.needsRehash stored 100000 then
    IO.println "  ✓ needsRehash flags low iteration count"
  else
    IO.println "  ✗ needsRehash failed to flag a weak hash"

  IO.println ""
  IO.println "── JWT HS256 sign + verify ────────────────────────────────"
  let claims := Lean.Json.mkObj [
    ("sub", Lean.Json.str "user-42"),
    ("iss", Lean.Json.str "leantea-test"),
    ("exp", Lean.Json.num (2_000_000_000 : Int))]
  let secret := "hunter2-rotate-me"
  let token := Jwt.signHS256 claims secret
  IO.println s!"  signed: {token.take 80}…"
  match Jwt.verifyHS256 token secret 1_700_000_000
          { algs := [.hs256], issuer := some "leantea-test" } with
  | .ok c =>
    IO.println s!"  ✓ verify ok, sub={(c.body.getObjVal? "sub").toOption}"
  | .error e => IO.println s!"  ✗ verify failed: {e}"
  -- Wrong secret → signature mismatch
  match Jwt.verifyHS256 token "wrong-secret" 1_700_000_000 {} with
  | .error e => IO.println s!"  ✓ rejected wrong secret ({e})"
  | .ok _    => IO.println s!"  ✗ accepted wrong secret"
  -- alg=none must be rejected even if explicitly in allow-list?
  -- We still reject because signing input had a real signature; the
  -- key check is that `none` not being in the default allow-list
  -- prevents the classic "forged header" attack.
  let forged :=
    Base64.encodeUrl "{\"alg\":\"none\",\"typ\":\"JWT\"}".toUTF8 ++ "." ++
    Base64.encodeUrl "{\"sub\":\"attacker\"}".toUTF8 ++ "."
  match Jwt.verifyHS256 forged secret 1_700_000_000 {} with
  | .error e => IO.println s!"  ✓ rejected alg=none forgery ({e})"
  | .ok _    => IO.println s!"  ✗ accepted alg=none forgery"
  -- Issuer mismatch
  match Jwt.verifyHS256 token secret 1_700_000_000
          { issuer := some "other" } with
  | .error e => IO.println s!"  ✓ rejected wrong issuer ({e})"
  | .ok _    => IO.println s!"  ✗ accepted wrong issuer"

  IO.println ""
  IO.println "── SAML assertion parsing (no signature verify) ───────────"
  -- A minimal but realistic-shaped SAML assertion. We only test the
  -- pure-Lean XML extractor here; signature check is delegated to
  -- xmlsec1 and exercised via integration tests.
  let samlXml :=
    "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">" ++
    "<saml:Assertion ID=\"a1\" xmlns:saml=\"urn:oasis:names:tc:SAML:2.0:assertion\">" ++
    "<saml:Issuer>https://idp.example.com</saml:Issuer>" ++
    "<saml:Subject>" ++
    "<saml:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\">alice@example.com</saml:NameID>" ++
    "</saml:Subject>" ++
    "<saml:Conditions NotBefore=\"2026-06-17T00:00:00Z\" NotOnOrAfter=\"2026-06-17T01:00:00Z\">" ++
    "<saml:AudienceRestriction>" ++
    "<saml:Audience>https://sp.example.com</saml:Audience>" ++
    "</saml:AudienceRestriction>" ++
    "</saml:Conditions>" ++
    "<saml:AttributeStatement>" ++
    "<saml:Attribute Name=\"groups\">" ++
    "<saml:AttributeValue>engineers</saml:AttributeValue>" ++
    "<saml:AttributeValue>oncall</saml:AttributeValue>" ++
    "</saml:Attribute>" ++
    "</saml:AttributeStatement>" ++
    "</saml:Assertion>" ++
    "</samlp:Response>"
  match LeanTea.Auth.Saml.parseResponse samlXml with
  | .error e => IO.println s!"  ✗ SAML parse: {e}"
  | .ok a =>
    expect "issuer" a.issuer "https://idp.example.com"
    expect "nameId" a.nameId "alice@example.com"
    expect "nameIdFormat" a.nameIdFormat
           "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"
    expect "notBefore" a.notBefore "2026-06-17T00:00:00Z"
    expect "notOnOrAfter" a.notOnOrAfter "2026-06-17T01:00:00Z"
    expect "audience" (a.audiences.headD "") "https://sp.example.com"
    match a.attributes.head? with
    | some attr =>
      expect "attr name" attr.name "groups"
      expect "attr values" (String.intercalate "," attr.values)
             "engineers,oncall"
    | none => IO.println "  ✗ no attributes parsed"

  IO.println ""
  IO.println "── Native FFI parity vs. pure Lean ────────────────────────"
  if Native.isAvailable then
    IO.println "  ✓ libcrypto FFI compiled in"
    -- SHA-256 cross-check
    let nativeShaAbc ← Native.sha256 "abc".toUTF8
    expect "native SHA-256 abc"
      (Sha256.toHex nativeShaAbc)
      (Sha256.hashHex "abc")
    -- HMAC-SHA256 cross-check
    let k20 : ByteArray := Id.run do
      let mut b : ByteArray := .empty
      for _ in [:20] do b := b.push 0x0b
      return b
    let nativeMac ← Native.hmacSha256 k20 "Hi There".toUTF8
    expect "native HMAC-SHA256"
      (Sha256.toHex nativeMac)
      (Hmac.sha256Hex k20 "Hi There".toUTF8)
    -- PBKDF2 cross-check (iter=1 to keep both paths fast)
    let nativePbk ← Native.pbkdf2Sha256 "password".toUTF8 "salt".toUTF8 1 32
    expect "native PBKDF2 iter=1"
      (Sha256.toHex nativePbk)
      (Sha256.toHex (Pbkdf2.derive "password".toUTF8 "salt".toUTF8 1 32))
  else
    IO.println "  · libcrypto FFI not compiled in (rebuild with LEANTEA_CRYPTO=1)"

  IO.println ""
  IO.println "── Security helpers (XSS / SQLi) ──────────────────────────"
  match LeanTea.Auth.Security.safeUrl "javascript:alert(1)" with
  | none => IO.println "  ✓ safeUrl rejects javascript: scheme"
  | some _ => IO.println "  ✗ safeUrl let javascript: through"
  match LeanTea.Auth.Security.safeUrl "https://example.com/x" with
  | some _ => IO.println "  ✓ safeUrl accepts https://"
  | none   => IO.println "  ✗ safeUrl rejected https"
  match LeanTea.Auth.Security.safeUrl "/relative/path" with
  | some _ => IO.println "  ✓ safeUrl accepts relative path"
  | none   => IO.println "  ✗ safeUrl rejected relative path"
  -- SQL param checker
  let goodCheck := LeanTea.Auth.Security.checkParams
    "SELECT * FROM u WHERE id = ? AND name = ?" #["7", "alice"]
  if goodCheck.ok then
    IO.println s!"  ✓ checkParams accepts matching count ({goodCheck.placeholders}=={goodCheck.params})"
  else
    IO.println s!"  ✗ checkParams rejected matching counts"
  let badCheck := LeanTea.Auth.Security.checkParams
    "SELECT * FROM u WHERE id = ? AND name = ?" #["7"]
  if !badCheck.ok then
    IO.println s!"  ✓ checkParams flags mismatch ({badCheck.placeholders} vs {badCheck.params})"
  else
    IO.println s!"  ✗ checkParams missed an injection-shaped mismatch"

  IO.println ""
  IO.println "ok"
