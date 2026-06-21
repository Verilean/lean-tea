import LeanTea
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

/-! # examples/Tests/PureSpec.lean — pure-Lean subsystems in one binary.

Consolidates the template + crypto smokes into a single LSpec runner.

  * Template — `{{var}}` / `{{#each}}` / `{{#if}}` / `{{#include}}`
  * Crypto — FIPS 180-4 SHA-256, RFC 4231 HMAC, PBKDF2-HMAC-SHA256
    RFC 6070 vectors, Base64 round-trips, Password hash/verify, JWT
    HS256 sign+verify (including alg=none forgery reject), SAML
    parsing, native libcrypto FFI parity, `Auth.Security.safeUrl` +
    `checkParams`.

Same CI shape as `persist_spec` / `security_spec`: one binary, one
step, exit non-zero on any failure. -/

open LeanTea LeanTea.LSpec

/-! ## Group 1 — Template engine (was `template_smoke`). -/

namespace TemplateGroup

open LeanTea.Template

def flatSrc    : String := "Hello, {{name}}! You have {{count}} new messages."
def listSrc    : String := "<ul>{{#each fruits}}<li>{{this}}</li>{{/each}}</ul>"
def dictSrc    : String :=
  "<ul>{{#each users}}<li>{{name}} ({{age}})</li>{{/each}}</ul>"
def ifSrc      : String :=
  "{{#if banner}}<b>{{banner}}</b>{{else}}<em>no banner</em>{{/if}}"
def includeSrc : String :=
  "[main start] {{#include \"examples/Smoke/fixtures/partial.html\"}} [main end]"

def run : IO LSpec := do
  let flat    ← (parse flatSrc).renderFlat [("name", "Junji"), ("count", "3")]
  let listOut ← (parse listSrc).render
    [("fruits", .list [.str "apple", .str "banana", .str "cherry"])]
  let dictOut ← (parse dictSrc).render
    [("users", .list [
      .dict [("name", .str "Alice"), ("age", .str "30")],
      .dict [("name", .str "Bob"),   ("age", .str "25")]
    ])]
  let ifTrue   ← (parse ifSrc).renderFlat [("banner", "compile error")]
  let ifEmpty  ← (parse ifSrc).renderFlat [("banner", "")]
  let ifAbsent ← (parse ifSrc).renderFlat []
  let inc      ← (parse includeSrc).renderFlat [("name", "Partial-san")]

  return group "Template engine" [
    it "flat substitution"
      (flat == "Hello, Junji! You have 3 new messages."),
    it "#each over string list" (listOut.endsWith "</ul>" && listOut.startsWith "<ul>"
                                  && (listOut.splitOn "<li>").length == 4),
    it "#each over dict list — Alice"  ((dictOut.splitOn "Alice (30)").length == 2),
    it "#each over dict list — Bob"    ((dictOut.splitOn "Bob (25)").length == 2),
    it "#if truthy → then branch"      ((ifTrue.splitOn "<b>compile error</b>").length == 2),
    it "#if empty string → else"       ((ifEmpty.splitOn "no banner").length == 2),
    it "#if absent key → else"         ((ifAbsent.splitOn "no banner").length == 2),
    it "#include splices a partial"
      ((inc.splitOn "main start").length == 2
        && (inc.splitOn "main end").length == 2
        && inc != "[main start]  [main end]")
  ]

end TemplateGroup

/-! ## Group 2 — Crypto known-answer + FFI parity (was `crypto_smoke`). -/

namespace CryptoGroup

open LeanTea.Crypto

private def k1 : ByteArray := Id.run do
  let mut b : ByteArray := .empty
  for _ in [:20] do b := b.push 0x0b
  return b

private def forgedToken : String :=
  Base64.encodeUrl "{\"alg\":\"none\",\"typ\":\"JWT\"}".toUTF8 ++ "." ++
  Base64.encodeUrl "{\"sub\":\"attacker\"}".toUTF8 ++ "."

private def samlXml : String :=
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

def run : IO LSpec := do
  /- Password — needs IO for random salt. -/
  let stored ← Password.hash "correct horse battery staple" 1000
  let acceptsRight := Password.verify "correct horse battery staple" stored
  let rejectsWrong := !(Password.verify "wrong" stored)
  let flagsRehash  := Password.needsRehash stored 100000

  /- JWT — verifyHS256 is pure but lives behind `Except`. -/
  let claims := Lean.Json.mkObj [
    ("sub", Lean.Json.str "user-42"),
    ("iss", Lean.Json.str "leantea-test"),
    ("exp", Lean.Json.num (2_000_000_000 : Int))]
  let secret := "hunter2-rotate-me"
  let token := Jwt.signHS256 claims secret
  let jwtOK    := Jwt.verifyHS256 token secret 1_700_000_000
                  { algs := [.hs256], issuer := some "leantea-test" }
  let jwtBadKey := Jwt.verifyHS256 token "wrong-secret" 1_700_000_000 {}
  let jwtNoneAlg := Jwt.verifyHS256 forgedToken secret 1_700_000_000 {}
  let jwtBadIss  := Jwt.verifyHS256 token secret 1_700_000_000
                    { issuer := some "other" }

  /- SAML — pure parse. -/
  let saml := LeanTea.Auth.Saml.parseResponse samlXml

  /- Native FFI — optional. -/
  let nativeOn := Native.isAvailable
  let nativeSha ← if nativeOn then Native.sha256 "abc".toUTF8 else pure .empty
  let nativeMac ← if nativeOn then Native.hmacSha256 k1 "Hi There".toUTF8 else pure .empty
  let nativePbk ← if nativeOn then Native.pbkdf2Sha256 "password".toUTF8 "salt".toUTF8 1 32
                  else pure .empty

  let baseGroups : List LSpec := [
    group "SHA-256 (FIPS 180-4 vectors)" [
      it "empty"
        (Sha256.hashHex "" ==
          "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
      it "abc"
        (Sha256.hashHex "abc" ==
          "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
      it "56-byte boundary"
        (Sha256.hashHex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq" ==
          "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
    ],
    group "HMAC-SHA256 (RFC 4231 tc1)" [
      it "20-byte 0x0b key, 'Hi There'"
        (Hmac.sha256Hex k1 "Hi There".toUTF8 ==
          "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
    ],
    group "PBKDF2-HMAC-SHA256 (RFC 6070-shape)" [
      it "iter=1, dkLen=32"
        (Sha256.toHex (Pbkdf2.derive "password".toUTF8 "salt".toUTF8 1 32) ==
          "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b")
    ],
    group "Base64 round-trips" [
      it "encode"
        (Base64.encode "any carnal pleasure.".toUTF8 == "YW55IGNhcm5hbCBwbGVhc3VyZS4="),
      it "decode"
        (String.fromUTF8! ((Base64.decode "YW55IGNhcm5hbCBwbGVhc3VyZS4=").getD .empty) ==
          "any carnal pleasure."),
      it "encodeUrl drops padding"
        (Base64.encodeUrl "any+carnal/pleasure?".toUTF8 == "YW55K2Nhcm5hbC9wbGVhc3VyZT8")
    ],
    group "Password hash/verify (PBKDF2 storage format)" [
      it "verify accepts matching password" acceptsRight,
      it "verify rejects wrong password"    rejectsWrong,
      it "needsRehash flags low iteration"  flagsRehash
    ],
    group "JWT HS256 sign + verify" [
      it "sign + verify ok"
        (match jwtOK with | .ok _ => true | .error _ => false),
      it "rejects wrong secret"
        (match jwtBadKey with | .ok _ => false | .error _ => true),
      it "rejects alg=none forgery"
        (match jwtNoneAlg with | .ok _ => false | .error _ => true),
      it "rejects wrong issuer"
        (match jwtBadIss with | .ok _ => false | .error _ => true)
    ],
    group "SAML 2.0 assertion parse" (
      match saml with
      | .error e => [it s!"SAML parse: {e}" false]
      | .ok a => [
          it "issuer"        (a.issuer == "https://idp.example.com"),
          it "nameId"        (a.nameId == "alice@example.com"),
          it "nameIdFormat"
            (a.nameIdFormat == "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"),
          it "notBefore"     (a.notBefore == "2026-06-17T00:00:00Z"),
          it "notOnOrAfter"  (a.notOnOrAfter == "2026-06-17T01:00:00Z"),
          it "audience"      (a.audiences.headD "" == "https://sp.example.com"),
          it "attribute name + values"
            (match a.attributes.head? with
             | some attr => attr.name == "groups"
                            && String.intercalate "," attr.values == "engineers,oncall"
             | none => false)
        ]),
    group "Auth.Security helpers (XSS / SQLi pre-flight)" [
      it "safeUrl rejects javascript: scheme"
        ((LeanTea.Auth.Security.safeUrl "javascript:alert(1)").isNone),
      it "safeUrl accepts https://"
        ((LeanTea.Auth.Security.safeUrl "https://example.com/x").isSome),
      it "safeUrl accepts relative path"
        ((LeanTea.Auth.Security.safeUrl "/relative/path").isSome),
      it "checkParams accepts matching count"
        (LeanTea.Auth.Security.checkParams
          "SELECT * FROM u WHERE id = ? AND name = ?" #["7", "alice"]).ok,
      it "checkParams flags injection-shaped mismatch"
        !(LeanTea.Auth.Security.checkParams
          "SELECT * FROM u WHERE id = ? AND name = ?" #["7"]).ok
    ]
  ]

  let nativeGroups : List LSpec :=
    if nativeOn then [
      group "Native libcrypto FFI parity (LEANTEA_CRYPTO=1)" [
        it "FFI available"            true,
        it "native SHA-256 abc matches pure Lean"
          (Sha256.toHex nativeSha == Sha256.hashHex "abc"),
        it "native HMAC-SHA256 matches pure Lean"
          (Sha256.toHex nativeMac == Hmac.sha256Hex k1 "Hi There".toUTF8),
        it "native PBKDF2 iter=1 matches pure Lean"
          (Sha256.toHex nativePbk ==
            Sha256.toHex (Pbkdf2.derive "password".toUTF8 "salt".toUTF8 1 32))
      ]
    ] else [
      group "Native libcrypto FFI parity" [
        it "skipped — rebuild with LEANTEA_CRYPTO=1 for FFI tests" true
      ]
    ]

  return group "Crypto" (baseGroups ++ nativeGroups)

end CryptoGroup

/-! ## Entry point. -/

def main : IO Unit := do
  let t ← TemplateGroup.run
  let c ← CryptoGroup.run
  let tree := group "LeanTEA pure-Lean subsystems" [t, c]
  let code ← lspecIO tree
  if code != 0 then IO.Process.exit code.toUInt8
