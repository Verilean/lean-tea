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
import Sheet.App

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
def escapeSrc  : String := "<p title=\"{{v}}\">{{v}}</p>{{{html}}}"
def listSrc    : String := "<ul>{{#each fruits}}<li>{{this}}</li>{{/each}}</ul>"
def dictSrc    : String :=
  "<ul>{{#each users}}<li>{{name}} ({{age}})</li>{{/each}}</ul>"
def ifSrc      : String :=
  "{{#if banner}}<b>{{banner}}</b>{{else}}<em>no banner</em>{{/if}}"
def includeSrc : String :=
  "[main start] {{#include \"examples/Smoke/fixtures/partial.html\"}} [main end]"

def run : IO LSpec := do
  let flat    ← (parse flatSrc).renderFlat [("name", "Junji"), ("count", "3")]
  let unclosedRaw ← (parse "a {{{b").renderFlat [("b", "x")]
  let unclosedMb ← (parse "a {{{あ").renderFlat []
  let unclosed ← (parse "a {{b c").renderFlat [("b", "x")]
  let escaped ← (parse escapeSrc).renderFlat
    [("v", "<script>alert('x')</script>\" onmouseover=\"x & y"), ("html", "<b>ok</b>")]
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
    it "an opener without a closer stays text (no panic, nothing dropped)"
      (unclosedRaw == "a {{{b" && unclosedMb == "a {{{あ" && unclosed == "a {{b c"),
    it "{{v}} is HTML-escaped in text and attributes; {{{v}}} is inserted as is"
      (escaped == "<p title=\"&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt;&quot; onmouseover=&quot;x &amp; y\">" ++
        "&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt;&quot; onmouseover=&quot;x &amp; y</p><b>ok</b>"),
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
  let signWith := fun (fields : List (String × Lean.Json)) => Jwt.signHS256 (Lean.Json.mkObj fields) secret
  let rejects := fun (r : Except String Jwt.Claims) => match r with | .ok _ => false | .error _ => true
  let noExp := signWith [("sub", Lean.Json.str "u")]
  let jwtNoExpDefault := Jwt.verifyHS256 noExp secret 1_700_000_000 {}
  let jwtNoExpRequired := Jwt.verifyHS256 noExp secret 1_700_000_000 { requireExp := true }
  let fracExp := signWith [("exp", (Lean.Json.parse "2000000000.5").toOption.getD Lean.Json.null)]
  let strExp := signWith [("exp", Lean.Json.str "2000000000")]
  let expired := signWith [("exp", Lean.Json.num (1_600_000_000 : Int))]
  let notYet := signWith [("exp", Lean.Json.num (2_000_000_000 : Int)), ("nbf", Lean.Json.num (1_800_000_000 : Int))]
  let badUtf8 := Base64.encodeUrl ⟨#[0xff, 0xfe]⟩ ++ "." ++ Base64.encodeUrl "{}".toUTF8 ++ ".x"

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
        (match jwtBadIss with | .ok _ => false | .error _ => true),
      it "a token without exp passes by default and fails with requireExp"
        (!rejects jwtNoExpDefault && rejects jwtNoExpRequired),
      it "a fractional or string exp is refused, not skipped"
        (rejects (Jwt.verifyHS256 fracExp secret 1_700_000_000 {}) &&
          rejects (Jwt.verifyHS256 strExp secret 1_700_000_000 {})),
      it "an expired token and a not-yet-valid token are refused"
        (rejects (Jwt.verifyHS256 expired secret 1_700_000_000 {}) &&
          rejects (Jwt.verifyHS256 notYet secret 1_700_000_000 {})),
      it "a segment that is not UTF-8 is an error, not a panic"
        (rejects (Jwt.verifyHS256 badUtf8 secret 1_700_000_000 {}))
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

/-! ## Hardening checks that stop before any crypto or network -/

namespace HardeningGroup

open LeanTea.Auth.Passkey in
def passkeyReq (flags : UInt8) (counter stored : UInt32) (clientData : ByteArray) : AuthenticateRequest :=
  let rpHash := LeanTea.Crypto.Sha256.hashString "example.com"
  let ad := rpHash.push flags |>.push (counter >>> 24).toUInt8 |>.push (counter >>> 16).toUInt8
            |>.push (counter >>> 8).toUInt8 |>.push counter.toUInt8
  { authenticatorData := ad, clientDataJSON := clientData, signature := .empty,
    expectedChallenge := "abc", expectedOrigin := "https://example.com", expectedRpId := "example.com",
    stored := { credId := "c", publicKeyPem := "", signCount := stored, rpId := "example.com" } }

def run : IO LSpec := do
  let cd := "{\"type\":\"webauthn.get\",\"challenge\":\"abc\",\"origin\":\"https://example.com\"}".toUTF8
  let errText := fun (r : Except String UInt32) => match r with | .ok _ => "" | .error e => e
  let has := fun (s sub : String) => (s.splitOn sub).length > 1
  let cloneZero ← LeanTea.Auth.Passkey.verifyAuthentication (passkeyReq 0x01 0 5 cd)
  let noUp ← LeanTea.Auth.Passkey.verifyAuthentication (passkeyReq 0x00 6 5 cd)
  let badUtf8 ← LeanTea.Auth.Passkey.verifyAuthentication (passkeyReq 0x01 6 5 ⟨#[0xff, 0xfe]⟩)
  let gemini : LeanTea.Cloud.Gemini.Config := { apiKey := "unused" }
  let optRev ← try
      let _ ← LeanTea.Cloud.Gemini.reviewDiff gemini "." "--output=/tmp/leantea-pwned" "HEAD"
      pure ""
    catch e => pure (toString e)
  return group "Hardening" [
    it "passkey: a counter of 0 after a non-zero count is refused (clone)"
      (has (errText cloneZero) "signCount") (errText cloneZero),
    it "passkey: an assertion without the User Present flag is refused"
      (has (errText noUp) "user presence") (errText noUp),
    it "passkey: clientDataJSON that is not UTF-8 is an error, not a panic"
      (has (errText badUtf8) "UTF-8") (errText badUtf8),
    it "reviewDiff refuses a revision that looks like a git option"
      (has optRev "may not start with '-'" && !(← System.FilePath.pathExists "/tmp/leantea-pwned")) optRev
  ]

end HardeningGroup

/-! ## Entry point. -/

/-! ## Sheet's model in the `X-Model` header -/

namespace SheetGroup

/-- What a browser does to a header value: drop spaces and tabs at the ends. -/
def trimmed (s : String) : String := s.trimAscii.toString

def roundTrips (m : Sheet.Model) : Bool :=
  match Sheet.decodeModel (trimmed (Sheet.encodeModel m)) with
  | some m' => m'.selected == m.selected && m'.cells == m.cells
  | none => false

def run : IO LSpec := do
  let empty : Sheet.Model := { selected := "B2" }
  let awkward : Sheet.Model := {
    selected := "C3"
    cells := [("A1", "6*7"), ("B2", "A1 + 1 "), ("C3", "a|b;c\td/e,f:g%h+i"), ("D4", " ")] }
  return group "Sheet model codec (X-Model header)" [
    it "an empty sheet survives header trimming" (roundTrips empty) (Sheet.encodeModel empty),
    it "values with spaces, tabs, separators, % and + round-trip" (roundTrips awkward)
      (Sheet.encodeModel awkward),
    it "the encoding never starts or ends with whitespace"
      ([empty, awkward].all fun m => trimmed (Sheet.encodeModel m) == Sheet.encodeModel m)
  ]

end SheetGroup

def main : IO Unit := do
  let t ← TemplateGroup.run
  let c ← CryptoGroup.run
  let h ← HardeningGroup.run
  let sh ← SheetGroup.run
  let tree := group "LeanTEA pure-Lean subsystems" [t, c, h, sh]
  let code ← lspecIO tree
  if code != 0 then IO.Process.exit code.toUInt8
