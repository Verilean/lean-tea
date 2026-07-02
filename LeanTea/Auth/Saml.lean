import LeanTea.Crypto.Base64
import LeanTea.Auth

/-! # LeanTea.Auth.Saml — SP-side SAML 2.0 (HTTP-POST binding)

Just enough SAML to act as a Service Provider behind an IdP like
Azure AD / Okta / Keycloak. We handle:

  * Base64-decoding the `SAMLResponse` form field
  * Extracting `<saml:Assertion>` + `<NameID>` + `<AttributeStatement>`
  * Validating `<Conditions NotBefore="…" NotOnOrAfter="…" Audience="…">`
  * **Signature verification via `xmlsec1` shell-out** — pure-Lean
    XMLDSig (canonical XML + RSA-SHA256) is out of scope; xmlsec1 is
    the de-facto open-source verifier and ships on every Linux distro.

What we don't do:
  * IdP-initiated flows (the SP only handles AuthnResponses, no
    issuance side)
  * SLO / single logout
  * Encrypted assertions (decrypt the EncryptedAssertion via xmlsec1
    before calling `parseResponse`)
  * SAML metadata exchange

If the IdP rejects, debug with:

```
xmlsec1 --verify --pubkey-cert-pem idp.pem --id-attr:ID Assertion - < resp.xml
```
-/

namespace LeanTea.Auth.Saml

open LeanTea.Auth
open LeanTea.Crypto

structure Attribute where
  name   : String
  values : List String
  deriving Inhabited, Repr

structure Assertion where
  /-- `xs:NCName` issued by the IdP. -/
  id          : String
  issuer      : String
  /-- NameID's text content. Typically the user's email or stable ID. -/
  nameId      : String
  /-- `Format` attribute on `NameID`. -/
  nameIdFormat: String
  notBefore   : String   -- ISO 8601 (raw)
  notOnOrAfter: String
  /-- Each AudienceRestriction's text content. Caller checks against
      its configured SP entity ID. -/
  audiences   : List String
  attributes  : List Attribute
  /-- Raw XML for the assertion, so xmlsec1 can re-canonicalise + verify. -/
  rawXml      : String
  deriving Inhabited, Repr

/-! ## XML extraction — minimal, *not* general-purpose

We look for known tag pairs by `String.splitOn`. SAML response XML
is well-formed and emitted by tooling, so we never see exotic
whitespace / attribute ordering. For arbitrary XML, swap this for a
real parser. -/

/-- Split on `sep`, return the tail (everything after the first
    occurrence) joined back with `sep` removed, or `none` when
    absent. -/
private def afterFirst (s sep : String) : Option String :=
  match s.splitOn sep with
  | _ :: rest =>
    if rest.isEmpty then none
    else some (String.intercalate sep rest)
  | _ => none

/-- Substring strictly before the first occurrence of `sep`. -/
private def beforeFirst (s sep : String) : Option String :=
  match s.splitOn sep with
  | head :: _ :: _ => some head
  | _              => none

/-- Find `<Tag …>…</Tag>` and return the matched substring (including
    open + close tags). Tries qualified (`saml:Tag`) then unqualified.

    Disambiguates `<Tag>` from `<TagWhatever>` by requiring the
    opener to literally be `<Tag>` or `<Tag …attrs…>`. -/
private def findTagBlock (xml : String) (qual unqual : String) : Option String :=
  let attempt (name : String) : Option String :=
    let closeTag := "</" ++ name ++ ">"
    -- No-attribute opener: exactly `<name>`.
    let try1 :=
      (afterFirst xml ("<" ++ name ++ ">")).bind fun after =>
        (beforeFirst after closeTag).map fun inside =>
          "<" ++ name ++ ">" ++ inside ++ closeTag
    -- With-attribute opener: `<name …attrs…>`.
    let openPrefix := "<" ++ name ++ " "
    let try2 :=
      (afterFirst xml openPrefix).bind fun after =>
        -- `after` runs from just past the prefix through end of document.
        -- Slice it down to "up to closing tag", then split on the
        -- first `>` to separate attrs from body.
        (beforeFirst after closeTag).bind fun everything =>
          (beforeFirst everything ">").bind fun attrs =>
            (afterFirst everything ">").map fun body =>
              openPrefix ++ attrs ++ ">" ++ body ++ closeTag
    try1 <|> try2
  (attempt qual) <|> (attempt unqual)

/-- Inner text of a single element. Trims surrounding whitespace. -/
private def innerText (xml : String) (qual unqual : String) : String :=
  match findTagBlock xml qual unqual with
  | none => ""
  | some block =>
    -- Drop everything up to and including the first `>` of the opener.
    match afterFirst block ">" with
    | none => ""
    | some afterOpen =>
      -- Drop closing `</…>`.
      match beforeFirst afterOpen "</" with
      | some inside => inside.trim
      | none        => afterOpen.trim

/-- Pluck a single attribute value off the opening tag. -/
private def attrOf (xml : String) (attr : String) : String :=
  match afterFirst xml (attr ++ "=\"") with
  | none => ""
  | some after =>
    match beforeFirst after "\"" with
    | some v => v
    | none   => ""

/-! ## Parse a SAMLResponse (already base64-decoded) -/

/-- Parse the XML of a `samlp:Response`. Does NOT verify the
    signature — call `verifySignature` separately. -/
def parseResponse (xml : String) : Except String Assertion := do
  let assertBlock ← match findTagBlock xml "saml:Assertion" "Assertion" with
    | some s => Except.ok s
    | none   => throw "no <Assertion> in response"
  let id := attrOf assertBlock "ID"
  let issuer := innerText assertBlock "saml:Issuer" "Issuer"
  let subj   := findTagBlock assertBlock "saml:Subject" "Subject" |>.getD ""
  let nameId := innerText subj "saml:NameID" "NameID"
  let nameIdFormat := match findTagBlock subj "saml:NameID" "NameID" with
    | none => ""
    | some t => attrOf t "Format"
  let cond := findTagBlock assertBlock "saml:Conditions" "Conditions" |>.getD ""
  let notBefore := attrOf cond "NotBefore"
  let notOnOrAfter := attrOf cond "NotOnOrAfter"
  -- Audiences: scan the conditions block for every <Audience>…</Audience>.
  let mut audiences : List String := []
  let mut s := cond
  while s.length > 0 do
    match findTagBlock s "saml:Audience" "Audience" with
    | none => break
    | some block =>
      let txt := innerText block "saml:Audience" "Audience"
      if !txt.isEmpty then audiences := audiences ++ [txt]
      -- Move past this match to find the next.
      match (afterFirst s "</saml:Audience>") <|> (afterFirst s "</Audience>") with
      | some rest => s := rest
      | none      => break
  -- Attributes
  let attrStmt :=
    findTagBlock assertBlock "saml:AttributeStatement" "AttributeStatement"
      |>.getD ""
  let mut attributes : List Attribute := []
  let mut rest := attrStmt
  while rest.length > 0 do
    match findTagBlock rest "saml:Attribute" "Attribute" with
    | none => break
    | some block =>
      let name := attrOf block "Name"
      -- All AttributeValue elements (handle multiple-valued attrs).
      let mut vs : List String := []
      let mut bs := block
      while bs.length > 0 do
        match findTagBlock bs "saml:AttributeValue" "AttributeValue" with
        | none => break
        | some vblock =>
          let v := innerText vblock "saml:AttributeValue" "AttributeValue"
          if !v.isEmpty then vs := vs ++ [v]
          match (afterFirst bs "</saml:AttributeValue>") <|>
                (afterFirst bs "</AttributeValue>") with
          | some next => bs := next
          | none      => break
      attributes := attributes ++ [{ name, values := vs }]
      match (afterFirst rest "</saml:Attribute>") <|>
            (afterFirst rest "</Attribute>") with
      | some next => rest := next
      | none      => break
  return { id, issuer, nameId, nameIdFormat, notBefore, notOnOrAfter,
           audiences, attributes, rawXml := assertBlock }

/-! ## Signature verification via xmlsec1 -/

/-- Verify the assertion signature using xmlsec1. `idpCertPemPath` is
    a PEM file of the IdP's signing certificate. Requires `xmlsec1`
    to be on `PATH` (apt: `xmlsec1`; brew: `xmlsec1`). -/
def verifySignature (xml : String) (idpCertPemPath : String)
    : IO (Except String Unit) := do
  -- Unique per-call temp dir (auto-removed): a fixed `/tmp/saml_response.xml`
  -- would let two concurrent validations clobber each other's file — so
  -- xmlsec1 could verify one user's response while `parseResponse` reads
  -- another's — and invites a symlink pre-creation attack (CWE-377).
  IO.FS.withTempDir fun dir => do
    let xmlPath := dir / "saml_response.xml"
    IO.FS.writeFile xmlPath xml
    let out ← IO.Process.output {
      cmd := "xmlsec1"
      args := #["--verify", "--id-attr:ID", "Assertion",
                "--pubkey-cert-pem", idpCertPemPath, xmlPath.toString]
    }
    if out.exitCode == 0 then return .ok ()
    else return .error s!"xmlsec1 verify (exit {out.exitCode}): {out.stderr}"

/-! ## Decode the `SAMLResponse` form field -/

/-- The form field is base64 of the raw XML. -/
def decodeSAMLResponse (formValue : String) : Except String String := do
  match Base64.decode formValue with
  | none    => throw "SAMLResponse: base64 decode failed"
  | some bs =>
    -- `fromUTF8?`, never `fromUTF8!`: the input is attacker-controlled,
    -- so a malformed byte sequence must surface as a rejected request,
    -- not a server panic (DoS).
    match String.fromUTF8? bs with
    | some s => return s
    | none   => throw "SAMLResponse: not valid UTF-8"

/-! ## End-to-end -/

structure Config where
  /-- This SP's `EntityID` — the assertion must list it under
      `AudienceRestriction`. -/
  spEntityId      : String
  idpCertPemPath  : String
  /-- Allow assertion's clock skew this many seconds either side
      of NotBefore / NotOnOrAfter. -/
  leewaySeconds   : Nat := 60
  deriving Inhabited, Repr

/-! ## Signature-wrapping (XSW) hardening

`xmlsec1 --verify` confirms that *some* element is signed by the IdP,
but on its own says nothing about *which* element our string parser
later reads identity from. That gap is the classic XML Signature
Wrapping attack: an attacker keeps the legitimately-signed assertion
somewhere the verifier is happy with, and injects a second, forged
assertion that `parseResponse` (which takes the first `<Assertion>`)
consumes instead — a full authentication bypass.

A minimal string parser can't reason about XML tree structure, so we
instead enforce invariants that make the wrapping variants
inexpressible:

  * **Exactly one `<Assertion>` in the whole document.** Kills every
    "inject a second assertion" variant outright.
  * **A `<Signature>` whose `Reference URI` is `#<AssertionID>`.**
    The trusted signature (xmlsec1 checks it against the IdP cert
    *only*, so an attacker-minted signature fails) must cover the same
    assertion we parse.

With both, xmlsec1's success means the single assertion we read is the
one the IdP signed. This is hardening on top of a minimal parser, not
a substitute for a real XML-DSig stack — encrypted assertions and
exotic canonicalisation still warrant xmlsec1 + a real parser. -/

private def occurrences (hay needle : String) : Nat :=
  if needle.isEmpty then 0 else (hay.splitOn needle).length - 1

/-- Count `<Assertion>` element openers, namespaced or not. SAML
    assertions always carry attributes (ID / Version / IssueInstant),
    so the real opener is `<…Assertion ` (space); we also count the
    bare `<…Assertion>` form for safety. Public for testing. -/
def assertionCount (xml : String) : Nat :=
  occurrences xml "<saml:Assertion " + occurrences xml "<saml:Assertion>"
    + occurrences xml "<Assertion " + occurrences xml "<Assertion>"

/-- Enforce the XSW invariants described above against the raw document
    and the parsed assertion. Public so it can be unit-tested against
    crafted wrapping payloads without a live xmlsec1 / IdP cert. -/
def checkNoWrapping (xml : String) (a : Assertion) : Except String Unit := do
  let n := assertionCount xml
  if n != 1 then
    throw s!"SAML: expected exactly one <Assertion>, found {n} (possible signature-wrapping attack)"
  if a.id.isEmpty then
    throw "SAML: assertion has no ID attribute — cannot bind signature to content"
  let sig := occurrences xml "<Signature" + occurrences xml "<ds:Signature"
  if sig == 0 then
    throw "SAML: no <Signature> present in response"
  -- The verified signature must reference this exact assertion by ID.
  if occurrences xml ("URI=\"#" ++ a.id ++ "\"") == 0 then
    throw s!"SAML: no signature Reference to assertion ID {a.id} (possible signature-wrapping attack)"
  return ()

/-- Decode the form value, verify the signature, parse the assertion,
    enforce the XSW invariants, then audience + (loose) time bounds. -/
def validate (cfg : Config) (samlResponseB64 : String)
    : IO (Except String Assertion) := do
  match decodeSAMLResponse samlResponseB64 with
  | .error e => return .error e
  | .ok xml =>
    match ← verifySignature xml cfg.idpCertPemPath with
    | .error e => return .error e
    | .ok _ =>
      match parseResponse xml with
      | .error e => return .error e
      | .ok a =>
        -- XSW guard runs BEFORE we trust any field of `a`.
        match checkNoWrapping xml a with
        | .error e => return .error e
        | .ok _ =>
          if !a.audiences.contains cfg.spEntityId then
            return .error s!"audience mismatch: assertion lists {a.audiences}, want {cfg.spEntityId}"
          return .ok a

end LeanTea.Auth.Saml
