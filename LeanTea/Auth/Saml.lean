import LeanTea.Crypto.Base64
import LeanTea.Auth
import Std.Data.HashMap
import Std.Time

/-! # LeanTea.Auth.Saml — SP-side SAML 2.0 (HTTP-POST binding)

Just enough SAML to act as a Service Provider behind an IdP like
Azure AD / Okta / Keycloak. We handle:

  * Base64-decoding the `SAMLResponse` form field
  * Extracting `<saml:Assertion>` + `<NameID>` + `<AttributeStatement>`
  * Validating `<Conditions NotBefore="…" NotOnOrAfter="…" Audience="…">`,
    the bearer `<SubjectConfirmationData NotOnOrAfter="…" Recipient="…"
    InResponseTo="…">`, and that an assertion ID is accepted only once
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
  /-- Whether the subject has a bearer `SubjectConfirmation` (the Web
      Browser SSO profile requires one). The next three fields are its
      `SubjectConfirmationData` attributes, empty when absent. -/
  bearer              : Bool := false
  confirmNotOnOrAfter : String := ""
  confirmRecipient    : String := ""
  confirmInResponseTo : String := ""
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
      | some inside => inside.trimAscii.toString
      | none        => afterOpen.trimAscii.toString

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
  -- `<SubjectConfirmationData …/>` is usually self-closing, so read the
  -- attributes off its opener rather than looking for a closing tag.
  let bearer := (subj.splitOn "urn:oasis:names:tc:SAML:2.0:cm:bearer").length > 1
  let confirm :=
    ((afterFirst subj "<saml:SubjectConfirmationData ") <|> (afterFirst subj "<SubjectConfirmationData "))
      |>.bind (beforeFirst · ">") |>.getD ""
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
           audiences, attributes, rawXml := assertBlock, bearer,
           confirmNotOnOrAfter := attrOf confirm "NotOnOrAfter",
           confirmRecipient := attrOf confirm "Recipient",
           confirmInResponseTo := attrOf confirm "InResponseTo" }

/-! ## Times -/

private def digits (cs : List Char) : Option Nat :=
  if cs.isEmpty || !cs.all Char.isDigit then none else (String.ofList cs).toNat?

/-- Days from 1970-01-01 to the given proleptic Gregorian date. -/
private def daysFromCivil (y m d : Int) : Int :=
  let y := if m ≤ 2 then y - 1 else y
  let era := (if y ≥ 0 then y else y - 399) / 400
  let yoe := y - era * 400
  let doy := (153 * (m + (if m > 2 then -3 else 9)) + 2) / 5 + d - 1
  let doe := yoe * 365 + yoe / 4 - yoe / 100 + doy
  era * 146097 + doe - 719468

/-- Seconds since the Unix epoch for an `xs:dateTime` with a zone, as
    SAML writes them: `2026-11-24T09:30:00Z`, optionally with a fraction
    (dropped) or a `±HH:MM` offset. `none` for anything else, including
    a time without a zone. Public for testing. -/
def parseInstant (s : String) : Option Int := do
  let cs := s.toList
  guard (cs.length ≥ 20)
  guard (cs[4]? == some '-' && cs[7]? == some '-' && cs[10]? == some 'T' &&
         cs[13]? == some ':' && cs[16]? == some ':')
  let y ← digits (cs.take 4)
  let mo ← digits ((cs.drop 5).take 2)
  let d ← digits ((cs.drop 8).take 2)
  let h ← digits ((cs.drop 11).take 2)
  let mi ← digits ((cs.drop 14).take 2)
  let sec ← digits ((cs.drop 17).take 2)
  guard (1 ≤ mo && mo ≤ 12 && 1 ≤ d && d ≤ 31 && h ≤ 23 && mi ≤ 59 && sec ≤ 60)
  let rest := cs.drop 19
  let rest := match rest with
    | '.' :: frac => frac.dropWhile Char.isDigit
    | r => r
  let offset ← match rest with
    | ['Z'] => some (0 : Int)
    | [sign, h1, h2, ':', m1, m2] =>
      let oh ← digits [h1, h2]
      let om ← digits [m1, m2]
      guard (oh ≤ 23 && om ≤ 59)
      if sign == '+' then some (Int.ofNat (oh * 3600 + om * 60))
      else if sign == '-' then some (-Int.ofNat (oh * 3600 + om * 60))
      else none
    | _ => none
  let days := daysFromCivil y mo d
  return days * 86400 + h * 3600 + mi * 60 + sec - offset

/-! ## Signature verification via xmlsec1 -/

/-- `--verify` plus the options that pin it to the configured
    certificate: keys only from `--pubkey-cert-pem` (never a `KeyValue`
    or certificate carried in the document) and references only within
    the document (no external URIs fetched). xmlsec1 1.3 also needs
    `--lax-key-search` to use a key given without a name; 1.2 does not
    know that option, so it is added by version. -/
def xmlsecVerifyArgs : IO (Array String) := do
  let out ← IO.Process.output { cmd := "xmlsec1", args := #["--version"] }
  -- "xmlsec1 1.3.7 (openssl)"
  let ver := (out.stdout.splitOn " ")[1]? |>.getD ""
  let nums := (ver.splitOn ".").map (·.toNat?.getD 0)
  let atLeast13 := match nums with
    | major :: minor :: _ => major > 1 || (major == 1 && minor ≥ 3)
    | _ => false
  return #["--verify"] ++ (if atLeast13 then #["--lax-key-search"] else #[]) ++
    #["--enabled-key-data", "raw-x509-cert", "--enabled-reference-uris", "empty,same-doc"]

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
      args := (← xmlsecVerifyArgs) ++ #["--id-attr:ID", "Assertion",
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
  /-- This SP's Assertion Consumer Service URL. When set, the bearer
      confirmation's `Recipient` must equal it, so an assertion the IdP
      issued for another SP's endpoint is refused. -/
  acsUrl          : String := ""
  /-- The IdP's entity ID. When set, the assertion's `Issuer` must equal it. -/
  idpEntityId     : String := ""
  deriving Inhabited, Repr

/-! ## Replay

A bearer assertion is a password for its lifetime: whoever replays the
POST signs in. `ReplayCache` remembers each accepted assertion ID until
its `NotOnOrAfter` (plus leeway) has passed, and refuses it a second
time. `validate` uses one per process by default; servers behind a load
balancer need a cache shared between them (pass their own). -/

structure ReplayCache where
  seen : IO.Ref (Std.HashMap String Int)
  deriving Nonempty

def ReplayCache.new : IO ReplayCache := return { seen := ← IO.mkRef {} }

/-- Record `id` as used until `expiresAt` (Unix seconds). `false` when it is
    already recorded and not yet expired. Expired entries are dropped. -/
def ReplayCache.claim (c : ReplayCache) (id : String) (expiresAt now : Int) : IO Bool :=
  c.seen.modifyGet fun m =>
    let m := m.filter fun _ t => t > now
    if m.contains id then (false, m) else (true, m.insert id expiresAt)

initialize processReplayCache : ReplayCache ← ReplayCache.new

/-- Wall-clock seconds since the Unix epoch. -/
def nowSeconds : IO Int := do
  return (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val

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
  -- Every start tag whose local name is `Assertion`, whatever its prefix
  -- (`saml:`, `saml2:`, none …): an IdP that writes `saml2:` must not let
  -- a forged `<saml:Assertion>` go uncounted.
  (xml.splitOn "<").drop 1 |>.countP fun tag =>
    let name := (tag.takeWhile fun c => !(c == ' ' || c == '>' || c == '/' ||
                                          c == '\t' || c == '\n' || c == '\r')).toString
    name == "Assertion" || name.endsWith ":Assertion"

/-- `xml` without `<!-- … -->` comments, or an error when it has an
    unterminated comment, a DOCTYPE / entity declaration or a CDATA
    section. Signature verification ignores comments and reads CDATA as
    text, while the string parser here would see markup in both, so a
    forged `<Assertion>` could hide there. SAML responses need none of
    them. -/
def stripComments (xml : String) : Except String String := do
  if (xml.splitOn "<!DOCTYPE").length > 1 || (xml.splitOn "<!ENTITY").length > 1 then
    throw "SAML: DOCTYPE / ENTITY declarations are not accepted"
  if (xml.splitOn "<![CDATA[").length > 1 then
    throw "SAML: CDATA sections are not accepted"
  match xml.splitOn "<!--" with
  | [] => return xml
  | first :: rest =>
    let mut out := first
    for part in rest do
      match part.splitOn "-->" with
      | _ :: after :: more => out := out ++ String.intercalate "-->" (after :: more)
      | _ => throw "SAML: unterminated comment"
    return out

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

/-- Everything `validate` checks after the signature, at time `now`
    (Unix seconds), on the decoded response `xml`:

    * the XSW invariants (`checkNoWrapping`), before any field is trusted;
    * `Issuer` (when `cfg.idpEntityId` is set) and the audience;
    * `Conditions`: `NotOnOrAfter` is required and must be in the
      future, `NotBefore` (when present) in the past, both within
      `cfg.leewaySeconds`;
    * a bearer `SubjectConfirmationData` whose `NotOnOrAfter` is in the
      future, whose `Recipient` is `cfg.acsUrl` (when set), and whose
      `InResponseTo` is `inResponseTo` (when the SP sent the request;
      `none` accepts IdP-initiated responses).

    Returns the assertion and the time until which its ID must not be
    accepted again. Pure, so it can be tested at fixed clocks. -/
def checkAssertion (cfg : Config) (now : Int) (xml : String)
    (inResponseTo : Option String := none) : Except String (Assertion × Int) := do
  let xml ← stripComments xml
  let a ← parseResponse xml
  checkNoWrapping xml a
  if !cfg.idpEntityId.isEmpty && a.issuer != cfg.idpEntityId then
    throw s!"SAML: issuer {a.issuer}, want {cfg.idpEntityId}"
  if !a.audiences.contains cfg.spEntityId then
    throw s!"audience mismatch: assertion lists {a.audiences}, want {cfg.spEntityId}"
  let leeway : Int := cfg.leewaySeconds
  let some expiresAt := parseInstant a.notOnOrAfter
    | throw s!"SAML: Conditions NotOnOrAfter missing or unreadable ({a.notOnOrAfter})"
  if now ≥ expiresAt + leeway then throw s!"SAML: assertion expired at {a.notOnOrAfter}"
  if !a.notBefore.isEmpty then
    let some from_ := parseInstant a.notBefore
      | throw s!"SAML: Conditions NotBefore unreadable ({a.notBefore})"
    if now + leeway < from_ then throw s!"SAML: assertion not valid before {a.notBefore}"
  if !a.bearer then throw "SAML: no bearer SubjectConfirmation"
  let some confirmUntil := parseInstant a.confirmNotOnOrAfter
    | throw s!"SAML: SubjectConfirmationData NotOnOrAfter missing or unreadable ({a.confirmNotOnOrAfter})"
  if now ≥ confirmUntil + leeway then
    throw s!"SAML: subject confirmation expired at {a.confirmNotOnOrAfter}"
  if !cfg.acsUrl.isEmpty && a.confirmRecipient != cfg.acsUrl then
    throw s!"SAML: Recipient {a.confirmRecipient}, want {cfg.acsUrl}"
  if let some r := inResponseTo then
    if a.confirmInResponseTo != r then
      throw s!"SAML: InResponseTo {a.confirmInResponseTo}, want {r}"
  return (a, max expiresAt confirmUntil + leeway)

/-- Decode the form value, verify the signature, run `checkAssertion`
    at the current time, then refuse an assertion ID seen before
    (`replay`, by default one cache for the whole process). Pass
    `inResponseTo` = the ID of the `AuthnRequest` this browser was sent
    with, when the SP keeps it. -/
def validate (cfg : Config) (samlResponseB64 : String)
    (inResponseTo : Option String := none) (replay : ReplayCache := processReplayCache)
    : IO (Except String Assertion) := do
  match decodeSAMLResponse samlResponseB64 with
  | .error e => return .error e
  | .ok xml =>
    match ← verifySignature xml cfg.idpCertPemPath with
    | .error e => return .error e
    | .ok _ =>
      let now ← nowSeconds
      match checkAssertion cfg now xml inResponseTo with
      | .error e => return .error e
      | .ok (a, expiresAt) =>
        if ← replay.claim a.id expiresAt now then return .ok a
        else return .error s!"SAML: assertion {a.id} was already used"

end LeanTea.Auth.Saml
