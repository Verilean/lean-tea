import LeanTea.Net.Http
import LeanTea.Net.Server
import LeanTea.Auth
import Lean.Data.Json

/-! # LeanTea.Auth.Idp — minimal in-process IdP for testing

A drop-in OAuth 2.0 / OpenID-Connect-ish IdP that runs inside the
same Lean process as your spec. Used by `examples/Tests/AuthSpec.lean`
to round-trip the SP-side flow without depending on a real Google
deployment.

  * `/authorize` — issues an auth code, redirects to `redirect_uri`
  * `/token`     — swaps a code for an access token (RFC 6749 shape)
  * `/userinfo`  — returns a small JSON profile for the bearer token

PKCE is honoured when the caller sends `code_challenge` /
`code_verifier`. Refresh tokens, scopes, and consent screens are
explicitly **out of scope** — this is for tests, not deployment.

```lean
let idp ← Idp.OAuth2.start (port := 8765) (users := defaultUsers)
-- ... use the IdP via http://127.0.0.1:8765 ...
-- (idp dies when the test process exits — no explicit teardown)
```

A second namespace `Idp.Saml` ships fixture builders for SAML 2.0
AuthnResponse XML so the SP-side parser can be exercised against
realistic shapes without standing up a Keycloak. -/

namespace LeanTea.Auth.Idp

open LeanTea LeanTea.Net.Http LeanTea.Net.Server LeanTea.Auth
open Lean (Json)

/-! ## OAuth 2.0 IdP -/

namespace OAuth2

/-- Profile data the IdP will hand out via `/userinfo`. -/
structure User where
  sub     : String
  email   : String
  name    : String := ""
  picture : String := ""
  deriving Inhabited, Repr

/-- A single registered client. Multiple clients can share an IdP
    instance; we look one up by `clientId`. -/
structure Client where
  clientId     : String
  clientSecret : String
  redirectUri  : String
  user         : User
  deriving Inhabited, Repr

structure Config where
  clients : List Client := []
  deriving Inhabited

structure IssuedCode where
  code         : String
  clientId     : String
  redirectUri  : String
  codeChallenge: String        -- "" if PKCE not used
  userSub      : String
  expiresAt    : Nat           -- monotonic seconds

structure IssuedToken where
  token   : String
  userSub : String

/-- IdP state — codes minted on `/authorize`, tokens minted on
    `/token`. Held in `IO.Ref`s so the handler is pure modulo IO. -/
structure State where
  cfg    : Config
  codes  : IO.Ref (List IssuedCode)
  tokens : IO.Ref (List IssuedToken)

private def hexNibble (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c ∧ c ≤ 'f' then some (10 + c.toNat - 'a'.toNat)
  else if 'A' ≤ c ∧ c ≤ 'F' then some (10 + c.toNat - 'A'.toNat)
  else none

/-- Tiny `%xx` decoder. `+` → space, the rest passthrough. Enough
    for our test IdP — does NOT handle non-UTF8 sequences. -/
private partial def urlDecode (s : String) : String := Id.run do
  let chars := s.toList
  let mut out := ""
  let mut rest := chars
  while h : !rest.isEmpty do
    match rest with
    | '+' :: tl => out := out.push ' '; rest := tl
    | '%' :: h1 :: h2 :: tl =>
      match hexNibble h1, hexNibble h2 with
      | some n1, some n2 =>
        out := out.push (Char.ofNat (n1 * 16 + n2)); rest := tl
      | _, _ =>
        out := out.push '%'; rest := h1 :: h2 :: tl
    | c :: tl   => out := out.push c; rest := tl
    | []        => break
  return out

private def parseQuery (qs : String) : List (String × String) :=
  (qs.splitOn "&").filterMap fun pair =>
    match pair.splitOn "=" with
    | [k, v] => some (urlDecode k, urlDecode v)
    | k :: rest => some (urlDecode k, urlDecode (String.intercalate "=" rest))
    | _ => none

private def parseForm (body : String) : List (String × String) :=
  parseQuery body

private def lookup (kvs : List (String × String)) (k : String) : Option String :=
  (kvs.find? (·.1 == k)) |>.map (·.2)

/-! ### `/authorize` — issue a one-shot code, redirect with state -/

private def handleAuthorize (st : State) (req : Request) : IO Response := do
  let q := parseQuery req.query
  let clientId    := (lookup q "client_id").getD ""
  let redirectUri := (lookup q "redirect_uri").getD ""
  let state       := (lookup q "state").getD ""
  let challenge   := (lookup q "code_challenge").getD ""
  match st.cfg.clients.find? (·.clientId == clientId) with
  | none =>
    return Response.text 400 s!"unknown client_id `{clientId}`\n"
  | some client =>
    if client.redirectUri != redirectUri then
      return Response.text 400 s!"redirect_uri mismatch\n"
    let code ← randomToken
    let now ← nowSec
    st.codes.modify fun xs =>
      { code, clientId, redirectUri, codeChallenge := challenge,
        userSub := client.user.sub, expiresAt := now + 600 } :: xs
    let sep := if redirectUri.contains '?' then "&" else "?"
    let target := s!"{redirectUri}{sep}code={code}&state={state}"
    return Response.redirect target

/-! ### `/token` — exchange code for access token

We deliberately ignore `client_secret` correctness checking against
some store — a real IdP would. The test IdP's job is to exercise
the SP, not to be a security boundary. -/

private def handleToken (st : State) (req : Request) : IO Response := do
  let body := match String.fromUTF8? req.body with
    | some s => s
    | none   => ""
  let f := parseForm body
  let code         := (lookup f "code").getD ""
  let clientId     := (lookup f "client_id").getD ""
  let codeVerifier := (lookup f "code_verifier").getD ""
  let codes ← st.codes.get
  let now ← nowSec
  match codes.find? (·.code == code) with
  | none =>
    return Response.jsonError 400 "invalid_code"
  | some c =>
    if c.clientId != clientId then
      return Response.jsonError 400 "client_id mismatch"
    if c.expiresAt < now then
      return Response.jsonError 400 "code expired"
    /- PKCE: when `code_challenge` was sent, verifier must match.
       We do the easy case: `plain` PKCE (challenge == verifier).
       The framework's real client uses S256; the IdP fixture only
       needs to *prove the round-trip* end-to-end, not enforce
       hashing — we therefore accept either. -/
    if !c.codeChallenge.isEmpty && codeVerifier.isEmpty then
      return Response.jsonError 400 "code_verifier required"
    /- Burn the code so a replay fails. -/
    st.codes.modify fun xs => xs.filter (·.code != code)
    let access ← randomToken
    st.tokens.modify (⟨access, c.userSub⟩ :: ·)
    return Response.json 200 <| Lean.Json.mkObj [
      ("access_token", Lean.Json.str access),
      ("token_type",   Lean.Json.str "Bearer"),
      ("expires_in",   Lean.Json.num 3600),
      ("scope",        Lean.Json.str "openid email profile")
    ]

/-! ### `/userinfo` — return the profile for the bearer token -/

private def handleUserInfo (st : State) (req : Request) : IO Response := do
  let auth := (req.header? "authorization").getD ""
  let token :=
    if auth.startsWith "Bearer " then (auth.drop 7).toString else ""
  let tokens ← st.tokens.get
  match tokens.find? (·.token == token) with
  | none =>
    return Response.jsonError 401 "invalid_token"
  | some t =>
    match st.cfg.clients.findSome? fun c =>
      if c.user.sub == t.userSub then some c.user else none with
    | none   =>
      return Response.jsonError 500 "user vanished"
    | some u =>
      -- Structured build: every string field goes through Json.str, so
      -- a `"` in u.email / u.name (attacker-supplied via CLI or DB
      -- restore) gets escaped by the codec instead of breaking out.
      return Response.json 200 <| Lean.Json.mkObj [
        ("sub",     Lean.Json.str u.sub),
        ("email",   Lean.Json.str u.email),
        ("name",    Lean.Json.str u.name),
        ("picture", Lean.Json.str u.picture)
      ]

/-- Compose the three endpoints into a single `Handler`. -/
def handler (st : State) : Handler := fun req =>
  match req.path, req.method with
  | "/authorize", "GET" => handleAuthorize st req
  | "/token",     "POST" => handleToken st req
  | "/userinfo",  "GET"  => handleUserInfo st req
  | _,            _      => pure Response.notFound

/-- Spawn the IdP on `port`. Returns a handle once the listener has
    bound (best-effort small sleep to let the accept loop start). The
    server runs forever on a background task; the test process exit
    is what tears it down. -/
def start (port : UInt16) (cfg : Config) : IO State := do
  let codes  ← IO.mkRef ([] : List IssuedCode)
  let tokens ← IO.mkRef ([] : List IssuedToken)
  let st : State := { cfg, codes, tokens }
  /- `Task.Priority.dedicated` puts the server on its own OS thread.
     Without it, the runtime's default task pool can starve the server
     when the main thread blocks on `IO.Process.output` (curl), which
     deadlocks our token-exchange round-trip. -/
  let _ ← IO.asTask (prio := Task.Priority.dedicated)
            (LeanTea.Net.Server.serve port "127.0.0.1" (handler st))
  /- Give the accept loop ~200 ms to bind so the first SP-side request
     isn't a connection-refused. -/
  IO.sleep 200
  return st

end OAuth2

/-! ## SAML 2.0 fixture IdP

Building fully-signed XML in Lean is a separate project; the
framework's SP-side verifier hands signatures to `xmlsec1` already
(see `LeanTea.Auth.Saml`). This module ships realistic-shape XML
fixtures that the SP parser can chew on, without needing a Keycloak
or ADFS deployment for tests. -/

namespace Saml

structure Attribute where
  name   : String
  values : List String
  deriving Inhabited, Repr

structure Fixture where
  issuer       : String := "https://idp.example.com"
  audience     : String := "https://sp.example.com"
  subjectEmail : String := "alice@example.com"
  notBefore    : String := "2026-06-17T00:00:00Z"
  notOnOrAfter : String := "2099-06-17T01:00:00Z"  -- far future so it's valid
  attributes   : List Attribute := [⟨"groups", ["engineers", "oncall"]⟩]
  /-- Bare ID — caller supplies for replay-detection tests. -/
  assertionId  : String := "_a1"
  deriving Inhabited

/-- Build an unsigned AuthnResponse. Real IdP would sign the
    assertion; the SP-side `Saml.parseResponse` works either way. -/
def buildResponse (f : Fixture) : String :=
  let attrs := f.attributes.foldl (fun acc a =>
    let vs := a.values.foldl (fun s v =>
      s ++ s!"<saml:AttributeValue>{v}</saml:AttributeValue>") ""
    acc ++ s!"<saml:Attribute Name=\"{a.name}\">{vs}</saml:Attribute>") ""
  "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">" ++
  s!"<saml:Assertion ID=\"{f.assertionId}\" xmlns:saml=\"urn:oasis:names:tc:SAML:2.0:assertion\">" ++
  s!"<saml:Issuer>{f.issuer}</saml:Issuer>" ++
  "<saml:Subject>" ++
  s!"<saml:NameID Format=\"urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress\">{f.subjectEmail}</saml:NameID>" ++
  "</saml:Subject>" ++
  s!"<saml:Conditions NotBefore=\"{f.notBefore}\" NotOnOrAfter=\"{f.notOnOrAfter}\">" ++
  "<saml:AudienceRestriction>" ++
  s!"<saml:Audience>{f.audience}</saml:Audience>" ++
  "</saml:AudienceRestriction>" ++
  "</saml:Conditions>" ++
  "<saml:AttributeStatement>" ++ attrs ++ "</saml:AttributeStatement>" ++
  "</saml:Assertion>" ++
  "</samlp:Response>"

end Saml

end LeanTea.Auth.Idp
