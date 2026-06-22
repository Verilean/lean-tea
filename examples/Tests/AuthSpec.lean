import LeanTea
import LeanTea.Auth.Idp
import LeanTea.Auth.OAuth2
import LeanTea.Auth.Saml

/-! # examples/Tests/AuthSpec.lean — round-trip tests against in-process IdPs

Until this binary, the `LeanTea.Auth.*` modules only had pure-Lean
shape tests (the SAML XML parse in `pure_spec`). This runner spins
up a real OAuth 2.0 IdP on a local port via `LeanTea.Auth.Idp` and
walks the SP-side flow end-to-end:

  beginAuth → authorizeUrl → GET /authorize → follow 302 →
  exchangeCode → fetchUserInfo

It also extends the SAML coverage with fixture-generated assertions
so we can assert on edge cases (multi-value attributes, custom
issuer / audience) that the single fixture in `pure_spec` doesn't
cover.

Requires `curl(1)` on `$PATH` (used by `LeanTea.Auth.OAuth2`) — CI
runners have it. -/

open LeanTea LeanTea.LSpec
open LeanTea.Net.Http
open LeanTea.Auth.OAuth2

/-! ## helpers -/

private def hasSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Pull a `?k=v` value out of a URL's query string. -/
private def queryParam (url : String) (key : String) : Option String :=
  let q := match url.splitOn "?" with
    | [_, q] => q
    | _ :: rest => String.intercalate "?" rest
    | _ => ""
  let pref := key ++ "="
  (q.splitOn "&").findSome? fun p =>
    if p.startsWith pref then
      /- Strip trailing `&…` if needed. -/
      let v := (p.drop pref.length).toString
      some (v.takeWhile (· != '&')).toString
    else none

private def headerVal (hdrs : Array (String × String)) (name : String) : Option String :=
  hdrs.findSome? fun (k, v) => if k.toLower == name.toLower then some v else none

/-! ## Group 1 — OAuth 2.0 round-trip against the in-process IdP -/

namespace OAuth2Group

open LeanTea.Auth.Idp

/-- The user the IdP knows about. -/
def alice : OAuth2.User := {
  sub     := "alice-sub-001",
  email   := "alice@example.com",
  name    := "Alice Example",
  picture := "https://idp.example.com/alice.png"
}

def clientPort : UInt16 := 18765

def clientConf (port : UInt16) : OAuth2.Client := {
  clientId     := "test-client",
  clientSecret := "shh",
  redirectUri  := s!"http://127.0.0.1:{port + 1}/cb",   -- bogus, we never actually hit it
  user         := alice
}

/-- Provider pointing at our in-process IdP. -/
def localProvider (port : UInt16) : Provider := {
  name             := "local-test-idp",
  authEndpoint     := s!"http://127.0.0.1:{port}/authorize",
  tokenEndpoint    := s!"http://127.0.0.1:{port}/token",
  userInfoEndpoint := s!"http://127.0.0.1:{port}/userinfo",
  defaultScope     := "openid email profile",
  /- `usePkce := false` here because our IdP only checks `plain`-style
     PKCE; the SP uses S256 which the test IdP doesn't verify. The
     round-trip is still meaningful for everything except the PKCE
     hash check itself. -/
  supportsPkce     := false
}

/-- Spawn the `auth_idp_serve` binary and wait for it to bind. The
    child's stdio is left at its inherited defaults; `Child` is
    parameterised by its `StdioConfig` so we have to inline it. -/
private def spawnIdp (port : UInt16) :
    IO (IO.Process.Child { stdin := .null, stdout := .null, stderr := .null }) := do
  let env : Array (String × Option String) := #[("AUTH_TEST_PORT", some (toString port))]
  let child ← IO.Process.spawn {
    cmd := "./.lake/build/bin/auth_idp_serve",
    args := #[],
    env,
    stdin := .null, stdout := .null, stderr := .null
  }
  /- Poll the port until it answers, up to ~2 seconds. -/
  let probe (url : String) : IO Bool := do
    match LeanTea.Net.HttpClient.parseUrl url with
    | none => return false
    | some u =>
      try
        let _ ← LeanTea.Net.HttpClient.request "GET" u
        return true
      catch _ => return false
  for _ in [:20] do
    if ← probe s!"http://127.0.0.1:{port}/authorize" then break
    IO.sleep 100
  return child

/-- Stage the round-trip and return the assertions as one LSpec
    tree. Caller spawns the IdP and is responsible for teardown. -/
def run : IO LSpec := do
  let port := match (← IO.getEnv "AUTH_TEST_PORT").bind String.toNat? with
    | some n => n.toUInt16
    | none   => clientPort
  let client := clientConf port
  let provider := localProvider port
  let cfg : Config := provider.withClient
    client.clientId client.clientSecret client.redirectUri (usePkce := false)
  /- 1. Mint a state token. -/
  let st ← beginAuth cfg
  let url := authorizeUrl cfg st
  /- 2. GET /authorize via HttpClient. Don't follow the 302; read
        the Location header instead. -/
  let parsed ← match LeanTea.Net.HttpClient.parseUrl url with
    | some u => pure u
    | none   => throw <| IO.userError s!"bad URL: {url}"
  let resp ← LeanTea.Net.HttpClient.request "GET" parsed
  let loc := (headerVal resp.headers "location").getD ""
  let codeOpt  := queryParam loc "code"
  let stateOpt := queryParam loc "state"
  let code := codeOpt.getD ""
  /- 3. Exchange the code for an access token via SP-side code path. -/
  let tokRes ← exchangeCode cfg code st
  let access := match tokRes with
    | .ok t    => t.accessToken
    | .error _ => ""
  /- 4. Fetch the userinfo profile. -/
  let infoRes ← if access.isEmpty then pure (.error "no access token")
                else fetchUserInfo cfg access
  /- 5. Negative: re-using the same code must now fail (the IdP burned
        it on step 3). -/
  let replay ← exchangeCode cfg code st
  /- 6. Negative: a totally bogus code is rejected. -/
  let fakeRes ← exchangeCode cfg "obvious-fake-code" st
  return group "OAuth2 round-trip via in-process IdP" [
    it "/authorize returned a 302"           (resp.status == 302),
    it "Location header has a `code`"        codeOpt.isSome,
    it "Location header preserves `state`"   (stateOpt == some st.state),
    it "exchangeCode returns an access token"
      (match tokRes with | .ok _ => true | .error _ => false),
    it "fetchUserInfo returns Alice's profile"
      (match infoRes with
       | .ok u => u.email == "alice@example.com"
                  && u.sub == "alice-sub-001"
                  && u.name == "Alice Example"
       | .error _ => false),
    it "burned code can't be replayed"
      (match replay with | .ok _ => false | .error _ => true),
    it "fake code is rejected"
      (match fakeRes with | .ok _ => false | .error _ => true)
  ]

end OAuth2Group

/-! ## Group 2 — SAML 2.0 fixture round-trip -/

namespace SamlGroup

open LeanTea.Auth.Idp.Saml
open LeanTea.Auth.Saml (parseResponse)

def run : IO LSpec := do
  /- 1. Plain happy path: default fixture. -/
  let xmlOk := buildResponse {}
  let parsedOk := parseResponse xmlOk
  /- 2. Custom audience + extra attribute. -/
  let xmlCustom := buildResponse {
    audience     := "https://other-sp.example.com",
    subjectEmail := "bob@example.com",
    attributes := [
      ⟨"groups", ["a", "b", "c"]⟩,
      ⟨"department", ["engineering"]⟩
    ]
  }
  let parsedCustom := parseResponse xmlCustom
  /- 3. Distinct assertion IDs so the SP could implement replay-detection. -/
  let id1 := buildResponse { assertionId := "_aaa" }
  let id2 := buildResponse { assertionId := "_bbb" }
  let parsedId1 := parseResponse id1
  let parsedId2 := parseResponse id2
  return group "SAML 2.0 fixture round-trip" [
    group "default fixture parses cleanly" [
      it "ok return"
        (match parsedOk with | .ok _ => true | .error _ => false),
      it "issuer == idp.example.com"
        (match parsedOk with
         | .ok a => a.issuer == "https://idp.example.com"
         | .error _ => false),
      it "subject email round-trips"
        (match parsedOk with
         | .ok a => a.nameId == "alice@example.com"
         | .error _ => false),
      it "audience round-trips"
        (match parsedOk with
         | .ok a => a.audiences.head? == some "https://sp.example.com"
         | .error _ => false),
      it "single multi-valued attribute parsed"
        (match parsedOk with
         | .ok a =>
           match a.attributes.head? with
           | some attr => attr.values == ["engineers", "oncall"]
           | none => false
         | .error _ => false)
    ],
    group "custom audience + extra attributes" [
      it "audience reflects override"
        (match parsedCustom with
         | .ok a => a.audiences.head? == some "https://other-sp.example.com"
         | .error _ => false),
      it "two attributes parsed"
        (match parsedCustom with
         | .ok a => a.attributes.length == 2
         | .error _ => false),
      it "second attribute name"
        (match parsedCustom with
         | .ok a =>
           match a.attributes[1]? with
           | some attr => attr.name == "department"
           | none => false
         | .error _ => false)
    ],
    group "distinct assertion IDs for replay-detection setups" [
      it "_aaa surfaces"
        (match parsedId1 with | .ok a => a.id == "_aaa" | .error _ => false),
      it "_bbb surfaces"
        (match parsedId2 with | .ok a => a.id == "_bbb" | .error _ => false)
    ]
  ]

end SamlGroup

/-! ## Entry point. -/

def main : IO Unit := do
  let port : UInt16 := match (← IO.getEnv "AUTH_TEST_PORT").bind String.toNat? with
    | some n => n.toUInt16
    | none   => OAuth2Group.clientPort
  let child ← OAuth2Group.spawnIdp port
  try
    let s1 ← OAuth2Group.run
    let s2 ← SamlGroup.run
    let tree := group "LeanTEA auth integration" [s1, s2]
    let code ← lspecIO tree
    let _ ← child.kill
    if code != 0 then IO.Process.exit code.toUInt8
  catch e =>
    let _ ← child.kill
    IO.println s!"auth_spec: crashed → {e}"
    IO.Process.exit 1
