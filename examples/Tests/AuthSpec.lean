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

Group 3 drives `OAuth2.gate` (both session backends) the same way, and
group 4 the sign-in proxy: in-process, then the `auth_proxy` executable
as its own process, plus the TLS client's timeout. Build with
`LEANTEA_TLS=1` (the timeout case needs the real OpenSSL client). -/

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
  picture := "https://idp.example.com/alice.png",
  login   := "alice"
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
  defaultScope     := "openid email profile"
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
    client.clientId client.clientSecret client.redirectUri
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
  /- 7. Negative: a fresh code with the wrong PKCE verifier. -/
  let st2 ← beginAuth cfg
  let resp2 ← LeanTea.Net.HttpClient.request "GET"
    ((LeanTea.Net.HttpClient.parseUrl (authorizeUrl cfg st2)).getD parsed)
  let code2 := (queryParam ((headerVal resp2.headers "location").getD "") "code").getD ""
  let wrongVerifier ← exchangeCode cfg code2 { st2 with codeVerifier := "not-the-verifier" }
  return group "OAuth2 round-trip via in-process IdP" [
    it "/authorize returned a 302"           (resp.status == 302),
    it "Location header has a `code`"        codeOpt.isSome,
    it "Location header preserves `state`"   (stateOpt == some st.state),
    it "authorize URL sends an S256 challenge"
      (hasSubstr url "code_challenge_method=S256"),
    it "exchangeCode returns an access token"
      (match tokRes with | .ok _ => true | .error _ => false),
    it "fetchUserInfo returns Alice's profile"
      (match infoRes with
       | .ok u => u.email == "alice@example.com"
                  && u.sub == "alice-sub-001"
                  && u.name == "Alice Example"
                  && u.login == "alice"
       | .error _ => false),
    it "burned code can't be replayed"
      (match replay with | .ok _ => false | .error _ => true),
    it "fake code is rejected"
      (match fakeRes with | .ok _ => false | .error _ => true),
    it "a wrong PKCE verifier is rejected"
      (match wrongVerifier with | .ok _ => false | .error _ => true)
  ]

end OAuth2Group

/-! ## Group 3 — `OAuth2.gate` against the in-process IdP

The gate is called in-process with hand-built requests; only its calls
to the IdP (token, userinfo) and the browser's visit to `/authorize` go
over the network. A settable clock drives the expiry cases. Every case
runs against both session backends: SQLite rows and signed cookies. -/

namespace GateGroup

open LeanTea.Auth.Idp

def request (path : String) (query : String := "")
    (cookies : List (String × String) := []) (method := "GET")
    (headers : Array (String × String) := #[]) (body : ByteArray := .empty) : Request :=
  let hdr := String.intercalate "; " (cookies.map fun (k, v) => k ++ "=" ++ v)
  { method, path, query, body,
    headers := headers ++ (if cookies.isEmpty then #[] else #[("cookie", hdr)]) }

/-- Every `set-cookie` of a response. -/
def setCookies (r : Response) : List String :=
  r.headers.toList.filterMap fun (k, v) => if k.toLower == "set-cookie" then some v else none

/-- The value a response sets for cookie `name`, if it sets one. -/
def cookieSet (r : Response) (name : String) : Option String :=
  (setCookies r).findSome? fun c =>
    if c.startsWith (name ++ "=") then
      some ((c.drop (name.length + 1)).takeWhile (· != ';')).toString
    else none

def location (r : Response) : String := (headerVal r.headers "location").getD ""

def body (r : Response) : String := (String.fromUTF8? r.body).getD ""

/-- Shows who `inner` saw, so the tests can read it back. -/
private def inner : Option User → Handler
  | some u, _ => pure (Response.text 200 s!"{u.provider}|{u.login}|{u.sub}|{u.email}|{u.name}")
  | none, _ => pure (Response.text 200 "guest")

/-- A browser's sign-in through `h`: the login route, the IdP's
    `/authorize`, then the callback at `callback`. `editQuery` changes
    the callback query (to tamper with the state); `beforeCallback` runs
    just before it (to move the clock). Returns the login and callback
    responses. -/
def signIn (h : Handler) (next : String := "/private") (loginPath := "/auth/login")
    (callback := "/cb") (editQuery : String → String := id) (sendStateCookie := true)
    (beforeCallback : IO Unit := pure ()) : IO (Response × Response) := do
  let r1 ← h (request loginPath s!"next={LeanTea.Auth.urlEncode next}")
  let stateCookie := (cookieSet r1 "session_login").getD ""
  let some u := LeanTea.Net.HttpClient.parseUrl (location r1)
    | throw <| IO.userError s!"login route did not redirect to the IdP: {location r1}"
  let r2 ← LeanTea.Net.HttpClient.request "GET" u
  let query := match ((r2.header? "location").getD "").splitOn "?" with
    | [_, q] => q
    | _ => ""
  beforeCallback
  let cookies := if sendStateCookie then [("session_login", stateCookie)] else []
  let r3 ← h (request callback (editQuery query) cookies)
  return (r1, r3)

def idpConfig : IO Config := do
  let port := match (← IO.getEnv "AUTH_TEST_PORT").bind String.toNat? with
    | some n => n.toUInt16
    | none   => OAuth2Group.clientPort
  let client := OAuth2Group.clientConf port
  return (OAuth2Group.localProvider port).withClient
    client.clientId client.clientSecret client.redirectUri

/-- The gate cases against one session backend. -/
def cases (backend : String) (sessions : Sessions) : IO LSpec := do
  let cfg ← idpConfig
  let now ← IO.mkRef 1000000
  let g : GateConfig := {
    client := cfg, allowedLogins := ["Alice"], sessionTtl := 3600, stateTtl := 600,
    publicPaths := ["/health"], clock := now.get }
  let h := gate g sessions inner
  let hBob := gate { g with allowedLogins := ["bob"] } sessions inner
  let hSecure := gate { g with cookieSecure := true } sessions inner

  /- Before signing in. -/
  let page ← h (request "/private" "tab=2")
  let api ← h (request "/api/issues")
  let pub ← h (request "/health")
  let secureLogin ← hSecure (request "/auth/login")

  /- A good sign-in, then using the session. -/
  let (login, cb) ← signIn h
  let session := (cookieSet cb "session").getD ""
  let sessionCookie := (setCookies cb).find? (·.startsWith "session=") |>.getD ""
  let home ← h (request "/private" "" [("session", session)])
  let homeApi ← h (request "/api/issues" "" [("session", session)])
  let forged ← h (request "/api/issues" "" [("session", session ++ "x")])

  /- State: changed, missing cookie, used twice, too old. -/
  let (_, tampered) ← signIn h (editQuery := fun q =>
    String.intercalate "&" ((q.splitOn "&").map fun p =>
      if p.startsWith "state=" then "state=0000" else p))
  let (_, noCookie) ← signIn h (sendStateCookie := false)
  let (_, slow) ← signIn h (beforeCallback := now.modify (· + 601))
  let (lgn, used) ← signIn h
  let usedCookie := (cookieSet lgn "session_login").getD ""
  let usedState := (queryParam (location lgn) "state").getD ""
  let replay ← h (request "/cb" s!"code=again&state={usedState}" [("session_login", usedCookie)])

  /- Someone not on the list; a session whose name left the list. -/
  let (_, refused) ← signIn hBob
  let (_, cbD) ← signIn h
  let sD := (cookieSet cbD "session").getD ""
  let delisted ← hBob (request "/api/issues" "" [("session", sD)])
  let delistedBack ← h (request "/api/issues" "" [("session", sD)])

  /- `next` must stay on this site. -/
  let (_, offsite) ← signIn h (next := "//evil.example/x")

  /- Logout ends the session. -/
  let (_, cb2) ← signIn h
  let s2 := (cookieSet cb2 "session").getD ""
  let out ← h (request "/auth/logout" "" [("session", s2)])
  let after ← h (request "/api/issues" "" [("session", s2)])
  let firstStill ← h (request "/api/issues" "" [("session", session)])

  /- Expiry: the first session is an hour old after this. -/
  now.modify (· + 3600)
  let expiredApi ← h (request "/api/issues" "" [("session", session)])
  let expiredPage ← h (request "/private" "" [("session", session)])

  return group s!"sessions in {backend}" [
    group "without a session" [
      it "a page redirects to the login route, keeping the path"
        (page.status == 302 && location page == "/auth/login?next=%2Fprivate%3Ftab%3D2"),
      it "an API path is 401" (api.status == 401),
      it "a public path reaches inner as a guest" (pub.status == 200 && body pub == "guest")
    ],
    group "sign-in" [
      it "login redirects to the IdP with state and an S256 challenge"
        (login.status == 302 && hasSubstr (location login) "state="
          && hasSubstr (location login) "code_challenge_method=S256"),
      it "login sets a short HttpOnly, SameSite=Lax state cookie"
        ((setCookies login).any fun c => c.startsWith "session_login="
          && hasSubstr c "HttpOnly" && hasSubstr c "SameSite=Lax" && hasSubstr c "Max-Age=600"),
      it "the callback returns to `next` with a session cookie"
        (cb.status == 302 && location cb == "/private" && !session.isEmpty),
      it "the session cookie is HttpOnly, SameSite=Lax, with the session's lifetime"
        (hasSubstr sessionCookie "HttpOnly" && hasSubstr sessionCookie "SameSite=Lax"
          && hasSubstr sessionCookie "Max-Age=3600" && !hasSubstr sessionCookie "Secure"),
      it "cookieSecure marks the cookies Secure"
        (!(setCookies secureLogin).isEmpty && (setCookies secureLogin).all (hasSubstr · "; Secure")),
      it "the callback clears the state cookie"
        ((setCookies cb).any fun c => c.startsWith "session_login=;" && hasSubstr c "Max-Age=0"),
      it "inner gets the user: provider, login, sub, email, name"
        (home.status == 200
          && body home == "local-test-idp|alice|alice-sub-001|alice@example.com|Alice Example"),
      it "the session opens API paths too" (homeApi.status == 200),
      it "the allow-list ignores case (Alice = alice)" (home.status == 200),
      it "an altered session cookie is not a session" (forged.status == 401),
      it "a `next` to another host becomes /" (offsite.status == 302 && location offsite == "/")
    ],
    group "state" [
      it "a changed state is refused" (tampered.status == 400 && cookieSet tampered "session" == none),
      it "a state without this browser's state cookie is refused"
        (noCookie.status == 400 && cookieSet noCookie "session" == none),
      it "a used login cannot be finished again"
        (used.status == 302 && replay.status != 302 && cookieSet replay "session" == none),
      it "a login older than stateTtl is refused"
        (slow.status == 400 && cookieSet slow "session" == none)
    ],
    group "allow-list" [
      it "a user not on the list is refused with 403"
        (refused.status == 403 && cookieSet refused "session" == none),
      it "a session whose name left the list is refused" (delisted.status == 401),
      it "…and stays ended" (delistedBack.status == 401)
    ],
    group "logout" [
      it "logout clears the cookie and redirects"
        (out.status == 302
          && (setCookies out).any (fun c => c.startsWith "session=;" && hasSubstr c "Max-Age=0")),
      it "the logged-out cookie no longer works" (after.status == 401),
      it "other sessions stay" (firstStill.status == 200)
    ],
    group "expiry" [
      it "after sessionTtl an API path is 401" (expiredApi.status == 401),
      it "after sessionTtl a page redirects to login"
        (expiredPage.status == 302 && (location expiredPage).startsWith "/auth/login")
    ]
  ]

def run : IO LSpec := do
  let db ← LeanTea.Sqlite.open' ":memory:"
  let sqlite := (← GateStore.attach db).sessions
  let signed ← signedSessions "0123456789abcdef0123456789abcdef".toUTF8
  let a ← cases "SQLite" sqlite
  let b ← cases "signed cookies" signed
  return group "OAuth2.gate via in-process IdP" [a, b]

end GateGroup

/-! ## Group 4 — the sign-in proxy

An in-process upstream echoes what it received as JSON. The proxy's
handler is first called in-process (as the gate is above), then the
`auth_proxy` executable is run as its own process with its secrets in a
credentials directory, and driven over HTTP. -/

namespace ProxyGroup

open GateGroup
open Lean (Json)

def upstreamPort : UInt16 := 18767
def proxyPort : UInt16 := 18768

/-- Answers with the request it got: method, path, query, body and headers. -/
def echo : Handler := fun req => do
  let hs := Json.mkObj (req.headers.toList.map fun (k, v) => (k.toLower, Json.str v))
  return Response.json 200 <| Json.mkObj [
    ("method", Json.str req.method), ("path", Json.str req.path),
    ("query", Json.str req.query), ("body", Json.str ((String.fromUTF8? req.body).getD "")),
    ("headers", hs)]

/-- A field of the echoed request; `headers.x` for a header. -/
def seen (r : Response) (key : String) : Option String := do
  let j ← (Json.parse (body r)).toOption
  match key.splitOn "." with
  | ["headers", h] => ((j.getObjVal? "headers").toOption.bind (·.getObjVal? h |>.toOption)).bind
                        (·.getStr?.toOption)
  | _ => (j.getObjVal? key).toOption.bind (·.getStr?.toOption)

def spoofed : Array (String × String) :=
  #[("X-Forwarded-User", "mallory"), ("X-Forwarded-Email", "mallory@example.com"),
    ("x-forwarded-for", "10.0.0.66"), ("X-Forwarded-Preferred-Username", "mallory"),
    ("Forwarded", "for=10.0.0.66"), ("X-Real-Ip", "10.0.0.66")]

/-- The proxy handler, in-process. -/
def inProcess : IO LSpec := do
  let cfg ← GateGroup.idpConfig
  let g : GateConfig := { client := cfg, allowedLogins := ["alice"], publicPaths := ["/health"] }
  let sessions ← signedSessions "proxy-test-secret-0123456789abcdef".toUTF8
  let up : LeanTea.Auth.Proxy.Upstream := { port := upstreamPort, maxBody := 64 }
  let h := LeanTea.Auth.Proxy.handler up g sessions
  let (_, cb) ← signIn h
  let s := (cookieSet cb "session").getD ""
  let get ← h (request "/api/items" "page=2" [("session", s), ("theme", "dark")] (headers := spoofed))
  let post ← h (request "/api/items" "" [("session", s)] "POST"
    #[("Content-Type", "application/json")] "{\"title\":\"hi\"}".toUTF8)
  let big ← h (request "/api/items" "" [("session", s)] "POST" #[] (String.ofList (List.replicate 65 'x')).toUTF8)
  let anon ← h (request "/api/items" (headers := spoofed))
  let pub ← h (request "/health" (headers := spoofed))
  let down ← LeanTea.Auth.Proxy.handler { port := 1 } g sessions
    (request "/api/items" "" [("session", s)])
  return group "proxy handler (in-process)" [
    it "a signed-in GET reaches the upstream with path and query"
      (get.status == 200 && seen get "method" == some "GET" && seen get "path" == some "/api/items"
        && seen get "query" == some "page=2"),
    it "spoofed X-Forwarded-User is replaced by the signed-in login"
      (seen get "headers.x-forwarded-user" == some "alice"),
    it "spoofed X-Forwarded-Email and Preferred-Username are replaced"
      (seen get "headers.x-forwarded-email" == some "alice@example.com"
        && seen get "headers.x-forwarded-preferred-username" == some "alice"),
    it "other client X-Forwarded-*, Forwarded and X-Real-Ip are dropped"
      (seen get "headers.x-forwarded-for" == none && seen get "headers.forwarded" == none
        && seen get "headers.x-real-ip" == none),
    it "the gate's cookies are not passed on; others are"
      (seen get "headers.cookie" == some "theme=dark"),
    it "a POST body and its content type are forwarded"
      (post.status == 200 && seen post "method" == some "POST"
        && seen post "body" == some "{\"title\":\"hi\"}"
        && seen post "headers.content-type" == some "application/json"),
    it "a body over maxBody is refused with 413" (big.status == 413),
    it "without a session an API path is 401 and not forwarded"
      (anon.status == 401 && seen anon "method" == none),
    it "a public path is forwarded with no X-Forwarded-User at all"
      (pub.status == 200 && seen pub "headers.x-forwarded-user" == none),
    it "an unreachable upstream is 502" (down.status == 502)
  ]

private def httpGet (path : String) (cookies : List (String × String) := [])
    (headers : Array (String × String) := #[]) : IO LeanTea.Net.HttpClient.Response := do
  let hs := headers ++ (if cookies.isEmpty then #[] else
    #[("Cookie", String.intercalate "; " (cookies.map fun (k, v) => k ++ "=" ++ v))])
  LeanTea.Net.HttpClient.request "GET" { host := "127.0.0.1", port := proxyPort, path } .empty hs

private def setCookieOf (r : LeanTea.Net.HttpClient.Response) (name : String) : String :=
  (r.headers.toList.findSome? fun (k, v) =>
    if k.toLower == "set-cookie" && v.startsWith (name ++ "=") then
      some ((v.drop (name.length + 1)).takeWhile (· != ';')).toString
    else none).getD ""

/-- `auth_proxy` as its own process. -/
def binary (idpPort : UInt16) : IO LSpec := do
  let dir : System.FilePath := ".lake/build/auth_proxy_test"
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "client-secret") "shh\n"
  IO.FS.writeFile (dir / "cookie-secret") "a3f1c2d4e5b6a7980123456789abcdef0123456789abcdef\n"
  let idp := s!"http://127.0.0.1:{idpPort}"
  let args := #["--listen", s!"127.0.0.1:{proxyPort}", "--upstream", s!"127.0.0.1:{upstreamPort}",
    "--provider", "github", "--client-id", "proxy-client", "--allowed-users", "alice",
    "--redirect-url", s!"http://127.0.0.1:{proxyPort}/oauth2/callback", "--cookie-secure", "false",
    "--auth-url", s!"{idp}/authorize", "--token-url", s!"{idp}/token",
    "--userinfo-url", s!"{idp}/userinfo"]
  let refusedArg ← IO.Process.output
    { cmd := "./.lake/build/bin/auth_proxy", args := #["--client-secret", "shh"] }
  let credDir := (← IO.FS.realPath dir).toString
  let env : Array (String × Option String) := #[("CREDENTIALS_DIRECTORY", some credDir)]
  let child ← IO.Process.spawn {
    cmd := "./.lake/build/bin/auth_proxy", args := args, env := env,
    stdin := .null, stdout := .null, stderr := .null
  }
  let mut up := false
  for _ in [:50] do
    let ok ← try let _ ← httpGet "/health"; pure true catch _ => pure false
    if ok then up := true; break
    IO.sleep 100
  try
    let start ← httpGet "/oauth2/start" -- login
    let st := setCookieOf start "session_login"
    let some toIdp := LeanTea.Net.HttpClient.parseUrl ((start.header? "location").getD "")
      | throw <| IO.userError "auth_proxy did not redirect to the IdP"
    let fromIdp ← LeanTea.Net.HttpClient.request "GET" toIdp
    let q := ((fromIdp.header? "location").getD "").splitOn "?" |>.getD 1 ""
    let cb ← httpGet s!"/oauth2/callback?{q}" [("session_login", st)]
    let s := setCookieOf cb "session"
    let echoed ← httpGet "/api/x" [("session", s)] spoofed
    let r : Response := { status := echoed.status, headers := echoed.headers, body := echoed.body }
    let out ← httpGet "/oauth2/sign_out" [("session", s)]
    let after ← httpGet "/api/x" [("session", s)]
    let _ ← child.kill
    return group "auth_proxy executable" [
      it "--client-secret on the command line is refused" (refusedArg.exitCode != 0),
      it "starts with secrets from $CREDENTIALS_DIRECTORY" up,
      it "signs in through the IdP" (cb.status == 302 && !s.isEmpty),
      it "forwards with X-Forwarded-User = the login, spoofing removed"
        (r.status == 200 && seen r "headers.x-forwarded-user" == some "alice"
          && seen r "headers.x-forwarded-for" == none),
      it "sign_out ends the session" (out.status == 302 && after.status == 401)
    ]
  catch e =>
    let _ ← child.kill
    throw e

/-- A TLS request to a server that accepts the connection but never
    answers must fail within its timeout. The "server" is an HTTP
    server: it reads the ClientHello as the start of a request and waits
    for the rest, so it never says anything. -/
def tlsTimeout : IO LSpec := do
  let _ ← IO.asTask (prio := .dedicated)
    (LeanTea.Net.Server.serve 18769 "127.0.0.1" fun _ => pure (Response.text 200 "never"))
  IO.sleep 200
  let t0 ← IO.monoMsNow
  let msg ← try
      let _ ← LeanTea.Net.TlsClient.requestUrl "GET" "https://127.0.0.1:18769/" (timeoutMs := 1500)
      pure "answered"
    catch e => pure (toString e)
  let ms := (← IO.monoMsNow) - t0
  return group "TlsClient timeout" [
    it s!"a silent server gives \"timed out\" ({msg})" (hasSubstr msg "timed out"),
    it s!"…after about the timeout ({ms} ms)" (ms >= 1400 && ms < 5000)
  ]

def run (idpPort : UInt16) : IO LSpec := do
  let _ ← IO.asTask (prio := .dedicated) (LeanTea.Net.Server.serve upstreamPort "127.0.0.1" echo)
  IO.sleep 200
  let a ← inProcess
  let b ← binary idpPort
  let c ← tlsTimeout
  return group "sign-in proxy" [a, b, c]

end ProxyGroup

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
  /- 4. Signature-wrapping (XSW) payloads for `checkNoWrapping`. -/
  let sigFor := fun (id : String) =>
    s!"<ds:Signature><ds:SignedInfo><ds:Reference URI=\"#{id}\"></ds:Reference>" ++
    "</ds:SignedInfo></ds:Signature>"
  -- One assertion, signed, self-referencing → accepted.
  let signedOk :=
    (buildResponse { assertionId := "_a1" }).replace
      "</saml:Assertion>" (sigFor "_a1" ++ "</saml:Assertion>")
  -- Wrapping: a forged unsigned assertion first, the real signed one
  -- second. `parseResponse` reads identity from the FIRST — the guard
  -- must reject on the assertion count (2).
  let evilAssert :=
    "<saml:Assertion ID=\"_evil\" xmlns:saml=\"urn:oasis:names:tc:SAML:2.0:assertion\">" ++
    "<saml:Subject><saml:NameID>attacker@evil.com</saml:NameID></saml:Subject>" ++
    "</saml:Assertion>"
  let realAssert :=
    (buildResponse { assertionId := "_a1", subjectEmail := "alice@example.com" }
      |>.replace "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">" ""
      |>.replace "</samlp:Response>" "")
      |>.replace "</saml:Assertion>" (sigFor "_a1" ++ "</saml:Assertion>")
  let wrapped :=
    "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">" ++
    evilAssert ++ realAssert ++ "</samlp:Response>"
  -- One assertion but the signature references a different id → reject.
  let wrongRef :=
    (buildResponse { assertionId := "_a1" }).replace
      "</saml:Assertion>" (sigFor "_other" ++ "</saml:Assertion>")
  -- Unsigned single assertion → reject (no <Signature>).
  let unsigned := buildResponse { assertionId := "_a1" }
  let guard := fun (xml : String) =>
    match parseResponse xml with
    | .ok a => match LeanTea.Auth.Saml.checkNoWrapping xml a with
               | .ok _ => true | .error _ => false
    | .error _ => false
  return group "SAML 2.0 fixture round-trip" [
    group "signature-wrapping (XSW) guard" [
      it "single signed self-referencing assertion is accepted"
        (guard signedOk),
      it "two-assertion wrapping payload is rejected"
        (!(guard wrapped)),
      it "wrapped payload parses to the forged (first) assertion"
        (match parseResponse wrapped with
         | .ok a => a.id == "_evil" | .error _ => false),
      it "signature referencing a different id is rejected"
        (!(guard wrongRef)),
      it "unsigned assertion is rejected"
        (!(guard unsigned)),
      it "assertionCount sees exactly one in a normal response"
        (LeanTea.Auth.Saml.assertionCount signedOk == 1),
      it "assertionCount sees two in a wrapping payload"
        (LeanTea.Auth.Saml.assertionCount wrapped == 2)
    ],
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
    let s3 ← GateGroup.run
    let s4 ← ProxyGroup.run port
    let tree := group "LeanTEA auth integration" [s1, s3, s4, s2]
    let code ← lspecIO tree
    let _ ← child.kill
    -- exit explicitly: a returning `main` waits for the in-process servers' tasks
    IO.Process.exit code.toUInt8
  catch e =>
    let _ ← child.kill
    IO.println s!"auth_spec: crashed → {e}"
    IO.Process.exit 1
