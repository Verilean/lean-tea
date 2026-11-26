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
  let g : GateConfig := { client := cfg, allowedLogins := ["alice"], publicPaths := ["/health"],
                          publicPrefixes := ["/static/"] }
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
  let underscore : Array (String × String) := #[("X-Forwarded_User", "mallory"), ("X_Real_IP", "10.0.0.66")]
  let pubUnderscore ← h (request "/health" (headers := underscore))
  let getUnderscore ← h (request "/api/items" "" [("session", s)] (headers := underscore))
  let static ← h (request "/static/app.css")
  let tricks ← ["/static/../api/items", "/static/%2e%2e/api/items", "/static/%2E%2e/api/items",
                "/static/..%2fapi/items", "/static//../api/items", "/static/.%2e/api/items",
                "/static\\..\\api/items", "/static/..;/api/items", "/static/..%3B/api/items",
                "/static/%252e%252e/api/items", "/static/a;x=1/../../api/items"].mapM fun p => h (request p)
  -- WebSocket upgrades behind the same sign-in.
  let wsInner : User → LeanTea.Net.WebSocketServer.Router := fun u _ =>
    pure (some (.error (Response.text 200 s!"ws for {u.login}")))
  let wsr := gateRouter g sessions wsInner
  let wsOutcome := fun (r : Option (Except Response LeanTea.Net.WebSocketServer.Session)) =>
    match r with | some (.error resp) => resp.status | some (.ok _) => 101 | none => 0
  let wsHost : Array (String × String) := #[("host", "app.test"), ("origin", "https://app.test")]
  let wsOk ← wsr (request "/ws" "" [("session", s)] (headers := wsHost))
  let wsAnon ← wsr (request "/ws" (headers := wsHost))
  let wsCross ← wsr (request "/ws" "" [("session", s)]
    (headers := #[("host", "app.test"), ("origin", "https://evil.test")]))
  let wsSecure := gateRouter { g with cookieSecure := true } sessions wsInner
  let wsHttpOnSecure ← wsSecure (request "/ws" "" [("session", s)]
    (headers := #[("host", "app.test"), ("origin", "http://app.test")]))
  let wsHttpsOnSecure ← wsSecure (request "/ws" "" [("session", s)]
    (headers := #[("host", "app.test"), ("origin", "https://app.test")]))
  let wsProxied := gateRouter g sessions wsInner (origins := some ["https://app.test"])
  let wsBehindProxy ← wsProxied (request "/ws" "" [("session", s)]
    (headers := #[("host", "127.0.0.1:9107"), ("origin", "https://app.test")]))
  let wsProxiedHttp ← wsProxied (request "/ws" "" [("session", s)]
    (headers := #[("host", "127.0.0.1:9107"), ("origin", "http://app.test")]))
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
    it "an unreachable upstream is 502" (down.status == 502),
    it "underscore spellings of identity headers are dropped (public path)"
      (pubUnderscore.status == 200 && seen pubUnderscore "headers.x-forwarded_user" == none
        && seen pubUnderscore "headers.x_real_ip" == none),
    it "underscore spellings of identity headers are dropped (signed in)"
      (seen getUnderscore "headers.x-forwarded_user" == none
        && seen getUnderscore "headers.x-forwarded-user" == some "alice"),
    it "a path under a public prefix is forwarded without a session"
      (static.status == 200 && seen static "path" == some "/static/app.css"),
    it "dot segments, encoded dots and slashes never make a path public"
      (tricks.all fun r => r.status != 200 && seen r "method" == none),
    it "canonicalPath? decodes plain escapes and refuses tricks"
      (canonicalPath? "/static/%61pp.css" == some "/static/app.css" &&
        canonicalPath? "/static/" == some "/static/" && canonicalPath? "/" == some "/" &&
        canonicalPath? "/static/../x" == none && canonicalPath? "/a%2fb" == none &&
        canonicalPath? "/a%zz" == none && canonicalPath? "a/b" == none &&
        canonicalPath? "/a//b" == none && canonicalPath? "/a%00" == none &&
        canonicalPath? "/static/..;/x" == none && canonicalPath? "/static/%2525" == none),
    it "gateRouter: a signed-in same-origin upgrade reaches inner with the user"
      (wsOutcome wsOk == 200),
    it "gateRouter: an upgrade without a session is 401" (wsOutcome wsAnon == 401),
    it "gateRouter: a cross-origin upgrade with the user's cookie is 403" (wsOutcome wsCross == 403),
    it "gateRouter: with cookieSecure, an http:// origin on the same host is 403"
      (wsOutcome wsHttpOnSecure == 403 && wsOutcome wsHttpsOnSecure == 200),
    it "gateRouter: an origin list works behind a proxy that rewrites Host, scheme included"
      (wsOutcome wsBehindProxy == 200 && wsOutcome wsProxiedHttp == 403)
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

/-! ## Group 5 — the streaming proxy: per-login upstreams on Unix sockets

Two upstreams listen on Unix sockets (`serveUnix`), one per user, and
the proxy runs as a `Server.Takeover` with a routing table. Sessions are
made directly (the sign-in itself is group 4's), and the proxy is driven
with raw bytes over TCP, so streaming and the WebSocket tunnel are seen
as a browser sees them. -/

namespace RelayGroup

open GateGroup
open LeanTea.Net
open LeanTea.Auth.Proxy

def proxyPort : UInt16 := 18780
def dir : System.FilePath := ".lake/build/relay_test"

/-- Answers who it is and what it was sent. -/
def echoAs (name : String) : Handler := fun req =>
  pure <| Response.text 200
    s!"{name}|{req.path}|{(req.header? "x-forwarded-user").getD ""}|{(req.header? "host").getD ""}|{(req.header? "cookie").getD ""}"

/-- `/slow` answers in two parts, 600 ms apart, with no length. -/
def slow : Server.Takeover := fun req => pure <|
  if req.path == "/slow" then some fun c _ => do
    c.send "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nConnection: close\r\n\r\nfirst\n".toUTF8
    IO.sleep 600
    c.send "second\n".toUTF8
  -- An interim 103 before the answer, in one write or two.
  else if req.path == "/hints" then some fun c _ =>
    c.send ("HTTP/1.1 103 Early Hints\r\nLink: </a.css>; rel=preload\r\n\r\n" ++
      "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok").toUTF8
  else if req.path == "/hints-split" then some fun c _ => do
    c.send "HTTP/1.1 103 Early Hints\r\nLink: </a.css>; rel=preload\r\n\r\n".toUTF8
    IO.sleep 100
    c.send "HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-".toUTF8
    IO.sleep 100
    c.send "Length: 2\r\n\r\nok".toUTF8
  else none

/-- WebSocket echo on `/stream`, prefixed with the upstream's name. -/
def wsEcho (name : String) : WebSocketServer.Router := fun req => pure <|
  if req.path != "/stream" then none else some (.ok fun conn => do
    repeat
      match ← conn.recv with
      | some (.text "bye") => break  -- the upstream ends the session
      | some (.text "spam") =>
        -- Sends until the other end is gone.
        repeat
          let ok ← try conn.sendText "x"; pure true catch _ => pure false
          if !ok then break
          IO.sleep 5
        break
      | some (.text s) => conn.sendText s!"{name}:{s}"
      | some (.binary b) => conn.sendBinary b
      | none => break)

def dial : IO Transport := do
  let s ← Std.Async.TCP.Socket.Client.mk
  (s.connect (.v4 { addr := Server.parseIPv4 "127.0.0.1", port := proxyPort })).block
  Transport.ofTcp s

/-- Read until the peer closes or `ms` pass. -/
partial def readFor (t : Transport) (ms : Nat) : IO ByteArray := do
  let deadline := (← IO.monoMsNow) + ms
  let rec go (acc : ByteArray) : IO ByteArray := do
    match ← t.recv deadline with
    | some b => go (acc ++ b)
    | none => return acc
  go .empty

def text (b : ByteArray) : String := (String.fromUTF8? b).getD ""

/-- One request; the whole answer as text. -/
def ask (path : String) (cookie : String := "") (extra : String := "") (method := "GET") : IO String := do
  let t ← dial
  let c := if cookie.isEmpty then "" else s!"Cookie: session={cookie}; theme=dark\r\n"
  t.send s!"{method} {path} HTTP/1.1\r\nHost: 127.0.0.1:{proxyPort}\r\n{c}{extra}\r\n".toUTF8
  let r := text (← readFor t 3000)
  t.close
  return r

def statusOf (r : String) : String := ((r.splitOn " ").getD 1 "").take 3 |>.toString
def bodyOf (r : String) : String := (r.splitOn "\r\n\r\n").getD 1 ""

def wsHandshake (cookie origin : String) : String :=
  s!"GET /stream HTTP/1.1\r\nHost: 127.0.0.1:{proxyPort}\r\nUpgrade: websocket\r\n" ++
  "Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n" ++
  "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ++
  s!"Origin: {origin}\r\nCookie: session={cookie}\r\n\r\n"

def userOf (login : String) : User :=
  { provider := "test", login, sub := login, email := login, name := login, picture := "" }

/-- A connected pair of TCP transports on `port` (ours, the server's). -/
def tcpPair (port : UInt16) : IO (Transport × Transport) := do
  let srv ← Std.Async.TCP.Socket.Server.mk
  srv.bind (.v4 { addr := Server.parseIPv4 "127.0.0.1", port })
  srv.listen 1
  let acc ← IO.asTask (prio := .dedicated) (srv.accept).block
  let c ← Std.Async.TCP.Socket.Client.mk
  (c.connect (.v4 { addr := Server.parseIPv4 "127.0.0.1", port })).block
  let s ← IO.ofExcept (← IO.wait acc)
  return (← Transport.ofTcp c, ← Transport.ofTcp s)

/-- A connected pair of Unix transports through a socket in `dir`. -/
def unixPair (dir : System.FilePath) (name : String) : IO (Transport × Transport) := do
  let path := (dir / name).toString
  let l ← Unix.listen path 0o600
  let acc ← IO.asTask (prio := .dedicated) (Unix.accept l)
  let c ← Unix.connect path
  let s ← IO.ofExcept (← IO.wait acc)
  Unix.close l
  return (← Transport.ofUnix c, ← Transport.ofUnix s)

/-- How long `t` took to finish, waiting at most `ms` (then `ms`). -/
def finishesIn (t : Task (Except IO.Error Unit)) (ms : Nat) : IO Nat := do
  let t0 ← IO.monoMsNow
  while !(← IO.hasFinished t) && (← IO.monoMsNow) - t0 < ms do IO.sleep 10
  return (← IO.monoMsNow) - t0

/-- `close` wakes a task waiting in `recv`, on TCP and on a Unix socket:
    whether the receive was still waiting just before, and how long it
    took to return after. The peer stays open throughout (dropping its
    last reference would close it and end the receive by itself). -/
def closeWakes (sockDir : System.FilePath) : IO ((Bool × Nat) × (Bool × Nat)) := do
  let (tPeer, tSrv) ← tcpPair 18781
  let (uPeer, uSrv) ← unixPair sockDir "pair.sock"
  let measure (peer t : Transport) : IO (Bool × Nat) := do
    let r ← IO.asTask (prio := .dedicated) (do let _ ← t.recv 0; pure ())
    IO.sleep 100
    let waiting := !(← IO.hasFinished r)
    let t0 ← IO.monoMsNow
    t.close
    let _ ← finishesIn r 3000
    let ms := (← IO.monoMsNow) - t0
    peer.close
    return (waiting, ms)
  return (← measure tPeer tSrv, ← measure uPeer uSrv)

def upgradeRequest : Request := {
  method := "GET", path := "/stream", query := "", body := .empty, version := "HTTP/1.1"
  headers := #[("host", "127.0.0.1"), ("upgrade", "websocket"), ("connection", "Upgrade"),
               ("sec-websocket-version", "13"), ("sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ==")] }

/-- Run `relay` for a WebSocket upgrade on `srv`, read the 101 on `ours`,
    then `act`; how long the relay took to end after that (at most 3 s). -/
def tunnelEnds (g : GateConfig) (aSock : System.FilePath) (ours srv : Transport)
    (act : IO Unit) : IO (Bool × Nat) := do
  let r ← IO.asTask (prio := .dedicated)
    (relay { routes := [] } g (some (userOf "alice@example.com")) (.unix aSock.toString)
      upgradeRequest srv .empty)
  let head ← ours.recv ((← IO.monoMsNow) + 3000)
  let ok := (String.fromUTF8? (head.getD .empty)).any (fun h => (h.splitOn "101").length > 1)
  act
  let ms ← finishesIn r 3000
  -- Our end stays referenced until now, so it is not closed by being freed.
  ours.close
  return (ok, ms)

def frame (s : String) : ByteArray :=
  WebSocketFrame.encode WebSocketFrame.opText s.toUTF8 (mask := some (ByteArray.mk #[1, 2, 3, 4]))

def run : IO LSpec := do
  IO.FS.createDirAll dir
  -- A socket path must fit in 108 bytes; a build directory may be deeper.
  let sockDir ← IO.FS.createTempDir
  let aSock := sockDir / "alice.sock"
  let bSock := sockDir / "bob.sock"
  let _ ← IO.asTask (prio := .dedicated)
    (Server.serveUnix aSock.toString 0o660 (echoAs "alice") (ws := wsEcho "alice") (takeover := slow))
  let _ ← IO.asTask (prio := .dedicated)
    (Server.serveUnix bSock.toString 0o600 (echoAs "bob") (ws := wsEcho "bob"))
  let table := s!"# who            where     upstream\n" ++
    s!"alice@example.com  /        unix:{aSock}\n" ++
    s!"alice@example.com  /stream  unix:{aSock}\n" ++
    s!"bob@example.com    /        unix:{bSock}\n" ++
    s!"*                  /mcp     deny\n" ++
    s!"*                  /health  unix:{bSock}\n"
  let routes ← IO.ofExcept (parseRoutes table)
  let cfg ← GateGroup.idpConfig
  let g : GateConfig := {
    client := cfg, publicPaths := ["/health"]
    allowedLogins := ["alice@example.com", "bob@example.com", "carol@example.com"] }
  let sessions ← signedSessions "relay-test-secret-0123456789abcdef".toUTF8
  let _ ← IO.asTask (prio := .dedicated) (Server.serveConcurrent proxyPort "127.0.0.1"
    (fun _ => pure Response.notFound) (takeover := takeover { routes } g sessions))
  IO.sleep 300
  let exp := (← wallSec) + 3600
  let alice ← sessions.create (userOf "alice@example.com") exp
  let bob ← sessions.create (userOf "bob@example.com") exp
  let carol ← sessions.create (userOf "carol@example.com") exp

  let aGet ← ask "/api/x" alice "X-Forwarded-User: mallory\r\nX-Forwarded_User: mallory\r\n"
  let bGet ← ask "/api/x" bob
  let cGet ← ask "/api/x" carol
  let mcp ← ask "/mcp" alice
  let mcpSub ← ask "/mcp/tools" alice
  let dotted ← ask "/x/../mcp" alice
  let anonApi ← ask "/api/x"
  let anonPage ← ask "/desktop"
  let health ← ask "/health"
  let post ← ask "/api/x" alice "Origin: https://evil.test\r\nContent-Length: 0\r\n" "POST"
  let postSame ← ask "/api/x" alice s!"Origin: http://127.0.0.1:{proxyPort}\r\nContent-Length: 0\r\n" "POST"
  -- Streaming: the first part arrives before the upstream sends the second.
  let t ← dial
  let t0 ← IO.monoMsNow
  t.send s!"GET /slow HTTP/1.1\r\nHost: x\r\nCookie: session={alice}\r\n\r\n".toUTF8
  let first ← t.recv ((← IO.monoMsNow) + 3000)
  let firstMs := (← IO.monoMsNow) - t0
  let rest ← readFor t 3000
  let allMs := (← IO.monoMsNow) - t0
  t.close
  let slowText := text ((first.getD .empty) ++ rest)
  -- WebSocket through the tunnel.
  let w ← dial
  w.send (wsHandshake alice s!"http://127.0.0.1:{proxyPort}").toUTF8
  let head ← w.recv ((← IO.monoMsNow) + 3000)
  let mask := ByteArray.mk #[1, 2, 3, 4]
  w.send (WebSocketFrame.encode WebSocketFrame.opText "hello".toUTF8 (mask := some mask))
  let reply ← w.recv ((← IO.monoMsNow) + 3000)
  let echoed := match WebSocketFrame.decode (reply.getD .empty) with
    | .frame f _ => text f.payload
    | _ => ""
  w.close
  let wsCross ← ask "/stream" alice
    s!"Upgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nOrigin: https://evil.test\r\n"
  let bobWs ← ask "/stream" bob
    s!"Upgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nOrigin: http://127.0.0.1:{proxyPort}\r\n"
  -- The Unix client and the socket's mode.
  let direct ← HttpClient.requestUnix aSock.toString "GET" "/direct"
  let modeA ← IO.Process.output { cmd := "stat", args := #["-c", "%a", aSock.toString] }
  let modeB ← IO.Process.output { cmd := "stat", args := #["-c", "%a", bSock.toString] }
  -- A file that is not a socket is never replaced.
  let plain := dir / "plain.sock"
  IO.FS.writeFile plain "keep"
  let refused ← try Server.serveUnix plain.toString (handler := echoAs "x"); pure false
    catch _ => pure true
  let kept ← IO.FS.readFile plain
  -- How the tunnel ends.
  let ((tcpWaiting, tcpWake), (unixWaiting, unixWake)) ← closeWakes sockDir
  let (o1, p1) ← tcpPair 18782
  let (up1, upFirst) ← tunnelEnds g aSock o1 p1 (o1.send (frame "bye"))
  let (o2, p2) ← tcpPair 18783
  let (up2, clientFirst) ← tunnelEnds g aSock o2 p2 o2.close
  let (o3, p3) ← unixPair sockDir "spam.sock"
  let (up3, sendFails) ← tunnelEnds g aSock o3 p3 do
    o3.send (frame "spam")
    let _ ← o3.recv ((← IO.monoMsNow) + 3000)
    o3.close
  -- Interim responses.
  let hints ← ask "/hints" alice
  let hintsSplit ← ask "/hints-split" alice
  try IO.FS.removeDirAll sockDir catch _ => pure ()
  return group "streaming proxy (Unix upstreams, routing table)" [
    it "alice reaches her upstream; her login replaces the spoofed ones"
      (statusOf aGet == "200" && bodyOf aGet == "alice|/api/x|alice@example.com|localhost|theme=dark") aGet,
    it "bob reaches his own upstream" (bodyOf bGet == "bob|/api/x|bob@example.com|localhost|theme=dark") bGet,
    it "a signed-in login the table does not route gets 403" (statusOf cGet == "403") cGet,
    it "a deny line refuses its prefix for everyone" (statusOf mcp == "403" && statusOf mcpSub == "403"),
    it "a dot segment cannot step into a denied prefix (400)" (statusOf dotted == "400") dotted,
    it "without a session: 401 for the API, a redirect for a page"
      (statusOf anonApi == "401" && statusOf anonPage == "302"),
    it "a public path goes to the `*` route with no X-Forwarded-User"
      (bodyOf health == "bob|/health||localhost|") health,
    it "a cross-origin POST is 403, a same-origin one passes"
      (statusOf post == "403" && statusOf postSame == "200") s!"{post} / {postSame}",
    it s!"a response is relayed as it arrives (first part {firstMs} ms, all {allMs} ms)"
      (firstMs < 450 && allMs ≥ 550 && hasSubstr slowText "first\n" && hasSubstr slowText "second\n") slowText,
    it "a WebSocket upgrade becomes a tunnel to the user's upstream"
      (hasSubstr (text (head.getD .empty)) "101 Switching Protocols" && echoed == "alice:hello") echoed,
    it "a cross-origin WebSocket upgrade is 403" (statusOf wsCross == "403") wsCross,
    it "bob's /stream goes to bob's upstream, not alice's" (statusOf bobWs == "101") bobWs,
    it "HttpClient.requestUnix talks to a serveUnix server"
      (direct.status == 200 && direct.bodyText == "alice|/direct||localhost|"),
    it "serveUnix sets the socket's mode"
      (modeA.stdout.trimAscii.toString == "660" && modeB.stdout.trimAscii.toString == "600"),
    it "serveUnix does not replace a file that is not a socket" (refused && kept == "keep"),
    it "prefixes match whole segments" (prefixCovers "/stream" "/stream/x" &&
      !prefixCovers "/stream" "/streams" && prefixCovers "/" "/anything" && prefixCovers "/a/" "/a"),
    it "the longest prefix wins, and deny wins a tie"
      (pick routes (some "alice@example.com") "/stream/x" == some (.unix aSock.toString) &&
        pick [{ login := "a", pathPrefix := "/x", target := .tcp "127.0.0.1" 1 },
              { login := "*", pathPrefix := "/x", target := .deny }] (some "a") "/x" == some .deny &&
        pick routes none "/api" == none),
    it "a malformed table is refused with its line number"
      (match parseRoutes "a / unix:/s\nb /x" with | .error e => hasSubstr e "line 2" | .ok _ => false),
    it s!"close wakes a waiting recv (TCP {tcpWake} ms, Unix {unixWake} ms)"
      (tcpWaiting && unixWaiting && tcpWake < 500 && unixWake < 500),
    it s!"the tunnel ends when the upstream closes first ({upFirst} ms)" (up1 && upFirst < 2000),
    it s!"the tunnel ends when the client closes first ({clientFirst} ms)" (up2 && clientFirst < 2000),
    it s!"the tunnel ends when sending to the client fails ({sendFails} ms)" (up3 && sendFails < 2000),
    it "an interim 103 is passed on and the final answer follows (one write)"
      (hasSubstr hints "103 Early Hints" && hasSubstr hints "HTTP/1.1 200 OK" &&
        hints.endsWith "\r\n\r\nok") hints,
    it "interim 103 and 100, then the answer, over several writes"
      (hasSubstr hintsSplit "103 Early Hints" && hasSubstr hintsSplit "100 Continue" &&
        hasSubstr hintsSplit "HTTP/1.1 200 OK" && hintsSplit.endsWith "\r\n\r\nok") hintsSplit,
    it "a trailing slash does not let a route beat deny on the same prefix"
      (let allow : Route := { login := "alice", pathPrefix := "/mcp/", target := .tcp "127.0.0.1" 1234 }
       let deny : Route := { login := "*", pathPrefix := "/mcp", target := .deny }
       [[deny, allow], [allow, deny]].all fun rs =>
         ["/mcp", "/mcp/", "/mcp/tools"].all fun p => pick rs (some "alice") p == some .deny),
    it "targets: unix needs an absolute path, TCP an IPv4 address"
      (Target.parse? "unix:rel" == none && Target.parse? "example.com:80" == none &&
        Target.parse? "http://127.0.0.1:8080/" == some (.tcp "127.0.0.1" 8080))
  ]

/-- With no `login`, the e-mail address is the user name, but only when
    the provider says it verified it. -/
def emailLogin : IO LSpec := do
  let cfg ← GateGroup.idpConfig
  let sessions ← signedSessions "relay-test-secret-0123456789abcdef".toUTF8
  let base : GateConfig := { client := cfg, allowedLogins := ["alice@example.com", "bob@example.com"] }
  let as (id : String) : GateConfig := { base with client := { cfg with clientId := id } }
  let inner : Option User → Handler := fun u _ => pure (Response.text 200 ((u.map (·.login)).getD ""))
  let (_, bobCb) ← signIn (gate (as "verified-client") sessions inner)
  let (_, eveCb) ← signIn (gate (as "unverified-client") sessions inner)
  return group "e-mail as the login" [
    it "a verified address signs in as that address" (bobCb.status == 302 && (cookieSet bobCb "session").isSome),
    it "an unverified address (claiming an allowed user's) does not sign in"
      (eveCb.status == 403 && (cookieSet eveCb "session").isNone) (body eveCb)
  ]

end RelayGroup

/-! ## Group 2 — SAML 2.0 fixture round-trip -/

namespace SamlGroup

open LeanTea.Auth.Idp.Saml
open LeanTea.Auth.Saml (parseResponse)

/-- The XMLDSig template xmlsec1 fills in: an enveloped RSA-SHA256
    signature over the assertion `id`. -/
def sigTemplate (id : String) : String :=
  "<ds:Signature xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:SignedInfo>" ++
  "<ds:CanonicalizationMethod Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/>" ++
  "<ds:SignatureMethod Algorithm=\"http://www.w3.org/2001/04/xmldsig-more#rsa-sha256\"/>" ++
  s!"<ds:Reference URI=\"#{id}\"><ds:Transforms>" ++
  "<ds:Transform Algorithm=\"http://www.w3.org/2000/09/xmldsig#enveloped-signature\"/>" ++
  "<ds:Transform Algorithm=\"http://www.w3.org/2001/10/xml-exc-c14n#\"/></ds:Transforms>" ++
  "<ds:DigestMethod Algorithm=\"http://www.w3.org/2001/04/xmlenc#sha256\"/><ds:DigestValue/>" ++
  "</ds:Reference></ds:SignedInfo><ds:SignatureValue/></ds:Signature>"

def hasTool (cmd : String) : IO Bool := do
  try
    let out ← IO.Process.output { cmd, args := #["version"] }
    pure (out.exitCode == 0 || !out.stdout.isEmpty)
  catch _ => pure false

/-- Sign `xml` (with a template inside) with xmlsec1 using `key`/`cert`. -/
def xmlsecSign (dir : System.FilePath) (key cert xml : String) : IO String := do

  let lax := (← LeanTea.Auth.Saml.xmlsecVerifyArgs).contains "--lax-key-search"
  IO.FS.writeFile (dir / "tmpl.xml") xml
  let out ← IO.Process.output { cmd := "xmlsec1", args :=
    #["--sign"] ++ (if lax then #["--lax-key-search"] else #[]) ++
    #["--privkey-pem", s!"{key},{cert}", "--id-attr:ID", "Assertion",
      "--output", (dir / "signed.xml").toString, (dir / "tmpl.xml").toString] }
  if out.exitCode != 0 then throw <| IO.userError s!"xmlsec1 --sign: {out.stderr}"
  IO.FS.readFile (dir / "signed.xml")

/-- `Saml.validate` end to end on responses really signed by xmlsec1
    with a throwaway IdP key. Skipped when xmlsec1 or openssl is missing
    (CI installs both). -/
def xmlsecE2E : IO LSpec := do
  unless (← hasTool "xmlsec1") && (← hasTool "openssl") do
    return group "validate with xmlsec1 (skipped: xmlsec1 or openssl not installed)" []
  IO.FS.withTempDir fun dir => do
    for who in ["idp", "evil"] do
      let out ← IO.Process.output { cmd := "openssl", args := #["req", "-x509", "-newkey", "rsa:2048",
        "-nodes", "-days", "1", "-subj", s!"/CN={who}",
        "-keyout", (dir / s!"{who}.key").toString, "-out", (dir / s!"{who}.pem").toString] }
      if out.exitCode != 0 then throw <| IO.userError s!"openssl: {out.stderr}"
    let withSig := fun (f : Fixture) =>
      (buildResponse f).replace "</saml:Issuer>" ("</saml:Issuer>" ++ sigTemplate f.assertionId)
    let sign := fun (who : String) (f : Fixture) =>
      xmlsecSign dir (dir / s!"{who}.key").toString (dir / s!"{who}.pem").toString (withSig f)
    let cfg : LeanTea.Auth.Saml.Config := {
      spEntityId := "https://sp.example.com", idpCertPemPath := (dir / "idp.pem").toString,
      acsUrl := "https://sp.example.com/saml/acs", idpEntityId := "https://idp.example.com" }
    let b64 := fun (s : String) => LeanTea.Crypto.Base64.encode s.toUTF8
    let cache ← LeanTea.Auth.Saml.ReplayCache.new
    let good ← sign "idp" { assertionId := "_e2e1" }
    let first ← LeanTea.Auth.Saml.validate cfg (b64 good) (replay := cache)
    let replayed ← LeanTea.Auth.Saml.validate cfg (b64 good) (replay := cache)
    let evil ← sign "evil" { assertionId := "_e2e2", subjectEmail := "mallory@example.com" }
    let evilR ← LeanTea.Auth.Saml.validate cfg (b64 evil) (replay := cache)
    let tampered := good.replace "alice@example.com" "mallory@example.com"
    let tamperedR ← LeanTea.Auth.Saml.validate cfg (b64 tampered) (replay := cache)
    let expired ← sign "idp" { assertionId := "_e2e3", notOnOrAfter := "2020-01-01T00:00:00Z" }
    let expiredR ← LeanTea.Auth.Saml.validate cfg (b64 expired) (replay := cache)
    -- Forged identity inside the signature element (outside the digest).
    let forgedSubject := "<saml:Subject><saml:NameID>admin@example.com</saml:NameID>" ++
      "<saml:SubjectConfirmation Method=\"urn:oasis:names:tc:SAML:2.0:cm:bearer\">" ++
      "<saml:SubjectConfirmationData NotOnOrAfter=\"2099-01-01T00:00:00Z\" Recipient=\"https://sp.example.com/saml/acs\"/>" ++
      "</saml:SubjectConfirmation></saml:Subject>"
    let objSigned ← sign "idp" { assertionId := "_e2e4" }
    let objInjected := objSigned.replace "</ds:Signature>"
      ("<ds:Object>" ++ forgedSubject ++ "</ds:Object></ds:Signature>")
    let objR ← LeanTea.Auth.Saml.validate cfg (b64 objInjected) (replay := cache)
    -- A processing-instruction pair that a comment stripper would read as
    -- a comment around the real assertion, followed by a forged one.
    let piSigned ← sign "idp" { assertionId := "_e2e5" }
    -- A complete assertion (audience, conditions, bearer data) for
    -- admin, with a fake non-dsig signature naming its own ID.
    let forgedAssertion :=
      ((buildResponse { assertionId := "_f", subjectEmail := "admin@example.com" })
        |>.replace "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">" ""
        |>.replace "</samlp:Response>" "")
        |>.replace "</saml:Assertion>" "<Signature><Reference URI=\"#_f\"/></Signature></saml:Assertion>"
    let piInjected := (piSigned.replace "<saml:Assertion " "<?x <!--?><saml:Assertion ").replace
      "</saml:Assertion>" ("</saml:Assertion><?y -->?>" ++ forgedAssertion)
    let piR ← LeanTea.Auth.Saml.validate cfg (b64 piInjected) (replay := cache)
    -- Signed with another key that the document carries as a certificate.
    let x509Template := fun (f : Fixture) =>
      (buildResponse f).replace "</saml:Issuer>" ("</saml:Issuer>" ++
        (sigTemplate f.assertionId).replace "<ds:SignatureValue/>"
          "<ds:SignatureValue/><ds:KeyInfo><ds:X509Data><ds:X509Certificate/></ds:X509Data></ds:KeyInfo>")
    let embedded ← xmlsecSign dir (dir / "evil.key").toString (dir / "evil.pem").toString
      (x509Template { assertionId := "_e2e6" })
    let embeddedR ← LeanTea.Auth.Saml.validate cfg (b64 embedded) (replay := cache)
    let isOk := fun (r : Except String LeanTea.Auth.Saml.Assertion) => match r with
      | .ok a => a.nameId == "alice@example.com" | .error _ => false
    let isErr := fun (r : Except String LeanTea.Auth.Saml.Assertion) => match r with
      | .ok _ => false | .error _ => true
    let detail := fun (r : Except String LeanTea.Auth.Saml.Assertion) => match r with
      | .ok a => s!"ok {a.nameId}" | .error e => e
    return group "validate with xmlsec1 (really signed responses)" [
      it "a response signed by the IdP is accepted" (isOk first) (detail first),
      it "the same response a second time is refused (replay)" (isErr replayed) (detail replayed),
      it "a response signed by another key is refused" (isErr evilR) (detail evilR),
      it "a signed response edited afterwards is refused" (isErr tamperedR) (detail tamperedR),
      it "a signed but expired response is refused" (isErr expiredR) (detail expiredR),
      it "an identity planted inside <ds:Signature> is never read"
        (match objR with | .ok a => a.nameId == "alice@example.com" | .error _ => true) (detail objR),
      it "a processing-instruction wrapping is refused" (isErr piR) (detail piR),
      it "a response signed by another key carried in the document is refused"
        (isErr embeddedR) (detail embeddedR)]

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
  /- 5. Validity window, bearer confirmation, recipient, InResponseTo,
     issuer — `checkAssertion` at a fixed clock (2030-01-01T00:00:00Z). -/
  let signed := fun (f : Fixture) =>
    (buildResponse f).replace "</saml:Assertion>" (sigFor f.assertionId ++ "</saml:Assertion>")
  let cfg : LeanTea.Auth.Saml.Config := {
    spEntityId := "https://sp.example.com", idpCertPemPath := "",
    acsUrl := "https://sp.example.com/saml/acs", idpEntityId := "https://idp.example.com" }
  let now : Int := 1893456000
  let accepts := fun (f : Fixture) (irt : Option String) =>
    match LeanTea.Auth.Saml.checkAssertion cfg now (signed f) irt with
    | .ok _ => true | .error _ => false
  let ok := fun (f : Fixture) => accepts f none
  let cache ← LeanTea.Auth.Saml.ReplayCache.new
  let first ← cache.claim "_r1" (now + 300) now
  let again ← cache.claim "_r1" (now + 300) now
  let later ← cache.claim "_r1" (now + 900) (now + 301)
  /- 6. Markup the signature check never sees: comments, DOCTYPE, CDATA,
     and a forged assertion under another prefix. -/
  let forgedIn := "<saml:Assertion ID=\"_a1\"><saml:Issuer>https://idp.example.com</saml:Issuer>" ++
    "<saml:Subject><saml:NameID>mallory@example.com</saml:NameID></saml:Subject></saml:Assertion>"
  let commented := (signed {}).replace "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">"
    ("<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\"><!--" ++ forgedIn ++ "-->")
  let commentedR := LeanTea.Auth.Saml.checkAssertion cfg now commented
  let saml2 := ((signed {}).replace "<saml:" "<saml2:").replace "</saml:" "</saml2:"
  let prefixed := saml2.replace "<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">"
    ("<samlp:Response xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\">" ++ forgedIn)
  let refusedRaw := fun (xml : String) =>
    match LeanTea.Auth.Saml.checkAssertion cfg now xml with | .ok _ => false | .error _ => true
  let e2e ← xmlsecE2E
  let guard := fun (xml : String) =>
    match parseResponse xml with
    | .ok a => match LeanTea.Auth.Saml.checkNoWrapping xml a with
               | .ok _ => true | .error _ => false
    | .error _ => false
  return group "SAML 2.0 fixture round-trip" [
    group "validity window and confirmation (checkAssertion at a fixed clock)" [
      it "a current assertion for this SP is accepted" (ok {}),
      it "NotOnOrAfter two minutes ago is refused"
        (!ok { notOnOrAfter := "2029-12-31T23:58:00Z" }),
      it "NotOnOrAfter 30 s ago is within the 60 s leeway"
        (ok { notOnOrAfter := "2029-12-31T23:59:30Z" }),
      it "NotBefore five minutes ahead is refused"
        (!ok { notBefore := "2030-01-01T00:05:00Z" }),
      it "a missing NotOnOrAfter is refused" (!ok { notOnOrAfter := "" }),
      it "an unreadable NotOnOrAfter is refused" (!ok { notOnOrAfter := "tomorrow" }),
      it "a time without a zone is refused" (!ok { notOnOrAfter := "2099-06-17T01:00:00" }),
      it "no bearer SubjectConfirmation is refused" (!ok { bearer := false }),
      it "an expired SubjectConfirmationData is refused"
        (!ok { confirmNotOnOrAfter := "2029-12-31T23:50:00Z" }),
      it "a Recipient other than this SP's ACS URL is refused"
        (!ok { recipient := "https://other-sp.example.com/acs" }),
      it "InResponseTo must match the request the SP sent"
        (accepts { inResponseTo := "_req1" } (some "_req1") &&
          !accepts { inResponseTo := "_req1" } (some "_req2") &&
          !accepts {} (some "_req1")),
      it "another issuer is refused" (!ok { issuer := "https://evil.example.com" }),
      it "another audience is refused" (!ok { audience := "https://other-sp.example.com" }),
      it "parseInstant: epoch, offsets and fractions"
        (LeanTea.Auth.Saml.parseInstant "1970-01-01T00:00:00Z" == some 0 &&
          LeanTea.Auth.Saml.parseInstant "2030-01-01T09:00:00+09:00" == some now &&
          LeanTea.Auth.Saml.parseInstant "2030-01-01T00:00:00.123Z" == some now &&
          LeanTea.Auth.Saml.parseInstant "2030-13-01T00:00:00Z" == none),
      it "ReplayCache accepts an ID once until it expires" (first && !again && later)
    ],
    group "markup the signature check never sees" [
      it "a forged assertion inside a comment is ignored, the real one is read"
        (match commentedR with | .ok (a, _) => a.nameId == "alice@example.com" | .error _ => false),
      it "a forged saml: assertion beside a real saml2: one is refused"
        (LeanTea.Auth.Saml.assertionCount prefixed == 2 && refusedRaw prefixed),
      it "withoutSignatures removes signature elements of any prefix"
        (match LeanTea.Auth.Saml.withoutSignatures "<a><ds:Signature><x/></ds:Signature><b/><dsig:Signature/>c</a>" with
          | .ok r => r == "<a><b/>c</a>" | .error _ => false),
      it "parseInstant refuses impossible dates and an empty fraction"
        (LeanTea.Auth.Saml.parseInstant "2026-02-31T00:00:00Z" == none &&
          LeanTea.Auth.Saml.parseInstant "2028-02-29T00:00:00Z" != none &&
          LeanTea.Auth.Saml.parseInstant "2026-01-01T00:00:00.Z" == none),
      it "processing instructions other than the XML declaration are refused"
        (refusedRaw ("<?x <!--?>" ++ signed {}) &&
          !refusedRaw ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++ signed {})),
      it "DOCTYPE, ENTITY, CDATA and unterminated comments are refused"
        (refusedRaw ("<!DOCTYPE r [<!ENTITY x \"y\">]>" ++ signed {}) &&
          refusedRaw ((signed {}).replace "alice@example.com" "<![CDATA[alice@example.com]]>") &&
          refusedRaw ((signed {}) ++ "<!-- open"))
    ],
    e2e,
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
    let s5 ← RelayGroup.run
    let s6 ← RelayGroup.emailLogin
    let tree := group "LeanTEA auth integration" [s1, s3, s4, s5, s6, s2]
    let code ← lspecIO tree
    let _ ← child.kill
    -- exit explicitly: a returning `main` waits for the in-process servers' tasks
    IO.Process.exit code.toUInt8
  catch e =>
    let _ ← child.kill
    IO.println s!"auth_spec: crashed → {e}"
    IO.Process.exit 1
