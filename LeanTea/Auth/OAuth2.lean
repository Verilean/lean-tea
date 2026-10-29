import LeanTea.Auth
import LeanTea.Persist.Store
import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Base64
import Lean.Data.Json
import Std.Time
import Std.Data.HashMap
import LeanTea.Crypto.Hmac

/-! # LeanTea.Auth.OAuth2 — provider-agnostic OAuth 2.0 client

Generalises `LeanTea.Auth`'s Google-specific flow to any RFC 6749
Authorization Code provider, with optional RFC 7636 PKCE.

```lean
let cfg := OAuth2.providers.google
              |>.withClient "<id>" "<secret>" "https://app.example.com/cb"
let st ← OAuth2.beginAuth cfg                 -- returns state + challenge
let url := OAuth2.authorizeUrl cfg st
-- … redirect user, get the callback …
let tok ← OAuth2.exchangeCode cfg code st
let info ← OAuth2.fetchUserInfo cfg tok.accessToken
```

Why a separate module? The original `Auth.lean` mixed Google
endpoints with session-store wiring; this one keeps the protocol
clean so JWT / OIDC verification can layer on top.

`gate` (at the end) puts a `Handler` behind sign-in with any of these
providers: login / callback / logout routes, state + PKCE, a session
cookie and an allow-list of user names. -/

namespace LeanTea.Auth.OAuth2

open LeanTea.Auth
open LeanTea.Crypto
open LeanTea.Net.Http
open LeanTea.Persist
open Lean (Json)

/-! ## Provider description -/

structure Provider where
  /-- Display label (e.g. "google", "github"). Used in URLs / cookies. -/
  name             : String
  authEndpoint     : String
  tokenEndpoint    : String
  userInfoEndpoint : String
  /-- Space-separated scopes. Provider-specific defaults. -/
  defaultScope     : String
  /-- True when the provider supports PKCE S256 (most do). -/
  supportsPkce     : Bool := true
  /-- Field name in token-endpoint JSON that holds the access token.
      Always `access_token` for RFC-conformant providers. -/
  accessField      : String := "access_token"
  /-- Field for ID token when OpenID Connect (`openid` scope) is in
      use. Empty if the provider isn't OIDC-shaped. -/
  idField          : String := "id_token"
  deriving Inhabited, Repr

/-- Pre-baked providers. Add more as needed. -/
def providers.google : Provider :=
  { name             := "google"
  , authEndpoint     := "https://accounts.google.com/o/oauth2/v2/auth"
  , tokenEndpoint    := "https://oauth2.googleapis.com/token"
  , userInfoEndpoint := "https://www.googleapis.com/oauth2/v3/userinfo"
  , defaultScope     := "openid email profile" }

def providers.github : Provider :=
  { name             := "github"
  , authEndpoint     := "https://github.com/login/oauth/authorize"
  , tokenEndpoint    := "https://github.com/login/oauth/access_token"
  , userInfoEndpoint := "https://api.github.com/user"
  , defaultScope     := "read:user user:email"
  , idField          := "" }     -- GitHub OAuth isn't OIDC

def providers.microsoft : Provider :=
  { name             := "microsoft"
  , authEndpoint     := "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
  , tokenEndpoint    := "https://login.microsoftonline.com/common/oauth2/v2.0/token"
  , userInfoEndpoint := "https://graph.microsoft.com/oidc/userinfo"
  , defaultScope     := "openid email profile" }

/-! ## Client config -/

structure Config where
  provider     : Provider
  clientId     : String
  clientSecret : String      -- ignored for public PKCE-only clients
  redirectUri  : String
  scope        : String      -- override `provider.defaultScope`
  usePkce      : Bool := true
  deriving Inhabited, Repr

def Provider.withClient (p : Provider) (clientId clientSecret redirectUri : String)
    (scope : Option String := none) (usePkce : Bool := true) : Config :=
  { provider := p
  , clientId
  , clientSecret
  , redirectUri
  , scope := scope.getD p.defaultScope
  , usePkce := usePkce && p.supportsPkce }

/-! ## State + PKCE -/

structure AuthState where
  /-- Anti-CSRF token round-tripped through the provider. -/
  state         : String
  /-- High-entropy verifier kept *server-side*. The challenge sent
      to the provider is its SHA-256, base64url-encoded. -/
  codeVerifier  : String
  deriving Inhabited, Repr

/-- Generate a fresh `(state, code_verifier)` pair. Both come from
    `/dev/urandom`. -/
def beginAuth (_ : Config) : IO AuthState := do
  let st ← randomToken            -- 64-hex (~32 bytes entropy)
  let ver ← randomToken
  return { state := st, codeVerifier := ver }

/-- Base64url(SHA-256(verifier)) — the S256 PKCE challenge. -/
def pkceChallenge (verifier : String) : String :=
  Base64.encodeUrl (Sha256.hashString verifier)

/-- Build the authorize URL the user is redirected to. -/
def authorizeUrl (cfg : Config) (st : AuthState) : String :=
  let base :=
    cfg.provider.authEndpoint ++ "?" ++
    "client_id=" ++ urlEncode cfg.clientId ++
    "&redirect_uri=" ++ urlEncode cfg.redirectUri ++
    "&response_type=code" ++
    "&scope=" ++ urlEncode cfg.scope ++
    "&state=" ++ urlEncode st.state
  if cfg.usePkce then
    base ++
    "&code_challenge=" ++ pkceChallenge st.codeVerifier ++
    "&code_challenge_method=S256"
  else base

/-! ## Token exchange -/

structure TokenSet where
  accessToken : String
  idToken     : String := ""
  tokenType   : String := ""
  expiresIn   : Nat := 0
  scope       : String := ""
  raw         : String := ""   -- raw JSON for debugging
  deriving Inhabited, Repr

private def jStr (j : Json) (k : String) : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD ""

private def jNat (j : Json) (k : String) : Nat :=
  match (j.getObjVal? k).toOption with
  | some v => (v.getNat?.toOption).getD 0
  | none   => 0

/-- POST authorization code → token. Provider-agnostic. -/
def exchangeCode (cfg : Config) (code : String) (st : AuthState)
    : IO (Except String TokenSet) := do
  let mut body :=
    "grant_type=authorization_code" ++
    "&code=" ++ urlEncode code ++
    "&client_id=" ++ urlEncode cfg.clientId ++
    "&redirect_uri=" ++ urlEncode cfg.redirectUri
  if !cfg.clientSecret.isEmpty then
    body := body ++ "&client_secret=" ++ urlEncode cfg.clientSecret
  if cfg.usePkce then
    body := body ++ "&code_verifier=" ++ urlEncode st.codeVerifier
  -- GitHub answers form-encoded unless asked for JSON; JSON suits everyone.
  let resp ← postForm cfg.provider.tokenEndpoint body #[("Accept", "application/json")]
  if resp.status != 200 then
    return .error s!"{cfg.provider.name} token endpoint: HTTP {resp.status}: {resp.body}"
  match Json.parse resp.body with
  | .error e => return .error s!"token JSON parse: {e}"
  | .ok j    =>
    let access := jStr j cfg.provider.accessField
    if access.isEmpty then
      return .error s!"no `{cfg.provider.accessField}` in: {resp.body}"
    return .ok
      { accessToken := access
      , idToken     := if cfg.provider.idField.isEmpty then ""
                       else jStr j cfg.provider.idField
      , tokenType   := jStr j "token_type"
      , expiresIn   := jNat j "expires_in"
      , scope       := jStr j "scope"
      , raw         := resp.body }

/-! ## User info -/

structure UserInfo where
  /-- Stable provider-issued ID. Falls back to email if missing. -/
  sub     : String
  /-- The user name: GitHub's `login`, OIDC's `preferred_username`,
      else the email. `gate`'s allow-list matches this. -/
  login   : String := ""
  email   : String
  name    : String
  picture : String
  raw     : String := ""
  deriving Inhabited, Repr

/-- Hit the provider's userinfo endpoint with the access token. -/
def fetchUserInfo (cfg : Config) (accessToken : String)
    : IO (Except String UserInfo) := do
  -- GitHub refuses `/user` calls without a User-Agent.
  let headers := #[("Authorization", s!"Bearer {accessToken}"),
                   ("Accept", "application/json"), ("User-Agent", "LeanTea-OAuth2")]
  let resp ← providerCall "GET" cfg.provider.userInfoEndpoint headers
  if resp.status != 200 then
    return .error s!"{cfg.provider.name} userinfo: HTTP {resp.status}: {resp.body}"
  match Json.parse resp.body with
  | .error e => return .error s!"userinfo JSON parse: {e}"
  | .ok j    =>
    let email := jStr j "email"
    let sub :=
      let s := jStr j "sub"
      if !s.isEmpty then s
      else
        -- GitHub uses `id` (numeric) instead of `sub`.
        let i := jNat j "id"
        if i > 0 then toString i else email
    -- GitHub's `name` field is `name`; picture is `avatar_url`.
    let picture :=
      let p := jStr j "picture"
      if !p.isEmpty then p else jStr j "avatar_url"
    let login :=
      [jStr j "login", jStr j "preferred_username", email].find? (!·.isEmpty) |>.getD ""
    return .ok { sub, login, email, name := jStr j "name", picture, raw := resp.body }

/-! ## Gate: sign-in in front of a `Handler`

```lean
let sessions := (← OAuth2.GateStore.attach (← Sqlite.open' "app.db")).sessions
-- or, with no database: ← OAuth2.signedSessions cookieSecret
let g : OAuth2.GateConfig := {
  client := OAuth2.providers.github.withClient id secret "https://app.example.com/auth/callback"
  allowedLogins := ["octo"], cookieSecure := true }
serve 8080 "0.0.0.0" (OAuth2.gate g sessions fun user => app user)
```

Routes: `loginPath` (default `/auth/login`, takes `?next=/path`), the
callback (the path of the client's `redirectUri`) and `logoutPath`
(`/auth/logout`). Everything else goes to `inner`, with the user when
signed in. Without a session, `publicPaths` / `publicPrefixes` still
reach `inner` (with `none`), `apiPrefixes` get 401, and other pages
redirect to the login route.

Where sessions live is a `Sessions` value; two come with the gate:

* `GateStore.sessions` — SQLite rows. Logout is final, even across
  restarts, and the PKCE verifier stays on the server. Only the SHA-256
  of a session token is stored, so a copy of the database holds no live
  cookies.
* `signedSessions secret` — no storage: the session and the login in
  progress are HMAC-SHA256-signed cookies. Logout is final while the
  process runs (ended sessions are remembered in memory until they
  expire); after a restart, a copied cookie works until its expiry.
  Rotating the secret ends every session.

Either way the allow-list is checked on every request, so a name taken
off the list loses its sessions at once.

The `state` must come back from the provider *and* match the short
cookie set when the login started (a login started in another browser
cannot be finished in this one), and the login may not be older than
`stateTtl`. -/

/-- Seconds since the Unix epoch. Wall-clock time, since sessions
    outlive a restart. -/
def wallSec : IO Nat := do
  return (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val.toNat

/-- The signed-in user, as `gate` hands it to the inner handler. -/
structure User where
  provider : String
  login    : String
  sub      : String
  email    : String
  name     : String
  picture  : String
  deriving Inhabited, Repr, BEq

structure SessionRow where
  tokenHash : String
  user      : User
  createdAt : Nat
  expiresAt : Nat

instance : Entity SessionRow where
  table := "oauth2_sessions"
  ddl :=
    "CREATE TABLE IF NOT EXISTS oauth2_sessions(" ++
    "token_hash TEXT PRIMARY KEY, provider TEXT NOT NULL, login TEXT NOT NULL," ++
    "sub TEXT NOT NULL, email TEXT NOT NULL, name TEXT NOT NULL, picture TEXT NOT NULL," ++
    "created_at INTEGER NOT NULL, expires_at INTEGER NOT NULL)"
  columns := ["token_hash", "provider", "login", "sub", "email", "name", "picture",
              "created_at", "expires_at"]
  toRow r := #[r.tokenHash, r.user.provider, r.user.login, r.user.sub, r.user.email,
               r.user.name, r.user.picture, toString r.createdAt, toString r.expiresAt]
  fromRow row :=
    match row.toList with
    | [h, p, l, s, e, n, pic, c, x] =>
      match c.toNat?, x.toNat? with
      | some c, some x =>
        .ok { tokenHash := h, createdAt := c, expiresAt := x,
              user := { provider := p, login := l, sub := s, email := e, name := n, picture := pic } }
      | _, _ => .error "SessionRow: integer parse failed"
    | _ => .error s!"SessionRow: expected 9 columns, got {row.size}"

/-- A login in progress: the state sent to the provider, the PKCE
    verifier kept here, and the page to return to. -/
structure PendingRow where
  state     : String
  verifier  : String
  next      : String
  createdAt : Nat

instance : Entity PendingRow where
  table := "oauth2_pending"
  ddl :=
    "CREATE TABLE IF NOT EXISTS oauth2_pending(" ++
    "state TEXT PRIMARY KEY, verifier TEXT NOT NULL, next TEXT NOT NULL," ++
    "created_at INTEGER NOT NULL)"
  columns := ["state", "verifier", "next", "created_at"]
  toRow r := #[r.state, r.verifier, r.next, toString r.createdAt]
  fromRow row :=
    match row.toList with
    | [s, v, n, c] =>
      match c.toNat? with
      | some c => .ok { state := s, verifier := v, next := n, createdAt := c }
      | none => .error "PendingRow: integer parse failed"
    | _ => .error s!"PendingRow: expected 4 columns, got {row.size}"

/-- Where `gate` keeps sessions and logins in progress. Cookie values
    are whatever the backend hands out. -/
structure Sessions where
  /-- Start a session for the user until `expiresAt`; returns the cookie value. -/
  create : User → (expiresAt : Nat) → IO String
  /-- The user of a session cookie, if it is live at `now`. -/
  find   : String → (now : Nat) → IO (Option User)
  /-- End a session. -/
  drop   : String → IO Unit
  /-- Remember a login in progress; returns the state cookie value. -/
  begin  : PendingRow → IO String
  /-- Use up the login in progress for a callback's `state` and this
      browser's state cookie; `none` when they do not belong together. -/
  take   : (state cookie : String) → IO (Option PendingRow)
  /-- Forget what ended before `now` (sessions) or started before `cutoff` (logins). -/
  purge  : (now cutoff : Nat) → IO Unit

structure GateStore where
  sessionRows : Repo SessionRow
  pendingRows : Repo PendingRow

/-- Create the two tables (if missing) on `db`. -/
def GateStore.attach (db : Sqlite.Db) : IO GateStore := do
  let s : GateStore := { sessionRows := Repo.new db, pendingRows := Repo.new db }
  s.sessionRows.migrate
  s.pendingRows.migrate
  return s

private def tokenHash (token : String) : String := Sha256.hashHex token

/-- Sessions as SQLite rows; the cookie is a random token. -/
def GateStore.sessions (s : GateStore) : Sessions where
  create user expiresAt := do
    let token ← randomToken
    let _ ← s.sessionRows.insert
      { tokenHash := tokenHash token, user, createdAt := 0, expiresAt }
    return token
  find token now := do
    let rows ← s.sessionRows.query
      "SELECT * FROM oauth2_sessions WHERE token_hash = ? AND expires_at > ?"
      #[tokenHash token, toString now]
    return rows[0]?.map (·.user)
  drop token := do
    let _ ← s.sessionRows.execRaw "DELETE FROM oauth2_sessions WHERE token_hash = ?" #[tokenHash token]
  begin p := do
    let _ ← s.pendingRows.insert p
    return p.state
  take state cookie := do
    if state.isEmpty || cookie != state then return none
    let rows ← s.pendingRows.query "SELECT * FROM oauth2_pending WHERE state = ?" #[state]
    let _ ← s.pendingRows.execRaw "DELETE FROM oauth2_pending WHERE state = ?" #[state]
    return rows[0]?
  purge now cutoff := do
    let _ ← s.sessionRows.execRaw "DELETE FROM oauth2_sessions WHERE expires_at <= ?" #[toString now]
    let _ ← s.pendingRows.execRaw "DELETE FROM oauth2_pending WHERE created_at < ?" #[toString cutoff]

/-! ### Signed cookies -/

/-- Equal, in time that depends only on the lengths. -/
private def sameBytes (a b : ByteArray) : Bool := Id.run do
  if a.size != b.size then return false
  let mut d : UInt8 := 0
  for i in [:a.size] do d := d ||| (a.get! i ^^^ b.get! i)
  return d == 0

/-- `base64url(json).base64url(hmac)`; `kind` keeps a state cookie from
    passing as a session cookie. -/
def signCookie (secret : ByteArray) (kind : String) (j : Json) : String :=
  let payload := Base64.encodeUrl j.compress.toUTF8
  payload ++ "." ++ Base64.encodeUrl (Hmac.sha256 secret (kind ++ "." ++ payload).toUTF8)

def verifyCookie (secret : ByteArray) (kind : String) (v : String) : Option Json := do
  let [payload, mac] := v.splitOn "." | none
  let expected := Hmac.sha256 secret (kind ++ "." ++ payload).toUTF8
  guard (sameBytes expected ((Base64.decodeUrl mac).getD .empty))
  let text ← String.fromUTF8? (← Base64.decodeUrl payload)
  (Json.parse text).toOption

private def userJson (u : User) (expiresAt : Nat) : Json :=
  Json.mkObj [("p", u.provider), ("l", u.login), ("s", u.sub), ("e", u.email),
              ("n", u.name), ("i", u.picture), ("x", expiresAt)]

/-- Sessions as signed cookies, with no storage. `secret` should be 32
    random bytes or more. Ended sessions are remembered (in memory) until
    they would have expired, so logout holds while the process runs. -/
def signedSessions (secret : ByteArray) : IO Sessions := do
  let ended ← IO.mkRef (∅ : Std.HashMap String Nat)
  return {
    create := fun user expiresAt => do
      -- a nonce, so two sessions of one user are told apart on logout
      let j := (userJson user expiresAt).setObjVal! "r" (← randomToken)
      return signCookie secret "session" j
    find := fun v now => do
      let some j := verifyCookie secret "session" v | return none
      if (← ended.get).contains (tokenHash v) then return none
      let x := jNat j "x"
      if x <= now then return none
      return some { provider := jStr j "p", login := jStr j "l", sub := jStr j "s",
                    email := jStr j "e", name := jStr j "n", picture := jStr j "i" }
    drop := fun v => do
      if let some j := verifyCookie secret "session" v then
        ended.modify (·.insert (tokenHash v) (jNat j "x"))
    begin := fun p =>
      return signCookie secret "login" <| Json.mkObj
        [("st", p.state), ("v", p.verifier), ("n", p.next), ("c", p.createdAt)]
    take := fun state cookie => do
      let some j := verifyCookie secret "login" cookie | return none
      if state.isEmpty || jStr j "st" != state then return none
      return some { state, verifier := jStr j "v", next := jStr j "n", createdAt := jNat j "c" }
    purge := fun now _ => ended.modify fun m => m.filter fun _ x => x > now
  }

structure GateConfig where
  client         : Config
  loginPath      : String := "/auth/login"
  logoutPath     : String := "/auth/logout"
  /-- Where logging out lands. -/
  afterLogout    : String := "/"
  cookieName     : String := "session"
  /-- Mark the cookies `Secure` (sent over HTTPS only). Turn on behind TLS. -/
  cookieSecure   : Bool := false
  /-- Seconds a session lasts. -/
  sessionTtl     : Nat := 7 * 86400
  /-- Seconds a login may take, the provider round-trip included. -/
  stateTtl       : Nat := 600
  /-- Who may sign in, by `UserInfo.login`, ignoring case. Empty: nobody,
      unless `allowAnyone`. -/
  allowedLogins  : List String := []
  /-- Let every user of the provider in. -/
  allowAnyone    : Bool := false
  publicPaths    : List String := []
  publicPrefixes : List String := []
  /-- Paths answered 401 instead of redirected to the login route. -/
  apiPrefixes    : List String := ["/api/"]
  /-- The clock, in seconds; tests replace it. -/
  clock          : IO Nat := wallSec

/-- The callback route: the path of the client's `redirectUri`. -/
def GateConfig.callbackPath (g : GateConfig) : String :=
  let u := g.client.redirectUri
  let rest := match u.splitOn "://" with
    | [_, r] => r
    | _ => u
  let path := match rest.splitOn "/" with
    | _ :: ps => "/" ++ String.intercalate "/" ps
    | [] => "/"
  (path.splitOn "?").headD "/"

def GateConfig.allows (g : GateConfig) (login : String) : Bool :=
  !login.isEmpty && (g.allowAnyone || g.allowedLogins.any (·.toLower == login.toLower))

def GateConfig.isPublic (g : GateConfig) (path : String) : Bool :=
  g.publicPaths.contains path || g.publicPrefixes.any (path.startsWith ·)

private def GateConfig.stateCookie (g : GateConfig) : String := g.cookieName ++ "_login"

private def hexVal (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c ∧ c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if 'A' ≤ c ∧ c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

/-- Decode a query value (`%xx`, `+`). Invalid UTF-8 leaves it as is. -/
def formDecode (s : String) : String := Id.run do
  let cs := s.toList.toArray
  let mut out : ByteArray := .empty
  let mut i := 0
  while i < cs.size do
    let c := cs[i]!
    if c == '+' then
      out := out.push 32; i := i + 1
    else if c == '%' && i + 2 < cs.size then
      match hexVal cs[i+1]!, hexVal cs[i+2]! with
      | some a, some b => out := out.push (a * 16 + b).toUInt8; i := i + 3
      | _, _ => out := out.push 37; i := i + 1
    else
      out := out ++ c.toString.toUTF8; i := i + 1
  return (String.fromUTF8? out).getD s

private def param (query key : String) : Option String :=
  (query.splitOn "&").findSome? fun p =>
    if p.startsWith (key ++ "=") then some (formDecode (p.drop (key.length + 1)).toString)
    else none

/-- A page of this site to return to after login; anything else
    (another host, `//host`, control characters) becomes `/`. -/
def safeNext (s : String) : String :=
  if s.startsWith "/" && !s.startsWith "//" && !s.contains '\\' && !s.any (fun (c : Char) => c.toNat < 32)
  then s else "/"

/-- The user of this request's session, if it is live and still allowed.
    A session whose name left the allow-list is ended. -/
def currentUser (g : GateConfig) (sessions : Sessions) (req : Request) : IO (Option User) := do
  let some v := req.cookie? g.cookieName | return none
  if v.isEmpty then return none
  let some u ← sessions.find v (← g.clock) | return none
  if g.allows u.login then return some u
  sessions.drop v
  return none

private def loginRoute (g : GateConfig) (sessions : Sessions) (req : Request) : IO Response := do
  let now ← g.clock
  sessions.purge now (now - g.stateTtl)
  let st ← beginAuth g.client
  let next := safeNext ((param req.query "next").getD "/")
  let v ← sessions.begin { state := st.state, verifier := st.codeVerifier, next, createdAt := now }
  return (Response.redirect (authorizeUrl g.client st)).withCookie g.stateCookie v
    (maxAgeSec := g.stateTtl) (secure := g.cookieSecure)

private def callbackRoute (g : GateConfig) (sessions : Sessions) (req : Request) : IO Response := do
  let done (r : Response) := r.clearCookie g.stateCookie (secure := g.cookieSecure)
  if let some e := param req.query "error" then
    return done (Response.text 403 s!"sign-in was not completed ({e})\n")
  let state := (param req.query "state").getD ""
  let some p ← sessions.take state ((req.cookie? g.stateCookie).getD "")
    | return done (Response.text 400 "login state mismatch: start again from the login page\n")
  let now ← g.clock
  if p.createdAt + g.stateTtl <= now then
    return done (Response.text 400 "the login took too long: start again from the login page\n")
  let some code := param req.query "code"
    | return done (Response.text 400 "missing code\n")
  -- provider errors go to the log, not to the browser
  match ← exchangeCode g.client code { state, codeVerifier := p.verifier } with
  | .error e =>
    IO.eprintln s!"oauth2 gate: {e}"
    return done (Response.text 502 "the sign-in provider refused the code\n")
  | .ok tok =>
  match ← fetchUserInfo g.client tok.accessToken with
  | .error e =>
    IO.eprintln s!"oauth2 gate: {e}"
    return done (Response.text 502 "the sign-in provider did not return the user\n")
  | .ok ui =>
  if !g.allows ui.login then
    return done (Response.text 403 s!"{ui.login} may not sign in here\n")
  let user : User := { provider := g.client.provider.name, login := ui.login, sub := ui.sub,
                       email := ui.email, name := ui.name, picture := ui.picture }
  let v ← sessions.create user (now + g.sessionTtl)
  return done ((Response.redirect p.next).withCookie g.cookieName v
    (maxAgeSec := g.sessionTtl) (secure := g.cookieSecure))

private def logoutRoute (g : GateConfig) (sessions : Sessions) (req : Request) : IO Response := do
  if let some v := req.cookie? g.cookieName then sessions.drop v
  return (Response.redirect g.afterLogout).clearCookie g.cookieName (secure := g.cookieSecure)

/-- Put `inner` behind sign-in. `inner` gets the user on every request
    with a live session, and `none` only on public paths. -/
def gate (g : GateConfig) (sessions : Sessions) (inner : Option User → Handler) : Handler :=
  fun req => do
    if req.path == g.loginPath && req.method == "GET" then loginRoute g sessions req
    else if req.path == g.callbackPath && req.method == "GET" then callbackRoute g sessions req
    else if req.path == g.logoutPath then logoutRoute g sessions req
    else
      let user ← currentUser g sessions req
      if user.isSome || g.isPublic req.path then inner user req
      else if g.apiPrefixes.any (req.path.startsWith ·) then
        return Response.text 401 "login required\n"
      else
        let next := if req.query.isEmpty then req.path else req.path ++ "?" ++ req.query
        return Response.redirect (g.loginPath ++ "?next=" ++ urlEncode next)

end LeanTea.Auth.OAuth2
