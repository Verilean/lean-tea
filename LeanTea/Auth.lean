import LeanTea.Net.Http
import LeanTea.Persist.Sqlite
import LeanTea.Persist.Store
import Lean.Data.Json

/-! # Google OAuth 2.0 + session management

A self-contained auth layer that gates a `Handler` behind Google
sign-in. HTTPS calls to Google are made by shelling out to `curl(1)`
— Lean's `Std.Internal.Async.TCP` only speaks plain TCP. -/

namespace LeanTea.Auth

open LeanTea.Net.Http
open LeanTea.Persist
open Lean (Json)

/-! ## Entities -/

structure Session where
  token     : String
  email     : String
  name      : String
  picture   : String
  createdAt : Nat
  expiresAt : Nat
  deriving Inhabited, Repr

instance : Entity Session where
  table := "sessions"
  ddl :=
    "CREATE TABLE IF NOT EXISTS sessions(" ++
    "token TEXT PRIMARY KEY," ++
    "email TEXT NOT NULL," ++
    "name TEXT NOT NULL DEFAULT ''," ++
    "picture TEXT NOT NULL DEFAULT ''," ++
    "created_at INTEGER NOT NULL," ++
    "expires_at INTEGER NOT NULL)"
  columns := ["token", "email", "name", "picture", "created_at", "expires_at"]
  toRow s := #[s.token, s.email, s.name, s.picture,
               toString s.createdAt, toString s.expiresAt]
  fromRow row :=
    match row.toList with
    | [t, e, n, p, c, x] =>
      match c.toNat?, x.toNat? with
      | some cN, some xN =>
        .ok { token := t, email := e, name := n, picture := p,
              createdAt := cN, expiresAt := xN }
      | _, _ => .error "Session: integer parse failed"
    | _ => .error s!"Session: expected 6 columns, got {row.size}"

/-- One-shot CSRF state token used during the OAuth round-trip. -/
structure OAuthState where
  state     : String
  createdAt : Nat
  deriving Inhabited, Repr

instance : Entity OAuthState where
  table := "oauth_states"
  ddl :=
    "CREATE TABLE IF NOT EXISTS oauth_states(" ++
    "state TEXT PRIMARY KEY," ++
    "created_at INTEGER NOT NULL)"
  columns := ["state", "created_at"]
  toRow s := #[s.state, toString s.createdAt]
  fromRow row :=
    match row.toList with
    | [s, c] => match c.toNat? with
                | some cN => .ok { state := s, createdAt := cN }
                | none    => .error "OAuthState: int parse failed"
    | _ => .error "OAuthState: expected 2 columns"

structure AuthStore where
  db       : Sqlite.Db
  sessions : Repo Session
  states   : Repo OAuthState

def AuthStore.attach (db : Sqlite.Db) : IO AuthStore := do
  let sessions : Repo Session := Repo.new db
  let states   : Repo OAuthState := Repo.new db
  sessions.migrate
  states.migrate
  return { db, sessions, states }

def AuthStore.addSession (s : AuthStore) (sess : Session) : IO Unit := do
  let _ ← s.sessions.insert sess

def AuthStore.findSession (s : AuthStore) (token : String) (nowSec : Nat)
    : IO (Option Session) := do
  let rows ← s.sessions.query
    "SELECT * FROM sessions WHERE token = ? AND expires_at > ?"
    #[token, toString nowSec]
  return rows[0]?

def AuthStore.deleteSession (s : AuthStore) (token : String) : IO Unit := do
  let _ ← s.sessions.execRaw "DELETE FROM sessions WHERE token = ?" #[token]

def AuthStore.addState (s : AuthStore) (state : String) (nowSec : Nat) : IO Unit := do
  let _ ← s.states.insert { state, createdAt := nowSec }

def AuthStore.takeState (s : AuthStore) (state : String) : IO Bool := do
  -- Returns true if the state existed (and is now consumed).
  let n ← s.states.execRaw "DELETE FROM oauth_states WHERE state = ?" #[state]
  return n > 0

def AuthStore.purgeExpired (s : AuthStore) (nowSec : Nat) : IO Unit := do
  let _ ← s.sessions.execRaw "DELETE FROM sessions WHERE expires_at <= ?"
    #[toString nowSec]
  let _ ← s.states.execRaw "DELETE FROM oauth_states WHERE created_at < ?"
    #[toString (nowSec - 600)]  -- 10 minutes

/-! ## Tiny utilities -/

/-- Hex-encode a `ByteArray`. -/
def hex (ba : ByteArray) : String := Id.run do
  let digits := "0123456789abcdef".toList.toArray
  let mut s := ""
  for i in [:ba.size] do
    let b := ba.get! i
    s := s.push digits[(b.toNat / 16)]!
    s := s.push digits[(b.toNat % 16)]!
  return s

/-- 32 bytes of randomness from `/dev/urandom`, hex-encoded.
    **Important**: `/dev/urandom` is infinite; we must `open + read 32`
    instead of `readBinFile` (which reads until EOF and never
    returns). -/
def randomToken : IO String := do
  let result : IO ByteArray := do
    let h ← IO.FS.Handle.mk "/dev/urandom" .read
    h.read 32
  let bytes ← result.catchExceptions (fun _ => pure .empty)
  if bytes.size == 32 then
    return hex bytes
  -- Fallback to IO.rand if /dev/urandom isn't available.
  -- 16 × 4-byte chunks → 64-char hex.
  let mut s := ""
  for _ in [:16] do
    let n ← IO.rand 0 0xffffffff
    let mut b : ByteArray := .empty
    b := b.push ((n / 0x1000000).toUInt8)
    b := b.push (((n / 0x10000) % 0x100).toUInt8)
    b := b.push (((n / 0x100) % 0x100).toUInt8)
    b := b.push ((n % 0x100).toUInt8)
    s := s ++ hex b
  return s

/-- Unix seconds. -/
def nowSec : IO Nat := do
  let ms ← IO.monoMsNow
  return ms / 1000

/-! ## URL-encode (RFC 3986 unreserved chars passthrough) -/

private def unreserved (c : Char) : Bool :=
  c.isAlpha || c.isDigit || c == '-' || c == '_' || c == '.' || c == '~'

def urlEncode (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    if unreserved c then
      out := out.push c
    else
      for b in c.toString.toUTF8 do
        let hi := b.toNat / 16
        let lo := b.toNat % 16
        let hx := "0123456789ABCDEF".toList.toArray
        out := out.push '%'
        out := out.push hx[hi]!
        out := out.push hx[lo]!
  return out

/-! ## HTTPS via curl -/

structure HttpsResp where
  status : Nat
  body   : String

/-- Best-effort HTTPS call. Returns the response body and HTTP status.
    Adds `-sS --max-time 10` so we fail loudly but quickly. -/
def curlPost (url : String) (form : String)
    (headers : Array String := #[]) : IO HttpsResp := do
  let mut args : Array String := #[
    "-sS", "--max-time", "10",
    "-X", "POST",
    "-w", "\n___STATUS:%{http_code}",
    "-H", "content-type: application/x-www-form-urlencoded",
    "-d", form, url]
  for h in headers do args := args ++ #["-H", h]
  let out ← IO.Process.output { cmd := "curl", args := args }
  let raw := out.stdout
  let parts := raw.splitOn "\n___STATUS:"
  match parts with
  | [body, codeS] =>
    return { status := codeS.trimAscii.toString.toNat?.getD 0, body }
  | _ => return { status := 0, body := raw }

def curlGet (url : String) (headers : Array String := #[]) : IO HttpsResp := do
  let mut args : Array String := #[
    "-sS", "--max-time", "10",
    "-w", "\n___STATUS:%{http_code}", url]
  for h in headers do args := args ++ #["-H", h]
  let out ← IO.Process.output { cmd := "curl", args := args }
  let raw := out.stdout
  let parts := raw.splitOn "\n___STATUS:"
  match parts with
  | [body, codeS] =>
    return { status := codeS.trimAscii.toString.toNat?.getD 0, body }
  | _ => return { status := 0, body := raw }

/-! ## Google OAuth config + flow -/

structure Config where
  clientId     : String
  clientSecret : String
  baseUrl      : String        -- e.g. "https://app.example.com"
  cookieSecure : Bool := false
  -- Optional allowlist: if non-empty, only emails in this list sign in.
  allowedEmails : List String := []

/-- Pull a `Config` from environment variables. Returns `none` if
    the required `GOOGLE_CLIENT_ID` / `GOOGLE_CLIENT_SECRET` are not
    set. -/
def Config.fromEnv : IO (Option Config) := do
  let cid ← IO.getEnv "GOOGLE_CLIENT_ID"
  let csec ← IO.getEnv "GOOGLE_CLIENT_SECRET"
  let base := (← IO.getEnv "BASE_URL").getD "http://localhost:8001"
  let secure := match ← IO.getEnv "COOKIE_SECURE" with
    | some "1" | some "true" => true
    | _ => false
  let allow ← IO.getEnv "ALLOWED_EMAILS"
  let allowed : List String := (allow.getD "").splitOn "," |>.map (·.trimAscii.toString)
                                |>.filter (!·.isEmpty)
  match cid, csec with
  | some cid, some csec =>
    return some { clientId := cid, clientSecret := csec, baseUrl := base,
                  cookieSecure := secure, allowedEmails := allowed }
  | _, _ => return none

def authorizeUrl (cfg : Config) (state : String) : String :=
  "https://accounts.google.com/o/oauth2/v2/auth?" ++
  "client_id=" ++ urlEncode cfg.clientId ++
  "&redirect_uri=" ++ urlEncode (cfg.baseUrl ++ "/auth/google/callback") ++
  "&response_type=code" ++
  "&scope=" ++ urlEncode "openid email profile" ++
  "&state=" ++ urlEncode state ++
  "&access_type=online" ++
  "&prompt=select_account"

/-- POST the auth code to Google's token endpoint. Returns the
    parsed access token or an error string. -/
def exchangeCode (cfg : Config) (code : String) : IO (Except String String) := do
  let body :=
    "code=" ++ urlEncode code ++
    "&client_id=" ++ urlEncode cfg.clientId ++
    "&client_secret=" ++ urlEncode cfg.clientSecret ++
    "&redirect_uri=" ++ urlEncode (cfg.baseUrl ++ "/auth/google/callback") ++
    "&grant_type=authorization_code"
  let resp ← curlPost "https://oauth2.googleapis.com/token" body
  if resp.status != 200 then
    return .error s!"google token endpoint returned {resp.status}: {resp.body}"
  match Json.parse resp.body with
  | .error e => return .error s!"json parse: {e}"
  | .ok j =>
    match (j.getObjVal? "access_token").toOption.bind (·.getStr?.toOption) with
    | some tok => return .ok tok
    | none     => return .error s!"no access_token in: {resp.body}"

structure UserInfo where
  email   : String
  name    : String
  picture : String

def fetchUserInfo (accessToken : String) : IO (Except String UserInfo) := do
  let resp ← curlGet "https://www.googleapis.com/oauth2/v3/userinfo"
    #[s!"Authorization: Bearer {accessToken}"]
  if resp.status != 200 then
    return .error s!"userinfo returned {resp.status}: {resp.body}"
  match Json.parse resp.body with
  | .error e => return .error s!"json parse: {e}"
  | .ok j =>
    let str (k : String) : String :=
      (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD ""
    return .ok { email := str "email", name := str "name", picture := str "picture" }

/-! ## HTTP handlers for /auth/google/* -/

def loginHandler (cfg : Config) (store : AuthStore) : IO Response := do
  let state ← randomToken
  let now ← nowSec
  store.addState state now
  return Response.redirect (authorizeUrl cfg state)

def callbackHandler (cfg : Config) (store : AuthStore) (req : Request) : IO Response := do
  -- Parse query string for code + state.
  let lookup (key : String) : Option String :=
    let pref := key ++ "="
    (req.query.splitOn "&").findSome? fun p =>
      if p.startsWith pref then some (p.drop pref.length).toString else none
  let code? := lookup "code"
  let state? := lookup "state"
  match code?, state? with
  | none, _ => return Response.badRequest
  | _, none => return Response.badRequest
  | some codeEnc, some stateEnc =>
    let code := codeEnc  -- code is opaque, no need to decode
    let state := stateEnc
    -- CSRF state check
    let ok ← store.takeState state
    if !ok then
      return Response.text 400 "invalid state\n"
    match ← exchangeCode cfg code with
    | .error e => return Response.text 502 s!"token exchange failed: {e}\n"
    | .ok tok =>
      match ← fetchUserInfo tok with
      | .error e => return Response.text 502 s!"userinfo failed: {e}\n"
      | .ok ui =>
        -- Allowlist check
        if !cfg.allowedEmails.isEmpty && !cfg.allowedEmails.contains ui.email then
          return Response.text 403 s!"email {ui.email} is not allowed\n"
        let token ← randomToken
        let now ← nowSec
        store.addSession {
          token := token, email := ui.email, name := ui.name, picture := ui.picture,
          createdAt := now, expiresAt := now + 86400 * 7
        }
        let resp := Response.redirect "/"
        return resp.withCookie "session" token (secure := cfg.cookieSecure)

def logoutHandler (store : AuthStore) (req : Request) (cookieSecure : Bool) : IO Response := do
  if let some token := req.cookie? "session" then
    store.deleteSession token
  return (Response.redirect "/").clearCookie "session" (secure := cookieSecure)

/-! ## Gate wrapper -/

/-- Identify the current user for a request, if any. -/
def currentUser (store : AuthStore) (req : Request) : IO (Option Session) := do
  match req.cookie? "session" with
  | none => return none
  | some token =>
    let now ← nowSec
    store.findSession token now

/-- Wrap `inner` so that protected paths require a valid session.
    The function intercepts `/auth/google/login`, `/auth/google/callback`
    and `/auth/logout` itself. Any other path is delegated to `inner`,
    which receives the resolved user via `withUser`. -/
def gate (cfg : Config) (store : AuthStore)
    (publicPaths : List String) (inner : Session → Handler) : Handler := fun req => do
  match req.path, req.method with
  | "/auth/google/login", "GET" => loginHandler cfg store
  | "/auth/google/callback", "GET" => callbackHandler cfg store req
  | "/auth/logout", _ => logoutHandler store req cfg.cookieSecure
  | _, _ =>
    if publicPaths.contains req.path then
      -- Public path: still run inner if logged in, otherwise pass a
      -- guest session.
      let guest : Session := { token := "", email := "", name := "guest", picture := "",
                               createdAt := 0, expiresAt := 0 }
      let user ← (currentUser store req).map (·.getD guest)
      inner user req
    else
      match ← currentUser store req with
      | some sess => inner sess req
      | none =>
        if req.path.startsWith "/api/" || req.path == "/mcp" then
          -- APIs get 401 instead of redirect.
          return Response.text 401 "login required\n"
        else
          return Response.redirect "/auth/google/login"

end LeanTea.Auth
