import LeanTea.Auth
import LeanTea.Crypto.Sha256
import LeanTea.Crypto.Base64
import Lean.Data.Json

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
clean so JWT / OIDC verification can layer on top. -/

namespace LeanTea.Auth.OAuth2

open LeanTea.Auth
open LeanTea.Crypto
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
  let headers : Array String :=
    if cfg.provider.name == "github" then
      -- GitHub returns form-encoded by default; ask for JSON.
      #["Accept: application/json"]
    else #[]
  let resp ← curlPost cfg.provider.tokenEndpoint body headers
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
  email   : String
  name    : String
  picture : String
  raw     : String := ""
  deriving Inhabited, Repr

/-- Hit the provider's userinfo endpoint with the access token. -/
def fetchUserInfo (cfg : Config) (accessToken : String)
    : IO (Except String UserInfo) := do
  let mut headers : Array String := #[s!"Authorization: Bearer {accessToken}"]
  -- GitHub still requires a UA on `/user` calls.
  if cfg.provider.name == "github" then
    headers := headers.push "User-Agent: LeanTea-OAuth2"
  let resp ← curlGet cfg.provider.userInfoEndpoint headers
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
    return .ok { sub, email, name := jStr j "name", picture, raw := resp.body }

end LeanTea.Auth.OAuth2
