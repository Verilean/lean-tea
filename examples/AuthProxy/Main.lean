import LeanTea
import LeanTea.Auth.Proxy

/-! # auth_proxy — sign in with an OAuth 2.0 provider, then forward to a local service

```
auth_proxy --listen 0.0.0.0:2343 --upstream 127.0.0.1:9107 \
  --provider github --client-id ID --allowed-users octo,someone \
  --redirect-url https://host.example:2343/oauth2/callback
```

Routes: `/oauth2/start` (login; `?next=/path`), the path of
`--redirect-url` (the callback) and `/oauth2/sign_out`. Everything else
needs a session and is forwarded to `--upstream` with the signed-in
login in `X-Forwarded-User` (see `LeanTea.Auth.Proxy` for exactly which
headers pass). Without a session, paths under `--api-prefix` (default
`/api/`) get 401 and pages redirect to the login route.

**Secrets are read from files, never from arguments** (arguments are
visible to every local user in `/proc/PID/cmdline`):

* `--client-secret-file` (default `$CREDENTIALS_DIRECTORY/client-secret`)
* `--cookie-secret-file` (default `$CREDENTIALS_DIRECTORY/cookie-secret`;
  32 bytes or more, e.g. `openssl rand -hex 32`). It signs the session
  cookies; changing it ends every session.

With systemd, run it as its own user and hand the files over with
`LoadCredential`, so the user running other jobs cannot read them:

```ini
[Service]
DynamicUser=yes
LoadCredential=client-secret:/etc/auth-proxy/client-secret
LoadCredential=cookie-secret:/etc/auth-proxy/cookie-secret
ExecStart=/path/to/auth_proxy --listen 0.0.0.0:2343 --upstream 127.0.0.1:9107 ...
```

HTTPS to the provider needs the OpenSSL client: build with
`LEANTEA_TLS=1 lake build -R auth_proxy` (see `tlsLinkArgs` in the
lakefile). Without it, sign-in fails with "TLS not enabled".

Other flags: `--cookie-secure true|false` (default true; `false` only
for plain-HTTP setups), `--cookie-name`, `--session-ttl SECONDS`
(default 86400), `--public PATH` and `--public-prefix PREFIX` (reach the
upstream without a session), `--api-prefix PREFIX`, `--max-body BYTES`
(default 1 MiB), `--scope`, and `--auth-url` / `--token-url` /
`--userinfo-url` to point at another provider (the tests use a local
one). -/

open LeanTea.Auth.OAuth2

structure Opts where
  listen          : String := "127.0.0.1:4180"
  upstream        : String := ""
  provider        : String := "github"
  clientId        : String := ""
  clientSecretFile : Option String := none
  cookieSecretFile : Option String := none
  allowed         : List String := []
  redirectUrl     : String := ""
  cookieSecure    : Bool := true
  cookieName      : String := "session"
  sessionTtl      : Nat := 86400
  publicPaths     : List String := []
  publicPrefixes  : List String := []
  apiPrefixes     : List String := []
  maxBody         : Nat := 1024 * 1024
  scope           : Option String := none
  authUrl         : Option String := none
  tokenUrl        : Option String := none
  userInfoUrl     : Option String := none

private def usage : String :=
  "usage: auth_proxy --upstream HOST:PORT --client-id ID --redirect-url URL " ++
  "--allowed-users a,b [--listen HOST:PORT] [--provider github|google|microsoft] " ++
  "[--client-secret-file F] [--cookie-secret-file F] [--cookie-secure true|false] ..."

private def bool? : String → Option Bool
  | "true" | "1" | "yes" => some true
  | "false" | "0" | "no" => some false
  | _ => none

partial def parse (o : Opts) : List String → Except String Opts
  | [] => .ok o
  | flag :: rest =>
    match flag, rest with
    | "--client-secret", _ | "--cookie-secret", _ =>
      .error s!"{flag} is not accepted: pass the secret in a file ({flag}-file)"
    | "--listen", v :: r => parse { o with listen := v } r
    | "--upstream", v :: r => parse { o with upstream := v } r
    | "--provider", v :: r => parse { o with provider := v } r
    | "--client-id", v :: r => parse { o with clientId := v } r
    | "--client-secret-file", v :: r => parse { o with clientSecretFile := some v } r
    | "--cookie-secret-file", v :: r => parse { o with cookieSecretFile := some v } r
    | "--allowed-users", v :: r =>
      parse { o with allowed := o.allowed ++ ((v.splitOn ",").map (·.trimAscii.toString)
                                               |>.filter (!·.isEmpty)) } r
    | "--redirect-url", v :: r => parse { o with redirectUrl := v } r
    | "--cookie-secure", v :: r =>
      match bool? v with
      | some b => parse { o with cookieSecure := b } r
      | none => .error s!"--cookie-secure: expected true or false, got {v}"
    | "--cookie-name", v :: r => parse { o with cookieName := v } r
    | "--session-ttl", v :: r =>
      match v.toNat? with
      | some n => parse { o with sessionTtl := n } r
      | none => .error s!"--session-ttl: expected seconds, got {v}"
    | "--public", v :: r => parse { o with publicPaths := o.publicPaths ++ [v] } r
    | "--public-prefix", v :: r => parse { o with publicPrefixes := o.publicPrefixes ++ [v] } r
    | "--api-prefix", v :: r => parse { o with apiPrefixes := o.apiPrefixes ++ [v] } r
    | "--max-body", v :: r =>
      match v.toNat? with
      | some n => parse { o with maxBody := n } r
      | none => .error s!"--max-body: expected bytes, got {v}"
    | "--scope", v :: r => parse { o with scope := some v } r
    | "--auth-url", v :: r => parse { o with authUrl := some v } r
    | "--token-url", v :: r => parse { o with tokenUrl := some v } r
    | "--userinfo-url", v :: r => parse { o with userInfoUrl := some v } r
    | _, _ => .error s!"unknown or incomplete flag: {flag}"

/-- `HOST:PORT` → host and port. -/
private def hostPort (what s : String) : Except String (String × UInt16) :=
  match s.splitOn ":" with
  | [h, p] =>
    match p.toNat? with
    | some n => if n > 0 && n < 65536 then .ok (if h.isEmpty then "127.0.0.1" else h, n.toUInt16)
                else .error s!"{what}: bad port in {s}"
    | none => .error s!"{what}: bad port in {s}"
  | _ => .error s!"{what}: expected HOST:PORT, got {s}"

/-- A secret file's content without trailing whitespace. The content is
    never printed. -/
private def readSecret (what : String) (path? : Option String) (name : String) : IO String := do
  let path ← match path? with
    | some p => pure p
    | none =>
      match ← IO.getEnv "CREDENTIALS_DIRECTORY" with
      | some d => pure s!"{d}/{name}"
      | none =>
        let hint := s!"pass --{name}-file, or run under systemd with LoadCredential={name}:…"
        throw (IO.userError s!"{what}: {hint}")
  let text ← try IO.FS.readFile path catch _ =>
    throw <| IO.userError s!"{what}: cannot read {path}"
  let v := text.trimAsciiEnd.toString
  if v.isEmpty then throw <| IO.userError s!"{what}: {path} is empty"
  return v

private def provider (o : Opts) : Except String Provider := do
  let base ← match o.provider with
    | "github" => pure providers.github
    | "google" => pure providers.google
    | "microsoft" => pure providers.microsoft
    | other => .error s!"--provider: unknown provider {other} (github, google, microsoft)"
  return { base with
    authEndpoint := o.authUrl.getD base.authEndpoint
    tokenEndpoint := o.tokenUrl.getD base.tokenEndpoint
    userInfoEndpoint := o.userInfoUrl.getD base.userInfoEndpoint }

def run (args : List String) : IO Unit := do
  let o ← IO.ofExcept (parse {} args)
  if o.upstream.isEmpty || o.clientId.isEmpty || o.redirectUrl.isEmpty then
    throw <| IO.userError s!"--upstream, --client-id and --redirect-url are required\n{usage}"
  if o.allowed.isEmpty then
    throw <| IO.userError "--allowed-users is required (nobody could sign in otherwise)"
  let (lHost, lPort) ← IO.ofExcept (hostPort "--listen" o.listen)
  let (uHost, uPort) ← IO.ofExcept (hostPort "--upstream" o.upstream)
  let p ← IO.ofExcept (provider o)
  let clientSecret ← readSecret "client secret" o.clientSecretFile "client-secret"
  let cookieSecret ← readSecret "cookie secret" o.cookieSecretFile "cookie-secret"
  if cookieSecret.utf8ByteSize < 32 then
    throw <| IO.userError "cookie secret: use 32 bytes or more (e.g. openssl rand -hex 32)"
  let g : GateConfig := {
    client := p.withClient o.clientId clientSecret o.redirectUrl o.scope
    loginPath := "/oauth2/start", logoutPath := "/oauth2/sign_out"
    cookieName := o.cookieName, cookieSecure := o.cookieSecure, sessionTtl := o.sessionTtl
    allowedLogins := o.allowed
    publicPaths := o.publicPaths, publicPrefixes := o.publicPrefixes
    apiPrefixes := if o.apiPrefixes.isEmpty then ["/api/"] else o.apiPrefixes }
  let sessions ← signedSessions cookieSecret.toUTF8
  IO.eprintln s!"auth_proxy: {o.provider} sign-in for {o.allowed}, forwarding to {uHost}:{uPort}"
  LeanTea.Net.Server.serveConcurrent lPort lHost
    (LeanTea.Auth.Proxy.handler { host := uHost, port := uPort, maxBody := o.maxBody } g sessions)

def main (args : List String) : IO UInt32 := do
  try
    run args
    return 0
  catch e =>
    IO.eprintln s!"auth_proxy: {e}"
    return 2
