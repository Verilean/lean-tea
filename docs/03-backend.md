# 3 · Backend — Elm-style in Lean

The server is also a pure function of (state, request) → (state, response).
The novelty: in LeanTEA, the *frontend's* state and the *backend's*
state are the same Lean type. The browser doesn't keep its own copy —
it forwards the encoded `Model` on every action.

```
┌────────┐       request + X-Model: <encoded model>      ┌─────────┐
│Browser ├────────────────────────────────────────────►│ Server  │
│        │                                              │         │
│ stores │       HTML body + X-Model: <new model>       │ stateless│
│ model  │◄────────────────────────────────────────────┤  loop   │
└────────┘                                              └────┬────┘
                                                            │
                                              SQLite (Persist)
```

## The stateless API loop

A request handler is `Request → IO Response`. `LeanTea.Net.Server.serve`
just runs that handler on every incoming connection.

For an Elm-style app, the typical handler shape is:

```lean
def handleMsg (store : Store) (req : Request) : IO Response := do
  let oldModel := req.header? "x-model" |>.getD ""
  let msg      := lookupParam req.query "msg"
  /- Side-effect writes to SQLite BEFORE the pure update. -/
  match msg with
  | some "add-shape" => store.setCell …
  | _                => pure ()
  /- The pure update. -/
  let (newModel, html) := WebApp.step app (some oldModel) msg
  return {
    status  := 200,
    headers := #[("content-type", "text/html"),
                 ("x-model", newModel)],
    body    := html.toUTF8
  }
```

The browser receives the new `X-Model` header, stashes it in
`dataset.model`, and sends it back next time. The server never has to
remember anything about a particular browser. *Restartable, scalable,
debuggable.*

## A real worked example — Sheet

`examples/Sheet/Serve.lean` ties everything together: a SQLite-backed
shape store, multi-page tabs, MCP endpoint at `/mcp`, dev-mode auto-reload.
The router is the typed `LeanTea.Rpc` chain (Chapter 5) ending in:

```lean
def handler (pageProv : Template.Provider) (store : Store) (devMode : Bool)
    (startedAt : Nat) : Handler :=
  LeanTea.Rpc.chainWith (SheetRpc.routes store) fun req => do
    match req.path, req.method with
    | "/", "GET"             => homePage pageProv store devMode req
    | "/_dev/ping", "GET"    => handleDevPing startedAt req
    | "/api/version", "GET"  => handleVersion store req
    | "/cells", "GET"       => respondSvg store req
    | "/mcp", "POST"         => handleMcp store req
    | "/mcp", "GET"          => return Response.html 200 "<h1>MCP endpoint</h1>"
    | "/favicon.ico", _      => return { status := 204, headers := #[], body := .empty }
    | _, _                   => return Response.notFound
```

Every clause is one function from a `Request` to a `Response` — no
middleware stack, no implicit context. The MCP endpoint shares the
same port as the SPA (Chapter 8 explains the `LeanTea.Mcp.Handler`).

## Where Net.Server fits

```lean
open LeanTea LeanTea.Net.Server

def serveMain (args : List String) : IO Unit := do
  let store ← Store.open ".leantea-state/myapp.sqlite"
  let handler : Handler := myAppHandler store
  serve 8001 "0.0.0.0" handler
```

`serve` is a thin wrapper over `Std.Internal.Async.TCP` accepts. No big
TLS story, no HTTP/2 — sit it behind a reverse proxy if you need that.
For local dev and small production it just works.

The matching client is `LeanTea.Net.HttpClient` — pure Lean HTTP/1.1
with `Content-Length` honouring (no curl shell-out, no node bridge).
Used by every MCP server and the LLM streaming client.

## Sessions, auth, security

For anything that needs to outlive the X-Model round-trip there's
`LeanTea.Auth`:

- `LeanTea.Auth` — session cookies, CSRF tokens
- `LeanTea.Auth.OAuth2` — Google / GitHub / Microsoft OAuth flows
- `LeanTea.Auth.Saml` — SAML SP, signed-assertion verification
- `LeanTea.Auth.Passkey` — WebAuthn / FIDO2 registration & assertion
- `LeanTea.Auth.Security` — constant-time compare, CSP / HSTS headers

All of these compose into the same `Request → IO Response` handler —
they're values, not framework magic.

### Session storage

`AuthStore.attach` materialises the session + state-token tables on
an existing SQLite connection (so your app shares one DB):

```lean
import LeanTea.Auth
import LeanTea.Persist.Sqlite

open LeanTea.Auth

def boot : IO AuthStore := do
  let db ← Sqlite.open' ".leantea-state/myapp.sqlite"
  AuthStore.attach db                       -- creates sessions / oauth_state
```

Issuing a session after login is one call:

```lean
def login (auth : AuthStore) (subject : String) : IO String := do
  let token ← randomToken                   -- 64-hex from /dev/urandom
  let now   ← nowSec
  auth.addSession {
    token, subject,
    issuedAt := now,
    expiresAt := now + 3600 * 24 * 7        -- 1 week
  }
  return token
```

Lookups verify expiry server-side; nothing happens unless the cookie
matches a row that hasn't aged out:

```lean
def whoami (auth : AuthStore) (req : Request) : IO (Option String) := do
  let token := (req.header? "cookie").bind extractSession |>.getD ""
  let now ← nowSec
  match ← auth.findSession token now with
  | some s => return some s.subject
  | none   => return none
```

### OAuth2 (Google example)

The shipped `providers.google` / `.github` / `.microsoft` records
fill in the well-known endpoints; you supply client id, secret,
redirect URI:

```lean
import LeanTea.Auth.OAuth2

open LeanTea.Auth.OAuth2

def googleCfg : Config :=
  providers.google.withClient
    "GOOGLE_CLIENT_ID"
    "GOOGLE_CLIENT_SECRET"
    "https://your.app/auth/callback"

def loginStart (auth : AuthStore) : IO Response := do
  let st  ← beginAuth googleCfg               -- PKCE verifier + state
  auth.addState st.state (← nowSec)
  return Response.redirect (authorizeUrl googleCfg st)

def loginCallback (auth : AuthStore) (req : Request) : IO Response := do
  let code  := lookupParam req.query "code"  |>.getD ""
  let state := lookupParam req.query "state" |>.getD ""
  if !(← auth.takeState state) then
    return Response.text 400 "bad state"
  let st : AuthState := { state, codeVerifier := /-…recovered…-/ "" }
  match ← exchangeCode googleCfg code st with
  | .ok tok =>
    let user ← fetchUserInfo googleCfg tok.accessToken
    let session ← login auth user.subject
    return (Response.redirect "/").setCookie "sid" session
  | .error e => return Response.text 400 s!"oauth: {e}"
```

`beginAuth` returns a random `state` and PKCE `codeVerifier`; the
challenge `S256(codeVerifier)` ends up on the authorize URL.
`exchangeCode` verifies the returned code, swaps for an access token,
and `fetchUserInfo` reads the user's `{subject, email, name, …}` from
the provider's userinfo endpoint.

For SAML SP flows the same shape applies (`LeanTea.Auth.Saml`); for
Passkey (WebAuthn) registration & assertion see
`LeanTea.Auth.Passkey` — `verifyRegistration` and
`verifyAuthentication` are the entry points.

> **Don't forget to call the auth check?** The framework can make that
> a compile error. See **[Chapter 11 · Secure by Construction](11-secure-by-construction.md)**
> for `Auth.Proof` — a typed proof argument every authorised handler
> demands in its signature. Dropping the argument stops the route
> from compiling. The implementation lives in
> `LeanTea/Auth/Proof.lean`; the worked demo is
> `examples/Smoke/AuthProof.lean`.

## Side-channel state

Beyond the Elm-style model, two side-channels are common:

- **Persist** (Chapter 4) — anything that needs to survive a server
  restart: shapes, preferences, audit logs.
- **Static assets** — CSS, JS, images. Either bake them into the binary
  via `include_str` (the framework's `runtime.js` is served this way)
  or serve from disk via a small whitelist of paths.

Both are *intentionally explicit*. No automatic asset pipeline, no
opaque middleware stack. If a request needs a special header, the
handler that writes it is one function from the route to the response.

## WebSockets, RPC, MCP

When you need a long-lived bidirectional channel (CDP, real-time data),
`LeanTea.Net.WebSocket` is a pure-Lean RFC 6455 client (handshake +
masking + frame encoder). The Chrome-CDP MCP server (Chapter 8) is
built on it.

For typed RPC between Lean server and Lean (or `.leanjs`) client see
Chapter 5.

## Concurrency story

Each request runs the pure update independently. The only shared state
is the SQLite store (thread-safe via SQLite's own locking). Two browser
tabs from the same user simply forward whichever `X-Model` each had
last seen; the writes serialise at the DB layer.

If your business logic *needs* coordination across browsers (multi-user
chat, collaborative editing), you'll have to layer that on top: write
to a topic, subscribe via SSE, etc. The stateless loop doesn't preclude
it; it just doesn't bake it in.

## When to *not* use this pattern

- **High-frequency interactive** (every keystroke → server) — round-trip
  costs add up; consider a `.leanjs` widget that maintains local state
  and only syncs on submit.
- **Long-running operations** — handlers are synchronous Lean IO; an
  LLM call that streams for 20 s blocks one connection. Use SSE or
  open a separate WebSocket path.
- **Binary protocols** — the X-Model header is a text-encoded blob.
  Useful for small models; not so great if your model is a serialised
  image.

The next chapter introduces the table half of state.
