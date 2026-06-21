# LeanTEA

**Lean 4 + TEA ([The Elm Architecture](https://guide.elm-lang.org/architecture/))**.
A tiny full-stack framework — and a handful of apps built on top of it
(a functional spreadsheet with an MCP endpoint, a board-game SPA, a
Chrome-CDP MCP server, and several other AI-driving MCP servers).

[![ci](https://github.com/Verilean/lean-tea/actions/workflows/ci.yml/badge.svg)](https://github.com/Verilean/lean-tea/actions/workflows/ci.yml)
[![pages](https://github.com/Verilean/lean-tea/actions/workflows/pages.yml/badge.svg)](https://github.com/Verilean/lean-tea/actions/workflows/pages.yml)
[![Discord](https://img.shields.io/badge/discord-LeanTEA-5865F2?logo=discord&logoColor=white)](https://discord.gg/94Xueve8WD)

Questions, design discussion, and weekly progress threads live in the Discord channel above.

- **Pure Lean** stack: HTTP server, WebSocket client, and SQLite live
  in Lean.
- **No Node.js, no Python at runtime** (Python is used only by a few
  build helpers in `tools/`).
- **SQLite is vendored** — `c/sqlite3.c` is the amalgamation, linked
  into the binary, so deployment doesn't need `-lsqlite3`.
- The browser only ever sees plain HTML + one inlined JS file
  (Web Speech API is the only external browser API used).

## Inspirations

The name **LeanTEA** is **Lean 4 + TEA** — *The Elm Architecture*. The
framework borrows ideas across the Elm + Haskell-flavoured ecosystem
and ports them into Lean 4:

- **[The Elm Architecture (TEA)](https://guide.elm-lang.org/architecture/)** —
  `Model / Msg / update / view` on both the TUI and the in-browser
  runtimes. Lean's structures + dependent types let `Msg` be an
  inductive that the compiler exhaustiveness-checks for you.
- **[Yesod](https://www.yesodweb.com/)** — the "full-stack typed web
  framework" framing: routing, sessions, OAuth, and templates as
  first-class Lean values rather than stringly-typed configuration.
- **[Persistent](https://www.yesodweb.com/book/persistent)** — the
  `Entity / Repo` typeclasses under `LeanTea.Persist.*` are
  Persistent-style: define a record, derive a backend-agnostic store,
  swap SQLite / MySQL / in-memory at the call site.
- **[Servant](https://docs.servant.dev/)** — the typed-RPC layer
  (`LeanTea.Rpc`) treats the API surface as a Lean type the server
  and client share. No hand-written JSON wrangling, no schema drift.

> Not affiliated with or endorsed by the Elm, Yesod, Persistent, or
> Servant projects.

### Sibling projects in the Lean 4 ecosystem

- **[Verso](https://github.com/leanprover/verso)** — a Lean-native
  authoring tool for documentation and books (Scribble / Sphinx
  lineage). **Complementary to LeanTEA, not overlapping**: Verso
  generates static documents; LeanTEA serves dynamic web apps.
  We're considering migrating `docs/` to Verso once the framework
  stabilises so the code snippets in the book are actually
  type-checked.

## Secure by Construction

LeanTEA's headline property is that whole classes of vulnerabilities
**can't be expressed in user code that compiles**. **Eight primitives
ship today**; one more is planned. The shipped set covers most of
the IPA 「安全なウェブサイトの作り方」 11 categories and OWASP Top 10
2021. See [SECURITY.md](SECURITY.md) for the design + threat model,
[docs/11](docs/11-secure-by-construction.md) for the walk-through,
and [ROADMAP.md](ROADMAP.md) for sequencing.

| Vulnerability class | LeanTEA primitive | IPA / OWASP | Status |
|---|---|---|---|
| Authorization bypass / IDOR | `LeanTea.Auth.Proof` (`Proof c` + `Capability` lattice + dependent `Proof (.owner id)`) | IPA §3.7 / A01 | ✅ shipped — [walk](docs/11-secure-by-construction.md#2--authproof--authorization-that-cant-be-forgotten) · [demo](examples/Tests/PersistSpec.lean) |
| SQL injection | `LeanTea.Persist.SafeQuery` (typed `Where` / `Select` / `Update` / `Delete` + `.trusted decl_name%` audit) | IPA §3.1 / A03 | ✅ shipped — [walk](docs/11-secure-by-construction.md#3--safequery--sql-injection-that-cannot-be-expressed) · [demo](examples/Tests/PersistSpec.lean) |
| XSS (URL scheme + event-handler names) | `LeanTea.Html.SafeAttr` (private `mk` + URL allow-list + `on*` rejection) | IPA §3.5 / A03 | ✅ shipped — [walk](docs/11-secure-by-construction.md#4--safehtml--xss-that-cant-be-introduced) · [demo](examples/Tests/SecuritySpec.lean) |
| Path traversal | `LeanTea.Net.SafePath` (workspace-relative + `..` / NUL / sibling-prefix reject) | IPA §3.4 / A01 | ✅ shipped — [walk](docs/11-secure-by-construction.md#5--safepath--paths-that-cant-escape-their-workspace) · [demo](examples/Tests/SecuritySpec.lean) |
| OS command injection | `LeanTea.Os.SafeCmd` (`args : List String` + shell-name allow-list reject + grep-able `SafeCmd.shell` audit) | IPA §3.3 / A03 | ✅ shipped — [walk](docs/11-secure-by-construction.md#6--safecmd--ioprocessrun-that-cant-get-shell-injected) · [demo](examples/Tests/SecuritySpec.lean) |
| HTTP header injection | `Response.setHeader` (CR / LF / NUL reject) | IPA §3.6 / A03 | ✅ shipped — [walk](docs/11-secure-by-construction.md#7--responsesetheader--defaultsecurityheaders--header-injection--clickjacking) · [demo](examples/Tests/SecuritySpec.lean) |
| Clickjacking + MIME sniffing | `Response.defaultSecurityHeaders` (XFO / nosniff / Referrer-Policy / Permissions-Policy) | IPA §3.10 / A05 | ✅ shipped — [walk](docs/11-secure-by-construction.md#7--responsesetheader--defaultsecurityheaders--header-injection--clickjacking) · [demo](examples/Tests/SecuritySpec.lean) |
| Open redirect | `LeanTea.Net.SafeRedirect` (allow-listed origin + relative-path-only mode + scheme reject + sibling-prefix reject) | IPA §3.9 / A01 | ✅ shipped — [walk](docs/11-secure-by-construction.md#8--saferedirect--open-redirect-that-needs-an-allow-list) · [demo](examples/Tests/SecuritySpec.lean) |
| Invalid state transitions | `OrderState` / `Transition s s'` style proofs | — | 🚧 planned |

### Snippet — `SafeQuery` rejects string-shaped SQL at compile time

```lean
-- ✅ Compiles — typed builders, positionally bound:
let rows ← SafeQuery.run users
  { where_ := .and (UserCols.email.eq "alice@x.com")
                   (.not (UserCols.deleted.eq true)) }

-- ❌ Compile error — `Where.eq` is `private` to SafeQuery.lean.
--   The framework gives no path from a raw String to a `Where` clause.
let bad := Where.eq "email" rawUserInput
-- error: Unknown constant `LeanTea.Persist.SafeQuery.Where.eq`
```

### Snippet — `Auth.Proof` enforces the auth check in the type signature

```lean
-- The admin handler demands an unforgeable `Proof .admin`:
def handleAdminDelete (proof : Proof .admin) (req : Request) : IO Response := …

-- Removing the `proof` parameter breaks the route registration:
def handleAdminDelete (req : Request) : IO Response := …
-- error: Type mismatch in route registration
--   expected: Proof .admin → Request → IO Response
--   got:      Request → IO Response
```

The proof's `mk` is `private` to the auth module — only `Proof.issue`
(which checks the session) can mint one. Forgetting the auth check
is now a build failure, not a CVE.

For the full walk-through (capability lattice, dependent
`Proof (.owner id)`, the `.trusted decl_name%` audit-grep escape, the
~480-LOC trusted core across all eight primitives), see
**[docs/11-secure-by-construction.md](docs/11-secure-by-construction.md)**.

## Layout

```
LeanTea/
├── Cmd.lean Sub.lean Runtime.lean    -- TUI Elm runtime
├── Web.lean Html.lean Css.lean Js.lean -- WebApp (Model/Msg/update/view) + DSLs
├── Template.lean                     -- {{var}} / {{#each}} / {{#if}} / {{#include}}
├── Rpc.lean JsonRpc.lean             -- Servant-style typed RPC + JSON-RPC envelope
├── Mcp.lean                          -- MCP Handler (stdio + HTTP transports)
├── Markdown.lean Markdown/           -- CommonMark-ish parser
├── Json/                             -- terse Json accessors (.getStrD etc.)
├── Net/
│   ├── Http.lean Server.lean         -- HTTP/1.1 server + Request/Response/Handler
│   ├── HttpClient.lean               -- pure-Lean HTTP/1.1 client
│   ├── WebSocket.lean                -- pure-Lean RFC 6455 client (handshake, masking)
│   ├── Desktop.lean Memcached.lean   -- OS desktop FFI, memcached client
├── Persist/
│   ├── Sqlite.lean Mysql.lean        -- backend FFI
│   ├── Store.lean Query.lean Backend.lean Migrate.lean -- Entity / Repo / migration
│   └── SafeQuery.lean                -- typed Where / Select / Update / Delete (no `String → SQL`)
├── Auth.lean                         -- session store
│   ├── OAuth2.lean Saml.lean Passkey.lean Security.lean
│   └── Proof.lean                    -- Capability + Proof.issue (Authorization)
├── Crypto/                           -- Base64 / SHA-1 / SHA-256 / HMAC / PBKDF2 / JWT
├── Browser.lean Comfy.lean Diffuse.lean -- 3rd-party tool bridges
├── Llm/Openai.lean                   -- streaming OpenAI-compatible client (LM Studio)
├── Agent/                            -- run history, replayable scripts
├── LSpec.lean                        -- tiny test runner (group / it / lspecIO)
└── assets/runtime.js styles.css      -- embedded client runtime

LeanJs/                               -- Lean-subset → JavaScript compiler
├── Ast.lean Parser.lean JsParser.lean
├── Check.lean                        -- arity + record-field guard
├── Codegen.lean Eval.lean LeanEmit.lean Includes.lean

c/
├── sqlite3.c sqlite3.h               -- SQLite amalgamation (vendored, ~9 MB)
├── leantea_sqlite.c                  -- SQLite FFI wrapper
├── leantea_mysql.c                   -- MySQL FFI (opt-in via LEANTEA_MYSQL=1)
├── leantea_crypto.c                  -- OpenSSL bindings (opt-in via LEANTEA_CRYPTO=1)
└── leantea_desktop.c                 -- macOS Quartz bindings (opt-in via LEANTEA_DESKTOP=1)

examples/
├── Counter/ Quiz/ CounterWeb/         -- TUI + browser TEA demos (~50 lines each)
├── Sheet/                             -- functional spreadsheet + /mcp (typed Rpc + Persist + Mcp)
├── Reversi/                           -- board game (.leanjs client, Lean server)
├── Gpu/                               -- WebGPU demo
├── ChromeCdpMcp/                      -- real-Chrome driver via CDP (10 tools)
├── BrowserMcp/ BrowserAgent/          -- Playwright-driven browser + LLM agent
├── ComfyuiMcp/                        -- ComfyUI HTTP/WebSocket driver
├── DesktopMcp/                        -- OS-level mouse + screenshot (macOS Quartz)
├── ImageMcp/                          -- HTML/CSS → PNG compositor
├── UiScript/ UiReport/                -- AI-driven E2E test runner + HTML report
├── Smoke/                             -- subsystem smoke tests (one per area)
├── Tests/                             -- LeanJs spec runner
├── Tools/                             -- gen_site + leanjs_{compile,interp,run} CLIs
└── Docs/                              -- runnable doc examples

tools/
├── dev.py                             -- file watcher + auto reload for the dev loop
├── browser-bridge/                    -- node + Playwright (used by BrowserMcp)
└── run-tests.sh run-docs.sh           -- CI entry points
```

## Build and run

```sh
# Build everything (~3 min cold, seconds incrementally)
lake build

# In-browser counter (TEA in 50 lines)
./.lake/build/bin/counter_web --port 8001
open http://127.0.0.1:8001/

# Multi-user SVG editor + MCP endpoint at /mcp
./.lake/build/bin/sheet_serve --port 8002 --db ../.leantea-state/sheet.sqlite
open http://127.0.0.1:8002/

# Board game (Reversi) — client logic is a .leanjs file compiled at startup
./.lake/build/bin/reversi_serve --port 8005

# Chrome-CDP MCP server (drives your already-open Chrome)
# 1. Launch Chrome with: --remote-debugging-port=9222 --user-data-dir=/tmp/chrome-cdp
# 2. Then:
./.lake/build/bin/chrome_cdp_mcp_serve --stdio
```

Tests are organised into two consolidated LSpec runners plus a
handful of subsystem smokes:

- **`persist_spec`** — Store roundtrips + Query DSL + Migration
  runner + Auth.Proof dispatch + SafeQuery typed SQL (32
  assertions, one binary, one CI step).
- **`security_spec`** — SafeHtml + SafePath + SafeCmd + SafeHeader
  + SafeRedirect construction-time guarantees (60 assertions, one
  binary, one CI step).
- **`template_smoke` / `crypto_smoke` / `http_*_smoke`** — narrower
  per-module subsystem smokes that don't yet have an LSpec runner.

Read the runner source under `examples/Tests/` as the shortest
"this is what works" demonstration of any given area.

## Architecture

```
[Browser]
  index.html  ←──── GET /, /styles.css, /runtime.js
  runtime.js  ──┐
                │ fetch('/api/step?msg=...', X-Model: <encoded>)
                ▼
[Lean: sheet_serve]
  Net.Server (Std.Internal.Async.TCP)
    ↓
  SheetServe.handler
    ├─ "/"                → render the toolbar + SVG host from Template
    ├─ "/cells"          → SVG fragment built from Persist.Store.shapes
    ├─ "/api/*"           → Rpc.dispatch (typed Endpoint records)
    ├─ "/mcp"  (POST)     → LeanTea.Mcp.handleMcp (text / image content)
    └─ everything else    → 404
[Lean: Persist.Store (SQLite via FFI)]
  shapes (id, kind, x, y, w, h, text, color, page_id)
  pages  (id, name)
  audit  (id, action, ts)
```

The client encodes the current `Model` in the `X-Model` header on
every action; the server runs `WebApp.step` (pure) and ships the new
model back the same way. SQLite is for things that need to outlive a
restart (shape DB, sessions, audit). **No middleware stack, no
implicit context** — every clause in `handler` is one function from a
`Request` to a `Response`.

## Persistent-style typed DB API

```lean
structure CellRow where
  kind  : String  -- "rect" / "ellipse" / "text" / "sticky" / "pen"
  x y   : Int
  w h   : Int
  text  : String
  color : String

instance : Entity CellRow where
  table   := "shapes"
  ddl     := "CREATE TABLE IF NOT EXISTS shapes(...)"
  columns := ["kind", "x", "y", "w", "h", "text", "color"]
  toRow s   := #[s.kind, toString s.x, ..., s.color]
  fromRow r := ...

-- Usage — SafeQuery makes SQL injection unrepresentable:
namespace CellCols
  open LeanTea.Persist.SafeQuery
  def kind : Col CellRow String := ⟨"kind"⟩
  def x    : Col CellRow Int    := ⟨"x"⟩
end CellCols

let shapes : Repo CellRow := Repo.new db
shapes.migrate
let _ ← shapes.insert { kind := "rect", x := 0, y := 0, w := 80, h := 40,
                        text := "hello", color := "#38bdf8" }
let rects ← SafeQuery.run shapes
  { where_ := .and (CellCols.kind.eq "rect")
                   (CellCols.x.gt 100) }
```

`Where`'s value-leaf constructors are `private` to the SafeQuery
module — there's no path from a raw `String` into a `Where`, so an
LLM-generated `Where.eq "email" userInput` is a compile error.

## Sheet app + MCP server

`examples/Sheet/` is a small functional spreadsheet:

```sh
lake build sheet_serve
./.lake/build/bin/sheet_serve --port 8002 --db ../.leantea-state/sheet.sqlite
open http://127.0.0.1:8002/
```

- SVG rendering for rect / ellipse / text / sticky / freehand pen
- Click to select, drag to move, drag corner handles to resize,
  double-click for in-place text editing (foreignObject editor),
  separate ✏️ Pen tool, color picker and W / H inputs
- State lives in SQLite (`cells` table)

### MCP (Model Context Protocol) support

`POST /mcp` is a minimal JSON-RPC 2.0 endpoint so Claude or other
clients can edit cells directly:

```sh
# handshake
curl -X POST http://127.0.0.1:8002/mcp \
  -H 'content-type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize"}'

# list tools
curl -X POST http://127.0.0.1:8002/mcp \
  -H 'content-type: application/json' \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'

# add a shape
curl -X POST http://127.0.0.1:8002/mcp \
  -H 'content-type: application/json' \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call",
       "params":{"name":"set_cell",
                 "arguments":{"kind":"rect","x":100,"y":200,"text":"Hello"}}}'
```

Tools exposed:

- `add_shape(kind, x, y, w?, h?, text?, color?)` → new id
- `move_cell(id, x, y)`, `resize_shape(id, w, h)`
- `set_text(id, text)`, `set_color(id, color)`
- `delete_shape(id)`, `list_shapes()`, `clear_all()`

When wiring this into Claude Desktop / Claude Code, point its MCP
client at `transport: "http"`, `url: "http://localhost:8002/mcp"`.

## Typed RPC (Servant-style)

Both the browser side and the server side read a single source of
truth (`examples/Sheet/Api.lean`):

```lean
def setCell : Endpoint := {
  name := "apiSetCell", path := "/api/set", method := "POST",
  params := ["kind", "x", "y", "w", "h", "text", "color"],
  carrier := .form, output := .text
}
```

The server is wired via `Rpc.chainWith SheetRpc.routes (fallback)`
to dispatch path + method to a typed `Handler : List String → IO String`.
The browser gets the matching `async function apiSetCell(ref, formula)`
auto-generated from `Rpc.clientLib SheetRpc.all` and prepended to
the `<script>` block. Drop an endpoint here and both sides update
together.

## CSS / JS DSLs

Both CSS and JS are also small ASTs with a `render` step.

```lean
open LeanTea.Css in
def sheetStyles : Sheet := [
  rule ".btn" [("background", "#0284c7"), ("color", "#fff")],
  rule ".btn:hover" [("background", "#0369a1")],
  keyframes "ripple" [
    ("0%,100%", [("box-shadow", "0 0 0 8px rgba(239,68,68,0.25)")]),
    ("50%",     [("box-shadow", "0 0 0 12px rgba(239,68,68,0.2)")])
  ]
]
```

```lean
open LeanTea.Js LeanTea.Js.E LeanTea.Js.S LeanTea.Js.Dom in
def helloFn : Stmt :=
  afn "hello" [] [
    constV "btn" (getById "btn-hello"),
    doE (addEventListener (i "btn") "click"
      (aarrow [] [doE (await_ (call (i "alert") [s "hi"]))]))
  ]
```

`Block.render` produces compact (one-line) JavaScript — readable but
not pretty. Used by `LeanTea.Rpc.clientLib` to emit the typed RPC
client functions.

## Google OAuth login

`LeanTea.Auth` is plugged into the example servers (`sheet_serve`,
`reversi_serve`, …) and activates **only when the environment
variables below are set** so local development is unaffected.

| Variable                | Required | Example                                      |
| ----------------------- | -------- | -------------------------------------------- |
| `GOOGLE_CLIENT_ID`      | yes      | `123-abc.apps.googleusercontent.com`         |
| `GOOGLE_CLIENT_SECRET`  | yes      | `GOCSPX-...`                                 |
| `BASE_URL`              | yes      | `https://your-app.fly.dev` (for redirect_uri) |
| `COOKIE_SECURE`         | no       | `1` to flag cookies `Secure` on HTTPS        |
| `ALLOWED_EMAILS`        | no       | comma-separated email allowlist              |

When both `*_ID` and `*_SECRET` are present:

- `GET /auth/google/login` → mints a CSRF state and 302-redirects to Google
- `GET /auth/google/callback?code=&state=` → posts to Google's `/token` and `/userinfo` via `curl(1)`, mints a session, sets an HttpOnly cookie
- `GET /auth/logout` → drops the cookie and the DB row
- `/mcp` and a few static asset paths are on a public allowlist
- API paths under `/api/*` return 401 when unauthenticated; UI paths 302 to the login page
- Sessions live in the `sessions` SQLite table, CSRF state in `oauth_states`

### Google Cloud Console setup

1. Create an OAuth 2.0 Client ID (Web application) at
   https://console.cloud.google.com/apis/credentials.
2. Add `${BASE_URL}/auth/google/callback` to "Authorized redirect URIs".
3. Approve `email`, `profile`, `openid` on the consent screen.
4. Push the secrets to Fly:

   ```sh
   flyctl secrets set \
     GOOGLE_CLIENT_ID=… \
     GOOGLE_CLIENT_SECRET=… \
     BASE_URL=https://your-app.fly.dev \
     COOKIE_SECURE=1 \
     ALLOWED_EMAILS=you@example.com
   ```

### Implementation notes

- HTTPS calls to Google go through `curl(1)` (the Lean stdlib has no
  TLS). The runtime image already bundles curl.
- Session tokens are 32 random bytes from `/dev/urandom`, hex-encoded
  (`Auth.randomToken`). **Important**: use `IO.FS.Handle.mk` +
  `read 32` — `IO.FS.readBinFile` reads to EOF, and `/dev/urandom`
  never EOFs, so it spirals into an OOM. Caught the hard way.
- `Auth.gate cfg store publicPaths inner` is a `Handler → Handler`
  wrapper. The inner handler is typed as `Session → Handler` so the
  logged-in user is available without leaking through globals.

## Cloud deployment

### Docker (works on any container host)

`Dockerfile` and `fly.toml` live at the repo root. The multi-stage
build installs the Lean toolchain on Debian Bookworm, builds the
binary, then copies just the binary into a slim runtime image
(~170 MB).

```sh
docker build -t leantea-sheet .
docker run -d --name leantea \
    -p 8080:8080 \
    -v leantea_data:/data \
    leantea-sheet
open http://127.0.0.1:8080/
```

`/data` holds the SQLite file, so a container restart keeps history.

### Fly.io (free tier)

The hobby tier is free for shared-cpu-1x × 3 machines (256 MB each)
with up to 3 GB of persistent volume. `auto_stop_machines` is on, so
machines park themselves when idle and there's a 10–20 s cold start
on the next request — perfect for a personal app.

```sh
curl -L https://fly.io/install.sh | sh
flyctl auth login

flyctl launch --no-deploy          # picks app name + region
flyctl volumes create sheet_data --region nrt --size 1
flyctl deploy
flyctl open
```

### Other options

- **Oracle Cloud Free Tier** — Always-free ARM Ampere instances
  (4 vCPU, 24 GB RAM). SSH in and run the binary, or deploy the same
  Docker image.
- **Render** — Free web service tier (sleeps after 15 min idle).
  Dockerfile works out of the box; persistent disk requires a paid
  plan.
- **Google Cloud Run** — Pay-per-use, 2 M requests/month free. The
  Lean binary runs fine; persistence has to move to Firestore or
  Cloud SQL since the FS is ephemeral.

## Development & testing

LeanTEA's testing story has three layers, by intent:

| Layer | Tool | Purpose | Status |
|---|---|---|---|
| **Type-level proofs** (the negative space) | The compiler itself + `Proof of Authorization` + `SafeQuery` | "Could this auth bypass / SQL injection / XSS *ever* happen?" — answered statically. **What the compiler proves, you don't have to test.** | ✅ shipped |
| **Unit & smoke** | [`LeanTea.LSpec`](LeanTea/LSpec.lean) — a tiny LSpec-shaped runner with `group` / `it` + tree output | Pure-function business logic, `update : Msg → Model → Model`, codec round-trips, render output. Used by every `examples/Smoke/*` binary. | ✅ shipped |
| **E2E (LLM-driven, exploratory)** | [`examples/UiScript`](examples/UiScript/Run.lean) + `browser_mcp_serve` | Declarative JSON scripts (click → screenshot → LLM classify) that survive DOM refactors because the LLM reasons about *intent*. Pairs with [`examples/BrowserAgent`](examples/BrowserAgent/Run.lean) for record-once / replay-many. | ✅ shipped |
| **E2E (typed, deterministic)** | `LeanTea.WebSpec` (planned v0.2) — `do`-notation over `ChromeCdpMcp` | CI/CD regression tests with `group "login flow" [ it "rejects bad password" do … ]`. Same mental model as LSpec, but with `navigate` / `fill` / `click` / `expectText` primitives. | 🚧 planned |

What this means in practice:

- Security tests **don't exist** in a LeanTEA codebase — the compiler
  already enforced the property. Your test suite is just the
  happy-path business logic, which is drastically smaller than the
  equivalent Rails / Django suite (most of which exists to assert
  "the framework didn't let this bad input through").
- For UI regressions today, write a `UiScript` JSON and let the LLM
  drive it. For deterministic golden tests, wait for v0.2 or hand-roll
  the `chrome_cdp_*` calls in a smoke binary — the surface area is
  already there.

### Dev loop

`tools/dev.py` is a tiny stdlib-only file watcher: it `lake build`s on
save, restarts the dev server with `DEV_MODE=1`, and the page polls
`GET /_dev/ping` once a second so the browser auto-reloads after a
successful build.

```sh
python3 tools/dev.py --app sheet --port 8801
```

## Community

- **Discord**: [https://discord.gg/94Xueve8WD](https://discord.gg/94Xueve8WD)
  — design discussion, weekly progress threads, beginner Q&A.
- **GitHub issues**: bug reports + feature requests on this repo.
- **Roadmap & security model**: [ROADMAP.md](ROADMAP.md) +
  [SECURITY.md](SECURITY.md).

## License

`c/sqlite3.c` / `c/sqlite3.h` are public domain
(https://www.sqlite.org/copyright.html). Everything else is MIT.
