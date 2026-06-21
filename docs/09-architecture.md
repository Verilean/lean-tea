# 9 · Architecture overview

> Not a runnable chapter — this is the map you can pin on the wall.

Earlier chapters introduce the parts; this one shows how they fit
together. If you've shipped any of the examples (`sheet_serve`,
`reversi_serve`, `chrome_cdp_mcp_serve`, `counter_web`, …) you have
already exercised most of this picture without naming it.

## The picture

```
┌──────────────────────────────────────────────────────────────────┐
│                       LeanTea (the framework)                    │
│                                                                  │
│   Html ─┐                                                        │
│         ├──► WebApp ──► Model / Msg / update / view (Elm-style)  │
│   Css ──┘                                                        │
│                                                                  │
│   Net.Http      : Request / Response / Handler                   │
│   Net.Server    : serve port host handler                        │
│   Net.HttpClient: pure-Lean HTTP/1.1 client                      │
│   Net.WebSocket : pure-Lean RFC 6455 client                      │
│   Rpc           : Endpoint + dispatch + clientLib                │
│   JsonRpc       : JSON-RPC 2.0 envelope                          │
│   Mcp           : Handler {init, tools, callTool} + transports   │
│   Auth          : Sessions, OAuth2, SAML, Passkey, CSP           │
│   Template      : `.html` files with {{var}} / {{#each}} / etc.  │
│   Persist       : SQLite Entity + Repo, Migrations, Backend      │
│   Js            : Typed JS AST + JsBuilder (for embedding Lean   │
│                   values into client code)                       │
│   Markdown      : CommonMark-ish parser                          │
│   Crypto        : Base64, SHA-1/256, HMAC, PBKDF2, JWT, native   │
│   Json.Helpers  : terse defaults-with-fallback accessors         │
│   Browser       : Playwright-via-node bridge                     │
│   Comfy         : ComfyUI HTTP+WS client                         │
│   Diffuse       : Local diffusers sidecar HTTP client            │
│   Llm.Openai    : streaming OpenAI-compatible client (LM Studio) │
│   LSpec         : tiny test runner used by every smoke binary    │
└──────────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────────┐
│                  LeanJs (the subset compiler)                    │
│                                                                  │
│   Ast       : surface syntax                                     │
│   Parser    : `.leanjs` source → Ast                             │
│   JsParser  : JS-flavoured surface → Ast (same target)           │
│   Check     : arity check on direct call sites                   │
│   Codegen   : Ast → LeanTea.Js.Expr → JavaScript string          │
│   Eval      : Ast → value (Lean-side interpreter, cross-check)   │
│   LeanEmit  : Ast → real Lean source (for `lean --run` bilingual)│
│   Includes  : `#include` resolution                              │
└──────────────────────────────────────────────────────────────────┘
```

## Two ways to produce client-side JS, and when to use which

| Path | Where the source lives | When it shines |
|---|---|---|
| **Typed `LeanTea.Js` DSL** (see Chapter 2 §"Where browser JS lives") | inside Lean (`Js.Expr`, `Js.Stmt`) | embedding Lean values (`ToJsExpr`), conditional generation from server state, anything that needs to type-check against your Lean types |
| **LeanJs subset** (Chapter 6 + `LeanJs/README.md`) | `.leanjs` file on disk | game / SPA logic, anything you'd rather edit in vim with JS-shaped syntax, code that doesn't depend on Lean values |

Both lower through the same renderer (`LeanTea.Js`), so you can mix
them — Sheet does: the toolbar HTML comes from a `.html` template
(`Template`), the JS client comes from the typed DSL with embedded
Lean state.

## End-to-end request, the Sheet version

```
Browser ──HTTP──► Net.Server
                     │
                     ├─ "/api/add?kind=rect&x=…"
                     │     └─ Rpc dispatch: typed handler
                     │         ↳ Store.setCell  (SQLite write)
                     │         ↳ return new shape id (JSON)
                     │
                     ├─ "/mcp" (POST)
                     │     └─ Mcp.handleMcp h req
                     │         ↳ Mcp.dispatchOnce (initialize | tools/list | tools/call)
                     │         ↳ callTool store name args  →  Content (text / image / err)
                     │
                     ├─ "/cells" (SVG fragment)
                     │     └─ build view from Store.shapes  →  Html.render
                     │
                     ├─ "/runtime.js"
                     │     └─ embedded constant (include_str)
                     │
                     └─ "/"
                           └─ Template.renderFlat page [("rpcClient", …)]
```

Two things to note. First, `runtime.js` is served from the embedded
constant rather than a static dist file — that's the only way to keep
the framework's client runtime in lock-step with the server binary you
just built. Second, `/mcp` and `/` share a single port: the MCP tool
surface and the SPA are one binary, no separate gateway.

## How state survives a restart

| What | Lives in | Survives restart? |
|---|---|---|
| Cells on the sheet | `cells` table | yes |
| Page tabs | `pages` table | yes |
| Audit log | `audit` table | yes |
| Current selection / hover | client model only (X-Model) | no — that's session state |
| MCP server config (CDP URL, workspace) | `IO.Ref` in process | no, re-supplied via CLI |
| LLM transcript | not stored by the framework | depends on the MCP client |

The pure update function never touches IO. Everything DB-backed
goes through the server's handlers.

## How the typed DSL composes

```
Sheet/App.lean ─► uses LeanTea.Html ─► server-side render ─► dist HTML
                ─► uses Sheet.Api  ─► LeanTea.Rpc.clientLib ─► generated JS client
                ─► uses LeanTea.Js  ─► dot/call/arrow/etc.   ─► cell render JS

Sheet/Serve.lean ─► uses LeanTea.Net.Server + .Template ─► glue
                                                          ↳ Persist + Mcp
```

The Rpc layer is what keeps the Lean and JS sides honest: each
endpoint declares an `Endpoint` record once, and three artefacts
read from it:

* the server's router (dispatch by path + method)
* the typed client lib (auto-generated JS with the right shapes)
* the JSON discovery document (`/api/_endpoints`)

When you add an endpoint you touch one record; the surface area on
the other side updates for free.

## How the LeanJs pipeline composes

```
examples/Reversi/Game.leanjs ─┐
your-app/page.leanjs         ─┤
                              │
                              ▼
                       LeanJs.Parser
                              │
                              ▼
                       LeanJs.Check  (arity guard against the FFI list,
                                      record-field guard against record decls)
                              │
                              ▼
                       LeanJs.Codegen ─► LeanTea.Js.Expr ─► JS string
                              │
                              └─► LeanJs.LeanEmit (pure subset only)
                                       │
                                       └─► `lean --run` for bilingual cross-check
```

What flows through to `lean --run`? Anything not using `extern js`.
Reversi's logic crosses over partly; anything that touches `window` /
`document` / DOM APIs is JS-only.

The arity check (`LeanJs.Check`) reads each FFI binding's RHS,
extracts the parameter count from `(a, b) => …` and `x => …`
arrows, and verifies every direct call against it. Opaque FFIs
(`extern js "console.log"`) skip the check.

## How the MCP servers compose

```
examples/ChromeCdpMcp/Serve.lean ─┐
examples/ComfyuiMcp/Serve.lean   ─┤
examples/BrowserMcp/Serve.lean   ─┤   (one Handler each — three fields)
examples/DesktopMcp/Serve.lean   ─┤
examples/ImageMcp/Serve.lean     ─┤
examples/Sheet/Serve.lean       ─┘
                              │
                              ▼
                       LeanTea.Mcp.Handler
                              │
                  ┌───────────┴──────────┐
                  ▼                      ▼
            serveStdio                serveHttp port host
            (Claude Code,             (curl / browser /
             Cursor, Zed)              shared SPA port)
```

The shared `LeanTea.Mcp` module owns the JSON-RPC envelope, the
content shapes, the dispatch table, and both transports — Chapter 8
walks through it.

## Where to look next

| Want to … | Read |
|---|---|
| Understand the language we compile to JS | `LeanJs/README.md` |
| Compare LeanJs to plain JS and to real Lean | `LeanJs/COMPARISON.md` |
| See a full client written in the subset | `examples/Reversi/Game.leanjs` |
| Add a new RPC endpoint | `docs/05-rpc.md` |
| Build an MCP server | `docs/08-mcp.md` |
| Embed a Lean value in JS | `docs/02-frontend.md` (the "Where browser JS lives" section) |
| Add a persistence entity | `docs/04-persist.md` + `LeanTea/Persist/Store.lean` |
| Wire a new HTTP route | `LeanTea/Net/Server.lean` + any example `Serve.lean` |
| Drive a real Chrome from Lean | `LeanTea/Net/WebSocket.lean` + `examples/ChromeCdpMcp/Serve.lean` |
