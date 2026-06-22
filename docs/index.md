# LeanTEA — the book

A Lean 4 framework for writing **the entire web app in Lean**, on both
ends of the wire, with one mental model: the Elm-style triple
(`Model / Msg / update / view`).

## Read in order

- **0 · [Quickstart](00-quickstart.md)** — `lake new` to a running web app in ten minutes. Start here.
- **1 · [Overview](01-overview.md)** — what LeanTEA is, what each piece covers, what ships, how to build.
- **2 · [Frontend (Elm-style)](02-frontend.md)** — `Model / Msg / update / view`, the typed `Html` and `Css` layers, where browser JS lives. Worked example: Reversi.
- **3 · [Backend (Elm-style)](03-backend.md)** — the stateless API loop, `Handler`, sessions, the typed HTML shell. Worked example: Sheet.
- **4 · [Persist](04-persist.md)** — typed SQLite tables via `Entity` and `Repo`, manual migrations, the `Store` aggregate.
- **5 · [RPC](05-rpc.md)** — one `Endpoint` record drives router + JS client + discovery doc.
- **6 · [LeanJs](06-leanjs.md)** — the `.leanjs` subset and when to reach for it. Language reference: [`LeanJs/README.md`](../LeanJs/README.md); LeanJs vs JS vs Lean: [`LeanJs/COMPARISON.md`](../LeanJs/COMPARISON.md).
- **7 · [Template](07-template.md)** — `.html` files with `{{var}}`, `{{#each}}`, `{{#if}}`, `{{#include}}` and hot-reload via `Provider`.
- **8 · [MCP servers](08-mcp.md)** — the `LeanTea.Mcp` library, stdio + HTTP transports, and how the bundled Chrome-CDP / ComfyUI / Browser / Desktop / Image servers are built.
- **9 · [Architecture overview](09-architecture.md)** — the system map. Once you've seen the parts, here's how they fit and where state lives across a restart.
- **10 · [Testing](10-testing.md)** — three strategies (compiler-as-test + LSpec + LLM-driven `UiScript` + deterministic `WebSpec`) and what you *don't* test because the type system already did.
- **11 · [Secure by Construction](11-secure-by-construction.md)** — `Auth.Proof`, `SafeQuery`, `SafeHtml`, `SafePath`, `SafeCmd`, `SafeRedirect`, `Response.setHeader` + `defaultSecurityHeaders` — eight shipped construction-time security primitives, walked through with the exact compile errors and IPA/OWASP mapping. **Read this if you came for the headline.**
- **12 · [WebSpec](12-webspec.md)** — typed deterministic E2E tests. `do`-notation over Chrome DevTools Protocol, LSpec-shaped tree, ten primitives (`navigate` / `fill` / `click` / `waitFor` / `expectText` / `screenshot` / …). The complement to UiScript when you want CI-friendly golden runs instead of LLM-judged exploratory ones.

## Skim by question

| If you want to … | Start at |
|---|---|
| Decide whether LeanTEA fits | Overview |
| Add an interactive page | Frontend |
| Add a route or stateless API | Backend |
| Store something across restarts | Persist |
| Share an endpoint between Lean and JS | RPC |
| Generate browser JS from Lean | LeanJs |
| Build a static HTML shell | Template |
| Drive a browser / Chrome / desktop from an LLM | MCP servers |
| Understand the whole system | Architecture overview |
| Test a LeanTEA app (and what *not* to test) | Testing |

## What ships

| App | Binary | Source |
|---|---|---|
| Counter (TUI demo) | `lake exe counter` | `examples/Counter/` |
| Quiz (TUI demo) | `lake exe quiz` | `examples/Quiz/` |
| Counter (browser demo) | `lake exe counter_web` | `examples/CounterWeb/` |
| Sheet (functional spreadsheet + MCP) | `lake exe sheet_serve` | `examples/Sheet/` |
| Reversi (server SPA) | `lake exe reversi_serve` | `examples/Reversi/` |
| Chrome-CDP MCP | `lake exe chrome_cdp_mcp_serve` | `examples/ChromeCdpMcp/` |
| Playwright Browser MCP | `lake exe browser_mcp_serve` | `examples/BrowserMcp/` |
| ComfyUI MCP | `lake exe comfyui_mcp_serve` | `examples/ComfyuiMcp/` |
| Desktop MCP (macOS Quartz) | `lake exe desktop_mcp_serve` | `examples/DesktopMcp/` |
| Image MCP (HTML→PNG) | `lake exe image_mcp_serve` | `examples/ImageMcp/` |
| Browser agent (LLM driver) | `lake exe browser_agent` | `examples/BrowserAgent/` |
| UI script runner | `lake exe ui_script` | `examples/UiScript/` |
| UI HTML report | `lake exe ui_report` | `examples/UiReport/` |
| LeanJs CLI trio | `lake exe leanjs_{compile,interp,run}` | `examples/Tools/` |
| Spec runner | `lake exe leanjs_spec` | `examples/Tests/` |

Everything in the book quotes code from one of these — nothing is
pseudocode. Where a chapter shows a snippet, the same shape exists in
the corresponding example.
