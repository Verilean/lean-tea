# 1 · Overview — what `LeanTEA` is

`LeanTEA` is a Lean 4 framework for writing **the entire web app
in Lean**, on both ends of the wire, using one mental model:
**the Elm-style triple** (`TEA = The Elm Architecture`).

```
Model  : Type        -- all your application state
Msg    : Type        -- every event that can change state
update : Msg → Model → Model
view   : Model → Html
```

That triple is the spine. Around it the framework gives you:

| You write | …in this style | The framework gives you |
|---|---|---|
| HTML | typed Lean (`LeanTea.Html`) | server-side render, dynamic refresh |
| CSS | typed Lean (`LeanTea.Css.Sheet`) **or** plain `.css` files | both supported |
| Browser JS | `.leanjs` file **or** typed `LeanTea.Js` DSL | compile to JS, inline in the page |
| HTTP routes | one `Endpoint` record per route | server router + JSON client + discovery doc, generated |
| SQLite tables | `Entity` instance per table | typed CRUD via `Repo` |
| HTML shells | `.html` files with `{{var}}` / `{{#each}}` / `{{#if}}` / `{{#include}}` | hot-reload in `--dev` |
| MCP servers | `LeanTea.Mcp.Handler` + tool list | stdio + HTTP transports |

The same `Model / Msg / update / view` shape applies whether you're
building a stateless ping endpoint or an interactive board game. The
next two chapters split it: **frontend** is `update / view`
plus the typed Html / Css / Js layers; **backend** is the same shape,
just with the model that survives across requests in SQLite instead of
in memory.

> **Just want to start coding?** → [Chapter 0 · Quickstart](00-quickstart.md)
> walks you through `lake new` to a running Elm-style web app in
> ten minutes.

## Why LeanTEA? Secure by Construction

LeanTEA's *killer* property — and the reason we picked Lean 4
specifically — is that the compiler doesn't only check that your
code runs; it checks that whole **classes of vulnerabilities can't
even be expressed**.

- **SQL injection is impossible** when the typed `Repo` accepts only
  parameter-bound queries; concatenating user input into a query is
  a type error.
- **XSS is impossible** when DOM text comes from `LeanTea.Html` rather
  than from raw strings — untrusted text never reaches the page
  without escaping.
- **Missing authorisation is a build error** when the handler signature
  demands a `Proof Admin` argument that only the auth middleware can
  produce.
- **Invalid state transitions can't compile** when the `Msg → Model →
  Model` shape uses Lean's dependent types to forbid e.g. paying out
  a balance below zero.

You won't read about an exploit because **the AI typing the code
couldn't write a bug-shaped program in the first place** — the
compiler rejects it, the LLM retries, you ship.

This is the property other framework stacks (Spring / Django /
Rails / Next) cannot back-port: their root languages lack a proof
system. LeanTEA inherits it from Lean 4 for free.

The current docs cover the Elm-style triple and the surrounding
plumbing. The secure-by-construction primitives (`SafeQuery`,
`SafeHtml`, `Proof of Authorization`) are tracked in
[`SECURITY.md`](../SECURITY.md) as the next major milestone.

## What this book covers

```text
 1. Overview               ── you are here
 2. Frontend (Elm-style)   ── Model / Msg / update / view, Html, Css, Js
 3. Backend (Elm-style)    ── stateless HTTP loop, sessions, typed shell
 4. Persist                ── Entity / Repo / SQLite
 5. RPC                    ── one Endpoint record drives router + client
 6. LeanJs                 ── the .leanjs subset, when to reach for it
 7. Template               ── {{var}} / {{#if}} / {{#each}} / {{#include}}
 8. MCP servers            ── LeanTea.Mcp + Chrome/Browser/Comfy/Desktop
 9. Architecture overview  ── the system map
```

Each chapter follows the same shape:

1. **What it is** — one paragraph
2. **Smallest example that does the job**
3. **API surface** — what you actually type
4. **When to use / when not to** — the trade-off

## Who this is for

You've used Lean 4 a bit. You've seen Elm (or Redux / Reagent /
anything unidirectional). You want a single language end-to-end. You
don't need full CommonMark / a real type-checker on every line / a
generic plugin system — you want the small set of pieces LeanTEA
ships, well documented, and the same `Model/Msg/update/view` mental
model carrying all the way through.

## What ships

Every example here is real and runnable:

| App | Binary | Source |
|---|---|---|
| Counter (TUI) | `lake exe counter` | `examples/Counter/` |
| Quiz (TUI) | `lake exe quiz` | `examples/Quiz/` |
| Counter (browser) | `lake exe counter_web` | `examples/CounterWeb/` |
| Sheet (functional spreadsheet + MCP) | `lake exe sheet_serve` | `examples/Sheet/` |
| Reversi (server SPA) | `lake exe reversi_serve` | `examples/Reversi/` |
| Chrome-CDP MCP | `lake exe chrome_cdp_mcp_serve` | `examples/ChromeCdpMcp/` |
| Browser MCP (Playwright) | `lake exe browser_mcp_serve` | `examples/BrowserMcp/` |
| Browser agent | `lake exe browser_agent` | `examples/BrowserAgent/` |
| ComfyUI MCP | `lake exe comfyui_mcp_serve` | `examples/ComfyuiMcp/` |
| Desktop MCP | `lake exe desktop_mcp_serve` | `examples/DesktopMcp/` |
| Image MCP | `lake exe image_mcp_serve` | `examples/ImageMcp/` |
| UI script runner | `lake exe ui_script` | `examples/UiScript/` |
| LeanJs CLI trio | `lake exe leanjs_{compile,interp,run}` | `examples/Tools/` |
| Spec runner | `lake exe leanjs_spec` | `examples/Tests/` |

When a chapter quotes code, you can grep it in the corresponding
example. Nothing in this book is pseudocode.

## Build and run

```sh
# Build everything (~3 min cold, seconds incrementally)
lake build

# Run a TUI counter
./.lake/build/bin/counter

# Serve Sheet at http://127.0.0.1:8002
./.lake/build/bin/sheet_serve

# Serve Reversi at http://127.0.0.1:8005
./.lake/build/bin/reversi_serve

# Drive an existing Chrome instance via CDP (launch Chrome with
# --remote-debugging-port=9222 --user-data-dir=/tmp/chrome-cdp first)
./.lake/build/bin/chrome_cdp_mcp_serve --stdio
```

The smoke tests under `examples/Smoke/` exercise each subsystem
(`http_client_smoke`, `sqlite_smoke`, `crypto_smoke`, `template_smoke`,
etc.) — read them as the shortest possible "this is what works" for
the corresponding module.
