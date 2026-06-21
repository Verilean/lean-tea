# Roadmap

What's shipped, what's next, and how the next month sequences.

> **Status (2026-06-21)**: initial public commit. TEA / Persist / Rpc /
> Mcp surface is stable; Secure-by-Construction primitives are the
> headline milestone.

## Shipped (initial commit)

- **Elm-style triple** (`Model / Msg / update / view`) in browser + TUI
  via `LeanTea.WebApp`.
- **JSON codec derivation** via `WebApp.deriveJson` (`Model` and `Msg`
  just `deriving ToJson, FromJson`).
- **History API** integration via optional `viewToUrl` / `urlToMsg`
  (`X-Url` header → `pushState` / `popstate`).
- **Typed `Html` + `Css` DSLs**; templating engine with `{{var}}`,
  `{{#each}}`, `{{#if}}`, `{{#include}}` and hot-reload.
- **Persist**: SQLite + MySQL backends, `Entity` / `Repo` typeclasses,
  versioned migration runner, JSON-blob columns for big rows.
- **Net**: pure-Lean HTTP/1.1 server + client, pure-Lean WebSocket
  (RFC 6455) client.
- **Rpc** (Servant-style typed endpoints, generated JS client,
  `/api/_endpoints` discovery doc), **JsonRpc** envelope.
- **Mcp library**: single `Handler` record drives stdio + HTTP
  transports. Six MCP servers shipped (Chrome-CDP, Browser, ComfyUI,
  Desktop, Image, Canvas).
- **Auth**: sessions / CSRF, OAuth2 (Google / GitHub / Microsoft),
  SAML SP, Passkey (WebAuthn / FIDO2).
- **Crypto**: SHA-1/256, HMAC, PBKDF2, Base64, JWT, password hashing,
  optional native libcrypto FFI.
- **LeanJs**: parser + checker + codegen + Lean evaluator + Lean
  emitter, with three CLIs (`leanjs_compile`, `leanjs_interp`,
  `leanjs_run`) and a spec runner.
- **Docs**: 0-Quickstart + 9 chapters + index, all worked examples
  drawn from the public repo (Counter / CounterWeb / Canvas / Reversi
  / ChromeCdpMcp).

## Next milestone (≈ 4 weeks): Secure by Construction PoCs

The goal is to ship one self-contained PoC per primitive in
[SECURITY.md](SECURITY.md), each ~150–250 LOC + tests + a doc, in this
order:

### Week 1 — `Proof of Authorization`

The most marketable demo. Concrete deliverables:

- `LeanTea.Auth.Proof` module (~80 LOC) — `Capability`, `Proof`,
  `HasCapability`, `Proof.issue`, `Proof.weaken`.
- `LeanTea.Rpc.Endpoint` gains `(c : Capability)` parameter; dispatch
  runs `Proof.issue` once per request.
- `Σ c, Endpoint req res c` (Option A from `SECURITY.md`) for
  heterogeneous routing tables.
- Worked example: extend Canvas's `/api/delete` to require
  `Proof .admin`; show the resulting compile error when an unauthorised
  path tries to call the handler.
- Dependent owner example (`def handleEdit (id : String) (proof : Proof
  (.owner id)) (newText : String)`) on a tiny new endpoint.
- Updated `docs/03-backend.md` "Sessions, auth, security" section
  pointing to the new primitives.

### Week 2 — `SafeQuery`

- `LeanTea.Persist.SafeQuery` module (~150 LOC) — `Cols`, `Where`,
  `From` (single-table v1), render-to-SQL with positional binding.
- `Repo.query` rewires onto `SafeQuery E`.
- `.trusted` audit escape hatch with `Lean.Name` tag.
- Migration note: hand-written SQL in existing code stays but flags
  on `lake build` as `.trusted "needs review"`.
- Updated `docs/04-persist.md` section showing the typed builder vs.
  the audited escape hatch.

### Week 3 — `SafeHtml` / `SafeAttr`

- `LeanTea.Html.SafeAttr` (~80 LOC) — `url` / `text` / `num`
  constructors, sanitisers, allow-listed URL schemes.
- `Html.attr` signature changes to require `SafeAttr`; mechanical
  migration of existing call sites.
- Updated `docs/02-frontend.md` Html section showing the safe API.

### Week 4 — Demo video & launch

- 5-minute screen capture: an LLM (via the Chrome-CDP MCP) writing a
  handler that touches admin-only data; the compiler rejects the
  unauthorised call; the LLM re-tries; the build goes green.
- Tweet / blog post pitching the "AI types code → compiler audits →
  ship" loop.
- Submit to `r/lean`, Hacker News, the Elm Discourse, Lobsters.

## Community strategy

Per Gemini's framework review, target **Elm / Haskell FP web
developers** first:

- They feel the "what about the backend" pain Elm leaves them with.
- They get the Lean type story without conversion friction.
- They produce the first real apps that become the case studies we
  bring to Enterprise.

Channels in priority order:

1. **Elm Discourse** ("a Lean port of TEA, with a typed full-stack")
2. **r/functionalprogramming**, **Lobsters**, **HN**
3. **Lean Zulip** ( #general announcement when the secure primitives
   ship )
4. **HN Show** post for the AI-driven Secure-by-Construction story
   once the demo video is ready

We **don't** lead with formal-verification academic venues; Web
practitioners don't read them and the academic surface (TPHOLs,
ICFP) won't move adoption.

## Known gaps (deferred past the first milestone)

| Gap | Why it's deferred | Notes |
|---|---|---|
| **`LeanTea.WebSpec` — typed deterministic E2E** | Wanted to ship; cut to keep v0.1 focused on Secure-by-Construction. Today: `LeanTea.LSpec` (unit) + `examples/UiScript` (LLM-driven E2E). | **v0.2 target**. `do`-notation API over `ChromeCdpMcp`. See [docs/10-testing.md](docs/10-testing.md) for the pitch. |
| Server-Sent Events / WebSocket server | The pattern works; we lack a typed-Msg streaming primitive | Probably 2 weeks once the v1 ships |
| HTTPS / TLS client | curl handles it for now; OpenSSL FFI is a project | `LeanTea.Net.HttpClient` stays HTTP-only |
| `lake exe leantea init` boilerplate generator | `lake new` + the [Quickstart](docs/00-quickstart.md) template covers it | Reassess after the launch reaction |
| State Machine Proof primitive | Domain-shaped, not framework-shaped | See `SECURITY.md` §"Primitive 4" |
| Editor LSP for `.leanjs` | Most "JS-ish" highlighters render it fine | Larger project, separate repo |

## Out-of-scope (forever)

- Visual-novel / game / NSFW templates (the games existed locally
  during development but ship to a private sibling repo)
- Cross-language tooling (GraphQL, gRPC, OpenAPI generation): use
  the underlying type instead of re-encoding it
- Plugin system / hook hierarchy: the framework is values + records;
  hooks would obscure that

## How to contribute

The architecture chapter (`docs/09-architecture.md`) is the orientation
map. The smoke binaries under `examples/Smoke/` are the shortest path
to "is X subsystem still green." The framework's invariant: every code
example in the docs is a verbatim `grep`-able snippet from the public
repo. Patches that break that invariant don't merge.
