# 10 · Testing — three strategies, one mental model

LeanTEA's testing story has **three layers** and a deliberate hole
where the fourth would go. Each layer answers a different question; you
mix them per app.

| Layer | Tool | Answers |
|---|---|---|
| **0 — type-level proof** | The compiler + `Auth.Proof` + `SafeQuery` | "Can `class X of bug` *ever* happen?" — statically. |
| **1 — unit / smoke** | `LeanTea.LSpec` | "Does this pure function compute the right value?" |
| **2 — E2E exploratory** | `UiScript` + `browser_mcp_serve` | "Does the UI still work after we refactored the DOM?" — LLM-driven, resilient. |
| **3 — E2E deterministic** | `LeanTea.WebSpec` (shipped v0.2) | "Does *this exact* button click produce *this exact* outcome?" — golden, CI-friendly. See [Chapter 12](12-webspec.md). |

The hole — Layer 0 — is where most frameworks have nothing. LeanTEA
fills it with the type system. That's the secure-by-construction story
(see [SECURITY.md](../SECURITY.md)).

## Layer 0 — what the compiler tests for you

If a class of bug is unrepresentable in code that compiles, you don't
need a test for it. LeanTEA puts four classes of bug behind the type
system:

| Bug class | LeanTEA primitive | Test you'd normally write | Test you write here |
|---|---|---|---|
| Authorization bypass | `Proof c` argument on the handler | Per-endpoint "guest can't hit /admin/delete" | **none** — guest can't even construct a `Proof .admin` |
| SQL / command injection | `SafeQuery` + private constructors | Fuzz the search box with `';--`, `' OR 1=1`, etc. | **none** — `String → Where` doesn't exist |
| XSS | `Html.SafeAttr` (v0.2) | DOM-injection fuzz suites | **none** — untrusted text can't become `SafeAttr` |
| Invalid state transitions | `Transition s s'` GADT (v0.2) | "can't pay a draft order" assertions | **none** — `apply Transition.pay` only typechecks against `Order .submitted` |

The size argument: skim a typical Django or Rails test suite — a
substantial fraction of the test bodies exist to assert "the framework
didn't let this bad input through." Move those tests into the type
system and **your remaining suite is drastically smaller, focused
almost entirely on happy-path business logic**.

## Layer 1 — `LeanTea.LSpec` (unit + smoke)

A 60-line LSpec analogue. Every `examples/Smoke/*` binary uses it.

```lean
import LeanTea
open LeanTea.LSpec

def specs : LSpec :=
  group "Model.update" [
    it "inc bumps count" (update .inc { count := 0 } == { count := 1 }),
    it "reset zeros" (update .reset { count := 42 } == { count := 0 })
  ]

def main : IO Unit := lspecIO specs
```

Run with `lake exe my_app_spec` — exit code is the failure count, tree
output goes to stdout. Wire it into CI like any other binary.

What goes in LSpec:

- `update : Msg → Model → Model` — every reducer
- `encodeModel` / `decodeModel` round-trips
- Render output (snapshot the HTML string for a known `Model`)
- Domain calculations (price totals, scoring, scoring rules)

What doesn't:

- HTTP round-trips (use a smoke binary that hits `Net.Http`)
- DOM behaviour (use a Layer 2 or 3 test)
- Anything the type system already proved (Layer 0)

## Layer 2 — `UiScript` (LLM-driven, exploratory)

`examples/UiScript/Run.lean` ships today. JSON script + LLM = a test
that survives a CSS refactor.

```json
{
  "steps": [
    { "act": "navigate",  "url": "/login" },
    { "act": "screenshot", "expect": "login_form_visible" },
    { "act": "fill",       "selector": "#email",    "text": "alice@x.com" },
    { "act": "fill",       "selector": "#password", "text": "wrong" },
    { "act": "click",      "selector": "button[type=submit]" },
    { "act": "screenshot", "expect": "error_displayed" }
  ]
}
```

The runner connects to `browser_mcp_serve` (Playwright or Chrome-CDP),
executes each step, and on every `expect` asks the configured LLM
"does this screenshot show `error_displayed`?" If the LLM disagrees, a
one-line entry lands in `~/.cache/leantea-agent/escalations.jsonl` and
the run exits non-zero.

**Why this is unusual**: the test doesn't pin down "the error div has
class `.alert-danger`." It pins down "an error is visible." Rewrite
the CSS, refactor to a toast notification, change the wording from
"Invalid" to "Login failed" — the test still passes. Tests that
break because of a CSS refactor are noise; this layer kills that noise.

**When it's wrong**: when "an error is visible" *is too loose*. You
want golden screenshots for a brand-critical button colour or a pixel-
exact dashboard layout. That's Layer 3 territory.

## Layer 3 — `LeanTea.WebSpec` (shipped v0.2)

`do`-notation over `ChromeCdpMcp`'s tool surface. The framework
ships [`LeanTea.WebSpec`](../LeanTea/WebSpec.lean) plus a
worked-example smoke at [`examples/Smoke/WebSpec.lean`](../examples/Smoke/WebSpec.lean)
that drives `counter_web`. Full walk-through (env setup, CDP
plumbing, primitives, escape hatches) is **[Chapter 12](12-webspec.md)**.

The minimum-viable shape:

```lean
import LeanTea
import LeanTea.WebSpec
open LeanTea.WebSpec

def counterSpec : Spec := group "counter_web" [
  it "initial render shows count = 0" do
    navigate "/"
    waitFor ".card p"
    expectContains ".card p" "count = 0"
  ,
  it "two inc clicks bump the counter to 2" do
    navigate "/"
    waitFor "a[data-msg=inc]"
    click "a[data-msg=inc]"
    click "a[data-msg=inc]"
    expectContains ".card p" "count = 2"
]

def main : IO Unit := do
  let d ← Driver.openFresh "http://127.0.0.1:9222" "http://127.0.0.1:8001"
  let code ← runSpec d counterSpec
  d.close
  if code != 0 then IO.Process.exit code.toUInt8
```

Shipped primitives: `navigate`, `evaluate`, `getText`, `fill`, `click`,
`waitFor`, `expectText`, `expectContains`, `expectUrlContains`,
`screenshot`. The spec tree is the same `it` / `group` value you
already know from LSpec, but each `it` takes a `StepM Unit` (Reader +
Except + IO) instead of a raw `Bool`. Failures bubble through
`throw` and surface in the tree exactly like LSpec failures.

Why this is its own layer instead of "just use UiScript":

- Deterministic — no LLM classify call, no flakiness from the model
  declining to answer
- Cheap — every step is one CDP round-trip; no screenshot + classifier
- Hand-writable — most failure modes are a missing `waitFor`, easy to
  fix; UiScript failures are usually "the LLM saw the screenshot
  differently today"

Why we shipped Layer 2 first: design exploration. The LLM-driven path
proves that the framework *can* be tested without a typed harness, so
WebSpec doesn't have to be the only path. Some teams might prefer
UiScript alone; others want both. Two-layer testing — typed +
exploratory — is the proposition we put forward.

For everything else (Chrome bootstrap, CI integration patterns, the
session-aware `asUser` helper that's still on the v0.3 roadmap), see
**[Chapter 12 · WebSpec](12-webspec.md)**.

## Dev loop

`tools/dev.py` watches the source tree, runs `lake build` on save,
restarts the dev server with `DEV_MODE=1`, and ships a one-line
auto-reload poll inside the page so the browser refreshes after every
successful build. Use it as the inner loop while writing Layer 0 / 1
tests; Layer 2 / 3 tests run against the built binary.

```sh
python3 tools/dev.py --app sheet --port 8801
```

## What we deliberately don't do

- **No "framework-blessed mocking layer."** Pure-function business
  logic doesn't need mocks; effects go through narrow `IO`
  signatures that smoke binaries supply with real implementations.
- **No "snapshot every render" addiction.** It's tempting because LSpec
  makes it cheap. Don't — a snapshot is only useful when the snapshot
  itself is a property you'd want to assert.
- **No test runner pulls third-party assertion DSLs.** `LSpec` is
  `group : String → List LSpec → LSpec` and `it : String → Bool →
  LSpec`. That's two constructors. Add helpers in your own app if
  you want fluent chaining — the framework stays minimal.
