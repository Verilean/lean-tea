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

## Layer 2.5 — Vision QA (local VLM, pixel-precise)

When there is no DOM to query — a canvas game, a native macOS app, a
terminal UI — the test has to *look*. `LeanTea.Vision` gives a small
local vision model (Qwen3-VL-4B via LM Studio) four jobs and keeps
everything else deterministic:

| need | API | script action |
|---|---|---|
| where is X? | `locate` / `locateZoom` → `Rect` in screenshot px | `click_described` |
| is X true? | `ask` → yes/no + reason | `assert_visual` |
| what does X say? | `readText` | `assert_text` |
| which screen? | `classify` | `expect` / `wait_for_screen` |
| exact colour | `Image.pixel` (no model) | `assert_pixel` |

`locateZoom` is the pixel-precision trick: a coarse pass on the full
screenshot, then the neighbourhood is cropped, upscaled ×3 and asked
again. Coordinates come back in the model's own convention
(`CoordFrame` — Qwen3-VL answers in 0–1000 normalised units) and are
converted to screenshot pixels.

The same script runs against three drivers (`LeanTea.Vision.Driver`):

```sh
lake build vision_qa
# headless Chromium
./.lake/build/bin/vision_qa examples/VisionQa/scripts/reversi.json \
    --target browser --url http://127.0.0.1:8005/
# the real macOS display (LEANTEA_DESKTOP=1 build; Retina handled)
./.lake/build/bin/vision_qa flow.json --target desktop
# a tmux pane, rasterised with its colours
./.lake/build/bin/vision_qa flow.json --target tmux --tmux-target qa:0.0
```

For tests that need control flow, skip the JSON and write Lean:
`examples/VisionQa/ReversiSpec.lean` (`vision_qa_reversi`) drives the
same checks with `Driver` + `Ground` + `LSpec` in ~90 lines.

`click_described` with a `key` caches the verified point in
`ui-map.json`, so the model is consulted once and later runs replay
the click deterministically. Manifests land in
`~/.cache/leantea-agent/runs/`, so `ui_report` renders them. For
agent use, `vision_qa_mcp_serve` exposes the same primitives as MCP
tools next to `browser_mcp_serve` / `desktop_mcp_serve`.

### Writing tests by chatting — `qa_studio`

```
lake exe qa_studio                                  # http://localhost:8020/ (tests its built-in /demo app)
lake exe qa_studio --url http://127.0.0.1:8005/     # any page
```

One screen: the page under test on the left (a live screenshot from a
headless browser, last click marked), a chat with the local VLM on the
right. Describe a check in plain words; the model works it out by
trial and error (`LeanTea.Vision.Explore`):

- each turn it sees the screenshot, the goal, the steps that worked,
  the last error and an outline of the page's inputs / buttons /
  headings (from the DOM — only to help a 4B model *describe*
  targets; the recorded steps stay visual), and proposes one step:
  `fill` (click a field, type), `click`, `key`, `assert`, `done`, `fail`;
- the step runs through the same kernel as `vision_qa`
  (`LeanTea.Vision.Qa`). Clicks, typing and keys must visibly change
  the page; a step that fails is dropped and the error goes back to
  the model; a repeat of a failed step is refused;
- the goal is reached when an assertion that expects "yes" passes.

Later messages continue from there, so a test is built goal by goal.
**Replay** reloads the start page and runs only the recorded steps;
**Save test** writes a `vision_qa` script (default
`examples/VisionQa/scripts/`) and shows the command. With
`qwen/qwen3-vl-4b` on the demo app, "sign in as alice with the password
secret and check the welcome message" took 4 proposals / 20 s, "add a
task called Buy milk and check it's listed" 3 / 17 s; the replay
passed (10 steps, 12 s) and the saved
`examples/VisionQa/scripts/notes.signin_add_task.json` passes under
`vision_qa` — and fails at the welcome check when the password is
changed to a wrong one.

#### Browser games and sites behind a login

```
# once: log in by hand in the visible window; the profile keeps the session
lake exe qa_studio --url https://example.com/game --profile ~/.cache/qa-profile --headed --clip auto
# afterwards (headless is fine), and for the saved test:
lake exe qa_studio --url https://example.com/game --profile ~/.cache/qa-profile --clip auto
vision_qa test.json --target browser --url https://example.com/game --profile ~/.cache/qa-profile --clip auto
```

- `--profile DIR` keeps a persistent browser profile (cookies, local
  storage). A profile can be open in one browser at a time: stop
  `qa_studio` (or copy the directory) before `vision_qa` uses it.
- `--clip auto` crops every screenshot to the largest iframe / canvas on
  the page — the game — and offsets clicks into it (`--clip SELECTOR`
  or `--clip x,y,w,h` also work). The model never sees the site around
  it; the DOM outline is switched off (a game is a canvas, usually in a
  cross-origin iframe).
- Every action waits for the screen to settle (bounded — animated
  screens never go quiet); the model can also `wait` N seconds, and the
  wait is recorded. On a screen that animates by itself a click is
  judged by the area around it instead of the whole screen.
- A new test starts with `wait_until "Is <first click target> visible?"`
  (a VLM poll), so a replay doesn't act while the game is still loading
  — the loading and title screens differ too little for a pixel check.

`/demo-game` is a stand-in to try this on (portal login, a canvas game
in an iframe, 2.5 s loading, animated background, Japanese buttons).
With `qwen/qwen3-vl-4b`: logging in through the portal by chat worked;
after a restart the profile skipped the login; "start the game and open
the quest screen, check クエスト選択" took 3 proposals / 11 s; "open
メインストーリー, sortie with 出撃, wait for the battle and check 勝利！"
3 proposals plus a self-chosen 3 s wait. The 8-step replay (gate waited
6.9 s for the title) passed in 21 s, and the saved script passed under
`vision_qa --profile … --clip auto`. One planning slip was fixed in the
prompt: on the victory screen the model clicked 次へ before asserting
勝利！ — it's now told to assert as soon as the goal is visible.

Mind the site's terms of service: many game platforms forbid automated
play. Use this on titles you build or are allowed to test.

### Choosing the model — `vision_bench`

Which small model is "accurate enough" is measured, not assumed:

```sh
lake build vision_bench_gen vision_bench_run vision_bench_report
./.lake/build/bin/vision_bench_gen                     # labelled synthetic screens
./.lake/build/bin/vision_bench_run --models qwen/qwen3-vl-4b,google/gemma-3-4b --limit 20
./.lake/build/bin/vision_bench_report bench/vision/runs/*.jsonl   # → bench/vision/report.html
```

`gen` renders five screen kinds (form UI, scattered icons 12–48 px,
dense spreadsheet, pixel-only canvas, terminal UI) in headless
Chromium and takes ground truth from the DOM. `run` discovers each
model's coordinate frame from the data, then scores locate (hit = the
predicted centre lands inside the target), yes/no asserts and OCR.
Answers are cached under `bench/vision/cache/`, which also makes
`vision_spec` (offline, in CI) able to replay recorded answers.

**Results (2026-10, M-series Mac, LM Studio, 20 samples per kind × task).**
Locate = predicted centre inside the target, zoom-refine; sizes are the
target's short side.

| model (LM Studio) | locate | <16 px | 16–23 px | ≥24 px | err p50 | assert | false yes | OCR exact | s / locate |
|---|---|---|---|---|---|---|---|---|---|
| **qwen/qwen3-vl-4b (MLX 4bit)** | **95%** | 90% | 94% | 98% | 1.1 px | **97%** | 0% | 99% | 3.6 |
| qwen/qwen3-vl-4b (MLX 8bit) | 93% | 90% | 92% | 95% | 0.8 px | 97% | 2% | 100% | 4.6 |
| gui-owl-1.5-4b (GGUF Q8) | 88% | 50% | 90% | 95% | 1.2 px | 97% | 4% | 98% | 6.2 |
| qwen/qwen3-vl-8b (MLX 4bit) | 89% | 70% | 88% | 95% | 1.3 px | 99% | 0% | 100% | 7.6 |
| qwen/qwen3-vl-2b (MLX 8bit) | 88% | 80% | 83% | 95% | 1.9 px | 84% | 29% | 96% | 2.8 |
| qwen/qwen2.5-vl-7b (GGUF Q4) | 82% | 50% | 75% | 98% | 4.8 px | 96% | 6% | 100% | 10.5 |
| google/gemma-4-e4b (2048 tok) | 50% | – | – | – | 11 px | 94% | – | 91% | 3.6 |
| google/gemma-3-4b | 0% | – | – | – | – | 60% | 69% | 94% | 3.0 |

Takeaways:

- **Default: Qwen3-VL-4B, 4-bit MLX.** 8-bit buys nothing measurable;
  8B is slower and no better at locating, which matches the public
  ScreenSpot-Pro ranking (4B 59.5 vs 8B 54.6).
- **Zoom-refine matters** for small targets: single-pass 88% → 95%.
- **2B is fast but says "yes" too easily** (29% false positives), so
  it can't be trusted for assertions.
- **Known 4B limits, found by running real scenarios:** spatial
  relations ("same column", "in a line") and very low-contrast
  highlights are unreliable. Assert those through text (`assert_text`
  on a status line) or `assert_pixel` instead.
- Holo2-4B (mradermacher GGUF) raised `Compute error` in LM Studio's
  llama.cpp 2.53 even on text-only prompts, so it is not scored.

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
