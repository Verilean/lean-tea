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
- the goal is reached when an assertion that expects "yes" passes and
  a second question ("has the WHOLE goal been achieved? what is left?")
  agrees. A 4B model tends to assert something easy once it has made
  some progress ("is the chapter ready to run?") and stop halfway. When it
  says no, the run goes on with what is left.
  The same question is asked after every action, after any step that
  fails, and before accepting that the model gives up: a small model otherwise keeps clicking past a finished goal
  ("click Quest" went on to Main Story and Sortie), or checks for the
  wrong thing and wanders off (looking for the battle after it was won,
  then pressing Next). Once the answer is yes, the model is told to
  assert what's on screen or reply done; if its own check fails anyway,
  the goal ends as done without a check. The judge sees each step as
  `DONE: <step> (purpose: <the model's reason>)`, so "click the title
  (purpose: tap to start)" counts as the tap the goal asked for. A reason
  written as "the goal is to click Next" read as a click still to make
  until "DONE:" was put first. On `/demo-game`, "Tap to start",
  "Click Quest" and "Run the main story quest until the battle is won"
  each stopped where the goal did.

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

#### Scenarios

Each test being recorded is a scenario, picked in the header: **New**
starts an empty one (the current one is kept), the selector switches,
**Rename** and **Delete** do what they say. A scenario keeps its own
steps, goals, plan and chat in `--dir`/scenarios/<name>.json; the open
one is reopened after a restart when it tests the same start page.
**Save test** names the file after the scenario unless given a name.

#### Picking from the screen: the model reads, you choose

A small model reads a screen well (where the buttons are, what they
say) and decides badly (what to click next, whether a goal is done). So
the most direct way to record a test is to let it only read: **Read
screen** lists the buttons, tabs and icons with their boxes
(`LeanTea.Vision.Elements`), numbered on the live view and in a list
under it. Pick one to **click** — it is clicked, recorded as
`wait_until "Is <label> visible…"` + `click_xy`, and the new screen is
read — or to **check** — recorded as a `wait_until` that it is shown.
Anything the list misses can still be clicked on the live view.

On the whole screen a 4B model fills its list with whatever repeats
most (a column of times, a row of dates) and misses the menus, so the
screen is read a quarter at a time (overlapping by 60 px, duplicates
merged). On a game home screen: whole screen 30 items, mostly one
repeated time label, no menu (10 s); quarters 17 items, the whole menu
bar and side icons (23 s with settling).

#### Broad instructions: plan, run, then edit

A 4B model explores one short goal well and loses its way on a broad
one ("clear every stage of chapter 1"). Type the broad instruction and
press **Plan** instead of Send. The model looks at the screen and writes
a plan (`LeanTea.Vision.Plan`): sub-goals one per line, with loops.
The plan appears in the Plan panel as editable text:

```
Open Quest, then Main Story, and check that Chapter 1 is shown
repeat until "Does the screen say All stages cleared?" max 4:
  Sortie, skip the story and check that the battle is won
  Click Next, then open Quest and Main Story, and check that Chapter 1 is shown
end
```

Text that is already a plan (several lines, or a `repeat until` block)
is taken as written instead of being sent to the model. Edit it and
press **Run plan**. Each sub-goal is explored in turn and
its steps are recorded under it. A loop becomes `repeat` …
`end_repeat` steps in the script (`{"act": "repeat", "until": "…",
"max": 4}`, `{"act": "end_repeat"}`):

- before each round the question is asked, and yes leaves the loop;
- the first round is explored;
- later rounds replay the steps that round recorded;
- more than `max` rounds fails.

`vision_qa` runs the loop the same way. A plan that stops (a sub-goal
not reached) can be helped with a hint or a click on the screen, or
edited (drop the lines already done) and run again: recording goes on
from where it is.

To tune the recorded test afterwards:

- the steps are listed under their sub-goals; hover a step for
  **▶** (replay from that step on the current screen) and **×**
  (delete it);
- **Script JSON** is editable: change a target, a question or a loop's
  `max`, then **Apply edits**.

The model's own plans are drafts. For "clear every stage of chapter 1"
it wrote a loop, but also a first round outside the loop and a guess
at screens it couldn't see yet. Hence the editor. With the hand-edited plan above on `/demo-game`
(now three stages, then "All stages cleared!"), round 1 was explored
(8 steps), rounds 2 and 3 replayed them, and the loop ended on the
cleared screen. The whole 15-step test replayed from the start page
(33 steps run, 140 s). With `max` edited down to 2 it failed with
"still not done after 2 rounds".

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
- For something that plays by itself for a while (a battle), the model
  can propose `wait_until` with a yes/no question (up to 600 s). A
  `wait_until` gives up early when the screen hasn't changed for 20 s
  (less than 0.3% of it moved): a story page or a dialog waits for a
  click, not for time. Plain
  waits may repeat up to 3 in a row; then it's told the screen needs an
  action (a story scene waits for a click, not for time).
- `click_described` takes up to 3 looks (600 ms apart) to find its
  target and a yes-assertion up to 3 looks to pass, so a blinking
  "TAP TO START" or a screen mid-transition doesn't fail a run; a
  no-assertion must hold on the first look. (Settling and retries cost
  time: the 10-step sign-in test now replays in ~35 s instead of ~12 s.)
- A new test starts with `wait_until "Is <first click target> visible?"`
  (a VLM poll), so a replay doesn't act while the game is still loading
  — the loading and title screens differ too little for a pixel check.

- When the model can't find a target, **click it in the live view**. With
  nothing running the studio clicks there and records it; during a run
  the click becomes the run's next step. Text sent during a run is
  passed to the model as a hint. The model can't see the mouse cursor,
  so a hint like "below the cursor" doesn't help it; point instead.
  A pointed click is recorded as `wait_until "Is <what's there>
  visible?"` (the VLM names a crop around the point) followed by
  `click_xy`, so a replay waits for the screen before clicking a
  fixed point. On `/demo-game`, pointing at TAP TO START and Quest
  recorded 4 steps whose replay passed in 5 s.

#### Working on the studio without losing the game

```
tools/qa-studio-dev.sh --url https://example.com/game --profile ~/.cache/qa-profile --clip auto --headed
```

rebuilds `qa_studio` when its sources (`LeanTea/`, `examples/QaStudio/`,
the browser bridge) change and restarts it. A restart takes a few
seconds and keeps:

- the browser: `--browser-port N` (the script uses 9333, or
  `QA_BROWSER_PORT`) runs it as its own process, which the bridge
  attaches to over CDP and leaves open on exit. The game stays logged
  in and on the same screen. Close it with
  `pkill -f remote-debugging-port=9333`;
- the scenarios (recorded steps, goals and chat), from `--dir`/scenarios/ (for the
  same start URL);
- the open studio tab, which reconnects by itself. `--dev` re-reads
  page.html on every load, so a UI edit needs only a reload.

A run in progress at the restart is lost, and a failed build leaves the
running studio alone. `--rules FILE` (one piece of advice per line, `#`
for comments) is re-read every turn, so the model's instructions can be
tuned during a run without restarting. On `/demo-game`, after a restart
the game was still on Quest Select with the 6 recorded steps, and the
next goal ("open Main Story and check Chapter 1 is shown") passed.

`/demo-game` is a stand-in to try this on (portal login, a canvas game
in an iframe, 2.5 s loading, animated background, and a story scene
before the battle whose Skip asks again in a confirmation dialog with a
second Skip). "Skip the story before the battle and check that the
battle starts" clicked the Skip in the corner, then "the Skip button in
the dialog", and verified the battle. Before, the second click was
refused as a repeat of the first, so a skip like that never got past the
dialog. A click may now repeat, and a dialog's button
is described as being in the dialog. Asked to locate a label that shows
twice (the button behind a dialog and the same label inside it), Qwen-VL
lists both boxes; the first one, which was used before, was the one
behind, so the click landed outside the dialog and closed it like
Cancel. `locate` now takes the brightest of several boxes, since a
dialog dims what is behind it. On such a screenshot the click went from
the dimmed corner button to the dialog's button, 3 times out of 3.
With `qwen/qwen3-vl-4b`: logging in through the portal by chat worked;
after a restart the profile skipped the login; "start the game and open
the quest screen, check Quest Select is shown" took 3 proposals / 11 s;
"open the main story, sortie, wait for the battle and check Victory! is
shown" 3 proposals plus a self-chosen 3 s wait. The 8-step replay (gate waited
~7 s for the title) passed, and the saved script passed 3 runs out of 3
under `vision_qa --profile … --clip auto` (19–21 s). Before the 3-look
retry, a replay failed when the screenshot caught the blinking title
prompt in its "off" phase. One planning slip was fixed in the
prompt: on the victory screen the model clicked Next before asserting
the victory — it's now told to assert as soon as the goal is visible.

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
