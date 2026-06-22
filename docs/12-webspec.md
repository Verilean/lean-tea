# 12 · WebSpec — typed deterministic end-to-end tests

`LeanTea.WebSpec` is the **CI-friendly E2E layer**: a real Chrome,
a real LeanTEA server, and a `do`-block per assertion. Unlike
[`UiScript`](../examples/UiScript/Run.lean) (Chapter 10, Layer 2)
this layer is **deterministic** — no LLM in the loop, no model
hallucinating that an error message did appear.

> **Mental model**: same shape as [`LeanTea.LSpec`](../LeanTea/LSpec.lean)
> (the `group "x" [it "y" cond, …]` tree you already know), but each
> `it` body is a `do`-block over CDP primitives instead of a single
> `Bool`. Failures `throw` a string; the runner catches and labels.

The framework ships **the runtime + 10 primitives + a worked-example
smoke**. Session-aware helpers (`asUser`, `expectModel`) are on the
v0.3 roadmap.

---

## 1 · The thirty-second pitch

```lean
import LeanTea
import LeanTea.WebSpec
open LeanTea.WebSpec

def loginFlow : Spec := group "login flow" [
  it "renders the empty form" do
    navigate "/login"
    waitFor "form#login"
    expectText "h1" "Sign in"
  ,
  it "rejects an empty password" do
    navigate "/login"
    fill "input[name=email]"    "alice@x.com"
    fill "input[name=password]" ""
    click "button[type=submit]"
    waitFor ".error"
    expectContains ".error" "password required"
  ,
  it "valid creds redirect to /" do
    navigate "/login"
    fill "input[name=email]"    "alice@x.com"
    fill "input[name=password]" "hunter2"
    click "button[type=submit]"
    waitFor "h1"
    expectUrlContains "/"
    expectContains "h1" "Welcome, alice"
]

def main : IO Unit := do
  let d ← Driver.openFresh "http://127.0.0.1:9222" "http://127.0.0.1:8001"
  let code ← runSpec d loginFlow
  d.close
  if code != 0 then IO.Process.exit code.toUInt8
```

Tree output mirrors LSpec:

```
● login flow
  ✓ renders the empty form
  ✓ rejects an empty password
  ✗ valid creds redirect to /
    → expectContains h1: `Sign in` did not contain `Welcome, alice`

  2 passed, 1 failed
```

Exit code = failure count, so CI gates the same way it does on
`leanjs_spec` or `security_spec`.

---

## 2 · Bootstrap — three terminals

WebSpec needs a real Chrome with CDP enabled and a live LeanTEA
server to drive. The three-terminal dance is intentional: it forces
you to confirm each piece is up before the spec runs, and any
intermediate failure has a clear surface.

```sh
# Terminal 1 — the app under test (any LeanTEA HTTP server works)
./.lake/build/bin/counter_web --port 8001

# Terminal 2 — Chrome with remote debugging enabled.
# A separate user-data-dir is REQUIRED on modern Chrome; pointing
# CDP at your normal profile is refused.
google-chrome \
  --remote-debugging-port=9222 \
  --user-data-dir=/tmp/leantea-cdp-profile
#   macOS: open -a "Google Chrome" --args --remote-debugging-port=9222 …
#   Chromium: drop in `chromium` for `google-chrome`
#   Headless CI: append `--headless=new --no-sandbox`

# Terminal 3 — the spec
./.lake/build/bin/webspec_smoke
```

Environment overrides:

| Variable          | Default                       | Purpose |
|---|---|---|
| `CHROME_CDP_URL`  | `http://127.0.0.1:9222`       | The CDP REST + WebSocket host. |
| `APP_BASE_URL`    | `http://127.0.0.1:8001`       | Joined onto relative `navigate "/foo"` calls. |

> **Why a separate profile?** CDP exposes credential autofill, session
> cookies, and history to whatever script connects. Sharing the
> profile with your daily browser means a flaky spec can corrupt your
> Slack login. The framework's smoke ships with the `/tmp/...`
> recommendation; treat it as a hard rule.

---

## 3 · The driver

`Driver` is one CDP target (one tab) bound to an app base URL:

```lean
structure Driver where
  base     : String   -- "http://127.0.0.1:9222"
  wsUrl    : String   -- ws://... for the target this driver owns
  appBase  : String   -- prefix for relative navigate paths
  targetId : String   -- so the runner can close the tab on teardown
```

Two ways to obtain one:

```lean
-- Take over the first existing tab (typically about:blank if Chrome
-- was just launched). Caller does NOT own the tab; close manually.
Driver.connectFirstTab (base : String) (appBase : String := "")
  : IO Driver

-- Open a brand-new tab pointed at `appBase`. The driver owns it;
-- call `d.close` on teardown.
Driver.openFresh (base : String) (appBase : String := "")
  : IO Driver
```

The runner does not auto-isolate per `it`. If two assertions in
the same spec need separate cookies (or one mutates state the other
relies on), open a fresh driver inside the `it` body:

```lean
it "logged-in dashboard" do
  -- Drop the outer driver for this assertion; everything below
  -- runs against a clean tab.
  let d ← liftM (Driver.openFresh "http://127.0.0.1:9222" "http://127.0.0.1:8001")
  ...
```

---

## 4 · The step monad

```lean
abbrev StepM := ReaderT Driver (ExceptT String IO)
```

Three powers:

* **`read : StepM Driver`** — the current driver (read inside any
  custom primitive).
* **`throw msg : StepM α`** — fail the step with a labelled
  message; the runner catches and reports.
* **`IO`** — `liftM` any `IO` action when you need it (e.g. a
  filesystem read or `nowSec`).

The shipped primitives all return `StepM Unit` (assertion) or
`StepM α` (read).

---

## 5 · Shipped primitives

### 5.1 Navigation + scripting

| Primitive | Type | Behaviour |
|---|---|---|
| `navigate (url : String)` | `StepM Unit` | `Page.navigate`. Joins relative URLs against `appBase`. Waits for `document.readyState === "complete"` (3 s budget). |
| `evaluate (expr : String)` | `StepM Json` | `Runtime.evaluate` with `returnByValue=true` and `awaitPromise=true`. Returns the `value` field of the result. |
| `currentUrl` | `StepM String` | `window.location.href`. |

### 5.2 DOM interaction

| Primitive | Type | Behaviour |
|---|---|---|
| `getText (sel)` | `StepM String` | First match's `.textContent`. Fails on no match. |
| `fill (sel) (text)` | `StepM Unit` | React-aware: uses the prototype value setter + `input`/`change` events for `<input>`/`<textarea>`, `execCommand('insertText', …)` for `contenteditable`. Fails on no match. |
| `click (sel)` | `StepM Unit` | Real `.click()` so React handlers fire. Fails on no match. |
| `waitFor (sel) (timeoutMs := 3000) (requireVisible := true)` | `StepM Unit` | `MutationObserver`-backed wait. Visibility = positive bbox AND `offsetParent !== null`. |

### 5.3 Assertions

| Assertion | Pass condition |
|---|---|
| `expectText (sel) (expected)` | First match's text equals `expected` (after `trimAscii`). |
| `expectContains (sel) (substr)` | First match's text contains `substr`. |
| `expectUrlContains (substr)` | `window.location.href` contains `substr`. |

### 5.4 Capture

| Primitive | Type | Behaviour |
|---|---|---|
| `screenshot (path : String)` | `StepM Unit` | `Page.captureScreenshot` (PNG). Writes the base64 string to `path` — pipe through `base64 -d` if you want the raw bytes. (A future revision will decode in-process.) |

### 5.5 The CDP escape hatch

When a built-in doesn't cover what you need (network throttling,
DOM mutation events, a tab-create flow), drop down to `evaluate`
plus `LeanTea.Cdp.cdpCommand`:

```lean
it "throttles the connection" do
  let d ← read
  liftM <| LeanTea.Cdp.cdpCommand d.wsUrl "Network.emulateNetworkConditions"
    (Json.mkObj [
      ("offline",            Json.bool false),
      ("latency",            Json.num (1000 : Int)),
      ("downloadThroughput", Json.num (1024 * 50 : Int)),
      ("uploadThroughput",   Json.num (1024 * 50 : Int))])
  navigate "/dashboard"
  ...
```

Every CDP method ([reference](https://chromedevtools.github.io/devtools-protocol/))
is one `cdpCommand` away. The framework's primitives are just
ergonomic wrappers around the same call.

---

## 6 · Comparing to Playwright / Cypress / Puppeteer

| Concern | Playwright | Cypress | LeanTea.WebSpec |
|---|---|---|---|
| Host language | JS / TS / Py / .NET | JS | Lean 4 |
| In-spec backend access | Out-of-process | iframe trickery | **In-process** — call your `Persist.Store` directly to mint sessions / seed rows |
| Transport | WebDriver BiDi / CDP | Custom over WS | CDP (one-shot WS per call) |
| Mental model | promise chains | promise chains | `do`-block, no implicit await chains |
| Assertion style | `expect(loc).toContainText` | `cy.contains` | `expectContains "sel" "substr"` |
| Type-level proof of selectors | none | none | none (yet — `Selector α` GADT is roadmap) |
| LOC of framework | ~200k | ~60k | ~280 + 50 (Cdp) |

The same-process angle is the big one: a WebSpec test can construct
a `Proof .admin` directly via `Proof.issueOwner` and call
`AuthStore.addSession`, then drive the browser into the resulting
session — no HTTP round-trip needed for the fixture. Playwright /
Cypress have to call a /test-only auth endpoint.

---

## 7 · CI integration

WebSpec is **opt-in in CI** like `browser_vision_smoke` —
GitHub-hosted runners don't get a real Chrome out of the box.
Two paths:

### 7.1 Headless Chrome on the runner

```yaml
- name: Install Chrome headless
  run: |
    wget -qO- https://dl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
    sudo apt-get install -y google-chrome-stable
- name: Run WebSpec
  run: |
    google-chrome --headless=new --no-sandbox \
      --remote-debugging-port=9222 \
      --user-data-dir=/tmp/cdp-profile &
    ./.lake/build/bin/counter_web --port 8001 &
    sleep 2
    ./.lake/build/bin/webspec_smoke
```

### 7.2 GitHub-hosted Chrome action

```yaml
- uses: browser-actions/setup-chrome@v1
```

then `chrome` is on `$PATH`. Same `--remote-debugging-port` bootstrap.

The framework's own `.github/workflows/ci.yml` leaves WebSpec out;
shipping a Chrome runtime is intentionally separate from "the
language part still builds." Add it back once your team commits to
the maintenance cost of browser-in-CI.

---

## 8 · What's not shipped yet (v0.3+)

- **`asUser email`** — mint a session via `Auth.Proof` in-process,
  set the cookie on the target, navigate. The session-aware
  shortcut. Today: open the tab and `fill`/`click` through `/login`.
- **`expectModel (predicate : Model → Bool)`** — decode the
  `X-Model` header on the next request, assert against the model
  shape directly. Today: assert via the rendered DOM.
- **`Selector α` GADT** — typed selectors that the compiler can
  cross-check against your `view : Model → Html`. Catches "renamed
  the class, forgot the spec" at build time.
- **Per-`it` driver isolation** — opt-in flag on `runSpec` to
  open + close a tab around every assertion.
- **`screenshot` in-process base64 decode** — currently writes the
  raw base64, requires `base64 -d` post-processing.

`Selector α` is the long-term play: pair WebSpec's runtime
determinism with the same secure-by-construction story the rest of
the framework leans on (cf. [Chapter 11](11-secure-by-construction.md)).

---

## 9 · Try it yourself

```sh
# 1. App under test
./.lake/build/bin/counter_web --port 8001 &

# 2. Chrome
google-chrome --remote-debugging-port=9222 --user-data-dir=/tmp/cdp-profile &
sleep 2

# 3. Spec
./.lake/build/bin/webspec_smoke
```

Expected output:

```
WebSpec smoke — CDP http://127.0.0.1:9222, app http://127.0.0.1:8001
● counter_web
  ✓ initial render shows count = 0
  ✓ two `inc` clicks bump the counter to 2
  ✓ one `dec` after two `inc`s leaves the counter at 1
  ✓ `reset` zeros the counter

  4 passed, 0 failed
```

Source: [`examples/Smoke/WebSpec.lean`](../examples/Smoke/WebSpec.lean)
(~80 lines, all four assertions visible side by side).
