import LeanTea
import LeanTea.WebSpec

/-! # webspec_smoke — drive `counter_web` via the WebSpec runner

End-to-end demonstration of `LeanTea.WebSpec`. This is **not** in CI
by default — like `browser_vision_smoke` it needs a live Chrome
running with remote debugging enabled.

## Prerequisites

```
# Terminal 1 — the LeanTEA app under test
./.lake/build/bin/counter_web --port 8001

# Terminal 2 — Chrome with CDP enabled (separate profile required)
google-chrome --remote-debugging-port=9222 --user-data-dir=/tmp/cdp-profile
# (or `chromium` / `open -a "Google Chrome" --args …` on macOS)

# Terminal 3 — the smoke
./.lake/build/bin/webspec_smoke
```

Override via env: `CHROME_CDP_URL` (default `http://127.0.0.1:9222`)
and `APP_BASE_URL` (default `http://127.0.0.1:8001`).

## What the spec checks

The counter app's frontend mutates an `Int` via three buttons
(`inc` / `dec` / `reset`). The spec asserts the round-trip:

  1. Load the page → counter shows `count = 0`.
  2. Click `inc` twice → counter shows `count = 2`.
  3. Click `dec` once → counter shows `count = 1`.
  4. Click `reset`    → counter shows `count = 0`. -/

open LeanTea.WebSpec

/-- One spec covering the four basic transitions of the counter app. -/
def counterSpec : Spec := group "counter_web" [
  it "initial render shows count = 0" do
    navigate "/"
    waitFor ".card p"
    expectContains ".card p" "count = 0"
  ,
  it "two `inc` clicks bump the counter to 2" do
    navigate "/"
    waitFor "a[data-msg=inc]"
    click "a[data-msg=inc]"
    click "a[data-msg=inc]"
    /- The runtime mutates the DOM in place after each request.
       The element selector remains the same; the body text changes. -/
    expectContains ".card p" "count = 2"
  ,
  it "one `dec` after two `inc`s leaves the counter at 1" do
    navigate "/"
    waitFor "a[data-msg=inc]"
    click "a[data-msg=inc]"
    click "a[data-msg=inc]"
    click "a[data-msg=dec]"
    expectContains ".card p" "count = 1"
  ,
  it "`reset` zeros the counter" do
    navigate "/"
    waitFor "a[data-msg=inc]"
    click "a[data-msg=inc]"
    click "a[data-msg=reset]"
    expectContains ".card p" "count = 0"
]

def main : IO Unit := do
  let cdpBase := (← IO.getEnv "CHROME_CDP_URL").getD "http://127.0.0.1:9222"
  let appBase := (← IO.getEnv "APP_BASE_URL").getD "http://127.0.0.1:8001"
  IO.println s!"WebSpec smoke — CDP {cdpBase}, app {appBase}"
  /- Open a clean tab so we don't trample whatever the operator was
     looking at. The driver owns it and closes on teardown. -/
  let d ← Driver.openFresh cdpBase appBase
  try
    let code ← runSpec d counterSpec
    d.close
    if code != 0 then IO.Process.exit code.toUInt8
  catch e =>
    /- Even on a runner crash, return the tab. -/
    try d.close catch _ => pure ()
    IO.println s!"webspec_smoke: {e}"
    IO.Process.exit 1
