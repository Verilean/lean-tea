import LeanTea.Cdp
import LeanTea.Json.Helpers
import Lean.Data.Json

/-! # LeanTea.WebSpec — typed deterministic E2E tests

The complement to `examples/UiScript` (which uses an LLM to drive
exploratory flows): WebSpec is a deterministic CI-friendly E2E
harness shaped like LSpec. Each `it` is a `do`-block over CDP
primitives that returns `pure ()` on success or `throw msg` on
failure.

```lean
open LeanTea.WebSpec

def loginFlow : Spec := group "login flow" [
  it "the form rejects an empty password" do
    navigate "/login"
    fill "input[name=user]" "alice@example.com"
    fill "input[name=pass]" ""
    click "button[type=submit]"
    waitFor ".error"
    expectContains ".error" "password required"
  ,
  it "valid credentials redirect home" do
    navigate "/login"
    fill "input[name=user]" "alice@example.com"
    fill "input[name=pass]" "hunter2"
    click "button[type=submit]"
    waitFor "h1"
    expectUrlContains "/"
    expectContains "h1" "Welcome, alice"
]

def main : IO Unit := do
  let d ← Driver.connectFirstTab (base := "http://127.0.0.1:9222")
                                 (appBase := "http://127.0.0.1:8001")
  let code ← runSpec d loginFlow
  if code != 0 then IO.Process.exit code.toUInt8
```

## Prerequisites

* Chrome launched with `--remote-debugging-port=9222 --user-data-dir=…`.
* The app under test reachable at `appBase`.

The runner reuses one target across the whole spec by default. Spec
authors who need per-test isolation can use `Driver.fresh` inside
their `it` blocks to open a clean tab. -/

namespace LeanTea.WebSpec

open LeanTea.Cdp
open Lean (Json)

/-! ## The driver: one target, one base URL. -/

structure Driver where
  /-- e.g. `http://127.0.0.1:9222`. The CDP REST + WS host. -/
  base    : String
  /-- The WebSocket URL of the one tab this driver is bound to. -/
  wsUrl   : String
  /-- Optional app-under-test base, prefixed onto relative `navigate`
      arguments so specs can write `navigate "/login"` instead of
      the full URL. -/
  appBase : String := ""
  /-- The tab's CDP target id, kept so the runner can close it
      after the spec finishes (when the driver owns the tab). -/
  targetId : String := ""
  deriving Inhabited

namespace Driver

/-- Attach to the first existing tab (typically `about:blank` if
    Chrome was just launched). The driver does not own the tab; it
    will not close it on teardown. -/
def connectFirstTab (base : String) (appBase : String := "") : IO Driver := do
  let ts ← listTargets base
  match ts.find? (·.url.startsWith "http") with
  | some t => return { base, wsUrl := t.wsUrl, appBase, targetId := t.id }
  | none   =>
    match ts[0]? with
    | some t => return { base, wsUrl := t.wsUrl, appBase, targetId := t.id }
    | none   => throw <| IO.userError s!"WebSpec: no Chrome tab found at {base}/json"

/-- Open a brand-new tab pointed at `appBase` (or `about:blank` if
    not set). The caller is responsible for closing it via
    `Driver.close` when finished. -/
def openFresh (base : String) (appBase : String := "") : IO Driver := do
  let startUrl := if appBase.isEmpty then "about:blank" else appBase
  let t ← newTarget base startUrl
  return { base, wsUrl := t.wsUrl, appBase, targetId := t.id }

/-- Close the tab this driver owns. No-op if `targetId` is empty. -/
def close (d : Driver) : IO Unit := do
  if d.targetId.isEmpty then return ()
  closeTarget d.base d.targetId

end Driver

/-! ## The step monad — Reader (Driver) + Except String + IO. -/

abbrev StepM := ReaderT Driver (ExceptT String IO)

/-- Run a step against a driver. Returns `Except` so the runner can
    label per-step pass/fail without crashing the whole tree. -/
def runStep (d : Driver) (s : StepM Unit) : IO (Except String Unit) :=
  (s.run d).run

private def fail (msg : String) : StepM α := throw msg

/-! ## CDP wrappers. Each one returns `.error` from the `StepM`
    on a transport or assertion failure. -/

private def cdpCall (method : String) (params : Json := Json.mkObj []) : StepM Json := do
  let d ← read
  try
    let r ← cdpCommand d.wsUrl method params
    return r
  catch e =>
    fail s!"{method}: {toString e}"

/-- Trivial helper — string → JSON.str. -/
private def jsStr (s : String) : Json := Json.str s

/-- Encode a string as a JS string literal, escaping `\\`, `'`, and
    `\n`. Sufficient for selectors / typed text in evaluate(). -/
def jsLit (s : String) : String :=
  let esc := s.replace "\\" "\\\\" |>.replace "'" "\\'" |>.replace "\n" "\\n"
  "'" ++ esc ++ "'"

/-! ## Primitives. -/

/-- Navigate to `url`. Relative paths (`/foo`) are joined to the
    driver's `appBase`. Returns when the document `load` event
    fires (via Page.loadEventFired through Runtime.evaluate). -/
def navigate (url : String) : StepM Unit := do
  let d ← read
  let full := if url.startsWith "http" then url
              else d.appBase ++ url
  let _ ← cdpCall "Page.navigate" (Json.mkObj [("url", jsStr full)])
  /- Poll readyState in case Page.navigate returned before the page
     parsed. 60 × 50 ms = 3 s budget. -/
  let waitExpr :=
    "(async () => { for (let i = 0; i < 60; i++) { " ++
    "  if (document.readyState === 'complete') return true; " ++
    "  await new Promise(r => setTimeout(r, 50)); " ++
    "} return false; })()"
  let res ← cdpCall "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    jsStr waitExpr),
      ("returnByValue", Json.bool true),
      ("awaitPromise",  Json.bool true)])
  let inner := res.getJsonD "result" (Json.mkObj [])
  if !inner.getBoolD "value" then
    fail s!"navigate {full}: page did not reach readyState=complete within 3 s"

/-- Evaluate a JS expression and return its `value` field. Plain
    strings / numbers / booleans / objects all come through; the
    caller decides what to extract. -/
def evaluate (expr : String) : StepM Json := do
  let res ← cdpCall "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    jsStr expr),
      ("returnByValue", Json.bool true),
      ("awaitPromise",  Json.bool true)])
  let inner := res.getJsonD "result" (Json.mkObj [])
  return (inner.getObjVal? "value").toOption.getD Json.null

/-- Return the `textContent` of the first element matching `selector`.
    Fails if no element matches. -/
def getText (selector : String) : StepM String := do
  let expr := "(() => { const e = document.querySelector(" ++ jsLit selector ++
              "); return e ? e.textContent : null; })()"
  let v ← evaluate expr
  match v.getStr?.toOption with
  | some s => return s
  | none   => fail s!"getText {selector}: no element matched"

/-- Return the current document URL. -/
def currentUrl : StepM String := do
  let v ← evaluate "window.location.href"
  return v.getStr?.toOption.getD ""

/-- Wait for `selector` to appear (and be visible if `requireVisible`).
    Default timeout 3 s. Fails with `selector: timeout (Nms)` on miss. -/
def waitFor (selector : String) (timeoutMs : Nat := 3000)
    (requireVisible : Bool := true) : StepM Unit := do
  let vis := if requireVisible then "true" else "false"
  let expr :=
    "(async () => {\n" ++
    "  const sel = " ++ jsLit selector ++ ";\n" ++
    "  const timeout = " ++ toString timeoutMs ++ ";\n" ++
    "  const requireVisible = " ++ vis ++ ";\n" ++
    "  const start = Date.now();\n" ++
    "  const check = () => {\n" ++
    "    const el = document.querySelector(sel);\n" ++
    "    if (!el) return null;\n" ++
    "    if (!requireVisible) return el;\n" ++
    "    const r = el.getBoundingClientRect();\n" ++
    "    if (r.width > 0 && r.height > 0 && el.offsetParent !== null) return el;\n" ++
    "    return null;\n" ++
    "  };\n" ++
    "  if (check()) return JSON.stringify({found:true, ms: Date.now()-start});\n" ++
    "  return new Promise(resolve => {\n" ++
    "    const obs = new MutationObserver(() => {\n" ++
    "      if (check()) { obs.disconnect(); clearTimeout(t); resolve(JSON.stringify({found:true, ms: Date.now()-start})); }\n" ++
    "    });\n" ++
    "    obs.observe(document.body, {childList:true, subtree:true, attributes:true});\n" ++
    "    const t = setTimeout(() => { obs.disconnect(); resolve(JSON.stringify({found:false, ms: Date.now()-start})); }, timeout);\n" ++
    "  });\n" ++
    "})()"
  let v ← evaluate expr
  let raw := v.getStr?.toOption.getD "{}"
  match Json.parse raw with
  | .error e => fail s!"waitFor {selector}: bad JSON ({e})"
  | .ok j =>
    if j.getBoolD "found" then pure ()
    else fail s!"waitFor {selector}: timeout ({timeoutMs}ms)"

/-- Type `text` into the first element matching `selector`. Handles
    `<input>` / `<textarea>` (via the React-aware value setter +
    `input` event) and `contenteditable` regions (via `execCommand`).
    Fails if no element matches. -/
def fill (selector text : String) : StepM Unit := do
  let expr :=
    "(() => { const e = document.querySelector(" ++ jsLit selector ++
    "); if (!e) return false; e.focus(); " ++
    "const isCE = e.isContentEditable || e.getAttribute('contenteditable') === 'true'; " ++
    "const text = " ++ jsLit text ++ "; " ++
    "if (isCE) { document.execCommand('selectAll', false); " ++
    "  document.execCommand('insertText', false, text); " ++
    "  e.dispatchEvent(new Event('input', { bubbles: true })); return true; } " ++
    "else { " ++
    "  const setter = Object.getOwnPropertyDescriptor(" ++
    "Object.getPrototypeOf(e), 'value').set; setter.call(e, text); " ++
    "  e.dispatchEvent(new Event('input',  { bubbles: true })); " ++
    "  e.dispatchEvent(new Event('change', { bubbles: true })); return true; } })()"
  let v ← evaluate expr
  if !v.getBoolD "value" && !v.getBoolD "" then
    /- evaluate returned either a bool directly or wrapped in {value:bool};
       second branch handles the raw shape. -/
    pure ()
  match v with
  | .bool true => pure ()
  | .bool false => fail s!"fill {selector}: no element matched"
  | _ => pure ()    -- Unknown shape; the next assertion will surface the issue

/-- Click the first element matching `selector`. Fails if no
    element matches. Dispatches a real `MouseEvent` so React
    handlers fire. -/
def click (selector : String) : StepM Unit := do
  let expr :=
    "(() => { const e = document.querySelector(" ++ jsLit selector ++
    "); if (!e) return false; e.click(); return true; })()"
  let v ← evaluate expr
  match v with
  | .bool true  => pure ()
  | .bool false => fail s!"click {selector}: no element matched"
  | _ => pure ()

/-! ## Assertions. -/

/-- Assert that `getText selector == expected`. -/
def expectText (selector expected : String) : StepM Unit := do
  let actual ← getText selector
  if actual.trimAscii.toString == expected then pure ()
  else fail s!"expectText {selector}: expected `{expected}`, got `{actual.trimAscii.toString}`"

/-- Assert that the first matching element's text contains `substr`. -/
def expectContains (selector substr : String) : StepM Unit := do
  let actual ← getText selector
  if (actual.splitOn substr).length > 1 then pure ()
  else fail s!"expectContains {selector}: `{actual}` did not contain `{substr}`"

/-- Assert that the current `window.location.href` contains `substr`. -/
def expectUrlContains (substr : String) : StepM Unit := do
  let url ← currentUrl
  if (url.splitOn substr).length > 1 then pure ()
  else fail s!"expectUrlContains: `{url}` did not contain `{substr}`"

/-- Capture a PNG of the current viewport to `path`. -/
def screenshot (path : String) : StepM Unit := do
  let d ← read
  try
    let res ← cdpCommand d.wsUrl "Page.captureScreenshot"
      (Json.mkObj [("format", jsStr "png")])
    let data := res.getStrD "data"
    if data.isEmpty then fail "screenshot: empty data"
    /- base64 decode + write — use LeanTea.Crypto.Base64 to avoid a
       new dependency. Defer the import to keep WebSpec lean: the
       caller can decode the base64 string itself if needed. For the
       v0.2 ship we just write the base64 as-is and let the caller
       run `base64 -d`. -/
    IO.FS.writeFile path data
  catch e => fail s!"screenshot: {toString e}"

/-! ## Spec tree + runner. -/

inductive Spec where
  | it    (name : String) (step : Driver → IO (Except String Unit))
  | group (name : String) (children : List Spec)

def it (name : String) (step : StepM Unit) : Spec :=
  .it name (runStep · step)

def group (name : String) (children : List Spec) : Spec :=
  .group name children

structure Counts where
  passed : Nat := 0
  failed : Nat := 0
  deriving Inhabited

private partial def go (d : Driver) (depth : Nat) (s : Spec)
    : StateT Counts IO Unit := do
  let indent := String.mk (List.replicate (depth * 2) ' ')
  match s with
  | .it name step => do
    let res ← step d
    match res with
    | .ok () =>
      IO.println s!"{indent}✓ {name}"
      modify fun c => { c with passed := c.passed + 1 }
    | .error e =>
      IO.println s!"{indent}✗ {name}"
      IO.println s!"{indent}  → {e}"
      modify fun c => { c with failed := c.failed + 1 }
  | .group name children =>
    IO.println s!"{indent}● {name}"
    for c in children do go d (depth + 1) c

/-- Run a spec tree against `d`. Prints LSpec-style results and
    returns 0 on full success, 1 otherwise. -/
def runSpec (d : Driver) (s : Spec) : IO UInt32 := do
  let ((), counts) ← (go d 0 s).run {}
  IO.println ""
  IO.println s!"  {counts.passed} passed, {counts.failed} failed"
  return if counts.failed == 0 then 0 else 1

end LeanTea.WebSpec
