import Lean.Data.Json

/-! # LeanTea.Browser — drive a real browser from Lean via Playwright

Spawns `tools/browser-bridge/bridge.js` as a child process and talks
to it with one JSON object per line. The bridge handles Playwright /
Chromium; we stay pure Lean. Useful for:

* End-to-end tests of the example apps (`canvas_serve`)
* Vision-LLM driven UI exploration (open page → screenshot →
  send to `LeanTea.Llm.Openai` → use the model's plan to click)
* Wrapping browser actions as MCP tools

Each `Session` owns one browser + one page. Spawn more sessions if you
need parallel pages.

```lean
open LeanTea.Browser

do
  let s ← Session.spawn
  let r ← s.navigate "http://127.0.0.1:8001/"
  let shot ← s.screenshot
  let dataUrl := s.dataUrl shot
  -- shot.base64 ready to feed into a vision model
  s.close
```
-/

namespace LeanTea.Browser

open Lean (Json)

/-! ## Bridge process handle -/

/-- A live browser session — really a wrapped child process and a
    request-id counter. -/
structure Session where
  child : IO.Process.Child { stdin := .piped, stdout := .piped, stderr := .piped }
  nextId : IO.Ref Nat

/-! ## Path resolution

The bridge script ships in `tools/browser-bridge/`. Mirror the same
candidates pattern other examples use so the binary works from either
`lean-elm/` or one level up. -/

private def candidates : List String := [
  "tools/browser-bridge/bridge.js",
  "../tools/browser-bridge/bridge.js",
  "../../tools/browser-bridge/bridge.js",
  "lean-elm/tools/browser-bridge/bridge.js"
]

/-- Find the Node bridge script. Resolution order:
    1. `$LEANTEA_BROWSER_BRIDGE` env var (set this when the binary is
       spawned from somewhere other than the project root — Claude
       Code / Claude Desktop / Cursor configs do this).
    2. Several candidate paths relative to cwd (works for local
       development).
    3. Sibling to the running binary: `<argv0>/../../tools/browser-bridge/bridge.js`. -/
private def resolveBridge : IO String := do
  if let some p ← IO.getEnv "LEANTEA_BROWSER_BRIDGE" then
    if ← System.FilePath.pathExists p then return p
  for path in candidates do
    if ← System.FilePath.pathExists path then
      return path
  /- Last-ditch: walk up from argv[0] (the binary location) and look
     under the parent's `tools/`. The Lake-built binary lives at
     `<repo>/.lake/build/bin/<exe>`, so the bridge is three levels up
     plus `tools/browser-bridge/bridge.js`. -/
  let argv0 := (← IO.appPath).toString
  let exeDir : System.FilePath := (System.FilePath.mk argv0).parent.getD "."
  let guesses : List System.FilePath := [
    exeDir / ".." / ".." / ".." / "tools" / "browser-bridge" / "bridge.js",
    exeDir / ".." / "tools" / "browser-bridge" / "bridge.js"
  ]
  for g in guesses do
    let s := g.toString
    if ← System.FilePath.pathExists s then return s
  throw <| IO.userError <|
    "couldn't locate tools/browser-bridge/bridge.js. " ++
    "Set LEANTEA_BROWSER_BRIDGE=/absolute/path/to/bridge.js " ++
    "(or run the binary from the project root). Tried: " ++
    String.intercalate ", " (candidates ++ guesses.map (·.toString))

/-! ## Session lifecycle -/

/-- Spawn the bridge. Blocks until it prints its `ready` marker so the
    first request after spawn always lands in a live page. -/
def Session.spawn : IO Session := do
  let bridge ← resolveBridge
  let child ← IO.Process.spawn {
    cmd := "node", args := #[bridge],
    stdin := .piped, stdout := .piped, stderr := .piped
  }
  /- Read the `ready` line. If the bridge fails to launch we surface
     whatever it wrote to stderr. -/
  let ready ← child.stdout.getLine
  if ready.isEmpty then
    let err ← child.stderr.readToEnd
    throw <| IO.userError s!"browser-bridge: failed to start\n{err}"
  let nextId ← IO.mkRef 1
  return { child, nextId }

/-- Politely close the session. Sends a `close` request, drops the
    stdin pipe, and waits for the child to exit. -/
def Session.close (s : Session) : IO Unit := do
  /- Best-effort `close` request; ignore failure since the process
     may already be on its way out. -/
  try
    let closeMsg := Lean.Json.mkObj [
      ("id",     Lean.Json.num 0),
      ("method", Lean.Json.str "close"),
      ("params", Lean.Json.mkObj [])
    ]
    s.child.stdin.putStr (closeMsg.compress ++ "\n")
    s.child.stdin.flush
  catch _ => pure ()
  /- Force EOF on the bridge's stdin. -/
  let (_, child) ← s.child.takeStdin
  let _ ← child.wait
  return ()

/-! ## JSON-RPC helpers -/

private def sendRequest (s : Session) (method : String) (params : Json)
    : IO Json := do
  let id ← s.nextId.get
  s.nextId.set (id + 1)
  let req := Json.mkObj [
    ("id",     Json.num (Int.ofNat id)),
    ("method", Json.str method),
    ("params", params)
  ]
  s.child.stdin.putStr (req.compress ++ "\n")
  s.child.stdin.flush
  /- Read one line — the bridge always echoes exactly one response per
     request (modulo the initial `ready` marker, which `spawn` already
     consumed). -/
  let line ← s.child.stdout.getLine
  if line.isEmpty then
    throw <| IO.userError "browser-bridge: bridge closed mid-request"
  match Json.parse line with
  | .error e => throw <| IO.userError s!"browser-bridge: bad JSON: {e}\n{line}"
  | .ok j    =>
    match j.getObjVal? "error" with
    | .ok msgJson =>
      let msg := msgJson.getStr?.toOption.getD "(unknown)"
      throw <| IO.userError s!"browser-bridge: {msg}"
    | .error _    =>
      match j.getObjVal? "result" with
      | .ok r    => return r
      | .error _ => return Json.null

/-! ## High-level methods

Every method here is a thin shim that hands the parameters to the
bridge and pulls the right fields off the response. We keep return
types narrow (strings / records) where useful and expose the raw
`Json` for everything else. -/

structure NavResult where
  url   : String
  title : String
  deriving Inhabited, Repr

private def getStr (j : Json) (key : String) : String :=
  (j.getObjVal? key).toOption.bind (·.getStr?.toOption) |>.getD ""

private def getNat (j : Json) (key : String) : Nat :=
  match (j.getObjVal? key).toOption with
  | none => 0
  | some v =>
    match v with
    | .num n => n.mantissa.toNat
    | _      => 0

/-- Open / re-open the browser. Returns the viewport dimensions plus
    the active headless flag (mirrors the bridge's persisted default
    after this call). `headless := none` keeps whatever the bridge
    currently has set; `some true` / `some false` flips it. -/
def Session.open (s : Session) (width height : Nat := 0)
    (headless : Option Bool := none) : IO (Nat × Nat × Bool) := do
  let mut p : List (String × Json) := []
  if width  > 0 then p := p ++ [("width",  Json.num (Int.ofNat width))]
  if height > 0 then p := p ++ [("height", Json.num (Int.ofNat height))]
  if let some h := headless then p := p ++ [("headless", Json.bool h)]
  let r ← sendRequest s "open" (Json.mkObj p)
  let h := (r.getObjVal? "headless").toOption.bind (fun j =>
    match j with | .bool b => some b | _ => none) |>.getD true
  return (getNat r "width", getNat r "height", h)

def Session.navigate (s : Session) (url : String) : IO NavResult := do
  let r ← sendRequest s "navigate" (Json.mkObj [("url", Json.str url)])
  return { url := getStr r "url", title := getStr r "title" }

def Session.click (s : Session) (selector : String) : IO Unit := do
  let _ ← sendRequest s "click" (Json.mkObj [("selector", Json.str selector)])
  return ()

/-- Click at viewport-pixel coordinates. Use this for canvas-rendered
    UIs (Pixi, Cocos, Three.js, WebGL games) where CSS selectors
    don't reach anything — pair with a screenshot, read off the
    button's location, click there. -/
def Session.clickXy (s : Session) (x y : Nat) : IO Unit := do
  let _ ← sendRequest s "clickXy" (Json.mkObj [
    ("x", Json.num (Int.ofNat x)),
    ("y", Json.num (Int.ofNat y))
  ])
  return ()

def Session.fill (s : Session) (selector text : String) : IO Unit := do
  let _ ← sendRequest s "fill" (Json.mkObj [
    ("selector", Json.str selector),
    ("text",     Json.str text)
  ])
  return ()

def Session.press (s : Session) (key : String) (selector : Option String := none)
    : IO Unit := do
  let base : List (String × Json) := [("key", Json.str key)]
  let p := match selector with
    | some sel => base ++ [("selector", Json.str sel)]
    | none     => base
  let _ ← sendRequest s "press" (Json.mkObj p)
  return ()

def Session.waitFor (s : Session) (selector : String)
    (state : String := "visible") (timeoutMs : Nat := 5000) : IO Unit := do
  let _ ← sendRequest s "waitFor" (Json.mkObj [
    ("selector", Json.str selector),
    ("state",    Json.str state),
    ("timeout",  Json.num (Int.ofNat timeoutMs))
  ])
  return ()

def Session.getText (s : Session) (selector : String := "body") : IO String := do
  let r ← sendRequest s "getText" (Json.mkObj [("selector", Json.str selector)])
  return getStr r "text"

def Session.getHtml (s : Session) (selector : String := "body") : IO String := do
  let r ← sendRequest s "getHtml" (Json.mkObj [("selector", Json.str selector)])
  return getStr r "html"

def Session.evaluate (s : Session) (expression : String) : IO Json := do
  let r ← sendRequest s "evaluate" (Json.mkObj [("expression", Json.str expression)])
  return (r.getObjVal? "result").toOption.getD Json.null

/-! ## Screenshot — the moneymaker

Returns a base64 PNG (or JPEG) ready to feed into
`LeanTea.Llm.Openai.userTextAndImage`. The `dataUrl` field is the
already-decorated `data:` URL so callers don't have to think about
the MIME prefix. -/

structure Screenshot where
  base64     : String
  mime       : String
  bytes      : Nat
  width      : Nat
  height     : Nat
  /-- If the caller passed `outputPath` to `screenshot`, the bridge
      also wrote the raw image bytes to that path. Empty string when
      no path was requested. -/
  outputPath : String := ""
  deriving Inhabited, Repr

def Screenshot.dataUrl (s : Screenshot) : String :=
  s!"data:{s.mime};base64,{s.base64}"

/-- Take a screenshot — full viewport by default, or scoped to a
    selector if given. Pass `fullPage := true` to capture beyond the
    viewport (useful for long pages). When `outputPath` is supplied,
    the bridge also writes the raw bytes there so callers can hand
    the file to other tools (e.g. `curl --data-binary @path`) without
    going through base64 decode. -/
def Session.screenshot (s : Session) (selector : Option String := none)
    (fullPage : Bool := false) (outputPath : Option String := none)
    : IO Screenshot := do
  let mut p : List (String × Json) := [("fullPage", Json.bool fullPage)]
  if let some sel := selector then
    p := p ++ [("selector", Json.str sel)]
  if let some out := outputPath then
    p := p ++ [("outputPath", Json.str out)]
  let r ← sendRequest s "screenshot" (Json.mkObj p)
  return {
    base64     := getStr r "base64",
    mime       := getStr r "mime",
    bytes      := getNat r "bytes",
    width      := getNat r "width",
    height     := getNat r "height",
    outputPath := getStr r "outputPath"
  }

/-- `with`-style helper: spawn, run a block, close even on exception. -/
def withSession {α : Type} (k : Session → IO α) : IO α := do
  let s ← Session.spawn
  try
    let v ← k s
    s.close
    return v
  catch e =>
    s.close
    throw e

end LeanTea.Browser
