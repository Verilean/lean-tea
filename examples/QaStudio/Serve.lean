import LeanTea

/-! # qa_studio — a chat window that turns plain-language checks into UI tests

```
$ lake exe qa_studio                         # http://localhost:8020/ , tests the built-in demo app
$ lake exe qa_studio --url http://127.0.0.1:8005/ [--model qwen/qwen3-vl-4b] [--headed]
      [--port 8020] [--out examples/VisionQa/scripts] [--base-url http://127.0.0.1:11211/v1]
      [--profile DIR] [--clip auto|SELECTOR|x,y,w,h]
```

For a site behind a login (a browser game), keep a browser profile and
log in once in a visible window:

```
$ lake exe qa_studio --url https://example.com/game --profile ~/.cache/qa-profile --headed --clip auto
  … log in in the Chromium window, then chat. Later runs reuse the login
    (headless is fine: --profile ~/.cache/qa-profile --clip auto).
```

`--clip auto` crops every screenshot to the largest iframe / canvas on
the page (the game) and offsets clicks into it, so the model never
sees the surrounding site. Pages that animate by themselves are
handled: actions wait for the screen to settle (bounded), the model can
`wait`, and a click is judged by the area around it.

One screen: the page under test on the left (a live screenshot of a
headless browser, with the last click marked), a chat on the right.
Describe what to check; the local VLM explores by trial and error
(`LeanTea.Vision.Explore`) — each step it proposes runs for real, the
ones that work are recorded, failures go back to the model. Then:

  * **Replay** reloads the start page and runs the recorded steps as a
    plain script (`LeanTea.Vision.Qa.runScript`, no planning) — proof
    the test is deterministic;
  * **Save test** writes it as a `vision_qa` script and shows the
    command that runs it (CI, a terminal, or `ui_report`).

Further messages continue from where the last goal left off, so a
test can be built up goal by goal. `/demo` serves a small sign-in +
to-do app to try it on; `/demo-game` a stand-in browser game (login
gate, game canvas in an iframe, slow loading, animated background) to
try `--profile` / `--clip auto` on. -/

namespace QaStudio

open Lean (Json)
open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanTea.Agent.Script
open LeanTea.Vision LeanTea.Vision.Qa

structure Args where
  port    : UInt16 := 8020
  url?    : Option String := none
  model   : String := "qwen/qwen3-vl-4b"
  baseUrl : String := "http://127.0.0.1:11211/v1"
  headed  : Bool := false
  outDir  : String := "examples/VisionQa/scripts"
  dir     : String := "/tmp/qa-studio"
  profile? : Option String := none
  clip?   : Option String := none

partial def parseArgs : List String → Args → Args
  | "--port" :: v :: r, a     => parseArgs r { a with port := (v.toNat?.getD 8020).toUInt16 }
  | "--url" :: v :: r, a      => parseArgs r { a with url? := some v }
  | "--model" :: v :: r, a    => parseArgs r { a with model := v }
  | "--base-url" :: v :: r, a => parseArgs r { a with baseUrl := v }
  | "--headed" :: r, a        => parseArgs r { a with headed := true }
  | "--out" :: v :: r, a      => parseArgs r { a with outDir := v }
  | "--dir" :: v :: r, a      => parseArgs r { a with dir := v }
  | "--profile" :: v :: r, a  => parseArgs r { a with profile? := some v }
  | "--clip" :: v :: r, a     => parseArgs r { a with clip? := some v }
  | _ :: r, a                 => parseArgs r a
  | [], a                     => a

/-! ## Session state -/

structure State where
  events   : Array Json := #[]
  busy     : Bool := false
  steps    : List Step := []
  startUrl : String := ""
  goals    : List String := []
  verified : Option Bool := none
  saved?   : Option (String × String) := none
  lastShot : Option String := none

structure Studio where
  args    : Args
  session : LeanTea.Browser.Session
  drv     : Driver
  vlm     : Vlm
  dir     : System.FilePath
  state   : IO.Ref State
  stopReq : IO.Ref Bool
  shotN   : IO.Ref Nat

def Studio.ctx (s : Studio) (steps : List Step) : IO Ctx := do
  return { drv := s.drv, vlm := s.vlm, zoom := 3, dir := s.dir,
           script := { name := "studio", steps }, pending := ← IO.mkRef none,
           lastPoint := ← IO.mkRef none }

/-- Keep a copy of an evidence screenshot under a unique name (later
    attempts reuse step numbers and would overwrite it); its URL. -/
def Studio.keepShot (s : Studio) (path? : Option String) : IO (Option String) := do
  let some path := path? | return none
  if !(← System.FilePath.pathExists path) then return none
  let n ← s.shotN.modifyGet fun n => (n, n + 1)
  let name := s!"shot{n}.png"
  IO.FS.writeBinFile (s.dir / name) (← IO.FS.readBinFile path)
  s.state.modify fun st => { st with lastShot := some (s.dir / name).toString }
  return some s!"/shot/{name}"

def Studio.push (s : Studio) (fields : List (String × Json)) : IO Unit :=
  s.state.modify fun st =>
    { st with events := st.events.push (Json.mkObj (("id", .num (Int.ofNat st.events.size)) :: fields)) }

def pointJson : Option (Nat × Nat) → Json
  | some (x, y) => .arr #[.num (Int.ofNat x), .num (Int.ofNat y)]
  | none => .null

def Studio.emit (s : Studio) : Explore.Event → IO Unit
  | .thinking t => s.push [("kind", "thinking"), ("text", .str t)]
  | .proposal t why => s.push [("kind", "proposal"), ("text", .str t), ("why", .str why)]
  | .stepOk t obs shot? pt => do
    let url ← s.keepShot shot?
    s.push [("kind", "stepOk"), ("text", .str t), ("observed", .str obs),
            ("shot", (url.map Json.str).getD .null), ("point", pointJson pt)]
  | .stepFailed t err shot? pt => do
    let url ← s.keepShot shot?
    s.push [("kind", "stepFailed"), ("text", .str t), ("error", .str err),
            ("shot", (url.map Json.str).getD .null), ("point", pointJson pt)]
  | .note t => s.push [("kind", "note"), ("text", .str t)]
  | .finished ok t => s.push [("kind", "finished"), ("ok", .bool ok), ("text", .str t)]

def Studio.script (s : Studio) (st : State) (name : String) : Script :=
  { name, steps := st.steps,
    description := s!"Recorded with qa_studio on {st.startUrl} (model {s.vlm.model}). Goals: " ++
      String.intercalate " / " st.goals.reverse }

/-- Claim the browser for a background job; false if one is running. -/
def Studio.claim (s : Studio) : IO Bool :=
  s.state.modifyGet fun st => if st.busy then (false, st) else (true, { st with busy := true })

def Studio.release (s : Studio) : IO Unit := s.state.modify ({ · with busy := false })

/-- Run `job` on its own thread with the browser claimed. -/
def Studio.background (s : Studio) (job : IO Unit) : IO Bool := do
  if !(← s.claim) then return false
  s.stopReq.set false
  let _ ← IO.asTask (prio := .dedicated) do
    try job catch e => s.push [("kind", "finished"), ("ok", .bool false), ("text", .str s!"error: {e}")]
    s.release
  return true

def Studio.goto (s : Studio) (url : String) : IO Unit := do
  let _ ← s.session.navigate url
  IO.sleep 400

/-! ## Jobs -/

/-- Visible inputs, buttons, links, headings and short messages, one per
    line — the DOM's help for a small model describing targets. The
    recorded steps stay visual (`click_described`). -/
def outlineJs : String := "(() => {
  const out = [];
  const vis = (e) => { const r = e.getBoundingClientRect(); const s = getComputedStyle(e);
    return r.width > 0 && r.height > 0 && s.visibility !== 'hidden' && s.display !== 'none'; };
  const label = (e) => { if (e.id) { const l = document.querySelector('label[for=\"' + e.id + '\"]'); if (l) return l.innerText.trim(); }
    return (e.getAttribute('aria-label') || e.placeholder || e.name || '').trim(); };
  for (const e of document.querySelectorAll('h1,h2,h3,input,textarea,select,button,a,[role=button],.error,[role=alert],li,p')) {
    if (!vis(e)) continue;
    const t = e.tagName.toLowerCase();
    if (t === 'input' || t === 'textarea' || t === 'select') {
      const kind = e.type === 'checkbox' ? 'checkbox' + (e.checked ? ' (checked)' : '') : (e.type === 'password' ? 'password field' : 'text field');
      out.push(kind + ' \"' + label(e) + '\"' + (e.type !== 'password' && e.value ? ' containing \"' + e.value + '\"' : '') + (document.activeElement === e ? ' [focused]' : ''));
    } else {
      const txt = (e.innerText || '').trim().replace(/\\s+/g, ' ').slice(0, 80);
      if (!txt) continue;
      out.push((t === 'button' || e.getAttribute('role') === 'button' ? 'button' : t === 'a' ? 'link' : t === 'li' ? 'list item' : /^h[123]$/.test(t) ? 'heading' : 'text') + ' \"' + txt + '\"');
    }
  }
  return out.slice(0, 40).join('\\n');
})()"

def Studio.outline (s : Studio) : IO String := do
  -- with a clip the model sees only the clipped region (a game's canvas,
  -- usually a cross-origin iframe): the page's DOM would only mislead
  if s.args.clip?.isSome then return ""
  match ← s.session.evaluate outlineJs with
  | .str t => return t
  | _ => return ""

def Studio.explore (s : Studio) (goal : String) : IO Unit := do
  let st ← s.state.get
  let ctx ← s.ctx st.steps
  let notes := st.goals.reverse.map (s!"Earlier goal, already done: " ++ ·)
  let (steps, _) ← Explore.explore ctx {} goal notes st.steps s.emit (s.stopReq.get) s.outline
  -- a fresh test opens the page and must not act before it's ready (a
  -- game takes a while to load): gate it on its first click target
  let steps := match st.steps, steps with
    | [], first :: rest =>
      match first.act with
      | .clickDescribed t _ =>
        { act := .waitUntil s!"Is {t} visible on the screen?", note := "wait for the start screen" } :: first :: rest
      | _ => steps
    | _, _ => steps
  s.state.modify fun st => { st with steps, verified := none, goals := goal :: st.goals }

def Studio.replay (s : Studio) : IO Unit := do
  let st ← s.state.get
  s.push [("kind", "note"), ("text", .str s!"replaying {st.steps.length} steps from {st.startUrl}…")]
  s.goto st.startUrl
  let ctx ← s.ctx st.steps
  let res ← runScript ctx fun _ r => do
    let url ← s.keepShot r.evidencePath
    s.push [("kind", if r.ok then "stepOk" else "stepFailed"),
            ("text", .str (Explore.describe r.step.act)), ("observed", .str (r.observed.getD "")),
            ("error", .str (r.error.getD "")), ("shot", (url.map Json.str).getD .null),
            ("point", .null)]
  s.state.modify ({ · with verified := some res.passed })
  s.push [("kind", "finished"), ("ok", .bool res.passed),
          ("text", .str (if res.passed then s!"replay passed ({res.steps.size} steps, {res.totalMs / 1000}s)"
                         else "replay failed — see the red step"))]

/-! ## HTTP -/

def jsonBody (req : Request) : Json :=
  (Json.parse (String.fromUTF8! req.body)).toOption.getD (Json.mkObj [])

def str (j : Json) (k : String) : String := (j.getObjValD k).getStr?.toOption.getD ""

def ok (fields : List (String × Json) := []) : Response := Response.json 200 (Json.mkObj (("ok", .bool true) :: fields))
def err (status : Nat) (msg : String) : Response := Response.json status (Json.mkObj [("error", .str msg)])
def busyErr : Response := err 409 "busy — wait for the current run or press Stop"

def queryNat (q : String) (k : String) : Nat :=
  (q.splitOn "&").findSome? (fun kv =>
    match kv.splitOn "=" with
    | [a, b] => if a == k then b.toNat? else none
    | _ => none) |>.getD 0

def png (bytes : ByteArray) : Response :=
  { status := 200, headers := #[("content-type", "image/png"), ("cache-control", "no-store")], body := bytes }

def sanitize (name : String) : String :=
  let n := String.ofList (name.toList.filter fun c => c.isAlphanum || c == '.' || c == '_' || c == '-')
  if n.isEmpty then "qa_studio.test" else n

def handler (s : Studio) (page : Template.Provider) (demos : List (String × String)) : Handler := fun req => do
  match req.method, req.path with
  | "GET", "/" =>
    let st ← s.state.get
    let quoteAttr := fun (v : String) => v.replace "&" "&amp;" |>.replace "\"" "&quot;" |>.replace "<" "&lt;"
    return Response.html 200 (← (← page).renderFlat [("model", quoteAttr s.vlm.model), ("url", quoteAttr st.startUrl)])
  | "GET", "/live.png" =>
    let st ← s.state.get
    if !st.busy && (← s.claim) then
      try
        let p := s.dir / "live.png"
        s.drv.screenshot p
        return png (← IO.FS.readBinFile p)
      finally s.release
    match st.lastShot with
    | some p => return png (← IO.FS.readBinFile p)
    | none => return err 503 "no screenshot yet"
  | "GET", path =>
    if let some (_, html) := demos.find? (·.1 == path) then return Response.html 200 html
    if path.startsWith "/shot/" then
      let name := (path.drop 6).toString
      if name.any (· == '/') || name.startsWith "." then return Response.notFound
      let p := s.dir / name
      if ← p.pathExists then return png (← IO.FS.readBinFile p) else return Response.notFound
    else if path == "/api/state" then
      let st ← s.state.get
      let since := queryNat req.query "since"
      return Response.json 200 (Json.mkObj [
        ("events", .arr (st.events.extract since st.events.size)),
        ("busy", .bool st.busy),
        ("url", .str st.startUrl),
        ("steps", .arr (st.steps.toArray.map fun step => .str (Explore.describe step.act))),
        ("script", (s.script st "qa_studio.draft").toJson),
        ("verified", match st.verified with | some b => .bool b | none => .null),
        ("saved", match st.saved? with
          | some (p, c) => Json.mkObj [("path", .str p), ("command", .str c)]
          | none => .null)])
    else return Response.notFound
  | "POST", "/api/chat" =>
    let text := (str (jsonBody req) "text").trimAscii.toString
    if text.isEmpty then return err 400 "empty message"
    s.push [("kind", "user"), ("text", .str text)]
    if ← s.background (s.explore text) then return ok
    else return busyErr
  | "POST", "/api/stop" => s.stopReq.set true; return ok
  | "POST", "/api/navigate" =>
    let url := str (jsonBody req) "url"
    if url.isEmpty then return err 400 "no url"
    let started ← s.background do
      s.goto url
      s.state.modify fun st => { st with startUrl := url, steps := [], goals := [], verified := none, saved? := none }
      s.push [("kind", "note"), ("text", .str s!"opened {url} — recorded steps cleared")]
    return if started then ok else busyErr
  | "POST", "/api/reset" =>
    let started ← s.background do
      let st ← s.state.get
      s.goto st.startUrl
      s.state.modify fun st => { st with steps := [], goals := [], verified := none, saved? := none }
      s.push [("kind", "note"), ("text", .str "reset: start page reloaded, recorded steps cleared")]
    return if started then ok else busyErr
  | "POST", "/api/undo" =>
    s.state.modify fun st => { st with steps := st.steps.dropLast, verified := none }
    return ok
  | "POST", "/api/replay" =>
    if (← s.state.get).steps.isEmpty then return err 400 "nothing recorded yet"
    return if ← s.background s.replay then ok else busyErr
  | "POST", "/api/save" =>
    let st ← s.state.get
    if st.steps.isEmpty then return err 400 "nothing recorded yet"
    let name := sanitize (str (jsonBody req) "name")
    let dir : System.FilePath := s.args.outDir
    IO.FS.createDirAll dir
    let path := dir / s!"{name}.json"
    IO.FS.writeFile path ((s.script st name).toJson.pretty 100 ++ "\n")
    let extra := (s.args.profile?.map (s!" --profile {·}")).getD "" ++ (s.args.clip?.map (s!" --clip {·}")).getD ""
    let cmd := s!"./.lake/build/bin/vision_qa {path} --target browser --url {st.startUrl} --model {s.vlm.model}{extra}"
    s.state.modify ({ · with saved? := some (path.toString, cmd) })
    s.push [("kind", "note"), ("text", .str s!"saved {path}")]
    return ok [("path", .str path.toString), ("command", .str cmd)]
  | _, _ => return Response.notFound

private def findFile (rel : String) : IO String := do
  for p in [s!"examples/QaStudio/{rel}", s!"../examples/QaStudio/{rel}", s!"lean-tea/examples/QaStudio/{rel}"] do
    if ← System.FilePath.pathExists p then return p
  throw <| IO.userError s!"couldn't find examples/QaStudio/{rel} (run from the repo root)"

def main (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  let dir : System.FilePath := a.dir
  IO.FS.createDirAll dir
  let page ← Template.mkProvider (← findFile "page.html") false
  let demos ← [("/demo", "demo.html"), ("/demo-game", "game.html"), ("/demo-game/frame", "game-frame.html")].mapM
    fun (route, file) => do return (route, ← IO.FS.readFile (← findFile file))
  let session ← LeanTea.Browser.Session.spawn
  let _ ← session.open 1280 800 (headless := some !a.headed) (profile := a.profile?)
  let vlm : Vlm := { cfg := { baseUrl := a.baseUrl, timeoutSec := some 300 }, model := a.model,
                     frame := CoordFrame.guess a.model }
  let startUrl := a.url?.getD s!"http://127.0.0.1:{a.port}/demo"
  let drv ← match a.clip? with
    | some spec => (Driver.browser session).clipTo (browserRegion session spec)
    | none => pure (Driver.browser session)
  let s : Studio := { args := a, session, drv, vlm, dir,
                      state := ← IO.mkRef { startUrl }, stopReq := ← IO.mkRef false, shotN := ← IO.mkRef 0 }
  -- open the start page once the server below is answering (it may be /demo)
  let _ ← IO.asTask (prio := .dedicated) do
    IO.sleep 700
    let _ ← s.background (s.goto startUrl)
  IO.println s!"qa_studio: http://127.0.0.1:{a.port}/   testing {startUrl} with {a.model}"
  if a.headed && a.profile?.isSome then
    IO.println "  log in in the Chromium window if the site asks — the profile keeps it for next time"
  -- concurrent: the headless browser keeps a keep-alive connection open
  -- (to /demo), which would block a one-connection-at-a-time server
  serveConcurrent a.port "127.0.0.1" (handler s page demos)

end QaStudio

def main (argv : List String) : IO Unit := QaStudio.main argv
