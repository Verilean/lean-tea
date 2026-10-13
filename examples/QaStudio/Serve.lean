import LeanTea

/-! # qa_studio — a chat window that turns plain-language checks into UI tests

```
$ lake exe qa_studio                         # http://localhost:8020/ , tests the built-in demo app
$ lake exe qa_studio --url http://127.0.0.1:8005/ [--model qwen/qwen3-vl-4b] [--headed]
      [--port 8020] [--out examples/VisionQa/scripts] [--base-url http://127.0.0.1:11211/v1]
      [--profile DIR] [--clip auto|SELECTOR|x,y,w,h]
      [--browser-port 9333] [--dev] [--rules FILE]
```

While working on the studio itself, `tools/qa-studio-dev.sh` rebuilds
and restarts it whenever its sources change. `--browser-port` keeps the
browser in its own process (the game stays on the same screen, still
logged in), the recorded steps and chat are restored from
`--dir`/state.json, `--dev` re-reads page.html on every load and
`--rules FILE` (one piece of advice per line) is re-read every turn.

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

**Read screen** has the model list the buttons and labels on the
screen (`LeanTea.Vision.Elements`); the user picks one to click or to
check, and the pick is recorded — the model reads, the user decides.

Further messages continue from where the last goal left off, so a
test can be built up goal by goal. For a broad instruction, **Plan**
asks the model for sub-goals (with `repeat until` loops) that you edit
and then run (`LeanTea.Vision.Plan`); the recorded steps are grouped by
sub-goal and can be deleted, replayed from any step, or edited as JSON. `/demo` serves a small sign-in +
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
  browserPort? : Option Nat := none
  dev     : Bool := false
  rules?  : Option String := none

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
  | "--browser-port" :: v :: r, a => parseArgs r { a with browserPort? := v.toNat? }
  | "--dev" :: r, a           => parseArgs r { a with dev := true }
  | "--rules" :: v :: r, a    => parseArgs r { a with rules? := some v }
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
  /-- The plan being worked on, in its editable text form (`Plan.render`). -/
  plan     : String := ""
  /-- The broad instruction the plan was made from (a note for each sub-goal). -/
  instruction : String := ""
  /-- The screen read as a list (`Elements.list`), for the user to pick
      from; cleared when a job starts (the screen is about to change). -/
  elements : List Elements.Element := []

structure Studio where
  args    : Args
  session : LeanTea.Browser.Session
  drv     : Driver
  vlm     : Vlm
  dir     : System.FilePath
  state   : IO.Ref State
  stopReq : IO.Ref Bool
  shotN   : IO.Ref Nat
  /-- Held while something drives the browser (a job, or one live-view
      screenshot). Separate from `State.busy`, which means "a job runs":
      a live screenshot must never make the chat look busy. -/
  lock    : IO.Ref Bool
  /-- Messages sent while a job runs, handed to it as hints. -/
  hints   : IO.Ref (Array String)
  /-- Where the user clicked the live view while a job runs: the job's
      next step clicks there. -/
  pointer : IO.Ref (Option (Nat × Nat))
  /-- An explore run (the only job that takes hints and pointed clicks). -/
  exploring : IO.Ref Bool

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

/-! ## Surviving a restart -/

def Studio.statePath (s : Studio) : System.FilePath := s.dir / "state.json"

/-- Write what a restart must not lose: the recorded steps, goals and chat. -/
def Studio.persist (s : Studio) : IO Unit := do
  let st ← s.state.get
  let j := Json.mkObj [
    ("startUrl", .str st.startUrl),
    ("goals", .arr (st.goals.toArray.map Json.str)),
    ("script", ({ name := "studio", steps := st.steps } : Script).toJson),
    ("events", .arr st.events),
    ("shotN", .num (Int.ofNat (← s.shotN.get))),
    ("lastShot", (st.lastShot.map Json.str).getD .null),
    ("plan", .str st.plan), ("instruction", .str st.instruction)]
  IO.FS.writeFile s.statePath j.compress

/-- Bring back a session saved by `persist` for the same start page. -/
def Studio.restore (s : Studio) : IO Bool := do
  if !(← s.statePath.pathExists) then return false
  let some j := (Json.parse (← IO.FS.readFile s.statePath)).toOption | return false
  let url := (j.getObjValD "startUrl").getStr?.toOption.getD ""
  if url != (← s.state.get).startUrl then return false
  let steps := match Script.fromJson (j.getObjValD "script") with
    | .ok sc => sc.steps
    | .error _ => []
  let goals := match j.getObjValD "goals" with
    | .arr a => a.toList.filterMap (·.getStr?.toOption)
    | _ => []
  let events := match j.getObjValD "events" with | .arr a => a | _ => #[]
  let lastShot := (j.getObjValD "lastShot").getStr?.toOption
  if let .num n := j.getObjValD "shotN" then s.shotN.set n.mantissa.toNat
  let plan := (j.getObjValD "plan").getStr?.toOption.getD ""
  let instruction := (j.getObjValD "instruction").getStr?.toOption.getD ""
  s.state.modify fun st => { st with steps, goals, events, lastShot, plan, instruction }
  return true

/-- Mark a job as running; false if one already is. -/
def Studio.claim (s : Studio) : IO Bool :=
  s.state.modifyGet fun st => if st.busy then (false, st) else (true, { st with busy := true })

def Studio.release (s : Studio) : IO Unit := s.state.modify ({ · with busy := false })

def Studio.tryLock (s : Studio) : IO Bool :=
  s.lock.modifyGet fun held => if held then (false, held) else (true, true)

def Studio.unlock (s : Studio) : IO Unit := s.lock.set false

/-- Take the browser, waiting for a live-view screenshot to finish. -/
partial def Studio.waitLock (s : Studio) : IO Unit := do
  if !(← s.tryLock) then
    IO.sleep 50
    s.waitLock

/-- Run `job` on its own thread, holding the browser for its duration. -/
def Studio.background (s : Studio) (job : IO Unit) : IO Bool := do
  if !(← s.claim) then return false
  s.stopReq.set false
  s.state.modify ({ · with elements := [] })
  let _ ← IO.asTask (prio := .dedicated) do
    s.waitLock
    try job catch e => s.push [("kind", "finished"), ("ok", .bool false), ("text", .str s!"error: {e}")]
    s.unlock
    s.release
    try s.persist catch _ => pure ()
  return true

/-- Hints sent since the last call (the running job picks them up). -/
def Studio.drainHints (s : Studio) : IO (List String) :=
  s.hints.modifyGet fun hs => (hs.toList, #[])

/-- "yes", "ok", "good!", "thanks" — a reply, not something to test. -/
def isAck (text : String) : Bool :=
  let words := (text.toLower.map fun c => if c.isAlphanum || c == ' ' then c else ' ').splitOn " " |>.filter (!·.isEmpty)
  let acks := ["yes", "y", "ok", "okay", "good", "great", "nice", "cool", "thanks", "thank", "you", "fine", "sure", "right", "perfect"]
  !words.isEmpty && words.length ≤ 3 && words.all acks.contains

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

/-- The lines of `--rules FILE`, read fresh (edit it while a run goes). -/
def Studio.rules (s : Studio) : IO (List String) := do
  let some path := s.args.rules? | return []
  if !(← System.FilePath.pathExists path) then return []
  return (← IO.FS.readFile path).splitOn "\n" |>.map (·.trimAscii.toString)
    |>.filter (fun l => !l.isEmpty && !l.startsWith "#")

/-- Explore one goal from where the recorded test stands; the steps that
    worked are recorded under `goal`. True if the goal was reached. -/
def Studio.exploreGoal (s : Studio) (goal : String) : IO Bool := do
  let st ← s.state.get
  let ctx ← s.ctx st.steps
  let notes := (if st.instruction.isEmpty then [] else [s!"The whole test: {st.instruction}"]) ++
    st.goals.reverse.map (s!"Earlier goal, already done: " ++ ·)
  let _ ← s.drainHints
  s.pointer.set none
  s.exploring.set true
  let (steps, reached) ← try
      Explore.explore ctx {} goal notes st.steps s.emit (s.stopReq.get) s.outline s.drainHints
        (s.pointer.modifyGet fun p => (p, none)) s.rules
    finally s.exploring.set false
  let fresh := (steps.drop st.steps.length).map ({ · with goal := some goal })
  -- a fresh test opens the page and must not act before it's ready (a
  -- game takes a while to load): gate it on its first click target
  let fresh := match st.steps, fresh with
    | [], first :: rest =>
      match first.act with
      | .clickDescribed t _ =>
        { act := .waitUntil s!"Is {t} visible on the screen?", note := "wait for the start screen", goal := some goal }
          :: first :: rest
      | _ => fresh
    | _, _ => fresh
  s.state.modify fun st => { st with steps := st.steps ++ fresh, verified := none, goals := goal :: st.goals }
  return reached

def Studio.explore (s : Studio) (goal : String) : IO Unit := do
  let _ ← s.exploreGoal goal

/-- The user clicked the live view with nothing running: click there and
    record it (a fixed point — fine for a game canvas, which `--clip`
    keeps at the same place). -/
def Studio.pointClick (s : Studio) (x y : Nat) (name? : Option String := none) : IO Bool := do
  let st ← s.state.get
  let ctx ← s.ctx st.steps
  let animated := (← settle ctx (maxMs := 2000)) ≥ 0.01
  let act := Action.clickXy x y
  let name? ← match name? with
    | some n => pure (some n)
    | none =>
      let (_, img) ← ctx.shot st.steps.length "look"
      try Explore.nameAt ctx img x y catch _ => pure none
  match ← Explore.attempt ctx {} st.steps.length act "the user pointed here" true animated with
  | .ok (obs, ev, pt) =>
    s.emit (.stepOk (Explore.describe act) obs ev pt)
    if let some n := name? then s.emit (.note s!"recorded as: wait until {n} shows, then click ({x}, {y})")
    s.state.modify fun st => { st with steps := st.steps ++ Explore.pointedSteps name? x y, verified := none }
    return true
  | .error (e, ev, pt) => s.emit (.stepFailed (Explore.describe act) e ev pt); return false

/-- Read the screen into a list of elements for the user to pick from. -/
def Studio.readScreen (s : Studio) : IO Unit := do
  let st ← s.state.get
  let ctx ← s.ctx st.steps
  let _ ← settle ctx (maxMs := 3000)
  let (_, img) ← ctx.shot st.steps.length "read"
  let t0 ← IO.monoMsNow
  let (es, answer) ← Elements.list s.vlm img
  s.state.modify ({ · with elements := es })
  if es.isEmpty then s.push [("kind", "note"), ("text", .str s!"nothing could be read from the model's answer: {answer.take 300}")]
  s.push [("kind", "note"),
          ("text", .str s!"read the screen: {es.length} elements ({((← IO.monoMsNow) - t0) / 1000}s) — pick one to click or check")]

/-- The user picked a listed element to check: record that it is shown
    (it is — the model just read it there). No browser needed, and the
    list stays. -/
def Studio.pickCheck (s : Studio) (e : Elements.Element) : IO Unit := do
  let q := s!"Is {e.label} visible on the screen?"
  s.state.modify fun st => { st with
    steps := st.steps ++ [{ act := .waitUntil q (timeoutMs := 30000), note := "check picked from the screen list" }],
    verified := none }
  s.push [("kind", "stepOk"), ("text", .str s!"check: {e.label} is shown"), ("observed", .str "recorded"),
          ("shot", .null), ("point", .null)]

/-- The user picked a listed element to click: click its centre, record
    it, and read the new screen. -/
def Studio.pickClick (s : Studio) (e : Elements.Element) : IO Unit := do
  if ← s.pointClick e.rect.cx.toUInt64.toNat e.rect.cy.toUInt64.toNat (some e.label) then s.readScreen

def Studio.pushResult (s : Studio) (r : StepResult) : IO Unit := do
  let url ← s.keepShot r.evidencePath
  s.push [("kind", if r.ok then "stepOk" else "stepFailed"),
          ("text", .str (Explore.describe r.step.act)), ("observed", .str (r.observed.getD "")),
          ("error", .str (r.error.getD "")), ("shot", (url.map Json.str).getD .null),
          ("point", .null)]

/-- Run the recorded steps as a plain script. From the start page, or
    (`start > 0`) from that step on the screen as it is now. -/
def Studio.replay (s : Studio) (start : Nat := 0) : IO Unit := do
  let st ← s.state.get
  if start == 0 then
    s.push [("kind", "note"), ("text", .str s!"replaying {st.steps.length} steps from {st.startUrl}…")]
    s.goto st.startUrl
  else
    s.push [("kind", "note"), ("text", .str s!"replaying from step {start + 1} on the current screen…")]
  let ctx ← s.ctx st.steps
  let res ← runScript ctx (fun _ r => s.pushResult r) start s.stopReq.get
  if start == 0 then s.state.modify ({ · with verified := some res.passed })
  s.push [("kind", "finished"), ("ok", .bool res.passed),
          ("text", .str (if res.passed then s!"replay passed ({res.steps.size} steps, {res.totalMs / 1000}s)"
                         else "replay failed — see the red step"))]

/-! ## Plans -/

def Studio.record (s : Studio) (step : Step) : IO Unit :=
  s.state.modify fun st => { st with steps := st.steps ++ [step], verified := none }

def Studio.askNow (s : Studio) (question : String) : IO Bool := do
  let st ← s.state.get
  let ctx ← s.ctx st.steps
  let (_, img) ← ctx.shot st.steps.length "until"
  return (← ask ctx.vlm img question).yes? == some true

/-- Work through plan items, recording as it goes. A sub-goal is
    explored; a loop's first round is explored and later rounds replay
    the steps it recorded. False when it stopped (a sub-goal not reached,
    a failed round, Stop). -/
partial def Studio.runItems (s : Studio) (items : List Plan.Item) : IO Bool := do
  for item in items do
    if ← s.stopReq.get then
      s.push [("kind", "finished"), ("ok", .bool false), ("text", .str "stopped")]
      return false
    match item with
    | .goal g =>
      s.push [("kind", "note"), ("text", .str s!"▶ {g}")]
      if !(← s.exploreGoal g) then
        s.push [("kind", "finished"), ("ok", .bool false),
                ("text", .str s!"plan stopped at: {g} — help with a hint or a click on the screen, or edit the plan and run the rest")]
        return false
    | .loop q mx body =>
      let tag := some s!"repeat until {q}"
      let startIdx := (← s.state.get).steps.length
      s.record { act := .repeatStart q mx, goal := tag }
      if ← s.askNow q then
        s.push [("kind", "note"), ("text", .str s!"repeat: already done ({q}), 0 rounds")]
        s.record { act := .repeatEnd, goal := tag }
        continue
      s.push [("kind", "note"), ("text", .str s!"repeat round 1 (exploring) until: {q}")]
      if !(← s.runItems body) then return false
      s.record { act := .repeatEnd, goal := tag }
      let all := (← s.state.get).steps
      let bodySteps := ((all.drop (startIdx + 1)).dropLast).toArray
      let mut finished := false
      for r in [1:mx + 1] do
        if ← s.stopReq.get then
          s.push [("kind", "finished"), ("ok", .bool false), ("text", .str "stopped")]
          return false
        if ← s.askNow q then
          finished := true
          break
        if r == mx then break
        s.push [("kind", "note"), ("text", .str s!"repeat round {r + 1}: replaying the {bodySteps.size} recorded steps")]
        let ctx ← s.ctx all
        let (ok, _, _) ← runSteps ctx bodySteps 0 (fun _ res => s.pushResult res) s.stopReq.get
        if !ok then
          s.push [("kind", "finished"), ("ok", .bool false),
                  ("text", .str s!"repeat round {r + 1} failed — edit the step in the test (or the plan) and run again")]
          return false
      if !finished then
        s.push [("kind", "finished"), ("ok", .bool false), ("text", .str s!"repeat: still not done after {mx} rounds — {q}")]
        return false
      s.push [("kind", "note"), ("text", .str s!"repeat done: {q}")]
  return true

/-- Turn a broad instruction into an editable plan (not run yet). -/
def Studio.makePlan (s : Studio) (instruction : String) : IO Unit := do
  let st ← s.state.get
  let ctx ← s.ctx st.steps
  let (_, img) ← ctx.shot st.steps.length "plan"
  s.push [("kind", "thinking"), ("text", .str "planning…")]
  let notes := (← s.rules) ++ st.goals.reverse.map (s!"Already done: " ++ ·)
  let items ← Plan.make ctx img instruction notes
  let text := Plan.render items
  s.state.modify ({ · with plan := text, instruction })
  s.push [("kind", "plan"), ("text", .str text)]

def Studio.runPlan (s : Studio) (text : String) : IO Unit := do
  let items ← IO.ofExcept (Plan.parse text)
  s.state.modify ({ · with plan := text })
  s.push [("kind", "note"), ("text", .str s!"running the plan ({items.length} items)")]
  if ← s.runItems items then
    s.push [("kind", "finished"), ("ok", .bool true), ("text", .str "plan done — Replay to check it runs by itself")]

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
    -- borrow the browser only when no job runs and nobody else holds it;
    -- otherwise show the last evidence screenshot
    if !st.busy && (← s.tryLock) then
      try
        let p := s.dir / "live.png"
        s.drv.screenshot p
        return png (← IO.FS.readBinFile p)
      finally s.unlock
    match st.lastShot with
    | some p => return png (← IO.FS.readBinFile p)
    | none => return err 503 "no screenshot yet"
  | "GET", path =>
    if let some (_, file) := demos.find? (·.1 == path) then return Response.html 200 (← IO.FS.readFile file)
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
        ("total", .num (Int.ofNat st.events.size)),
        ("busy", .bool st.busy),
        ("url", .str st.startUrl),
        ("steps", .arr (st.steps.toArray.map fun step => Json.mkObj [
          ("text", .str (Explore.describe step.act)),
          ("goal", (step.goal.map Json.str).getD .null),
          ("kind", .str (match step.act with
            | .repeatStart .. => "repeat" | .repeatEnd => "end" | _ => "step"))])),
        ("plan", .str st.plan),
        ("elements", .arr (st.elements.toArray.map fun e => Json.mkObj [
          ("label", .str e.label),
          ("x", .num (Int.ofNat e.rect.x.toUInt64.toNat)), ("y", .num (Int.ofNat e.rect.y.toUInt64.toNat)),
          ("w", .num (Int.ofNat e.rect.w.toUInt64.toNat)), ("h", .num (Int.ofNat e.rect.h.toUInt64.toNat))])),
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
    let hint := fun (_ : Unit) => do
      s.hints.modify (·.push text)
      s.push [("kind", "note"), ("text", .str "passed to the running attempt as a hint")]
      return ok
    if (← s.state.get).busy then
      if ← s.exploring.get then return ← hint ()
      return busyErr
    if isAck text then
      s.push [("kind", "note"), ("text", .str "OK — send the next thing to test (or Replay / Save the test).")]
      return ok
    if ← s.background (s.explore text) then return ok
    else return busyErr
  | "POST", "/api/point" =>
    let j := jsonBody req
    let num := fun k => match j.getObjValD k with
      | .num n => some (n.mantissa.toNat / 10 ^ n.exponent)
      | _ => none
    let (some x, some y) := (num "x", num "y") | return err 400 "x and y needed"
    s.push [("kind", "user"), ("text", .str s!"(clicked the screen at {x}, {y})")]
    if (← s.state.get).busy then
      if !(← s.exploring.get) then return busyErr
      s.pointer.set (some (x, y))
      s.push [("kind", "note"), ("text", .str "the running attempt clicks there next")]
      return ok
    return if ← s.background (do let _ ← s.pointClick x y) then ok else busyErr
  | "POST", "/api/read" =>
    return if ← s.background s.readScreen then ok else busyErr
  | "POST", "/api/pick" =>
    let j := jsonBody req
    let some i := (j.getObjValD "index").getNat?.toOption | return err 400 "index needed"
    let some e := (← s.state.get).elements[i]? | return err 400 "no such element — read the screen again"
    let check := str j "mode" == "check"
    s.push [("kind", "user"), ("text", .str s!"({if check then "check" else "click"} [{i + 1}] {e.label})")]
    if check then
      s.pickCheck e
      s.persist
      return ok
    return if ← s.background (s.pickClick e) then ok else busyErr
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
  | "POST", "/api/plan" =>
    let text := (str (jsonBody req) "text").trimAscii.toString
    if text.isEmpty then return err 400 "empty instruction"
    s.push [("kind", "user"), ("text", .str s!"plan: {text}")]
    -- text that is already a plan (several lines, or a loop) is taken as
    -- written: asking the model would only rewrite it
    let written := match Plan.parse text with
      | .ok items => if items.length ≥ 2 || items.any (fun | .loop .. => true | _ => false) then some items else none
      | .error _ => none
    match written with
    | some items =>
      let plan := Plan.render items
      s.state.modify ({ · with plan })
      s.push [("kind", "plan"), ("text", .str plan)]
      s.push [("kind", "note"), ("text", .str "that is already a plan — taken as written (press Run plan)")]
      return ok
    | none => return if ← s.background (s.makePlan text) then ok else busyErr
  | "POST", "/api/runplan" =>
    let text := str (jsonBody req) "plan"
    match Plan.parse text with
    | .error e => return err 400 s!"plan: {e}"
    | .ok [] => return err 400 "the plan is empty"
    | .ok _ => return if ← s.background (s.runPlan text) then ok else busyErr
  | "POST", "/api/script" =>
    if (← s.state.get).busy then return busyErr
    match Script.fromJson ((jsonBody req).getObjValD "script") with
    | .error e => return err 400 s!"script: {e}"
    | .ok sc =>
      s.state.modify fun st => { st with steps := sc.steps, verified := none }
      s.push [("kind", "note"), ("text", .str s!"test edited: {sc.steps.length} steps")]
      s.persist
      return ok
  | "POST", "/api/delete" =>
    if (← s.state.get).busy then return busyErr
    let j := jsonBody req
    let some i := (j.getObjValD "index").getNat?.toOption | return err 400 "index needed"
    s.state.modify fun st => { st with steps := st.steps.eraseIdx i, verified := none }
    s.persist
    return ok
  | "POST", "/api/undo" =>
    s.state.modify fun st => { st with steps := st.steps.dropLast, verified := none }
    s.persist
    return ok
  | "POST", "/api/replay" =>
    if (← s.state.get).steps.isEmpty then return err 400 "nothing recorded yet"
    let start := ((jsonBody req).getObjValD "from").getNat?.toOption.getD 0
    return if ← s.background (s.replay start) then ok else busyErr
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
  let page ← Template.mkProvider (← findFile "page.html") a.dev
  let demos ← [("/demo", "demo.html"), ("/demo-game", "game.html"), ("/demo-game/frame", "game-frame.html")].mapM
    fun (route, file) => do return (route, ← findFile file)
  let session ← LeanTea.Browser.Session.spawn
  let _ ← session.open 1280 800 (headless := some !a.headed) (profile := a.profile?) (cdpPort := a.browserPort?)
  let vlm : Vlm := { cfg := { baseUrl := a.baseUrl, timeoutSec := some 300 }, model := a.model,
                     frame := CoordFrame.guess a.model }
  let startUrl := a.url?.getD s!"http://127.0.0.1:{a.port}/demo"
  let drv ← match a.clip? with
    | some spec => (Driver.browser session).clipTo (browserRegion session spec)
    | none => pure (Driver.browser session)
  let s : Studio := { args := a, session, drv, vlm, dir,
                      state := ← IO.mkRef { startUrl }, stopReq := ← IO.mkRef false, shotN := ← IO.mkRef 0,
                      lock := ← IO.mkRef false, hints := ← IO.mkRef #[], pointer := ← IO.mkRef none, exploring := ← IO.mkRef false }
  let restored ← s.restore
  if restored then
    IO.println s!"  restored {(← s.state.get).steps.length} recorded steps from {s.statePath}"
  -- a kept browser already showing a page stays where it is (that's
  -- the point of keeping it); otherwise open the start page once the
  -- server below is answering (it may be /demo)
  let here ← try (match ← session.evaluate "location.href" with | .str u => pure u | _ => pure "")
    catch _ => pure ""
  let keep := a.browserPort?.isSome && !here.isEmpty && here != "about:blank"
  if keep then
    s.push [("kind", "note"), ("text", .str s!"restarted — the browser kept its page ({here})")]
  else
    let _ ← IO.asTask (prio := .dedicated) do
      IO.sleep 700
      let _ ← s.background (s.goto startUrl)
  IO.println s!"qa_studio: http://127.0.0.1:{a.port}/   testing {startUrl} with {a.model}"
  if a.headed && a.profile?.isSome then
    IO.println "  log in in the Chromium window if the site asks — the profile keeps it for next time"
  let _ ← IO.asTask (prio := .dedicated) (s.persist)
  -- concurrent: the headless browser keeps a keep-alive connection open
  -- (to /demo), which would block a one-connection-at-a-time server
  serveConcurrent a.port "127.0.0.1" (handler s page demos)

end QaStudio

def main (argv : List String) : IO Unit := QaStudio.main argv
