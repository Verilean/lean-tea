import LeanTea.Vision.Qa

/-! # LeanTea.Vision.Explore — let the VLM work out a UI test by trial and error

Give it a goal in plain words ("log in as alice and check the welcome
message"). Each turn the model sees the current screenshot, the goal,
the user's notes, the steps that worked so far and the error from the
last attempt, and proposes ONE next step as JSON. The step runs through
the same kernel as `vision_qa` (`LeanTea.Vision.Qa.runStep`):

  * it worked → it's appended to the recorded script
  * it failed (target not found, assertion false, a click that changed
    nothing on screen) → it's dropped and the error goes back to the
    model, which tries something else

A goal is reached as soon as an assertion that expects "yes" passes
(small models rarely say `done` on their own and keep re-checking);
`done` before that is refused — a test must check something. The turn
also ends when the model says `fail`, runs out of turns / failures, or
is stopped. What's left is a
deterministic `Script`: replay it with `vision_qa`, no exploring.

The proposal format is deliberately small so a 4B model can follow it:

```json
{"action": "click",  "target": "the blue Log in button", "why": "…"}
{"action": "type",   "text": "alice"}
{"action": "key",    "key": "Enter"}
{"action": "assert", "question": "Does the page say Welcome, alice?", "expect": "yes"}
{"action": "done"}
{"action": "fail",   "reason": "…"}
``` -/

namespace LeanTea.Vision.Explore

open Lean (Json)
open LeanTea.Agent.Script
open LeanTea.Vision
open LeanTea.Vision.Qa
open LeanTea.Llm.Openai (chat userTextAndImage)

/-! ## Proposals -/

inductive Proposal where
  | step (act : Action) (why : String)
  /-- Click a field, then type into it (recorded as those two steps). -/
  | fill (target text : String) (why : String)
  | done (summary : String)
  | fail (reason : String)
  deriving Inhabited

/-- The first `{…}` object in a reply (models wrap JSON in prose or fences). -/
def extractObject (s : String) : Option String := do
  let cs := s.toList
  let start ← cs.findIdx? (· == '{')
  let mut depth := 0
  let mut inStr := false
  let mut esc := false
  for (c, k) in (cs.drop start).zipIdx do
    if inStr then
      if esc then esc := false
      else if c == '\\' then esc := true
      else if c == '"' then inStr := false
    else if c == '"' then inStr := true
    else if c == '{' then depth := depth + 1
    else if c == '}' then
      depth := depth - 1
      if depth == 0 then return String.ofList ((cs.drop start).take (k + 1))
  none

private def str (j : Json) (k : String) : String :=
  (j.getObjValD k).getStr?.toOption.getD ""

/-- Parse the model's reply into a proposal. -/
def parseProposal (reply : String) : Except String Proposal := do
  let some obj := extractObject reply | throw s!"no JSON object in the reply: {reply.take 200}"
  let j ← Json.parse obj
  let why := str j "why"
  match (str j "action").toLower with
  | "click" =>
    let t := str j "target"
    if t.isEmpty then throw "click needs a \"target\"" else return .step (.clickDescribed t) why
  | "type" =>
    let t := str j "text"
    if t.isEmpty then throw "type needs a non-empty \"text\"" else return .step (.typeText t) why
  | "fill" =>
    let (t, x) := (str j "target", str j "text")
    if t.isEmpty || x.isEmpty then throw "fill needs a \"target\" field and a \"text\""
    else return .fill t x why
  | "key" =>
    let k := str j "key"
    return .step (.key (if k.isEmpty then "Enter" else k)) why
  | "assert" =>
    let q := str j "question"
    if q.isEmpty then throw "assert needs a \"question\"" else
    let want := (str j "expect").toLower != "no"
    return .step (.assertVisual q want) why
  | "assert_text" => return .step (.assertText (str j "target") (str j "equals")) why
  | "wait" =>
    let secs := match (j.getObjValD "seconds") with
      | .num n => (n.mantissa.toNat / 10 ^ n.exponent)
      | _ => 2
    return .step (.wait ((max 1 (min 10 secs)) * 1000)) why
  | "wait_until" =>
    let q := str j "question"
    if q.isEmpty then throw "wait_until needs a \"question\"" else
    let secs := match (j.getObjValD "seconds") with
      | .num n => (n.mantissa.toNat / 10 ^ n.exponent)
      | _ => 120
    return .step (.waitUntil q ((max 5 (min 600 secs)) * 1000)) why
  | "done" => return .done (if why.isEmpty then str j "summary" else why)
  | "fail" => return .fail (str j "reason")
  | a => throw s!"unknown action \"{a}\" (use click / type / key / assert / done / fail)"

/-- One line describing a step, as shown to the model and in the chat. -/
def describe : Action → String
  | .clickDescribed t _ => s!"click \"{t}\""
  | .clickXy x y => s!"click at ({x}, {y})"
  | .typeText t => s!"type \"{t}\""
  | .key k => s!"press {k}"
  | .assertVisual q w => s!"assert \"{q}\" is {if w then "yes" else "no"}"
  | .assertText t e => s!"assert \"{t}\" reads \"{e}\""
  | .assertPixel x y rgb _ => s!"assert pixel ({x}, {y}) is {rgb}"
  | .wait ms => s!"wait {ms} ms"
  | .waitUntil q _ => s!"wait until \"{q}\""
  | .repeatStart q mx => s!"repeat until \"{q}\" (max {mx})"
  | .repeatEnd => "end repeat"
  | _ => "other step"

/-- The per-turn prompt. -/
def promptFor (goal : String) (notes : List String) (worked : List Action)
    (lastError? : Option String) (turnsLeft : Nat) (outline : String := "") (earlier : Nat := 0) : String :=
  -- only this goal's steps: a small model copies the last line it sees,
  -- and an earlier goal's closing check is the wrong thing to copy
  let done := (if earlier == 0 then "" else s!"({earlier} steps for earlier goals, not shown)\n") ++
    (if worked.isEmpty then "(none yet for this goal)" else
      String.intercalate "\n" (worked.zipIdx.map fun (a, k) => s!"{k + 1}. {describe a} — ok"))
  let noteText := if notes.isEmpty then "" else
    "Notes from the user:\n" ++ String.intercalate "\n" (notes.map ("- " ++ ·)) ++ "\n"
  let err := match lastError? with
    | some e => s!"Your last step FAILED: {e}\nTry a different step (another description, another element, or another approach).\n"
    | none => ""
  let outlineText := if outline.isEmpty then "" else
    s!"Elements on the page now (use their visible text to describe targets):\n{outline}\n"
  s!"You are testing the web page in the screenshot.\nGoal: {goal}\n{noteText}" ++
  s!"Steps that already worked:\n{done}\n{outlineText}{err}" ++
  s!"You have {turnsLeft} steps left. Choose the ONE next step. Reply with ONLY a JSON object, one of:\n" ++
  "{\"action\": \"fill\", \"target\": \"<the input field, e.g. the Username field>\", \"text\": \"<text to enter>\", \"why\": \"<short>\"}\n" ++
  "{\"action\": \"click\", \"target\": \"<the element to click, described by its visible text, colour and position>\", \"why\": \"<short>\"}\n" ++
  "{\"action\": \"key\", \"key\": \"Enter\", \"why\": \"<short>\"}\n" ++
  "{\"action\": \"wait\", \"seconds\": 3, \"why\": \"<short — e.g. the screen is still loading>\"}\n" ++
  "{\"action\": \"wait_until\", \"question\": \"<yes/no question, yes when the wait is over — e.g. Is the battle result shown?>\", \"seconds\": 180, \"why\": \"<short — e.g. the battle plays by itself>\"}\n" ++
  "{\"action\": \"assert\", \"question\": \"<a yes/no question about what is visible now that checks the goal>\", \"expect\": \"yes\", \"why\": \"<short>\"}\n" ++
  "{\"action\": \"done\", \"why\": \"<what was verified>\"}   (only after an assert proved the goal, or when told the goal looks achieved)\n" ++
  "{\"action\": \"fail\", \"reason\": \"<why the goal cannot be done on this page>\"}\n" ++
  "Fill every field the goal mentions before submitting. Check the goal with an assert that expects yes.\n" ++
  "Do ONLY what the goal asks. Once its last action is done, the next step is an assert of the result: " ++
  "do not go on to other buttons or screens.\n" ++
  "If the goal only asks to check something and the screen ALREADY shows it, assert it NOW. " ++
  "If the goal asks to click, skip, open, start or run something, do that first — a button being visible is not the goal.\n" ++
  "If a dialog or popup is open, the button to press is inside it: describe it as \"the <label> button in the dialog\" " ++
  "(the same label may also appear behind the dialog)."

/-! ## The loop -/

/-- What the explorer reports while it works (the UI renders these). -/
inductive Event where
  | thinking (text : String)
  | proposal (text : String) (why : String)
  | stepOk (text : String) (observed : String) (shot? : Option String) (point? : Option (Nat × Nat))
  | stepFailed (text : String) (error : String) (shot? : Option String) (point? : Option (Nat × Nat))
  | note (text : String)
  | finished (ok : Bool) (summary : String)
  deriving Inhabited

structure Config where
  maxTurns    : Nat := 16
  maxFailures : Nat := 6
  maxTokens   : Nat := 300
  /-- A click / typing / key whose before and after screenshots differ
      in fewer sampled pixels than this "changed nothing" and fails. -/
  minChangedPixels : Nat := 4

/-- Assertions that can confirm a goal (a "no" can't). -/
private def confirms : Action → Bool
  | .assertVisual _ want => want
  | .assertText .. | .assertPixel .. => true
  | _ => false

/-- Steps that must visibly change the page to count. -/
private def mustChange : Action → Bool
  | .clickDescribed .. | .clickXy .. | .typeText .. | .key .. => true
  | _ => false

/-- Ask the model for the next step (temperature rises after failures so
    a retry isn't the same answer again). -/
def propose (c : Ctx) (img : Image) (prompt : String) (temperature : Float) (maxTokens : Nat) : IO String := do
  let res ← chat c.vlm.cfg {
    model := c.vlm.model,
    messages := [userTextAndImage prompt (← img.dataUrl)],
    temperature := some temperature,
    maxTokens := some maxTokens }
  return stripThink res.content

/-- A short name for what is at `(x, y)` (the user pointed there), from a
    crop around the point — used to gate a replayed fixed-point click on
    that thing being on screen. -/
def nameAt (c : Ctx) (img : Image) (x y : Nat) : IO (Option String) := do
  let crop := img.crop (x - min x 160) (y - min y 80) 320 160
  let res ← chat c.vlm.cfg {
    model := c.vlm.model,
    messages := [userTextAndImage ("Name the button, icon or text at the centre of this image in at most 6 words " ++
      "(e.g. \"the Section 3 button\"). Reply with only the name.") (← crop.dataUrl)],
    temperature := some 0.0, maxTokens := some 30 }
  let name := (stripThink res.content).trimAscii.toString
  let name := String.ofList (name.toList.filter (fun ch => ch != '"' && ch != '\n'))
  let name := if name.endsWith "." then (name.dropEnd 1).toString else name
  -- "No button here", "There is nothing…": not a name to wait for
  let low := name.toLower
  let nothing := ["no ", "none", "nothing", "there is", "there are", "i cannot", "i can't", "empty"].any (fun (p : String) => low.startsWith p)
  return if name.isEmpty || name.length > 60 || nothing then none else some name

/-- The steps recorded for a click the user pointed at: wait until the
    thing there shows (so a replay doesn't click a loading screen), then
    click the point. -/
def pointedSteps (name? : Option String) (x y : Nat) : List Step :=
  (match name? with
   | some n => [{ act := .waitUntil s!"Is {n} visible on the screen?" (timeoutMs := 30000),
                  note := "wait for what the user pointed at" }]
   | none => []) ++
  [{ act := .clickXy x y, note := "the user pointed here" }]

/-- Is the whole goal done? Asked after a check passes: a small model
    tends to assert something easy ("is the chapter ready to run?") once
    it has made some progress, and stop halfway. `(true, _)` when the
    reply can't be read, so a confused judge never blocks a run. -/
def goalReached (c : Ctx) (img : Image) (goal : String) (worked : List Step) : IO (Bool × String) := do
  -- with the model's reason, so "click the title text (purpose: tap to
  -- start)" reads as the tap the goal asked for
  let done := if worked.isEmpty then "(none)" else
    String.intercalate "\n" (worked.zipIdx.map fun (st, k) =>
      -- "DONE:" first: a reason like "the goal is to click Next" otherwise
      -- reads as a click still to make
      s!"{k + 1}. DONE: {describe st.act}" ++ (if st.note.isEmpty then "" else s!" (purpose: {st.note})"))
  let prompt := s!"Goal: {goal}\nSteps done so far for this goal (each one worked):\n{done}\n" ++
    "Has the WHOLE goal been achieved? Count the steps above as done: a goal that only asks to click or press " ++
    "something is achieved once that click is in the steps, even if the button is gone now. " ++
    "If the goal asks for something the steps have not done yet, answer no and say what is left.\n" ++
    "Reply with ONLY a JSON object: {\"answer\": \"yes\" or \"no\", \"left\": \"<what is left, or empty>\"}"
  let res ← chat c.vlm.cfg {
    model := c.vlm.model,
    messages := [userTextAndImage prompt (← img.dataUrl)],
    temperature := some 0.0, maxTokens := some 120 }
  let some obj := extractObject (stripThink res.content) | return (true, "")
  let some j := (Json.parse obj).toOption | return (true, "")
  return ((str j "answer").toLower != "no", str j "left")

/-- Run one action, insisting that clicks / typing / keys change the
    screen. `.ok (observed, evidence, point)` or `.error (msg, evidence, point)`. -/
def attempt (c : Ctx) (cfg : Config) (idx : Nat) (act : Action) (why : String) (requireChange := true)
    (animated := false) :
    IO (Except (String × Option String × Option (Nat × Nat)) (String × Option String × Option (Nat × Nat))) := do
  let (_, before) ← c.shot idx "pre"
  c.lastPoint.set none
  let r ← runStep c idx { act, note := why }
  let point ← c.lastPoint.get
  match r.error with
  | some e => return .error (e, r.evidencePath, point)
  | none =>
    if requireChange && mustChange act then
      if let some after := r.evidencePath then
        let afterImg ← Image.load after
        -- on a screen that animates by itself the whole-screen diff is
        -- meaningless; judge a click by the area around it instead
        let changed := match act, point, animated with
          | .clickDescribed .., some (x, y), true | .clickXy .., some (x, y), true =>
            changedPixelsIn before afterImg (x - min x 120) (y - min y 120) 240 240
          | _, _, _ => changedPixels before afterImg
        if changed < cfg.minChangedPixels then
          let what := match act with
            | .typeText _ => "typing changed nothing on the screen — the field probably doesn't have focus (use fill)"
            | .key k => s!"pressing {k} changed nothing on the screen"
            | _ => s!"the click at {point.map (fun (x, y) => s!"({x}, {y})") |>.getD "?"} changed nothing on the screen — probably the wrong element"
          return .error (what, r.evidencePath, point)
    return .ok (r.observed.getD "", r.evidencePath, point)

/-- Explore toward `goal`, starting from the steps that already worked
    (`worked`, from earlier goals in the same session). Returns the
    steps that worked — the recorded script so far — and whether the
    goal was reached. `stop` is polled between turns. -/
def explore (c : Ctx) (cfg : Config) (goal : String) (notes : List String) (worked : List Step)
    (emit : Event → IO Unit) (stop : IO Bool) (outline : IO String := pure "")
    (hints : IO (List String) := pure [])
    (pointed : IO (Option (Nat × Nat)) := pure none)
    (rules : IO (List String) := pure []) : IO (List Step × Bool) := do
  let mut notes := notes
  -- a check that passes before anything was done doesn't prove the goal
  let mut acted := false
  let mut steps := worked
  let mut lastError : Option String := none
  let mut failures := 0
  let mut tried : List (String × String) := []
  let mut checkedThisGoal := false
  -- the judge said the goal is achieved after an action: `done` (or a
  -- passing check) ends the goal; more clicking would wander off it
  let mut judgedDone := false
  let mut idx := worked.length
  for turn in [0:cfg.maxTurns] do
    if failures ≥ cfg.maxFailures then
      emit (.finished false s!"stopped after {failures} failed attempts in a row")
      return (steps, false)
    if ← stop then
      emit (.finished false "stopped")
      return (steps, false)
    -- let loading / transitions finish; a page that never goes quiet is animated
    let motion ← settle c (maxMs := 4000)
    let animated := motion ≥ 0.01
    let (_, img) ← c.shot idx "look"
    emit (.thinking s!"turn {turn + 1}: looking at the page…")
    -- messages the user sent while we work
    let fresh ← hints
    for h in fresh do
      notes := notes ++ [s!"User hint (follow it): {h}"]
      emit (.note s!"hint received: {h}")
    -- the user clicked the live view: that's the next step, no model call
    if let some (x, y) ← pointed then
      let act := Action.clickXy x y
      emit (.proposal (describe act) "the user pointed here")
      let name? ← try nameAt c img x y catch _ => pure none
      match ← attempt c cfg idx act "the user pointed here" true animated with
      | .ok (obs, ev, pt) =>
        let added := pointedSteps name? x y
        steps := steps ++ added
        idx := idx + added.length
        acted := true
        failures := 0
        lastError := none
        emit (.stepOk (describe act) obs ev pt)
        if let some n := name? then emit (.note s!"recorded as: wait until {n} shows, then click ({x}, {y})")
      | .error (e, ev, pt) =>
        lastError := some s!"the user's click at ({x}, {y}) failed: {e}"
        emit (.stepFailed (describe act) e ev pt)
      continue
    let pageOutline ← try outline catch _ => pure ""
    -- standing advice, re-read every turn so it can be edited mid-run
    let ruleNotes ← try rules catch _ => pure []
    let prompt := promptFor goal (ruleNotes ++ notes) ((steps.drop worked.length).map (·.act)) lastError
      (cfg.maxTurns - turn) pageOutline worked.length
    let reply ← try propose c img prompt (if failures == 0 then 0.0 else 0.5) cfg.maxTokens
      catch e => do
        emit (.finished false s!"model call failed: {e}")
        return (steps, false)
    match parseProposal reply with
    | .error e =>
      failures := failures + 1
      lastError := some s!"your reply was not a valid step ({e}). Reply with one JSON object."
      emit (.note s!"unreadable reply: {e}")
    | .ok (.fail reason) =>
      if judgedDone then
        emit (.finished true "goal done (its last action was judged to finish it; no check of its own)")
        return (steps, true)
      -- a small model gives up looking for a button the goal already got
      -- past ("there is no Next here, this is the selection screen")
      if acted then
        let (_, now) ← c.shot idx "judge"
        let (whole, _) ← try goalReached c now goal (steps.drop worked.length)
          catch e => do emit (.note s!"judge call failed: {e}"); pure (false, "")
        if whole then
          emit (.finished true s!"goal done (judged from the screen; the model had given up: {reason})")
          return (steps, true)
      emit (.finished false s!"the model gave up: {reason}")
      return (steps, false)
    | .ok (.done summary) =>
      if judgedDone then
        emit (.finished true (if summary.isEmpty then "goal done" else summary))
        return (steps, true)
      if checkedThisGoal then
        let (_, now) ← c.shot idx "judge"
        let (whole, left) ← try goalReached c now goal (steps.drop worked.length) catch _ => pure (true, "")
        if whole then
          emit (.finished true (if summary.isEmpty then "goal verified" else summary))
          return (steps, true)
        failures := failures + 1
        lastError := some s!"(not an error) the goal is not finished yet: {left}. Do that next."
        emit (.note s!"not finished yet: {left}")
        continue
      failures := failures + 1
      lastError := some "you said done, but no assert has verified the goal yet. Add an assert step that checks the goal on screen."
      emit (.note "done refused: no passing assert yet")
    | .ok proposal =>
      -- a `fill` is a click on the field followed by typing into it
      let (acts, why) := match proposal with
        | .fill t x why => ([Action.clickDescribed t, .typeText x], why)
        | .step act why => ([act], why)
        | _ => ([], "")
      let text := match proposal with
        | .fill t x _ => s!"fill \"{t}\" with \"{x}\""
        | _ => String.intercalate ", then " (acts.map describe)
      emit (.proposal text why)
      -- the same step again right after it worked adds nothing — except
      -- that insisting on a check that already passed, before doing
      -- anything, means the goal was only that check
      -- (a click / key may well repeat: the same label on the next
      -- screen, e.g. Skip and then Skip in its confirmation dialog)
      let isWait := acts.any fun a => match a with | .wait _ | .waitUntil .. => true | _ => false
      -- waiting may repeat (a long battle), but not forever on a screen
      -- that waits for a click: after 3 waits in a row, ask for an action
      let waitsInARow := ((steps.drop worked.length).reverse.takeWhile fun st =>
        match st.act with | .wait _ | .waitUntil .. => true | _ => false).length
      if isWait && waitsInARow ≥ 3 then
        failures := failures + 1
        lastError := some ("waiting has not moved the screen on (3 waits in a row). It needs an action: " ++
          "a click (a Skip / Next / close button, or the screen itself), a key, or wait_until with a question if " ++
          "something really plays by itself.")
        emit (.note "refused a 4th wait in a row")
        continue
      if acts.length == 1 && !acts.any mustChange && !isWait && steps.getLast?.map (·.act |> describe) == some text then
        if !acted && acts.all confirms && checkedThisGoal then
          let (_, now) ← c.shot idx "judge"
          let (whole, left) ← try goalReached c now goal (steps.drop worked.length) catch _ => pure (true, "")
          if whole then
            emit (.finished true s!"goal verified (a check only): {text}")
            return (steps, true)
          failures := failures + 1
          lastError := some s!"(not an error) checking is not enough, the goal is not finished: {left}. Do that next (click / fill …)."
          emit (.note s!"not finished yet: {left}")
          continue
        failures := failures + 1
        lastError := some s!"`{text}` was just done and holds. Do NOT assert again: the next step must be a click, fill, key or wait that moves toward the goal."
        emit (.note s!"skipped a repeat of `{text}`")
        continue
      match tried.find? (·.1 == text) with
      | some (_, prev) =>
        failures := failures + 1
        lastError := some s!"you already tried `{text}`; it failed with: {prev}. Choose something else."
        emit (.stepFailed text "already tried and failed" none none)
      | none =>
        let mut done : List Step := []
        let mut problem : Option String := none
        for (act, k) in acts.zipIdx do
          if problem.isSome then break
          -- in a fill, the click may land on a field that already has focus
          -- (nothing visible changes); the typing that follows must show
          let requireChange := !(acts.length > 1 && k == 0)
          match ← attempt c cfg (idx + done.length) act why requireChange animated with
          | .ok (obs, ev, pt) =>
            done := done ++ [{ act, note := why }]
            emit (.stepOk (describe act) obs ev pt)
          | .error (e, ev, pt) =>
            problem := some e
            emit (.stepFailed (describe act) e ev pt)
        match problem with
        | none =>
          steps := steps ++ done
          idx := idx + done.length
          lastError := none
          failures := 0
          -- what failed on the old screen may work on the new one
          if acts.any mustChange then tried := []
          if acts.any (fun a => !confirms a && !(match a with | .assertVisual .. => true | _ => false)) then
            acted := true
            -- did that action finish the goal? (then say so, before the
            -- model wanders on to the next button it sees)
            if !judgedDone then
              let (_, now) ← c.shot idx "judge"
              let (whole, _) ← try goalReached c now goal (steps.drop worked.length)
                catch e => do emit (.note s!"judge call failed: {e}"); pure (false, "")
              if whole then
                judgedDone := true
                lastError := some ("(not an error) the goal looks achieved now. Check the result with an assert that " ++
                  "expects yes, or reply done. Do not click anything else.")
                emit (.note "the goal looks done — asking for a final check")
          if acts.any confirms then
            if acted then
              let (_, now) ← c.shot idx "judge"
              -- already judged done after the last action: this check closes it
              let (whole, left) ← if judgedDone then pure (true, "") else
                try goalReached c now goal (steps.drop worked.length) catch _ => pure (true, "")
              if whole then
                emit (.finished true s!"goal verified: {text}")
                return (steps, true)
              -- the check stays recorded (it held); the goal goes on
              lastError := some s!"(not an error) that check passed, but the goal is not finished yet: {left}. Do the next action the goal asks for."
              emit (.note s!"check passed, but not finished yet: {left}")
              continue
            checkedThisGoal := true
            lastError := some ("(not an error) that check already passes before doing anything. " ++
              "Now do what the goal asks (click / fill …), then assert the result. If the goal only asked to check, reply done.")
            emit (.note "that already holds — now doing what the goal asks")
        | some err =>
          failures := failures + 1
          tried := (text, err) :: tried
          lastError := some err
          -- a check that misses may be checking the wrong thing: the
          -- screen can already show the goal (a battle that ended while
          -- the model looked for the battle). Ask before it wanders on.
          if acted && !judgedDone then
            let (_, now) ← c.shot idx "judge"
            let (whole, _) ← try goalReached c now goal (steps.drop worked.length)
              catch e => do emit (.note s!"judge call failed: {e}"); pure (false, "")
            if whole then
              judgedDone := true
              lastError := some (s!"that step failed ({err}), but the goal looks achieved on the screen now. " ++
                "Assert what the screen shows now that proves the goal, or reply done. Do not click anything else.")
              emit (.note "the goal looks done — asking for a check of what is on screen")
    if failures ≥ cfg.maxFailures then
      emit (.finished false s!"stopped after {failures} failed attempts in a row")
      return (steps, false)
  emit (.finished false s!"out of turns ({cfg.maxTurns})")
  return (steps, false)

end LeanTea.Vision.Explore
