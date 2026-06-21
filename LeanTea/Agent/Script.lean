import Lean.Data.Json
import LeanTea.Agent.Memory

/-! # LeanTea.Agent.Script — deterministic UI script runner

Most UI tests aren't open-ended exploration: they're "click these
buttons in this order, then assert the final state". For that, a
small LLM is overkill — each round costs ~10–30s and 99% of wall
time. A flat JSON script of clicks + waits + asserts runs in seconds,
deterministically, and only invokes an LLM at the failure boundary
(screen classification mismatch → escalate).

This module is the type + execution kernel. The CLI driver lives in
`examples/UiScript/Run.lean`. Action set is intentionally tiny —
add to it only when a real script needs the new step.

  ```json
  {
    "name": "dmm.main_story_battle_smoke",
    "steps": [
      {"act":"click_known","key":"dmm.hub.quest_tab","expect":"quest_select"},
      {"act":"click_known","key":"dmm.quest_select.main_story_button","expect":"chapter_list"},
      {"act":"click_known","key":"dmm.quest_detail.shutsugeki_button"},
      {"act":"wait_for_screen","screen":"quest_clear","timeoutMs":120000},
      {"act":"click_known","key":"dmm.quest_clear.next_button"}
    ]
  }
  ``` -/

namespace LeanTea.Agent.Script

open Lean (Json)

/-! ## Actions -/

inductive Action where
  /-- Click at raw viewport coordinates. -/
  | clickXy   (x y : Nat)
  /-- Click using a coordinate cached in `ui-map.json` by key. The
      runner resolves it through `LeanTea.Agent.Memory.recall`. -/
  | clickKnown (key : String)
  /-- Sleep this many milliseconds. The browser doesn't have to be
      idle; we just stall the runner. -/
  | wait      (ms : Nat)
  /-- Take a screenshot. When `saveAs` is set, the bridge writes the
      file to that absolute path. -/
  | screenshot (saveAs : Option String := none)
  /-- Poll a classifier until it reports the named screen, or
      `timeoutMs` elapses (whichever first). With no classifier
      attached, this degrades to a plain `wait timeoutMs` and
      records the screen name for the audit log. -/
  | waitForScreen (screen : String) (timeoutMs : Nat := 30000)
  /-- Call an arbitrary MCP tool. `args` is forwarded verbatim. When
      `saveAs` is set and the response carries an `image` block, the
      bytes are decoded from base64 and written to that path —
      that's how a comfyui_txt2img call becomes a step that produces
      a usable file on disk. -/
  | toolCall (tool : String) (args : Lean.Json)
             (saveAs : Option String := none)
             (timeoutMs : Nat := 180000)
  deriving Inhabited

structure Step where
  act    : Action
  /-- Expected screen name after this step. The runner asks the
      classifier (if any) to verify. With no classifier it's a
      free-form note for the audit log. -/
  expect : Option String := none
  /-- Free-form note shown in the step trace. -/
  note   : String := ""
  deriving Inhabited

structure Script where
  /-- Identifier — typically `<game>.<flow>`. -/
  name        : String
  description : String := ""
  steps       : List Step
  deriving Inhabited

/-! ## JSON codec -/

private def actionToJson : Action → Json
  | .clickXy x y => Json.mkObj [
      ("act", Json.str "click_xy"),
      ("x",   Json.num (Int.ofNat x)),
      ("y",   Json.num (Int.ofNat y))
    ]
  | .clickKnown k => Json.mkObj [
      ("act", Json.str "click_known"),
      ("key", Json.str k)
    ]
  | .wait ms => Json.mkObj [
      ("act", Json.str "wait"),
      ("ms",  Json.num (Int.ofNat ms))
    ]
  | .screenshot saveAs =>
    let base : List (String × Json) := [("act", Json.str "screenshot")]
    Json.mkObj (match saveAs with
      | some p => base ++ [("saveAs", Json.str p)]
      | none   => base)
  | .waitForScreen screen timeoutMs => Json.mkObj [
      ("act",       Json.str "wait_for_screen"),
      ("screen",    Json.str screen),
      ("timeoutMs", Json.num (Int.ofNat timeoutMs))
    ]
  | .toolCall tool args saveAs timeoutMs =>
    let base : List (String × Json) := [
      ("act",       Json.str "tool_call"),
      ("tool",      Json.str tool),
      ("args",      args),
      ("timeoutMs", Json.num (Int.ofNat timeoutMs))
    ]
    Json.mkObj (match saveAs with
      | some p => base ++ [("saveAs", Json.str p)]
      | none   => base)

private def stepToJson (s : Step) : Json :=
  let base := match actionToJson s.act with
    | .obj kvs => kvs.toList
    | _        => []
  let withExpect := match s.expect with
    | some e => base ++ [("expect", Json.str e)]
    | none   => base
  let withNote :=
    if s.note.isEmpty then withExpect
    else withExpect ++ [("note", Json.str s.note)]
  Json.mkObj withNote

def Script.toJson (s : Script) : Json :=
  Json.mkObj [
    ("name",        Json.str s.name),
    ("description", Json.str s.description),
    ("steps",       Json.arr (s.steps.toArray.map stepToJson))
  ]

private def getStr (j : Json) (k : String) : Except String String :=
  match j.getObjVal? k with
  | .ok v => v.getStr?
  | .error e => .error e

private def getStrOpt (j : Json) (k : String) : Option String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption)

private def getNatOpt (j : Json) (k : String) (default : Nat := 0) : Nat :=
  match j.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

private def actionFromJson (j : Json) : Except String Action := do
  let act ← getStr j "act"
  match act with
  | "click_xy" =>
    return .clickXy (getNatOpt j "x") (getNatOpt j "y")
  | "click_known" =>
    let k ← getStr j "key"
    return .clickKnown k
  | "wait" =>
    return .wait (getNatOpt j "ms")
  | "screenshot" =>
    return .screenshot (getStrOpt j "saveAs")
  | "wait_for_screen" =>
    let screen ← getStr j "screen"
    return .waitForScreen screen (getNatOpt j "timeoutMs" 30000)
  | "tool_call" =>
    let tool ← getStr j "tool"
    let args := (j.getObjVal? "args").toOption.getD (Json.mkObj [])
    return .toolCall tool args (getStrOpt j "saveAs")
      (getNatOpt j "timeoutMs" 180000)
  | other => .error s!"unknown action: {other}"

private def stepFromJson (j : Json) : Except String Step := do
  let act ← actionFromJson j
  return {
    act,
    expect := getStrOpt j "expect",
    note   := getStrOpt j "note" |>.getD ""
  }

def Script.fromJson (j : Json) : Except String Script := do
  let name ← getStr j "name"
  let description := getStrOpt j "description" |>.getD ""
  let stepsJson ← (j.getObjVal? "steps").bind (·.getArr?)
  let steps ← stepsJson.toList.mapM stepFromJson
  return { name, description, steps }

/-! ## Classifier — pluggable -/

/-- A classifier inspects a screenshot (via `outputPath` written by
    the bridge) and returns its best guess at the screen's name —
    one of `candidates` if it can decide, or "unknown" if not.

    Concrete implementations live elsewhere (see
    `examples/UiScript/Run.lean` for the LM Studio backed one), so
    this module stays free of model knowledge. -/
def Classifier := String → List String → IO String

/-! ## Execution result -/

structure StepResult where
  step          : Step
  ok            : Bool
  observed      : Option String := none   -- what the classifier saw
  /-- For audit: the screenshot path captured during this step,
      if any. Lets a downstream tool surface evidence on failure. -/
  evidencePath  : Option String := none
  /-- Wall clock for the step (milliseconds). -/
  durationMs    : Nat := 0
  error         : Option String := none
  deriving Inhabited

structure ScriptResult where
  script  : Script
  steps   : Array StepResult
  passed  : Bool
  totalMs : Nat
  /-- Number of steps in the script that were never reached because
      an earlier step failed. Used by the tree renderer. -/
  skipped : Nat := 0
  /-- Optional summary URL / path the runner saved the full manifest to. -/
  reportPath : Option String := none
  deriving Inhabited

/-! ## Text tree renderer

Renders a `ScriptResult` to an ASCII tree that fits comfortably in a
80-col terminal. Same shape as the one a downstream HTML report
should mirror — keeping both views in lockstep makes failures easier
to triage. -/

private def fmtAct : Action → String
  | .clickXy x y          => s!"clickXy ({x}, {y})"
  | .clickKnown k         => s!"clickKnown {k}"
  | .wait ms              => s!"wait {ms}ms"
  | .screenshot _         => "screenshot"
  | .waitForScreen s tms  => s!"waitForScreen {s} timeout={tms}ms"
  | .toolCall t _ saveAs _ =>
    match saveAs with
    | some p => s!"toolCall {t} → {p}"
    | none   => s!"toolCall {t}"

private def truncate (s : String) (n : Nat) : String :=
  if s.length ≤ n then s else (s.take (n - 1)).toString ++ "…"

private def fmtMs (ms : Nat) : String :=
  if ms < 1000 then s!"{ms}ms"
  else s!"{ms/1000}.{(ms % 1000) / 100}s"

/-- Render a single step result as a tree branch. `isLast` controls
    the connector glyph (`├─` vs `└─`). -/
private def renderStep (idx : Nat) (total : Nat) (r : StepResult) (isLast : Bool) : String :=
  let mark := if r.ok then "✓" else "✗"
  let connector := if isLast then "└─" else "├─"
  let head := s!"{connector} [{idx + 1}/{total}] {truncate (fmtAct r.step.act) 38} {mark} {fmtMs r.durationMs}"
  let observed := match r.observed with
    | some o => s!" ({truncate o 30})"
    | none   => ""
  let mainLine := head ++ observed
  let indent := if isLast then "   " else "│  "
  let extras :=
    (match r.error with
      | some e =>
        let pretty := truncate (e.replace "\n" " ") 70
        s!"\n{indent}  ✗ {pretty}"
      | none   => "") ++
    (match r.evidencePath with
      | some p => s!"\n{indent}  📸 {p}"
      | none   => "")
  mainLine ++ extras

/-- Full tree of a run — verdict header + every step branch +
    optional skipped tail + footer pointer to the manifest. -/
def ScriptResult.renderTree (r : ScriptResult) : String :=
  let verdict := if r.passed then "✓ PASS" else "✗ FAIL"
  let header := s!"{r.script.name} {verdict}  total={fmtMs r.totalMs}"
  let total := r.script.steps.length
  let executed := r.steps.toList.mapIdx fun i s =>
    /- A step is the last visual branch when (a) it's the literal
       last step in the run and (b) there are no skipped steps to
       follow it. -/
    let isLast := i + 1 == r.steps.size && r.skipped == 0
    renderStep i total s isLast
  let skippedNote :=
    if r.skipped > 0 then
      let from_ := r.steps.size + 1
      let to_ := r.script.steps.length
      [s!"└─ [{from_}/{total}..{to_}/{total}] ⊘ skipped ({r.skipped} after failure)"]
    else []
  let footer :=
    match r.reportPath with
    | some p => [s!"\n→ {p}"]
    | none   => []
  String.intercalate "\n" ([header] ++ executed ++ skippedNote ++ footer)

/-! ## JSON manifest of a run

Persisted to `~/.cache/leantea-agent/runs/<script>.<ts>.json`. Lets a
separate report tool (HTML, dashboard, …) consume the same data
without having to re-execute. -/

private def actToCompactStr (a : Action) : String := fmtAct a

private def stepResultToJson (idx : Nat) (r : StepResult) : Json :=
  let opt (k : String) : Option String → List (String × Json)
    | some v => [(k, Json.str v)]
    | none   => []
  Json.mkObj <|
    [("index",      Json.num (Int.ofNat (idx + 1))),
     ("action",     Json.str (actToCompactStr r.step.act)),
     ("expect",     match r.step.expect with | some e => Json.str e | none => Json.null),
     ("ok",         Json.bool r.ok),
     ("durationMs", Json.num (Int.ofNat r.durationMs))]
    ++ opt "observed" r.observed
    ++ opt "evidence" r.evidencePath
    ++ opt "error"    r.error

def ScriptResult.toJson (r : ScriptResult) : Json :=
  Json.mkObj [
    ("script",      Json.str r.script.name),
    ("description", Json.str r.script.description),
    ("passed",      Json.bool r.passed),
    ("totalMs",     Json.num (Int.ofNat r.totalMs)),
    ("steps",       Json.arr (r.steps.mapIdx (fun i s => stepResultToJson i s))),
    ("skipped",     Json.num (Int.ofNat r.skipped)),
    ("schema",      Json.str "leantea-agent/run/v1")
  ]

end LeanTea.Agent.Script
