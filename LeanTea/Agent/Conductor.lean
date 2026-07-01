import LeanTea.Agent.Playbook
import LeanTea.Llm.McpOrchestrator
import Lean.Data.Json

/-! # LeanTea.Agent.Conductor — observe / pick / run / record loop

Sits on top of `LeanTea.Llm.McpOrchestrator` (for MCP tool calls)
and `LeanTea.Agent.Playbook` (for the routines to schedule). The
loop is:

```
loop:
  if paused or aborted: bail
  obs ← observer(orch)
  candidates ← playbooks.filter (·.pre.matches obs.screen ∧ ·.enabled)
  candidates ← drop those that hit `maxBurst`
  if candidates.empty:
    decision ← escalator { reason := "no_match", observation := obs, ... }
    apply decision
  else:
    pb ← bandit.select(candidates, stats)
    try:
      outcome ← runPlaybook(pb)
      stats.record(pb, outcome.success, outcome.reward)
    catch ex:
      decision ← escalator { reason := "exception", ..., error? := some ex }
      apply decision
```

The bandit is UCB1 over `Stats.avgReward + sqrt(2 ln N / n)`,
with the playbook's `estReward` as the optimistic prior used until
the first run is recorded. Same shape as the one used in most
contextual-bandit demos. -/

namespace LeanTea.Agent.Conductor

open Lean (Json)
open LeanTea.Agent.Playbook
open LeanTea.Llm.McpOrchestrator

/-! ## Observation -/

structure Observation where
  /-- Identifier the conductor matches preconditions against. -/
  screen     : String
  /-- Optional data URL screenshot, surfaced to the dashboard. -/
  screenshot : String := ""
  /-- Free-form context the observer can attach (entity counts,
      energy bar, anything the playbooks might read in the future). -/
  extra      : Json := Json.null
  deriving Inhabited

/-- An observer derives the current state from the live MCP world.

    Common implementations:
      * `dom.querySelector(...).textContent` via `browser__browser_evaluate`
      * a static value (when the conductor drives a single-screen game)
      * an LLM classifier given a screenshot

    Keeping this pluggable means a dashboard can swap classifiers
    without restarting the conductor process. -/
def Observer := Orchestrator → IO Observation

/-! ## Run outcome -/

structure RunOutcome where
  success      : Bool
  reward       : Float := 0.0
  durationMs   : Nat   := 0
  /-- The MCP tool responses, in execution order. Useful for the
      dashboard's "live" pane. -/
  steps        : Array Json := #[]
  error?       : Option String := none
  deriving Inhabited

/-! ## Step executor

We translate `LeanTea.Agent.Script.Action` into orchestrator
`callTool` invocations. Only the actions we actually need for v1
are handled; everything else routes through `.toolCall` which is
just "call any MCP tool with these args". -/

open LeanTea.Agent.Script in
private def runAction (orch : Orchestrator) (a : Action) : IO Json := do
  match a with
  | .clickXy x y =>
    orch.callTool "browser__browser_click_xy" <| Json.mkObj [
      ("x", Json.num (Int.ofNat x)),
      ("y", Json.num (Int.ofNat y))
    ]
  | .clickKnown _ =>
    /- v1 doesn't wire Memory.recall here yet; surface a clear error so
       the calling playbook switches to clickXy. -/
    throw <| IO.userError "clickKnown is not yet supported by the v1 Conductor — \
use clickXy or wrap the lookup in a tool_call to ui_recall"
  | .wait ms =>
    IO.sleep ms.toUInt32
    return Json.mkObj [("waited", Json.num (Int.ofNat ms))]
  | .screenshot _ =>
    orch.callTool "browser__browser_screenshot" (Json.mkObj [])
  | .waitForScreen _ timeoutMs =>
    /- Best-effort: we just sleep. A future version can poll the
       observer until the screen tag matches. -/
    IO.sleep timeoutMs.toUInt32
    return Json.mkObj [("waited_for_screen", Json.bool true)]
  | .toolCall tool args _ _ =>
    orch.callTool tool args

private def runPlaybookSteps (orch : Orchestrator) (pb : Playbook)
    : IO (Bool × Array Json × Option String) := do
  let mut acc : Array Json := #[]
  for s in pb.script.steps do
    try
      let r ← runAction orch s.act
      acc := acc.push r
    catch e =>
      return (false, acc, some s!"{e}")
  return (true, acc, none)

/-- Extract the `reward` field from the last step's result, if any.
    Playbooks that want to report a custom reward should make their
    last action a `tool_call` whose response carries a number under
    `reward`; otherwise a successful run is worth `pb.estReward`. -/
private def rewardFromSteps (pb : Playbook) (steps : Array Json) : Float :=
  match steps.back? with
  | none   => pb.estReward
  | some last =>
    match (last.getObjVal? "reward").toOption with
    | some (.num n) => n.mantissa.toNat.toFloat
    | some (.str s) =>
      /- Reuse Playbook's tiny float parser by going through Json. -/
      match s.toNat? with
      | some n => n.toFloat
      | none   => pb.estReward
    | _ => pb.estReward

/-! ## UCB1 selection

`score = avgReward + c * sqrt(ln totalRuns / n)` with c=√2. Playbooks
that haven't been run yet are scored with their `estReward` plus a
big bonus so each gets tried once. -/

private def ucbScore (s : Stats) (estReward : Float) (totalRuns : Nat) : Float :=
  if s.runs == 0 then
    estReward + 1e6   -- enormous explore bonus; rank above any played arm
  else
    let avg  := s.avgReward
    let nFlt := s.runs.toFloat
    let totl := (totalRuns.max 1).toFloat
    let bonus := (1.41421356 : Float) * Float.sqrt (Float.log totl / nFlt)
    avg + bonus

/-- Pick the best candidate by UCB1. -/
def selectPlaybook (candidates : Array (Playbook × Stats)) (totalRuns : Nat)
    : Option Playbook :=
  if candidates.isEmpty then none
  else
    let scored := candidates.map fun (p, s) => (p, ucbScore s p.estReward totalRuns)
    let best := scored.foldl
      (init := scored[0]!)
      (fun acc cur => if cur.2 > acc.2 then cur else acc)
    some best.1

/-! ## Escalation -/

structure EscalationCtx where
  reason       : String   -- "no_match" | "exception" | "burst_cap" | "manual"
  observation  : Observation
  playbook?    : Option Playbook := none
  error?       : Option String   := none
  /-- Recent (id, success) outcomes for context. -/
  recent       : Array (String × Bool) := #[]
  deriving Inhabited

inductive EscalationDecision where
  | retry
  | usePlaybook (id : String)
  | skipFor (ms : Nat)
  | pause
  | abort
  deriving Repr, Inhabited

def Escalator := EscalationCtx → IO EscalationDecision

/-- Default escalator: log to stderr + `LeanTea.Agent.Memory`
    escalations.jsonl, then `skipFor 2000` to try again later. Good
    enough for the "headless smoke" case. -/
def defaultEscalator : Escalator := fun ctx => do
  IO.eprintln s!"⚠ escalation: reason={ctx.reason} screen={ctx.observation.screen}"
  if let some e := ctx.error? then IO.eprintln s!"  error: {e}"
  return .skipFor 2000

/-! ## Live state -/

/-- One row of the run history, suitable for the dashboard timeline. -/
structure HistoryEntry where
  ts         : Nat
  playbookId : String
  success    : Bool
  reward     : Float
  durationMs : Nat
  deriving Inhabited

structure LiveState where
  /-- Currently running playbook id, or empty when idle. -/
  current     : String := ""
  observation : Observation := { screen := "(unknown)" }
  /-- Recent history (cap ~ 200 entries). -/
  history     : Array HistoryEntry := #[]
  stats       : Std.HashMap String Stats := ∅
  startedAtMs : Nat := 0
  cumReward   : Float := 0.0
  /-- Count of consecutive runs of the same playbook. Reset on switch
      / failure; used to enforce `maxBurst`. -/
  burstId     : String := ""
  burstCount  : Nat    := 0
  deriving Inhabited

/-! ## Config -/

structure Config where
  orch        : Orchestrator
  observer    : Observer
  escalator   : Escalator := defaultEscalator
  storeDir    : String
  /-- Time to sleep between iterations of the loop (ms). -/
  tickMs      : Nat := 250
  /-- Flag the dashboard flips to pause/unpause. -/
  paused      : IO.Ref Bool
  /-- Flag the dashboard flips to abort entirely. -/
  aborted     : IO.Ref Bool

/-- Build a fresh `Config` with default control refs. -/
def Config.mk' (orch : Orchestrator) (observer : Observer) (storeDir : String)
    (escalator : Escalator := defaultEscalator) (tickMs : Nat := 250)
    : IO Config := do
  let paused ← IO.mkRef false
  let aborted ← IO.mkRef false
  return { orch, observer, escalator, storeDir, tickMs, paused, aborted }

/-! ## One step -/

private def applyDecision (cfg : Config) (decision : EscalationDecision) : IO Unit := do
  match decision with
  | .retry        => pure ()
  | .usePlaybook _ => pure ()        -- next iteration will see fresh stats
  | .skipFor ms   => IO.sleep ms.toUInt32
  | .pause        => cfg.paused.set true
  | .abort        => cfg.aborted.set true

private def recordOutcome (cfg : Config) (state : IO.Ref LiveState)
    (pb : Playbook) (outcome : RunOutcome) : IO Unit := do
  let now ← IO.monoMsNow
  let st ← state.get
  let cur := (st.stats.get? pb.id).getD {}
  let cur := cur.record outcome.success outcome.reward now
  let stats := st.stats.insert pb.id cur
  let entry : HistoryEntry := {
    ts := now, playbookId := pb.id,
    success := outcome.success, reward := outcome.reward,
    durationMs := outcome.durationMs
  }
  let hist := st.history.push entry
  let hist := if hist.size > 200 then hist.extract (hist.size - 200) hist.size else hist
  let burstCount :=
    if pb.id == st.burstId then st.burstCount + 1 else 1
  state.set { st with
    current := "",
    history := hist,
    stats,
    cumReward := st.cumReward + outcome.reward,
    burstId := pb.id,
    burstCount
  }
  /- Persist stats so a conductor restart doesn't lose the bandit
     history. Cheap — written every tick at most. -/
  saveAllStats cfg.storeDir stats

private def runPlaybook (cfg : Config) (state : IO.Ref LiveState) (pb : Playbook)
    : IO Unit := do
  let now ← IO.monoMsNow
  state.modify (fun s => { s with current := pb.id })
  let (ok, steps, err?) ← runPlaybookSteps cfg.orch pb
  let later ← IO.monoMsNow
  let outcome : RunOutcome := {
    success := ok,
    reward := if ok then rewardFromSteps pb steps else 0.0,
    durationMs := later - now,
    steps,
    error? := err?
  }
  recordOutcome cfg state pb outcome
  unless ok do
    let st ← state.get
    let ctx : EscalationCtx := {
      reason := "exception",
      observation := st.observation,
      playbook? := some pb,
      error? := outcome.error?,
      recent := st.history.toList.reverse.take 5
        |>.map (fun h => (h.playbookId, h.success))
        |>.toArray
    }
    let decision ← cfg.escalator ctx
    applyDecision cfg decision

def tick (cfg : Config) (state : IO.Ref LiveState) : IO Unit := do
  if ← cfg.aborted.get then return
  if ← cfg.paused.get then
    IO.sleep cfg.tickMs.toUInt32
    return
  /- Observe. -/
  let obs ← cfg.observer cfg.orch
  state.modify (fun s => { s with observation := obs })
  /- Load playbooks (cheap; live reload). -/
  let playbooks ← listPlaybooks cfg.storeDir
  let st ← state.get
  let candidates := playbooks.filter fun pb =>
    pb.enabled
    && pb.pre.matches obs.screen
    && (pb.maxBurst == 0 || pb.id != st.burstId || st.burstCount < pb.maxBurst)
  if candidates.isEmpty then
    let ctx : EscalationCtx := {
      reason := "no_match", observation := obs,
      recent := st.history.toList.reverse.take 5
        |>.map (fun h => (h.playbookId, h.success))
        |>.toArray
    }
    let decision ← cfg.escalator ctx
    applyDecision cfg decision
    return
  let totalRuns := st.history.size
  let scored := candidates.map fun pb =>
    let s := (st.stats.get? pb.id).getD {}
    (pb, s)
  match selectPlaybook scored totalRuns with
  | none    => IO.sleep cfg.tickMs.toUInt32
  | some pb => runPlaybook cfg state pb

partial def runLoop (cfg : Config) (state : IO.Ref LiveState) : IO Unit := do
  if ← cfg.aborted.get then return
  try tick cfg state
  catch e => IO.eprintln s!"conductor: tick threw: {e}"
  IO.sleep cfg.tickMs.toUInt32
  runLoop cfg state

end LeanTea.Agent.Conductor
