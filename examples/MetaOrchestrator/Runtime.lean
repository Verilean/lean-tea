import MetaOrchestrator.Zellij
import MetaOrchestrator.Director
import MetaOrchestrator.Config
import LeanTea.Cloud.Gemini

/-! # examples/MetaOrchestrator/Runtime.lean — one polling loop per agent

Each `ManagedAgent` spawned via `Runtime.start` runs its own
`IO.asTask`-backed coroutine that polls its pane, calls Gemini on
stall, and writes back. Each agent's state lives behind an `IO.Ref`
so the controller (TUI / web / CLI) can snapshot it for display
and ask agents to stop without racing the poll loop.

`IO.Ref.modify` is the atomic-enough primitive for our access
pattern (single ref, read-modify-write entirely inside one
function call; no cross-ref invariants).
-/

namespace MetaOrchestrator.Runtime

open MetaOrchestrator
open LeanTea.Cloud.Gemini in
abbrev GeminiConfig := Config

/-- Per-agent runtime state. Owned by the agent's task, snapshotted
    via `get` by the controller for display. -/
structure AgentState where
  agent       : Config.ManagedAgent
  status      : String         -- "starting" | "running" | "stable" | "asking-gemini" | "instructed" | "awaiting-user" | "stopped" | "disabled" | "error: …"
  memos       : List Director.Memo
  lastHash    : UInt64 := 0
  stableSec   : Nat := 0
  preDecisionHash : UInt64 := 0
  lastDecision : String := ""  -- one-line summary for the UI
  lastDecisionAt : Nat := 0    -- monoMs at last decision
  pollCount   : Nat := 0       -- how many poll iterations have run
  /-- Cooperative shutdown flag. Set to true to ask the loop to exit
      after its current poll. -/
  stopRequested : Bool := false
  deriving Inhabited

abbrev AgentHandle := IO.Ref AgentState

structure RuntimeState where
  cfg        : GeminiConfig
  /-- id ↦ handle. We rebuild this list on /add and /remove. -/
  agents     : IO.Ref (List (String × AgentHandle))
  configPath : String
  config     : IO.Ref Config.Config

/-! ## Logging helpers -/

private def isoNow : IO String := do
  let now ← IO.monoMsNow
  return s!"t+{now}ms"

private def appendJsonl (path : String) (j : Lean.Json) : IO Unit := do
  let line := j.compress ++ "\n"
  let h ← IO.FS.Handle.mk path .append
  h.putStr line
  h.flush

private def memoLogPath (logDir : String) (agentId : String) : String :=
  logDir ++ "/" ++ agentId ++ ".memos.jsonl"

private def decisionLogPath (logDir : String) (agentId : String) : String :=
  logDir ++ "/" ++ agentId ++ ".decisions.jsonl"

private def diffSummary (prev cur : UInt64) : String :=
  if prev == cur then "(no change in pane)" else "(pane changed)"

private def decisionName : Director.Decision → String
  | .continue   => "continue"
  | .instruct _ => "instruct"
  | .askUser _  => "ask_user"

private def decisionText : Director.Decision → String
  | .continue   => ""
  | .instruct s => s
  | .askUser s  => s

/-! ## Memo replay (for resume) -/

def loadMemos (logDir : String) (agentId : String) (keep : Nat)
    : IO (List Director.Memo) := do
  let path := memoLogPath logDir agentId
  if !(← System.FilePath.pathExists path) then return []
  let contents ← IO.FS.readFile path
  let lines := contents.splitOn "\n" |>.filter (fun l => !l.trim.isEmpty)
  let mut acc : Array Director.Memo := #[]
  for line in lines do
    match Lean.Json.parse line with
    | .ok j =>
      match Director.Memo.ofJson? j with
      | some m =>
        if m.sessionId == agentId then acc := acc.push m
      | none => pure ()
    | .error _ => pure ()
  let total := acc.size
  let take := if total > keep then keep else total
  let recent := acc.toList.drop (total - take)
  return recent.reverse

/-! ## The per-agent loop -/

partial def loop (rt : RuntimeState) (handle : AgentHandle) : IO Unit := do
  let st ← handle.get
  if st.stopRequested then
    handle.modify (fun s => { s with status := "stopped" })
    return
  if !st.agent.enabled then
    handle.modify (fun s => { s with status := "disabled" })
    IO.sleep 1000
    loop rt handle
    return
  IO.sleep (st.agent.pollSec.toUInt32 * 1000)
  let dump ← Zellij.dumpScreen st.agent.pane
  let h := Zellij.cheapHash dump
  let nextStable := if h == st.lastHash then st.stableSec + st.agent.pollSec else 0
  let stall := nextStable ≥ st.agent.stallSec
  if !stall then
    handle.modify (fun s =>
      { s with lastHash := h, stableSec := nextStable, pollCount := s.pollCount + 1,
               status := if h == s.lastHash then "stable" else "running" })
    loop rt handle
    return
  let memosWithSummary :=
    match st.memos with
    | [] => st.memos
    | m :: rest =>
      if m.afterSummary.isEmpty then
        ({ m with afterSummary := diffSummary st.preDecisionHash h }) :: rest
      else st.memos
  handle.modify (fun s => { s with status := "asking-gemini" })
  let verdict ← try Director.decide rt.cfg st.agent.goal memosWithSummary dump
                catch e => pure {
                  decision := .continue,
                  reasoning := s!"err: {e}"
                }
  let actName := decisionName verdict.decision
  let text := decisionText verdict.decision
  let ts ← isoNow
  let cfg ← rt.config.get
  appendJsonl (decisionLogPath cfg.logDir st.agent.id) <| Lean.Json.mkObj [
    ("sessionId", Lean.Json.str st.agent.id),
    ("ts", Lean.Json.str ts),
    ("action", Lean.Json.str actName),
    ("reasoning", Lean.Json.str verdict.reasoning),
    ("text", Lean.Json.str text)
  ]
  let memo : Director.Memo := {
    sessionId := st.agent.id, ts := ts,
    action := actName, text := text, afterSummary := ""
  }
  appendJsonl (memoLogPath cfg.logDir st.agent.id) memo.toJson
  let memos' := memo :: (memosWithSummary.take 9)
  let now ← IO.monoMsNow
  match verdict.decision with
  | .instruct s => Zellij.submit st.agent.pane s
  | .askUser q =>
    IO.eprintln s!"\n=== {st.agent.id}: askUser ===\n{q}\n=============="
    IO.eprintln s!"  reply with: /reply {st.agent.id} <your message>"
  | .continue => pure ()
  let nextStatus := match verdict.decision with
                    | .continue   => "continue"
                    | .instruct _ => "instructed"
                    | .askUser _  => "awaiting-user"
  let nextStable' := match verdict.decision with
                     | .continue => nextStable
                     | _ => 0
  handle.modify (fun _ => { st with
    status := nextStatus,
    memos := memos',
    lastHash := h,
    stableSec := nextStable',
    preDecisionHash := h,
    lastDecision := s!"[{ts}] {actName}: {verdict.reasoning}",
    lastDecisionAt := now,
    pollCount := st.pollCount + 1
  })
  loop rt handle

/-! ## Spawn / stop -/

/-- Spawn a fresh task for `agent` and register it under its id. If
    an agent with the same id is already running, the old one is
    asked to stop first. -/
def start (rt : RuntimeState) (agent : Config.ManagedAgent) : IO Unit := do
  -- Stop any existing instance with this id first.
  let lst ← rt.agents.get
  match lst.find? (fun (id, _) => id == agent.id) with
  | some (_, oldHandle) =>
    oldHandle.modify (fun s => { s with stopRequested := true })
  | none => pure ()
  let cfg ← rt.config.get
  let memos ← loadMemos cfg.logDir agent.id 10
  let handle ← IO.mkRef ({
    agent := agent,
    status := "starting",
    memos := memos
  } : AgentState)
  rt.agents.modify (fun lst =>
    -- Drop any stopped twin of the same id, then push the new handle.
    lst.filter (fun (id, _) => id != agent.id) ++ [(agent.id, handle)])
  let _ ← IO.asTask (loop rt handle)
  IO.eprintln s!"[runtime] started agent '{agent.id}' (pane={agent.pane}, resumed_memos={memos.length})"

/-- Ask an agent to stop (cooperatively, on its next poll wake-up). -/
def stop (rt : RuntimeState) (agentId : String) : IO Unit := do
  let lst ← rt.agents.get
  match lst.find? (fun (id, _) => id == agentId) with
  | some (_, h) =>
    h.modify (fun s => { s with stopRequested := true })
    IO.eprintln s!"[runtime] stop requested for '{agentId}'"
  | none => IO.eprintln s!"[runtime] no such agent: {agentId}"

/-- Snapshot every agent's current state — used by the TUI / web view. -/
def snapshot (rt : RuntimeState) : IO (List AgentState) := do
  let pairs ← rt.agents.get
  pairs.mapM (fun (_, h) => h.get)

/-- Inject a free-text reply for an agent currently in `awaiting-user`. -/
def replyToUser (rt : RuntimeState) (agentId : String) (text : String) : IO Unit := do
  let lst ← rt.agents.get
  match lst.find? (fun (id, _) => id == agentId) with
  | some (_, h) =>
    let st ← h.get
    Zellij.submit st.agent.pane text
    h.modify (fun s => { s with status := "running", stableSec := 0 })
  | none => IO.eprintln s!"[runtime] no such agent: {agentId}"

end MetaOrchestrator.Runtime
