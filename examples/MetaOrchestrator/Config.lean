import Lean.Data.Json

/-! # examples/MetaOrchestrator/Config.lean — persisted config + runtime state

Two layers:

* `ManagedAgent` — declarative (persisted as JSON). Adding / removing
  Claude Code pane is `Config.agents.push` / `filter`. Hand-editable.

* `Config` — wraps the agent list plus orchestrator-wide knobs
  (`logDir`, `geminiModel`, default poll/stall). Read on startup,
  re-written on `/save`. SQLite is overkill for this size — single
  JSON file lets you `git diff` your orchestrator setup.

Memos themselves stay in their own JSONL file (`memoLog`) per agent,
because they're append-only and we don't want to rewrite them on
every config change.
-/

namespace MetaOrchestrator.Config

open Lean (Json)

/-- One managed Claude Code instance. The runtime polling loop reads
    this and never mutates it — runtime state (status, lastDecision,
    paneHash) lives in a separate `IO.Ref` keyed by `id`. -/
structure ManagedAgent where
  id        : String   -- short slug ("kernel-bwd"); doubles as session id
  pane      : String   -- zellij pane id
  goal      : String   -- shipped to Gemini on every decision
  stallSec  : Nat := 30
  pollSec   : Nat := 5
  enabled   : Bool := true  -- /remove just flips this; full delete via /forget
  deriving Inhabited, Repr

structure Config where
  agents      : List ManagedAgent := []
  logDir      : String := "."
  geminiModel : String := "gemini-2.5-pro"
  deriving Inhabited

/-! ## JSON codec — hand-rolled because the record is small and we want
    forgiving decode (missing fields → defaults). -/

private def jstr (j : Json) (k : String) (default : String := "") : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD default

private def jnat (j : Json) (k : String) (default : Nat := 0) : Nat :=
  match (j.getObjVal? k).toOption.bind (·.getNat?.toOption) with
  | some n => n
  | none => default

private def jbool (j : Json) (k : String) (default : Bool := true) : Bool :=
  match (j.getObjVal? k).toOption.bind (·.getBool?.toOption) with
  | some b => b
  | none => default

def ManagedAgent.toJson (m : ManagedAgent) : Json := Json.mkObj [
  ("id", Json.str m.id),
  ("pane", Json.str m.pane),
  ("goal", Json.str m.goal),
  ("stallSec", Json.num m.stallSec),
  ("pollSec", Json.num m.pollSec),
  ("enabled", Json.bool m.enabled)
]

def ManagedAgent.ofJson? (j : Json) : Option ManagedAgent :=
  let id := jstr j "id"
  let pane := jstr j "pane"
  let goal := jstr j "goal"
  if id.isEmpty || pane.isEmpty || goal.isEmpty then none
  else some {
    id, pane, goal,
    stallSec := jnat j "stallSec" 30,
    pollSec  := jnat j "pollSec" 5,
    enabled  := jbool j "enabled" true
  }

def Config.toJson (c : Config) : Json := Json.mkObj [
  ("agents", Json.arr (c.agents.map ManagedAgent.toJson |>.toArray)),
  ("logDir", Json.str c.logDir),
  ("geminiModel", Json.str c.geminiModel)
]

def Config.ofJson? (j : Json) : Config :=
  let agents :=
    match (j.getObjVal? "agents").toOption.bind (·.getArr?.toOption) with
    | some a => a.toList.filterMap ManagedAgent.ofJson?
    | none => []
  {
    agents,
    logDir      := jstr j "logDir" ".",
    geminiModel := jstr j "geminiModel" "gemini-2.5-pro"
  }

/-! ## File I/O -/

def Config.load (path : String) : IO Config := do
  if !(← System.FilePath.pathExists path) then
    IO.eprintln s!"[config] {path} not found — starting with an empty config"
    return {}
  let body ← IO.FS.readFile path
  match Json.parse body with
  | .ok j => return Config.ofJson? j
  | .error e =>
    IO.eprintln s!"[config] failed to parse {path}: {e} — using empty config"
    return {}

def Config.save (c : Config) (path : String) : IO Unit := do
  let pretty := (Config.toJson c).pretty
  IO.FS.writeFile path (pretty ++ "\n")

/-! ## Manipulation -/

def Config.addAgent (c : Config) (m : ManagedAgent) : Config :=
  -- Replace existing entry with the same id, else append.
  let filtered := c.agents.filter (fun x => x.id != m.id)
  { c with agents := filtered ++ [m] }

def Config.removeAgent (c : Config) (id : String) : Config :=
  { c with agents := c.agents.filter (fun x => x.id != id) }

def Config.findAgent? (c : Config) (id : String) : Option ManagedAgent :=
  c.agents.find? (fun x => x.id == id)

end MetaOrchestrator.Config
