import LeanTea.Agent.Script
import Lean.Data.Json

/-! # LeanTea.Agent.Playbook — named routine with precondition + reward

A **playbook** is a `LeanTea.Agent.Script` plus the metadata a
conductor needs to pick it over its siblings: which screen it
applies to, how much reward we expect, how long it's allowed to
run.

Playbooks are the unit the conductor schedules. The conductor:

  1. observes the current screen
  2. picks a playbook whose `pre.whenScreen` matches (via a bandit
     over `estReward` + recorded stats)
  3. runs its script via the MCP orchestrator
  4. records the outcome

Persistence: one JSON file per playbook under
`<storeDir>/playbooks/<id>.json`. Stats live in a sibling
`stats.json` so a conductor restart never loses the bandit history. -/

namespace LeanTea.Agent.Playbook

open Lean (Json)

/-! ## Precondition

For v1 a precondition is just a tag match on the observed screen.
`"*"` matches anything. We keep this trivial on purpose — the
LLM-driven escalation handler is what handles complex conditions. -/

structure Precondition where
  /-- Screen tag the playbook expects. Glob with `*` only — same
      shape as `LeanTea.Llm.Policy`. -/
  whenScreen : String
  deriving Inhabited, Repr

private partial def globMatch (pat str : String) : Bool :=
  go pat.toList str.toList
where
  go : List Char → List Char → Bool
    | [],         []        => true
    | [],         _         => false
    | '*' :: ps,  []        => go ps []
    | '*' :: ps,  s@(_::ss) => go ps s || go ('*' :: ps) ss
    | p :: ps,    c :: cs   => p == c && go ps cs
    | _ :: _,     []        => false

def Precondition.matches (p : Precondition) (screen : String) : Bool :=
  globMatch p.whenScreen screen

/-! ## Playbook -/

structure Playbook where
  /-- File-stem ID (e.g. `daily_quest_easy`). Unique per store. -/
  id          : String
  /-- Human-readable name shown in the dashboard. -/
  name        : String := ""
  description : String := ""
  pre         : Precondition
  /-- A priori expected reward; the bandit uses this as the
      optimistic prior before any runs are recorded. -/
  estReward   : Float := 1.0
  /-- Cap on consecutive runs of THIS same playbook. 0 = unlimited.
      Use to avoid greedy loops where the agent just keeps re-running
      the highest-reward routine and starves the rest. -/
  maxBurst    : Nat := 0
  /-- Hard run timeout in ms. -/
  timeoutMs   : Nat := 60000
  /-- Disable without deleting — easy way to bench a flaky playbook. -/
  enabled     : Bool := true
  /-- The script body. -/
  script      : LeanTea.Agent.Script.Script
  deriving Inhabited

/-! ## Stats — what the bandit accumulates -/

structure Stats where
  runs        : Nat   := 0
  wins        : Nat   := 0
  losses      : Nat   := 0
  /-- Sum of `RunOutcome.reward` across all runs. -/
  totalReward : Float := 0.0
  /-- monoMsNow of last execution. Used by the dashboard "last run". -/
  lastRunMs   : Nat   := 0
  /-- Last 20 outcomes — true = success, false = failure. The
      dashboard uses this for a sparkline-style trend. -/
  recentWins  : Array Bool := #[]
  deriving Inhabited, Repr

def Stats.avgReward (s : Stats) : Float :=
  if s.runs == 0 then 0.0 else s.totalReward / s.runs.toFloat

def Stats.winRate (s : Stats) : Float :=
  if s.runs == 0 then 0.0 else s.wins.toFloat / s.runs.toFloat

/-- Append one outcome, capping `recentWins` at 20. -/
def Stats.record (s : Stats) (success : Bool) (reward : Float) (now : Nat) : Stats :=
  let w := s.recentWins.push success
  let w := if w.size > 20 then w.extract (w.size - 20) w.size else w
  { runs        := s.runs + 1,
    wins        := s.wins + (if success then 1 else 0),
    losses      := s.losses + (if success then 0 else 1),
    totalReward := s.totalReward + reward,
    lastRunMs   := now,
    recentWins  := w }

/-! ## JSON codec -/

private def precondToJson (p : Precondition) : Json :=
  Json.mkObj [("whenScreen", Json.str p.whenScreen)]

private def precondFromJson (j : Json) : Except String Precondition := do
  let w ← (j.getObjVal? "whenScreen").bind (·.getStr?)
  return { whenScreen := w }

def Playbook.toJson (p : Playbook) : Json :=
  Json.mkObj [
    ("id",          Json.str p.id),
    ("name",        Json.str p.name),
    ("description", Json.str p.description),
    ("pre",         precondToJson p.pre),
    ("estReward",   Json.str (toString p.estReward)),
    ("maxBurst",    Json.num (Int.ofNat p.maxBurst)),
    ("timeoutMs",   Json.num (Int.ofNat p.timeoutMs)),
    ("enabled",     Json.bool p.enabled),
    ("script",      p.script.toJson)
  ]

private def getStr (j : Json) (k : String) : Except String String :=
  match j.getObjVal? k with
  | .ok v    => v.getStr?
  | .error e => .error e

private def getStrOpt (j : Json) (k : String) (default : String := "") : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD default

private def getNatOpt (j : Json) (k : String) (default : Nat := 0) : Nat :=
  match j.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

private def getBoolOpt (j : Json) (k : String) (default : Bool := false) : Bool :=
  match j.getObjVal? k with
  | .ok (.bool b) => b
  | _             => default

/-- Parse a decimal like `1.5` or `-0.25` to a Float. Tiny — only
    handles the shapes we actually serialise. Returns `default` on
    anything weird. -/
private def parseFloat (s : String) (default : Float := 0.0) : Float :=
  let s := s.trimAscii.toString
  if s.isEmpty then default
  else
    let (neg, body) :=
      if s.startsWith "-" then (true, (s.drop 1).toString)
      else (false, s)
    match body.splitOn "." with
    | [whole]      =>
      match whole.toNat? with
      | some n => (if neg then -1.0 else 1.0) * n.toFloat
      | none   => default
    | [whole, frac] =>
      let wn := whole.toNat?.getD 0
      let fn := frac.toNat?.getD 0
      let denom : Float := (10.0 : Float) ^ frac.length.toFloat
      let v := wn.toFloat + (fn.toFloat / denom)
      if neg then -v else v
    | _ => default

private def getFloatOpt (j : Json) (k : String) (default : Float := 0.0) : Float :=
  /- Float can land here as either a JSON number (e.g. `10.0`) or a
     string (we serialise our own floats as strings to dodge JsonNumber
     quirks). For the number case we round-trip through `toString` then
     `parseFloat` so mantissa/exponent are handled correctly. -/
  match j.getObjVal? k with
  | .ok (.str s) => parseFloat s default
  | .ok (.num n) => parseFloat (toString n) default
  | _            => default

def Playbook.fromJson (j : Json) : Except String Playbook := do
  let id  ← getStr j "id"
  let pre ← (j.getObjVal? "pre").bind precondFromJson
  let scriptJ ← (j.getObjVal? "script")
  let script ← LeanTea.Agent.Script.Script.fromJson scriptJ
  return {
    id,
    name        := getStrOpt j "name" id,
    description := getStrOpt j "description",
    pre,
    estReward   := getFloatOpt j "estReward" 1.0,
    maxBurst    := getNatOpt j "maxBurst",
    timeoutMs   := getNatOpt j "timeoutMs" 60000,
    enabled     := getBoolOpt j "enabled" true,
    script
  }

def Stats.toJson (s : Stats) : Json :=
  Json.mkObj [
    ("runs",        Json.num (Int.ofNat s.runs)),
    ("wins",        Json.num (Int.ofNat s.wins)),
    ("losses",      Json.num (Int.ofNat s.losses)),
    ("totalReward", Json.str (toString s.totalReward)),
    ("lastRunMs",   Json.num (Int.ofNat s.lastRunMs)),
    ("recentWins",  Json.arr (s.recentWins.map Json.bool))
  ]

def Stats.fromJson (j : Json) : Stats :=
  let recent :=
    match (j.getObjVal? "recentWins").toOption.bind (·.getArr?.toOption) with
    | some arr => arr.filterMap fun v =>
      match v with | .bool b => some b | _ => none
    | none => #[]
  {
    runs        := getNatOpt j "runs",
    wins        := getNatOpt j "wins",
    losses      := getNatOpt j "losses",
    totalReward := getFloatOpt j "totalReward",
    lastRunMs   := getNatOpt j "lastRunMs",
    recentWins  := recent
  }

/-! ## Disk -/

private def playbooksDir (storeDir : String) : String := s!"{storeDir}/playbooks"
private def statsPath (storeDir : String) : String := s!"{storeDir}/stats.json"

private def ensureDir (dir : String) : IO Unit := do
  unless (← System.FilePath.pathExists dir) do
    IO.FS.createDirAll dir

/-- Persist a single playbook. -/
def Playbook.save (storeDir : String) (p : Playbook) : IO Unit := do
  let dir := playbooksDir storeDir
  ensureDir dir
  IO.FS.writeFile s!"{dir}/{p.id}.json" p.toJson.pretty

/-- Load a single playbook by id. -/
def loadPlaybook (storeDir id : String) : IO (Option Playbook) := do
  let path := s!"{playbooksDir storeDir}/{id}.json"
  unless ← System.FilePath.pathExists path do return none
  try
    let src ← IO.FS.readFile path
    match Json.parse src with
    | .error _ => return none
    | .ok j    =>
      match Playbook.fromJson j with
      | .ok p    => return some p
      | .error _ => return none
  catch _ => return none

/-- Load every playbook from disk. -/
def listPlaybooks (storeDir : String) : IO (Array Playbook) := do
  let dir := playbooksDir storeDir
  ensureDir dir
  let entries ← System.FilePath.readDir dir
  let mut acc : Array Playbook := #[]
  for e in entries do
    if e.fileName.endsWith ".json" then
      try
        let src ← IO.FS.readFile e.path.toString
        match Json.parse src with
        | .ok j =>
          match Playbook.fromJson j with
          | .ok p    => acc := acc.push p
          | .error _ => pure ()
        | .error _ => pure ()
      catch _ => pure ()
  return acc

/-- Delete a playbook by id. Returns `true` if a file was removed. -/
def deletePlaybook (storeDir id : String) : IO Bool := do
  let path := s!"{playbooksDir storeDir}/{id}.json"
  if ← System.FilePath.pathExists path then
    IO.FS.removeFile path
    return true
  else
    return false

/-! ## Stats persistence (single file shared by all playbooks) -/

def loadAllStats (storeDir : String) : IO (Std.HashMap String Stats) := do
  let path := statsPath storeDir
  unless ← System.FilePath.pathExists path do return ∅
  try
    let src ← IO.FS.readFile path
    match Json.parse src with
    | .error _ => return ∅
    | .ok j =>
      match j with
      | .obj kvs =>
        let mut m : Std.HashMap String Stats := ∅
        for (k, v) in kvs.toList do
          m := m.insert k (Stats.fromJson v)
        return m
      | _ => return ∅
  catch _ => return ∅

def saveAllStats (storeDir : String) (m : Std.HashMap String Stats) : IO Unit := do
  ensureDir storeDir
  let kvs := m.toList.map fun (k, v) => (k, Stats.toJson v)
  IO.FS.writeFile (statsPath storeDir) (Json.mkObj kvs).pretty

end LeanTea.Agent.Playbook
