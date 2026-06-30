import MetaOrchestrator.Zellij
import MetaOrchestrator.Director
import LeanTea.Cloud.Gemini

/-! # examples/MetaOrchestrator/Main.lean — main loop

Polls the target zellij pane, hashes the visible viewport, and only
wakes Gemini when:

  * the hash has been stable for `--stall-secs` seconds (no output
    movement, i.e. agent is idle), OR
  * the user pressed Enter on stdin (manual nudge — useful while
    iterating on the system prompt).

Each Gemini decision lands as an audit line in `--log FILE` (JSONL).
Each `Director.Memo` (the structured-for-LLM-replay form) lands as a
separate JSONL line in `--memo-log FILE`. On startup the loop replays
the memo log for the current session and continues with that context,
so a `--resume` (or just restarting with the same `--goal`) picks up
where Gemini left off.

## Session id

Memos are tagged with a session id so the same memo log can serve
several concurrent / consecutive orchestrators without their memories
cross-contaminating. By default we derive a stable id from the goal
text (FNV-1a, first 8 hex). `--session FOO` pins it explicitly when
you want to fork (same goal, two trial paths).

## CLI

  meta_orchestrator \\
    --pane PANE_ID            # zellij pane id (e.g. terminal_3)
    --goal "PROJECT GOAL"     # one-liner shipped on every prompt
    --stall-secs N            # default 30 — how long no-change = idle
    --poll-secs N             # default 5 — how often to dump-screen
    --log FILE                # default ./meta_orchestrator.jsonl   (decision audit)
    --memo-log FILE           # default ./meta_orchestrator.memos.jsonl (resume source)
    --session ID              # override the auto-derived session id
    [--fresh]                 # ignore the memo log on startup
    [--dry-run]               # print decisions instead of writing back -/

open MetaOrchestrator

private structure Args where
  pane     : String := ""
  goal     : String := ""
  stallSec : Nat := 30
  pollSec  : Nat := 5
  log      : String := "meta_orchestrator.jsonl"
  memoLog  : String := "meta_orchestrator.memos.jsonl"
  session  : String := ""    -- "" = derive from goal
  fresh    : Bool := false
  dryRun   : Bool := false

private partial def parseArgs : List String → Args → Args
  | "--pane"       :: v :: rest, a => parseArgs rest { a with pane := v }
  | "--goal"       :: v :: rest, a => parseArgs rest { a with goal := v }
  | "--stall-secs" :: v :: rest, a => parseArgs rest { a with stallSec := v.toNat?.getD 30 }
  | "--poll-secs"  :: v :: rest, a => parseArgs rest { a with pollSec := v.toNat?.getD 5 }
  | "--log"        :: v :: rest, a => parseArgs rest { a with log := v }
  | "--memo-log"   :: v :: rest, a => parseArgs rest { a with memoLog := v }
  | "--session"    :: v :: rest, a => parseArgs rest { a with session := v }
  | "--fresh"      :: rest,      a => parseArgs rest { a with fresh := true }
  | "--dry-run"    :: rest,      a => parseArgs rest { a with dryRun := true }
  | _              :: rest,      a => parseArgs rest a
  | [],                          a => a

private def isoNow : IO String := do
  let now ← IO.monoMsNow
  -- ISO-ish; we don't need wall-clock precision for log correlation.
  return s!"t+{now}ms"

private def logLine (path : String) (j : Lean.Json) : IO Unit := do
  let line := j.compress ++ "\n"
  let h ← IO.FS.Handle.mk path .append
  h.putStr line
  h.flush

private def decisionName : Director.Decision → String
  | .continue    => "continue"
  | .instruct _  => "instruct"
  | .askUser _   => "ask_user"

private def decisionText : Director.Decision → String
  | .continue       => ""
  | .instruct s     => s
  | .askUser s      => s

/-- 8 hex chars derived from the FNV-1a hash of `s`. Used as a default
    session id derived from the goal so identical goals share memos. -/
private def sessionFromGoal (goal : String) : String :=
  let h := Zellij.cheapHash goal
  let hex := Nat.toDigits 16 h.toNat
  let s := String.mk hex
  -- right-trim to 8 chars (or left-pad with 0 if shorter)
  if s.length ≥ 8 then (s.drop (s.length - 8)).toString
  else String.ofList (List.replicate (8 - s.length) '0') ++ s

/-- Read the memo log, parse one JSON object per line, keep only memos
    that match `sessionId`, return the last `keep` of them in
    insertion order (i.e. most recent at the head, which matches the
    in-memory shape the loop uses). -/
private def loadMemos (memoLogPath : String) (sessionId : String)
    (keep : Nat) : IO (List Director.Memo) := do
  if !(← System.FilePath.pathExists memoLogPath) then return []
  let contents ← IO.FS.readFile memoLogPath
  let lines := contents.splitOn "\n" |>.filter (fun l => !l.trim.isEmpty)
  let mut acc : Array Director.Memo := #[]
  for line in lines do
    match Lean.Json.parse line with
    | .ok j =>
      match Director.Memo.ofJson? j with
      | some m =>
        if m.sessionId == sessionId then acc := acc.push m
      | none => pure ()
    | .error _ => pure ()
  -- Most recent at the head — take the last `keep`, then reverse.
  let total := acc.size
  let take := if total > keep then keep else total
  let recent := acc.toList.drop (total - take)
  return recent.reverse

/-- Heuristic 1-line summary of "what happened between the previous
    decision and now". Just compares the pane hash. -/
private def diffSummary (prevHash : UInt64) (curHash : UInt64) : String :=
  if prevHash == curHash then "(no change in pane)"
  else "(pane changed)"

partial def loop (cfg : LeanTea.Cloud.Gemini.Config) (a : Args)
    (memos : List Director.Memo) (lastHash : UInt64) (stableSec : Nat)
    (preDecisionHash : UInt64) : IO Unit := do
  IO.sleep (a.pollSec.toUInt32 * 1000)
  let dump ← Zellij.dumpScreen a.pane
  let h := Zellij.cheapHash dump
  let nextStable :=
    if h == lastHash then stableSec + a.pollSec
    else 0
  if h != lastHash then
    IO.eprintln s!"[poll] pane changed (stable: 0s)"
    loop cfg a memos h 0 preDecisionHash
  else if nextStable < a.stallSec then
    IO.eprintln s!"[poll] pane stable for {nextStable}s (threshold {a.stallSec}s)"
    loop cfg a memos h nextStable preDecisionHash
  else
    -- Stalled: backfill the previous memo's afterSummary, then wake the Director.
    let memosWithSummary :=
      match memos with
      | [] => memos
      | m :: rest =>
        if m.afterSummary.isEmpty then
          ({ m with afterSummary := diffSummary preDecisionHash h }) :: rest
        else memos
    IO.eprintln s!"[stall] pane stable for {nextStable}s — asking Gemini"
    let verdict ← try Director.decide cfg a.goal memosWithSummary dump
                  catch e => do
                    IO.eprintln s!"[err] Gemini call failed: {e}"
                    pure { decision := .continue, reasoning := s!"err: {e}" }
    let actName := decisionName verdict.decision
    let text := decisionText verdict.decision
    IO.eprintln s!"[director] {actName}: {verdict.reasoning}"
    if !text.isEmpty then IO.eprintln s!"           → {text}"
    let ts ← isoNow
    -- Decision audit log (free-form, includes reasoning).
    logLine a.log <| Lean.Json.mkObj [
      ("sessionId", Lean.Json.str a.session),
      ("ts", Lean.Json.str ts),
      ("action", Lean.Json.str actName),
      ("reasoning", Lean.Json.str verdict.reasoning),
      ("text", Lean.Json.str text),
      ("dryRun", Lean.Json.bool a.dryRun)
    ]
    -- Memo log (structured, replayable on --resume).
    let memo : Director.Memo := {
      sessionId := a.session, ts := ts,
      action := actName, text := text, afterSummary := ""
    }
    logLine a.memoLog memo.toJson
    let memos' := memo :: (memosWithSummary.take 9)
    match verdict.decision with
    | .instruct s =>
      if a.dryRun then
        IO.eprintln "[dry] (would write to pane)"
      else
        Zellij.submit a.pane s
      loop cfg a memos' h 0 h
    | .askUser q =>
      IO.println s!"\n=== askUser ===\n{q}\n==============="
      IO.println "Type a reply and press Enter (sent to the agent), or just Enter to skip:"
      let line := (← (← IO.getStdin).getLine).trim
      if !line.isEmpty && !a.dryRun then Zellij.submit a.pane line
      loop cfg a memos' h 0 h
    | .continue =>
      loop cfg a memosWithSummary h nextStable preDecisionHash

def main (argv : List String) : IO Unit := do
  let mut a := parseArgs argv {}
  if a.pane.isEmpty then
    IO.eprintln "usage: meta_orchestrator --pane PANE_ID --goal 'TEXT' [--stall-secs N] [--poll-secs N] [--log FILE] [--memo-log FILE] [--session ID] [--fresh] [--dry-run]"
    IO.eprintln "  PANE_ID examples: terminal_3, plugin_2, or bare integer."
    IO.eprintln "  Find ids in zellij with: zellij action dump-layout"
    return
  if a.goal.isEmpty then
    IO.eprintln "meta_orchestrator: --goal is required (the long-term objective shipped to Gemini)"
    return
  if a.session.isEmpty then
    a := { a with session := sessionFromGoal a.goal }
  let cfg ← LeanTea.Cloud.Gemini.Config.fromEnv!
  -- Resume memos for this session unless --fresh.
  let memos ← if a.fresh then pure [] else loadMemos a.memoLog a.session 10
  -- Banner on stderr so logs land in real-time (stdout is fully buffered
  -- to a pipe; stderr is line-buffered).
  IO.eprintln s!"meta_orchestrator: pane={a.pane} session={a.session}"
  IO.eprintln s!"  goal: {a.goal}"
  IO.eprintln s!"  poll={a.pollSec}s stall_threshold={a.stallSec}s"
  IO.eprintln s!"  log={a.log}  memo_log={a.memoLog}  dry_run={a.dryRun}"
  IO.eprintln s!"  gemini_model={cfg.model}  resumed_memos={memos.length}"
  loop cfg a memos 0 0 0
