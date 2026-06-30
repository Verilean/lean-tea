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
The `Director`'s `Memo` history is the in-process scrollback we ship
back to Gemini on the next decision.

This is the minimum-viable shape: single pane, single agent, single
Gemini call per stall. Multi-agent / parallel monitoring is a
follow-up — the cleanly-separated `Zellij` and `Director` modules
mean adding a second agent is "another loop, same Director instance".

## CLI

  meta_orchestrator \\
    --pane PANE_ID            # zellij pane id (e.g. terminal_3)
    --goal "PROJECT GOAL"     # one-liner shipped on every prompt
    --stall-secs N            # default 30 — how long no-change = idle
    --poll-secs N             # default 5 — how often to dump-screen
    --log FILE                # default ./meta_orchestrator.jsonl
    [--dry-run]               # print decisions instead of writing back -/

open MetaOrchestrator

private structure Args where
  pane     : String := ""
  goal     : String := ""
  stallSec : Nat := 30
  pollSec  : Nat := 5
  log      : String := "meta_orchestrator.jsonl"
  dryRun   : Bool := false

private partial def parseArgs : List String → Args → Args
  | "--pane"       :: v :: rest, a => parseArgs rest { a with pane := v }
  | "--goal"       :: v :: rest, a => parseArgs rest { a with goal := v }
  | "--stall-secs" :: v :: rest, a => parseArgs rest { a with stallSec := v.toNat?.getD 30 }
  | "--poll-secs"  :: v :: rest, a => parseArgs rest { a with pollSec := v.toNat?.getD 5 }
  | "--log"        :: v :: rest, a => parseArgs rest { a with log := v }
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

open LeanTea.Cloud.Gemini in
partial def loop (cfg : Config) (a : Args)
    (memos : List Director.Memo) (lastHash : UInt64) (stableSec : Nat)
    : IO Unit := do
  IO.sleep (a.pollSec.toUInt32 * 1000)
  let dump ← Zellij.dumpScreen a.pane
  let h := Zellij.cheapHash dump
  let nextStable :=
    if h == lastHash then stableSec + a.pollSec
    else 0
  if h != lastHash then
    IO.eprintln s!"[poll] pane changed (stable: 0s)"
    loop cfg a memos h 0
  else if nextStable < a.stallSec then
    IO.eprintln s!"[poll] pane stable for {nextStable}s (threshold {a.stallSec}s)"
    loop cfg a memos h nextStable
  else
    -- Stalled: wake the Director.
    IO.eprintln s!"[stall] pane stable for {nextStable}s — asking Gemini"
    let verdict ← try Director.decide cfg a.goal memos dump
                  catch e => do
                    IO.eprintln s!"[err] Gemini call failed: {e}"
                    pure { decision := .continue, reasoning := s!"err: {e}" }
    let actName := decisionName verdict.decision
    let text := decisionText verdict.decision
    IO.eprintln s!"[director] {actName}: {verdict.reasoning}"
    if !text.isEmpty then IO.eprintln s!"           → {text}"
    let ts ← isoNow
    logLine a.log <| Lean.Json.mkObj [
      ("ts", Lean.Json.str ts),
      ("action", Lean.Json.str actName),
      ("reasoning", Lean.Json.str verdict.reasoning),
      ("text", Lean.Json.str text),
      ("dryRun", Lean.Json.bool a.dryRun)
    ]
    let memo : Director.Memo := {
      ts := ts, action := actName, text := text, afterSummary := ""
    }
    let memos' := memo :: (memos.take 9)  -- keep last ~10
    match verdict.decision with
    | .instruct s =>
      if a.dryRun then
        IO.eprintln "[dry] (would write to pane)"
      else
        Zellij.submit a.pane s
      loop cfg a memos' h 0  -- reset stall counter after intervention
    | .askUser q =>
      IO.println s!"\n=== askUser ===\n{q}\n==============="
      IO.println "Type a reply and press Enter (sent to the agent), or just Enter to skip:"
      let line := (← (← IO.getStdin).getLine).trim
      if !line.isEmpty && !a.dryRun then Zellij.submit a.pane line
      loop cfg a memos' h 0
    | .continue =>
      loop cfg a memos h nextStable  -- keep waiting; don't reset

def main (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  if a.pane.isEmpty then
    IO.eprintln "usage: meta_orchestrator --pane PANE_ID --goal 'TEXT' [--stall-secs N] [--poll-secs N] [--log FILE] [--dry-run]"
    IO.eprintln "  PANE_ID examples: terminal_3, plugin_2, or bare integer."
    IO.eprintln "  Find ids in zellij with: zellij action dump-layout"
    return
  if a.goal.isEmpty then
    IO.eprintln "meta_orchestrator: --goal is required (the long-term objective shipped to Gemini)"
    return
  let cfg ← LeanTea.Cloud.Gemini.Config.fromEnv!
  IO.println s!"meta_orchestrator: pane={a.pane} goal={a.goal}"
  IO.println s!"  poll={a.pollSec}s stall_threshold={a.stallSec}s log={a.log} dry_run={a.dryRun}"
  IO.println s!"  gemini_model={cfg.model}"
  loop cfg a [] 0 0
