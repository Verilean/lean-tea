import MetaOrchestrator.Runtime
import MetaOrchestrator.Config
import LeanTea.Cloud.Gemini

/-! # examples/MetaOrchestrator/Main.lean — controller

Starts every enabled agent from the config file, spawns one polling
task per agent (each its own zellij-pane watcher + Gemini caller),
then reads slash commands on stdin to add/remove agents at runtime.

This commit is CLI text-mode only — a richer TUI rendering of agent
state lives in the next commit. The data layer (Config + Runtime) is
already structured for it: `Runtime.snapshot` returns a snapshot list
the TUI / web layer just paints.

## Slash commands

  /list                              — show every agent and its status
  /add ID PANE GOAL...               — register and start a new agent
  /stop ID                           — cooperatively stop an agent
  /start ID                          — re-enable a stopped/disabled agent
  /remove ID                         — drop the agent from the config
  /reply ID TEXT...                  — send TEXT to the named pane (use for `awaiting-user`)
  /save [PATH]                       — write the config to PATH (default: --config path)
  /load PATH                         — load config from PATH (replaces in-memory config; running agents keep going until /stop)
  /quit                              — stop everything and exit

## Config file shape

  {
    "agents": [
      {"id": "kernel-bwd", "pane": "terminal_18", "goal": "...",
       "stallSec": 45, "pollSec": 5, "enabled": true},
      ...
    ],
    "logDir": "./logs",
    "geminiModel": "gemini-2.5-pro"
  }
-/

open MetaOrchestrator

private structure CliArgs where
  configPath : String := "meta_orchestrator.config.json"

private partial def parseCli : List String → CliArgs → CliArgs
  | "--config" :: v :: rest, a => parseCli rest { a with configPath := v }
  | _ :: rest,               a => parseCli rest a
  | [],                      a => a

/-! ## Slash command dispatch -/

private def rightpad (s : String) (n : Nat) : String :=
  if s.length < n then
    s ++ String.ofList (List.replicate (n - s.length) ' ')
  else s

private def fmtAgentLine (st : Runtime.AgentState) : String :=
  let id := rightpad st.agent.id 16
  let status := rightpad st.status 14
  let poll := s!"poll={st.pollCount}"
  let recent := if st.lastDecision.isEmpty then "(no decisions yet)"
                else (st.lastDecision.take 80).toString
  s!"{id}  {status}  pane={st.agent.pane}  {poll}  {recent}"

private def cmdList (rt : Runtime.RuntimeState) : IO Unit := do
  let snaps ← Runtime.snapshot rt
  if snaps.isEmpty then
    IO.println "(no agents registered — try /add)"
  else
    for st in snaps do IO.println (fmtAgentLine st)

private def cmdAdd (rt : Runtime.RuntimeState)
    (id : String) (pane : String) (goal : String) : IO Unit := do
  if id.isEmpty || pane.isEmpty || goal.isEmpty then
    IO.println "usage: /add ID PANE GOAL..."
    return
  let agent : Config.ManagedAgent := { id, pane, goal }
  rt.config.modify (fun c => c.addAgent agent)
  Runtime.start rt agent

private def cmdStop (rt : Runtime.RuntimeState) (id : String) : IO Unit :=
  Runtime.stop rt id

private def cmdStart (rt : Runtime.RuntimeState) (id : String) : IO Unit := do
  let cfg ← rt.config.get
  match cfg.findAgent? id with
  | some a => Runtime.start rt { a with enabled := true }
  | none => IO.println s!"no agent '{id}' in config — try /add first"

private def cmdRemove (rt : Runtime.RuntimeState) (id : String) : IO Unit := do
  Runtime.stop rt id
  rt.config.modify (fun c => c.removeAgent id)
  IO.println s!"[runtime] removed '{id}' from config (running task will exit on next poll)"

private def cmdReply (rt : Runtime.RuntimeState) (id : String) (text : String) : IO Unit :=
  Runtime.replyToUser rt id text

private def cmdSave (rt : Runtime.RuntimeState) (path? : Option String) : IO Unit := do
  let path := path?.getD rt.configPath
  let cfg ← rt.config.get
  cfg.save path
  IO.println s!"[runtime] saved config → {path}"

private def cmdLoad (rt : Runtime.RuntimeState) (path : String) : IO Unit := do
  let loaded ← Config.Config.load path
  rt.config.modify (fun _ => loaded)
  IO.println s!"[runtime] loaded config from {path} (use /start ID to spawn agents)"

private def parseSlash (line : String) : Option (String × List String) :=
  if !line.startsWith "/" then none
  else
    let body := (line.drop 1).toString
    let parts := body.splitOn " " |>.filter (·.length > 0)
    match parts with
    | [] => none
    | cmd :: rest => some (cmd, rest)

private partial def repl (rt : Runtime.RuntimeState) : IO Unit := do
  IO.print "> "
  (← IO.getStdout).flush
  let stdin ← IO.getStdin
  let line := (← stdin.getLine).trim
  if line.isEmpty then repl rt
  else
    match parseSlash line with
    | none =>
      IO.println "(commands start with /; try /list /add /stop /start /remove /reply /save /load /quit)"
      repl rt
    | some ("list", _) => cmdList rt; repl rt
    | some ("add", id :: pane :: goalParts) =>
      cmdAdd rt id pane (String.intercalate " " goalParts); repl rt
    | some ("add", _) => IO.println "usage: /add ID PANE GOAL..."; repl rt
    | some ("stop", [id]) => cmdStop rt id; repl rt
    | some ("start", [id]) => cmdStart rt id; repl rt
    | some ("remove", [id]) => cmdRemove rt id; repl rt
    | some ("reply", id :: textParts) =>
      cmdReply rt id (String.intercalate " " textParts); repl rt
    | some ("reply", _) => IO.println "usage: /reply ID TEXT..."; repl rt
    | some ("save", []) => cmdSave rt none; repl rt
    | some ("save", [p]) => cmdSave rt (some p); repl rt
    | some ("load", [p]) => cmdLoad rt p; repl rt
    | some ("load", _) => IO.println "usage: /load PATH"; repl rt
    | some ("quit", _) =>
      let snaps ← Runtime.snapshot rt
      for st in snaps do Runtime.stop rt st.agent.id
      cmdSave rt none
      IO.println "bye"
    | some (cmd, _) => IO.println s!"unknown command: /{cmd}"; repl rt

def main (argv : List String) : IO Unit := do
  let cli := parseCli argv {}
  IO.eprintln s!"meta_orchestrator: loading config from {cli.configPath}"
  let cfg ← Config.Config.load cli.configPath
  -- Ensure log dir exists.
  if !(← System.FilePath.pathExists cfg.logDir) then
    IO.FS.createDirAll cfg.logDir
  let geminiCfg ← LeanTea.Cloud.Gemini.Config.fromEnv!
  let agentsRef ← IO.mkRef ([] : List (String × Runtime.AgentHandle))
  let configRef ← IO.mkRef cfg
  let rt : Runtime.RuntimeState := {
    cfg := { geminiCfg with model := cfg.geminiModel },
    agents := agentsRef,
    configPath := cli.configPath,
    config := configRef
  }
  -- Spawn every enabled agent.
  for agent in cfg.agents do
    if agent.enabled then Runtime.start rt agent
  IO.eprintln s!"meta_orchestrator: {cfg.agents.length} agent(s) in config, log_dir={cfg.logDir}"
  IO.eprintln "type /list, /add, /stop, /start, /remove, /reply, /save, /load, /quit"
  repl rt
