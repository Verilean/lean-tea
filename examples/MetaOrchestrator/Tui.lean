import MetaOrchestrator.Runtime
import MetaOrchestrator.Config
import MetaOrchestrator.Director
import MetaOrchestrator.Zellij
import LeanTea.Tui

/-! # examples/MetaOrchestrator/Tui.lean — full-screen TUI on LeanTea.Tui

Replaces the stdin REPL from the previous commit. Same slash
commands (`/list`, `/add ID PANE GOAL...`, `/stop ID`, `/start ID`,
`/remove ID`, `/reply ID TEXT...`, `/save`, `/load PATH`, `/quit`)
but rendered as a proper widget tree so:

  * agent state is always visible (id, status, poll count, last
    decision) rather than only after `/list`;
  * every keystroke re-snapshots the runtime, so status changes
    from the background polling tasks show up immediately;
  * commands still complete instantly because the LLM calls run in
    the per-agent tasks (`IO.asTask`), not in the TUI thread.

## Widget tree

```
vbox
├─ headerBar         (title + agent count)
├─ agentTable        (one row per agent, listView-ish)
├─ messagesArea      (recent command output / errors)
└─ commandBar        (prompt + input line)
```

The command bar is the only focusable widget in v0 — we don't need
Tab to move focus into the agent table until we add per-agent
actions like `<Enter> to open the memo log`. Focus lives on the
input the whole time.

Reads a keystroke, dispatches via the widget tree, folds into
state, repaints. Snapshots refresh on every keystroke — cheap
enough for a handful of agents.
-/

open LeanTea.Tui
open MetaOrchestrator

namespace MetaOrchestrator.Tui

/-! ## State + messages -/

structure TuiState where
  input     : String := ""
  snapshots : List Runtime.AgentState := []
  messages  : List String := []          -- newest first, capped at 20
  quit      : Bool := false
  deriving Inhabited

inductive TuiMsg where
  | typeChar (c : Char)
  | back
  | submit
  | quit
  deriving Inhabited

/-! ## Rendering -/

private def rightpad (s : String) (n : Nat) : String :=
  if s.length < n then s ++ String.ofList (List.replicate (n - s.length) ' ')
  else s

private def truncate (s : String) (n : Nat) : String :=
  if s.length ≤ n then s
  else (s.take (n - 1)).toString ++ "…"

private def agentRow (st : Runtime.AgentState) (colW : Nat) : String :=
  let id     := rightpad (truncate st.agent.id 14) 14
  let status := rightpad (truncate st.status 14) 14
  let poll   := rightpad s!"poll={st.pollCount}" 10
  let last   := truncate st.lastDecision (colW - 40)
  s!"{id}  {status}  {poll}  {last}"

private def headerBar (count : Nat) : Widget TuiMsg :=
  let bar := padding 0 <| withStyle { fg := .black, bg := .cyan, bold := true }
    (text s!"  meta_orchestrator — {count} agent(s)")
  { bar with prefHeight := some 1 }

private def agentTable (snaps : List Runtime.AgentState) : Widget TuiMsg :=
  if snaps.isEmpty then
    padding 1 (text "  (no agents yet — try: /add ID PANE GOAL...)"
      { dim := true })
  else
    let rows := snaps.map (fun st => text (agentRow st 100))
    border .line (padding 1 (vbox rows))

private def messagesArea (msgs : List String) : Widget TuiMsg :=
  if msgs.isEmpty then blank
  else
    let rows := (msgs.take 8).reverse.map (fun m => text s!"  {m}" { dim := true })
    { vbox rows with prefHeight := some 8 }

private def commandBar (inputStr : String) : Widget TuiMsg :=
  let prompt : Widget TuiMsg := { text "> " { fg := .cyan, bold := true } with prefWidth := some 2 }
  let inputW : Widget TuiMsg := input "cmd" inputStr {
    onChar := TuiMsg.typeChar,
    onBackspace := TuiMsg.back,
    onSubmit := TuiMsg.submit,
    onEsc := some TuiMsg.quit
  }
  let bar := border .line (hbox [prompt, inputW])
  { bar with prefHeight := some 3 }

def view (state : TuiState) : Widget TuiMsg :=
  vbox [
    headerBar state.snapshots.length,
    agentTable state.snapshots,
    messagesArea state.messages,
    commandBar state.input
  ]

/-! ## Command execution (side-effecting)

Called after the pure `TuiMsg.submit` folds into a fresh state
with an empty input. We then interpret the previous input string
as a slash command and mutate the runtime accordingly. Any output
lines are collected into the returned `List String`, which the
outer loop prepends to `state.messages`. -/

private def parseSlash (line : String) : Option (String × List String) :=
  if !line.startsWith "/" then none
  else
    let body := (line.drop 1).toString
    let parts := body.splitOn " " |>.filter (·.length > 0)
    match parts with
    | []          => none
    | cmd :: rest => some (cmd, rest)

private def isoNow' : IO String := do
  let now ← IO.monoMsNow
  return s!"t+{now}ms"

/-- Run the given command line against the runtime. Returns the
    output lines the TUI should surface. -/
def dispatchCommand (rt : Runtime.RuntimeState) (line : String) : IO (List String) := do
  let cmd? := parseSlash line
  match cmd? with
  | none => return [s!"unknown: {line}"]
  | some ("list", _) =>
    let snaps ← Runtime.snapshot rt
    return snaps.map (fun st => agentRow st 100)
  | some ("add", id :: pane :: goalParts) =>
    let goal := String.intercalate " " goalParts
    let agent : Config.ManagedAgent := { id, pane, goal }
    rt.config.modify (fun c => c.addAgent agent)
    Runtime.start rt agent
    return [s!"added '{id}' (pane={pane})"]
  | some ("add", _) => return ["usage: /add ID PANE GOAL..."]
  | some ("stop", [id]) => Runtime.stop rt id; return [s!"stop requested for '{id}'"]
  | some ("start", [id]) =>
    let cfg ← rt.config.get
    match cfg.findAgent? id with
    | some a => Runtime.start rt { a with enabled := true }; return [s!"started '{id}'"]
    | none   => return [s!"no such agent in config: '{id}'"]
  | some ("remove", [id]) =>
    Runtime.stop rt id
    rt.config.modify (fun c => c.removeAgent id)
    return [s!"removed '{id}'"]
  | some ("reply", id :: textParts) =>
    let msg := String.intercalate " " textParts
    Runtime.replyToUser rt id msg
    return [s!"replied to '{id}': {msg}"]
  | some ("review", [agentId]) => do
    let cfg ← rt.config.get
    let lst ← rt.agents.get
    match lst.find? (fun (id, _) => id == agentId) with
    | none => return [s!"[review] no such agent: '{agentId}'"]
    | some (_, h) =>
      let st ← h.get
      let memos ← Runtime.loadMemos cfg.logDir agentId 100
      let dump ← Zellij.dumpScreen st.agent.pane
      try
        let report ← Director.review cfg.reviewBackend st.agent.goal memos dump
        -- Break the review into one line per sentence so the TUI's
        -- narrow message area doesn't horizontally overflow.
        let lines := report.replace "\n" " " |>.splitOn ". "
                     |>.filter (fun s => !s.trim.isEmpty)
        return s!"[review {cfg.reviewBackend.describe}] {agentId} ({memos.length} memos):" :: lines
      catch e =>
        return [s!"[review] failed: {e}"]
  | some ("review", _) => return ["usage: /review ID"]
  | some ("save", args) =>
    let path :=
      match args with
      | [] => rt.configPath
      | p :: _ => p
    let cfg ← rt.config.get
    cfg.save path
    return [s!"saved config → {path}"]
  | some ("load", [p]) =>
    let loaded ← Config.Config.load p
    rt.config.modify (fun _ => loaded)
    return [s!"loaded {p}"]
  | some ("quit", _) => return ["(exiting)"]
  | some (c, _) => return [s!"unknown command: /{c}"]

/-! ## Loop

We deliberately don't reuse `LeanTea.Tui.App.runWith` because we
need `update` to be in `IO` (dispatch may call `Runtime.start` /
`Runtime.stop` / save the config). Instead we roll a thin loop that
mirrors `runWith` but interleaves the pure widget-key dispatch with
IO side-effects. -/

private def esc (s : String) : String := s!"\x1b[{s}"
private def altScreenOn  := esc "?1049h"
private def altScreenOff := esc "?1049l"
private def hideCursor   := esc "?25l"
private def showCursor   := esc "?25h"
private def clearScreen  := esc "2J" ++ esc "H"

private def sttyRawOn : IO Unit := do
  let _ ← IO.Process.run { cmd := "sh", args := #["-c", "stty raw -echo < /dev/tty"] }; pure ()
private def sttyRawOff : IO Unit := do
  let _ ← IO.Process.run { cmd := "sh", args := #["-c", "stty -raw echo < /dev/tty"] }; pure ()

/-- Cap the tail of a list so it doesn't grow without bound. -/
private def capList (xs : List a) (n : Nat) : List a :=
  xs.take n

partial def loop (rt : Runtime.RuntimeState) (state : TuiState) : IO Unit := do
  let stdout ← IO.getStdout
  let stdin ← IO.getStdin
  if state.quit then return
  -- Refresh snapshots on every repaint so background status changes
  -- (poll count / last decision) show up when the user is idle.
  let snaps ← Runtime.snapshot rt
  let state := { state with snapshots := snaps }
  let w := view state
  let box := w.render 120 30 false
  stdout.putStr (LeanTea.Tui.renderBoxAnsi box)
  stdout.flush
  let key ← LeanTea.Tui.readKey stdin
  match key with
  | .ctrl 'c' => return ()
  | _ =>
    match w.dispatchKey "cmd" key with
    | none => loop rt state
    | some msg =>
      let state' ← match msg with
        | .typeChar c => pure { state with input := state.input.push c }
        | .back       => pure { state with input := (state.input.dropEnd 1).toString }
        | .quit       => pure { state with quit := true }
        | .submit     => do
          let line := state.input.trim
          if line.isEmpty then pure state
          else do
            let ts ← isoNow'
            let out ← dispatchCommand rt line
            let annotated := out.map (fun s => s!"[{ts}] {s}")
            let quit := line == "/quit"
            pure { state with
              input := "",
              messages := capList (annotated ++ state.messages) 20,
              quit := quit
            }
      loop rt state'

/-- Enter TUI mode, restore the terminal on exit. -/
def run (rt : Runtime.RuntimeState) : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStr (altScreenOn ++ hideCursor ++ clearScreen)
  stdout.flush
  sttyRawOn
  try
    loop rt {}
  finally
    sttyRawOff
    stdout.putStr (showCursor ++ altScreenOff)
    stdout.flush

end MetaOrchestrator.Tui
