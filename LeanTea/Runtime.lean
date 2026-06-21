import LeanTea.Cmd
import LeanTea.Sub

namespace LeanTea

/-- An Elm-style application: pure `init / update / view`, plus a
    subscription function that exposes the events the runtime should poll. -/
structure App (Model Msg : Type) where
  init   : Model × Cmd Msg
  update : Msg → Model → Model × Cmd Msg
  view   : Model → String
  subs   : Model → Sub Msg
  /-- Optional: when `update` returns this in the model the runtime exits
      gracefully. Default: never. -/
  isDone : Model → Bool := fun _ => false

/-- ANSI: clear screen + home cursor. The runtime calls this before each
    view so the terminal acts like a tiny DOM. -/
private def clearScreen : IO Unit := do
  let h ← IO.getStdout
  h.putStr "\x1b[2J\x1b[H"
  h.flush

private def render (s : String) : IO Unit := do
  let h ← IO.getStdout
  h.putStr s
  h.putStrLn ""
  h.flush

private def runEffects (cmd : Cmd Msg) : IO (List Msg) := do
  let mut msgs : List Msg := []
  for eff in cmd.effects do
    match (← eff) with
    | some m => msgs := msgs ++ [m]
    | none   => pure ()
  return msgs

/-- Pull every message produced by the current subscription set. The
    minimal runtime collapses subscriptions to "read one stdin line";
    timers are not implemented here so we keep the code small. -/
private def collectInput (sub : Sub Msg) : IO (List Msg) := do
  let stdinHandler? := sub.handlers.find? (fun (s, _) => s == Source.stdin)
  match stdinHandler? with
  | none => return []
  | some (_, f) =>
    let stdin ← IO.getStdin
    let line ← stdin.getLine
    let trimmed := line.trimAscii.toString
    match f trimmed with
    | some m => return [m]
    | none   => return []

partial def loop (app : App Model Msg) (model : Model) (pending : List Msg) : IO Unit := do
  if app.isDone model then
    render (app.view model)
    return
  clearScreen
  render (app.view model)
  let nextMsgs ← match pending with
    | [] => collectInput (app.subs model)
    | _  => pure pending
  match nextMsgs with
  | [] => loop app model []  -- nothing happened; redraw
  | m :: rest =>
    let (model', cmd) := app.update m model
    let produced ← runEffects cmd
    loop app model' (rest ++ produced)

def run (app : App Model Msg) : IO Unit := do
  let (model, cmd) := app.init
  let initialMsgs ← runEffects cmd
  loop app model initialMsgs

end LeanTea
