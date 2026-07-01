import LeanTea
import LeanTea.Tui
import Lean.Data.Json

/-! # llm_chat_tui — chat UX built on the LeanTea.Tui widget kit

Full-screen chat: coloured message bubbles above a `>` prompt,
whole-screen repaint on every turn. Cooked-mode line input
(`getLine`) — no raw-mode keystroke handling. Progress hooks paint
a one-line status under the prompt while the LLM is busy, then a
full repaint on completion.

## What changed vs. the pre-widget-kit revision

The old file (353 lines) rolled its own ANSI helpers — `dim`,
`bold`, `cyan`, `saveCursor`, `restoreCursor`, bespoke soft-wrap
+ divider drawing. This one leans on `LeanTea.Tui`: `Style`
records instead of escape helpers, `vbox` / `hbox` / `text`
instead of `String.intercalate` layout math, `renderBoxAnsi` as
the single point that serialises the widget tree to the
terminal.

Progress hooks still write direct ANSI (save/restore cursor +
status line) because they fire *during* a blocking LLM turn, when
we can't repaint the widget tree.

```
llm_chat_tui --config llm-chat.json
```
-/

open Lean (Json)
open LeanTea.Llm.McpOrchestrator
open LeanTea.Tui

namespace LlmChatTui

/-! ## Style palette — one place to tweak the look. -/

private def styleTitle    : Style := { fg := .magenta, bold := true }
private def styleDim      : Style := { dim := true }
private def styleUser     : Style := { fg := .cyan, bold := true }
private def styleAi       : Style := { fg := .green, bold := true }
private def styleTool     : Style := { fg := .yellow }
private def styleToolBody : Style := { dim := true }
private def styleError    : Style := { fg := .red }
private def stylePrompt   : Style := { bold := true }
private def styleAttach   : Style := { fg := .blue }

/-! ## Soft wrap — split long strings without breaking wide-char escapes.

Trivial line-based wrap; we don't try to word-break — a chat UX
already reads a mix of natural prose and code / URLs, and word
splits at arbitrary widths look worse than a hard cut. -/

private partial def softWrapLine (width : Nat) (line : String) : List String :=
  if line.length ≤ width then [line]
  else
    let head := (line.take width).toString
    let tail := (line.drop width).toString
    head :: softWrapLine width tail

private def softWrap (width : Nat) (text : String) : List String :=
  text.splitOn "\n" |>.flatMap (softWrapLine width)

private def truncate (s : String) (n : Nat) : String :=
  if s.length ≤ n then s
  else (s.take (if n == 0 then 0 else n - 1)).toString ++ "…"

/-! ## Message bubbles as widget rows

Each `ChatMsg` produces a list of `Widget Unit` rows — one per
displayed line — so the vbox that holds the history can `vbox`
them all in order. The tag prefix (`you`, `ai`, `  ↳`) is a
separate coloured widget; continuation lines are indented so the
prefix column stays visually clean. -/

private def imgBadgeText (imgs : Array String) : String :=
  if imgs.isEmpty then ""
  else
    let kb := (imgs.foldl (fun acc u => acc + u.length * 3 / 4) 0) / 1024
    s!" [📎 {imgs.size} img, {kb} kB]"

private def messageRows (width : Nat) (m : ChatMsg) : List (Widget Unit) :=
  match m.role with
  | .user =>
    let body := softWrap (width - 4) m.text
    let badge := imgBadgeText m.images
    match body with
    | []      => [text s!"you {badge}" styleUser]
    | l :: ls =>
      text s!"you {l}{badge}" styleUser
        :: ls.map (fun x => text s!"    {x}" {})
  | .assistant =>
    if m.text.isEmpty && !m.toolCalls.isEmpty then
      [text "(assistant requested tool calls)" styleDim]
    else
      let body := softWrap (width - 4) m.text
      match body with
      | []      => [text "ai" styleAi]
      | l :: ls =>
        text s!"ai  {l}" styleAi
          :: ls.map (fun x => text s!"    {x}" {})
  | .tool =>
    let preview := truncate (m.text.replace "\n" "↵") (width - 8)
    [text s!"    ↳ {m.toolName}{imgBadgeText m.images}" styleTool,
     text s!"      {preview}" styleToolBody]
  | .system => []

private def toolCallRows (calls : Array Json) : List (Widget Unit) :=
  calls.toList.map fun c =>
    let fn := (c.getObjVal? "function").toOption.getD Json.null
    let name := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
    let argsS := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
    text s!"    → {name}({argsS})" styleTool

private def historyView (width : Nat) (history : Array ChatMsg) : Widget Unit :=
  let rows := history.toList.flatMap fun m =>
    messageRows width m ++ toolCallRows m.toolCalls
  vbox rows

/-! ## Full screen -/

private def bannerLine (model : String) (nServers nTools : Nat) : Widget Unit :=
  { text s!" llm-chat — model={model} — {nServers} server(s), {nTools} tool(s)" styleTitle
      with prefHeight := some 1 }

private def dividerRow (width : Nat) : Widget Unit :=
  { text (String.ofList (List.replicate width '─')) styleDim
      with prefHeight := some 1 }

private def statusRow (status : String) : Widget Unit :=
  { text status styleDim with prefHeight := some 1 }

private def promptRow : Widget Unit :=
  { text "> " stylePrompt with prefHeight := some 1 }

structure Screen where
  width      : Nat := 100
  height     : Nat := 40
  model      : String
  nServers   : Nat
  nTools     : Nat
  history    : Array ChatMsg
  status     : String

def screen (s : Screen) : Widget Unit :=
  vbox [
    bannerLine s.model s.nServers s.nTools,
    dividerRow s.width,
    historyView s.width s.history,
    dividerRow s.width,
    statusRow s.status,
    promptRow
  ]

/-! ## Repaint & progress-line ANSI

`clearScreen` erases the terminal (which is a scrollback-loss but
matches the old behaviour), then we ship the widget tree as one
ANSI blob via `renderBoxAnsi`. The progress line for the LLM/tool
hooks writes underneath the prompt using save/restore-cursor so
the user's typed input isn't clobbered mid-turn. -/

private def esc (s : String) : String := s!"\x1b[{s}"
private def clearScreen : String := esc "2J" ++ esc "H"
private def clearToEnd  : String := esc "0J"
private def saveCursor  : String := "\x1b7"
private def restoreCursor : String := "\x1b8"

private def paint (s : Screen) : IO Unit := do
  let box := (screen s).render s.width s.height false
  let stdout ← IO.getStdout
  stdout.putStr clearScreen
  stdout.putStr (renderBoxAnsi box)
  stdout.flush

private def styleAnsi (st : Style) : String := Id.run do
  -- Small helper for the progress line, which doesn't go through
  -- the widget tree. Assembles the SGR sequence for `st.fg` +
  -- optional dim/bold. Reset with esc "0m".
  let mut out := esc "0m"
  out := out ++ (
    match st.fg with
    | .cyan    => esc "36m"
    | .green   => esc "32m"
    | .yellow  => esc "33m"
    | .red     => esc "31m"
    | .blue    => esc "34m"
    | .magenta => esc "35m"
    | _        => "")
  if st.bold then out := out ++ esc "1m"
  if st.dim  then out := out ++ esc "2m"
  return out

private def writeStatus (s : String) : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStr saveCursor
  stdout.putStr "\n"
  stdout.putStr clearToEnd
  stdout.putStr (styleAnsi styleDim ++ s ++ esc "0m")
  stdout.putStr restoreCursor
  stdout.flush

/-- Synchronous y/n/a/d prompt on the TUI's input stream. -/
private def askDecision (name : String) (args : Json) : IO UserDecision := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let preview := truncate args.compress 80
  let mut decided : Option UserDecision := none
  while decided.isNone do
    stdout.putStr saveCursor
    stdout.putStr "\n"
    stdout.putStr clearToEnd
    stdout.putStr (styleAnsi styleTool)
    stdout.putStrLn s!"⚠ approve `{name}({preview})`?"
    stdout.putStr (styleAnsi stylePrompt)
    stdout.putStr "  [y allow once / n deny once / a always allow / d always deny] > "
    stdout.putStr (esc "0m")
    stdout.putStr restoreCursor
    stdout.flush
    let line ← stdin.getLine
    match line.trimAscii.toString.toLower with
    | "y" => decided := some .allowOnce
    | "n" => decided := some .denyOnce
    | "a" => decided := some .allowAlways
    | "d" => decided := some .denyAlways
    | _   => pure ()
  return decided.get!

private def tuiHooks : ProgressHooks := {
  onLlmStart   := fun n   => writeStatus s!"⏳ round {n + 1} — thinking…",
  onLlmEnd     := fun _   => writeStatus "",
  onToolCall   := fun n a => writeStatus s!"→ {n}({truncate a.compress 80})",
  onToolResult := fun _ r =>
    let oneLine := r.replace "\n" " "
    writeStatus s!"← {truncate oneLine 80}",
  onAsk := askDecision
}

/-! ## CLI args -/

private structure Args where
  configPath : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--config" :: v :: rest => parseArgs rest { a with configPath := v }
  | _ :: rest               => parseArgs rest a
  | []                      => a

private def guessImageMime (path : String) : String :=
  let lc := path.toLower
  if      lc.endsWith ".png"  then "image/png"
  else if lc.endsWith ".jpg" || lc.endsWith ".jpeg" then "image/jpeg"
  else if lc.endsWith ".webp" then "image/webp"
  else if lc.endsWith ".gif"  then "image/gif"
  else "image/png"

private def imageToDataUrl (path : String) : IO String :=
  LeanTea.Llm.Openai.imageDataUrlFromFile path (guessImageMime path)

def main (rawArgs : List String) : IO Unit := do
  let a := parseArgs rawArgs {}
  if a.configPath.isEmpty then
    IO.eprintln "usage: llm_chat_tui --config <file.json>"
    IO.Process.exit 2
  let fc ← loadConfig a.configPath
  IO.eprintln s!"loading {fc.servers.size} MCP server(s)…"
  let o ← fromConfig fc
  let nServers := o.servers.size
  let nTools := o.openAiTools.size
  let storeDir ← (·.getD (← LeanTea.Llm.ChatStore.defaultDir)) <$> IO.getEnv "LLM_CHAT_STORE"
  let policy ← LeanTea.Llm.Policy.LiveRef.fromDisk storeDir
  let policyCfg : PolicyConfig := { policy := some policy }
  let stdin ← IO.getStdin
  let mut history : Array ChatMsg := #[]
  let mut activeId : String := ""
  let mut pendingImages : Array String := #[]
  let mut screenState : Screen := {
    model := o.model, nServers := nServers, nTools := nTools,
    history := history,
    status := "commands: /attach  /clear-attach  /clear  /sessions  /save  /load  /new  /policy  /policy-rm <n>  /quit"
  }
  paint screenState
  let mut loop := true
  while loop do
    let line ← stdin.getLine
    if line.isEmpty then
      loop := false
    else
      let user := line.trimAscii.toString
      if user.isEmpty then pure ()
      else if user == "/quit" then loop := false
      else if user == "/clear" then
        history := #[]
        screenState := { screenState with history := history, status := "(history cleared)" }
        paint screenState
      else if user == "/clear-attach" then
        pendingImages := #[]
        screenState := { screenState with status := "(attachments cleared)" }
        paint screenState
      else if user == "/sessions" then
        let summaries ← LeanTea.Llm.ChatStore.list storeDir
        let lines := summaries.toList.map fun s =>
          let mark := if s.id == activeId then "*" else " "
          s!"{mark} {s.id} ({s.count} msgs) {s.name}"
        let joined := if lines.isEmpty then "(no saved sessions)"
                      else String.intercalate " · " lines
        screenState := { screenState with status := joined }
        paint screenState
      else if user == "/new" then
        history := #[]
        activeId := ""
        pendingImages := #[]
        screenState := { screenState with history := history, status := "(new session)" }
        paint screenState
      else if user.startsWith "/save" then
        let nameArg := (user.drop "/save".length).trimAscii.toString
        let session : LeanTea.Llm.ChatStore.Session ←
          if activeId.isEmpty then LeanTea.Llm.ChatStore.newSession nameArg
          else pure {
            id := activeId, name := nameArg,
            created := 0, updated := 0,
            messages := history
          }
        let saved ← LeanTea.Llm.ChatStore.save storeDir
          { session with messages := history }
        activeId := saved.id
        screenState := { screenState with status := s!"(saved {saved.id} `{saved.name}`)" }
        paint screenState
      else if user.startsWith "/load " then
        let id := (user.drop "/load ".length).trimAscii.toString
        match ← LeanTea.Llm.ChatStore.load storeDir id with
        | none =>
          screenState := { screenState with status := s!"no such session: {id}" }
          paint screenState
        | some s =>
          history := s.messages
          activeId := s.id
          screenState := { screenState with history := history,
                                             status := s!"(loaded {s.id} `{s.name}`)" }
          paint screenState
      else if user == "/policy" then
        let rules ← policy.get
        let lines := rules.toArray.zipIdx.toList.map fun (r, i) =>
          s!"[{i}] {r.action} {r.pattern}"
        let joined := if lines.isEmpty then "(no policy rules)"
                      else String.intercalate " · " lines
        screenState := { screenState with status := joined }
        paint screenState
      else if user.startsWith "/policy-rm " then
        let n := (user.drop "/policy-rm ".length).trimAscii.toString.toNat?.getD 0
        policy.deleteAt n
        screenState := { screenState with status := s!"(removed policy rule {n})" }
        paint screenState
      else if user.startsWith "/attach " then
        let path := (user.drop "/attach ".length).trimAscii.toString
        try
          let url ← imageToDataUrl path
          pendingImages := pendingImages.push url
          screenState := { screenState with
            status := s!"📎 attached {path} ({url.length * 3 / 4 / 1024} kB) — {pendingImages.size} pending" }
          paint screenState
        catch e =>
          screenState := { screenState with status := s!"attach failed: {e}" }
          paint screenState
      else
        try
          let newMsgs ← o.runTurnFull history user pendingImages tuiHooks policyCfg
          history := history ++ newMsgs
          pendingImages := #[]
          /- Auto-save the session each turn. -/
          let session : LeanTea.Llm.ChatStore.Session ←
            if activeId.isEmpty then
              let fresh ← LeanTea.Llm.ChatStore.newSession
              pure { fresh with messages := history }
            else pure {
              id := activeId, name := "",
              created := 0, updated := 0,
              messages := history
            }
          let saved ← LeanTea.Llm.ChatStore.save storeDir session
          activeId := saved.id
          screenState := { screenState with history := history,
                                             status := s!"session {saved.id}" }
          paint screenState
        catch e =>
          screenState := { screenState with status := s!"error: {e}" }
          paint screenState
  o.shutdown
  let stdout ← IO.getStdout
  stdout.putStrLn ""
  stdout.putStr (styleAnsi styleDim ++ "(bye)" ++ esc "0m")
  stdout.putStrLn ""

end LlmChatTui

def main (args : List String) : IO Unit := LlmChatTui.main args
