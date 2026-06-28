import LeanTea
import Lean.Data.Json

/-! # llm_chat_tui — full-screen colored chat with ANSI repaint

Renders the chat history as styled bubbles above a `>` prompt;
repaints the whole screen on every turn so the history scrolls
naturally and tool calls/results appear in-place. Cooked-mode line
input (`getLine`) — no raw-mode keystroke handling needed for a
chat UX.

The progress hooks paint a one-line status under the prompt while
the LLM is busy and as tool calls fire, then the screen is
repainted with the full result on completion.

```
llm_chat_tui --config llm-chat.json
``` -/

open Lean (Json)
open LeanTea.Llm.McpOrchestrator

namespace LlmChatTui

/-! ## ANSI helpers -/

private def esc (s : String) : String := s!"\x1b[{s}"

private def clearScreen : String := esc "2J" ++ esc "H"
private def clearToEnd  : String := esc "0J"
private def cursorHome  : String := esc "H"
private def cursorTo (row col : Nat) : String := esc s!"{row};{col}H"
private def hideCursor  : String := esc "?25l"
private def showCursor  : String := esc "?25h"
private def saveCursor  : String := "\x1b7"
private def restoreCursor : String := "\x1b8"

private def styleReset : String := esc "0m"
private def dim     (s : String) : String := esc "2m" ++ s ++ styleReset
private def bold    (s : String) : String := esc "1m" ++ s ++ styleReset
private def cyan    (s : String) : String := esc "36m" ++ s ++ styleReset
private def green   (s : String) : String := esc "32m" ++ s ++ styleReset
private def yellow  (s : String) : String := esc "33m" ++ s ++ styleReset
private def magenta (s : String) : String := esc "35m" ++ s ++ styleReset
private def red     (s : String) : String := esc "31m" ++ s ++ styleReset
private def blue    (s : String) : String := esc "34m" ++ s ++ styleReset

/-! ## Soft wrap to terminal width.

We don't query the TTY width — falling back to 100 columns means
the display gracefully wraps long lines without splitting in the
middle of an ANSI escape. -/

private partial def softWrapLine (width : Nat) (line : String) : List String :=
  if line.length ≤ width then [line]
  else
    let head := (line.take width).toString
    let tail := (line.drop width).toString
    head :: softWrapLine width tail

private def softWrap (width : Nat) (text : String) : List String :=
  text.splitOn "\n" |>.flatMap (softWrapLine width)

/-! ## Bubble rendering -/

private def truncate (s : String) (n : Nat) : String :=
  if s.length ≤ n then s
  else (s.take n).toString ++ "…"

private def imgBadge (imgs : Array String) : String :=
  if imgs.isEmpty then ""
  else
    let kb := (imgs.foldl (fun acc u => acc + u.length * 3 / 4) 0) / 1024
    blue s!" [📎 {imgs.size} img, {kb} kB]"

private def renderMessage (width : Nat) (m : ChatMsg) : List String :=
  match m.role with
  | .user =>
    let head := cyan (bold "you ") ++ imgBadge m.images
    let body := softWrap (width - 4) m.text
    match body with
    | []      => [head]
    | l :: ls => (head ++ l) :: ls.map (fun x => "    " ++ x)
  | .assistant =>
    if m.text.isEmpty && !m.toolCalls.isEmpty then
      [dim "(assistant requested tool calls)"]
    else
      let head := green (bold "ai  ")
      let body := softWrap (width - 4) m.text
      match body with
      | []      => [head]
      | l :: ls => (head ++ l) :: ls.map (fun x => "    " ++ x)
  | .tool =>
    let head := yellow s!"    ↳ {m.toolName}{imgBadge m.images}"
    let preview := truncate (m.text.replace "\n" "↵") (width - 8)
    [head, dim s!"      {preview}"]
  | .system =>
    /- We never push system messages into the visible history. -/
    []

private def renderToolCalls (calls : Array Json) : List String :=
  calls.toList.map fun c =>
    let fn := (c.getObjVal? "function").toOption.getD Json.null
    let name := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
    let argsS := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
    yellow s!"    → {name}({argsS})"

private def renderHistory (width : Nat) (history : Array ChatMsg) : String :=
  let lines := history.toList.flatMap fun m =>
    let toolLines := renderToolCalls m.toolCalls
    renderMessage width m ++ toolLines
  String.intercalate "\n" lines

/-! ## Screen layout

```
┌─────────── llm-chat — model=… ─[3 servers, 14 tools]──┐
│                                                       │
│ you  hello                                            │
│ ai   hi! what can I help with?                        │
│ you  open example.com                                 │
│     → chrome::chrome_navigate({"url":"..."})          │
│   ↳ chrome::chrome_navigate                           │
│     ok                                                │
│ ai   Done — page title is "Example Domain".           │
│                                                       │
│ > _                                                   │
└───────────────────────────────────────────────────────┘
``` -/

private def banner (modelName : String) (nServers nTools : Nat) : String :=
  let title := s!" llm-chat — model={modelName} — {nServers} server(s), {nTools} tool(s) "
  magenta (bold title)

private def divider (width : Nat) : String :=
  dim (String.ofList (List.replicate width '─'))

private def repaint (modelName : String) (nServers nTools : Nat)
    (history : Array ChatMsg) (status : String := "") (width : Nat := 100)
    : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStr clearScreen
  stdout.putStrLn (banner modelName nServers nTools)
  stdout.putStrLn (divider width)
  stdout.putStrLn (renderHistory width history)
  stdout.putStrLn ""
  stdout.putStrLn (divider width)
  unless status.isEmpty do stdout.putStrLn (dim status)
  stdout.putStr (bold "> ")
  stdout.flush

/-! ## Progress hooks — write a sticky status line via cursor save/restore -/

private def writeStatus (s : String) : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStr saveCursor
  stdout.putStr "\n"
  stdout.putStr clearToEnd
  stdout.putStr (dim s)
  stdout.putStr restoreCursor
  stdout.flush

/-- Synchronous y/n/a/d prompt on the TUI's input stream. We
    repaint the bottom of the screen with the question, then
    `getLine` from stdin (cooked mode — the user just types one
    letter + Enter). -/
private def askDecision (name : String) (args : Json) : IO UserDecision := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let preview := truncate args.compress 80
  let mut decided : Option UserDecision := none
  while decided.isNone do
    stdout.putStr saveCursor
    stdout.putStr "\n"
    stdout.putStr clearToEnd
    stdout.putStrLn (yellow s!"⚠ approve `{name}({preview})`?")
    stdout.putStr (bold "  [y allow once / n deny once / a always allow / d always deny] > ")
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

/-- Guess image mime from extension. -/
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
  let mut status := dim "commands: /attach  /clear-attach  /clear  /sessions  \
/save  /load  /new  /policy  /policy-rm <n>  /quit"
  repaint o.model nServers nTools history status
  let mut loop := true
  while loop do
    let line ← stdin.getLine
    if line.isEmpty then
      loop := false
    else
      let user := line.trimAscii.toString
      if user.isEmpty then
        pure ()
      else if user == "/quit" then
        loop := false
      else if user == "/clear" then
        history := #[]
        status := dim "(history cleared)"
        repaint o.model nServers nTools history status
      else if user == "/clear-attach" then
        pendingImages := #[]
        status := dim "(attachments cleared)"
        repaint o.model nServers nTools history status
      else if user == "/sessions" then
        let summaries ← LeanTea.Llm.ChatStore.list storeDir
        let lines := summaries.toList.map fun s =>
          let mark := if s.id == activeId then "*" else " "
          s!"{mark} {s.id} ({s.count} msgs) {s.name}"
        let joined := if lines.isEmpty then "(no saved sessions)"
                      else String.intercalate "\n" lines
        status := dim joined
        repaint o.model nServers nTools history status
      else if user == "/new" then
        history := #[]
        activeId := ""
        pendingImages := #[]
        status := dim "(new session)"
        repaint o.model nServers nTools history status
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
        status := dim s!"(saved {saved.id} `{saved.name}`)"
        repaint o.model nServers nTools history status
      else if user.startsWith "/load " then
        let id := (user.drop "/load ".length).trimAscii.toString
        match ← LeanTea.Llm.ChatStore.load storeDir id with
        | none =>
          status := red s!"no such session: {id}"
          repaint o.model nServers nTools history status
        | some s =>
          history := s.messages
          activeId := s.id
          status := dim s!"(loaded {s.id} `{s.name}`)"
          repaint o.model nServers nTools history status
      else if user == "/policy" then
        let rules ← policy.get
        let lines := rules.toArray.zipIdx.toList.map fun (r, i) =>
          s!"  [{i}] {r.action}  {r.pattern}"
        let joined := if lines.isEmpty then "(no policy rules)"
                      else String.intercalate "\n" lines
        status := dim joined
        repaint o.model nServers nTools history status
      else if user.startsWith "/policy-rm " then
        let n := (user.drop "/policy-rm ".length).trimAscii.toString.toNat?.getD 0
        policy.deleteAt n
        status := dim s!"(removed policy rule {n})"
        repaint o.model nServers nTools history status
      else if user.startsWith "/attach " then
        let path := (user.drop "/attach ".length).trimAscii.toString
        try
          let url ← imageToDataUrl path
          pendingImages := pendingImages.push url
          status := blue s!"📎 attached {path} ({(url.length * 3 / 4 / 1024)} kB) — \
{pendingImages.size} pending"
          repaint o.model nServers nTools history status
        catch e =>
          status := red s!"attach failed: {e}"
          repaint o.model nServers nTools history status
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
          status := dim s!"session {saved.id}"
          repaint o.model nServers nTools history status
        catch e =>
          repaint o.model nServers nTools history (red s!"error: {e}")
  o.shutdown
  let stdout ← IO.getStdout
  stdout.putStrLn ""
  stdout.putStrLn (dim "(bye)")

end LlmChatTui

def main (args : List String) : IO Unit := LlmChatTui.main args
