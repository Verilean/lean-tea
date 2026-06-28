import LeanTea
import Lean.Data.Json

/-! # llm_chat_cli — thinnest possible REPL on top of `McpOrchestrator`

```
llm_chat_cli --config llm-chat.json
> open github.com and tell me the page title
[chrome::chrome_navigate]({"url":"https://github.com"})
[chrome::chrome_get_text]({})
The GitHub homepage shows the headline "Let's build from here".
> ^D
```

ANSI colors on stderr-style progress lines; the final assistant
text goes to stdout so the binary composes with other CLI tools
(`llm_chat_cli ... < script.txt`). -/

open Lean (Json)
open LeanTea.Llm.McpOrchestrator

namespace LlmChatCli

private def cYellow (s : String) : String := s!"\x1b[33m{s}\x1b[0m"
private def cGrey   (s : String) : String := s!"\x1b[2m{s}\x1b[0m"
private def cCyan   (s : String) : String := s!"\x1b[36m{s}\x1b[0m"
private def cRed    (s : String) : String := s!"\x1b[31m{s}\x1b[0m"
private def cBold   (s : String) : String := s!"\x1b[1m{s}\x1b[0m"

private def truncate (s : String) (n : Nat) : String :=
  if s.length ≤ n then s
  else (s.take n).toString ++ "…"

private def cliHooks : ProgressHooks := {
  onLlmStart := fun n => IO.eprintln (cGrey s!"  ⏳ round {n + 1} — asking LLM"),
  onLlmEnd   := fun _ => pure (),
  onToolCall := fun name args =>
    IO.eprintln (cYellow s!"  → {name}({truncate args.compress 200})"),
  onToolResult := fun _ result =>
    let oneLine := result.replace "\n" " "
    IO.eprintln (cGrey s!"  ← {truncate oneLine 200}")
}

/-- Guess the image mime type from a file extension. -/
private def guessImageMime (path : String) : String :=
  let lc := path.toLower
  if      lc.endsWith ".png"  then "image/png"
  else if lc.endsWith ".jpg" || lc.endsWith ".jpeg" then "image/jpeg"
  else if lc.endsWith ".webp" then "image/webp"
  else if lc.endsWith ".gif"  then "image/gif"
  else "image/png"

/-- Read an image file and return a `data:…` URL. Errors propagate. -/
private def imageToDataUrl (path : String) : IO String :=
  LeanTea.Llm.Openai.imageDataUrlFromFile path (guessImageMime path)

private structure Args where
  configPath : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--config" :: v :: rest => parseArgs rest { a with configPath := v }
  | _ :: rest               => parseArgs rest a
  | []                      => a

def main (rawArgs : List String) : IO Unit := do
  let a := parseArgs rawArgs {}
  if a.configPath.isEmpty then
    IO.eprintln "usage: llm_chat_cli --config <file.json>"
    IO.Process.exit 2
  let fc ← loadConfig a.configPath
  IO.eprintln (cCyan s!"model={fc.model} via {fc.baseUrl}")
  IO.eprintln (cCyan s!"system={truncate fc.systemPrompt 100}")
  let serverList := String.intercalate ", " <| fc.servers.toList.map (·.name)
  IO.eprintln (cCyan s!"servers=[{serverList}]")
  let o ← fromConfig fc
  IO.eprintln (cCyan s!"loaded {o.openAiTools.size} tools across {o.servers.size} server(s)")
  let storeDir ← (·.getD (← LeanTea.Llm.ChatStore.defaultDir)) <$> IO.getEnv "LLM_CHAT_STORE"
  IO.eprintln (cCyan s!"store={storeDir}")
  IO.eprintln (cGrey "commands: /attach <path>  /clear-attach  /history  /clear  \
/sessions  /save [name]  /load <id>  /new  /quit")
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let mut history : Array ChatMsg := #[]
  let mut activeId : String := ""
  let mut pendingImages : Array String := #[]
  let mut pendingPaths  : Array String := #[]
  let mut loop := true
  while loop do
    let attachNote :=
      if pendingPaths.isEmpty then ""
      else cYellow s!" [📎 {pendingPaths.size} attachment(s)]"
    stdout.putStr (cBold s!"\n>{attachNote} ")
    stdout.flush
    let line ← stdin.getLine
    if line.isEmpty then
      IO.eprintln (cGrey "\n(eof — bye)")
      loop := false
    else
      let user := line.trimAscii.toString
      if user.isEmpty then
        pure ()
      else if user == "/quit" then
        loop := false
      else if user == "/clear" then
        history := #[]
        IO.eprintln (cGrey "(history cleared)")
      else if user == "/clear-attach" then
        pendingImages := #[]
        pendingPaths  := #[]
        IO.eprintln (cGrey "(attachments cleared)")
      else if user == "/sessions" then
        let summaries ← LeanTea.Llm.ChatStore.list storeDir
        if summaries.isEmpty then
          IO.eprintln (cGrey "(no saved sessions)")
        else
          for s in summaries do
            let mark := if s.id == activeId then "*" else " "
            IO.eprintln (cGrey s!"  {mark} {s.id}  ({s.count} msgs)  {s.name}")
      else if user == "/new" then
        history := #[]
        activeId := ""
        pendingImages := #[]
        pendingPaths  := #[]
        IO.eprintln (cGrey "(new session)")
      else if user.startsWith "/save" then
        let nameArg := (user.drop "/save".length).trimAscii.toString
        let session : LeanTea.Llm.ChatStore.Session ←
          if activeId.isEmpty then LeanTea.Llm.ChatStore.newSession nameArg
          else pure {
            id := activeId, name := nameArg,
            created := 0, updated := 0,
            messages := history
          }
        let session := { session with messages := history }
        let saved ← LeanTea.Llm.ChatStore.save storeDir session
        activeId := saved.id
        IO.eprintln (cGrey s!"(saved {saved.id} as `{saved.name}`)")
      else if user.startsWith "/load " then
        let id := (user.drop "/load ".length).trimAscii.toString
        match ← LeanTea.Llm.ChatStore.load storeDir id with
        | none =>
          IO.eprintln (cRed s!"no such session: {id}")
        | some s =>
          history := s.messages
          activeId := s.id
          IO.eprintln (cGrey s!"(loaded {s.id} `{s.name}` — {s.messages.size} msgs)")
      else if user == "/history" then
        IO.eprintln (cGrey s!"history: {history.size} message(s)")
        for m in history do
          let role := toString m.role
          let preview := truncate (m.text.replace "\n" " ") 80
          let imgs := if m.images.isEmpty then "" else s!" [📎 {m.images.size}]"
          IO.eprintln (cGrey s!"  [{role}]{imgs} {preview}")
      else if user.startsWith "/attach " then
        let path := (user.drop "/attach ".length).trimAscii.toString
        try
          let url ← imageToDataUrl path
          pendingImages := pendingImages.push url
          pendingPaths  := pendingPaths.push path
          IO.eprintln (cYellow s!"  📎 attached {path} ({(url.length * 3 / 4 / 1024)} kB)")
        catch e =>
          IO.eprintln (cRed s!"attach failed: {e}")
      else
        try
          let newMsgs ← o.runTurnFull history user pendingImages cliHooks
          history := history ++ newMsgs
          pendingImages := #[]
          pendingPaths  := #[]
          /- Auto-persist after each turn so closing the terminal
             never loses the conversation. Create a session on the
             first turn if one isn't active yet. -/
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
          /- Final assistant message in this turn = the last message
             that is `.assistant` with no tool_calls. -/
          let finalText :=
            (newMsgs.filter (fun m => m.role == .assistant && m.toolCalls.isEmpty))
            |>.back?.map (·.text) |>.getD ""
          /- Surface any tool-generated images by file path. -/
          let toolImgs := newMsgs.filter (fun m => m.role == .tool && !m.images.isEmpty)
          unless toolImgs.isEmpty do
            for m in toolImgs do
              IO.eprintln (cYellow s!"  🖼 {m.toolName} returned {m.images.size} image(s)")
          stdout.putStrLn ""
          stdout.putStrLn (cBold "assistant:")
          stdout.putStrLn finalText
          IO.eprintln (cGrey s!"  (session {saved.id})")
        catch e =>
          IO.eprintln (cRed s!"error: {e}")
  o.shutdown

end LlmChatCli

def main (args : List String) : IO Unit := LlmChatCli.main args
