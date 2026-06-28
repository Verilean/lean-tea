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
    IO.eprintln (cGrey s!"  ← {truncate (result.replace "\n" " ") 200}")
}

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
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let mut history : Array ChatMsg := #[]
  let mut loop := true
  while loop do
    stdout.putStr (cBold "\n> ")
    stdout.flush
    let line ← stdin.getLine
    if line.isEmpty then
      IO.eprintln (cGrey "\n(eof — bye)")
      loop := false
    else
      let user := line.trimAscii.toString
      unless user.isEmpty do
        try
          let newMsgs ← o.runTurn history user cliHooks
          history := history ++ newMsgs
          /- Final assistant message in this turn = the last message
             that is `.assistant` with no tool_calls. -/
          let finalText :=
            (newMsgs.filter (fun m => m.role == .assistant && m.toolCalls.isEmpty))
            |>.back?.map (·.text) |>.getD ""
          stdout.putStrLn ""
          stdout.putStrLn (cBold "assistant:")
          stdout.putStrLn finalText
        catch e =>
          IO.eprintln (cRed s!"error: {e}")
  o.shutdown

end LlmChatCli

def main (args : List String) : IO Unit := LlmChatCli.main args
