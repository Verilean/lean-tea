import LeanTea
import Lean.Data.Json

/-! # zellij_mcp_serve — MCP server driving `zellij` for AI pane orchestration

The Zellij sibling of `tmux_mcp_serve`. Same shape as the other LeanTEA MCP
servers (`tmux_mcp_serve`, `desktop_mcp_serve`, …): one binary, stdio + HTTP
transports, tools under a common prefix, shelling out to `zellij(1)` via
`IO.Process.output` with `args : Array String` (no shell concatenation).

### Zellij vs tmux

Zellij's model is *focus-relative*, not address-based: `zellij action …`
operates on the currently-focused pane of a session, and panes/tabs aren't
addressed like tmux's `session:window.pane`. So the tools here target a
**session** (`--session NAME`) and act on its focused pane — write to it,
dump its screen, move focus, spawn panes/tabs. Discover session names with
`zellij_list_sessions` first. Handy detail: `dump-screen` prints to STDOUT,
so capturing a pane needs no temp file.

### Use cases

* Let an LLM drive the Zellij session you're working in: type a command,
  read the resulting screen, open a pane for a build, watch its output.
* One-shot: `zellij_run` opens a pane for a command, waits, and dumps the
  screen — the "run this and show me" primitive.

### Security boundary

Like `tmux_mcp_serve`, this exposes shell execution (a pane runs whatever
you write/spawn) to whatever LLM connects. Trusted local development only;
never expose the HTTP port. Constrain the LLM via prompts, not the wire.

```
zellij_mcp_serve --port 8020      # HTTP for curl + LLM clients
zellij_mcp_serve                  # stdio for MCP-Lite clients
```
-/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace ZellijMcp

open LeanTea.Mcp (jsonOk jsonErr textContent errContent
                  argSchema toolDef defaultInitializeResult)

/-! ## Argument extraction (mirrors TmuxMcp) -/

private def getStr (args : Json) (k : String) : Except String String :=
  match args.getObjVal? k with
  | .ok v => v.getStr?
  | .error e => .error e

private def getStrOpt (args : Json) (k : String) (default : String := "") : String :=
  match args.getObjVal? k with
  | .ok v => match v.getStr? with
             | .ok s => s
             | _ => default
  | _ => default

private def getBoolOpt (args : Json) (k : String) (default : Bool := false) : Bool :=
  match args.getObjVal? k with
  | .ok (.bool b) => b
  | _             => default

private def getNatOpt (args : Json) (k : String) (default : Nat := 0) : Nat :=
  match args.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

/-! ## ANSI stripping — `zellij list-sessions` colours its output even
    under NO_COLOR, so strip CSI escapes to keep the tool text clean. -/

private partial def dropCsi : List Char → List Char
  | c :: t => if c.toNat ≥ 0x40 ∧ c.toNat ≤ 0x7e then t else dropCsi t
  | []     => []

private partial def stripAnsiAux : List Char → String → String
  | '\x1b' :: '[' :: rest, acc => stripAnsiAux (dropCsi rest) acc
  | c :: rest, acc            => stripAnsiAux rest (acc.push c)
  | [], acc                   => acc

private def stripAnsi (s : String) : String := stripAnsiAux s.toList ""

/-! ## Low-level: run `zellij ARGS…`. -/

private structure RunResult where
  exitCode : UInt32
  stdout   : String
  stderr   : String

private def runZellij (args : Array String) : IO RunResult := do
  let out ← IO.Process.output { cmd := "zellij", args := args }
  return { exitCode := out.exitCode, stdout := out.stdout, stderr := out.stderr }

/-- Run zellij and return stdout as a `textContent` on success, else an
    `errContent` carrying stderr. -/
private def runZellijText (args : Array String) (okLabel : String) : IO Json := do
  let r ← runZellij args
  if r.exitCode != 0 then
    let msg := if r.stderr.isEmpty then s!"zellij exit {r.exitCode}" else r.stderr.trimAscii.toString
    return errContent s!"{okLabel}: {msg}"
  let body := if r.stdout.isEmpty then okLabel else r.stdout.trimAscii.toString
  return textContent body

/-- Prepend the `--session NAME action` prefix (or bare `action` when no
    session is given — only meaningful when the server itself runs inside a
    Zellij session). -/
private def actionArgs (session : String) (rest : Array String) : Array String :=
  (if session.isEmpty then #["action"] else #["--session", session, "action"]) ++ rest

/-! ## Tool catalogue -/

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "zellij_list_sessions"
        ("List Zellij sessions with their status, one per line (a `(current)` / "
         ++ "EXITED marker distinguishes live from dead ones). Use a live session's "
         ++ "name as the `session` arg of the other tools.")
        #[] #[],
      toolDef "zellij_dump_screen"
        ("Dump the visible content of the session's focused pane (to STDOUT). "
         ++ "Pass `full=true` to include scrollback. This is how you read what a pane shows.")
        #[ argSchema "session" "string"  "target session name (from zellij_list_sessions)",
           argSchema "full"    "boolean" "(optional) include full scrollback" ]
        #["session"],
      toolDef "zellij_write"
        ("Type `text` into the session's focused pane (via write-chars). By default a "
         ++ "carriage return is appended (`enter=true`) so it runs like a typed command; "
         ++ "pass `enter=false` to inject text without submitting.")
        #[ argSchema "session" "string"  "target session name",
           argSchema "text"    "string"  "characters to type into the focused pane",
           argSchema "enter"   "boolean" "(default true) append a carriage return" ]
        #["session", "text"],
      toolDef "zellij_new_pane"
        ("Open a new pane in the session. Optional `direction` (right|down), `cwd`, "
         ++ "`name`, and `cmd` (run in the new pane via `sh -c`). The new pane takes focus.")
        #[ argSchema "session"   "string" "target session name",
           argSchema "direction" "string" "(optional) right | down",
           argSchema "cwd"       "string" "(optional) working directory",
           argSchema "name"      "string" "(optional) pane name",
           argSchema "cmd"       "string" "(optional) command to run in the new pane" ]
        #["session"],
      toolDef "zellij_new_tab"
        "Open a new tab in the session. Optional `name` and `cwd`."
        #[ argSchema "session" "string" "target session name",
           argSchema "name"    "string" "(optional) tab name",
           argSchema "cwd"     "string" "(optional) working directory" ]
        #["session"],
      toolDef "zellij_close_pane"
        "Close the session's focused pane."
        #[ argSchema "session" "string" "target session name" ]
        #["session"],
      toolDef "zellij_move_focus"
        "Move pane focus in a direction (left|right|up|down)."
        #[ argSchema "session"   "string" "target session name",
           argSchema "direction" "string" "left | right | up | down" ]
        #["session", "direction"],
      toolDef "zellij_go_to_tab"
        "Switch the session to the tab at 1-based `index`."
        #[ argSchema "session" "string" "target session name",
           argSchema "index"   "number" "1-based tab index" ]
        #["session", "index"],
      toolDef "zellij_run"
        ("Convenience: open a pane running `cmd` (via `sh -c`), wait `waitMs` "
         ++ "(default 600) so it can produce output, then dump the focused pane's "
         ++ "screen. The 'run this and show me the result' primitive.")
        #[ argSchema "session" "string" "target session name",
           argSchema "cmd"     "string" "command to run",
           argSchema "cwd"     "string" "(optional) working directory",
           argSchema "waitMs"  "number" "(default 600) ms to wait before dumping" ]
        #["session", "cmd"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-tea-zellij-mcp"

/-! ## Tool dispatch -/

def callTool (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "zellij_list_sessions" =>
      let r ← runZellij #["list-sessions"]
      let raw := if r.stdout.isEmpty then r.stderr else r.stdout
      let clean := (stripAnsi raw).trimAscii.toString
      return textContent (if clean.isEmpty then "no sessions" else clean)
    | "zellij_dump_screen" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        let full := getBoolOpt args "full"
        let a := actionArgs session (#["dump-screen"] ++ (if full then #["--full"] else #[]))
        let r ← runZellij a
        if r.exitCode != 0 then return errContent s!"dump-screen: {r.stderr.trimAscii}"
        return textContent r.stdout
    | "zellij_write" =>
      match getStr args "session", getStr args "text" with
      | .error e, _ => return errContent s!"session: {e}"
      | _, .error e => return errContent s!"text: {e}"
      | .ok session, .ok text =>
        let r1 ← runZellij (actionArgs session #["write-chars", text])
        if r1.exitCode != 0 then return errContent s!"write-chars: {r1.stderr.trimAscii}"
        if getBoolOpt args "enter" true then
          let r2 ← runZellij (actionArgs session #["write", "13"])
          if r2.exitCode != 0 then return errContent s!"write (Enter): {r2.stderr.trimAscii}"
        return textContent s!"wrote to `{session}`"
    | "zellij_new_pane" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        let dir  := getStrOpt args "direction"
        let cwd  := getStrOpt args "cwd"
        let pname := getStrOpt args "name"
        let cmd  := getStrOpt args "cmd"
        let mut rest : Array String := #["new-pane"]
        if !dir.isEmpty   then rest := rest ++ #["-d", dir]
        if !cwd.isEmpty   then rest := rest ++ #["--cwd", cwd]
        if !pname.isEmpty then rest := rest ++ #["-n", pname]
        if !cmd.isEmpty   then rest := rest ++ #["--", "sh", "-c", cmd]
        runZellijText (actionArgs session rest) s!"new pane in `{session}`"
    | "zellij_new_tab" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        let tname := getStrOpt args "name"
        let cwd   := getStrOpt args "cwd"
        let mut rest : Array String := #["new-tab"]
        if !tname.isEmpty then rest := rest ++ #["-n", tname]
        if !cwd.isEmpty   then rest := rest ++ #["-c", cwd]
        runZellijText (actionArgs session rest) s!"new tab in `{session}`"
    | "zellij_close_pane" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        runZellijText (actionArgs session #["close-pane"]) s!"closed pane in `{session}`"
    | "zellij_move_focus" =>
      match getStr args "session", getStr args "direction" with
      | .error e, _ => return errContent s!"session: {e}"
      | _, .error e => return errContent s!"direction: {e}"
      | .ok session, .ok dir =>
        runZellijText (actionArgs session #["move-focus", dir]) s!"moved focus {dir}"
    | "zellij_go_to_tab" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        let idx := getNatOpt args "index" 1
        runZellijText (actionArgs session #["go-to-tab", toString idx]) s!"went to tab {idx}"
    | "zellij_run" =>
      match getStr args "session", getStr args "cmd" with
      | .error e, _ => return errContent s!"session: {e}"
      | _, .error e => return errContent s!"cmd: {e}"
      | .ok session, .ok cmd =>
        let cwd    := getStrOpt args "cwd"
        let waitMs := match getNatOpt args "waitMs" with | 0 => 600 | n => n
        let mut rest : Array String := #["new-pane"]
        if !cwd.isEmpty then rest := rest ++ #["--cwd", cwd]
        rest := rest ++ #["--", "sh", "-c", cmd]
        let r1 ← runZellij (actionArgs session rest)
        if r1.exitCode != 0 then return errContent s!"zellij_run: new-pane failed: {r1.stderr.trimAscii}"
        IO.sleep waitMs.toUInt32
        let r2 ← runZellij (actionArgs session #["dump-screen"])
        if r2.exitCode != 0 then return errContent s!"zellij_run: dump-screen failed: {r2.stderr.trimAscii}"
        return textContent r2.stdout
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## HTTP + stdio transports (mirrors TmuxMcp) -/

private structure Args where
  mode : String := "stdio"
  port : UInt16 := 8020
  host : String := "0.0.0.0"

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port"  :: v :: rest =>
    parseArgs rest { a with mode := "http", port := (v.toNat?.getD 8020).toUInt16 }
  | "--host"  :: v :: rest => parseArgs rest { a with host := v }
  | _ :: rest              => parseArgs rest a
  | []                     => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool
  }
  match a.mode with
  | "http" =>
    IO.eprintln s!"zellij-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "zellij-mcp: stdio mode"
    mcpHandler.serveStdio

end ZellijMcp

def main (args : List String) : IO Unit := ZellijMcp.serveMain args
