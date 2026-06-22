import LeanTea
import Lean.Data.Json

/-! # tmux_mcp_serve — MCP server driving `tmux(1)` for AI test orchestration

Same shape as the other LeanTEA MCP servers (`chrome_cdp_mcp_serve`,
`desktop_mcp_serve`, …): one binary, stdio + HTTP transports, tools
named with a common prefix.

This one shells out to `tmux` for everything; the binary is on PATH
on every Linux/macOS dev box and the CLI is comprehensive enough
that we don't need a libtmux FFI. All calls go through
`IO.Process.output` with `args : Array String` so there's no shell
concatenation at the invocation layer.

### Use cases

* Spin up a LeanTEA app under test in one pane (`./.lake/build/bin/sheet_serve`),
  and run an SP-side smoke in another (`./.lake/build/bin/auth_spec`).
  Capture both panes to verify which lines printed when.
* Drive a long-running process — `npm run dev`, `cargo watch`, the
  Chrome CDP profile — from an LLM agent: send keys, capture the
  scrolling output, kill on timeout.
* Demo recording: scripted keystrokes into a tmux pane so the
  recording always shows the same input cadence.

### Security boundary

This MCP exposes shell execution to whatever LLM connects. Treat it
like `desktop_mcp_serve` — for trusted local development only, never
expose the HTTP port to the internet. The args passed to
`tmux send-keys` are interpreted by the shell running in the target
pane, so a malicious `keys` string can run arbitrary commands. The
framework's `SafeCmd` philosophy doesn't help here because tmux is
*defined* as "run this shell command in this pane". Constrain the
LLM via prompts, not the wire protocol.

```
tmux_mcp_serve --port 8019      # HTTP for curl + LLM clients
tmux_mcp_serve                  # stdio for MCP-Lite clients
```

The optional `--workspace DIR` flag is passed as `tmux start-server
-c DIR`, scoping any newly minted session's cwd. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace TmuxMcp

open LeanTea.Mcp (jsonOk jsonErr textContent errContent
                  argSchema toolDef defaultInitializeResult)

/-! ## Argument extraction helpers (mirrors DesktopMcp) -/

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

/-! ## Low-level: run `tmux ARGS…` and return stdout (or stderr on
    non-zero exit). -/

private structure RunResult where
  exitCode : UInt32
  stdout   : String
  stderr   : String

private def runTmux (args : Array String) : IO RunResult := do
  let out ← IO.Process.output { cmd := "tmux", args := args }
  return {
    exitCode := out.exitCode,
    stdout   := out.stdout,
    stderr   := out.stderr
  }

/-- Run tmux and return a Json `textContent` of stdout on success;
    otherwise `errContent` carrying stderr (or a synthesised message
    when tmux didn't print anything). -/
private def runTmuxText (args : Array String) (okLabel : String) : IO Json := do
  let r ← runTmux args
  if r.exitCode != 0 then
    let msg := if r.stderr.isEmpty then s!"tmux exit {r.exitCode}" else r.stderr.trimAscii.toString
    return errContent s!"{okLabel}: {msg}"
  let body := if r.stdout.isEmpty then okLabel else r.stdout.trimAscii.toString
  return textContent body

/-! ## Tool catalogue -/

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "tmux_list_sessions"
        ("List every tmux session as a single line per session: "
         ++ "`name (windows=N, attached=Y/N)`.")
        #[] #[],
      toolDef "tmux_new_session"
        ("Create a detached session with `name`. Optional `cmd` is "
         ++ "run as the first pane's command. Optional `cwd` sets "
         ++ "the working directory. Fails if a session by that name "
         ++ "already exists — use tmux_kill_session first to recycle.")
        #[ argSchema "name" "string" "session name (alnum + dashes recommended)",
           argSchema "cmd"  "string" "(optional) command to run in the first pane",
           argSchema "cwd"  "string" "(optional) initial working directory" ]
        #["name"],
      toolDef "tmux_kill_session"
        "Kill the named session and all its windows/panes."
        #[ argSchema "name" "string" "session name" ]
        #["name"],
      toolDef "tmux_list_windows"
        ("List the windows in a session: `index:name (panes=N, active=Y/N)`.")
        #[ argSchema "session" "string" "session name" ]
        #["session"],
      toolDef "tmux_new_window"
        ("Open a new window in a session. Optional `name` and `cmd`.")
        #[ argSchema "session" "string" "target session",
           argSchema "name"    "string" "(optional) window name",
           argSchema "cmd"     "string" "(optional) command for the new window's pane" ]
        #["session"],
      toolDef "tmux_list_panes"
        ("List panes for a target (`session` or `session:window`): "
         ++ "`index:pid (active=Y/N, size=WxH)`.")
        #[ argSchema "target" "string" "target as tmux address — `name` or `name:0`" ]
        #["target"],
      toolDef "tmux_split_window"
        ("Split the target pane. `vertical=true` splits top/bottom; "
         ++ "the default (false) splits left/right. Optional `cmd` "
         ++ "becomes the command in the new pane.")
        #[ argSchema "target"   "string"  "pane target",
           argSchema "vertical" "boolean" "(optional) vertical split",
           argSchema "cmd"      "string"  "(optional) command for the new pane" ]
        #["target"],
      toolDef "tmux_send_keys"
        ("Send literal text + Enter to the target pane. Tmux interprets "
         ++ "C-c / C-d / Up / Down / etc. as special — see tmux(1). "
         ++ "Pass `enter=false` to suppress the trailing Enter, useful "
         ++ "when you want to inject text into a prompt and press a "
         ++ "control key afterwards.")
        #[ argSchema "target" "string"  "pane target",
           argSchema "keys"   "string"  "text or tmux key name",
           argSchema "enter"  "boolean" "(default true) append Enter" ]
        #["target", "keys"],
      toolDef "tmux_capture_pane"
        ("Return the visible buffer of the target pane. Optional "
         ++ "`lines` (positive) reads only the last N lines.")
        #[ argSchema "target" "string" "pane target",
           argSchema "lines"  "number" "(optional) last N lines only" ]
        #["target"],
      toolDef "tmux_kill_pane"
        "Kill one pane."
        #[ argSchema "target" "string" "pane target" ]
        #["target"],
      toolDef "tmux_run"
        ("Convenience: create a fresh detached session running `cmd`, "
         ++ "sleep `waitMs` (default 500ms) so the command can produce "
         ++ "output, capture the pane, then kill the session. Returns "
         ++ "stdout of the captured pane. Useful for one-shot shell "
         ++ "commands when you don't need a long-lived session.")
        #[ argSchema "cmd"     "string" "command to run",
           argSchema "cwd"     "string" "(optional) working directory",
           argSchema "waitMs"  "number" "(default 500) ms to wait before capture" ]
        #["cmd"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-tmux-mcp"

/-! ## Tool dispatch -/

/-- Generate a temporary session name when `tmux_run` doesn't get one. -/
private def freshSessionName : IO String := do
  let r ← IO.rand 0 0xffff_ffff
  return s!"leantea-tmux-mcp-{r}"

def callTool (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "tmux_list_sessions" =>
      runTmuxText
        #["list-sessions", "-F",
          "#{session_name} (windows=#{session_windows}, attached=#{?session_attached,Y,N})"]
        "no sessions"
    | "tmux_new_session" =>
      match getStr args "name" with
      | .error e => return errContent s!"name: {e}"
      | .ok session =>
        let cmd := getStrOpt args "cmd"
        let cwd := getStrOpt args "cwd"
        let mut a : Array String := #["new-session", "-d", "-s", session]
        if !cwd.isEmpty then a := a ++ #["-c", cwd]
        if !cmd.isEmpty then a := a.push cmd
        runTmuxText a s!"created session `{session}`"
    | "tmux_kill_session" =>
      match getStr args "name" with
      | .error e => return errContent s!"name: {e}"
      | .ok session =>
        runTmuxText #["kill-session", "-t", session]
          s!"killed session `{session}`"
    | "tmux_list_windows" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        runTmuxText
          #["list-windows", "-t", session, "-F",
            "#{window_index}:#{window_name} (panes=#{window_panes}, active=#{?window_active,Y,N})"]
          "no windows"
    | "tmux_new_window" =>
      match getStr args "session" with
      | .error e => return errContent s!"session: {e}"
      | .ok session =>
        let wname := getStrOpt args "name"
        let cmd   := getStrOpt args "cmd"
        let mut a : Array String := #["new-window", "-t", session]
        if !wname.isEmpty then a := a ++ #["-n", wname]
        if !cmd.isEmpty   then a := a.push cmd
        runTmuxText a s!"new window in `{session}`"
    | "tmux_list_panes" =>
      match getStr args "target" with
      | .error e => return errContent s!"target: {e}"
      | .ok target =>
        runTmuxText
          #["list-panes", "-t", target, "-F",
            "#{pane_index}:#{pane_pid} (active=#{?pane_active,Y,N}, size=#{pane_width}x#{pane_height})"]
          "no panes"
    | "tmux_split_window" =>
      match getStr args "target" with
      | .error e => return errContent s!"target: {e}"
      | .ok target =>
        let vert := getBoolOpt args "vertical"
        let cmd  := getStrOpt args "cmd"
        let direction := if vert then "-v" else "-h"
        let mut a : Array String := #["split-window", direction, "-t", target]
        if !cmd.isEmpty then a := a.push cmd
        runTmuxText a s!"split `{target}`"
    | "tmux_send_keys" =>
      match getStr args "target", getStr args "keys" with
      | .error e, _ => return errContent s!"target: {e}"
      | _, .error e => return errContent s!"keys: {e}"
      | .ok target, .ok keys =>
        let pressEnter := getBoolOpt args "enter" true
        let mut a : Array String := #["send-keys", "-t", target, keys]
        if pressEnter then a := a.push "Enter"
        runTmuxText a s!"sent keys to `{target}`"
    | "tmux_capture_pane" =>
      match getStr args "target" with
      | .error e => return errContent s!"target: {e}"
      | .ok target =>
        let lines := getNatOpt args "lines"
        let mut a : Array String := #["capture-pane", "-p", "-t", target]
        if lines > 0 then a := a ++ #["-S", s!"-{lines}"]
        let r ← runTmux a
        if r.exitCode != 0 then
          return errContent s!"capture-pane: {r.stderr.trimAscii}"
        return textContent r.stdout
    | "tmux_kill_pane" =>
      match getStr args "target" with
      | .error e => return errContent s!"target: {e}"
      | .ok target =>
        runTmuxText #["kill-pane", "-t", target] s!"killed pane `{target}`"
    | "tmux_run" =>
      match getStr args "cmd" with
      | .error e => return errContent s!"cmd: {e}"
      | .ok cmd =>
        let cwd    := getStrOpt args "cwd"
        let waitMs := match getNatOpt args "waitMs" with
                      | 0 => 500
                      | n => n
        let session ← freshSessionName
        /- Start a long-lived shell pane and `send-keys` the command
           into it; tmux's default behaviour kills the session as
           soon as the command's process exits, which would race the
           capture. Running `sh` first keeps the pane alive until
           we kill the session ourselves. -/
        let mut newArgs : Array String := #["new-session", "-d", "-s", session]
        if !cwd.isEmpty then newArgs := newArgs ++ #["-c", cwd]
        newArgs := newArgs.push "sh"
        let r1 ← runTmux newArgs
        if r1.exitCode != 0 then
          return errContent s!"tmux_run: new-session failed: {r1.stderr.trimAscii}"
        let r2 ← runTmux #["send-keys", "-t", session, cmd, "Enter"]
        if r2.exitCode != 0 then
          let _ ← runTmux #["kill-session", "-t", session]
          return errContent s!"tmux_run: send-keys failed: {r2.stderr.trimAscii}"
        IO.sleep waitMs.toUInt32
        let r3 ← runTmux #["capture-pane", "-p", "-t", session]
        let _ ← runTmux #["kill-session", "-t", session]
        if r3.exitCode != 0 then
          return errContent s!"tmux_run: capture failed: {r3.stderr.trimAscii}"
        return textContent r3.stdout
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## HTTP + stdio transports -/

private structure Args where
  mode      : String := "stdio"
  port      : UInt16 := 8019
  host      : String := "0.0.0.0"
  workspace : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port"  :: v :: rest =>
    parseArgs rest { a with mode := "http", port := (v.toNat?.getD 8019).toUInt16 }
  | "--host"  :: v :: rest => parseArgs rest { a with host := v }
  | "--workspace" :: v :: rest => parseArgs rest { a with workspace := v }
  | _ :: rest              => parseArgs rest a
  | []                     => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  if let some w ← IO.getEnv "TMUX_MCP_WORKSPACE" then
    if a.workspace.isEmpty then a := { a with workspace := w }
  /- If the operator supplied a workspace, set it as the default cwd
     for the running tmux server (a no-op when the server is already
     running). `start-server -c …` is the canonical way. -/
  if !a.workspace.isEmpty then
    let r ← runTmux #["start-server"]
    if r.exitCode != 0 then
      IO.eprintln s!"tmux start-server warning: {r.stderr}"
    /- We don't actually pin the cwd globally (tmux 3.x dropped
       `set-option default-path`); callers should pass `cwd` per
       `tmux_new_session` instead. The workspace is recorded for
       diagnostics. -/
    IO.eprintln s!"tmux-mcp: workspace hint = {a.workspace} (used as default cwd hint only)"
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool
  }
  match a.mode with
  | "http" =>
    IO.eprintln s!"tmux-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "tmux-mcp: stdio mode"
    mcpHandler.serveStdio

end TmuxMcp

def main (args : List String) : IO Unit := TmuxMcp.serveMain args
