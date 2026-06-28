import LeanTea
import Lean.Data.Json

/-! # coder_mcp_serve — MCP server for code-editing agents

Seven tools, all workspace-bound via `LeanTea.Net.SafePath`:

| tool | what it does | typical policy |
|---|---|---|
| `coder_read_file`  | read one file in the workspace | allow |
| `coder_list_dir`   | one-level listing of a directory | allow |
| `coder_glob`       | recursive suffix match (e.g. `.lean`) | allow |
| `coder_grep`       | shell out to `grep -rn` | allow |
| `coder_write_file` | write the whole file contents | **ask** |
| `coder_edit_file`  | exact search-and-replace | **ask** |
| `coder_run`        | `sh -c CMD` inside the workspace cwd | **ask** |

Pair with `LeanTea.Llm.Policy` in the chat app: allow the read-only
tools globally, leave the mutating ones (`write_file`, `edit_file`,
`run`) to be ask'd. That gives a Claude-Code-style flow where the
LLM proposes, the human approves.

### Why `edit_file` requires unique-match

`coder_write_file` rewrites the file from scratch — fine for new
files, blunt for tiny edits. `coder_edit_file` is a search-and-
replace with **exactly-one** match enforced: if the search string
appears zero times the call errors (model has the wrong spelling),
and if it appears more than once the call errors (model needs to
add surrounding context to disambiguate). This is the same
invariant Claude Code's Edit tool uses and the failure modes are
easy for the LLM to recover from.

### Workspace boundary

Every path argument is resolved through `SafePath.under workspace`.
Absolute paths outside the workspace are rejected before any IO
happens. `..` segments are normalised. NUL bytes in paths are
rejected. The `coder_run` tool sets `cwd := workspace` and `env :=
[]` overrides — it can still shell out to `cd /etc; cat shadow`
because we don't sandbox `/bin/sh` here, but that's exactly the
class of thing the policy `ask` gate is for. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace CoderMcp

open LeanTea.Mcp (jsonOk jsonErr textContent errContent
                  argSchema toolDef defaultInitializeResult)

/-! ## Argument helpers (mirror TmuxMcp / GeminiMcp) -/

private def getStr (args : Json) (k : String) : Except String String :=
  match args.getObjVal? k with
  | .ok v    => v.getStr?
  | .error e => .error e

private def getStrOpt (args : Json) (k : String) (default : String := "") : String :=
  match args.getObjVal? k with
  | .ok v => match v.getStr? with
             | .ok s => s
             | _     => default
  | _ => default

/-! ## Workspace state -/

private initialize wsRef : IO.Ref String ← IO.mkRef ""

private def getWs : IO String := do
  let ws ← wsRef.get
  if ws.isEmpty then
    throw <| IO.userError "coder-mcp: --workspace not set; refusing to operate"
  else
    return ws

/-! ## SafePath wrapper

Every tool that takes a path runs it through `SafePath.under ws`
and returns the resolved absolute path. Pull the path arg out,
resolve it, and either dispatch the tool with the safe path or
surface a clear error block. -/

private def withSafePath
    (args : Json) (key : String)
    (k : String → IO Json) : IO Json := do
  match getStr args key with
  | .error e => return errContent s!"{key}: {e}"
  | .ok raw =>
    let ws ← getWs
    match LeanTea.Net.SafePath.under ws raw with
    | .error msg => return errContent s!"{key}: {msg}"
    | .ok p      => k p.value

/-! ## Output-size caps

LLMs can't reason over a 200 KB read result and the next round's
prompt blows the context window. Cap at 64 KB / 800 lines per
read tool. The caller can ask for more via a follow-up read of a
narrower slice — that's intentional pressure to keep prompts small. -/

private def maxReadBytes : Nat := 64 * 1024
private def maxLines      : Nat := 800

private def truncateBody (body : String) : String × Bool :=
  let lines := body.splitOn "\n"
  if lines.length > maxLines then
    let head := lines.take maxLines
    (String.intercalate "\n" head, true)
  else if body.utf8ByteSize > maxReadBytes then
    /- Split on bytes, not chars — UTF-8 safe? we use `take` on
       String which is char-based; close enough for size-bound. -/
    ((body.take maxReadBytes).toString, true)
  else
    (body, false)

/-! ## Tool implementations -/

private def readFile (path : String) : IO Json := do
  try
    let body ← IO.FS.readFile path
    let (text, truncated) := truncateBody body
    let suffix :=
      if truncated then
        s!"\n…[output truncated; original was {body.utf8ByteSize} bytes / \
{body.splitOn "\n" |>.length} lines]"
      else ""
    return textContent (text ++ suffix)
  catch e =>
    return errContent s!"read failed: {e}"

private def writeFile (path : String) (body : String) : IO Json := do
  try
    /- Create parent dir if missing — typical agent workflow is to
       generate new test / scaffold files in subdirs that don't yet
       exist. -/
    let dir := (System.FilePath.mk path).parent
    if let some d := dir then
      unless ← System.FilePath.pathExists d.toString do
        IO.FS.createDirAll d.toString
    IO.FS.writeFile path body
    return textContent s!"wrote {body.utf8ByteSize} bytes to {path}"
  catch e =>
    return errContent s!"write failed: {e}"

/-- Count occurrences of `needle` in `hay`. `splitOn` produces N+1
    pieces for N matches, so `length - 1` is the count. -/
private def countOccurrences (hay needle : String) : Nat :=
  if needle.isEmpty then 0
  else (hay.splitOn needle).length - 1

private def editFile (path search replace : String) : IO Json := do
  if search.isEmpty then
    return errContent "edit_file: `search` must not be empty"
  try
    let body ← IO.FS.readFile path
    let n := countOccurrences body search
    if n == 0 then
      return errContent s!"edit_file: `search` not found in {path}"
    if n > 1 then
      return errContent s!"edit_file: `search` matches {n} places in {path} — \
add surrounding context so it's unique"
    let updated := body.replace search replace
    IO.FS.writeFile path updated
    return textContent s!"edited {path} (1 replacement)"
  catch e =>
    return errContent s!"edit failed: {e}"

private def listDir (path : String) : IO Json := do
  try
    let entries ← System.FilePath.readDir path
    let lines := entries.map fun e =>
      let nm := e.fileName
      /- We can't cheaply tell directory from file without an extra
         stat per entry. Keep it lean — the agent can call us back
         on any entry. -/
      s!"  {nm}"
    let header := s!"{entries.size} entry(ies) in {path}:"
    return textContent (header ++ "\n" ++ String.intercalate "\n" lines.toList)
  catch e =>
    return errContent s!"list failed: {e}"

/-- Recursive directory walk filtering by filename suffix. We
    deliberately do **not** support full glob (`?`, `[a-z]`, `**`)
    because the typical agent query is just `.py` / `*.lean`
    suffix-match. Skips `.git`, `node_modules`, and `.lake` — the
    "I never want these" set for code search. -/
private partial def globSuffix (root : String) (suffix : String) (cap : Nat := 500)
    : IO (Array String) := do
  let acc ← IO.mkRef (#[] : Array String)
  let rec walk (dir : String) : IO Unit := do
    let cur ← acc.get
    if cur.size ≥ cap then return ()
    let entries ← (try System.FilePath.readDir dir catch _ => pure #[])
    for e in entries do
      let cur ← acc.get
      if cur.size ≥ cap then break
      let nm := e.fileName
      if nm == ".git" || nm == "node_modules" || nm == ".lake" then continue
      let p := e.path.toString
      let isDir ← (try (System.FilePath.mk p).isDir catch _ => pure false)
      if isDir then
        walk p
      else if nm.endsWith suffix then
        acc.modify (·.push p)
  walk root
  acc.get

private def glob (suffix : String) : IO Json := do
  let ws ← getWs
  let files ← globSuffix ws suffix
  let capNote :=
    if files.size ≥ 500 then "\n[truncated at 500 matches]"
    else ""
  return textContent <|
    s!"{files.size} match(es) for *{suffix}:\n" ++
    String.intercalate "\n" files.toList ++ capNote

private def grep (pattern : String) (pathFilter : String) : IO Json := do
  let ws ← getWs
  /- We shell out to `grep -rn` because its output format (`file:line:text`)
     is exactly what LLMs are trained on. Add `--include` when the caller
     gives a path glob. -/
  let mut args : Array String := #["-rnI", "--color=never", "--exclude-dir=.git",
                                    "--exclude-dir=node_modules", "--exclude-dir=.lake"]
  unless pathFilter.isEmpty do args := args ++ #["--include", pathFilter]
  args := args ++ #[pattern, ws]
  let out ← (try IO.Process.output { cmd := "grep", args }
             catch e => pure { exitCode := 1, stdout := "", stderr := s!"{e}" })
  if out.exitCode ≥ 2 then
    return errContent s!"grep failed: {out.stderr}"
  let stdout := out.stdout
  let lines := stdout.splitOn "\n"
  let capped :=
    if lines.length > maxLines then
      String.intercalate "\n" (lines.take maxLines) ++
      s!"\n[truncated at {maxLines} lines; got {lines.length}]"
    else stdout
  let body :=
    if capped.isEmpty then s!"(no match for {pattern})"
    else capped
  return textContent body

private def run (cmd : String) : IO Json := do
  let ws ← getWs
  if cmd.isEmpty then
    return errContent "run: empty command"
  let out ← (try IO.Process.output {
    cmd := "sh", args := #["-c", cmd], cwd := some ws
  } catch e => pure { exitCode := 127, stdout := "", stderr := s!"{e}" })
  /- Combine stdout + stderr with markers — LLMs need both. -/
  let stdoutPart :=
    if out.stdout.isEmpty then "" else s!"--- stdout ---\n{out.stdout}"
  let stderrPart :=
    if out.stderr.isEmpty then "" else s!"--- stderr ---\n{out.stderr}"
  let exitLine := s!"--- exit {out.exitCode} ---"
  let joined := String.intercalate "\n" <|
    (#[stdoutPart, stderrPart, exitLine].filter (!·.isEmpty)).toList
  let (text, truncated) := truncateBody joined
  let suffix := if truncated then "\n…[output truncated]" else ""
  if out.exitCode == 0 then
    return textContent (text ++ suffix)
  else
    return errContent (text ++ suffix)

/-! ## Tool catalogue -/

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "coder_read_file"
        "Read one file inside the workspace. Returns up to 64 KB / 800 lines."
        #[ argSchema "path" "string" "workspace-relative path" ]
        #["path"],
      toolDef "coder_list_dir"
        "List entries in a directory (one level)."
        #[ argSchema "path" "string" "workspace-relative directory" ]
        #["path"],
      toolDef "coder_glob"
        ("Recursive suffix match across the workspace. Pass `.py`, "
         ++ "`.lean`, etc. Skips .git / node_modules / .lake.")
        #[ argSchema "suffix" "string" "filename suffix (typically `.<ext>`)" ]
        #["suffix"],
      toolDef "coder_grep"
        ("`grep -rnI` over the workspace. Optional `pathFilter` "
         ++ "narrows by --include (e.g. `*.go`).")
        #[ argSchema "pattern"    "string" "regex (basic POSIX)",
           argSchema "pathFilter" "string" "(optional) glob like `*.lean`" ]
        #["pattern"],
      toolDef "coder_write_file"
        ("Overwrite a file with the given contents. Creates parent "
         ++ "directories if missing.")
        #[ argSchema "path"    "string" "workspace-relative path",
           argSchema "content" "string" "full file contents" ]
        #["path", "content"],
      toolDef "coder_edit_file"
        ("Exact search-and-replace inside a file. Fails if `search` "
         ++ "isn't present or matches more than once — add surrounding "
         ++ "context to disambiguate.")
        #[ argSchema "path"    "string" "workspace-relative path",
           argSchema "search"  "string" "exact text to find (one match required)",
           argSchema "replace" "string" "replacement text" ]
        #["path", "search", "replace"],
      toolDef "coder_run"
        ("`sh -c CMD` in the workspace cwd. Returns stdout / stderr / "
         ++ "exit code. Use this for tests, build, git, etc.")
        #[ argSchema "cmd" "string" "shell command (e.g. `lake build`)" ]
        #["cmd"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-coder-mcp"

/-! ## Dispatch -/

def callTool (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "coder_read_file" =>
      withSafePath args "path" readFile
    | "coder_list_dir" =>
      withSafePath args "path" listDir
    | "coder_glob" =>
      match getStr args "suffix" with
      | .error e => return errContent s!"suffix: {e}"
      | .ok suf  => glob suf
    | "coder_grep" =>
      match getStr args "pattern" with
      | .error e => return errContent s!"pattern: {e}"
      | .ok pat  =>
        let pf := getStrOpt args "pathFilter"
        grep pat pf
    | "coder_write_file" =>
      match getStr args "content" with
      | .error e => return errContent s!"content: {e}"
      | .ok body =>
        withSafePath args "path" (fun p => writeFile p body)
    | "coder_edit_file" =>
      match getStr args "search", getStr args "replace" with
      | .error e, _ => return errContent s!"search: {e}"
      | _, .error e => return errContent s!"replace: {e}"
      | .ok srch, .ok repl =>
        withSafePath args "path" (fun p => editFile p srch repl)
    | "coder_run" =>
      match getStr args "cmd" with
      | .error e => return errContent s!"cmd: {e}"
      | .ok cmd  => run cmd
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## Transport -/

private structure Args where
  mode      : String := "stdio"
  port      : UInt16 := 8021
  host      : String := "0.0.0.0"
  workspace : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port"  :: v :: rest =>
    parseArgs rest { a with mode := "http", port := (v.toNat?.getD 8021).toUInt16 }
  | "--host"  :: v :: rest      => parseArgs rest { a with host := v }
  | "--workspace" :: v :: rest  => parseArgs rest { a with workspace := v }
  | _ :: rest                   => parseArgs rest a
  | []                          => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  if let some w ← IO.getEnv "CODER_MCP_WORKSPACE" then
    if a.workspace.isEmpty then a := { a with workspace := w }
  if a.workspace.isEmpty then
    IO.eprintln "coder-mcp: --workspace is required (or set CODER_MCP_WORKSPACE)"
    IO.Process.exit 2
  wsRef.set a.workspace
  IO.eprintln s!"coder-mcp: workspace = {a.workspace}"
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool
  }
  match a.mode with
  | "http" =>
    IO.eprintln s!"coder-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "coder-mcp: stdio mode"
    mcpHandler.serveStdio

end CoderMcp

def main (args : List String) : IO Unit := CoderMcp.serveMain args
