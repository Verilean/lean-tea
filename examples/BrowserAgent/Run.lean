import LeanTea
import Lean.Data.Json

/-! # browser_agent — let Gemma 4 drive the browser-MCP

The user gives a task in plain English / Japanese; we hand it to
Gemma 4 with the browser-MCP tool catalogue exposed via OpenAI
function-calling. Each round-trip:

  1. POST `chat/completions` with the running message log + tools.
  2. If the assistant returns `tool_calls`, dispatch each one to the
     `browser_mcp_serve` child process (JSON-RPC over stdio), append
     a `role: "tool"` message with the result, loop.
  3. Otherwise print the assistant text and exit.

This is the "offload testing to an LLM" loop — the user writes
`browser_agent "open example.com and tell me the page title"` and
Gemma 4 chooses `browser_navigate` then `browser_get_text` then
answers in prose. Same pattern for any MCP server, but we hard-code
the browser one because that's the case the user asked for.

The MCP server is spawned as a child rather than reached over HTTP
because the stdio transport is the lowest-fiction MCP wiring — the
exact same binary Claude Code/Desktop spawn. -/

open Lean (Json)
open LeanTea.Llm.Openai (Config)

namespace BrowserAgent

/-! ## MCP stdio client

Minimal JSON-RPC 2.0 client. One in-flight request at a time, ids
auto-increment. The child is `browser_mcp_serve` — same binary the
desktop MCP clients hook into. -/

/-- Two transports: spawn a child `browser_mcp_serve` and talk over
    its pipes (default — clean lifecycle, fresh browser), or POST to
    an already-running HTTP MCP server (lets you attach to a browser
    a human has already logged into — useful for play-the-game tests
    that need session cookies). -/
inductive McpKind where
  | stdio (child : IO.Process.Child { stdin := .piped, stdout := .piped, stderr := .piped })
  | http  (url : String)

structure Mcp where
  kind   : McpKind
  nextId : IO.Ref Nat

private def findMcpBinary : IO String := do
  /- Use the in-repo built binary by default; let `BROWSER_MCP_BIN`
     override (handy when running from a different cwd). -/
  if let some p ← IO.getEnv "BROWSER_MCP_BIN" then
    if ← System.FilePath.pathExists p then return p
  let candidates := [
    "./.lake/build/bin/browser_mcp_serve",
    "../.lake/build/bin/browser_mcp_serve",
    "../../.lake/build/bin/browser_mcp_serve"
  ]
  for c in candidates do
    if ← System.FilePath.pathExists c then return c
  throw <| IO.userError <|
    "couldn't find browser_mcp_serve. Build it with " ++
    "`lake build browser_mcp_serve` or set BROWSER_MCP_BIN=/abs/path."

def Mcp.spawn : IO Mcp := do
  let bin ← findMcpBinary
  let child ← IO.Process.spawn {
    cmd := bin, args := #[],
    stdin := .piped, stdout := .piped, stderr := .piped
  }
  let nextId ← IO.mkRef 1
  /- MCP servers don't print anything before the first request — the
     stdio transport is purely request/response — so we don't try to
     read a ready marker. -/
  return { kind := .stdio child, nextId }

/-- Attach to a remote MCP server speaking the HTTP / JSON-RPC dialect
    (POST one JSON-RPC object per request to the given URL). -/
def Mcp.connect (url : String) : IO Mcp := do
  let nextId ← IO.mkRef 1
  return { kind := .http url, nextId }

def Mcp.close (m : Mcp) : IO Unit := do
  match m.kind with
  | .stdio child =>
    let (_, child') ← child.takeStdin
    let _ ← child'.wait
  | .http _ => return ()

private def sendReq (m : Mcp) (method : String) (params : Json) : IO Json := do
  let id ← m.nextId.get
  m.nextId.set (id + 1)
  let req := Json.mkObj [
    ("jsonrpc", Json.str "2.0"),
    ("id",      Json.num (Int.ofNat id)),
    ("method",  Json.str method),
    ("params",  params)
  ]
  let raw ← match m.kind with
    | .stdio child =>
      child.stdin.putStr (req.compress ++ "\n")
      child.stdin.flush
      let line ← child.stdout.getLine
      if line.isEmpty then
        throw <| IO.userError s!"mcp: server closed mid-request (method={method})"
      pure line
    | .http url =>
      /- Pure-Lean HTTP — no curl process spawn. Body can be huge
         (screenshots come back as base64 in tool results) but the
         client streams to/from the socket, so argv limits don't
         apply. -/
      LeanTea.Net.HttpClient.postJsonText url req.compress
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"mcp: bad JSON: {e}\n{raw}"
  | .ok j    => return j

/-- Handshake. MCP requires `initialize` before any `tools/*` call. -/
def Mcp.initialize (m : Mcp) : IO Unit := do
  let _ ← sendReq m "initialize" (Json.mkObj [
    ("protocolVersion", Json.str "2024-11-05"),
    ("capabilities", Json.mkObj []),
    ("clientInfo", Json.mkObj [
      ("name", Json.str "browser-agent"),
      ("version", Json.str "0.1.0")
    ])
  ])
  return ()

/-- Pull the tool catalogue. Returns the raw `tools` JSON array as
    advertised by the MCP server. -/
def Mcp.listTools (m : Mcp) : IO (Array Json) := do
  let resp ← sendReq m "tools/list" (Json.mkObj [])
  let result := (resp.getObjVal? "result").toOption.getD Json.null
  let tools  := (result.getObjVal? "tools").toOption.getD (Json.arr #[])
  match tools.getArr? with
  | .ok arr  => return arr
  | .error _ => return #[]

/-- Call a tool. Returns the `content` array Json from the response —
    we hand it straight to the LLM as a tool-result message. -/
def Mcp.callTool (m : Mcp) (name : String) (args : Json) : IO Json := do
  let resp ← sendReq m "tools/call" (Json.mkObj [
    ("name", Json.str name),
    ("arguments", args)
  ])
  /- Surface `result` if present, `error` otherwise — the LLM is more
     useful when it can see the failure mode. -/
  match resp.getObjVal? "result" with
  | .ok r => return r
  | .error _ =>
    let err := (resp.getObjVal? "error").toOption.getD Json.null
    return Json.mkObj [
      ("isError", Json.bool true),
      ("content", Json.arr #[
        Json.mkObj [
          ("type", Json.str "text"),
          ("text", Json.str err.compress)
        ]
      ])
    ]

/-! ## OpenAI request body

We don't use `LeanTea.Llm.Openai.chat` directly because we need
`tools` + `tool_choice` plus mixed-role messages with `tool_calls`,
none of which the structured `ChatRequest` exposes yet. Building the
JSON by hand is small and keeps the dependency one-way. -/

/-- Translate one MCP tool entry to the OpenAI `tools[]` shape:
    `{type:"function", function:{name, description, parameters}}`.
    MCP's `inputSchema` already matches OpenAI's `parameters` (both
    are JSON Schema), so we just rename and wrap. -/
private def mcpToolToOpenAi (t : Json) : Json :=
  let name := (t.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
  let desc := (t.getObjVal? "description").toOption.bind (·.getStr?.toOption) |>.getD ""
  let params := (t.getObjVal? "inputSchema").toOption.getD
    (Json.mkObj [("type", Json.str "object"), ("properties", Json.mkObj [])])
  Json.mkObj [
    ("type", Json.str "function"),
    ("function", Json.mkObj [
      ("name", Json.str name),
      ("description", Json.str desc),
      ("parameters", params)
    ])
  ]

/-- POST one round to LM Studio. Returns the parsed top-level JSON.
    Uses pure-Lean HTTP so we don't need curl in the environment;
    the socket-level transport also handles arbitrarily large bodies
    (screenshots come through here once they've been folded into the
    conversation history). -/
private def chatOnce (cfg : Config) (model : String) (messages : Array Json)
    (tools : Array Json) (maxTokens : Nat) : IO Json := do
  let body := Json.mkObj [
    ("model",      Json.str model),
    ("temperature", Json.num 0),
    ("max_tokens", Json.num (Int.ofNat maxTokens)),
    ("messages",   Json.arr messages),
    ("tools",      Json.arr tools),
    ("tool_choice", Json.str "auto")
  ]
  let url := s!"{cfg.baseUrl}/chat/completions"
  let raw ← LeanTea.Net.HttpClient.postJsonText url body.compress
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"openai: bad JSON\n{e}\n{raw}"
  | .ok j    => return j

/-! ## Agent loop

Standard OpenAI tool-call loop:
  while finish_reason == "tool_calls":
    dispatch each call → append tool message
  print final content. -/

/-- Pretty-print the non-image content of an MCP tool result down to
    a single string the LLM can read in a `role:"tool"` message.
    Image blocks are stripped — we surface them separately via a
    follow-up user message (see `runOneToolCall`). -/
private def renderToolResult (result : Json) : String :=
  let isErr := (result.getObjVal? "isError").toOption.bind (fun j =>
                 match j with | .bool b => some b | _ => none) |>.getD false
  let content := (result.getObjVal? "content").toOption.getD (Json.arr #[])
  let arr := (content.getArr?).toOption.getD #[]
  let parts := arr.map fun item =>
    let typ := (item.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD ""
    match typ with
    | "text"  =>
      (item.getObjVal? "text").toOption.bind (·.getStr?.toOption) |>.getD ""
    | "image" =>
      let mime := (item.getObjVal? "mimeType").toOption.bind (·.getStr?.toOption) |>.getD "image/png"
      s!"(screenshot of mime {mime} attached as the next user message)"
    | _ => item.compress
  let joined := String.intercalate "\n" parts.toList
  if isErr then s!"ERROR: {joined}" else joined

/-- Pull the first `image` block out of a tool result (if any), and
    return it as a `data:` URL ready to drop into a vision message. -/
private def extractImageDataUrl (result : Json) : Option String :=
  let content := (result.getObjVal? "content").toOption.getD (Json.arr #[])
  let arr := (content.getArr?).toOption.getD #[]
  let images := arr.filterMap fun item =>
    let typ := (item.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD ""
    if typ != "image" then none else
      let mime := (item.getObjVal? "mimeType").toOption.bind (·.getStr?.toOption) |>.getD "image/png"
      let data := (item.getObjVal? "data").toOption.bind (·.getStr?.toOption) |>.getD ""
      if data.isEmpty then none else some s!"data:{mime};base64,{data}"
  images[0]?

/-- Dispatch one tool_call object (`{id, function:{name, arguments}}`)
    against MCP, return:
      * the `role:"tool"` message Json (always),
      * an optional `role:"user"` message Json carrying the screenshot
        when the tool produced one — so a vision model can actually
        see what it just took a picture of. -/
private def runOneToolCall (mcp : Mcp) (call : Json) : IO (Json × Option Json) := do
  let id    := (call.getObjVal? "id").toOption.bind (·.getStr?.toOption) |>.getD ""
  let fn    := (call.getObjVal? "function").toOption.getD Json.null
  let name  := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
  let argsS := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
  /- `arguments` is a string (JSON-encoded), per OpenAI spec. -/
  let argsJ := match Json.parse argsS with
    | .ok j => j
    | .error _ => Json.mkObj []
  IO.eprintln s!"  → {name}({argsS})"
  let result ← mcp.callTool name argsJ
  let rendered := renderToolResult result
  /- Truncate long outputs (e.g. full-page text) — Gemma 4 doesn't
     need 200KB of HTML to reason about a click, and oversize logs
     blow the context window on the next round. -/
  let truncated : String :=
    if rendered.length > 4000 then
      (rendered.take 4000).toString ++ s!"\n…[{rendered.length - 4000} more chars omitted]"
    else rendered
  let preview : String := (truncated.take 200).toString
  IO.eprintln s!"  ← {preview}{if truncated.length > 200 then "…" else ""}"
  let toolMsg := Json.mkObj [
    ("role",         Json.str "tool"),
    ("tool_call_id", Json.str id),
    ("name",         Json.str name),
    ("content",      Json.str truncated)
  ]
  /- If the tool result has an image (e.g. browser_screenshot), wrap
     it as a `user` message right after the tool message so the LLM
     can actually look at it. Without this Gemma 4 only ever sees
     text and can't drive a canvas-rendered game. -/
  let imageMsg : Option Json := (extractImageDataUrl result).map fun url =>
    Json.mkObj [
      ("role", Json.str "user"),
      ("content", Json.arr #[
        Json.mkObj [
          ("type", Json.str "text"),
          ("text", Json.str ("Here's the screenshot you just took (the result "
                              ++ "of the previous tool call). Use it to decide "
                              ++ "your next action."))
        ],
        Json.mkObj [
          ("type", Json.str "image_url"),
          ("image_url", Json.mkObj [("url", Json.str url)])
        ]
      ])
    ]
  return (toolMsg, imageMsg)

/-- Per-run timing accumulators. Per-round timings go to stderr as
    `⏱ llm <ms>` / `⏱ tool <ms>` so a downstream script can grep+sum.
    Totals are tallied here and printed in the end-of-run summary. -/
structure Timings where
  llmMs    : IO.Ref Nat
  toolMs   : IO.Ref Nat
  startMs  : Nat

def Timings.fresh : IO Timings := do
  let now ← IO.monoMsNow
  return { llmMs := ← IO.mkRef 0, toolMs := ← IO.mkRef 0, startMs := now }

/-! ## Recording — agent trace → ui_script JSON

When `--record output.json` is set on the CLI, every successful tool
call gets projected to a `ui_script` step and appended to a buffer.
On exit, the buffer is dumped as a script file that — replayed with
`./ui_script <file>` — reproduces the same flow deterministically,
with no LLM cost. -/

/-- Convert one OpenAI tool_call invocation into a `ui_script` step
    JSON, or `none` when the call is bookkeeping (`ui_recall`,
    `ui_list`, `agent_escalate`, …). The key actions we freeze are
    clicks, waits, and screenshots. -/
private def toolCallToScriptStep (call : Json) : Option Json := do
  let fn   ← (call.getObjVal? "function").toOption
  let name := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
  let argsRaw := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
  let args := match Json.parse argsRaw with
    | .ok a    => a
    | .error _ => Json.mkObj []
  match name with
  | "browser_click_xy" =>
    let x := (args.getObjVal? "x").toOption.bind (fun j =>
      match j with | .num n => some n.mantissa.toNat | _ => none) |>.getD 0
    let y := (args.getObjVal? "y").toOption.bind (fun j =>
      match j with | .num n => some n.mantissa.toNat | _ => none) |>.getD 0
    some <| Json.mkObj [
      ("act", Json.str "click_xy"),
      ("x",   Json.num (Int.ofNat x)),
      ("y",   Json.num (Int.ofNat y))
    ]
  | "browser_click" =>
    let sel := (args.getObjVal? "selector").toOption.bind (·.getStr?.toOption) |>.getD ""
    some <| Json.mkObj [
      ("act", Json.str "click_xy"),
      ("note", Json.str s!"(was browser_click selector={sel} — translate by hand)")
    ]
  | "browser_screenshot" => some <| Json.mkObj [("act", Json.str "screenshot")]
  | "browser_evaluate" =>
    let expr := (args.getObjVal? "expression").toOption.bind (·.getStr?.toOption) |>.getD ""
    if expr.startsWith "new Promise(r=>setTimeout" then
      /- Pull the first run of digits — that's the millisecond count
         passed to setTimeout. -/
      let digits := (expr.toList.dropWhile (!·.isDigit)).takeWhile (·.isDigit)
      let ms := digits.foldl (fun n c => n * 10 + (c.toNat - '0'.toNat)) 0
      some <| Json.mkObj [
        ("act", Json.str "wait"),
        ("ms",  Json.num (Int.ofNat ms))
      ]
    else none
  | _ => none

partial def loop (cfg : Config) (model : String) (mcp : Mcp)
    (tools : Array Json) (messages : Array Json) (t : Timings)
    (record? : Option (IO.Ref (Array Json)) := none)
    (round : Nat := 0) (maxRounds : Nat := 12) (maxTokens : Nat := 2000)
    : IO String := do
  if round ≥ maxRounds then
    throw <| IO.userError s!"agent: hit maxRounds={maxRounds} without final answer"
  IO.eprintln s!"── round {round + 1} ─────────────────────────"
  let llmStart ← IO.monoMsNow
  let resp ← chatOnce cfg model messages tools maxTokens
  let llmEnd ← IO.monoMsNow
  let llmDt := llmEnd - llmStart
  t.llmMs.modify (· + llmDt)
  IO.eprintln s!"  ⏱ llm {llmDt}ms"
  /- Error envelope from LM Studio (e.g. model overload). -/
  if let .ok err := resp.getObjVal? "error" then
    let msg := (err.getObjVal? "message").toOption.bind (·.getStr?.toOption) |>.getD err.compress
    throw <| IO.userError s!"openai: {msg}"
  let choices := (resp.getObjVal? "choices").toOption.bind (fun j =>
    (j.getArr?).toOption) |>.getD #[]
  let choice := choices[0]?.getD Json.null
  let message := (choice.getObjVal? "message").toOption.getD Json.null
  let finish  := (choice.getObjVal? "finish_reason").toOption.bind (·.getStr?.toOption) |>.getD ""
  /- Append the assistant message verbatim — tool_calls included —
     since the next round's request needs the full conversation. -/
  let messages := messages.push message
  let toolCalls := (message.getObjVal? "tool_calls").toOption.bind (fun j =>
    (j.getArr?).toOption) |>.getD #[]
  if toolCalls.isEmpty then
    /- No tools requested — this is the final answer. -/
    let content := (message.getObjVal? "content").toOption.bind (·.getStr?.toOption) |>.getD ""
    IO.eprintln s!"  ← finish={finish}, returning final answer"
    return content
  let mut newMsgs := messages
  for call in toolCalls do
    let toolStart ← IO.monoMsNow
    let (toolMsg, imageMsgOpt) ← runOneToolCall mcp call
    let toolEnd ← IO.monoMsNow
    let toolDt := toolEnd - toolStart
    t.toolMs.modify (· + toolDt)
    IO.eprintln s!"  ⏱ tool {toolDt}ms"
    /- Record the call into the replay buffer if `--record` is on
       and the call maps to a known scriptable step. -/
    if let some buf := record? then
      if let some step := toolCallToScriptStep call then
        buf.modify (·.push step)
    newMsgs := newMsgs.push toolMsg
    if let some imageMsg := imageMsgOpt then
      newMsgs := newMsgs.push imageMsg
  loop cfg model mcp tools newMsgs t record? (round + 1) maxRounds maxTokens

/-! ## Entry point -/

private def systemPromptFresh : String :=
  "You drive a real Chromium browser via MCP tools. Your job: " ++
  "complete the user's task by calling the right sequence of tools. " ++
  "Always open the browser first with `browser_open(headless:false)` " ++
  "so the user can watch, then `browser_navigate(url)`, then read " ++
  "whatever is needed (`browser_get_text`, `browser_screenshot`, …) " ++
  "and reason about it. When the task is done, reply to the user in " ++
  "natural language summarising what you did and what you observed. " ++
  "Reply in the same language the user used."

private def systemPromptAttached : String :=
  "You drive a real Chromium browser via MCP tools. A browser " ++
  "session is ALREADY OPEN and possibly logged into a service; " ++
  "DO NOT call `browser_open` or `browser_navigate` to a login " ++
  "page (that would lose the current session). Viewport is 1280×800.\n\n" ++
  "IMPORTANT — this is often a canvas/WebGL game. CSS selectors " ++
  "WILL FAIL. Use `browser_click_xy(x, y)` for OS-level clicks.\n\n" ++
  "## You have a shared UI memory\n\n" ++
  "Before guessing coordinates from a screenshot, ALWAYS call " ++
  "`ui_recall(key)` first — most buttons you need have already " ++
  "been mapped by a previous (often stronger) agent run. Keys " ++
  "are SCREEN-SCOPED like `game.<screen>.<element>` " ++
  "(e.g. `dmm.hub.quest_tab` = the quest tab AS SEEN FROM the hub " ++
  "screen, NOT from anywhere else). Each stored value includes " ++
  "`screen` (the prerequisite screen) and `next_screen` (what you " ++
  "land on after clicking).\n\n" ++
  "Workflow: call `ui_list` at the start, FIRST take a " ++
  "`browser_screenshot` to identify which screen you're on, then " ++
  "pick the matching `<screen>.*` keys. NEVER use a coord whose " ++
  "stored `screen` doesn't match the current screen — that's the " ++
  "#1 way agents get stuck.\n\n" ++
  "After a successful click — VERIFIED by the next screenshot " ++
  "actually matching the expected `next_screen` — call " ++
  "`ui_remember(key, {x, y, screen, next_screen, notes})` " ++
  "so the next run is faster.\n\n" ++
  "## When to escalate\n\n" ++
  "Call `agent_escalate(reason)` and STOP if any of:\n" ++
  "  * You tried 2+ different coordinates for the same target and " ++
  "the screenshot didn't change.\n" ++
  "  * The screen shows something completely unexpected (a CAPTCHA, " ++
  "a login wall, an error dialog, a payment prompt).\n" ++
  "  * The user's task is ambiguous and you'd need to make a " ++
  "judgement call.\n" ++
  "After calling `agent_escalate`, give the user a brief plain-text " ++
  "summary of where you got stuck.\n\n" ++
  "## Standard workflow each round\n\n" ++
  "  1. `browser_screenshot` — image attached as next user message.\n" ++
  "  2. Identify next target. Try `ui_recall(\"that_button\")` first.\n" ++
  "  3. `browser_click_xy(x, y)`.\n" ++
  "  4. `browser_screenshot` to verify the result.\n" ++
  "  5. If verified, optionally `ui_remember` to cache.\n" ++
  "  6. Repeat until task is done, then summarise to user.\n\n" ++
  "Reply in the same language the user used."

private structure Args where
  task     : String := ""
  mcpUrl   : Option String := none
  /-- When set, the agent writes a `ui_script`-compatible JSON of the
      successful tool calls it made. Re-running that file via
      `ui_script` skips the LLM entirely. -/
  recordTo : Option String := none

private partial def parseArgs (xs : List String) (acc : Args) : Args :=
  match xs with
  | "--mcp-url" :: v :: rest => parseArgs rest { acc with mcpUrl := some v }
  | "--record" :: v :: rest  => parseArgs rest { acc with recordTo := some v }
  | x :: rest                =>
    let sep := if acc.task.isEmpty then "" else " "
    parseArgs rest { acc with task := acc.task ++ sep ++ x }
  | []                       => acc

def main (rawArgs : List String) : IO Unit := do
  let a := parseArgs rawArgs {}
  /- If no `--mcp-url` was given and no positional task either, read
     the task from stdin so this exe can be used in a pipeline. -/
  let task ←
    if a.task.isEmpty then
      let s ← (← IO.getStdin).readToEnd
      pure s.trimAscii.toString
    else
      pure a.task
  if task.isEmpty then
    IO.eprintln "usage: browser_agent [--mcp-url URL] <task>"
    IO.Process.exit 2

  let baseUrl := (← IO.getEnv "LMSTUDIO_BASE_URL").getD "http://127.0.0.1:11211/v1"
  let model   := (← IO.getEnv "LMSTUDIO_MODEL").getD "google/gemma-4-e4b"
  let cfg : Config := { baseUrl, timeoutSec := some 300 }

  IO.eprintln s!"browser-agent: model={model}"
  IO.eprintln s!"browser-agent: task={task}"

  let mcp ← match a.mcpUrl with
    | some url =>
      IO.eprintln s!"browser-agent: attaching to existing MCP at {url}"
      Mcp.connect url
    | none     =>
      IO.eprintln "browser-agent: spawning fresh MCP child"
      Mcp.spawn
  let systemPrompt := if a.mcpUrl.isSome then systemPromptAttached else systemPromptFresh
  try
    mcp.initialize
    let mcpTools ← mcp.listTools
    /- Drop `browser_close`: the agent owns lifecycle (we close at
       exit via the `finally` block below), and we've seen Gemma 4
       eagerly call close mid-task as a "cleanup" step, which then
       blocks the next round on a fresh `browser_open`. -/
    let mcpToolsFiltered := mcpTools.filter fun t =>
      let name := (t.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
      name != "browser_close"
    let openAiTools := mcpToolsFiltered.map mcpToolToOpenAi
    IO.eprintln s!"browser-agent: loaded {openAiTools.size} MCP tools (filtered from {mcpTools.size})"

    let messages : Array Json := #[
      Json.mkObj [("role", Json.str "system"), ("content", Json.str systemPrompt)],
      Json.mkObj [("role", Json.str "user"),   ("content", Json.str task)]
    ]
    let maxRounds := (← IO.getEnv "BROWSER_AGENT_MAX_ROUNDS").bind (·.toNat?)
                       |>.getD 12
    IO.eprintln s!"browser-agent: maxRounds={maxRounds}"
    let timings ← Timings.fresh
    let record? : Option (IO.Ref (Array Json)) ← match a.recordTo with
      | some _ => pure (some (← IO.mkRef #[]))
      | none   => pure none
    let answer ← loop cfg model mcp openAiTools messages timings record?
                   (maxRounds := maxRounds)
    let endMs ← IO.monoMsNow
    let llm ← timings.llmMs.get
    let tool ← timings.toolMs.get
    let total := endMs - timings.startMs
    let other := if total ≥ llm + tool then total - llm - tool else 0
    IO.eprintln "── timing summary ─────────────────────────"
    IO.eprintln s!"  wall   {total}ms (= {total/1000}.{(total%1000)/100}s)"
    IO.eprintln s!"  llm    {llm}ms  ({(llm * 100 / (total.max 1))}% of wall) ← agent inference"
    IO.eprintln s!"  tool   {tool}ms ({(tool * 100 / (total.max 1))}% of wall) ← MCP/browser/game"
    IO.eprintln s!"  other  {other}ms ({(other * 100 / (total.max 1))}% of wall) ← parsing, JSON, file I/O"
    /- Flush the recording if it was requested. The output is a
       `ui_script`-compatible file: drop it into `./ui_script
       <file>` and replay deterministically without any LLM cost. -/
    if let some path := a.recordTo then
      if let some buf := record? then
        let steps ← buf.get
        let scriptName :=
          let base := (System.FilePath.mk path).fileStem.getD "recorded"
          base
        let script := Json.mkObj [
          ("name",        Json.str scriptName),
          ("description", Json.str s!"recorded from browser_agent run: {task}"),
          ("steps",       Json.arr steps)
        ]
        IO.FS.writeFile path (script.pretty 2)
        IO.eprintln s!"── recorded {steps.size} step(s) → {path}"
    IO.println answer
  finally
    mcp.close

end BrowserAgent

def main (args : List String) : IO Unit := BrowserAgent.main args
