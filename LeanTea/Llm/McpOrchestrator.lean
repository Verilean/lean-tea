import LeanTea.Llm.Openai
import LeanTea.Llm.Policy
import LeanTea.Net.HttpClient
import Lean.Data.Json

/-! # LeanTea.Llm.McpOrchestrator — chat-loop driver over N MCP servers

Generalised version of the `BrowserAgent` loop: spawn an arbitrary
list of MCP servers (each as a stdio child or an HTTP endpoint),
aggregate their tool catalogues into one OpenAI `tools[]` array,
and run the standard tool-call chat loop against an OpenAI-compatible
backend (LM Studio with a local Gemma, OpenAI proper, etc.).

The three demo apps under `examples/LlmChat{Cli,Tui,Web}/` all sit
on top of this; the orchestrator owns:

  * MCP child lifecycle (spawn / initialize / list-tools / dispatch / close)
  * Name-prefix routing — tool names become `<server>__<tool>` so two
    servers can't collide
  * The chat history shape and the LLM call (`runTurn`)

The UI layers only deal with rendering and input.

## Wire shape

We talk to the LLM via the OpenAI Chat-Completions API (`/chat/completions`
with `tools[]` + `tool_choice:"auto"`). LM Studio speaks the same dialect,
so a local Gemma works without changing this code. The MCP servers speak
JSON-RPC 2.0 framed line-by-line over stdio (one request, one newline,
one response, one newline) — same wire `Claude Desktop` uses.

## Config shape

```json
{
  "model": "google/gemma-4-e4b",
  "baseUrl": "http://127.0.0.1:11211/v1",
  "system": "You are a helpful assistant. Reply in the user's language.",
  "servers": [
    { "name": "chrome", "bin": "./.lake/build/bin/chrome_cdp_mcp_serve", "args": [] },
    { "name": "gemini", "bin": "./.lake/build/bin/gemini_mcp_serve",
      "args": ["--workspace", "."] }
  ]
}
```

The `name` becomes the tool-name prefix shown to the LLM: a tool
called `chrome_navigate` on the `chrome` server becomes
`chrome__chrome_navigate` in the catalogue. (We keep the original
tool name as the second half; some MCP servers already prefix their
tools — that's fine, the extra prefix just disambiguates.) -/

namespace LeanTea.Llm.McpOrchestrator

open Lean (Json)
open LeanTea.Llm.Openai (Config)

/-! ## Server spec / wire client -/

structure ServerSpec where
  /-- Short, unique handle. Becomes the tool-name prefix. -/
  name : String
  /-- Either an absolute / relative path to an executable (stdio
      transport) or an `http://…/mcp` URL (HTTP transport). -/
  bin : String
  /-- CLI args for the stdio binary. Ignored for HTTP. -/
  args : Array String := #[]
  deriving Inhabited, Repr

/-- Two transports — same shape as `BrowserAgent.Mcp` but exposed
    publicly so the orchestrator can drive any MCP server. -/
inductive McpKind where
  | stdio (child : IO.Process.Child { stdin := .piped, stdout := .piped, stderr := .piped })
  | http  (url : String)

structure Mcp where
  kind   : McpKind
  nextId : IO.Ref Nat

private def Mcp.spawnStdio (bin : String) (args : Array String) : IO Mcp := do
  /- Unset PORT in the child env. Several LeanTEA MCP servers
     (gemini, tmux, browser, …) switch themselves into HTTP mode
     when PORT is set, which silently breaks the stdio handshake
     and steals the parent's port. -/
  let child ← IO.Process.spawn {
    cmd := bin, args,
    env := #[("PORT", none)],
    stdin := .piped, stdout := .piped, stderr := .piped
  }
  let nextId ← IO.mkRef 1
  return { kind := .stdio child, nextId }

private def Mcp.connectHttp (url : String) : IO Mcp := do
  let nextId ← IO.mkRef 1
  return { kind := .http url, nextId }

private def Mcp.spawn (spec : ServerSpec) : IO Mcp := do
  if spec.bin.startsWith "http://" || spec.bin.startsWith "https://" then
    Mcp.connectHttp spec.bin
  else
    Mcp.spawnStdio spec.bin spec.args

private def Mcp.close (m : Mcp) : IO Unit := do
  match m.kind with
  | .stdio child =>
    let (_, child') ← child.takeStdin
    let _ ← child'.wait
  | .http _ => return ()

private def Mcp.sendReq (m : Mcp) (method : String) (params : Json) : IO Json := do
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
      LeanTea.Net.HttpClient.postJsonText url req.compress
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"mcp: bad JSON: {e}\n{raw}"
  | .ok j    => return j

private def Mcp.initialize (m : Mcp) : IO Unit := do
  let _ ← m.sendReq "initialize" (Json.mkObj [
    ("protocolVersion", Json.str "2024-11-05"),
    ("capabilities",    Json.mkObj []),
    ("clientInfo",      Json.mkObj [
      ("name",    Json.str "leantea-llm-chat"),
      ("version", Json.str "0.1.0")
    ])
  ])

private def Mcp.listTools (m : Mcp) : IO (Array Json) := do
  let resp ← m.sendReq "tools/list" (Json.mkObj [])
  let result := (resp.getObjVal? "result").toOption.getD Json.null
  let tools  := (result.getObjVal? "tools").toOption.getD (Json.arr #[])
  match tools.getArr? with
  | .ok arr  => return arr
  | .error _ => return #[]

private def Mcp.callTool (m : Mcp) (name : String) (args : Json) : IO Json := do
  let resp ← m.sendReq "tools/call" (Json.mkObj [
    ("name",      Json.str name),
    ("arguments", args)
  ])
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

/-! ## Tool-name prefixing

Each tool gets the form `<server>__<tool>`. We pick `__` as the
separator because: it's two ASCII chars (one round trip through the
LLM's tokenizer = 1 token), it's never legal in an MCP tool name
(which match `[a-zA-Z_][a-zA-Z0-9_]*`-ish), and round-tripping is
trivial via `splitOn`. -/

private def prefixSep : String := "__"

private def prefixedToolName (server tool : String) : String :=
  server ++ prefixSep ++ tool

private def splitPrefixedName (full : String) : Option (String × String) :=
  match full.splitOn prefixSep with
  | server :: rest@(_ :: _) => some (server, String.intercalate prefixSep rest)
  | _ => none

/-! ## OpenAI tool conversion -/

/-- Convert one MCP tool entry to the OpenAI `tools[]` shape. The
    `name` is rewritten to its prefixed form so collisions across
    MCP servers can't happen, and so we can route on dispatch. -/
private def mcpToolToOpenAi (serverName : String) (t : Json) : Json :=
  let name := (t.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
  let desc := (t.getObjVal? "description").toOption.bind (·.getStr?.toOption) |>.getD ""
  let params := (t.getObjVal? "inputSchema").toOption.getD
    (Json.mkObj [("type", Json.str "object"), ("properties", Json.mkObj [])])
  Json.mkObj [
    ("type", Json.str "function"),
    ("function", Json.mkObj [
      ("name",        Json.str (prefixedToolName serverName name)),
      ("description", Json.str s!"[{serverName}] {desc}"),
      ("parameters",  params)
    ])
  ]

/-! ## Orchestrator -/

structure RunningServer where
  spec  : ServerSpec
  mcp   : Mcp
  /-- Raw MCP tool entries (unprefixed names). Kept for diagnostics. -/
  tools : Array Json
  /-- Tool entries already converted to OpenAI shape, with prefixed names. -/
  openAiTools : Array Json

structure Orchestrator where
  cfg          : Config
  model        : String
  systemPrompt : String
  servers      : Array RunningServer
  /-- Per-LLM-call upper bound on max_tokens. -/
  maxTokens    : Nat := 2000
  /-- Per-turn upper bound on tool-call rounds. -/
  maxRounds    : Nat := 12

/-- Spawn every server, run the MCP handshake, and gather tools. -/
def spawnAll (cfg : Config) (model systemPrompt : String)
    (specs : Array ServerSpec) : IO Orchestrator := do
  let mut running : Array RunningServer := #[]
  for spec in specs do
    let mcp ← Mcp.spawn spec
    try
      mcp.initialize
      let rawTools ← mcp.listTools
      let openAi := rawTools.map (mcpToolToOpenAi spec.name)
      running := running.push { spec, mcp, tools := rawTools, openAiTools := openAi }
    catch e =>
      mcp.close
      throw <| IO.userError s!"mcp server `{spec.name}` ({spec.bin}): {e}"
  return { cfg, model, systemPrompt, servers := running }

def Orchestrator.shutdown (o : Orchestrator) : IO Unit := do
  for s in o.servers do
    try s.mcp.close catch _ => pure ()

/-- The aggregated OpenAI tool catalogue across every server. -/
def Orchestrator.openAiTools (o : Orchestrator) : Array Json := Id.run do
  let mut all : Array Json := #[]
  for s in o.servers do
    all := all ++ s.openAiTools
  return all

/-- Route a prefixed tool name back to the owning server, then call it. -/
def Orchestrator.callTool (o : Orchestrator) (fullName : String) (args : Json)
    : IO Json := do
  match splitPrefixedName fullName with
  | none =>
    throw <| IO.userError s!"orchestrator: tool name `{fullName}` is not prefixed"
  | some (serverName, toolName) =>
    match o.servers.find? (·.spec.name == serverName) with
    | none =>
      throw <| IO.userError s!"orchestrator: no server named `{serverName}`"
    | some s => s.mcp.callTool toolName args

/-! ## LLM call (OpenAI chat-completions with tools) -/

/-- POST one round to the LLM. Returns the parsed top-level response. -/
private def chatOnce (o : Orchestrator) (messages : Array Json) : IO Json := do
  let body := Json.mkObj [
    ("model",       Json.str o.model),
    ("temperature", Json.num 0),
    ("max_tokens",  Json.num (Int.ofNat o.maxTokens)),
    ("messages",    Json.arr messages),
    ("tools",       Json.arr o.openAiTools),
    ("tool_choice", Json.str "auto")
  ]
  let url := s!"{o.cfg.baseUrl}/chat/completions"
  let raw ← LeanTea.Net.HttpClient.postJsonText url body.compress
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"openai: bad JSON\n{e}\n{raw}"
  | .ok j    => return j

/-! ## Tool result rendering

A tool result is an MCP `{content: [...]}` object where each item is
either `{type:"text",text:...}` or `{type:"image",mimeType:...,data:<base64>}`.
We split these:

  * `renderToolResult` joins all text parts into one string (which goes
    back to the LLM via the `role:"tool"` message — OpenAI tool messages
    only carry strings, not multimodal blocks).
  * `extractToolImages` pulls every image block out as a `data:` URL,
    ready for the UI to render and (for vision models) for the next
    LLM round to actually look at via a synthetic user message. -/

private def renderToolResult (result : Json) : String :=
  let isErr := (result.getObjVal? "isError").toOption.bind (fun j =>
                 match j with | .bool b => some b | _ => none) |>.getD false
  let content := (result.getObjVal? "content").toOption.getD (Json.arr #[])
  let arr := (content.getArr?).toOption.getD #[]
  let parts := arr.map fun item =>
    let typ := (item.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD ""
    match typ with
    | "text" =>
      (item.getObjVal? "text").toOption.bind (·.getStr?.toOption) |>.getD ""
    | "image" =>
      let mime := (item.getObjVal? "mimeType").toOption.bind (·.getStr?.toOption) |>.getD "image/png"
      s!"(image: {mime} — shown to the user; also attached as the next vision message)"
    | _ => item.compress
  let joined := String.intercalate "\n" parts.toList
  if isErr then s!"ERROR: {joined}" else joined

/-- Pull every `image` content block out of a tool result as
    `data:<mime>;base64,<…>` URLs. -/
private def extractToolImages (result : Json) : Array String :=
  let content := (result.getObjVal? "content").toOption.getD (Json.arr #[])
  let arr := (content.getArr?).toOption.getD #[]
  arr.filterMap fun item =>
    let typ := (item.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD ""
    if typ != "image" then none else
      let mime := (item.getObjVal? "mimeType").toOption.bind (·.getStr?.toOption) |>.getD "image/png"
      let data := (item.getObjVal? "data").toOption.bind (·.getStr?.toOption) |>.getD ""
      if data.isEmpty then none else some s!"data:{mime};base64,{data}"

/-! ## Public chat shape

A higher-level message representation that the UI layers render. We
keep both this and the raw Json (`raw`) so the UI can show pretty
bubbles while the next LLM round still gets the exact OpenAI shape. -/

inductive Role where
  | system | user | assistant | tool
  deriving DecidableEq, Repr

instance : Inhabited Role := ⟨.user⟩

def Role.toString : Role → String
  | .system => "system" | .user => "user"
  | .assistant => "assistant" | .tool => "tool"

instance : ToString Role := ⟨Role.toString⟩

structure ChatMsg where
  role : Role
  /-- Human-visible text. For `assistant` messages that are pure
      tool calls this can be empty; the call list lives in `toolCalls`. -/
  text : String := ""
  /-- Attached images as `data:<mime>;base64,…` URLs. Populated for
      user messages (uploads) and for tool messages (tool results that
      included image content blocks). The renderer for both UI and
      LLM uses this to surface images. -/
  images : Array String := #[]
  /-- For `assistant` messages that called tools — the raw call list
      from OpenAI. Each entry has `id` / `function.name` / `function.arguments`. -/
  toolCalls : Array Json := #[]
  /-- For `tool` messages — which call id this is responding to. -/
  toolCallId : String := ""
  /-- For `tool` messages — the prefixed tool name (so the UI can
      render `chrome__chrome_navigate` etc.). -/
  toolName : String := ""
  deriving Inhabited

/-- Build an OpenAI multi-block `content` array from a text part and
    a list of image data URLs. Used for `user` messages that carry
    attachments (file uploads, paste, drag-drop). -/
private def multiBlockContent (text : String) (images : Array String) : Json :=
  let textBlock := Json.mkObj [
    ("type", Json.str "text"),
    ("text", Json.str text)
  ]
  let imgBlocks := images.map fun url =>
    Json.mkObj [
      ("type",      Json.str "image_url"),
      ("image_url", Json.mkObj [("url", Json.str url)])
    ]
  Json.arr (#[textBlock] ++ imgBlocks)

/-- Serialise a `ChatMsg` back to the OpenAI message Json shape. User
    messages with attached images use the multi-block content shape
    (`[{type:"text",...}, {type:"image_url",...}]`); everything else
    is a plain string content. -/
def ChatMsg.toJson (m : ChatMsg) : Json :=
  match m.role with
  | .assistant =>
    let base : List (String × Json) := [
      ("role",    Json.str "assistant"),
      ("content", Json.str m.text)
    ]
    if m.toolCalls.isEmpty then Json.mkObj base
    else Json.mkObj (base ++ [("tool_calls", Json.arr m.toolCalls)])
  | .tool =>
    Json.mkObj [
      ("role",         Json.str "tool"),
      ("tool_call_id", Json.str m.toolCallId),
      ("name",         Json.str m.toolName),
      ("content",      Json.str m.text)
    ]
  | r =>
    let content : Json :=
      if m.images.isEmpty then Json.str m.text
      else multiBlockContent m.text m.images
    Json.mkObj [
      ("role",    Json.str r.toString),
      ("content", content)
    ]

/-- Build the OpenAI messages array for a request, prepending the
    system prompt automatically. -/
private def Orchestrator.toRequestMessages (o : Orchestrator)
    (history : Array ChatMsg) : Array Json :=
  let sys := Json.mkObj [
    ("role",    Json.str "system"),
    ("content", Json.str o.systemPrompt)
  ]
  #[sys] ++ history.map ChatMsg.toJson

/-! ## One turn of the chat loop

Append the user's new message, then loop until the LLM stops asking
for tools. Returns the appended messages so the UI can render the
intermediate tool calls + results, not just the final answer. -/

/-- What the user / policy decided about a specific tool invocation.
    `allowOnce` / `denyOnce` apply only to this one call;
    `allowAlways` / `denyAlways` also persist a new rule. -/
inductive UserDecision where
  | allowOnce
  | denyOnce
  | allowAlways
  | denyAlways
  deriving DecidableEq, Repr, Inhabited

/-- Optional callbacks so a UI can stream visible progress (typing
    indicator, tool-call notifications). All optional — pass `{}` to
    suppress. -/
structure ProgressHooks where
  /-- Called just before each LLM round. `round` is 0-indexed. -/
  onLlmStart : Nat → IO Unit := fun _ => pure ()
  /-- Called as soon as the LLM responds, before any tool dispatch. -/
  onLlmEnd   : Nat → IO Unit := fun _ => pure ()
  /-- Called for each tool call, before dispatch. `(name, argsJson)`. -/
  onToolCall : String → Json → IO Unit := fun _ _ => pure ()
  /-- Called after a tool call returns. `(name, rendered)`. -/
  onToolResult : String → String → IO Unit := fun _ _ => pure ()
  /-- Called when policy says `ask` — UI prompts the user and returns
      the decision. The default rejects (`denyOnce`) so a hookless
      caller can't be exploited by a tool that hasn't been allowed.
      `(toolName, args)`. -/
  onAsk : String → Json → IO UserDecision := fun _ _ => pure .denyOnce
  deriving Inhabited

/-- Bundle of the live policy ref + a hook for asking the user. When
    `policy` is `none` the orchestrator skips the check (legacy
    behaviour — same as the old `runTurnFull` signature). -/
structure PolicyConfig where
  policy : Option LeanTea.Llm.Policy.LiveRef := none
  deriving Inhabited

/-- Run one user turn. `history` is the existing conversation (without
    the new user message). `userImages` are `data:` URLs attached
    alongside the prompt (file uploads, drag-drop, clipboard paste —
    callers do the base64 conversion). Returns the new messages
    appended to history.

    Tool results that include image content blocks are surfaced in
    two places: the tool `ChatMsg` carries them as `images` (so the
    UI can render them inline), and a synthetic `user` message with
    those images is inserted right after the tool message so a
    vision-capable LLM can actually look at them next round.

    On the LLM/MCP side, errors are surfaced via `throw` — let the
    UI decide whether to render a red banner or rethrow. -/
partial def Orchestrator.runTurnFull (o : Orchestrator) (history : Array ChatMsg)
    (userInput : String) (userImages : Array String := #[])
    (hooks : ProgressHooks := {}) (policy : PolicyConfig := {})
    : IO (Array ChatMsg) := do
  let userMsg : ChatMsg := {
    role := .user, text := userInput, images := userImages
  }
  let mut acc : Array ChatMsg := #[userMsg]
  let mut working : Array ChatMsg := history.push userMsg
  let mut round : Nat := 0
  /- Bounded loop: each iteration is one LLM round-trip. We exit when
     the assistant returns no tool_calls. -/
  for _ in [0:o.maxRounds] do
    if round ≥ o.maxRounds then break
    hooks.onLlmStart round
    let resp ← chatOnce o (o.toRequestMessages working)
    hooks.onLlmEnd round
    /- Error envelope. -/
    if let .ok err := resp.getObjVal? "error" then
      let msg := (err.getObjVal? "message").toOption.bind (·.getStr?.toOption) |>.getD err.compress
      throw <| IO.userError s!"llm: {msg}"
    let choices := (resp.getObjVal? "choices").toOption.bind (fun j =>
      (j.getArr?).toOption) |>.getD #[]
    let choice := choices[0]?.getD Json.null
    let message := (choice.getObjVal? "message").toOption.getD Json.null
    let content := (message.getObjVal? "content").toOption.bind (·.getStr?.toOption) |>.getD ""
    let toolCalls := (message.getObjVal? "tool_calls").toOption.bind (fun j =>
      (j.getArr?).toOption) |>.getD #[]
    let assistantMsg : ChatMsg := {
      role := .assistant,
      text := content,
      toolCalls
    }
    acc := acc.push assistantMsg
    working := working.push assistantMsg
    if toolCalls.isEmpty then
      /- Final answer for this turn. -/
      return acc
    /- Dispatch each tool call, append a tool message, loop. -/
    for call in toolCalls do
      let id    := (call.getObjVal? "id").toOption.bind (·.getStr?.toOption) |>.getD ""
      let fn    := (call.getObjVal? "function").toOption.getD Json.null
      let name  := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
      let argsS := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
      let argsJ := match Json.parse argsS with
        | .ok j    => j
        | .error _ => Json.mkObj []
      hooks.onToolCall name argsJ
      /- Policy gate. If no live policy is configured, we skip this
         and just dispatch (legacy behaviour). Otherwise: check the
         rules; on `ask` invoke the UI hook; on `deny` synthesize a
         tool error rather than throwing so the LLM can recover. -/
      let decision : LeanTea.Llm.Policy.Decision ←
        match policy.policy with
        | none    => pure LeanTea.Llm.Policy.Decision.allow
        | some lr =>
          match ← lr.check name with
          | .allow => pure LeanTea.Llm.Policy.Decision.allow
          | .deny  => pure LeanTea.Llm.Policy.Decision.deny
          | .ask   =>
            match ← hooks.onAsk name argsJ with
            | .allowOnce   => pure LeanTea.Llm.Policy.Decision.allow
            | .denyOnce    => pure LeanTea.Llm.Policy.Decision.deny
            | .allowAlways =>
              lr.append { pattern := name, action := LeanTea.Llm.Policy.Action.allow }
              pure LeanTea.Llm.Policy.Decision.allow
            | .denyAlways  =>
              lr.append { pattern := name, action := LeanTea.Llm.Policy.Action.deny }
              pure LeanTea.Llm.Policy.Decision.deny
      let result ← match decision with
        | LeanTea.Llm.Policy.Decision.deny =>
          pure <| Json.mkObj [
            ("isError", Json.bool true),
            ("content", Json.arr #[Json.mkObj [
              ("type", Json.str "text"),
              ("text", Json.str s!"policy: tool `{name}` was denied by the user")
            ]])
          ]
        | _ =>
          try o.callTool name argsJ
          catch e => pure <| Json.mkObj [
            ("isError", Json.bool true),
            ("content", Json.arr #[Json.mkObj [
              ("type", Json.str "text"),
              ("text", Json.str s!"orchestrator: {e}")
            ]])
          ]
      let rendered := renderToolResult result
      let truncated :=
        if rendered.length > 4000 then
          (rendered.take 4000).toString ++ s!"\n…[{rendered.length - 4000} more chars omitted]"
        else rendered
      let images := extractToolImages result
      hooks.onToolResult name truncated
      let toolMsg : ChatMsg := {
        role := .tool,
        text := truncated,
        images,
        toolCallId := id,
        toolName := name
      }
      acc := acc.push toolMsg
      working := working.push toolMsg
      /- For vision-capable LLMs, append a synthetic user message with
         the images so they're visible next round. Tool messages
         carry only strings in the OpenAI spec; vision content has to
         live on a user message. We hide this from the UI via
         `synthetic := true`-style intent — the role stays `.user` but
         the empty text + non-empty images keeps it visually unobtrusive
         when the UI checks `m.text.isEmpty && m.images.isEmpty`. -/
      unless images.isEmpty do
        let visionMsg : ChatMsg := {
          role   := .user,
          text   := s!"(image attached above is the result of `{name}` — \
look at it to decide what to do next.)",
          images
        }
        working := working.push visionMsg
        /- We don't push the synthetic message into `acc` because the
           UI already renders the tool message's images; surfacing it
           twice would be confusing. The next round's request still
           uses `working`, so the LLM sees the images. -/
    round := round + 1
  /- Hit maxRounds without a tool-free reply. Surface as an error
     so the UI can show "agent gave up" rather than silently
     truncating. -/
  throw <| IO.userError s!"orchestrator: hit maxRounds={o.maxRounds} \
without a final answer"

/-- Image-less convenience wrapper. The three demo apps default to
    this when the caller doesn't attach images. -/
def Orchestrator.runTurn (o : Orchestrator) (history : Array ChatMsg)
    (userInput : String) (hooks : ProgressHooks := {})
    : IO (Array ChatMsg) :=
  o.runTurnFull history userInput #[] hooks

/-! ## Config JSON parsing

The three demo apps all accept `--config path/to/cfg.json`. We parse
it here so the UI binaries don't repeat the same code. -/

private def parseServerSpec (j : Json) : Except String ServerSpec := do
  let name ← (j.getObjVal? "name").bind (·.getStr?)
  let bin  ← (j.getObjVal? "bin").bind (·.getStr?)
  let args :=
    match (j.getObjVal? "args").toOption.bind (·.getArr?.toOption) with
    | some a => a.filterMap (·.getStr?.toOption)
    | none   => #[]
  return { name, bin, args }

structure FileConfig where
  model        : String
  baseUrl      : String
  systemPrompt : String
  servers      : Array ServerSpec
  deriving Inhabited

def loadConfig (path : String) : IO FileConfig := do
  let src ← IO.FS.readFile path
  match Json.parse src with
  | .error e => throw <| IO.userError s!"config {path}: bad JSON: {e}"
  | .ok j =>
    let model   := (j.getObjVal? "model").toOption.bind (·.getStr?.toOption)
      |>.getD "google/gemma-4-e4b"
    let baseUrl := (j.getObjVal? "baseUrl").toOption.bind (·.getStr?.toOption)
      |>.getD "http://127.0.0.1:11211/v1"
    let systemPrompt := (j.getObjVal? "system").toOption.bind (·.getStr?.toOption)
      |>.getD "You are a helpful assistant. Reply in the user's language."
    let serversJ := (j.getObjVal? "servers").toOption.bind (·.getArr?.toOption)
      |>.getD #[]
    let mut specs : Array ServerSpec := #[]
    for sj in serversJ do
      match parseServerSpec sj with
      | .ok s    => specs := specs.push s
      | .error e => throw <| IO.userError s!"config {path}: bad server entry: {e}"
    return { model, baseUrl, systemPrompt, servers := specs }

/-- Convenience: load config + spawn everything. -/
def fromConfig (fc : FileConfig) : IO Orchestrator := do
  let cfg : Config := { baseUrl := fc.baseUrl, timeoutSec := some 300 }
  spawnAll cfg fc.model fc.systemPrompt fc.servers

end LeanTea.Llm.McpOrchestrator
