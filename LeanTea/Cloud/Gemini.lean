import LeanTea.Net.SafePath
import LeanTea.Json.Helpers
import Lean.Data.Json

/-! # LeanTea.Cloud.Gemini — Google Gemini API client

A tiny client for the `v1beta` REST surface of the Gemini API.
Lean has no native TLS so HTTPS goes through `curl(1)` — same
shape as `LeanTea.Auth.OAuth2`.

## Models (as of 2026)

| model | input ctx | use-case |
|---|---|---|
| `gemini-2.5-pro` (default) | 2,097,152 | heavy code / architecture review |
| `gemini-2.5-flash`         | 1,048,576 | fast + cheap general-purpose |
| `gemini-2.5-flash-lite`    | 1,048,576 | summarisation / cheap lint |

For long-context use cases, `Gemini.reviewMany` bundles many
files through `SafePath` into a single Markdown prompt and ships
the lot in one request.

## Auth

Reads `GEMINI_API_KEY` from the environment. Issue a key at
https://aistudio.google.com/apikey. Free tier on `gemini-2.5-flash`
is 15 req/min / 1500 req/day.

## Example

```lean
let cfg ← Gemini.Config.fromEnv!
let resp ← Gemini.ask cfg "Hello, who are you?" {}
IO.println resp.text
IO.println s!"tokens in={resp.usage.input} out={resp.usage.output}"
```

```lean
-- Holistic multi-file review
let resp ← Gemini.reviewMany cfg
  (workspace := "/path/to/repo")
  (paths := #["LeanTea/Net/Http.lean", "LeanTea/Net/Server.lean"])
  (prompt := "Architecture review: call out design weaknesses")
  {}
``` -/

namespace LeanTea.Cloud.Gemini

open LeanTea.Net (SafePath)
open Lean (Json)

/-! ## Config -/

structure Config where
  apiKey   : String
  /-- Default `gemini-2.5-pro`. Override per-call via `CallOpts.model`. -/
  model    : String := "gemini-2.5-pro"
  endpoint : String := "https://generativelanguage.googleapis.com/v1beta"
  /-- Max output tokens. `none` uses the model default. -/
  maxOutputTokens : Option Nat := none
  /-- Temperature (`0.0` = deterministic). `none` uses the model default. -/
  temperature : Option Float := none
  /-- curl timeout in seconds. Long-context generations can take a
      while, so the default is 300s (5 min). -/
  timeoutSec : Nat := 300
  deriving Inhabited, Repr

/-- Build a `Config` from environment variables. `GEMINI_API_KEY`
    is required; `GEMINI_MODEL` / `GEMINI_ENDPOINT` are optional
    overrides. -/
def Config.fromEnv? : IO (Option Config) := do
  match ← IO.getEnv "GEMINI_API_KEY" with
  | none => return none
  | some key =>
    let model := (← IO.getEnv "GEMINI_MODEL").getD "gemini-2.5-pro"
    let endpoint := (← IO.getEnv "GEMINI_ENDPOINT").getD
      "https://generativelanguage.googleapis.com/v1beta"
    return some { apiKey := key, model, endpoint }

/-- Panic variant — useful in CI to fail fast when the key is
    missing rather than silently degrading. -/
def Config.fromEnv! : IO Config := do
  match ← Config.fromEnv? with
  | some c => return c
  | none   => throw (IO.userError
    "Gemini.Config.fromEnv!: GEMINI_API_KEY is not set")

/-! ## Request / Response types -/

/-- One message in a multi-turn chat. -/
structure Message where
  /-- `"user"` or `"model"`. -/
  role : String
  text : String
  deriving Inhabited, Repr

/-- Per-call options. All optional — pass `{}` for defaults. -/
structure CallOpts where
  /-- Override `Config.model`. -/
  model         : Option String := none
  /-- System prompt. Maps to Gemini's `systemInstruction` field. -/
  system        : Option String := none
  /-- Override `Config.maxOutputTokens`. -/
  maxOutputTokens : Option Nat := none
  /-- Override `Config.temperature`. -/
  temperature   : Option Float := none
  /-- Response JSON schema (structured output). -/
  jsonSchema    : Option Json := none
  deriving Inhabited

structure Usage where
  input  : Nat := 0
  output : Nat := 0
  total  : Nat := 0
  deriving Inhabited, Repr

structure Response where
  text  : String
  usage : Usage
  /-- Finish reason (`STOP` / `MAX_TOKENS` / `SAFETY` / …). -/
  finishReason : String := ""
  /-- Model that actually served the request (resolved at call time). -/
  model : String := ""
  /-- Raw response body — kept around for debugging / logging. -/
  raw : String := ""
  deriving Inhabited, Repr

/-! ## curl wrapper — TLS needs shell-out -/

private structure CurlResp where
  status : Nat
  body   : String
  err    : String := ""

/-- POST a JSON body to `url` via curl, returning status + body.
    The request body is staged through a temp file so very large
    prompts work and the bytes are passed binary-safe. -/
private def curlPost (url : String) (jsonBody : String) (timeoutSec : Nat)
    : IO CurlResp := do
  let tmpDir := (← IO.getEnv "TMPDIR").getD "/tmp"
  let bodyFile := s!"{tmpDir}/gemini-req-{← IO.rand 0 0xffff_ffff}.json"
  let respFile := s!"{tmpDir}/gemini-resp-{← IO.rand 0 0xffff_ffff}.json"
  IO.FS.writeFile bodyFile jsonBody
  let out ← IO.Process.output {
    cmd := "curl",
    args := #[
      "-sS",
      "--max-time", toString timeoutSec,
      "-X", "POST",
      "-H", "content-type: application/json",
      "-w", "\n___STATUS:%{http_code}",
      "-o", respFile,
      "--data-binary", s!"@{bodyFile}",
      url
    ]
  }
  try IO.FS.removeFile bodyFile catch _ => pure ()
  let body ← (try IO.FS.readFile respFile catch _ => pure "")
  try IO.FS.removeFile respFile catch _ => pure ()
  let parts := out.stdout.splitOn "\n___STATUS:"
  let status := match parts with
    | [_, codeS] => codeS.trimAscii.toString.toNat?.getD 0
    | _ => 0
  return { status, body, err := out.stderr }

/-! ## Request body builder -/

private def jObj (xs : List (String × Json)) : Json := Json.mkObj xs
private def jArr (xs : List Json) : Json := Json.arr xs.toArray

/-- `Json.num` wants a `JsonNumber`; we encode floats by
    `toString`+reparse. Good enough for temperature / topP. Same
    trick as `LeanTea.Llm.Openai`. -/
private def floatJson (f : Float) : Json :=
  match Json.parse (toString f) with
  | .ok j    => j
  | .error _ => Json.num 0

private def buildRequest (messages : List Message) (opts : CallOpts) : Json := Id.run do
  let contents := messages.map fun m =>
    jObj [("role", Json.str m.role), ("parts", jArr [jObj [("text", Json.str m.text)]])]
  let mut fields : List (String × Json) := [
    ("contents", jArr contents)
  ]
  if let some sys := opts.system then
    fields := fields ++ [("systemInstruction",
      jObj [("parts", jArr [jObj [("text", Json.str sys)]])])]
  let mut gc : List (String × Json) := []
  if let some n := opts.maxOutputTokens then
    gc := gc ++ [("maxOutputTokens", Json.num (Int.ofNat n))]
  if let some t := opts.temperature then
    gc := gc ++ [("temperature", floatJson t)]
  if let some sch := opts.jsonSchema then
    gc := gc ++ [
      ("responseMimeType", Json.str "application/json"),
      ("responseSchema", sch)]
  if !gc.isEmpty then fields := fields ++ [("generationConfig", jObj gc)]
  return jObj fields

/-! ## Response parser -/

private def parseResponse (model rawBody : String) : IO Response := do
  match Json.parse rawBody with
  | .error e => throw <| IO.userError s!"Gemini: bad JSON ({e})\n{rawBody.take 500}"
  | .ok j =>
    -- API error envelope
    match (j.getObjVal? "error").toOption with
    | some err =>
      let msg := err.getStrD "message"
      let code := err.getNatD "code"
      throw <| IO.userError s!"Gemini API error {code}: {msg}"
    | none => pure ()
    -- Concatenate candidates[0].content.parts[*].text
    let candidates := (j.getObjVal? "candidates").toOption.getD Json.null
    let arr := match candidates.getArr? with | .ok a => a.toList | .error _ => []
    let cand := arr.head?.getD Json.null
    let content := (cand.getObjVal? "content").toOption.getD Json.null
    let parts := (content.getObjVal? "parts").toOption.getD Json.null
    let partArr := match parts.getArr? with | .ok a => a.toList | .error _ => []
    let text := String.intercalate "" (partArr.map (·.getStrD "text"))
    let finishReason := cand.getStrD "finishReason"
    let usageM := (j.getObjVal? "usageMetadata").toOption.getD Json.null
    let usage : Usage := {
      input  := usageM.getNatD "promptTokenCount",
      output := usageM.getNatD "candidatesTokenCount",
      total  := usageM.getNatD "totalTokenCount"
    }
    return { text, usage, finishReason, model, raw := rawBody }

/-! ## Core: generate -/

/-- Send a message list (chat history) and return one turn's response. -/
def generate (cfg : Config) (messages : List Message) (opts : CallOpts := {})
    : IO Response := do
  let model := opts.model.getD cfg.model
  let url := s!"{cfg.endpoint}/models/{model}:generateContent?key={cfg.apiKey}"
  let body := (buildRequest messages opts).compress
  let resp ← curlPost url body cfg.timeoutSec
  if resp.status < 200 || resp.status >= 300 then
    throw <| IO.userError s!"Gemini HTTP {resp.status}: {resp.body.take 500}\n{resp.err}"
  parseResponse model resp.body

/-! ## Convenience APIs -/

/-- One-shot prompt. -/
def ask (cfg : Config) (prompt : String) (opts : CallOpts := {})
    : IO Response :=
  generate cfg [{ role := "user", text := prompt }] opts

/-- Multi-turn chat. `history` must be alternating user/model turns
    (Gemini's expected shape). -/
def chat (cfg : Config) (history : List Message) (message : String)
    (opts : CallOpts := {}) : IO Response :=
  generate cfg (history ++ [{ role := "user", text := message }]) opts

/-! ## Holistic codebase review

The headline API — bundles multiple files into one Markdown prompt
to exploit Pro's 2M-token context. `SafePath` guarantees the
workspace boundary is honoured even when the caller passes
attacker-controlled paths. -/

/-- Map a file extension to a fenced-code-block language hint. -/
private def guessLang (path : String) : String :=
  let lc := path.toLower
  if      lc.endsWith ".lean" then "lean"
  else if lc.endsWith ".go"   then "go"
  else if lc.endsWith ".rs"   then "rust"
  else if lc.endsWith ".ts"   then "ts"
  else if lc.endsWith ".tsx"  then "tsx"
  else if lc.endsWith ".js"   then "js"
  else if lc.endsWith ".jsx"  then "jsx"
  else if lc.endsWith ".py"   then "python"
  else if lc.endsWith ".java" then "java"
  else if lc.endsWith ".c" || lc.endsWith ".h" then "c"
  else if lc.endsWith ".cpp" || lc.endsWith ".cc" || lc.endsWith ".hpp" then "cpp"
  else if lc.endsWith ".cs"   then "csharp"
  else if lc.endsWith ".kt"   then "kotlin"
  else if lc.endsWith ".swift" then "swift"
  else if lc.endsWith ".rb"   then "ruby"
  else if lc.endsWith ".php"  then "php"
  else if lc.endsWith ".sh"   then "bash"
  else if lc.endsWith ".sql"  then "sql"
  else if lc.endsWith ".yaml" || lc.endsWith ".yml" then "yaml"
  else if lc.endsWith ".toml" then "toml"
  else if lc.endsWith ".json" then "json"
  else if lc.endsWith ".html" then "html"
  else if lc.endsWith ".css"  then "css"
  else if lc.endsWith ".md"   then "markdown"
  else if lc.endsWith ".proto" then "protobuf"
  else if lc.endsWith ".tf"   then "hcl"
  else if lc.endsWith ".dockerfile" || lc.endsWith "Dockerfile" then "dockerfile"
  else ""

/-- Render one file as a `## File: <path>\n\`\`\`lang\n<body>\n\`\`\`` block. -/
private def renderFile (relPath : String) (body : String) : String :=
  let lang := guessLang relPath
  s!"\n\n## File: {relPath}\n\n```{lang}\n{body}\n```\n"

/-- Default system prompt — asks the model to look across all files
    first, then drill into specifics. Override via `CallOpts.system`
    if you want a different style or language. -/
def defaultReviewSystem : String :=
"You are a senior software engineer. You will be given multiple
source files as a sequence of Markdown `## File: <path>` blocks.
Read every file before answering, then review in the following order:

1. **Codebase overview**: overall structure, separation of concerns,
   key dependencies.
2. **Consistency check**: conventions or patterns that contradict
   each other across files.
3. **Design weaknesses**: architecturally fragile spots (god classes,
   circular dependencies, over-abstraction, leaky abstractions, …).
4. **Concrete bug risks**: per-file defect candidates, cited as
   file:line.
5. **Improvement proposals**: 3–10 items, prioritised.

Emphasise **cross-file connections and design overview** over
single-file style nitpicks."

structure ReviewStats where
  fileCount : Nat
  totalBytes : Nat
  /-- Files we skipped while bundling, with reasons. -/
  skipped : List (String × String) := []
  deriving Inhabited, Repr

/-- Read multiple files through `SafePath`, bundle them into one
    Markdown prompt, and send to Gemini.

    - `workspace` is the read root (`..` can't escape it).
    - `paths` are workspace-relative.
    - `prompt` is the user instruction; empty falls back to the
      default review prompt.
    - When `opts.system` is unset, `defaultReviewSystem` is used.

    Returns the `Response` plus stats (how many files / bytes
    actually made it into the bundle, plus any skipped ones). -/
def reviewMany (cfg : Config)
    (workspace : String)
    (paths : Array String)
    (prompt : String := "")
    (opts : CallOpts := {})
    : IO (Response × ReviewStats) := do
  let mut bundle := ""
  let mut count := 0
  let mut bytes := 0
  let mut skipped : List (String × String) := []
  for raw in paths do
    match SafePath.under workspace raw with
    | .error msg =>
      skipped := skipped ++ [(raw, s!"SafePath: {msg}")]
    | .ok p =>
      try
        let body ← IO.FS.readFile p.value
        bundle := bundle ++ renderFile raw body
        count := count + 1
        bytes := bytes + body.utf8ByteSize
      catch e =>
        skipped := skipped ++ [(raw, s!"read failed: {e}")]
  let userText :=
    (if prompt.isEmpty then defaultReviewSystem else prompt) ++ bundle
  let optsWithSys :=
    if opts.system.isSome then opts
    else { opts with system := some defaultReviewSystem }
  let resp ← ask cfg userText optsWithSys
  return (resp, { fileCount := count, totalBytes := bytes, skipped })

/-! ## Diff review — lightweight git-diff focused review -/

/-- Run `git diff <base> <head>` inside `workspace` and pipe the
    output to Gemini for a focused review. Override the prompt /
    system message via `prompt` and `opts.system`. -/
def reviewDiff (cfg : Config) (workspace base head : String)
    (prompt : String := "") (opts : CallOpts := {})
    : IO Response := do
  let out ← IO.Process.output {
    cmd := "git",
    args := #["-C", workspace, "diff", base, head]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"git diff failed: {out.stderr}"
  let diff := out.stdout
  let sysDefault :=
"You are a senior software engineer. You will be given a unified
diff. Review it in the following order:

1. **Intent**: what is this change trying to achieve?
2. **Defect candidates**: bugs, regression risks, missing cases.
3. **Improvement proposals**: style / performance / maintainability
   nits worth flagging.
4. **Verdict**: approve / request changes / nit, with a reason."
  let userText :=
    (if prompt.isEmpty then "Please review per the criteria above." else prompt)
    ++ "\n\n```diff\n" ++ diff ++ "\n```"
  let optsWithSys :=
    if opts.system.isSome then opts
    else { opts with system := some sysDefault }
  ask cfg userText optsWithSys

end LeanTea.Cloud.Gemini
