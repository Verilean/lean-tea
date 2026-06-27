import LeanTea
import Lean.Data.Json

/-! # gemini_mcp_serve — MCP server fronting the Google Gemini API

Same shape as the other LeanTEA MCP servers (`tmux_mcp_serve`,
`chrome_cdp_mcp_serve`, …): one binary, stdio + HTTP transports,
tools named with a common `gemini_` prefix.

The point of going through MCP rather than calling Gemini directly
from each agent is twofold:

1. **One key, one boundary.** The `GEMINI_API_KEY` lives in this
   process only. Clients (Claude / Cursor / any MCP runner) see
   only a stable tool catalogue.
2. **Workspace-bound file access.** `gemini_review_files` and
   `gemini_review_diff` always go through `LeanTea.Net.SafePath`
   against a `--workspace DIR` root, so a buggy or adversarial
   LLM client can't make the MCP read `/etc/shadow` and ship it
   to Google.

### Why these five tools

`gemini_ask` is the one-shot prompt. `gemini_chat` is the
multi-turn variant; the history is provided as a JSON array each
call (MCP servers are stateless by spec — keeping turn history on
the server would break that). `gemini_review_files` is the
flagship: it bundles many files into one prompt to exploit Pro's
2M-token context for cross-file architectural review.
`gemini_review_diff` is the small-bites variant — quick PR review
without bundling the whole codebase. `gemini_list_models` returns
the supported model catalogue so clients can advertise the choices
without hard-coding.

### Model selection

Default model is `gemini-2.5-pro` (per `LeanTea.Cloud.Gemini`).
Every tool accepts an optional `model` argument so callers can
pick `gemini-2.5-flash` for speed or `gemini-2.5-flash-lite` for
cost. Override at server level via `GEMINI_MODEL` env or
`--model NAME` flag.

```
gemini_mcp_serve --port 8020 --workspace /path/to/repo
gemini_mcp_serve --stdio --workspace /path/to/repo --model gemini-2.5-flash
```

stdio mode is what MCP-Lite / Claude Desktop / Cursor speak;
`--port` is for plain `curl` testing. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace GeminiMcp

open LeanTea.Mcp (jsonOk jsonErr textContent errContent
                  argSchema toolDef defaultInitializeResult)

/-! ## Argument extraction helpers (mirrors TmuxMcp) -/

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

private def getNatOpt (args : Json) (k : String) (default : Nat := 0) : Nat :=
  match args.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

private def getFloatOpt (args : Json) (k : String) : Option Float :=
  match args.getObjVal? k with
  | .ok (.num n) => some n.toFloat
  | _            => none

/-- Pull a JSON array of strings; non-string entries are skipped. -/
private def getStrArr (args : Json) (k : String) : Array String :=
  match args.getObjVal? k with
  | .ok (.arr a) =>
    a.filterMap fun j => match j.getStr? with
      | .ok s    => some s
      | .error _ => none
  | _ => #[]

/-! ## Server-side state — the Gemini Config + workspace root.

The Config carries `apiKey` (which we never want to log), so it
goes through a top-level `IO.Ref` rather than being threaded
through `callTool`. -/

private initialize cfgRef : IO.Ref (Option LeanTea.Cloud.Gemini.Config) ←
  IO.mkRef none

private initialize workspaceRef : IO.Ref String ← IO.mkRef ""

private def getCfg : IO LeanTea.Cloud.Gemini.Config := do
  match ← cfgRef.get with
  | some c => return c
  | none   =>
    throw <| IO.userError
      "gemini_mcp: Config not initialised (set GEMINI_API_KEY)"

/-- Pull a `CallOpts` from the JSON args: `model` / `system` /
    `temperature` / `maxTokens`. All optional. -/
private def parseOpts (args : Json) : LeanTea.Cloud.Gemini.CallOpts :=
  let model       := getStrOpt args "model"
  let system      := getStrOpt args "system"
  let temperature := getFloatOpt args "temperature"
  let maxTokens   := getNatOpt args "maxTokens"
  {
    model         := if model.isEmpty  then none else some model,
    system        := if system.isEmpty then none else some system,
    temperature   := temperature,
    maxOutputTokens := if maxTokens == 0 then none else some maxTokens
  }

/-- Common reply formatter: surface text + usage + finish reason
    + resolved model name, separated by markers so callers can
    parse them out if they want. -/
private def renderResponse (r : LeanTea.Cloud.Gemini.Response)
    (extra : String := "") : Json :=
  let header :=
    s!"[model={r.model} tokens in={r.usage.input} out={r.usage.output} \
finish={r.finishReason}]"
  let extraLine := if extra.isEmpty then "" else extra ++ "\n"
  textContent (header ++ "\n" ++ extraLine ++ r.text)

/-! ## Tool catalogue -/

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "gemini_ask"
        ("Single-turn Gemini prompt. Returns the model's text reply "
         ++ "prefixed with `[model=… tokens in=… out=… finish=…]` so the "
         ++ "caller can track cost. Default model `gemini-2.5-pro`; "
         ++ "override with `model` (try `gemini-2.5-flash` for speed).")
        #[ argSchema "prompt"      "string" "the user prompt",
           argSchema "model"       "string" "(optional) override default model",
           argSchema "system"      "string" "(optional) system prompt",
           argSchema "temperature" "number" "(optional) 0.0=deterministic",
           argSchema "maxTokens"   "number" "(optional) maxOutputTokens cap" ]
        #["prompt"],
      toolDef "gemini_review_files"
        ("Bundle multiple workspace-relative files into a Markdown "
         ++ "prompt and ask Gemini for a holistic review. Exploits "
         ++ "Pro's 2M-token context; pair with `model=gemini-2.5-flash` "
         ++ "for cheaper passes. Paths are validated via `SafePath` "
         ++ "against the server's `--workspace` — `..` / NUL escape "
         ++ "attempts are rejected. The response is prefixed with the "
         ++ "usual `[model=… tokens…]` header followed by `Files: N "
         ++ "(M bytes)` then the review prose.")
        #[ argSchema "paths"       "array"  "workspace-relative paths to bundle",
           argSchema "prompt"      "string" "(optional) custom review prompt; default = holistic review",
           argSchema "model"       "string" "(optional) override default model",
           argSchema "system"      "string" "(optional) custom system prompt",
           argSchema "temperature" "number" "(optional)",
           argSchema "maxTokens"   "number" "(optional)" ]
        #["paths"],
      toolDef "gemini_review_diff"
        ("Run `git diff BASE HEAD` inside the workspace, pipe the diff "
         ++ "to Gemini, return a focused review (intent + defect "
         ++ "candidates + suggestions + approve/changes/nit verdict).")
        #[ argSchema "base"        "string" "git base ref",
           argSchema "head"        "string" "git head ref",
           argSchema "prompt"      "string" "(optional) custom review prompt",
           argSchema "model"       "string" "(optional) override default model",
           argSchema "system"      "string" "(optional) custom system prompt",
           argSchema "temperature" "number" "(optional)",
           argSchema "maxTokens"   "number" "(optional)" ]
        #["base", "head"],
      toolDef "gemini_chat"
        ("Multi-turn chat. `history` is a JSON array of "
         ++ "`{role: \"user\"|\"model\", text: \"...\"}` objects "
         ++ "alternating user/model turns; `message` is the new user "
         ++ "input. MCP servers are stateless so callers must keep "
         ++ "history themselves between calls.")
        #[ argSchema "message"     "string" "new user message",
           argSchema "history"     "array"  "prior turns (alternating user/model)",
           argSchema "model"       "string" "(optional) override default model",
           argSchema "system"      "string" "(optional) system prompt",
           argSchema "temperature" "number" "(optional)",
           argSchema "maxTokens"   "number" "(optional)" ]
        #["message"],
      toolDef "gemini_list_models"
        ("Return the catalogue of supported models with rough use-case "
         ++ "hints and context-window sizes. Pricing is informational "
         ++ "only — check ai.google.dev for current rates.")
        #[] #[]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-gemini-mcp"

/-! ## Static model catalogue -/

private def modelsCatalogue : Json :=
  let mkRow (id desc : String) (ctxK : Nat) : Json :=
    Json.mkObj [
      ("id",       Json.str id),
      ("desc",     Json.str desc),
      ("contextK", Json.num (Int.ofNat ctxK))
    ]
  Json.mkObj [
    ("default", Json.str "gemini-2.5-pro"),
    ("models", Json.arr #[
      mkRow "gemini-2.5-pro"
        "heavy code / architecture review; 2M context"
        2097,
      mkRow "gemini-2.5-flash"
        "fast + cheap general-purpose; 1M context"
        1048,
      mkRow "gemini-2.5-flash-lite"
        "smallest + cheapest, summarisation / lint; 1M context"
        1048
    ])
  ]

/-! ## Tool dispatch -/

def callTool (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "gemini_list_models" =>
      return textContent modelsCatalogue.pretty
    | "gemini_ask" =>
      match getStr args "prompt" with
      | .error e => return errContent s!"prompt: {e}"
      | .ok prompt =>
        let cfg ← getCfg
        let opts := parseOpts args
        let r ← LeanTea.Cloud.Gemini.ask cfg prompt opts
        return renderResponse r
    | "gemini_chat" =>
      match getStr args "message" with
      | .error e => return errContent s!"message: {e}"
      | .ok message =>
        let cfg ← getCfg
        let opts := parseOpts args
        let hist : List LeanTea.Cloud.Gemini.Message :=
          match args.getObjVal? "history" with
          | .ok (.arr a) =>
            (a.toList.filterMap fun j =>
              match j.getObjVal? "role", j.getObjVal? "text" with
              | .ok jr, .ok jt =>
                match jr.getStr?, jt.getStr? with
                | .ok role, .ok text => some ({ role, text } : LeanTea.Cloud.Gemini.Message)
                | _, _ => none
              | _, _ => none)
          | _ => []
        let r ← LeanTea.Cloud.Gemini.chat cfg hist message opts
        return renderResponse r
    | "gemini_review_files" =>
      let paths := getStrArr args "paths"
      if paths.isEmpty then
        return errContent "paths: empty (need at least one workspace-relative path)"
      else
        let cfg ← getCfg
        let ws  ← workspaceRef.get
        if ws.isEmpty then
          return errContent "gemini_mcp: --workspace not set; can't safely read files"
        else
          let prompt := getStrOpt args "prompt"
          let opts := parseOpts args
          /- Pre-check SafePath: if *every* path is rejected we'd
             otherwise spend a Gemini call on an empty bundle. -/
          let firstOk := paths.findSome? fun p =>
            match LeanTea.Net.SafePath.under ws p with
            | .ok _    => some p
            | .error _ => none
          if firstOk.isNone then
            let reasons := String.intercalate "\n  " <| paths.toList.map fun p =>
              match LeanTea.Net.SafePath.under ws p with
              | .error msg => s!"{p}: {msg}"
              | .ok _      => s!"{p}: (ok but somehow filtered)"
            return errContent s!"gemini_review_files: all paths rejected\n  {reasons}"
          let (r, stats) ← LeanTea.Cloud.Gemini.reviewMany cfg ws paths prompt opts
          let skipNote :=
            if stats.skipped.isEmpty then ""
            else
              "Skipped:\n" ++ String.intercalate "\n"
                (stats.skipped.map fun (p, reason) => s!"  - {p}: {reason}")
          let extra := s!"Files: {stats.fileCount} ({stats.totalBytes} bytes)"
                       ++ (if skipNote.isEmpty then "" else "\n" ++ skipNote)
          return renderResponse r extra
    | "gemini_review_diff" =>
      match getStr args "base", getStr args "head" with
      | .error e, _ => return errContent s!"base: {e}"
      | _, .error e => return errContent s!"head: {e}"
      | .ok base, .ok head =>
        let cfg ← getCfg
        let ws  ← workspaceRef.get
        if ws.isEmpty then
          return errContent "gemini_mcp: --workspace not set; can't run git diff"
        else
          let prompt := getStrOpt args "prompt"
          let opts := parseOpts args
          let r ← LeanTea.Cloud.Gemini.reviewDiff cfg ws base head prompt opts
          return renderResponse r
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## CLI argument parsing -/

private structure Args where
  mode      : String := "stdio"
  port      : UInt16 := 8020
  host      : String := "0.0.0.0"
  workspace : String := ""
  model     : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port"  :: v :: rest =>
    parseArgs rest { a with mode := "http", port := (v.toNat?.getD 8020).toUInt16 }
  | "--host"  :: v :: rest      => parseArgs rest { a with host := v }
  | "--workspace" :: v :: rest  => parseArgs rest { a with workspace := v }
  | "--model" :: v :: rest      => parseArgs rest { a with model := v }
  | _ :: rest                   => parseArgs rest a
  | []                          => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  if let some w ← IO.getEnv "GEMINI_MCP_WORKSPACE" then
    if a.workspace.isEmpty then a := { a with workspace := w }
  if let some m ← IO.getEnv "GEMINI_MODEL" then
    if a.model.isEmpty then a := { a with model := m }
  /- Initialise the global config from env + CLI overrides. We still
     return successfully without an API key — the tools just fail
     with a clear "Config not initialised" message when called.
     This keeps the catalogue (`tools/list`, `gemini_list_models`)
     reachable for clients that just want to discover capabilities. -/
  match ← LeanTea.Cloud.Gemini.Config.fromEnv? with
  | none =>
    IO.eprintln "gemini-mcp: GEMINI_API_KEY not set — \
tools/list works but gemini_ask / chat / review will error until you set it"
  | some baseCfg =>
    let cfg := if a.model.isEmpty then baseCfg else { baseCfg with model := a.model }
    cfgRef.set (some cfg)
    IO.eprintln s!"gemini-mcp: ready (default model = {cfg.model})"
  workspaceRef.set a.workspace
  if a.workspace.isEmpty then
    IO.eprintln "gemini-mcp: --workspace not set — \
gemini_review_files and gemini_review_diff will refuse to run"
  else
    IO.eprintln s!"gemini-mcp: workspace = {a.workspace}"
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool
  }
  match a.mode with
  | "http" =>
    IO.eprintln s!"gemini-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "gemini-mcp: stdio mode"
    mcpHandler.serveStdio

end GeminiMcp

def main (args : List String) : IO Unit := GeminiMcp.serveMain args
