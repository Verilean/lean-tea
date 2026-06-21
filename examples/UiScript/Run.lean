import LeanTea
import Lean.Data.Json

/-! # ui_script — execute a JSON UI script against `browser_mcp_serve`

```
./ui_script path/to/script.json [--mcp-url http://127.0.0.1:8009/mcp]
                                [--classify google/gemma-4-e4b]
                                [--mcp-bin /path/to/browser_mcp_serve]
                                [--evidence-dir /tmp/evidence/]
```

The runner:

1. Parses the script (see `LeanTea.Agent.Script` for the schema).
2. Connects to a `browser_mcp_serve` — over HTTP if `--mcp-url`, else
   spawns one as a child via stdio.
3. Executes each step deterministically: click → screenshot → if the
   step has `expect`, classifies the screenshot via the chosen LLM
   (when `--classify` is set) or just records the expected name.
4. On any mismatch / hard failure: writes one line to
   `~/.cache/leantea-agent/escalations.jsonl`, stops, exits 3.
5. On clean completion: prints a one-line summary, exits 0.

Most of an actual run is `screenshot` (~150ms) + classifier (~3–8 s/
call when on, ~0 when off) + waits — orders of magnitude faster than
"let an LLM drive each click". -/

open Lean (Json)
open LeanTea.Llm.Openai (Config)
open LeanTea.Agent.Script

namespace UiScript

/-! ## Mcp client — same dual-transport shape as BrowserAgent -/

inductive McpKind where
  | stdio (child : IO.Process.Child { stdin := .piped, stdout := .piped, stderr := .piped })
  | http  (url : String)

structure Mcp where
  kind   : McpKind
  nextId : IO.Ref Nat

private def findMcpBinary (override? : Option String) : IO String := do
  if let some p := override? then
    if ← System.FilePath.pathExists p then return p
  if let some p ← IO.getEnv "BROWSER_MCP_BIN" then
    if ← System.FilePath.pathExists p then return p
  let candidates := [
    "./.lake/build/bin/browser_mcp_serve",
    "../.lake/build/bin/browser_mcp_serve",
    "../../.lake/build/bin/browser_mcp_serve"
  ]
  for c in candidates do
    if ← System.FilePath.pathExists c then return c
  throw <| IO.userError
    "ui_script: couldn't find browser_mcp_serve. Build it with `lake build browser_mcp_serve` or pass --mcp-bin."

def Mcp.spawn (override? : Option String := none) : IO Mcp := do
  let bin ← findMcpBinary override?
  let child ← IO.Process.spawn {
    cmd := bin, args := #[],
    stdin := .piped, stdout := .piped, stderr := .piped
  }
  let nextId ← IO.mkRef 1
  return { kind := .stdio child, nextId }

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
      /- Pure-Lean HTTP — no curl process spawn, no tempfile-juggling. -/
      LeanTea.Net.HttpClient.postJsonText url req.compress
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"mcp: bad JSON: {e}\n{raw}"
  | .ok j    => return j

def Mcp.initialize (m : Mcp) : IO Unit := do
  let _ ← sendReq m "initialize" (Json.mkObj [
    ("protocolVersion", Json.str "2024-11-05"),
    ("capabilities", Json.mkObj []),
    ("clientInfo", Json.mkObj [
      ("name", Json.str "ui-script"),
      ("version", Json.str "0.1.0")
    ])
  ])
  return ()

def Mcp.callTool (m : Mcp) (name : String) (args : Json) : IO Json := do
  let resp ← sendReq m "tools/call" (Json.mkObj [
    ("name", Json.str name),
    ("arguments", args)
  ])
  match resp.getObjVal? "result" with
  | .ok r => return r
  | .error _ =>
    let err := (resp.getObjVal? "error").toOption.getD Json.null
    throw <| IO.userError s!"mcp: tool {name} failed: {err.compress}"

/-! ## Optional LLM-backed classifier

When `--classify <model>` is passed, each step with an `expect`
runs the screenshot through LM Studio + the chosen model, asking
"which of these names best describes the screen?" and matching the
response prefix. Fast enough to use as an assertion (a few seconds
per check) and orders of magnitude cheaper than letting the LLM
drive every click. -/

private def imageDataUrlOfFile (path : String) : IO String := do
  let bytes ← IO.FS.readBinFile path
  let mime := if path.endsWith ".jpg" || path.endsWith ".jpeg"
              then "image/jpeg" else "image/png"
  return s!"data:{mime};base64,{LeanTea.Llm.Openai.base64Encode bytes}"

/-- Single-turn classifier: returns the candidate the model picked,
    or "unknown" if it refused / produced something else. We do a
    case-insensitive prefix match because small models tend to add
    explanatory prose after the label. -/
def classifyWithOpenai (cfg : Config) (model : String)
    (screenshotPath : String) (candidates : List String) : IO String := do
  let url ← imageDataUrlOfFile screenshotPath
  let listed := String.intercalate ", " candidates
  let prompt := s!"You are a screen classifier. The screenshot shows ONE of these screens: {listed}. Reply with EXACTLY one of those names, nothing else. If the screen doesn't clearly match any candidate, reply exactly: unknown"
  let req : LeanTea.Llm.Openai.ChatRequest := {
    model,
    temperature := some 0.0,
    maxTokens   := some 100,
    messages := [
      LeanTea.Llm.Openai.system prompt,
      LeanTea.Llm.Openai.userTextAndImage "Which one?" url
    ]
  }
  let res ← LeanTea.Llm.Openai.chat cfg req
  let raw := res.content.trimAscii.toString.toLower
  for c in candidates do
    if raw.startsWith c.toLower then return c
  return "unknown"

/-! ## Step execution -/

structure RunCtx where
  mcp          : Mcp
  classifier?  : Option (String → List String → IO String)
  evidenceDir? : Option String
  /-- Path to the screenshot captured by the previous step. Used to
      decide "screen unchanged → don't bother the classifier". Reset
      each run. -/
  lastEvidence : IO.Ref (Option String)
  /-- Counter for classifier calls skipped due to byte-equal frames —
      surfaced in the final summary. -/
  classifierSkips : IO.Ref Nat

/-- Byte-equality of two on-disk PNGs. PNG encoding is deterministic
    so static UI between two captures hashes identical. For animated
    screens this returns false continuously, which is correct —
    those are exactly the cases we DO want to re-classify. -/
private def screenshotsEqual (a b : String) : IO Bool := do
  let ba ← IO.FS.readBinFile a
  let bb ← IO.FS.readBinFile b
  if ba.size != bb.size then return false
  let n := ba.size
  let mut i := 0
  while i < n do
    if ba[i]! != bb[i]! then return false
    i := i + 1
  return true

private def now : IO Nat := IO.monoMsNow

/-- Take a screenshot to disk for the step's audit trail. Returns the
    absolute path actually written (so the result struct can carry
    it). When `--evidence-dir` isn't set, the screenshot still
    happens but goes to a per-process temp slot. -/
private def captureEvidence (ctx : RunCtx) (script : Script) (idx : Nat)
    (overridePath? : Option String := none) : IO String := do
  let out := match overridePath? with
    | some p => p
    | none =>
      match ctx.evidenceDir? with
      | some dir => s!"{dir}/{script.name}.step{idx + 1}.png"
      | none     => s!"/tmp/ui-script-{script.name}.step{idx + 1}.png"
  let _ ← ctx.mcp.callTool "browser_screenshot" (Json.mkObj [
    ("outputPath", Json.str out)
  ])
  return out

private def runAction (ctx : RunCtx) (script : Script) (idx : Nat) (act : Action)
    : IO (Option String) := do
  /- Each action returns the optional evidence path (the screenshot
     used to verify the resulting state). -/
  match act with
  | .clickXy x y =>
    let _ ← ctx.mcp.callTool "browser_click_xy" (Json.mkObj [
      ("x", Json.num (Int.ofNat x)),
      ("y", Json.num (Int.ofNat y))
    ])
    return none
  | .clickKnown key =>
    let v ← LeanTea.Agent.Memory.recall key
    match v with
    | .null => throw <| IO.userError s!"click_known: key not in ui-map.json: {key}"
    | _ =>
      let x := (v.getObjVal? "x").toOption.bind (fun j =>
        match j with | .num n => some n.mantissa.toNat | _ => none) |>.getD 0
      let y := (v.getObjVal? "y").toOption.bind (fun j =>
        match j with | .num n => some n.mantissa.toNat | _ => none) |>.getD 0
      if x = 0 && y = 0 then
        throw <| IO.userError s!"click_known: stored value has no usable x/y for {key}"
      let _ ← ctx.mcp.callTool "browser_click_xy" (Json.mkObj [
        ("x", Json.num (Int.ofNat x)),
        ("y", Json.num (Int.ofNat y))
      ])
      return none
  | .wait ms =>
    /- Use the bridge's JS context for the wait so the runner stays
       responsive (avoids parking the Lean thread on the libuv loop). -/
    let _ ← ctx.mcp.callTool "browser_evaluate" (Json.mkObj [
      ("expression", Json.str s!"new Promise(r=>setTimeout(r,{ms}))")
    ])
    return none
  | .screenshot saveAs =>
    let path ← captureEvidence ctx script idx saveAs
    return some path
  | .toolCall tool args _ _ =>
    /- Generic MCP tool dispatch. The tool itself owns file writes —
       e.g. comfyui_txt2img accepts an `outputPath` arg and writes
       the PNG to that path. The runner just forwards args verbatim
       and surfaces the response in the audit log. -/
    let _ ← ctx.mcp.callTool tool args
    return none
  | .waitForScreen screen timeoutMs =>
    /- Polling loop. Without a classifier, this degrades to a single
       wait + screenshot. With one, we re-classify every 1s until
       success or timeout. -/
    let start ← now
    let mut last : String := "unknown"
    let mut path : String := ""
    let mut prevPath : Option String ← ctx.lastEvidence.get
    let mut done := false
    while !done do
      path ← captureEvidence ctx script idx none
      match ctx.classifier? with
      | none =>
        let _ ← ctx.mcp.callTool "browser_evaluate" (Json.mkObj [
          ("expression", Json.str s!"new Promise(r=>setTimeout(r,{timeoutMs}))")
        ])
        done := true
      | some cls =>
        /- Skip classifier when bytes are identical to the previous
           capture — the screen hasn't moved, so the answer can only
           be the same (still "unknown" / still "not target"). -/
        let unchanged ← match prevPath with
          | some p => screenshotsEqual p path
          | none   => pure false
        if unchanged then
          ctx.classifierSkips.modify (· + 1)
          let elapsed := (← now) - start
          if elapsed > timeoutMs then
            throw <| IO.userError
              s!"wait_for_screen: timed out after {elapsed}ms waiting for {screen} (last seen: {last})"
          let _ ← ctx.mcp.callTool "browser_evaluate" (Json.mkObj [
            ("expression", Json.str "new Promise(r=>setTimeout(r,1000))")
          ])
        else
          last ← cls path [screen]
          if last == screen then
            done := true
          else
            let elapsed := (← now) - start
            if elapsed > timeoutMs then
              throw <| IO.userError
                s!"wait_for_screen: timed out after {elapsed}ms waiting for {screen} (last seen: {last})"
            let _ ← ctx.mcp.callTool "browser_evaluate" (Json.mkObj [
              ("expression", Json.str "new Promise(r=>setTimeout(r,1000))")
            ])
          prevPath := some path
    return some path

private def runStep (ctx : RunCtx) (script : Script) (idx : Nat) (step : Step)
    : IO StepResult := do
  let t0 ← now
  try
    let evidence ← runAction ctx script idx step.act
    /- After actions other than screenshot/wait_for_screen, take a
       fresh screenshot for the expect check. -/
    let evidence ← match evidence with
      | some p => pure p
      | none   => captureEvidence ctx script idx none
    let observed ← match step.expect, ctx.classifier? with
      | some expected, some cls =>
        /- Skip the classifier when the post-action capture is byte-
           equal to the previous step's — same pixels mean the same
           answer, and if `expected` is something new it can't have
           been reached. Failing fast here is a 5-7s win per false
           hit and keeps an inert click from quietly "passing". -/
        let prev ← ctx.lastEvidence.get
        let unchanged ← match prev with
          | some p => screenshotsEqual p evidence
          | none   => pure false
        if unchanged then
          ctx.classifierSkips.modify (· + 1)
          throw <| IO.userError
            s!"expect mismatch: wanted {expected}, screen unchanged from previous step (classifier skipped, would have wasted a call)"
        let got ← cls evidence [expected]
        if got != expected then
          throw <| IO.userError
            s!"expect mismatch: wanted {expected}, classifier said {got}"
        pure (some got)
      | some expected, none =>
        /- Without a classifier we can only record the expectation;
           a downstream tool / human reviews the evidence dir. -/
        pure (some s!"(unverified) expected={expected}")
      | none, _ => pure none
    ctx.lastEvidence.set (some evidence)
    let t1 ← now
    return { step, ok := true, observed,
             evidencePath := some evidence,
             durationMs := t1 - t0 }
  catch e =>
    let t1 ← now
    return { step, ok := false,
             error := some (toString e),
             durationMs := t1 - t0 }

/-! ## Script execution -/

/-- Save the run manifest under `~/.cache/leantea-agent/runs/`.
    Returns the absolute path so the runner can echo it / drop it
    into the result struct. -/
private def saveManifest (r : ScriptResult) : IO String := do
  let dir ← LeanTea.Agent.Memory.agentDir
  let runsDir := dir / "runs"
  IO.FS.createDirAll runsDir
  let ts ← IO.monoMsNow
  let path := runsDir / s!"{r.script.name}.{ts}.json"
  IO.FS.writeFile path (r.toJson.pretty 2)
  return path.toString

def runScript (ctx : RunCtx) (script : Script) : IO ScriptResult := do
  let t0 ← now
  let mut results : Array StepResult := #[]
  let mut passed := true
  let mut idx : Nat := 0
  let mut stop := false
  for step in script.steps do
    if stop then continue
    let res ← runStep ctx script idx step
    if res.ok then
      results := results.push res
      idx := idx + 1
    else
      passed := false
      let errMsg := res.error.getD "(no error)"
      /- Escalate on first failure and skip remaining steps. The
         tree printout below shows the full picture; this jsonl
         entry is for downstream automation. -/
      LeanTea.Agent.Memory.escalate
        s!"ui_script: {script.name} step {idx + 1} failed: {errMsg}"
        (Json.mkObj [
          ("script", Json.str script.name),
          ("step",   Json.num (Int.ofNat (idx + 1))),
          ("evidence", Json.str (res.evidencePath.getD ""))
        ])
      results := results.push res
      stop := true
  let t1 ← now
  let skipped := script.steps.length - results.size
  let interim : ScriptResult := {
    script, steps := results, passed, totalMs := t1 - t0, skipped
  }
  /- Persist the manifest BEFORE rendering the tree so the tree can
     point at the saved file's path. -/
  let manifestPath ← saveManifest interim
  return { interim with reportPath := some manifestPath }

/-! ## CLI -/

private structure Args where
  scriptPath  : Option String := none
  mcpUrl?     : Option String := none
  classifier? : Option String := none
  mcpBin?     : Option String := none
  evidenceDir? : Option String := none

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--mcp-url" :: v :: rest    => parseArgs rest { a with mcpUrl? := some v }
  | "--classify" :: v :: rest   => parseArgs rest { a with classifier? := some v }
  | "--mcp-bin" :: v :: rest    => parseArgs rest { a with mcpBin? := some v }
  | "--evidence-dir" :: v :: rest =>
    parseArgs rest { a with evidenceDir? := some v }
  | p :: rest                   =>
    if a.scriptPath.isNone then parseArgs rest { a with scriptPath := some p }
    else parseArgs rest a
  | []                          => a

def main (rawArgs : List String) : IO Unit := do
  let a := parseArgs rawArgs {}
  let path ← match a.scriptPath with
    | some p => pure p
    | none   =>
      IO.eprintln "usage: ui_script <script.json> [--mcp-url URL] [--classify MODEL] [--evidence-dir DIR]"
      IO.Process.exit 2
  let raw ← IO.FS.readFile path
  let j ← match Json.parse raw with
    | .ok j => pure j
    | .error e => throw <| IO.userError s!"bad JSON in {path}: {e}"
  let script ← match Script.fromJson j with
    | .ok s => pure s
    | .error e => throw <| IO.userError s!"bad script schema: {e}"

  let mcp ← match a.mcpUrl? with
    | some url => Mcp.connect url
    | none     => Mcp.spawn a.mcpBin?
  let classifier? : Option (String → List String → IO String) ←
    match a.classifier? with
    | none       => pure none
    | some model =>
      let baseUrl := (← IO.getEnv "LMSTUDIO_BASE_URL").getD "http://127.0.0.1:11211/v1"
      let cfg : Config := { baseUrl, timeoutSec := some 120 }
      pure (some (classifyWithOpenai cfg model))
  if let some d := a.evidenceDir? then
    IO.FS.createDirAll d

  IO.eprintln s!"ui_script: {script.name} ({script.steps.length} steps)"
  if let some _ := a.classifier? then
    IO.eprintln s!"ui_script: classifier={a.classifier?.get!}"
  else
    IO.eprintln "ui_script: classifier=none (expects logged only, not verified)"

  try
    mcp.initialize
    let lastEvidence ← IO.mkRef (none : Option String)
    let classifierSkips ← IO.mkRef 0
    let ctx : RunCtx := {
      mcp, classifier?,
      evidenceDir? := a.evidenceDir?,
      lastEvidence, classifierSkips
    }
    let res ← runScript ctx script
    IO.eprintln ""
    IO.eprintln res.renderTree
    let skips ← classifierSkips.get
    if skips > 0 then
      IO.eprintln s!"\n(classifier skipped {skips}× on byte-equal frames)"
    if !res.passed then
      IO.eprintln "→ ~/.cache/leantea-agent/escalations.jsonl"
      IO.Process.exit 3
  finally
    mcp.close

end UiScript

def main (args : List String) : IO Unit := UiScript.main args
