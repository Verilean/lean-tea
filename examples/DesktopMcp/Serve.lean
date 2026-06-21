import LeanTea
import Lean.Data.Json

/-! # desktop_mcp_serve — MCP server backed by OS-level automation

Same JSON-RPC / stdio + HTTP shape as `browser_mcp_serve`, but
tools call `LeanTea.Net.Desktop` directly (Quartz on macOS today)
instead of going through the Node + Playwright bridge.

The tool catalogue is intentionally smaller — only the actions that
make sense at the OS layer (clicks, screenshots, key presses); no
`get_text` / `evaluate` because there's no DOM to query. A
`ui_script` written against this catalogue runs against any app —
browser, Steam game, IDE, anything visible on the desktop.

```
desktop_mcp_serve --port 8010      # HTTP for curl smoke
desktop_mcp_serve                  # stdio for MCP clients
```

Build needs the `LEANTEA_DESKTOP=1` env var. Without it the exe still
builds and starts, but every tool call fails with "desktop support
not compiled in" — useful for verifying the catalogue without the
linker permission dance. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace DesktopMcp

open LeanTea.Mcp (jsonOk jsonErr textContent errContent imageContent
                  argSchema toolDef defaultInitializeResult)

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "desktop_click_xy"
        ("Synthesize a real OS left-click at desktop pixel (x, y). "
         ++ "Works on whatever app currently owns those pixels — "
         ++ "browser tab, Steam window, IDE, anything.")
        #[ argSchema "x" "number" "x in absolute screen pixels",
           argSchema "y" "number" "y in absolute screen pixels" ]
        #["x", "y"],
      toolDef "desktop_screenshot"
        ("Capture the entire main display as PNG. If `outputPath` is "
         ++ "given the bytes are written to that absolute path as well "
         ++ "(handy for piping to a vision LLM). Always returns the "
         ++ "base64 PNG as an MCP `image` content block.")
        #[ argSchema "outputPath" "string"
             "(optional) absolute path to save the raw PNG bytes" ]
        #[],
      toolDef "desktop_key_press"
        ("Press and release a single key by its platform-specific "
         ++ "virtual keycode (macOS: kVK_*, see HIToolbox/Events.h). "
         ++ "Examples: 36 = Return, 53 = Escape, 48 = Tab.")
        #[ argSchema "keycode" "number" "virtual keycode" ]
        #["keycode"],
      toolDef "desktop_screen_size"
        "Return the main display dimensions as {width, height}."
        #[] #[],
      toolDef "desktop_backend"
        ("Return the linked-in backend name (`macos-quartz`, "
         ++ "`stub`, …). Call this once at startup to confirm the "
         ++ "real implementation is built in.")
        #[] #[],
      toolDef "ui_recall"
        ("Same shared `ui-map.json` as the browser MCP — desktop "
         ++ "scripts use the same key space (screen names usually "
         ++ "prefixed `desktop.<app>.<element>`).")
        #[ argSchema "key" "string" "element key" ] #["key"],
      toolDef "ui_remember"
        "Save a verified UI element coordinate to the shared map."
        #[ argSchema "key" "string" "element key",
           ("value", Json.mkObj [
             ("type", Json.str "object"),
             ("description", Json.str "freeform {x, y, notes?, …}")
           ]) ]
        #["key", "value"],
      toolDef "ui_list"
        "List all keys currently in the UI map." #[] #[],
      toolDef "agent_escalate"
        ("Flag the current task as needing human / stronger-model "
         ++ "review. Writes to "
         ++ "~/.cache/leantea-agent/escalations.jsonl.")
        #[ argSchema "reason" "string" "one-line summary of why",
           ("context", Json.mkObj [
             ("type", Json.str "object"),
             ("description", Json.str "any extra JSON context")
           ]) ]
        #["reason"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-desktop-mcp"

/-! ## Argument extraction -/

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

private def getNatOpt (args : Json) (k : String) (default : Nat := 0) : Nat :=
  match args.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

/-! ## Tool dispatch -/

def callTool (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "desktop_click_xy" =>
      let x := getNatOpt args "x"
      let y := getNatOpt args "y"
      LeanTea.Net.Desktop.clickXy x.toUInt32 y.toUInt32
      return textContent s!"clicked ({x}, {y})"
    | "desktop_screenshot" =>
      /- We always need a file path for the FFI side; if the caller
         didn't provide one, use a per-process temp path so the
         tool still returns the base64 PNG to the MCP client. -/
      let path :=
        match getStrOpt args "outputPath" "" with
        | "" => "/tmp/leantea-desktop-shot.png"
        | p  => p
      LeanTea.Net.Desktop.screenshot path
      let bytes ← IO.FS.readBinFile path
      let b64 := LeanTea.Llm.Openai.base64Encode bytes
      let (w, h) ← LeanTea.Net.Desktop.screenSize
      return imageContent "image/png" b64
        s!"{w}×{h} screenshot ({bytes.size} bytes) → {path}"
    | "desktop_key_press" =>
      let kc := getNatOpt args "keycode"
      LeanTea.Net.Desktop.keyPress kc.toUInt32
      return textContent s!"pressed keycode {kc}"
    | "desktop_screen_size" =>
      let (w, h) ← LeanTea.Net.Desktop.screenSize
      return textContent s!"{w}x{h}"
    | "desktop_backend" =>
      let b ← LeanTea.Net.Desktop.backendName
      return textContent b
    | "ui_recall" =>
      match getStr args "key" with
      | .error e => return errContent e
      | .ok k =>
        let v ← LeanTea.Agent.Memory.recall k
        return textContent v.compress
    | "ui_remember" =>
      match getStr args "key" with
      | .error e => return errContent e
      | .ok k =>
        let v := (args.getObjVal? "value").toOption.getD Json.null
        LeanTea.Agent.Memory.remember k v
        return textContent s!"remembered {k}"
    | "ui_list" =>
      let ks ← LeanTea.Agent.Memory.keys
      return textContent (Json.arr (ks.map Json.str)).compress
    | "agent_escalate" =>
      match getStr args "reason" with
      | .error e => return errContent e
      | .ok r =>
        let ctx := (args.getObjVal? "context").toOption.getD Json.null
        LeanTea.Agent.Memory.escalate r ctx
        return textContent s!"escalated: {r}"
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## HTTP + stdio transports — supplied by `LeanTea.Mcp.Handler` -/

private structure Args where
  mode : String := "stdio"
  port : UInt16 := 8010
  host : String := "0.0.0.0"

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port" :: v :: rest  => parseArgs rest { a with mode := "http",
                                                       port := (v.toNat?.getD 8010).toUInt16 }
  | "--host" :: v :: rest  => parseArgs rest { a with host := v }
  | _ :: rest              => parseArgs rest a
  | []                     => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  let backend ← LeanTea.Net.Desktop.backendName
  IO.eprintln s!"desktop-mcp: backend={backend}"
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool
  }
  match a.mode with
  | "http" =>
    IO.eprintln s!"desktop-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "desktop-mcp: stdio mode"
    mcpHandler.serveStdio

end DesktopMcp

def main (args : List String) : IO Unit := DesktopMcp.serveMain args
