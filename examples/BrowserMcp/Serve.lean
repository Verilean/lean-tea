import LeanTea
import Lean.Data.Json

/-! # browser_mcp_serve — MCP server that drives a real browser

Exposes the Playwright-backed `LeanTea.Browser` surface as a set of
MCP tools at `POST /mcp` (JSON-RPC 2.0). One server process owns one
browser session — call `browser_open` to start it (or just
`browser_navigate` which opens implicitly), then `browser_screenshot`
to get a base64 PNG you can hand to a vision model, etc.

Same JSON-RPC shape as `examples/Canvas/Serve.lean`'s MCP block —
methods covered:

  * `initialize`     — handshake
  * `tools/list`     — return the tool catalogue
  * `tools/call`     — run a tool

Tools:

  | name                  | purpose |
  |-----------------------|---------|
  | browser_open          | start / restart the page |
  | browser_navigate      | goto(url) |
  | browser_click         | click(selector) |
  | browser_fill          | fill(selector, text) |
  | browser_press         | press(key) |
  | browser_wait_for      | wait for selector |
  | browser_get_text      | innerText(selector) |
  | browser_get_html      | innerHTML(selector) |
  | browser_evaluate      | run JS in the page |
  | browser_screenshot    | base64 PNG of viewport or selector |
  | browser_close         | shut down the browser | -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace BrowserMcp

/-! ## Session — shared across MCP requests -/

/-- One MCP-server process owns one browser. The `IO.Ref` lets a
    request lazily spawn it (so `browser_mcp_serve` can boot without
    Playwright until the first tool call). -/
abbrev SessionRef := IO.Ref (Option LeanTea.Browser.Session)

private def getSession (ref : SessionRef) : IO LeanTea.Browser.Session := do
  match ← ref.get with
  | some s => return s
  | none   =>
    let s ← LeanTea.Browser.Session.spawn
    ref.set (some s)
    return s

private def closeSession (ref : SessionRef) : IO Unit := do
  match ← ref.get with
  | some s =>
    try s.close catch _ => pure ()
    ref.set none
  | none => pure ()

/-! ## MCP shapes — see `LeanTea.Mcp` for the shared implementation. -/

open LeanTea.Mcp (jsonOk jsonErr textContent errContent imageContent
                  argSchema toolDef defaultInitializeResult)

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "browser_open"
        ("Start / restart the Chromium page. `headless: false` opens "
         ++ "a visible window (great for live debugging); `headless: "
         ++ "true` runs hidden (default — use for CI / batch). The "
         ++ "chosen mode persists across subsequent implicit opens.")
        #[ argSchema "width"  "number"  "viewport width (px)",
           argSchema "height" "number"  "viewport height (px)",
           argSchema "headless" "boolean" "true = hidden window (default); false = visible" ]
        #[],
      toolDef "browser_navigate" "Open the given URL."
        #[ argSchema "url" "string" "destination URL" ] #["url"],
      toolDef "browser_click" "Click the element matching the CSS selector."
        #[ argSchema "selector" "string" "CSS selector" ] #["selector"],
      toolDef "browser_click_xy"
        ("Click at viewport pixel coordinates (x, y). Use this for "
         ++ "canvas-rendered UIs (games, WebGL apps) where there is no "
         ++ "DOM element to select — take a screenshot, read off the "
         ++ "button's pixel position, click there.")
        #[ argSchema "x" "number" "x coordinate in pixels (0 = left edge)",
           argSchema "y" "number" "y coordinate in pixels (0 = top edge)" ]
        #["x", "y"],
      toolDef "browser_fill" "Type text into an input matching the selector."
        #[ argSchema "selector" "string" "CSS selector for the input",
           argSchema "text"     "string" "text to type" ] #["selector", "text"],
      toolDef "browser_press" "Press a single key (e.g. Enter, Tab)."
        #[ argSchema "key"      "string" "key name",
           argSchema "selector" "string" "(optional) focus this selector first" ]
        #["key"],
      toolDef "browser_wait_for" "Wait until a selector reaches a given state."
        #[ argSchema "selector" "string" "CSS selector",
           argSchema "state"    "string" "visible | attached | hidden | detached" ]
        #["selector"],
      toolDef "browser_get_text" "Return innerText of an element (default: body)."
        #[ argSchema "selector" "string" "CSS selector" ] #[],
      toolDef "browser_get_html" "Return innerHTML of an element (default: body)."
        #[ argSchema "selector" "string" "CSS selector" ] #[],
      toolDef "browser_evaluate"
        "Run an expression in the page; return the JSON-serialisable result."
        #[ argSchema "expression" "string" "JS expression" ] #["expression"],
      toolDef "browser_screenshot"
        ("Take a screenshot. Returns a base64 PNG (visible in the "
         ++ "response as an `image` content block). When `outputPath` "
         ++ "is given the raw image bytes are also written to that "
         ++ "absolute path — handy for piping the file straight into "
         ++ "another tool (e.g. `curl --data-binary @path` to a vision "
         ++ "LLM) without having to decode the base64.")
        #[ argSchema "selector"  "string"  "(optional) scope to this element",
           ("fullPage", Json.mkObj [("type", Json.str "boolean"),
                                     ("description", Json.str "capture beyond the viewport")]),
           argSchema "outputPath" "string"  "(optional) absolute path to also save the raw PNG/JPEG bytes" ]
        #[],
      toolDef "browser_close" "Shut down the browser session." #[] #[],
      toolDef "ui_recall"
        ("Look up a previously learned UI element by key (e.g. "
         ++ "\"dmm.quest_tab\"). Returns the stored JSON (typically "
         ++ "{x, y, notes, …}) or null if not yet learned. Call this "
         ++ "BEFORE guessing coordinates from a screenshot — most "
         ++ "buttons you'll ever need have probably been mapped "
         ++ "already.")
        #[ argSchema "key" "string" "element key, e.g. dmm.quest_tab" ]
        #["key"],
      toolDef "ui_remember"
        ("Save a verified UI element to the shared map. Call this "
         ++ "AFTER you clicked at (x,y), screenshot-verified the click "
         ++ "actually triggered the right action, and want to spare "
         ++ "yourself / the next agent run from re-deriving the "
         ++ "coordinate. `value` is freeform JSON — at minimum "
         ++ "{x: number, y: number}, plus any notes you want.")
        #[ argSchema "key" "string" "element key, e.g. dmm.quest_tab",
           ("value", Json.mkObj [
             ("type", Json.str "object"),
             ("description",
               Json.str "freeform — {x, y, notes?, screenshot?, …}")
           ]) ]
        #["key", "value"],
      toolDef "ui_list"
        ("List all keys currently in the UI map. Use this when you "
         ++ "want to see what's known before guessing — saves a "
         ++ "round-trip vs `ui_recall` on every guess.")
        #[]
        #[],
      toolDef "agent_escalate"
        ("Flag the current task as needing human / stronger-model "
         ++ "review. Use this when you've tried and failed several "
         ++ "times, when you see an unexpected screen, when the page "
         ++ "asks for input you can't supply (e.g. a CAPTCHA), or "
         ++ "when the user's intent is genuinely ambiguous. Writes "
         ++ "the event to ~/.cache/leantea-agent/escalations.jsonl. "
         ++ "After calling this, stop and explain to the user that "
         ++ "you escalated.")
        #[ argSchema "reason"  "string"  "one-line summary of why",
           ("context", Json.mkObj [
             ("type", Json.str "object"),
             ("description",
               Json.str "any extra context — current task, last URL, etc.")
           ]) ]
        #["reason"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-browser-mcp"

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

private def getBoolOpt (args : Json) (k : String) (default : Bool := false) : Bool :=
  match args.getObjVal? k with
  | .ok (.bool b) => b
  | _             => default

private def getNatOpt (args : Json) (k : String) (default : Nat := 0) : Nat :=
  match args.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

/-! ## Dispatcher -/

def callTool (ref : SessionRef) (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "browser_open" =>
      let w := getNatOpt args "width"
      let h := getNatOpt args "height"
      /- `headless` is tri-valued: omitted (none = inherit bridge
         default), `true` (hidden), or `false` (visible window). -/
      let headlessOpt : Option Bool :=
        match args.getObjVal? "headless" with
        | .ok (.bool b) => some b
        | _             => none
      let s ← getSession ref
      let (w', h', hl) ← s.open w h headlessOpt
      let mode := if hl then "headless" else "headed"
      return textContent s!"open {w'}x{h'} ({mode})"
    | "browser_navigate" =>
      match getStr args "url" with
      | .error e => return errContent e
      | .ok url  =>
        let s ← getSession ref
        let r ← s.navigate url
        return textContent s!"navigated → {r.url} (title: {r.title})"
    | "browser_click" =>
      match getStr args "selector" with
      | .error e => return errContent e
      | .ok sel  =>
        let s ← getSession ref
        s.click sel
        return textContent s!"clicked {sel}"
    | "browser_click_xy" =>
      let x := getNatOpt args "x"
      let y := getNatOpt args "y"
      let s ← getSession ref
      s.clickXy x y
      return textContent s!"clicked ({x}, {y})"
    | "browser_fill" =>
      match getStr args "selector", getStr args "text" with
      | .ok sel, .ok t =>
        let s ← getSession ref
        s.fill sel t
        return textContent s!"filled {sel}"
      | .error e, _ => return errContent e
      | _, .error e => return errContent e
    | "browser_press" =>
      match getStr args "key" with
      | .error e => return errContent e
      | .ok k =>
        let s ← getSession ref
        let selOpt := match getStrOpt args "selector" "" with | "" => none | x => some x
        s.press k selOpt
        return textContent s!"pressed {k}"
    | "browser_wait_for" =>
      match getStr args "selector" with
      | .error e => return errContent e
      | .ok sel =>
        let st := getStrOpt args "state" "visible"
        let s ← getSession ref
        s.waitFor sel st
        return textContent s!"saw {sel} (state={st})"
    | "browser_get_text" =>
      let sel := getStrOpt args "selector" "body"
      let s ← getSession ref
      let t ← s.getText sel
      return textContent t
    | "browser_get_html" =>
      let sel := getStrOpt args "selector" "body"
      let s ← getSession ref
      let h ← s.getHtml sel
      return textContent h
    | "browser_evaluate" =>
      match getStr args "expression" with
      | .error e => return errContent e
      | .ok expr =>
        let s ← getSession ref
        let r ← s.evaluate expr
        return textContent r.compress
    | "browser_screenshot" =>
      let selOpt := match getStrOpt args "selector" "" with | "" => none | x => some x
      let fp := getBoolOpt args "fullPage"
      let outOpt := match getStrOpt args "outputPath" "" with | "" => none | x => some x
      let s ← getSession ref
      let shot ← s.screenshot selOpt fp outOpt
      let suffix := if shot.outputPath.isEmpty then ""
                    else s!" → {shot.outputPath}"
      return imageContent shot.mime shot.base64
        s!"{shot.width}×{shot.height} screenshot ({shot.bytes} bytes){suffix}"
    | "browser_close" =>
      closeSession ref
      return textContent "closed"
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

/-! ## Transports — supplied by `LeanTea.Mcp.Handler`. -/

private structure Args where
  mode : String := "stdio"     -- "stdio" (default, for MCP clients) | "http"
  port : UInt16 := 8009
  host : String := "0.0.0.0"

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port" :: v :: rest  => parseArgs rest { a with mode := "http",
                                                       port := (v.toNat?.getD 8009).toUInt16 }
  | "--host" :: v :: rest  => parseArgs rest { a with host := v }
  | _ :: rest              => parseArgs rest a
  | []                     => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  let ref ← IO.mkRef (none : Option LeanTea.Browser.Session)
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool ref
  }
  match a.mode with
  | "http" =>
    IO.println s!"browser-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    /- Default is stdio because every MCP client (Claude Code,
       Claude Desktop, Cursor, …) spawns the binary and talks over
       its pipes. Debug noise goes to stderr to stay out of the
       protocol stream. -/
    IO.eprintln "browser-mcp: stdio mode (one JSON-RPC line per request on stdin)"
    mcpHandler.serveStdio

end BrowserMcp

def main (args : List String) : IO Unit := BrowserMcp.serveMain args
