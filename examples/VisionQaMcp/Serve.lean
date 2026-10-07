import LeanTea
import Lean.Data.Json

/-! # vision_qa_mcp_serve — a local VLM's eyes as MCP tools

Pairs with any screenshot-producing MCP server (`browser_mcp_serve`,
`desktop_mcp_serve`, `chrome_cdp_mcp_serve`, …): take a screenshot to
a file there, then ask here *where* / *whether* / *what text*, then
click the returned pixel there. Backed by `LeanTea.Vision.Ground`
against LM Studio (or any OpenAI-compatible server).

Tools:
* `vision_locate`    — description → click point + box (zoom-refined)
* `vision_ask`       — yes/no question about the screenshot
* `vision_read_text` — transcribe a region
* `vision_classify`  — which of N named screens is this?
* `vision_pixel`     — exact RGBA at a pixel (no LLM)

Coordinates are pixels of the given image. For the macOS desktop
(Retina) divide by the scale (`image width / desktop_screen_size width`)
before `desktop_click_xy`.

```
vision_qa_mcp_serve [--model qwen/qwen3-vl-4b] [--frame norm1000]
                    [--base-url URL] [--port 8014 | --stdio]
``` -/

open LeanTea LeanTea.Vision LeanTea.Mcp
open Lean (Json)

namespace VisionQaMcp

def toolsList : Json :=
  Json.mkObj [("tools", Json.arr #[
    toolDef "vision_locate"
      ("Find a UI element in a screenshot from a natural-language description "
       ++ "(e.g. 'the Save button', 'the gear icon in the toolbar'). Returns the "
       ++ "click point {x, y} and box in image pixels. Uses a coarse pass plus a "
       ++ "zoomed second pass for small targets.")
      #[ argSchema "image"  "string"  "absolute path to a PNG/JPEG screenshot",
         argSchema "target" "string"  "what to find",
         argSchema "zoom"   "boolean" "(default true) refine with a zoomed second pass" ]
      #["image", "target"],
    toolDef "vision_ask"
      "Ask a yes/no question about a screenshot. Returns answer (yes/no/unknown) and the model's reason."
      #[ argSchema "image"    "string" "absolute path to the screenshot",
         argSchema "question" "string" "yes/no question, e.g. 'Is the Remember me checkbox checked?'" ]
      #["image", "question"],
    toolDef "vision_read_text"
      "Transcribe the text inside a rectangle of a screenshot (image pixels)."
      #[ argSchema "image" "string" "absolute path to the screenshot",
         argSchema "x" "number" "left", argSchema "y" "number" "top",
         argSchema "w" "number" "width", argSchema "h" "number" "height" ]
      #["image", "x", "y", "w", "h"],
    toolDef "vision_classify"
      "Pick which named screen the screenshot shows, or 'unknown'."
      #[ argSchema "image" "string" "absolute path to the screenshot",
         argSchema "candidates" "string" "comma-separated screen names" ]
      #["image", "candidates"],
    toolDef "vision_pixel"
      "Exact RGBA colour at a pixel — deterministic, no model involved."
      #[ argSchema "image" "string" "absolute path to the screenshot",
         argSchema "x" "number" "x", argSchema "y" "number" "y" ]
      #["image", "x", "y"]
  ])]

private def str (j : Json) (k : String) : Except String String :=
  (j.getObjVal? k).bind (·.getStr?)

private def nat (j : Json) (k : String) : Nat :=
  match j.getObjValD k with | .num n => n.toFloat.round.toUInt64.toNat | _ => 0

private def r1 (f : Float) : Json :=
  (Json.parse (toString ((f * 10).round / 10))).toOption.getD (Json.num 0)

def callTool (v : Vlm) (name : String) (args : Json) : IO Json := do
  try
    let img ← Image.load (← IO.ofExcept (str args "image"))
    match name with
    | "vision_locate" =>
      let target ← IO.ofExcept (str args "target")
      let zoom := match args.getObjValD "zoom" with | .bool b => b | _ => true
      let r ← if zoom then locateZoom v img target else locate v img target
      match r.rect? with
      | none => return errContent s!"could not locate {target}; model said: {r.answer}"
      | some b =>
        return textContent (Json.mkObj [
          ("x", .num b.cx.round.toUInt64.toNat), ("y", .num b.cy.round.toUInt64.toNat),
          ("box", Json.mkObj [("x", r1 b.x), ("y", r1 b.y), ("w", r1 b.w), ("h", r1 b.h)]),
          ("imageWidth", .num img.width), ("imageHeight", .num img.height),
          ("ms", .num r.ms)]).compress
    | "vision_ask" =>
      let a ← ask v img (← IO.ofExcept (str args "question"))
      let ans := match a.yes? with | some true => "yes" | some false => "no" | none => "unknown"
      return textContent (Json.mkObj [("answer", .str ans), ("raw", .str a.answer), ("ms", .num a.ms)]).compress
    | "vision_read_text" =>
      let rect : Rect := { x := (nat args "x").toFloat, y := (nat args "y").toFloat,
                           w := (nat args "w").toFloat, h := (nat args "h").toFloat }
      let a ← readText v img rect
      return textContent a.text
    | "vision_classify" =>
      let cands := (← IO.ofExcept (str args "candidates")).splitOn "," |>.map (·.trimAscii.toString)
      return textContent (← classify v img cands)
    | "vision_pixel" =>
      let (r, g, b, a) := img.pixel (nat args "x") (nat args "y")
      return textContent s!"rgba({r},{g},{b},{a})"
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

private structure Args where
  mode    : String := "stdio"
  port    : UInt16 := 8014
  host    : String := "127.0.0.1"
  model   : String := "qwen/qwen3-vl-4b"
  frame?  : Option CoordFrame := none
  baseUrl : String := "http://127.0.0.1:11211/v1"

private partial def parseArgs : List String → Args → Args
  | "--stdio" :: r, a         => parseArgs r { a with mode := "stdio" }
  | "--port" :: v :: r, a     => parseArgs r { a with mode := "http", port := (v.toNat?.getD 8014).toUInt16 }
  | "--host" :: v :: r, a     => parseArgs r { a with host := v }
  | "--model" :: v :: r, a    => parseArgs r { a with model := v }
  | "--frame" :: v :: r, a    => parseArgs r { a with frame? := CoordFrame.ofName? v }
  | "--base-url" :: v :: r, a => parseArgs r { a with baseUrl := v }
  | _ :: r, a => parseArgs r a
  | [], a => a

def serveMain (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  let v : Vlm := { cfg := { baseUrl := a.baseUrl, timeoutSec := some 300 }, model := a.model,
                   frame := a.frame?.getD (CoordFrame.guess a.model) }
  let h : LeanTea.Mcp.Handler := {
    initializeResult := defaultInitializeResult "lean-tea-vision-qa-mcp",
    toolsList, callTool := callTool v }
  IO.eprintln s!"vision-qa-mcp: model={v.model} frame={v.frame.name}"
  if a.mode == "http" then
    IO.eprintln s!"vision-qa-mcp: POST http://{a.host}:{a.port}/mcp"
    h.serveHttp a.port a.host
  else h.serveStdio

end VisionQaMcp

def main (argv : List String) : IO Unit := VisionQaMcp.serveMain argv
