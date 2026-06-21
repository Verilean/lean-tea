import LeanTea
import Lean.Data.Json

/-! # image_mcp_serve — MCP server for image composition

Wraps the headless Chromium bridge (same one `browser_mcp_serve`
uses) as an HTML-based image compositor. Speech bubbles, captions,
text overlays — anything you can lay out in CSS becomes a one-call
PNG.

Why HTML/CSS instead of PIL:

* Designers can edit the bubble style in `templates/*.html` without
  touching Lean.
* The same CSS that styles the in-game dialogue box styles the
  asset-pipeline overlays, so visual style stays consistent.
* Web fonts (e.g. `Zen Maru Gothic`) work out of the box; no need
  to ship `.ttc` files in the build.

Cost: ~1.5 s per composite (the bridge has to load fonts + paint
once). For batch jobs that's still cheap vs the 30-60 s a FLUX call
takes upstream.

```
image_mcp_serve --port 8012      # HTTP for curl smoke
image_mcp_serve                  # stdio for MCP clients
``` -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open Lean (Json)

namespace ImageMcp

/-! ## Shared headless browser session -/

abbrev SessionRef := IO.Ref (Option LeanTea.Browser.Session)

private def ensureSession (ref : SessionRef) (w h : Nat) : IO LeanTea.Browser.Session := do
  match ← ref.get with
  | some s =>
    /- Re-open to match requested viewport. `Session.open` reuses
       the running browser and just swaps the page size. -/
    let _ ← s.open w h (some true)
    return s
  | none =>
    let s ← LeanTea.Browser.Session.spawn
    let _ ← s.open w h (some true)
    ref.set (some s)
    return s

private def closeSession (ref : SessionRef) : IO Unit := do
  match ← ref.get with
  | some s => try s.close catch _ => pure ()
              ref.set none
  | none   => pure ()

/-! ## Default templates

Inlined HTML/CSS for the standard overlays. Power users hand a path
via the tool's `templatePath` arg to override; otherwise these
kick in. The placeholder syntax is `{{NAME}}` — replaced verbatim
before writing the file. -/

private def defaultSpeechBubble : String := "<!doctype html>
<html lang=\"ja\">
<head>
<meta charset=\"utf-8\">
<style>
  @import url('https://fonts.googleapis.com/css2?family=Zen+Maru+Gothic:wght@500;700&display=swap');
  html, body { margin: 0; padding: 0; background: transparent; }
  body {
    width: {{WIDTH}}px; height: {{HEIGHT}}px;
    position: relative;
    font-family: 'Zen Maru Gothic', 'Hiragino Maru Gothic ProN', system-ui, sans-serif;
  }
  .char {
    position: absolute; inset: 0;
    background: url(\"data:image/png;base64,{{CHAR_B64}}\") center/cover no-repeat;
  }
  .bubble {
    position: absolute;
    top: {{BUBBLE_Y}}px;
    right: {{BUBBLE_X}}px;
    min-width: 220px; max-width: 360px;
    background: #fff;
    border: 4px solid #1a1a1a;
    border-radius: 28px;
    padding: 22px 28px;
    box-shadow: 4px 4px 0 rgba(0,0,0,.15);
    text-align: center;
    font-weight: 700;
    font-size: {{FONT_SIZE}}px;
    color: #1a1a1a;
    line-height: 1.1;
    letter-spacing: 0.04em;
  }
  .bubble::before, .bubble::after {
    content: \"\"; position: absolute;
    left: 50px;
    width: 0; height: 0;
    border-style: solid;
  }
  .bubble::before {
    bottom: -34px;
    border-width: 30px 26px 0 0;
    border-color: #1a1a1a transparent transparent transparent;
  }
  .bubble::after {
    bottom: -25px;
    border-width: 22px 18px 0 0;
    border-color: #fff transparent transparent transparent;
  }
</style>
</head>
<body>
  <div class=\"char\"></div>
  <div class=\"bubble\">{{TEXT}}</div>
</body>
</html>
"

/-- Full visual-novel scene: background + character + optional
    dialog (caption bar) or speech bubble. Layers via CSS, so the
    in-game UI styling can share the exact same stylesheet. -/
private def defaultScene : String := "<!doctype html>
<html lang=\"ja\">
<head>
<meta charset=\"utf-8\">
<style>
  @import url('https://fonts.googleapis.com/css2?family=Zen+Maru+Gothic:wght@500;700&display=swap');
  html, body { margin: 0; padding: 0; }
  body {
    width: {{WIDTH}}px; height: {{HEIGHT}}px;
    position: relative; overflow: hidden;
    font-family: 'Zen Maru Gothic', 'Hiragino Maru Gothic ProN', system-ui, sans-serif;
  }
  .bg {
    position: absolute; inset: 0;
    background: url(\"data:image/png;base64,{{BG_B64}}\") center/cover no-repeat;
  }
  .bg::after {
    content: \"\"; position: absolute; inset: 0;
    background: linear-gradient(180deg, rgba(0,0,0,0) 35%, rgba(0,0,0,0.45) 100%);
  }
  .char {
    position: absolute;
    bottom: {{CHAR_Y}}px;
    left: 50%; transform: translateX(-50%);
    width: {{CHAR_W}}px; height: {{CHAR_H}}px;
    background: url(\"data:image/png;base64,{{CHAR_B64}}\") center top/contain no-repeat;
    /* multiply blend turns the white asset background invisible.
       For pristine alpha use the BRIA RMBG comfyui workflow first
       and bake true transparency into the character PNG. */
    mix-blend-mode: multiply;
    filter: brightness(1.15) contrast(1.08);
  }
  /* Optional dialog box (set DIALOG_DISPLAY=block when caption used) */
  .caption {
    display: {{DIALOG_DISPLAY}};
    position: absolute;
    left: 40px; right: 40px;
    bottom: 36px;
    background: rgba(20,20,28,0.86);
    color: #fff;
    border: 2px solid rgba(255,255,255,0.18);
    border-radius: 12px;
    padding: 22px 28px;
    font-weight: 500; font-size: {{FONT_SIZE}}px;
    line-height: 1.45; letter-spacing: 0.02em;
    backdrop-filter: blur(6px);
  }
  .speaker {
    color: #ffd479; font-weight: 700; margin-bottom: 6px;
  }
</style>
</head>
<body>
  <div class=\"bg\"></div>
  <div class=\"char\"></div>
  <div class=\"caption\">
    <div class=\"speaker\">{{SPEAKER}}</div>
    {{TEXT}}
  </div>
</body>
</html>
"

private def defaultCaptionBar : String := "<!doctype html>
<html lang=\"ja\">
<head>
<meta charset=\"utf-8\">
<style>
  @import url('https://fonts.googleapis.com/css2?family=Zen+Maru+Gothic:wght@500;700&display=swap');
  html, body { margin: 0; padding: 0; background: transparent; }
  body {
    width: {{WIDTH}}px; height: {{HEIGHT}}px;
    position: relative;
    font-family: 'Zen Maru Gothic', 'Hiragino Maru Gothic ProN', system-ui, sans-serif;
  }
  .char {
    position: absolute; inset: 0;
    background: url(\"data:image/png;base64,{{CHAR_B64}}\") center/cover no-repeat;
  }
  .caption {
    position: absolute;
    left: 32px; right: 32px;
    bottom: 32px;
    background: rgba(20,20,28,0.84);
    color: #fff;
    border: 2px solid rgba(255,255,255,0.16);
    border-radius: 12px;
    padding: 20px 24px;
    font-weight: 500;
    font-size: {{FONT_SIZE}}px;
    line-height: 1.45;
    letter-spacing: 0.02em;
    backdrop-filter: blur(6px);
  }
  .speaker {
    color: #ffd479;
    font-weight: 700;
    margin-bottom: 6px;
  }
</style>
</head>
<body>
  <div class=\"char\"></div>
  <div class=\"caption\">
    <div class=\"speaker\">{{SPEAKER}}</div>
    {{TEXT}}
  </div>
</body>
</html>
"

/-! ## HTML rendering -/

private def replaceAll (s : String) (subs : List (String × String)) : String :=
  subs.foldl (fun acc (k, v) => acc.replace ("{{" ++ k ++ "}}") v) s

/-- Render an HTML string through the headless bridge to a PNG. We
    use a `file://` URL because `data:` URLs hit Chromium's
    max-length limit when the embedded character image is large. -/
private def renderHtml (ref : SessionRef) (html : String) (w h : Nat)
    (outputPath : String) (waitMs : Nat := 1500) : IO Unit := do
  let tmpHtml := s!"/tmp/image-mcp-{← IO.monoMsNow}.html"
  IO.FS.writeFile tmpHtml html
  let session ← ensureSession ref w h
  let _ ← session.navigate s!"file://{tmpHtml}"
  /- Give web fonts a beat to fetch (Google Fonts → cache).
     1.5 s is enough on warm, ~2-3 s on first run. -/
  let _ ← session.evaluate s!"new Promise(r=>setTimeout(r,{waitMs}))"
  let _ ← session.screenshot none false (some outputPath)
  try IO.FS.removeFile tmpHtml catch _ => pure ()

/-! ## Tool: speech bubble -/

def speechBubble (ref : SessionRef)
    (charPath text outputPath : String)
    (width height : Nat)
    (bubbleX bubbleY fontSize : Nat)
    (templatePath : Option String := none) : IO Unit := do
  let bytes ← IO.FS.readBinFile charPath
  let b64 := LeanTea.Llm.Openai.base64Encode bytes
  let template ← match templatePath with
    | some p => IO.FS.readFile p
    | none   => pure defaultSpeechBubble
  let html := replaceAll template [
    ("CHAR_B64",  b64),
    ("TEXT",      text),
    ("WIDTH",     toString width),
    ("HEIGHT",    toString height),
    ("BUBBLE_X",  toString bubbleX),
    ("BUBBLE_Y",  toString bubbleY),
    ("FONT_SIZE", toString fontSize)
  ]
  renderHtml ref html width height outputPath

/-! ## Tool: caption bar (visual-novel-style bottom dialog box) -/

/-! ## Tool: background removal

Shells out to `tools/remove_bg.py` so the heavy lifting (numpy +
optional `rembg` ML) stays where Python's strong. The script tries
`rembg` first (catches non-white decorative noise like the hearts
FLUX likes to add) and falls back to a numpy white-key threshold
when rembg isn't installed — fine for clean white-bg sprites. -/

private def findRemoveBgScript : IO String := do
  /- Same candidate-path pattern other Lean exes use so we don't
     break when invoked from `lake build`'s cwd vs the repo root. -/
  let candidates := [
    "tools/remove_bg.py",
    "../tools/remove_bg.py",
    "../../tools/remove_bg.py",
    "lean-elm/tools/remove_bg.py"
  ]
  for p in candidates do
    if ← System.FilePath.pathExists p then return p
  throw <| IO.userError
    "image-mcp: tools/remove_bg.py not found — run from the project root or set LEANTEA_REMOVE_BG_SCRIPT"

def removeBackground (input output : String) (threshold : Nat := 235) : IO String := do
  let script ← match ← IO.getEnv "LEANTEA_REMOVE_BG_SCRIPT" with
    | some p => pure p
    | none   => findRemoveBgScript
  let out ← IO.Process.output {
    cmd := "python3",
    args := #[script, input, output, "--threshold", toString threshold]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"remove_bg.py failed: {out.stderr}"
  return out.stdout.trimAscii.toString

def captionBar (ref : SessionRef)
    (charPath speaker text outputPath : String)
    (width height fontSize : Nat)
    (templatePath : Option String := none) : IO Unit := do
  let bytes ← IO.FS.readBinFile charPath
  let b64 := LeanTea.Llm.Openai.base64Encode bytes
  let template ← match templatePath with
    | some p => IO.FS.readFile p
    | none   => pure defaultCaptionBar
  let html := replaceAll template [
    ("CHAR_B64",  b64),
    ("TEXT",      text),
    ("SPEAKER",   speaker),
    ("WIDTH",     toString width),
    ("HEIGHT",    toString height),
    ("FONT_SIZE", toString fontSize)
  ]
  renderHtml ref html width height outputPath

/-! ## MCP shapes — see `LeanTea.Mcp` for the shared implementation. -/

open LeanTea.Mcp (jsonOk jsonErr textContent errContent imageContent
                  argSchema toolDef defaultInitializeResult)

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "image_speech_bubble"
        ("Overlay a manga-style speech bubble on a character image. "
         ++ "Renders an HTML/CSS template via headless Chromium so "
         ++ "fonts and styles stay consistent across the asset set.")
        #[ argSchema "input"        "string" "absolute path to the character PNG",
           argSchema "text"         "string" "bubble text (UTF-8, can contain newlines)",
           argSchema "output"       "string" "absolute path to write the composite PNG",
           argSchema "width"        "number" "canvas width (default 768)",
           argSchema "height"       "number" "canvas height (default 1024)",
           argSchema "bubbleX"      "number" "bubble x from right (default 24)",
           argSchema "bubbleY"      "number" "bubble y from top (default 32)",
           argSchema "fontSize"     "number" "bubble font size (default 56)",
           argSchema "templatePath" "string" "(optional) custom HTML template with placeholders {{CHAR_B64}}, {{TEXT}}, …" ]
        #["input", "text", "output"],
      toolDef "image_remove_background"
        ("Strip the background from a character PNG. Tries the `rembg` "
         ++ "ML library first (catches non-white decoration like the "
         ++ "stray hearts FLUX likes to add); falls back to a numpy "
         ++ "white-key threshold when rembg isn't installed.")
        #[ argSchema "input"     "string" "absolute path to the source PNG",
           argSchema "output"    "string" "absolute path to write the alpha PNG",
           argSchema "threshold" "number" "white-key cutoff for the PIL fallback (default 235)" ]
        #["input", "output"],
      toolDef "image_caption_bar"
        ("Overlay a visual-novel bottom dialog box (speaker name "
         ++ "+ body text) on a character image. Same template system "
         ++ "as `image_speech_bubble`.")
        #[ argSchema "input"        "string" "absolute path to the character PNG",
           argSchema "speaker"      "string" "speaker name (shown above the body)",
           argSchema "text"         "string" "dialog body",
           argSchema "output"       "string" "absolute path to write the composite PNG",
           argSchema "width"        "number" "canvas width (default 768)",
           argSchema "height"       "number" "canvas height (default 1024)",
           argSchema "fontSize"     "number" "body font size (default 28)",
           argSchema "templatePath" "string" "(optional) custom HTML template" ]
        #["input", "text", "output"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-image-mcp"

/-! ## Args extraction -/

private def getStr (args : Json) (k : String) : Except String String :=
  match args.getObjVal? k with
  | .ok v => v.getStr?
  | .error e => .error e

private def getStrOpt (args : Json) (k : String) (default : String := "") : String :=
  match args.getObjVal? k with
  | .ok v => match v.getStr? with | .ok s => s | _ => default
  | _ => default

private def getNatOpt (args : Json) (k : String) (default : Nat := 0) : Nat :=
  match args.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

/-! ## Dispatcher -/

def callTool (ref : SessionRef) (name : String) (args : Json) : IO Json := do
  try
    match name with
    | "image_speech_bubble" =>
      match getStr args "input", getStr args "text", getStr args "output" with
      | .ok inp, .ok text, .ok out =>
        let w    := match getNatOpt args "width"    with | 0 => 768  | n => n
        let h    := match getNatOpt args "height"   with | 0 => 1024 | n => n
        let bx   := match getNatOpt args "bubbleX"  with | 0 => 24   | n => n
        let by_  := match getNatOpt args "bubbleY"  with | 0 => 32   | n => n
        let fs   := match getNatOpt args "fontSize" with | 0 => 56   | n => n
        let tpl  := match getStrOpt args "templatePath" "" with
                    | "" => none | p => some p
        speechBubble ref inp text out w h bx by_ fs tpl
        return textContent s!"speech bubble → {out}"
      | .error e, _, _ => return errContent s!"input: {e}"
      | _, .error e, _ => return errContent s!"text: {e}"
      | _, _, .error e => return errContent s!"output: {e}"
    | "image_remove_background" =>
      match getStr args "input", getStr args "output" with
      | .ok inp, .ok out =>
        let th := match getNatOpt args "threshold" with | 0 => 235 | n => n
        let backend ← removeBackground inp out th
        return textContent s!"remove_bg [{backend}] → {out}"
      | .error e, _ => return errContent s!"input: {e}"
      | _, .error e => return errContent s!"output: {e}"
    | "image_caption_bar" =>
      match getStr args "input", getStr args "text", getStr args "output" with
      | .ok inp, .ok text, .ok out =>
        let speaker := getStrOpt args "speaker"
        let w  := match getNatOpt args "width"    with | 0 => 768  | n => n
        let h  := match getNatOpt args "height"   with | 0 => 1024 | n => n
        let fs := match getNatOpt args "fontSize" with | 0 => 28   | n => n
        let tpl := match getStrOpt args "templatePath" "" with
                   | "" => none | p => some p
        captionBar ref inp speaker text out w h fs tpl
        return textContent s!"caption bar → {out}"
      | .error e, _, _ => return errContent s!"input: {e}"
      | _, .error e, _ => return errContent s!"text: {e}"
      | _, _, .error e => return errContent s!"output: {e}"
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## Transports — supplied by `LeanTea.Mcp.Handler`. -/

private structure Args where
  mode : String := "stdio"
  port : UInt16 := 8012
  host : String := "0.0.0.0"

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port" :: v :: rest  => parseArgs rest { a with mode := "http",
                                                       port := (v.toNat?.getD 8012).toUInt16 }
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
    IO.eprintln s!"image-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "image-mcp: stdio mode"
    mcpHandler.serveStdio

end ImageMcp

def main (args : List String) : IO Unit := ImageMcp.serveMain args
