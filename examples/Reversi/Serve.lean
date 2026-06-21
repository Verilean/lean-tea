import LeanTea
import LeanJs.Parser
import LeanJs.Codegen
import Reversi.Game

/-! # reversi_serve — Reversi SPA driven by `Game.leanjs` + `page.html`

At startup:
  1. Load the LeanJs subset source from `examples/Reversi/Game.leanjs`
  2. Parse + compile it to JavaScript via `LeanJs.Codegen`
  3. Load the HTML harness template from `examples/Reversi/page.html`
  4. Substitute the compiled JS into the `{{gameJs}}` placeholder

The HTML lives as an actual `.html` file so editors highlight it
correctly and a non-Lean developer can tweak the layout without
learning the framework's string-escape rules.

With `--dev`, both `Game.leanjs` and `page.html` are re-read +
re-parsed on every request, so edits to either show up on the
next browser refresh (no rebuild, no restart). Without it, the
compiled JS and parsed template are cached once at startup. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanJs

namespace ReversiServe

/-- Compile the subset source once. Returns the JS plus an "is this
    an error envelope?" flag. -/
def compileGame : IO (String × Bool) := do
  let src ← Reversi.loadSource
  match Parser.parseProgramString src with
  | .error e => return (s!"throw new Error({String.quote e});", true)
  | .ok p    =>
    match Codegen.compileChecked p with
    | .error e => return (s!"throw new Error({String.quote s!"LeanJs check: {e}"});", true)
    | .ok js   => return (js, false)

/-- A `GameProvider` plays the same role as `Template.Provider` but
    for the LeanJs pipeline (read → parse → codegen). In prod mode
    it caches once; in dev mode every call re-reads + re-compiles. -/
abbrev GameProvider := IO (String × Bool)

def mkGameProvider (devMode : Bool) : IO GameProvider := do
  if devMode then
    let _ ← compileGame  -- smoke-test at startup
    return compileGame
  else
    let cached ← compileGame
    return pure cached

/-! ## Routes -/

def handler (pageProv : Template.Provider) (gameProv : GameProvider)
    : Handler := fun req => do
  match req.path with
  | "/" =>
    let (gameJs, isError) ← gameProv
    let page ← pageProv
    let banner :=
      if isError then "<pre style=\"color:#f87171\">compile error — see /game.js</pre>"
      else ""
    let body ← page.renderFlat [
      ("gameJs",       gameJs),
      ("errorBanner",  banner)
    ]
    return Response.html 200 body
  | "/game.js" =>
    let (gameJs, _) ← gameProv
    return Response.text 200 gameJs
  | "/favicon.ico" =>
    return { status := 204, headers := #[], body := .empty }
  | _ => return Response.notFound

private structure Args where
  port : UInt16 := 8005
  host : String := "0.0.0.0"
  dev  : Bool := false

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port" :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8005).toUInt16 }
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | "--dev"  :: rest      => parseArgs rest { a with dev := true }
  | _ :: rest             => parseArgs rest a
  | []                    => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  let pageProv ← Template.mkProvider "examples/Reversi/page.html" a.dev
  let gameProv ← mkGameProvider a.dev
  let modeNote := if a.dev then "  [DEV: hot reload]" else ""
  IO.println s!"reversi server: http://{a.host}:{a.port}/{modeNote}"
  serve a.port a.host (handler pageProv gameProv)

end ReversiServe

def main (args : List String) : IO Unit := ReversiServe.serveMain args
