import LeanTea
import LeanJs.Parser
import LeanJs.Codegen
import ChuHan.Game

/-! # chuhan_serve — 楚漢恋歌 SPA

Same shape as `reversi_serve`: load `Game.leanjs`, compile to JS via
LeanJs, splice into `page.html`. Six character routes, two-layer
dialogue (outer + inner monologue), action battle in `<canvas>`. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanJs

namespace ChuHanServe

def compileGame : IO (String × Bool) := do
  let src ← ChuHan.loadSource
  match Parser.parseProgramString src with
  | .error e => return (s!"throw new Error({String.quote e});", true)
  | .ok p    =>
    match Codegen.compileChecked p with
    | .error e => return (s!"throw new Error({String.quote s!"LeanJs check: {e}"});", true)
    | .ok js   => return (js, false)

abbrev GameProvider := IO (String × Bool)

def mkGameProvider (devMode : Bool) : IO GameProvider := do
  if devMode then
    let _ ← compileGame
    return compileGame
  else
    let cached ← compileGame
    return pure cached

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
      ("gameJs",      gameJs),
      ("errorBanner", banner)
    ]
    return Response.html 200 body
  | "/game.js" =>
    let (gameJs, _) ← gameProv
    return Response.text 200 gameJs
  | "/favicon.ico" =>
    return { status := 204, headers := #[], body := .empty }
  | _ => return Response.notFound

private structure Args where
  port : UInt16 := 8050
  host : String := "0.0.0.0"
  dev  : Bool := false

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port" :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8050).toUInt16 }
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | "--dev"  :: rest      => parseArgs rest { a with dev := true }
  | _ :: rest             => parseArgs rest a
  | []                    => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  let pageProv ← Template.mkProvider "examples/ChuHan/page.html" a.dev
  let gameProv ← mkGameProvider a.dev
  let modeNote := if a.dev then "  [DEV: hot reload]" else ""
  IO.println s!"chuhan server: http://{a.host}:{a.port}/{modeNote}"
  serve a.port a.host (handler pageProv gameProv)

end ChuHanServe

def main (args : List String) : IO Unit := ChuHanServe.serveMain args
