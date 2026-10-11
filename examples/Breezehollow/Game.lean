import LeanJs.Parser
import LeanJs.Includes
import LeanJs.Codegen

/-! # examples/Breezehollow/Game.lean — load + compile `World.leanjs`

The client lives in `World.leanjs` (scene, input, HUD), which
`include`s `Physics.leanjs` (the pure movement rules). This module
finds the entry file (from the repo root or a subdirectory), parses
it, resolves includes, runs the LeanJs checks (arity, records, unknown
identifiers, field typos) and compiles it to an ES module. A failure
comes back as a module that throws, so the page shows the error
instead of a blank canvas. Shared by `breezehollow` and
`breezehollow_spec`. -/

namespace Breezehollow

private def candidates (file : String) : List String := [
  s!"examples/Breezehollow/{file}",
  s!"../examples/Breezehollow/{file}",
  s!"../../examples/Breezehollow/{file}",
  s!"lean-tea/examples/Breezehollow/{file}"
]

/-- Path of one of the demo's source files. -/
def findFile (file : String) : IO String := do
  for path in candidates file do
    if ← System.FilePath.pathExists path then return path
  throw <| IO.userError <|
    s!"couldn't locate Breezehollow/{file} — tried: " ++ String.intercalate ", " (candidates file)

/-- Parse `path`, resolve its includes, check, compile. `.error` carries
    the parse / include / check message. -/
def compileFile (path : String) : IO (Except String String) := do
  match LeanJs.Parser.parseProgramString (← IO.FS.readFile path) with
  | .error e => return .error s!"LeanJs parse ({path}): {e}"
  | .ok p =>
    let resolved ← (LeanJs.Includes.resolve path p).toBaseIO
    match resolved with
    | .error e => return .error s!"LeanJs {e}"
    | .ok p =>
      match LeanJs.Codegen.compileChecked p with
      | .error e => return .error s!"LeanJs check: {e}"
      | .ok js => return .ok js

/-- The served module: compiled JS, or one that throws the error. -/
def compileWorld : IO (String × Bool) := do
  match ← compileFile (← findFile "World.leanjs") with
  | .ok js => return (js, false)
  | .error e => return (s!"throw new Error({String.quote e});", true)

end Breezehollow
