import LeanJs.Parser
import LeanJs.Codegen

/-! # examples/Breezehollow/Game.lean — load + compile `World.leanjs`

The whole client lives in `World.leanjs` next to this file. This
module finds it (from the repo root or a subdirectory), parses it
with `LeanJs.Parser`, runs the arity / record checks and compiles it
to an ES module with `LeanJs.Codegen`. A parse or check failure comes
back as a module that throws, so the page shows the error instead of
a blank canvas. Shared by `breezehollow` and `breezehollow_spec`. -/

namespace Breezehollow

private def candidates : List String := [
  "examples/Breezehollow/World.leanjs",
  "../examples/Breezehollow/World.leanjs",
  "../../examples/Breezehollow/World.leanjs",
  "lean-tea/examples/Breezehollow/World.leanjs"
]

def loadSource : IO String := do
  for path in candidates do
    if ← System.FilePath.pathExists path then
      return ← IO.FS.readFile path
  throw <| IO.userError <|
    "couldn't locate Breezehollow/World.leanjs — tried: " ++ String.intercalate ", " candidates

/-- Compile LeanJs source to JS. `.error` carries the parse / check message. -/
def compileSource (src : String) : Except String String :=
  match LeanJs.Parser.parseProgramString src with
  | .error e => .error s!"LeanJs parse: {e}"
  | .ok p =>
    match LeanJs.Codegen.compileChecked p with
    | .error e => .error s!"LeanJs check: {e}"
    | .ok js => .ok js

/-- The served module: compiled JS, or one that throws the error. -/
def compileWorld : IO (String × Bool) := do
  match compileSource (← loadSource) with
  | .ok js => return (js, false)
  | .error e => return (s!"throw new Error({String.quote e});", true)

end Breezehollow
