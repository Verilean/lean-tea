/-! # examples/ChuHan/Game.lean — loader for `Game.leanjs`

Mirrors the `Reversi/Game.lean` shape — just probes a list of
candidate paths and returns the source. The actual game logic
(scene runner, character select, Liu Bang Act 1, action battle)
lives in `Game.leanjs` next door. -/

namespace ChuHan

private def candidates : List String := [
  "examples/ChuHan/Game.leanjs",
  "../examples/ChuHan/Game.leanjs",
  "../../examples/ChuHan/Game.leanjs",
  "lean-elm/examples/ChuHan/Game.leanjs"
]

def loadSource : IO String := do
  for path in candidates do
    if ← System.FilePath.pathExists path then
      let src ← IO.FS.readFile path
      IO.eprintln s!"chuhan: loaded {src.utf8ByteSize} bytes from {path}"
      return src
  throw <| IO.userError <|
    "couldn't locate ChuHan/Game.leanjs — tried: "
    ++ String.intercalate ", " candidates

end ChuHan
