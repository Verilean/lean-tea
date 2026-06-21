/-! # examples/Reversi/Game.lean — loader for `Game.leanjs`

The game logic itself now lives in `Game.leanjs` next to this file,
so it can be edited with whatever LeanJs-subset tooling you have
without touching any Lean source. This module just resolves the
file path at startup, reads it, and hands the contents to
`LeanJs.Parser`.

`loadSource` probes a small list of candidate paths so the same
binary runs from either `lean-elm/` (the natural Lake build dir)
or one level up (the repo root). Mirrors any `*_serve`'s
`resolveDist` shape. -/

namespace Reversi

/-- File-system candidates we try in order so the binary works no
    matter what the user's `cwd` happens to be. -/
private def candidates : List String := [
  "examples/Reversi/Game.leanjs",
  "../examples/Reversi/Game.leanjs",
  "../../examples/Reversi/Game.leanjs",
  "lean-elm/examples/Reversi/Game.leanjs"
]

/-- Find and read the game source. Errors loudly if we can't locate
    the file — the server is useless without it. -/
def loadSource : IO String := do
  for path in candidates do
    if ← System.FilePath.pathExists path then
      let src ← IO.FS.readFile path
      IO.eprintln s!"reversi: loaded {src.utf8ByteSize} bytes from {path}"
      return src
  throw <| IO.userError <|
    "couldn't locate Reversi/Game.leanjs — tried: "
    ++ String.intercalate ", " candidates

end Reversi
