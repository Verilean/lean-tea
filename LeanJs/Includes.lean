import LeanJs.Ast
import LeanJs.Parser

/-! # LeanJs.Includes — resolve `include "path.leanjs"` at compile time

LeanJs's `include` directive splices another file's top-level defs
into the current program. This module walks the program, loads each
included file from disk, parses it, recursively resolves its own
includes, and returns a flat `Program` with no `includeE` nodes left
for Codegen / Eval to worry about.

The traversal is iterative-with-stack:

  * `seen` — set of absolute paths already pulled in, prevents
    infinite recursion on cycles. Re-including the same path is a
    silent no-op (most-recent definition wins isn't a concern since
    we drop the repeat entirely).
  * `baseDir` — directory of the file we're currently parsing.
    Include paths are first tried as-is, then relative to `baseDir`,
    matching how Reversi (and other apps) lookup `.leanjs` files.

Errors loud — a missing include throws `IO.userError`, not a silent
skip, since silently swallowing it would mask wiring bugs. -/

namespace LeanJs.Includes

open LeanJs.Ast

/-- Search list for an include path. Tries the path verbatim first
    (so absolute paths or already-correct relatives work), then
    relative to the directory of the including file, then up the
    usual `lean-elm/` / `../` ladder that mirrors `Template.loadFile`
    and the various game loaders. -/
private def candidates (baseDir : String) (rel : String) : List String :=
  let baseJoin : String := if baseDir.isEmpty then rel else baseDir ++ "/" ++ rel
  [rel, baseJoin, "examples/" ++ rel, "../examples/" ++ rel,
   "../../examples/" ++ rel, "lean-elm/examples/" ++ rel]

/-- Pick the first existing path. Returns the absolute form so we can
    use it as the cycle-detection key. -/
private def resolvePath (baseDir : String) (rel : String) : IO String := do
  for c in candidates baseDir rel do
    if ← System.FilePath.pathExists c then
      let abs ← IO.FS.realPath c
      return abs.toString
  throw <| IO.userError <|
    s!"include: couldn't locate `{rel}` — tried: " ++
    String.intercalate ", " (candidates baseDir rel)

/-- Strip the file portion off a path, leaving its parent directory.
    Used so nested includes resolve relative to *their* file, not the
    top-level entrypoint. -/
private def dirnameOf (path : String) : String :=
  let parts := path.splitOn "/"
  match parts.reverse with
  | _ :: rev => String.intercalate "/" rev.reverse
  | []       => ""

/-- Resolve every `includeE` in `prog`. New includes encountered
    recursively are looked up relative to the file they appeared in.
    Returns a `Program` whose `includeE` nodes are all gone. -/
partial def resolveWith (seen : List String) (baseDir : String)
    (prog : Program) : IO Program := do
  let mut out : Array TopDef := #[]
  let mut seen' := seen
  for td in prog do
    match td with
    | .includeE rel =>
      let abs ← resolvePath baseDir rel
      if seen'.contains abs then
        -- already pulled in — skip silently to break cycles
        continue
      seen' := abs :: seen'
      let src ← IO.FS.readFile abs
      match Parser.parseProgramString src with
      | .error e =>
        throw <| IO.userError s!"include: parse error in `{rel}`: {e}"
      | .ok subProg =>
        let resolved ← resolveWith seen' (dirnameOf abs) subProg
        out := out ++ resolved
    | other =>
      out := out.push other
  return out

/-- Top-level entry. `entryPath` is the on-disk path of the file the
    program came from (used to anchor relative include lookups);
    pass `""` if the program was synthesized from a string. -/
def resolve (entryPath : String) (prog : Program) : IO Program := do
  let baseDir :=
    if entryPath.isEmpty then "" else dirnameOf entryPath
  -- Seed `seen` with the entry path so a self-include is a no-op
  -- rather than infinite recursion.
  let seed ← (do
    if entryPath.isEmpty then return []
    else if ← System.FilePath.pathExists entryPath then
      let abs ← IO.FS.realPath entryPath
      return [abs.toString]
    else return [entryPath])
  resolveWith seed baseDir prog

end LeanJs.Includes
