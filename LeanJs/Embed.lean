import Lean
import LeanJs.Parser
import LeanJs.Includes
import LeanJs.Codegen

/-! # Compile a `.leanjs` file when the Lean code is built

```lean
def clientJs : String := leanjs_file% "../web/client.leanjs"
```

reads the file (relative to the Lean file, like `include_str`), resolves
its `include`s, runs the parser and `LeanJs.Check` (arity, record fields,
unknown names, use before definition) and becomes the JavaScript as a
string literal. A mistake is a build error with its line and column, not
a page that throws when it loads, and the server does no compiling at
startup.

Lake does not see the file: list it as an `input_file` in the `needs` of
the library, so editing it rebuilds:

```lean
input_file clientLeanjs where
  path := "web/client.leanjs"
  text := true

lean_lib App where
  needs := #[clientLeanjs]
```

Use the run-time path (`Parser.parseProgramString` and
`Codegen.compileChecked`, as `examples/Breezehollow` does) when the file
should be edited while the server runs. -/

namespace LeanJs.Embed

open Lean Elab Term

/-- Parse, resolve `include`s, check and compile a `.leanjs` file. -/
def compileFile (file : System.FilePath) : IO (Except String String) := do
  let src ← IO.FS.readFile file
  match Parser.parseProgramString src with
  | .error e => return .error s!"LeanJs ({file}): {e}"
  | .ok prog =>
    match ← (Includes.resolve file.toString prog).toBaseIO with
    | .error e => return .error s!"LeanJs ({file}): {e}"
    | .ok prog =>
      return (Codegen.compileChecked prog).mapError fun e => s!"LeanJs ({file}): {e}"

/-- `leanjs_file% "path.leanjs"`: the file compiled to JavaScript, at build time. -/
elab "leanjs_file% " path:str : term => do
  let some dir := (System.FilePath.mk (← getFileName)).parent
    | throwError "leanjs_file%: no directory for {← getFileName}"
  let file := (dir / path.getString).normalize
  match ← (compileFile file).toBaseIO with
  | .error e => throwError "leanjs_file%: cannot read {file}: {toString e}"
  | .ok (.error e) => throwError e
  | .ok (.ok js) => return mkStrLit js

end LeanJs.Embed
