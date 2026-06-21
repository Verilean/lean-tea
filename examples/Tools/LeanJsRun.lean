import LeanJs.Parser
import LeanJs.Codegen
import LeanJs.Check
import LeanJs.Includes

/-! # leanjs_run — `.leanjs` → JS → `node`

`compile + node`. Full feature set: externs, async/await, FFI all
work because we hand the lowered JS to the real Node.js runtime.

```
$ lake exe leanjs_run examples/hello.leanjs
Hello, world!
```

For a pure-Lean evaluator (no node, but no FFI either) see
`leanjs_interp`. For "emit JS but don't run" see `leanjs_compile`.

Exit codes:
  0  success
  1  parse error
  2  check error
  N  node's own exit code
-/

def main (args : List String) : IO UInt32 := do
  let path :=
    match args with
    | [p] => p
    | _   => ""
  if path == "" then
    IO.eprintln "usage: leanjs_run <file.leanjs>"
    return 1
  let src ← IO.FS.readFile path
  match LeanJs.Parser.parseProgramString src with
  | .error e =>
    IO.eprintln s!"parse: {e}"
    return 1
  | .ok prog0 =>
    let prog ← LeanJs.Includes.resolve path prog0
    match LeanJs.Check.check prog with
    | .error e =>
      IO.eprintln s!"check: {e}"
      return 2
    | .ok _ =>
      let js := LeanJs.Codegen.compileToString prog
      let outPath :=
        if path.endsWith ".leanjs" then path.dropRight 7 ++ ".js"
        else path ++ ".js"
      IO.FS.writeFile outPath js
      let out ← IO.Process.output { cmd := "node", args := #[outPath] }
      IO.print out.stdout
      IO.eprint out.stderr
      return out.exitCode
