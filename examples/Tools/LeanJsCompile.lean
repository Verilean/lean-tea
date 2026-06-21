import LeanJs.Parser
import LeanJs.Codegen
import LeanJs.Check
import LeanJs.Includes

/-! # leanjs_compile — `.leanjs` → `.js` (emit only, no run)

The "compiler" CLI. Mirrors `gcc -S` / `tsc`: parse + check + lower
to JavaScript, write the result to stdout (default) or to a file
with `-o`.

```
$ lake exe leanjs_compile hello.leanjs
const greet = (name) => sconcat(...);
const main = greet('world');
console.log(main);

$ lake exe leanjs_compile hello.leanjs -o hello.js
$ node hello.js
Hello, world!
```

Exit codes:
  0  success
  1  parse error
  2  check error
  3  bad args / IO
-/

structure Args where
  inPath  : String := ""
  outPath : Option String := none

partial def parseArgs : List String → Except String Args
  | [] => .ok {}
  | "-o" :: v :: rest => do
    let a ← parseArgs rest
    return { a with outPath := some v }
  | p :: rest => do
    let a ← parseArgs rest
    if a.inPath == "" then return { a with inPath := p }
    else .error s!"unexpected extra argument: {p}"

def main (args : List String) : IO UInt32 := do
  match parseArgs args with
  | .error e =>
    IO.eprintln s!"args: {e}"
    IO.eprintln "usage: leanjs_compile <file.leanjs> [-o out.js]"
    return 3
  | .ok a =>
    if a.inPath == "" then
      IO.eprintln "usage: leanjs_compile <file.leanjs> [-o out.js]"
      return 3
    let src ← IO.FS.readFile a.inPath
    match LeanJs.Parser.parseProgramString src with
    | .error e =>
      IO.eprintln s!"parse: {e}"
      return 1
    | .ok prog0 =>
      let prog ← LeanJs.Includes.resolve a.inPath prog0
      match LeanJs.Check.check prog with
      | .error e =>
        IO.eprintln s!"check: {e}"
        return 2
      | .ok _ =>
        let js := LeanJs.Codegen.compileToString prog
        match a.outPath with
        | some out => IO.FS.writeFile out js
        | none     => IO.print js
        return 0
