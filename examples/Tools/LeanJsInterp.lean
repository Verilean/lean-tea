import LeanJs.Parser
import LeanJs.Check
import LeanJs.Eval
import LeanJs.Includes

/-! # leanjs_interp — pure-Lean interpreter for `.leanjs`

Evaluates the program *inside Lean* via `LeanJs.Eval.runProgram`,
then renders `main`'s value to stdout. No node, no browser, no
external runtime.

```
$ lake exe leanjs_interp hello.leanjs
"Hello, world!"
```

The renderer uses `LeanJs.Eval.render`, so strings come back quoted
(JSON-ish). This matches the spec runner's "lean side" of the
cross-check and lets you sanity-check pure code without touching JS.

**Subset:** the Lean evaluator covers the pure functional core —
arithmetic, `let`, `if`, `fun`, top-level `def`, top-level recursion
(self- and mutual-), sum types via `inductive` / `match`, arrays,
objects, dot / idx access, records, `class` / `instance`. It does
**not** support:

  * `extern js` — there is no JS runtime here. Any program that
    calls an FFI extern fails with `cannot invoke foreign value`.
  * `async def` / `await` — `await` is a no-op (returns the value
    as-is). Promises live in JS land.

For full coverage use `leanjs_run` (compile + node).

Exit codes:
  0  success
  1  parse error
  2  check error
  3  eval error / missing `main`
-/

def main (args : List String) : IO UInt32 := do
  let path :=
    match args with
    | [p] => p
    | _   => ""
  if path == "" then
    IO.eprintln "usage: leanjs_interp <file.leanjs>"
    return 3
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
      match LeanJs.Eval.runProgram prog with
      | .error e =>
        IO.eprintln s!"eval: {e}"
        return 3
      | .ok v =>
        IO.println (LeanJs.Eval.render v)
        return 0
