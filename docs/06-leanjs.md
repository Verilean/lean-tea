# 6 · LeanJs — a Lean-shaped subset that compiles to JS

The framework has *two* ways to produce browser JavaScript:

1. **`.leanjs` files** (this chapter) — Lean-shaped source compiled at
   server startup. Best for anything that touches the DOM or third-party
   libraries directly, anything you'd rather edit like a JS file with
   `--` comments.
2. **The typed `LeanTea.Js` DSL** — JS AST built inside Lean. Best
   when generated code embeds Lean values (`ToJsExpr`).

Both lower through the same renderer (`LeanTea.Js.render`). You can
mix them in one app. CounterWeb is pure Elm-style (no JS); Sheet
mixes typed DSL + a `.leanjs` runtime; Reversi ships a single
`.leanjs` file that's the whole client.

Detailed reference: [`LeanJs/README.md`](../LeanJs/README.md).
Side-by-side with plain JS and with Lean: [`LeanJs/COMPARISON.md`](../LeanJs/COMPARISON.md).

## Smallest example

`hello.leanjs`:

```text
def double(x) := x * 2
def main := double(21)
```

Compiled to JS:

```js
const double = (x) => { return (x * 2); };
const main   = double(21);
console.log(main);
```

…and to Lean (`lean --run`):

```lean
partial def double (x : Int) : Int := x * 2
def main : IO Unit := IO.println (double 21)
```

`lake exe leanjs_spec` runs both. Same source, two runtimes, asserted
byte-equal stdout.

## What .leanjs gives you over hand-written JS

- `--` line comments
- `match` over inductive constructors *and* over string literals (with
  `_` wildcard fall-through)
- `record Name where field : Type, …` plus `Name { field: value, … }`
  named-args constructor — `LeanJs.Check` verifies the field set so
  typos surface at compile time
- Compile-time **arity check** on every direct call: extern FFI
  declarations carry their own arity, and a mismatched call site is a
  build error
- `obj.field <- value` / `arr[i] <- value` assignment (the imperative
  extension)
- `new C(a, b)`, `obj?.field`, hex / float literals, `import`s
- Parser errors that include line, column, and offset

## Worked example — Reversi

`examples/Reversi/Game.leanjs` is the entire game client in ~115 lines:

```text
-- 4 FFI bindings for JS primitives we need
extern js "(n) => Array(n).fill(0)" mkArray
extern js "(arr, i, v) => arr.map((x, j) => j === i ? v : x)" arrSet
extern js "(arr, i) => arr[i]" arrGet
extern js "(a, b) => '' + a + b" sconcat

-- 0 = empty, 1 = black, 2 = white
def at(b, x, y) := arrGet(b, y * 8 + x)
def opp(c) := if c == 1 then 2 else 1

-- … walk + flip helpers …

def update(msg, model) :=
  let idx := msg;
  let y := idx / 8;
  let x := idx - y * 8;
  if isValid(model.board, x, y, model.player) == 1 then
    { board: makeMove(model.board, x, y, model.player),
      player: opp(model.player) }
  else model

def view(model) := …
def main := score(initBoard, 1)
```

The whole game — board representation, move validation, flipping,
scoring, view rendering — is in the subset. The Lean side parses,
compiles, and serves the JS at startup (see
`examples/Reversi/Serve.lean`).

## CLI trio

Three executables share the LeanJs frontend:

- **`leanjs_compile file.leanjs [-o out.js]`** — pure compiler
  (parse + check + codegen). No execution.
- **`leanjs_interp file.leanjs`** — pure-Lean evaluator via
  `LeanJs.Eval`. No node, no FFI.
- **`leanjs_run file.leanjs`** — compile + execute via `node`.
  Full feature set including externs / async / await.

Useful for testing and for one-off scripts that share logic with a
browser build.

## Record example

The named-args literal + check pass is the cleanest typo-killer the
subset has:

```text
record Card where
  suit : String,
  rank : Int,
  face : String

def royalFlush := [
  Card { suit: "♠", rank: 14, face: "A" },
  Card { suit: "♠", rank: 13, face: "K" },
  Card { suit: "♠", rank: 12, face: "Q" }
]
```

Misspell a field (`fcae:` instead of `face:`) and the served `.js`
is `throw new Error("Check: 'Card': unknown field 'fcae'");` — the
browser never sees a broken build.

## When to *not* use .leanjs

- **You need to embed Lean values** at compile time. Use the typed
  `LeanTea.Js` DSL with `ToJsExpr` so refactoring a Lean record
  updates the generated client automatically.
- **The code is interactive across server requests** (handlers,
  rendering). That's the Elm-style frontend (Chapter 2), not a JS
  blob.
- **You want the editor to fully understand the file**. `.leanjs`
  isn't a registered language — most "JS-ish" highlighters handle it
  but you don't get IDE jump-to-def into Lean from the file.

Everything else: write a `.leanjs`, load it from `Serve.lean`, ship.
