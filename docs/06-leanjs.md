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
- `match` with nested constructor patterns, literals, variables and `_`,
  checked for exhaustiveness (a gap is reported as the missing pattern)
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

## Worked example — Breezehollow (three.js)

`examples/Breezehollow/World.leanjs` is a whole 3D game client: an
anime-style open-world vignette with cel-shaded terrain, instanced
grass with a wind shader, floating islands, a wind shrine,
collectible motes, updrafts, a chibi hero who runs, jumps and glides,
bloom post-processing and a canvas minimap.

```text
import * as THREE from "three"
import { UnrealBloomPass } from "three/addons/postprocessing/UnrealBloomPass.js"

def toon(color) := new THREE.MeshToonMaterial({color: color, gradientMap: toonRamp})

def updatePlayer(dt) := do
  let kx := (if keys["d"] then 1 else 0) - (if keys["a"] then 1 else 0);
  …
  if P.gliding then (do
    P.vy <- Math.max(P.vy, -2.4);
    if inUpdraft(P.x, P.z, P.y) == 1 then (P.vy <- Math.min(P.vy + 42 * dt, 13)) else null) else null;
  …
```

`examples/Breezehollow/Serve.lean` compiles it once at startup
(`--dev` recompiles per request) and serves it as `/world.js` next to
a HUD page whose import map points `three` at a CDN:

```bash
$ lake exe breezehollow          # http://localhost:8010/
$ lake exe breezehollow_spec     # offline: compiles, node --check, page ids
```

The movement rules are a separate, pure file, `Physics.leanjs`:

```text
def step(p, inp, world, dt) :=
  let jumped := inp.jump && p.onGround;
  …
  { player: { p with x := at[0], z := at[1], vy := (if landed then 0 else vy), … },
    events: events }
```

`World.leanjs` `include`s it and is only the shell: it reads the keys,
calls `step`, stores the new player and turns the returned events
(`"jump"`, `"glide"`, `"skill"`, `"land"`) into particles. Because
`step` is a plain function, `PhysicsTest.leanjs` checks jump height,
glide sink rate, stamina, updrafts and the wind skill on toy worlds
under node, and `breezehollow_spec` turns each result into a test.

Shader source fits in ordinary string literals, which may span lines.

## Errors and checks

Parse errors give the line, column, the source line and a caret at
the deepest point the parser reached. `LeanJs.Check` names the
enclosing `def` and catches wrong arity, bad record fields, unknown
identifiers (suggesting the closest name) and typos in the fields of
top-level object literals — see the LeanJs README for the details.

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
