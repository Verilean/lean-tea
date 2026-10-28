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
collectible motes, updrafts, a heroine who runs, jumps, glides, flies
and fights monsters with wind, bloom post-processing and a canvas
minimap.

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

On a slow GPU (or software rendering) open `http://localhost:8010/?low`:
less grass, no shadows and no bloom.

The movement rules are a separate, pure file, `Physics.leanjs`:

```text
def step(p : Player, inp : Input, world : Terrain, dt : Float) : Moved :=
  let jumped := inp.jump && p.onGround;
  …
  Moved { player: { p with x := at[0], z := at[1], vy := (if landed then 0 else vy), … },
          events: events }
```

Fighting follows the same pattern in `Combat.leanjs`:

```text
inductive Mode where
  | Idle | Return | Chase | Windup | Strike | Recover | Hurt | Dead

def fight(s : Battle, p : Player, inp : Attack, moves : Array String,
          world : Terrain, dt : Float) : Outcome := do
  …
  Outcome { state: Battle {hero: h, foes: moved, bolts: flying}, events: evs,
            knock: knock, face: face,
            rooted: h.down > 0 || h.swing >= 0 && h.swing < COMBO[h.combo].dur * 0.8 }
```

The heroine's HP and combo, the monsters (slimes, wisps and golems that
wander, notice her, wind up and strike) and her wind bolts are one state
record. Each frame `fight` takes it with the player `step` just moved
and the events it returned (the wind skill hurts monsters too), and
gives back the next state, events for the renderer (an `inductive`:
`Swung(…)`, `Damaged(…)`, `Defeated(…)`, `Wounded(…)` …, which
`World.leanjs` takes apart with `match`), a knock to add to her
velocity and whether she is rooted mid-swing. A monster's kind
(`Slime | Wisp | Golem`) and what it is doing (`Mode`) are inductives
too, so a misspelt state is a compile error rather than a comparison
that is silently never true.

Both rule files are typed throughout, and `breezehollow_spec` has Lean
check them (see *Types through Lean* below). `World.leanjs` builds the monsters from
primitives and animates them from their state. `CombatTest.leanjs`
checks the combo, monster AI, guard time, defeat and respawn under
node, as `PhysicsTest.leanjs` does for movement.

`World.leanjs` `include`s both and is only the shell: it reads the keys,
calls `step`, stores the new player and turns the returned events
(`"jump"`, `"glide"`, `"fly"`, `"skill"`, `"land"`) into particles. Because
`step` is a plain function, `PhysicsTest.leanjs` checks jump height,
glide sink rate, stamina, updrafts, the wind skill and flight on toy
worlds under node, and `breezehollow_spec` turns each result into a test.

The heroine is a glTF model, `examples/Breezehollow/heroine.glb`, put
together in Blender from two freely licensed sample avatars: the body,
face and dress of one and the long hair of the other, recoloured toward
her character sheet. `tools/heroine-vrm/` has the scripts, where the
avatars come from and their licences:

- **`build.py`** combines the two avatars and recolours their textures
  (silver hair, violet eyes, a white and violet dress).
- **`rig.py`** gives her the skeleton and clips of
  `tools/blender/heroine.py`, so the bone names, spring chains and the
  clips `idle`, `run`, `glide`, `hover`, `fly`, `fall`, `attack_1`–`3`
  and `hurt` carry over.
  Each vertex keeps the avatar's weights, renamed to the game's bones;
  the avatar's hair and skirt weights go to the hair and coat chains.

```bash
$ blender -b --factory-startup --python tools/heroine-vrm/build.py -- \
    body.vrm hair.vrm /tmp/heroine.blend
$ blender -b --factory-startup --python tools/heroine-vrm/rig.py -- \
    /tmp/heroine.blend examples/Breezehollow/heroine.glb --preview /tmp/heroine.png
```

Run on its own, `tools/blender/heroine.py` still builds a complete
heroine from code (body, painted face, hair, coat, rig and clips), the
earlier version.

The game loads her with `GLTFLoader` and swaps each material for a toon
material with a rim light, plus an outline (an inverted hull grown along
the unskinned normal, so it follows the skin), tinted and thinner on
skin and hair. Materials are told apart by name: eyes, brows and lashes
are drawn flat with no outline, and see-through cloth gets none either. It cross-fades the clips by state.
Every frame it swings the hair and coat chains with a verlet spring per
bone, which keeps each bone outside a few spheres on her body.
`breezehollow_spec` checks that the `.glb` has every clip, chain and
collider bone that `World.leanjs` names.

Shader source fits in ordinary string literals, which may span lines.

## Errors and checks

Parse errors give the line, column, the source line and a caret at
the deepest point the parser reached. `LeanJs.Check` names the
enclosing `def` and catches wrong arity, bad record fields, unknown
identifiers (suggesting the closest name), typos in the fields of
top-level object literals and of parameters with a record type
(`f : Foe`), and a value used before its definition (which JavaScript
only reports when the page loads) — see the LeanJs README for the
details.

## Types through Lean

LeanJs has no type checker of its own, and doesn't need one: for pure
rules, `LeanJs.Typed` translates the program to Lean and lets Lean's
elaborator check it. Records become structures, inductives (with typed
constructor fields) become inductives, `do` blocks become `Id.run do`
with the same `let mut` / `for` / `if` / `match` statements, and arrays
become `Array`s.

```text
$ lake exe leanjs_compile rules.leanjs --typed -o rules.js
typed: in `walkStep`, …/typed.lean:226:187: error: Type mismatch
    | …onGround := (landed || (ny - g)), gliding := …
  ny - g
has type
  Float
but is expected to have type
  Bool
```

Parameters and integer-valued constants need their types
(`def HERO_HP : Float := 100`); everything else is inferred. What has
no type is refused instead of guessed at: `null` (write an inductive
with an empty case, like `Keep | Face(angle : Float)`), `extern js`,
assignment to fields. So the check fits the pure rule files
(`Physics.leanjs`, `Combat.leanjs`), not the three.js shell.
`--lean` prints the Lean source being checked.

## CLI trio

Three executables share the LeanJs frontend:

- **`leanjs_compile file.leanjs [--typed] [-o out.js]`** — pure compiler
  (parse + check + codegen). No execution. `--typed` also has Lean
  check the types.
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
