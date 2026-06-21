# LeanJs vs JavaScript vs Lean — a comparison

LeanJs is *not* JS-in-Lean clothing, and it's *not* Lean either.
It sits in a deliberate middle: most of Lean's surface ergonomics
(let-bindings, pattern matching, no `var`, expression-first), most
of JS's runtime model (mutation, async, ES modules, prototype
access). This page lays out where each differs so you can read a
`.leanjs` file without surprise.

For the formal language reference, see [`LeanJs/README.md`](README.md).
For the architecture context, see [`docs/10-architecture.md`](../docs/10-architecture.md).

---

## LeanJs ↔ JavaScript

These are the things that change when you take a JS program and
rewrite it as `.leanjs`. The right column is the emitted JavaScript,
which is what actually runs.

### Lexical / literals

| LeanJs | JavaScript | Notes |
|---|---|---|
| `42` `-7` | `42` `-7` | integers |
| `0xff` | `255` | hex literal is parsed; emitted as decimal |
| `3.14` `-0.5` | `3.14` `-0.5` | float literal text copied verbatim — no round-trip rounding |
| `"hello"` | `"hello"` | only double-quoted strings; no template literals |
| `true` `false` `null` | `true` `false` `null` | |
| `[1, 2, 3]` | `[1, 2, 3]` | array literal |
| `{x: 1, y: 2}` | `{x: 1, y: 2}` | **unquoted keys only**; no computed `[k]: v` |
| `-- line comment` | (none) | `//` is not a comment in LeanJs |

### Access and call

| LeanJs | JavaScript | Notes |
|---|---|---|
| `obj.field` | `obj.field` | dot access |
| `obj?.field` | `obj?.field` | optional chaining (single hop only) |
| `arr[i]` | `arr[i]` | indexing |
| `f(a, b)` | `f(a, b)` | call |
| `new C(a, b)` | `new C(a, b)` | constructor |
| `obj.method(a)` | `obj.method(a)` | works as dot + call combination |

### Operators

| LeanJs | JavaScript | Notes |
|---|---|---|
| `a + b` | `(a + b)` | always parenthesised by emitter |
| `a == b` | `(a == b)` | **loose equality** — no `===` form |
| `a != b` | `(a != b)` | |
| `a && b` `\|\|` | `(a && b)` | logical |
| `!cond` | `(!cond)` | unary not |
| `0 - x` | `(0 - x)` | unary minus on a non-literal — use the binary form |
| (no ternary) | `cond ? a : b` | use `if cond then a else b` instead |
| `lhs <- value` | `(lhs = value)` | **assignment uses `<-`**, not `=`; emits as a JS assignment expression |

### Statements and control flow

| LeanJs | JavaScript | Notes |
|---|---|---|
| `if c then t else e` | `(c ? t : e)` | **expression**, never a statement; both branches required |
| `let x := v; rest` | `((x) => rest)(v)` | `let` is also expression-shaped, via an IIFE |
| `let _ := side_effect; rest` | (side effect then `rest`) | the idiomatic "do this for effect" pattern; `_` discards the value |
| (no `while` / `for`) | `while` / `for` | use tail-recursive helper functions |
| (no `let mut`) | mutable bindings | use object-field mutation (`obj.x <- v`) or a small `Ref` extern |
| `def f() := body` | `const f = (() => { return body; });` | zero-arity *function* |
| `def f := body` | `const f = body;` | zero-arity *value* (no parens) |

### Functions and modules

| LeanJs | JavaScript | Notes |
|---|---|---|
| `def f(x, y) := body` | `const f = (x, y) => { return body; };` | sync |
| `async def f(x) := body` | `const f = async (x) => { return body; };` | async |
| `fun (x) => body` | `((x) => { return body; })` | inline arrow |
| `await e` | `await e` | only inside `async def` |
| `import * as X from "m"` | `import * as X from 'm';` | namespace import |
| `import { a, b as c } from "m"` | `import { a, b as c } from 'm';` | named imports |
| `extern js "raw" name` | `const name = raw;` | the FFI escape hatch |

### Sum types / pattern matching (no JS equivalent)

| LeanJs | JavaScript shape it lowers to |
|---|---|
| `inductive Color where \| RGB(r,g,b) \| None` | `const RGB = (r,g,b) => ({tag:"RGB",$0:r,$1:g,$2:b}); const None = {tag:"None"};` |
| `match c with \| RGB(r,_,_) => r \| None => 0` | `((__m) => __m.tag === "RGB" ? ((r,...) => r)(__m.$0, …) : __m.tag === "None" ? 0 : null)(c)` |

JS has no tagged unions natively. LeanJs gives you a small
runtime convention so `match` can dispatch and bind fields.

### Things JavaScript has, LeanJs doesn't

| JS feature | What to do instead |
|---|---|
| `var` / `let mut` | mutate object fields (`obj.x <- v`); for globals use a small mutable record + helpers |
| `for` / `while` | tail-recursive helpers |
| `===` strict equality | LeanJs only emits `==`; compare type explicitly if needed |
| ternary `?:` | `if c then a else b` |
| template literals | string concatenation: `"hello " + name` |
| destructuring `{a, b} = obj` | name each one: `let a := obj.a; let b := obj.b; …` |
| spread `...args` | wrap an FFI: `extern js "(...xs) => f(...xs)" fSpread` |
| `try/catch` | not in the subset; let JS propagate, handle in the host |
| classes with `this` | `inductive` + plain functions, or an `extern js` shim |

### Things LeanJs has, JavaScript doesn't

| LeanJs feature | Why it's useful |
|---|---|
| `--` line comments | `//` would collide with JS comments inside FFI strings |
| `inductive` + `match` | tagged unions without manual `.tag` boilerplate |
| Compile-time **arity check** (`LeanJs.Check`) | mismatched argument counts caught before the browser sees the file |
| Two surface flavours (`Parser` vs `JsParser`) | pick Lean-shaped or JS-shaped syntax; both target the same AST |
| Bilingual emit (`LeanEmit`) | the pure subset cross-checks against `lean --run` |
| `extern js "…" name` with arity inference | the FFI source itself drives the arity guard; no separate signature file |

---

## LeanJs ↔ Lean

LeanJs surface syntax is Lean-flavoured on purpose so the lines blur,
but they are *not* the same language. Here's where reading a
`.leanjs` snippet trips up an experienced Lean user.

### Different from Lean (looks like, isn't)

| Construct | LeanJs meaning | Real Lean meaning |
|---|---|---|
| `def f (x : Int) : Int := body` | type annotations are advisory; not type-checked | full elaboration, can't lie |
| `let x := v; body` | IIFE; `body` is one expression | `do`-style `let` (in `do` blocks) or `let … in …` (term mode) |
| `match e with \| C a => …` | dispatch on `e.tag`; binds `e.$0`, `e.$1` | exhaustive match against a real inductive |
| `obj.field` | JS property access | structure-field projection (typed) |
| `arr[i]` | runtime `[]` lookup | only on `GetElem` instances (`!` for total / `[]?` for `Option`) |
| `f(a, b)` | curried call, no implicit conversions | `f (a) (b)` — application is whitespace-separated, not comma |
| `inductive Color where \| RGB(r,g,b)` | constructor has arity 3, fields untyped | each field needs a type |
| `null` | JS null sentinel | not built-in; use `Option.none` |
| `true` `false` | JS booleans | also `Bool` literals, but not interchangeable with `Decidable.isTrue` etc. |
| `\|\|` `&&` | JS short-circuit (returns last truthy/falsy operand) | `Bool` operators, return `Bool` |
| `==` | JS loose equality | `BEq.beq` (typed) — different types don't compare |

### Doesn't exist in LeanJs

* Implicit arguments (`{α : Type}`), instance arguments (`[BEq α]`),
  metavariables — type inference doesn't happen.
* `do` notation with `<-` binders. In LeanJs `<-` is the **assignment
  operator**, not a monadic bind.
* Tactics, `theorem`, `example`, `#check`. LeanJs has no proof obligations.
* `Type`, `Sort`, universes. The language is monomorphic and runtime-only.
* `partial`, `mutual`, termination checking. JS recurses freely.
* `structure` (LeanJs uses `inductive` with a single constructor or
  an object literal `{a: 1, b: 2}` instead).

### Doesn't exist in Lean (so LeanEmit refuses)

The bilingual pipeline emits Lean source for the *pure* subset.
These constructs are rejected by `LeanJs.LeanEmit` and tag the
program "JS-only" in the spec runner:

* `extern js "…" name` — opaque JS
* `new C(args)` — JS constructor invocation
* `import` declarations
* `obj.field <- value` and `arr[i] <- value` assignment
* `obj?.field` optional chaining
* `null`
* Top-level expression statements (boot calls)

Programs that touch any of these still compile to JS just fine.
They just don't round-trip through `lean --run` for cross-check.

### Sample translation, all three sides

A small sum-over-array program:

```
-- LeanJs (.leanjs)
def sumLoop(i, acc) :=
  if i == 0 then acc
  else sumLoop(i - 1, acc + i)

def main := sumLoop(10, 0)
```

What `LeanJs.Codegen` emits:

```js
// JavaScript
const sumLoop = (i, acc) => { return ((i == 0) ? acc : sumLoop((i - 1), (acc + i))); };
const main    = sumLoop(10, 0);
console.log(main);
```

What `LeanJs.LeanEmit` emits:

```lean
-- Lean (run via `lean --run`)
partial def sumLoop (i : Int) (acc : Int) : Int :=
  if i == 0 then acc else sumLoop (i - 1) (acc + i)
def main : IO Unit := IO.println (sumLoop 10 0)
```

`lake exe leanjs_spec` runs both, asserts both print `55`. That's
the bilingual contract for the pure subset.

---

## When to pick which path

| If you need to … | Use |
|---|---|
| Embed Lean values into JS at server-render time | typed `LeanTea.Js` DSL (Chapter 5) |
| Generate JS conditionally from Lean state | typed DSL |
| Write a self-contained client program that doesn't need Lean values | LeanJs (`.leanjs`) |
| Express tagged-union logic on the client | LeanJs |
| Cross-check a numerical routine against `lean --run` | LeanJs pure subset |
| Mutate Three.js objects in a game loop | LeanJs imperative subset (imperative game) |
| Stay 100% on the Lean side | typed DSL — `.leanjs` files don't elaborate in Lean |

Both paths land in the same `LeanTea.Js.Expr` / `Stmt` and render
through the same code, so it's safe to mix them inside one app.
Canvas does (template HTML + typed JS client + a string-embedded
VRM bootstrap that's overdue for a port); the english app stays
fully on the typed DSL because it interleaves Lean values
(`speakLine(eg, 0.92)`) deeply with the page.
