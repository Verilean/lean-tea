# LeanJs — a Lean-shaped subset that compiles to JavaScript

LeanJs is a small expression language with Lean-style surface syntax
that compiles to plain JavaScript (no runtime required) and — for the
pure, FFI-free fragment — *also* runs through the real Lean compiler
via `lean --run`. Two backends, one source.

```
        ┌── LeanJs.Parser ──► Ast ──► Codegen ──► JS ──► node
source ─┤
        └── LeanJs.Parser ──► Ast ──► LeanEmit ─► Lean ─► lean --run
```

This README is the language reference. For the design story and the
"why parser combinators" rationale, see `docs/08-fay-style.md`. For a
full worked program, see `docs/09-reversi.md` and
`examples/Reversi/Game.leanjs`.

---

## At a glance

```text
-- examples/Hello.leanjs
def double(x) := x * 2
def main := double(21)        -- → 42
```

```bash
$ lake exe leanjs_spec        # runs every pure case through node + lean --run
$ lake exe reversi_serve      # loads + compiles a real .leanjs at startup
```

Files in this directory:

| File             | Role                                                  |
|------------------|-------------------------------------------------------|
| `Ast.lean`       | Abstract syntax — `Expr`, `TopDef`, `Program`         |
| `Parser.lean`    | Lean-shaped surface → `Ast` (parser combinators)      |
| `JsParser.lean`  | JS-shaped surface → `Ast` (same target, different skin)|
| `Codegen.lean`   | `Ast` → JS (via `LeanTea.Js.Expr` so it composes)     |
| `Eval.lean`      | Tree-walking interpreter (used for cross-checks)      |
| `LeanEmit.lean`  | `Ast` → real Lean source for `lean --run`             |

---

## Grammar

```text
Program ::= TopDef*

TopDef  ::= 'def' ident ('(' params ')')? (':' Ident)? ':=' Expr
         |  'async' 'def' ident ('(' params ')')? ':=' Expr
         |  'extern' 'js' '"' raw-js '"' ident
         |  'inductive' ident 'where' ('|' Ctor)+
         |  'class' ident 'where' ident+
         |  'instance' ident ident 'where' Method (',' Method)*

Ctor    ::= ident ('(' ident (',' ident)* ')')?
Method  ::= ident ':=' Expr
Param   ::= ident (':' Ident)?       -- type annotation optional

Expr    ::= 'let' ident ':=' Expr ';' Expr
         |  'if' Expr 'then' Expr 'else' Expr
         |  'fun' '(' params ')' '=>' Expr
         |  'match' Expr 'with' ('|' Pattern '=>' Expr)+
         |  'await' App
         |  Or

Pattern ::= ident ('(' ident (',' ident)* ')')?

Or      ::= And  ('||' And)*
And     ::= Eq   ('&&' Eq)*
Eq      ::= Cmp  (('=='|'!=') Cmp)*
Cmp     ::= Add  (('<'|'<='|'>'|'>=') Add)*
Add     ::= Mul  (('+'|'-') Mul)*
Mul     ::= App  (('*'|'/'|'%') App)*
App     ::= Atom ( '(' Expr (',' Expr)* ')'
                 | '.' ident
                 | '[' Expr ']' )*
Atom    ::= NUMBER | STRING | ArrayLit | ObjectLit | '(' Expr ')' | IDENT

ArrayLit ::= '[' (Expr (',' Expr)*)? ']'
ObjectLit::= '{' (ident ':' Expr (',' ident ':' Expr)*)? '}'
```

`--` starts a line comment, ended by newline or EOF.

Reserved words: `let in if then else fun def inductive where match
with async await extern js class instance`.

---

## Expressions

### Literals

```text
42            -- number (Int) — emits 42
-7            -- numbers can be signed
0xff          -- hex literal — emits 255
3.14          -- float (text copied verbatim — no rounding at parse)
"hello"       -- string — emits "hello"
true / false  -- boolean — emits true / false
null          -- null — emits null
[1, 2, 3]     -- array literal — emits [1,2,3]
{x: 1, y: 2}  -- object literal (unquoted keys) — emits {x:1,y:2}
```

### Variables and access

```text
x             -- variable reference
arr[i]        -- index — emits arr[i]
obj.field     -- dot access — emits obj.field
obj?.field    -- optional chain — emits obj?.field
f(a, b)       -- call — emits f(a,b)
new C(a, b)   -- constructor — emits new C(a,b)
!cond         -- logical not — emits (!cond)
```

`.field` and `?.field` require identifier-shaped names. For dynamic
property access use `obj["some key"]`.

### Assignment

```text
obj.field <- value      -- emits (obj.field = value)
arr[i]    <- value      -- emits (arr[i]    = value)
```

The assignment expression evaluates to the RHS, so it composes
inside `let _ := … ;` to chain side effects:

```text
def tick() :=
  let _ := state.count <- state.count + 1;
  let _ := renderer.render(scene, cam);
  null
```

For named refs prefer `Ref`-shaped helpers via `extern js`:

```text
extern js "(v) => ({v})"             mkRef
extern js "(r) => r.v"               getRef
extern js "(r, v) => (r.v = v)"      setRef
```

### Operators

By precedence (lowest first):

| Level | Operators              | Notes                          |
|-------|------------------------|--------------------------------|
| 1     | `\|\|`                 | logical OR (left-assoc)        |
| 2     | `&&`                   | logical AND                    |
| 3     | `==` `!=`              | equality                       |
| 4     | `<` `<=` `>` `>=`      | comparison                     |
| 5     | `+` `-`                | arithmetic add / subtract      |
| 6     | `*` `/` `%`            | multiply / divide / modulo     |

Unary minus is parsed only as part of a literal (`-7`). For
`-expression` use `0 - expression` or wrap the literal.

Equality emits as `==` (JS loose equality), not `===`. Programs
relying on strict equality should compare types explicitly.

### `let`

```text
let x := 1 + 2;
x * 10
```

emits

```js
((x) => x * 10)(1 + 2)
```

`let` is expression-shaped (an IIFE). There is no `let mut` — see
*What's not in the language* below.

### `if / then / else`

```text
if x == 0 then "zero" else "nonzero"
```

emits a ternary `(x == 0) ? "zero" : "nonzero"`. The `else` branch
is mandatory.

### `fun`

```text
fun (a, b) => a + b
```

emits `((a, b) => { return a + b; })`. Arrow expressions are always
parenthesized so they're safe to use as a call target inline.

### `await`

```text
def fetchPage := await get("/page")
```

emits `await get("/page")`. Only meaningful inside the body of an
`async def`. Awaitable values come from `extern js` (your binding to
`fetch`, a Promise factory, etc.).

### `match` over `inductive`

```text
inductive Color where
  | RGB(r, g, b)
  | Gray(v)
  | Hex(s)

def name(c) :=
  match c with
    | RGB(r, g, b) => "rgb"
    | Gray(v)      => "gray"
    | Hex(s)       => "hex"
```

A constructor of arity `n` emits as

```js
const RGB = (r, g, b) => ({tag: "RGB", $0: r, $1: g, $2: b});
const Gray = (v)      => ({tag: "Gray", $0: v});
const Hex  = (s)      => ({tag: "Hex",  $0: s});
```

Zero-arity constructors are values, not factories:

```text
inductive Maybe where
  | None
  | Some(v)
```

emits `const None = {tag: "None"};` and `const Some = (v) => ({tag:
"Some", $0: v});`. So you write `None` (not `None()`) for the value
and `Some(42)` to construct.

`match` lowers to a chain of ternaries that read the `tag` field and
bind `$0`, `$1`, … in order. A constructor name not in any branch
falls through to `null`.

---

## Top-level forms

### `def`

```text
def square(x) := x * x
def pi := 314 / 100             -- parameter list is optional
def ratio(a, b) : Int := a / b  -- optional return-type annotation
```

A `def` with no params emits as a `const` value (eagerly evaluated).
With params it emits as an arrow:

```js
const square = (x) => { return x * x; };
const pi     = 314 / 100;
const ratio  = (a, b) => { return a / b; };
```

The return-type and per-param `: Type` annotations are *advisory* —
LeanJs does not type-check. They flow into `LeanEmit` so the emitted
real-Lean source compiles; defaults to `Int` when absent.

Top-level `def main := …` is the conventional entry point used by
the bilingual test runners (its value is printed).

### `async def`

```text
async def page(url) := await fetch(url)
```

emits

```js
const page = async (url) => { return await fetch(url); };
```

Caller must `await` it. Inside an `async def`, `await` can be used
anywhere an expression is.

### `extern js` — the FFI

```text
extern js "(n) => Array(n).fill(0)" mkArray
extern js "(a, b) => '' + a + b" concat
```

binds the named identifier to a raw JS expression. The string is
spliced into the output **verbatim** — LeanJs does not parse or
validate it. Usage looks like an ordinary call:

```text
def init := mkArray(64)
```

emits

```js
const mkArray = (n) => Array(n).fill(0);
const init    = mkArray(64);
```

`extern` is how you reach `console.log`, `Math`, `fetch`, DOM
helpers, anything else from the host.

> **`globalThis.X` for global names.** A naive `extern js "document"
> document` emits `const document = document;` and crashes at module
> load (the LHS local shadows the global it tried to alias, hitting
> the temporal-dead-zone trap). Use `extern js "globalThis.document"
> document` to break the cycle.

> **Bilingual restriction:** `LeanEmit` can't translate `extern js`
> to Lean (the right-hand side is opaque JS), so any program that
> uses an `extern` is JS-only. The spec runner tags those cases
> `(js only)`.

> **Arity checking:** LeanJs's pre-codegen pass (`LeanJs.Check`)
> reads the FFI source, extracts the param count from common arrow
> shapes (`(a, b) => …` and `n => …`), and verifies every call site
> uses the right arg count. Opaque FFIs (a bare `Math.max`, a name
> reference) skip the check.

### `import` — ES modules

```text
import * as THREE from "three"
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js"
import { VRMLoaderPlugin as VRMP } from "@pixiv/three-vrm"
```

Emits the corresponding `import * as …` / `import { … }` lines at
the top of the JS output. Required for ES-module entry points
(e.g. game / Three.js code that runs in the browser).

### `inductive`

```text
inductive Shape where
  | Rect(w, h)
  | Circle(r)
```

See *match* above for the dispatch story.

### `class` / `instance`

```text
class Show where
  show

instance Show Int where
  show := fun (n) => "Int:" + n

instance Show String where
  show := fun (s) => "String:" + s
```

`class` emits `const Show = {};`. Each `instance Show Int where m :=
…` emits `Show.Int = { show: …, … };`. Call sites resolve by
explicit qualification: `Show.Int.show(42)`.

There is no automatic dispatch by value type. The instance table is
a typed lookup convention, not a vtable.

---

## How values lower to JS

| LeanJs                 | JS                                         |
|------------------------|--------------------------------------------|
| `42`, `-7`             | `42`, `-7`                                 |
| `"hi"`                 | `"hi"`                                     |
| `[1, 2]`               | `[1, 2]`                                   |
| `{x: 1, y: 2}`         | `{x: 1, y: 2}`                             |
| `obj.field`            | `obj.field`                                |
| `arr[i]`               | `arr[i]`                                   |
| `f(a, b)`              | `f(a, b)`                                  |
| `a + b`                | `(a + b)`                                  |
| `a && b` / `\|\|`      | `(a && b)` / `(a \|\| b)`                  |
| `a == b`               | `(a == b)`                                 |
| `let x := v; body`     | `((x) => body)(v)`                         |
| `if c then a else b`   | `(c ? a : b)`                              |
| `fun (x) => e`         | `((x) => { return e; })`                   |
| `match e with …`       | `((__m) => __m.tag === "C" ? … : null)(e)` |
| `await e`              | `await e`                                  |
| `def main := …`        | `const main = …;` (or arrow if params)     |
| `async def …`          | `const … = async (…) => { return …; };`    |
| `extern js "raw" id`   | `const id = raw;`                          |

Programs end with `console.log(main)` so a `node program.js`
invocation prints the entry value — this is what `Reversi.compileGame`
and `leanjs_spec` rely on.

---

## The bilingual story

`LeanJs.LeanEmit` translates the same `Ast` to Lean source that the
real `lean --run` accepts. For programs that don't touch the FFI,
the two outputs (`node program.js` and `lean --run program.lean`) are
asserted byte-equal in `lake exe leanjs_spec`.

What flows through both ends:

- numbers, strings, arithmetic
- `let`, `if`, `fun`, application
- recursion (accumulator pattern; Reversi's `walkCap`, `countLoop`)
- top-level `def main := …` as entry point

What stays JS-only:

- anything that calls an `extern js` binding
- `await` / `async def` (no Lean equivalent in the harness)
- `inductive` / `match` are JS-only today

This is why Reversi's game logic compiles to JS and is served at
runtime, but the same source isn't smoke-tested with `lean --run` —
it uses `extern js` for `mkArray`, `arrGet`, `arrSet`, `sconcat`.

---

## What's *not* in the language

Deliberate omissions, with the rationale and the workaround:

| Missing             | Why                                                | Workaround                                   |
|---------------------|----------------------------------------------------|----------------------------------------------|
| `let mut`           | breaks the bilingual story; `IO.Ref` on the Lean side | recursion with an accumulator parameter, or `<-` on object fields |
| Statement sequencing| LeanJs is expression-shaped end to end             | nest `let _ := … ;` so each step discards its value |
| `while` / `for`     | same — needs statements + mutation                 | tail-recursive helper functions              |
| Ternary `cond ? a : b` | use `if cond then a else b`                     | n/a                                          |
| Unary `-expr` (standalone) | only parsed as part of a literal           | `0 - expr` for negation                      |
| Type checking       | optional annotations exist but aren't enforced     | rely on `node` / `lean --run` at runtime; the arity check catches the cheap class |
| Source maps         | none                                               | column-1 errors in node's stack trace        |

Things you *can* express now (since the imperative extension landed):

* `obj.field <- value` and `arr[i] <- value` — assignment expressions
* `new C(args)` constructor invocation
* `obj?.field` optional chaining
* `null`, `true`, `false`
* Hex literals `0xff`, float literals `3.14`
* `import * as X from "…"` and `import { a, b as c } from "…"`
* `def f() := …` — explicit 0-arity function (becomes a callable, not a value)
* Top-level expression statements for boot calls (`tick()` at the
  end of a `.leanjs` file)

The combination is enough to write Three.js / VRM game loops
directly in `.leanjs` — see a `.leanjs` game source for the
worked example.

---

## Worked example — sumLoop, bilingual

```text
def sumLoop(i, acc) :=
  if i == 0 then acc
  else sumLoop(i - 1, acc + i)

def main := sumLoop(10, 0)
```

Compiles to JS:

```js
const sumLoop = (i, acc) => { return ((i == 0) ? acc : sumLoop((i - 1), (acc + i))); };
const main    = sumLoop(10, 0);
console.log(main);
```

…and to Lean:

```lean
partial def sumLoop (i : Int) (acc : Int) : Int :=
  if i == 0 then acc else sumLoop (i - 1) (acc + i)
def main : IO Unit := IO.println (sumLoop 10 0)
```

`lake exe leanjs_spec` runs both and asserts both print `55`.

---

## Where to look next

- [`LeanJs/COMPARISON.md`](COMPARISON.md) — LeanJs vs JS vs Lean,
  with side-by-side translation tables
- `docs/08-fay-style.md` — design rationale, scorecard, the
  bilingual claim
- `docs/09-reversi.md` — a full game written in this subset
- `docs/10-architecture.md` — where LeanJs fits in the bigger
  LeanTea picture
- `examples/Reversi/Game.leanjs` — pure-subset source, ~115 lines
- a `.leanjs` game source — imperative subset, ~200 lines
  with imports, mutation, `new`, optional chaining
- `examples/Tests/LeanJsSpec.lean` — the spec runner, with both
  layers (`node` and `lean --run`)
- `lake exe leanjs_spec` — the receipts, green or red
