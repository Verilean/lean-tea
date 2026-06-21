/-! # Minimal JavaScript AST + renderer

A small expression / statement AST so the browser-side glue can be
written in Lean instead of hand-stitched strings. The renderer emits
compact, semicolon-terminated source that's readable but not pretty.

Coverage is intentionally narrow — what the LeanTea sample apps need:

* variables, dot / index access, calls, `new`, async + arrow funcs
* template literals, ternary, await
* control flow: if / try-catch / return
* function declarations (regular + async)
* object & array literals, common operators

For anything outside this set, use the `raw` escape hatches. -/

namespace LeanTea.Js

mutual
  inductive Expr where
    | num    (n : Int) : Expr
    | str    (s : String) : Expr
    | bool   (b : Bool) : Expr
    | nul    : Expr
    | undef  : Expr
    | id     (name : String) : Expr
    | dot    (e : Expr) (field : String) : Expr
    /-- Optional-chained access `e?.field`. Returns `undefined`
        when `e` is `null` or `undefined`. -/
    | optDot (e : Expr) (field : String) : Expr
    | idx    (e : Expr) (key : Expr) : Expr
    | call   (f : Expr) (args : List Expr) : Expr
    | newE   (f : Expr) (args : List Expr) : Expr
    | binop  (op : String) (l r : Expr) : Expr
    | unop   (op : String) (e : Expr) : Expr
    | tern   (c t e : Expr) : Expr
    | obj    (fields : List (String × Expr)) : Expr
    | arr    (xs : List Expr) : Expr
    | arrow  (params : List String) (body : List Stmt) : Expr
    | aarrow (params : List String) (body : List Stmt) : Expr
    | await  (e : Expr) : Expr
    /-- `tmpl ["a=", ",b=", ""] [x, y]` → ``a=${x},b=${y}`` -/
    | tmpl   (parts : List String) (interps : List Expr) : Expr
    | raw    (src : String) : Expr
  deriving Inhabited

  inductive Stmt where
    | letS    (name : String) (value : Expr) : Stmt
    | constS  (name : String) (value : Expr) : Stmt
    | assignS (lhs rhs : Expr) : Stmt
    | exprS   (e : Expr) : Stmt
    | retS    (e : Option Expr) : Stmt
    | ifS     (cond : Expr) (then_ : List Stmt) (els : List Stmt) : Stmt
    | tryS    (try_ : List Stmt) (catchVar : String) (catch_ : List Stmt) : Stmt
    | fnS     (name : String) (params : List String) (body : List Stmt) : Stmt
    | asyncFnS (name : String) (params : List String) (body : List Stmt) : Stmt
    /-- `for (let v of expr) { body }`. -/
    | forOfS  (name : String) (iter : Expr) (body : List Stmt) : Stmt
    /-- `for (let v = init; cond; step) { body }`. -/
    | forS    (name : String) (init cond step : Expr) (body : List Stmt) : Stmt
    /-- `while (cond) { body }`. -/
    | whileS  (cond : Expr) (body : List Stmt) : Stmt
    | breakS : Stmt
    | continueS : Stmt
    /-- `import * as ns from 'src'` if `ns = some n`, else side-effect only. -/
    | importStarS (ns : String) (src : String) : Stmt
    /-- `import { a, b as c } from 'src'`. Each pair is `(imported, local)`. -/
    | importNamedS (binds : List (String × String)) (src : String) : Stmt
    | rawS    (src : String) : Stmt
  deriving Inhabited
end

abbrev Block := List Stmt

/-! ## String escapes -/

private def escStr (s : String) : String :=
  s.replace "\\" "\\\\"
   |>.replace "\n" "\\n"
   |>.replace "\r" "\\r"
   |>.replace "\t" "\\t"
   |>.replace "'" "\\'"

private def escTmpl (s : String) : String :=
  s.replace "\\" "\\\\"
   |>.replace "`" "\\`"
   |>.replace "$" "\\$"

/-! ## Render -/

mutual
  partial def renderExpr : Expr → String
    | .num n => toString n
    | .str s => "'" ++ escStr s ++ "'"
    | .bool b => if b then "true" else "false"
    | .nul   => "null"
    | .undef => "undefined"
    | .id n  => n
    | .dot e f => renderExpr e ++ "." ++ f
    | .optDot e f => renderExpr e ++ "?." ++ f
    | .idx e k => renderExpr e ++ "[" ++ renderExpr k ++ "]"
    | .call f xs =>
        renderExpr f ++ "(" ++ renderArgs xs ++ ")"
    | .newE f xs =>
        "new " ++ renderExpr f ++ "(" ++ renderArgs xs ++ ")"
    | .binop op l r =>
        "(" ++ renderExpr l ++ op ++ renderExpr r ++ ")"
    | .unop op e =>
        "(" ++ op ++ renderExpr e ++ ")"
    | .tern c t e =>
        "(" ++ renderExpr c ++ "?" ++ renderExpr t ++ ":" ++ renderExpr e ++ ")"
    | .obj fields =>
        "{" ++ renderFields fields ++ "}"
    | .arr xs =>
        "[" ++ renderArgs xs ++ "]"
    | .arrow params body =>
        -- Outer parens so the arrow is unambiguously an expression
        -- (IIFE position, member access, etc.).
        "((" ++ String.intercalate "," params ++ ")=>{" ++ renderBlock body ++ "})"
    | .aarrow params body =>
        "(async(" ++ String.intercalate "," params ++ ")=>{" ++ renderBlock body ++ "})"
    | .await e => "(await " ++ renderExpr e ++ ")"
    | .tmpl parts interps => "`" ++ renderTmpl parts interps ++ "`"
    | .raw s => s

  partial def renderArgs : List Expr → String
    | [] => ""
    | [x] => renderExpr x
    | x :: xs => renderExpr x ++ "," ++ renderArgs xs

  partial def renderKey (k : String) : String :=
    -- Quote any key that isn't a valid bare identifier (alpha, digit
    -- not in first position, _, $). Conservative: any non-ident char
    -- forces quoting.
    let isIdent : Bool :=
      !k.isEmpty &&
      (k.toList.all fun c => c.isAlpha || c.isDigit || c == '_' || c == '$') &&
      !(k.front.isDigit)
    if isIdent then k else "'" ++ k.replace "'" "\\'" ++ "'"

  partial def renderFields : List (String × Expr) → String
    | [] => ""
    | [(k, v)] => renderKey k ++ ":" ++ renderExpr v
    | (k, v) :: rest => renderKey k ++ ":" ++ renderExpr v ++ "," ++ renderFields rest

  partial def renderTmpl : List String → List Expr → String
    | [], _ => ""
    | [p], _ => escTmpl p
    | p :: ps, e :: es =>
        escTmpl p ++ "${" ++ renderExpr e ++ "}" ++ renderTmpl ps es
    | p :: ps, [] => escTmpl p ++ renderTmpl ps []

  partial def renderStmt : Stmt → String
    | .letS n v   => "let " ++ n ++ "=" ++ renderExpr v ++ ";"
    | .constS n v => "const " ++ n ++ "=" ++ renderExpr v ++ ";"
    | .assignS l r => renderExpr l ++ "=" ++ renderExpr r ++ ";"
    | .exprS e     => renderExpr e ++ ";"
    | .retS none   => "return;"
    | .retS (some e) => "return " ++ renderExpr e ++ ";"
    | .ifS c t e =>
        let elsPart := if e.isEmpty then "" else "else{" ++ renderBlock e ++ "}"
        "if(" ++ renderExpr c ++ "){" ++ renderBlock t ++ "}" ++ elsPart
    | .tryS t v c =>
        "try{" ++ renderBlock t ++ "}catch(" ++ v ++ "){" ++ renderBlock c ++ "}"
    | .fnS n params body =>
        "function " ++ n ++ "(" ++ String.intercalate "," params
          ++ "){" ++ renderBlock body ++ "}"
    | .asyncFnS n params body =>
        "async function " ++ n ++ "(" ++ String.intercalate "," params
          ++ "){" ++ renderBlock body ++ "}"
    | .forOfS n iter body =>
        "for(let " ++ n ++ " of " ++ renderExpr iter ++ "){"
          ++ renderBlock body ++ "}"
    | .forS n init cond step body =>
        "for(let " ++ n ++ "=" ++ renderExpr init ++ ";"
          ++ renderExpr cond ++ ";"
          ++ renderExpr step ++ "){" ++ renderBlock body ++ "}"
    | .whileS cond body =>
        "while(" ++ renderExpr cond ++ "){" ++ renderBlock body ++ "}"
    | .breakS    => "break;"
    | .continueS => "continue;"
    | .importStarS ns src =>
        "import * as " ++ ns ++ " from '" ++ escStr src ++ "';"
    | .importNamedS binds src =>
        let pairs := binds.map fun (imp, loc) =>
          if imp == loc then imp else imp ++ " as " ++ loc
        "import {" ++ String.intercalate "," pairs ++ "} from '"
          ++ escStr src ++ "';"
    | .rawS s => s

  partial def renderBlock (b : Block) : String :=
    b.foldl (fun acc s => acc ++ renderStmt s) ""
end

def Block.render (b : Block) : String := renderBlock b

/-! ## Convenience builders

These keep the call sites readable without forcing the AST into
every corner of the user's code. -/

namespace E
  def n (k : Int)        : Expr := .num k
  def s (x : String)     : Expr := .str x
  def b (x : Bool)       : Expr := .bool x
  def i (name : String)  : Expr := .id name
  def nul                : Expr := .nul
  def undef              : Expr := .undef
  def dot (e : Expr) (f : String) : Expr := .dot e f
  /-- `e?.field` — JS optional chaining. -/
  def optDot (e : Expr) (f : String) : Expr := .optDot e f
  def idx (e : Expr) (k : Expr) : Expr := .idx e k

  /-- Hex literal (`0xRRGGBB` style). Rendered lowercase with at
      least six digits, suitable for three.js / CSS colours. -/
  def hex (n : Nat) : Expr := .raw (s!"0x" ++ hexPadded n)
  where
    hexDigit (k : Nat) : Char :=
      "0123456789abcdef".get! ⟨k⟩
    hexPadded (n : Nat) : String := Id.run do
      let mut s : String := ""
      let mut x := n
      if x == 0 then return "000000"
      while x > 0 do
        s := (hexDigit (x % 16)).toString ++ s
        x := x / 16
      while s.length < 6 do s := "0" ++ s
      return s

  /-- Float literal. Renders via `Float.toString`; suitable for
      compile-time constants like `0.5` / `1.4`. -/
  def fl (x : Float) : Expr := .raw (toString x)
  def call (f : Expr) (xs : List Expr := []) : Expr := .call f xs
  def mcall (e : Expr) (m : String) (xs : List Expr := []) : Expr := .call (.dot e m) xs
  def new_ (f : Expr) (xs : List Expr := []) : Expr := .newE f xs
  def obj (fs : List (String × Expr)) : Expr := .obj fs
  def arr (xs : List Expr) : Expr := .arr xs
  def arrow (ps : List String := []) (body : Block) : Expr := .arrow ps body
  def aarrow (ps : List String := []) (body : Block) : Expr := .aarrow ps body
  def await_ (e : Expr) : Expr := .await e
  def tmpl (parts : List String) (interps : List Expr) : Expr := .tmpl parts interps
  def tern (c t e : Expr) : Expr := .tern c t e
  def eq      (l r : Expr) : Expr := .binop "==" l r
  def neq     (l r : Expr) : Expr := .binop "!=" l r
  def lt      (l r : Expr) : Expr := .binop "<" l r
  def le      (l r : Expr) : Expr := .binop "<=" l r
  def gt      (l r : Expr) : Expr := .binop ">" l r
  def ge      (l r : Expr) : Expr := .binop ">=" l r
  def add     (l r : Expr) : Expr := .binop "+" l r
  def sub     (l r : Expr) : Expr := .binop "-" l r
  def or_     (l r : Expr) : Expr := .binop "||" l r
  def and_    (l r : Expr) : Expr := .binop "&&" l r
  def not_    (e : Expr) : Expr := .unop "!" e

  /-! Infix notation for comparison ops on `Expr`. `LT`/`LE` would
      return `Prop`, which is the wrong shape — we want another
      `Expr`. So we use the same `<.` / `<=.` / `>=.` / `>.` family
      the Persist.Query DSL exposes for the same reason. -/
  scoped infix:50 " <. "  => lt
  scoped infix:50 " <=. " => le
  scoped infix:50 " >. "  => gt
  scoped infix:50 " >=. " => ge
  /-- Escape hatch for any binary operator (`*`, `/`, `%`, `|`, …).
      Prefer the named helpers when there is one. -/
  def binop (op : String) (l r : Expr) : Expr := .binop op l r
  /-- Escape hatch for unary operators (`!`, `-`, `+`, `typeof`, …). -/
  def unop  (op : String) (e : Expr) : Expr := .unop op e
  def raw (src : String) : Expr := .raw src
end E

namespace S
  def letV   (n : String) (v : Expr) : Stmt := .letS n v
  def constV (n : String) (v : Expr) : Stmt := .constS n v
  def assign (l r : Expr) : Stmt := .assignS l r
  def doE    (e : Expr) : Stmt := .exprS e
  def retV   : Stmt := .retS none
  def retE   (e : Expr) : Stmt := .retS (some e)
  def ifS    (c : Expr) (t : Block) (e : Block := []) : Stmt := .ifS c t e
  def tryS   (body : Block) (var : String) (rescue : Block) : Stmt := .tryS body var rescue
  def fn     (n : String) (ps : List String) (body : Block) : Stmt := .fnS n ps body
  def afn    (n : String) (ps : List String) (body : Block) : Stmt := .asyncFnS n ps body
  def forOf  (n : String) (iter : Expr) (body : Block) : Stmt := .forOfS n iter body
  def forC   (n : String) (init cond step : Expr) (body : Block) : Stmt := .forS n init cond step body
  def whileS (c : Expr) (body : Block) : Stmt := .whileS c body
  def break_    : Stmt := .breakS
  def continue_ : Stmt := .continueS
  /-- `import * as ns from 'src'`. -/
  def importStar (ns src : String) : Stmt := .importStarS ns src
  /-- `import { a, b as c, … } from 'src'`. Bind list is
      `(imported-name, local-name)`. -/
  def importNamed (binds : List (String × String)) (src : String) : Stmt :=
    .importNamedS binds src
  def raw    (s : String) : Stmt := .rawS s
end S

/-! ## Ergonomic instances on `Expr`

Lean's dot notation looks up `Expr.foo` automatically, so `e.dot
"x"` already calls the constructor. The instances below cover the
*operator* side: `a + b`, `-a`, `0` — the things you would write in
JS without thinking. They render to the corresponding JS operator,
so the printed source is what you'd type by hand. -/

instance : Add Expr := ⟨E.add⟩
instance : Sub Expr := ⟨E.sub⟩
instance : Mul Expr := ⟨fun a b => .binop "*" a b⟩
instance : Div Expr := ⟨fun a b => .binop "/" a b⟩
instance : Neg Expr := ⟨E.unop "-"⟩
/-- `0`, `1`, `2`, … as `Expr`. Negative numbers flow through `Neg`. -/
instance : OfNat Expr n := ⟨E.n (Int.ofNat n)⟩

/-! ### Path / call shortcuts -/

namespace Expr

/-- Chain a series of `.foo.bar.baz` accesses in one call:
    `(i "p").path ["viewBox", "baseVal", "width"]`. -/
def path (e : Expr) (parts : List String) : Expr :=
  parts.foldl (fun acc p => .dot acc p) e

/-- `e(args)` — same as `E.call`, available via dot notation. -/
def apply (e : Expr) (args : List Expr := []) : Expr := .call e args

end Expr

/-! ## Block builder monad

The constructor calls (`S.constV "x" e`, `S.ifS c then_ else_`, …)
work, but they nest awkwardly when a section grows past a few
statements. `JsBuilder` runs statements through a `StateM` accumulator
so a `do` block reads top-to-bottom like real JavaScript.

```lean
def refresh : Stmt := afnB "refresh" [] do
  if_ (i "editing") (return_v)
  const "params" (new_ (i "URLSearchParams") [])
  let_ "sq"      (call (i "selectedQS") [])
  if_ (i "sq") do
    call_ ((i "params").dot "set" |>.apply [s "selected", i "sq"])
  ...
```

The shape mirrors the rendered JS step-for-step, and the helpers
return `JsBuilder Unit` so `do` sequencing just works. -/

abbrev JsBuilder := StateM (Array Stmt)

namespace JsBuilder

/-- Run a builder to a finished `Block`. -/
def build (m : JsBuilder Unit) : Block :=
  ((m.run #[]).snd).toList

/-- Push one statement onto the accumulator. -/
def emit (s : Stmt) : JsBuilder Unit := modify (·.push s)

/-! ### Statement helpers (mirror `S.*`, but as `JsBuilder Unit`) -/

def let_   (name : String) (v : Expr) : JsBuilder Unit := emit (.letS name v)
def const (name : String) (v : Expr) : JsBuilder Unit := emit (.constS name v)
def assign_ (l r : Expr) : JsBuilder Unit := emit (.assignS l r)
def call_ (e : Expr) : JsBuilder Unit := emit (.exprS e)
def return_v : JsBuilder Unit := emit (.retS none)
def return_ (e : Expr) : JsBuilder Unit := emit (.retS (some e))
def break_    : JsBuilder Unit := emit .breakS
def continue_ : JsBuilder Unit := emit .continueS
def raw_ (src : String) : JsBuilder Unit := emit (.rawS src)

def if_ (cond : Expr) (body : JsBuilder Unit) : JsBuilder Unit :=
  emit (.ifS cond (build body) [])

def ifElse (cond : Expr) (then_ els : JsBuilder Unit) : JsBuilder Unit :=
  emit (.ifS cond (build then_) (build els))

def forOf_ (v : String) (it : Expr) (body : JsBuilder Unit) : JsBuilder Unit :=
  emit (.forOfS v it (build body))

def forC_ (v : String) (init cond step : Expr)
    (body : JsBuilder Unit) : JsBuilder Unit :=
  emit (.forS v init cond step (build body))

def while_ (cond : Expr) (body : JsBuilder Unit) : JsBuilder Unit :=
  emit (.whileS cond (build body))

def try_ (body : JsBuilder Unit) (catchVar : String)
    (rescue : JsBuilder Unit) : JsBuilder Unit :=
  emit (.tryS (build body) catchVar (build rescue))

end JsBuilder

/-! ### Function-builder shortcuts that take a `JsBuilder` body -/

def fnB (name : String) (params : List String) (body : JsBuilder Unit) : Stmt :=
  .fnS name params (JsBuilder.build body)

def afnB (name : String) (params : List String) (body : JsBuilder Unit) : Stmt :=
  .asyncFnS name params (JsBuilder.build body)

def arrowB (params : List String) (body : JsBuilder Unit) : Expr :=
  .arrow params (JsBuilder.build body)

def aarrowB (params : List String) (body : JsBuilder Unit) : Expr :=
  .aarrow params (JsBuilder.build body)

/-! ## Embedding Lean values into JS

`ToJsExpr α` lets a Lean value cross the wire as a JS literal. It's
the bridge for server-rendered initial state: anything you can
construct in Lean can be planted into the generated client code as
`const MODEL = …` without a runtime fetch.

```lean
let initial : List (String × Nat) := [("rect", 3), ("ellipse", 1)]
let stmt := S.constV "INITIAL" (toJsExpr initial)
-- → `const INITIAL=[["rect",3],["ellipse",1]];`
```

Built-in instances cover the primitives; users add instances for
their own structs.

```lean
structure Score where mode : String; correct : Nat; total : Nat
instance : ToJsExpr Score where
  toJsExpr s := E.obj
    [("mode", toJsExpr s.mode),
     ("correct", toJsExpr s.correct),
     ("total", toJsExpr s.total)]
``` -/

class ToJsExpr (α : Type) where
  toJsExpr : α → Expr

export ToJsExpr (toJsExpr)

instance : ToJsExpr String     where toJsExpr := E.s
instance : ToJsExpr Int        where toJsExpr n := E.n n
instance : ToJsExpr Nat        where toJsExpr n := E.n (Int.ofNat n)
instance : ToJsExpr Bool       where toJsExpr := E.b
instance : ToJsExpr UInt8      where toJsExpr n := E.n (Int.ofNat n.toNat)
instance : ToJsExpr UInt32     where toJsExpr n := E.n (Int.ofNat n.toNat)
instance : ToJsExpr UInt64     where toJsExpr n := E.n (Int.ofNat n.toNat)
instance : ToJsExpr Unit       where toJsExpr _ := E.nul
instance [ToJsExpr α] : ToJsExpr (Option α) where
  toJsExpr
    | none   => E.nul
    | some a => toJsExpr a
instance [ToJsExpr α] : ToJsExpr (List α) where
  toJsExpr xs := E.arr (xs.map toJsExpr)
instance [ToJsExpr α] : ToJsExpr (Array α) where
  toJsExpr xs := E.arr (xs.toList.map toJsExpr)
instance [ToJsExpr α] [ToJsExpr β] : ToJsExpr (α × β) where
  toJsExpr := fun (a, b) => E.arr [toJsExpr a, toJsExpr b]
/-- `Std.HashMap`-shaped lookup tables become JS objects when keyed by
    `String`. Other key types should be encoded as arrays of pairs. -/
instance [ToJsExpr α] : ToJsExpr (List (String × α)) where
  toJsExpr xs := E.obj (xs.map fun (k, v) => (k, toJsExpr v))

/-! ## DOM / Browser helpers

These are just shortcuts so you don't have to spell out
`E.dot (E.id "document") "..."` every time. -/

namespace Dom
  open E

  def document : Expr := i "document"
  def windowE  : Expr := i "window"
  def body     : Expr := dot document "body"

  def getById (idStr : String) : Expr :=
    mcall document "getElementById" [s idStr]
  def querySelector (root : Expr) (sel : String) : Expr :=
    mcall root "querySelector" [s sel]
  def addEventListener (target : Expr) (ev : String) (cb : Expr) : Expr :=
    mcall target "addEventListener" [s ev, cb]
  def setAttr (target : Expr) (k : Expr) (v : Expr) : Expr :=
    mcall target "setAttribute" [k, v]
  def getAttr (target : Expr) (k : String) : Expr :=
    mcall target "getAttribute" [s k]
  def closest (target : Expr) (sel : String) : Expr :=
    mcall target "closest" [s sel]
  def preventDefault (ev : Expr) : Expr := mcall ev "preventDefault" []
  def parseInt (e : Expr) : Expr := call (i "parseInt") [e]
  def parseFloat (e : Expr) : Expr := call (i "parseFloat") [e]
  def encodeURIComponent (e : Expr) : Expr := call (i "encodeURIComponent") [e]
end Dom

/-! ## Async I/O helpers -/

namespace Fetch
  open E
  /-- `await fetch(url)` -/
  def get (url : Expr) : Expr := await_ (call (i "fetch") [url])
  /-- `await fetch(url, { method, body, headers })` -/
  def post (url body : Expr)
      (headers : Expr := obj [("content-type", s "application/x-www-form-urlencoded")])
      : Expr :=
    let opts := obj [("method", s "POST"), ("body", body), ("headers", headers)]
    await_ (call (i "fetch") [url, opts])
end Fetch

end LeanTea.Js
