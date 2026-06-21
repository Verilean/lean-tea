/-! # LeanJs.Ast — abstract syntax for the Lean-subset we compile to JS

```text
Program ::= TopDef*

TopDef  ::= 'def' ident ('(' params ')')? ':=' Expr
         |  'inductive' ident 'where' ('|' Ctor)+

Ctor    ::= ident ('(' ident,* ')')?   -- arity-only, no types

Expr    ::= 'let' ident ':=' Expr ';' Expr
         |  'if' Expr 'then' Expr 'else' Expr
         |  'fun' '(' params ')' '=>' Expr
         |  'match' Expr 'with' ('|' Pattern '=>' Expr)+
         |  Add

Pattern ::= ident ('(' ident,* ')')?   -- constructor head + binders

Add     ::= Mul (('+' | '-') Mul)*
Mul     ::= App (('*' | '/') App)*
App     ::= Atom ('(' Expr,* ')')*
Atom    ::= NUMBER | STRING | IDENT | '(' Expr ')'
```

Sum types arrive via `inductive`: each declaration introduces one
type whose values are tagged variants. A constructor with arity > 0
is callable like a function (`Some(x)`); zero-arity constructors
are values (`None`). `match` does dispatch on the tag and binds
constructor fields by position.

The AST is what the parser produces and what the emitter consumes.
The emitter targets `LeanTea.Js.Expr` / `Stmt`, so the rendered
output composes with everything else the framework ships. -/

namespace LeanJs.Ast

/-- One arm of a `match` head. Three forms:
    * `ctor (name) (args)` — inductive constructor with named field
      binders. The legacy form; arity check applies.
    * `strLit "literal"` — matches a string scrutinee literally.
    * `wildcard` — matches anything; acts as the fall-through. -/
inductive Pattern where
  | ctor    (name : String) (args : List String) : Pattern
  | strLit  (s : String) : Pattern
  | wildcard : Pattern
  deriving Inhabited

mutual

inductive Expr where
  | num   (n : Int) : Expr
  /-- Floating-point literal, kept as the source text so we don't
      round at parse time. The emitter copies the literal verbatim. -/
  | numF  (lit : String) : Expr
  | str   (s : String) : Expr
  | var   (name : String) : Expr
  | app   (f : Expr) (args : List Expr) : Expr
  | binop (op : String) (l r : Expr) : Expr
  | letE  (name : String) (value body : Expr) : Expr
  | ifE   (c t e : Expr) : Expr
  | fnE   (params : List String) (body : Expr) : Expr
  /-- `match scrutinee with | Ctor1 a b => body1 | Ctor2 => body2 ...` -/
  | matchE (scrutinee : Expr) (branches : List MatchBranch) : Expr
  /-- `e.field` — accessor on an expression. Used for FFI calls
      like `arr.length` or `r.json()`. -/
  | dotE  (e : Expr) (field : String) : Expr
  /-- `await e` — evaluates an async value. -/
  | awaitE (e : Expr) : Expr
  /-- `[e1, e2, …]` — array literal. -/
  | arrE  (xs : List Expr) : Expr
  /-- `{k1: v1, k2: v2, …}` — object literal (string keys). -/
  | objE  (fields : List (String × Expr)) : Expr
  /-- `e[idx]` — index / lookup. Arrays by integer; objects by
      string. -/
  | idxE  (e idx : Expr) : Expr
  /-- `null` — the JS null sentinel. Distinct from absent / undefined
      so the host APIs that return it (e.g. `Map.get`) can be tested. -/
  | nullE : Expr
  /-- Boolean literal `true` / `false`. -/
  | boolE (b : Bool) : Expr
  /-- Unary prefix op — currently `!` (logical not) and `-` (numeric
      negate). Parens get added at emit so precedence is unambiguous. -/
  | unopE (op : String) (e : Expr) : Expr
  /-- `new C(args…)` — JS constructor invocation. `C` is itself an
      expression so `new mod.Klass(x)` works. -/
  | newE  (cls : Expr) (args : List Expr) : Expr
  /-- `e?.field` — optional chaining. Returns `undefined` if `e`
      is null/undefined; otherwise reads `field`. -/
  | optDotE (e : Expr) (field : String) : Expr
  /-- Assignment statement, valued as the RHS so it stays
      expression-shaped (consistent with the rest of LeanJs's IIFE
      lowering). LHS must be a `dotE`, `optDotE`, `idxE`, or `var`. -/
  | assignE (lhs rhs : Expr) : Expr
  /-- Sequenced expression `a; b`. Evaluates `a` for its effect,
      returns `b`. Lowers to a comma expression. -/
  | seqE   (a b : Expr) : Expr
  /-- `Cls { f1: v1, f2: v2, … }` — record-literal construction.
      Lowers to a call into the record's constructor (`Cls({…})`).
      `Check` verifies that the supplied field set matches the
      declared shape so typos surface at compile-time. -/
  | recordLitE (cls : String) (fields : List (String × Expr)) : Expr

/-- One arm of a `match`. `pat` carries either a constructor
    pattern (with named field binders), a string literal, or a
    wildcard. -/
inductive MatchBranch where
  | mk (pat : Pattern) (body : Expr) : MatchBranch

end

namespace MatchBranch
def pat : MatchBranch → Pattern
  | .mk p _ => p
def body : MatchBranch → Expr
  | .mk _ b => b
end MatchBranch

instance : Inhabited Expr := ⟨.num 0⟩
instance : Inhabited MatchBranch := ⟨.mk .wildcard (.num 0)⟩

/-- One constructor of an inductive declaration. We only carry
    the *arity* (number of fields); no type-checking happens at
    the AST level. -/
structure CtorDecl where
  name  : String
  arity : Nat
  deriving Inhabited, Repr

/-- One formal parameter of a `def`. `type? = none` means the
    parameter was written without an annotation — we default to
    `Int` when emitting Lean source so the program stays compilable. -/
structure Param where
  name  : String
  type? : Option String := none
  deriving Inhabited

/-- One binding of an `import` clause.
    * `namespaceAs none`        → `import { name } from "…"`
    * `namespaceAs (some "X")`  → `import { name as X } from "…"`
    A `*` (namespace import) is encoded as `name = "*"`. -/
structure ImportBinding where
  name        : String
  namespaceAs : Option String := none
  deriving Inhabited

inductive TopDef where
  /-- `def name (params) (: returnType)? := body` — sync def. -/
  | defE  (name : String) (params : List Param) (retType? : Option String)
          (body : Expr) : TopDef
  /-- `async def name (params) := body` — async def; emits an
      `async` arrow. Caller must `await` it. -/
  | asyncDefE (name : String) (params : List Param) (body : Expr) : TopDef
  /-- `inductive Name where | Ctor1 (a b) | Ctor2` — sum type. -/
  | indE  (name : String) (ctors : List CtorDecl) : TopDef
  /-- `extern js "raw-js-expr" name` — binds `name` at JS-emit time
      to the verbatim source on the right. The minimal FFI. -/
  | externE (name : String) (rawJs : String) : TopDef
  /-- `class Name where m1 m2 m3` — declares a typeclass with the
      named methods. Lowers to `const Name = {}` so instances can
      attach to it. -/
  | classE (name : String) (methods : List String) : TopDef
  /-- `instance Name Type where m1 := body1, m2 := body2` — registers
      a per-type method dictionary at `Name.Type`. Methods are
      called via `Name.Type.m1(args)`. -/
  | instE  (className typeName : String)
           (methods : List (String × Expr)) : TopDef
  /-- `import * as Name from "module"` (namespace import) or
      `import { a, b as c } from "module"` (named). -/
  | importE (bindings : List ImportBinding) (source : String) : TopDef
  /-- `include "path.leanjs"` — compile-time splice of another LeanJs
      file's top-level defs into this program. Unlike `importE` which
      lowers to a JS ESM import, `includeE` is resolved by
      `LeanJs.Includes.resolve` before Check / Codegen / Eval ever
      see the program — Codegen and friends just skip it. -/
  | includeE (path : String) : TopDef
  /-- `record Name where f1 : T1, f2 : T2, …` — declares a record
      type. Field types are recorded as strings (no type-check); the
      shape is what `Check` enforces on `recordLitE` constructions. -/
  | recordE (name : String) (fields : List (String × String)) : TopDef
  /-- Top-level expression statement. Useful for boot calls like
      `tick()` or `loadOne("p1")` that don't bind a name but must
      run at module load. -/
  | exprE  (e : Expr) : TopDef
  deriving Inhabited

abbrev Program := Array TopDef

end LeanJs.Ast
