import LeanJs.Ast
import LeanJs.Check
import LeanTea

/-! # LeanJs.Codegen — walk the Lean-subset AST, emit JavaScript

Targets `LeanTea.Js.Expr` / `Stmt` so the rendered output composes
with the rest of the framework.

Mapping:

  numbers / strings / vars     → matching JS literals
  binop op l r                → (l op r)
  let x := v; body             → IIFE `((x) => body)(v)`
  if c then a else b          → ternary
  fun (xs) => body            → arrow function
  app f args                  → call expression

Sum types:

  inductive Name where | Ctor1 (a b) | Ctor2
    → `const Ctor1 = (a, b) => ({tag:"Ctor1",$0:a,$1:b});`
      `const Ctor2 = ({tag:"Ctor2"});`
  match expr with | C a => e1 | D => e2
    → `((__m) => __m.tag === "C" ? ((a) => e1)(__m.$0) :
                                    __m.tag === "D" ? e2 :
                                    null)(expr)`

Tag dispatch uses chained ternaries so `match` stays an expression
— consistent with the rest of the lowering strategy. -/

namespace LeanJs.Codegen

open LeanJs.Ast LeanTea.Js LeanTea.Js.E LeanTea.Js.S

/-! ## Expressions -/

mutual

partial def compileExpr : Ast.Expr → LeanTea.Js.Expr
  | .num n        => E.n n
  | .numF lit     => E.raw lit
  | .str s'       => E.s s'
  | .var x        => E.i x
  | .binop op l r =>
    let l' := compileExpr l
    let r' := compileExpr r
    match op with
    | "+" => l' + r'
    | "-" => l' - r'
    | "*" => l' * r'
    | "/" => l' / r'
    | _   => binop op l' r'
  | .letE name value body =>
    let body' := compileExpr body
    let val'  := compileExpr value
    call (arrow [name] [retE body']) [val']
  | .ifE c t e =>
    tern (compileExpr c) (compileExpr t) (compileExpr e)
  | .fnE params body =>
    arrow params [retE (compileExpr body)]
  | .app f args =>
    call (compileExpr f) (args.map compileExpr)
  | .matchE scrutinee branches =>
    -- ((__m) => <chained ternary>)(scrutinee)
    let lam := arrow ["__m"] [retE (compileMatchChain branches)]
    call lam [compileExpr scrutinee]
  | .dotE e field => dot (compileExpr e) field
  | .awaitE e     => await_ (compileExpr e)
  | .arrE xs      => arr (xs.map compileExpr)
  | .objE kv      => obj (kv.map fun (k, v) => (k, compileExpr v))
  | .idxE e ix    => idx (compileExpr e) (compileExpr ix)
  | .nullE        => E.nul
  | .boolE b      => E.b b
  | .unopE op e   => E.unop op (compileExpr e)
  | .newE cls xs  => E.new_ (compileExpr cls) (xs.map compileExpr)
  | .optDotE e f  => E.optDot (compileExpr e) f
  | .assignE l r  =>
    /- JS assignment expression — `(lhs = rhs)`. `.assignS` is
       statement-only, so we render the sub-expressions and splice
       via `raw` to keep the whole thing expression-shaped. -/
    let lhs := LeanTea.Js.renderExpr (compileExpr l)
    let rhs := LeanTea.Js.renderExpr (compileExpr r)
    E.raw s!"({lhs} = {rhs})"
  | .seqE a b     =>
    /- Comma expression — `(a, b)` evaluates a (for effect),
       returns b. Same `raw` trick as assignment. -/
    let aStr := LeanTea.Js.renderExpr (compileExpr a)
    let bStr := LeanTea.Js.renderExpr (compileExpr b)
    E.raw s!"({aStr}, {bStr})"
  | .recordLitE cls fields =>
    /- `Cls({k1: v1, k2: v2, …})` — call the constructor with one
       object-shaped argument. Check has already verified the field
       set matches the declared shape, so codegen is unconditional. -/
    let lit := obj (fields.map fun (k, v) => (k, compileExpr v))
    call (i cls) [lit]

/-- One branch of a `match` lowered into a guarded value.
    Three pattern shapes:
      * `ctor C(a, b)` — `__m.tag === "C"`, binds field projections
      * `strLit "s"`   — `__m === "s"`, no bindings
      * `wildcard`     — no guard; collapses the rest of the chain -/
partial def compileMatchChain : List MatchBranch → LeanTea.Js.Expr
  | []            => E.nul
  | b :: rest     =>
    let body' := compileExpr b.body
    match b.pat with
    | .wildcard => body'
    | .strLit s' =>
      let guard := eq (i "__m") (E.s s')
      tern guard body' (compileMatchChain rest)
    | .ctor name args =>
      let guard := eq (dot (i "__m") "tag") (E.s name)
      let consumed :=
        if args.isEmpty then body'
        else
          let actuals : List LeanTea.Js.Expr :=
            args.length.fold (init := []) fun k _ acc =>
              acc ++ [idx (i "__m") (E.s s!"${k}")]
          call (arrow args [retE body']) actuals
      tern guard consumed (compileMatchChain rest)

end

/-! ## Top-level definitions -/

/-- A `def` becomes `const name = ...;`. Parameters drop their
    type annotations on the way to JS — JS has none. -/
def compileDefE (name : String) (params : List Param) (body : Ast.Expr) : Stmt :=
  let bodyE := compileExpr body
  let paramNames := params.map (·.name)
  let value : LeanTea.Js.Expr :=
    if paramNames.isEmpty then bodyE
    else arrow paramNames [retE bodyE]
  constV name value

/-- An `inductive` becomes one `const` per constructor. Arity-0
    constructors are tag-only objects; higher-arity constructors
    are arrow functions that build the tagged record. -/
def compileInductive (ctors : List CtorDecl) : Block :=
  ctors.map fun c =>
    let tagPair : (String × LeanTea.Js.Expr) := ("tag", s c.name)
    if c.arity == 0 then
      constV c.name (obj [tagPair])
    else
      -- Build params x0…x{arity-1}.
      let params : List String := c.arity.fold (init := []) fun k _ acc => acc ++ [s!"x{k}"]
      let fields : List (String × LeanTea.Js.Expr) :=
        tagPair :: params.zipIdx.map (fun (name, k) => (s!"${k}", i name))
      constV c.name (arrow params [retE (obj fields)])

/-- Flatten an `async def` body into a JS statement block so that
    `await` survives in the enclosing async function's lexical scope.

    Without this, `let x := y; rest` lowers to `((x) => rest)(y)` —
    the inner arrow is **not** async, and any `await` reachable from
    `rest` becomes a `SyntaxError: Unexpected identifier`. JS only
    permits `await` directly under an `async function` / arrow.

    Strategy: walk the `letE` chain producing `const x = compile(v);`
    statements; descend into `ifE` so branch bodies stay flattened;
    a terminal expression becomes `return compile(e);`. This is the
    closest analogue we can offer to Lean's `Task`/`Task.get` — both
    sides build linear "do" blocks where `await` (or `.get`) blends
    in with the surrounding sequence. -/
partial def compileAsyncBody : Ast.Expr → Block
  | .letE name value body =>
    -- `let _ := expr; rest` is the LeanJs idiom for "run expr for
    -- its side-effect, then continue". In IIFE form each `_` lived
    -- in its own scope; flattened, repeated `const _` would shadow
    -- in the same block (JS error). So drop the binder and emit the
    -- value as a bare expression statement.
    let stmt :=
      if name == "_" then doE (compileExpr value)
      else constV name (compileExpr value)
    stmt :: compileAsyncBody body
  | .ifE c t e =>
    [ifS (compileExpr c) (compileAsyncBody t) (compileAsyncBody e)]
  | terminal =>
    [retE (compileExpr terminal)]

def compileAsyncDefE (name : String) (params : List Param) (body : Ast.Expr) : Stmt :=
  let stmts := compileAsyncBody body
  let value := aarrow (params.map (·.name)) stmts
  constV name value

/-- A class declaration emits one `const Name = {};` so instances
    can attach themselves with `Name.Type = { ... }`. -/
def compileClass (name : String) : Block :=
  [constV name (obj [])]

/-- A record declaration emits a constructor function that takes the
    fields as a single object argument and returns it as-is. We pass
    the object straight through so callers can destructure fields,
    use spread, etc., without going through indirection. -/
def compileRecord (name : String) (_fields : List (String × String)) : Block :=
  /- `const Name = (rec) => rec;` -/
  [constV name (arrow ["__rec"] [retE (i "__rec")])]

/-- An instance writes the method dict at `ClassName.TypeName`. We
    use a raw assignment statement so the existing class binding is
    mutated in place. -/
def compileInstance (cls ty : String) (methods : List (String × Ast.Expr))
    : Block :=
  let methodObj : LeanTea.Js.Expr :=
    obj (methods.map fun (m, body) => (m, compileExpr body))
  [assign (dot (i cls) ty) methodObj]

/-- Lower `import { name as local, * } from "src"` into the
    framework's `importStar` / `importNamed` statements.
    A bindings list that contains a `*` falls back to a namespace
    import; otherwise we group the names. -/
def compileImport (bindings : List ImportBinding) (source : String) : Block :=
  match bindings.find? (·.name == "*") with
  | some b =>
    /- `import * as Foo from "src"` — namespaceAs holds the local
       alias if the user wrote `as Foo`; otherwise the alias *is*
       the namespace import's only name. -/
    let alias := b.namespaceAs.getD b.name
    [importStar alias source]
  | none =>
    let binds := bindings.map fun b => (b.name, b.namespaceAs.getD b.name)
    [importNamed binds source]

def compileTopDef : TopDef → Block
  | .defE name params _ body    => [compileDefE name params body]
  | .asyncDefE name params body => [compileAsyncDefE name params body]
  | .indE _ ctors               => compileInductive ctors
  | .externE name rawJs         => [constV name (raw rawJs)]
  | .classE name _              => compileClass name
  | .instE cls ty methods       => compileInstance cls ty methods
  | .importE bindings source    => compileImport bindings source
  | .includeE _                 => []  -- resolved before codegen runs
  | .recordE name fields        => compileRecord name fields
  | .exprE e                    => [doE (compileExpr e)]

/-- Top-level program → JS source. If a `main` def exists, we emit
    `console.log(main)` for a value-shaped main and `console.log(
    main())` for a function-shaped one. -/
def compileProgram (p : Program) : Block :=
  let defs : Block := p.toList.flatMap compileTopDef
  -- Look up the user's `main`. An async main always uses `await`
  -- on the call so `console.log` sees the resolved value.
  let mainDef? : Option (Bool × List Param) := p.findSome? fun
    | .defE       "main" params _ _ => some (false, params)
    | .asyncDefE  "main" params _   => some (true,  params)
    | _ => none
  let tail : Block := match mainDef? with
    | none                 => []
    | some (isAsync, params) =>
      let callExpr : LeanTea.Js.Expr :=
        if params.isEmpty && !isAsync then i "main" else call (i "main") []
      let target : LeanTea.Js.Expr :=
        if isAsync then await_ callExpr else callExpr
      if isAsync then
        -- Wrap in an IIFE so top-level await works in any node.
        let body : LeanTea.Js.Expr := aarrow [] [
          doE (mcall (i "console") "log" [target])
        ]
        [doE (call body [])]
      else
        [doE (mcall (i "console") "log" [target])]
  defs ++ tail

/-- The renderable JavaScript source string. -/
def compileToString (p : Program) : String :=
  (compileProgram p).render

/-- Like `compileToString` but runs `LeanJs.Check.check` first, so
    arity errors (calling a known binding with the wrong number of
    arguments) become an early, loud failure instead of a silent
    runtime bug. Use this from servers that read `.leanjs` at
    startup. -/
def compileChecked (p : Program) : Except String String := do
  let _ ← Check.check p
  return compileToString p

end LeanJs.Codegen
