import LeanJs.Ast

/-! # LeanJs.Eval — interpret `Ast.Program` directly in Lean

Without this module the LSpec tests could only check "the compiled
JS, run through node, prints the expected output." That's a one-
sided check. For the Fay-style claim to hold — *same source, same
result on both sides* — we need a Lean-side interpreter that walks
the very same AST the codegen consumes, and we need every test to
agree on the answer.

What Eval covers
  * numbers / strings / vars
  * binops (+ - * /)
  * `let` / `if` / `fun` / function application
  * `match` on `inductive` values
  * `extern` (returns `Value.foreign`, can't be invoked from Lean
    — see `LeanJsSpec.lean` for how those tests are flagged)
  * `class` / `instance` — namespaced dictionaries identical to the
    JS-side lowering
  * `async def` (same as `def` here; no real concurrency)
  * `await` (no-op — the awaited value is the result)

The renderer at the bottom produces a `String` shaped like what
JavaScript's `console.log` prints for primitives, so the cross-
check can `==` against node's stdout byte-for-byte. -/

namespace LeanJs.Eval

open LeanJs.Ast

/-! ## Values + environments -/

inductive Value where
  | int     (n : Int) : Value
  | str     (s : String) : Value
  | bool    (b : Bool) : Value
  | null    : Value
  /-- Inductive constructor application: tag + fully-applied
      positional fields. -/
  | ctor    (tag : String) (fields : List Value) : Value
  /-- Reified constructor function — awaits more arguments. -/
  | ctorFn  (tag : String) (arity : Nat) (captured : List Value) : Value
  /-- Closure / function. `envEnc` is a list of `(name, jsonOf value)`
      — boxed via the embedded inductive below so we stay
      non-mutual. -/
  | closure (params : List String) (body : Ast.Expr)
            (env : List (String × Value)) : Value
  /-- Object — used for class containers and instance method tables. -/
  | dict    (kv : List (String × Value)) : Value
  /-- JS-shaped array. -/
  | arr     (xs : List Value) : Value
  /-- Result of an `extern js` declaration. We can't execute foreign
      JS from Lean, so any operation involving this throws. -/
  | foreign (rawJs : String) : Value
  deriving Inhabited

abbrev Env := List (String × Value)

private def envLookup : Env → String → Option Value
  | [], _ => none
  | (k, v) :: rest, name => if k == name then some v else envLookup rest name

private def envInsert (e : Env) (name : String) (v : Value) : Env :=
  (name, v) :: e

/-- Update an existing key in `Env` (used by `instance` declarations
    that mutate the class container). Adds if missing. -/
private def envUpdate (e : Env) (name : String) (v : Value) : Env :=
  match e with
  | [] => [(name, v)]
  | (k, w) :: rest =>
    if k == name then (k, v) :: rest else (k, w) :: envUpdate rest name v

/-! ## Truth + binop semantics

JS-flavoured: `0` is falsy, non-zero is truthy. Strings are truthy
unless empty. Matches what the codegen emits. -/

private def isTruthy : Value → Bool
  | .int n  => n != 0
  | .str s  => !s.isEmpty
  | .bool b => b
  | .null   => false
  | _       => true

/-- JS-flavoured truthiness check shared between `?:` and `&&`/`||`. -/
private def truthy : Value → Bool
  | .int n  => n != 0
  | .str s  => !s.isEmpty
  | .bool b => b
  | .null   => false
  | _       => true

private def doBinop (op : String) : Value → Value → Except String Value
  -- short-circuit logicals — match JS semantics (return the operand)
  | a, b =>
    match op with
    | "&&" => .ok (if truthy a then b else a)
    | "||" => .ok (if truthy a then a else b)
    | _ =>
      match a, b with
      | .int x, .int y =>
        match op with
        | "+" => .ok (.int (x + y))
        | "-" => .ok (.int (x - y))
        | "*" => .ok (.int (x * y))
        | "/" =>
          if y == 0 then .error "division by zero" else .ok (.int (x / y))
        | "%" =>
          if y == 0 then .error "mod by zero" else .ok (.int (x % y))
        | "==" | "===" => .ok (.bool (x == y))
        | "!=" | "!=="  => .ok (.bool (x != y))
        | "<"  => .ok (.bool (x < y))
        | "<=" => .ok (.bool (x ≤ y))
        | ">"  => .ok (.bool (x > y))
        | ">=" => .ok (.bool (x ≥ y))
        | _    => .error s!"unknown int op {op}"
      | .str x, .str y =>
        match op with
        | "+"           => .ok (.str (x ++ y))
        | "==" | "==="  => .ok (.bool (x == y))
        | "!=" | "!=="  => .ok (.bool (x != y))
        | _             => .error s!"strings: only + or ==/!="
      | .str x, .int y => .ok (.str (x ++ toString y))
      | .int x, .str y => .ok (.str (toString x ++ y))
      | .bool x, .bool y =>
        match op with
        | "==" | "===" => .ok (.bool (x == y))
        | "!=" | "!==" => .ok (.bool (x != y))
        | _    => .error s!"bools: only ==/!="
      | _, _ => .error s!"binop {op} on mismatched types"

/-! ## The interpreter -/

/-! `globals` carries the post-pass top-level env (all `def`s after
    `runProgram` finishes building them). It's read-only and threaded
    through every recursive call. Variable lookup checks the local
    `env` first (let-bindings, args, closure captures), then falls
    through to `globals`. This is the tie-the-knot for top-level
    recursion — `fact` looks up its own name at call time and finds
    itself in `globals`, even though the closure was created before
    the binding landed. -/
mutual

partial def eval (globals env : Env) : Ast.Expr → Except String Value
  | .num n   => .ok (.int n)
  | .numF _  => .error "float literal: unsupported in pure-Lean eval"
  | .str s   => .ok (.str s)
  | .var x   =>
    match envLookup env x with
    | some v => .ok v
    | none   =>
      match envLookup globals x with
      | some v => .ok v
      | none   => .error s!"unbound variable: {x}"
  | .binop op l r => do
    let lv ← eval globals env l
    let rv ← eval globals env r
    doBinop op lv rv
  | .letE name v body => do
    let vv ← eval globals env v
    eval globals (envInsert env name vv) body
  | .ifE c t e => do
    let cv ← eval globals env c
    if isTruthy cv then eval globals env t else eval globals env e
  | .fnE params body => .ok (.closure params body env)
  | .app f args => do
    let fv ← eval globals env f
    let argvs ← args.foldlM
      (fun acc a => do return acc ++ [(← eval globals env a)]) []
    apply globals fv argvs
  | .matchE scrut branches => do
    let sv ← eval globals env scrut
    matchOn globals env sv branches
  | .dotE e field => do
    let v ← eval globals env e
    match v with
    | .dict kv =>
      match kv.find? (·.1 == field) with
      | some (_, v) => .ok v
      | none        => .error s!"no field `{field}`"
    | .arr xs =>
      if field == "length" then .ok (.int (Int.ofNat xs.length))
      else .error s!"array has no field `{field}`"
    | _ => .error s!"`.{field}` on non-dict (foreign value or primitive)"
  | .awaitE e => eval globals env e
  | .arrE xs => do
    let vs ← xs.foldlM
      (fun acc e => do return acc ++ [(← eval globals env e)]) []
    .ok (.arr vs)
  | .objE kv => do
    let pairs ← kv.foldlM
      (fun acc (k, e) => do return acc ++ [(k, (← eval globals env e))]) []
    .ok (.dict pairs)
  | .idxE e ix => do
    let ev  ← eval globals env e
    let ixv ← eval globals env ix
    match ev, ixv with
    | .arr xs, .int i =>
      if i < 0 then .error "negative array index" else
      match xs.toArray[i.toNat]? with
      | some v => .ok v
      | none   => .error s!"array index out of bounds: {i}"
    | .dict kv, .str k =>
      match kv.find? (·.1 == k) with
      | some (_, v) => .ok v
      | none        => .error s!"no key `{k}`"
    | _, _ => .error "indexing: unsupported (need array+int or obj+str)"
  | .nullE => .error "null is JS-only"
  | .boolE b => .ok (.bool b)
  | .unopE op e => do
    let v ← eval globals env e
    match op, v with
    | "!", .bool b => .ok (.bool !b)
    | "-", .int n  => .ok (.int (-n))
    | _, _         => .error s!"unop {op} on incompatible value"
  | .newE _ _    => .error "new is JS-only"
  | .optDotE _ _ => .error "?. is JS-only"
  | .assignE _ _ => .error "<- assignment is JS-only"
  | .seqE a b    => do
    let _ ← eval globals env a
    eval globals env b
  | .recordLitE _ fields => do
    /- Treat a record literal like a regular dict — the type system
       gates field names elsewhere (in `Check`), so eval just needs
       to build a value-shaped record. -/
    let pairs ← fields.foldlM
      (fun acc (k, e) => do return acc ++ [(k, (← eval globals env e))]) []
    .ok (.dict pairs)

/-- Apply a callable value to a list of arguments. -/
partial def apply (globals : Env) : Value → List Value → Except String Value
  | .closure params body env, args =>
    if args.length != params.length then
      .error s!"arity {params.length}, got {args.length}"
    else
      let env' := (params.zip args).reverse ++ env
      eval globals env' body
  | .ctorFn tag arity prev, args =>
    let all := prev ++ args
    if all.length < arity then .ok (.ctorFn tag arity all)
    else if all.length == arity then .ok (.ctor tag all)
    else .error s!"constructor {tag} over-applied"
  | .foreign rawJs, _ =>
    .error s!"cannot invoke foreign value `{rawJs}` from Lean eval"
  | _, _ => .error "value isn't callable"

/-- Walk the branches looking for one whose pattern matches the
    scrutinee. Wildcards always match; ctor patterns require the
    scrutinee to be a `.ctor` value with the same tag; strLit
    patterns require a `.str` scrutinee with matching contents. -/
partial def matchOn (globals env : Env) (sv : Value)
    : List MatchBranch → Except String Value
  | [] => .error s!"no match arm fired"
  | b :: rest =>
    match b.pat with
    | .wildcard => eval globals env b.body
    | .strLit s' =>
      match sv with
      | .str v => if v == s' then eval globals env b.body
                  else matchOn globals env sv rest
      | _      => matchOn globals env sv rest
    | .ctor name args =>
      match sv with
      | .ctor tag fields =>
        if name != tag then matchOn globals env sv rest
        else if args.length != fields.length then
          .error s!"branch arity mismatch on {tag}"
        else
          let env' := (args.zip fields).reverse ++ env
          eval globals env' b.body
      | _ => matchOn globals env sv rest

end

/-! ## Top-level evaluation -/

/-- Apply one top-level declaration to the environment.

    Zero-arity definitions and instance methods are evaluated eagerly
    using the current partial top-level env as both `globals` and the
    local scope. Closure-shaped definitions store the partial env as
    their capture but rely on `runProgram` later passing the *full*
    env as `globals` at call time — that's the tie-the-knot for
    top-level recursion. -/
def applyTopDef (env : Env) : TopDef → Except String Env
  | .defE name params _ body =>
    let names := params.map (·.name)
    if names.isEmpty then
      match eval env env body with
      | .ok v    => .ok (envInsert env name v)
      | .error e => .error s!"in def {name}: {e}"
    else
      .ok (envInsert env name (.closure names body env))
  | .asyncDefE name params body =>
    let names := params.map (·.name)
    if names.isEmpty then
      match eval env env body with
      | .ok v    => .ok (envInsert env name v)
      | .error e => .error s!"in async def {name}: {e}"
    else
      .ok (envInsert env name (.closure names body env))
  | .indE _ ctors => do
    let mut env' := env
    for c in ctors do
      let v : Value :=
        if c.arity == 0 then .ctor c.name []
        else .ctorFn c.name c.arity []
      env' := envInsert env' c.name v
    return env'
  | .externE name rawJs =>
    .ok (envInsert env name (.foreign rawJs))
  | .classE name _ =>
    .ok (envInsert env name (.dict []))
  | .instE clsName tyName methods => do
    let mut methodKV : List (String × Value) := []
    for (m, body) in methods do
      methodKV := methodKV ++ [(m, ← eval env env body)]
    -- Read the existing class dict (created by `classE`) and add
    -- our type entry under it.
    match envLookup env clsName with
    | some (.dict existing) =>
      let newClass : Value := .dict (existing ++ [(tyName, .dict methodKV)])
      .ok (envUpdate env clsName newClass)
    | _ =>
      -- Auto-create the class container if the user forgot the
      -- `class` declaration.
      let newClass : Value := .dict [(tyName, .dict methodKV)]
      .ok (envInsert env clsName newClass)
  | .importE _ _ =>
    /- Imports are a JS-runtime concept (module loading). The Lean
       evaluator has no notion of external modules — skip them. -/
    .ok env
  | .includeE _ =>
    /- Resolved before this point by `LeanJs.Includes.resolve`. If
       one survives to eval, treat it as a no-op rather than crash. -/
    .ok env
  | .recordE _ _ =>
    /- A record declaration is a shape-only construct (no runtime
       value). Field validation happens in `Check`; here we just
       drop the declaration. -/
    .ok env
  | .exprE _ =>
    /- Top-level expression statements run for their side effect.
       The eval interpreter is pure, so we have nothing to do here.
       (The compiled JS pipeline emits them as statements.) -/
    .ok env

/-- Walk the program, then evaluate `main` (or invoke it if it's a
    function). Returns the value `console.log` would have printed. -/
def runProgram (p : Program) : Except String Value := do
  let mut env : Env := []
  for d in p do
    env ← applyTopDef env d
  match envLookup env "main" with
  | none => .error "no `main` defined"
  | some v =>
    match v with
    | .closure _ _ _ =>
      -- Function-shaped main: call with no args (matches the JS
      -- emitter's `console.log(main())`). Pass the full top-level
      -- env as `globals` so any closure body reached from main can
      -- resolve siblings — closing the tie-the-knot loop.
      apply env v []
    | other => .ok other

/-! ## Rendering — match `console.log` for primitives -/

partial def render : Value → String
  | .int n      => toString n
  | .str s      => s
  | .bool b     => if b then "true" else "false"
  | .null       => "null"
  | .ctor tag fields =>
    let inside := String.intercalate ", " (fields.map render)
    s!"{tag}({inside})"
  | .ctorFn tag _ _ => s!"[ctor:{tag}]"
  | .closure _ _ _  => "[Function]"
  | .dict _         => "[Object]"
  | .arr xs         =>
    "[" ++ String.intercalate "," (xs.map render) ++ "]"
  | .foreign raw    => s!"[foreign:{raw}]"

end LeanJs.Eval
