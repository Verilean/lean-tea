import LeanJs.Ast

/-! # LeanJs.LeanEmit — render the pure subset as actual Lean source

When the source uses no FFI (no `extern js`), each `Ast.TopDef`
maps to a perfectly normal `def` that the real Lean compiler can
elaborate. This module produces that text. Together with
`LeanJs.Codegen` (the JS target) it makes the pure subset honestly
bilingual: the same source compiles to JS *and* to Lean, and you can
run a program through both pipelines and compare the output.

What it covers
  * `def f (x : T) (y : T) : R := body` with optional type
    annotations (defaulting to `Int` when absent)
  * arithmetic, comparison, logical, modulo
  * `let` / `if` / `match` / lambdas
  * sum types as `inductive`
  * arrays as `List`, indexing as `List.get!`
  * objects, async/await, FFI, classes — **not** supported on the
    Lean side. The emitter returns an error explaining why.

The companion test (`leanjs_spec`) feeds emitted source through
`lean --run` and asserts it prints the same value as the node
pipeline. -/

namespace LeanJs.LeanEmit

open LeanJs.Ast

/-- Default type when the user didn't annotate. We use `Int` so
    arithmetic round-trips with the JS semantics on the same
    inputs. -/
private def defaultType : String := "Int"

/-! ## Expressions -/

partial def emitExpr : Ast.Expr → String
  | .num n        => if n < 0 then s!"({n})" else toString n
  | .numF lit     => s!"({lit} : Float)"
  | .str s        => "\"" ++ escape s ++ "\""
  | .var x        => x
  | .binop op l r =>
    -- Lean uses the same arithmetic / comparison glyphs we do; ==
    -- needs to become BEq's `==` which works on most types.
    s!"({emitExpr l} {op} {emitExpr r})"
  | .letE n v b   => s!"(let {n} := {emitExpr v}; {emitExpr b})"
  | .ifE c t e    => s!"(if {emitExpr c} then {emitExpr t} else {emitExpr e})"
  | .fnE ps body  =>
    let bound := String.intercalate " " ps
    s!"(fun {bound} => {emitExpr body})"
  | .app f xs     =>
    let args := String.intercalate " " (xs.map fun a => s!"({emitExpr a})")
    if xs.isEmpty then emitExpr f else s!"({emitExpr f} {args})"
  | .matchE scr branches =>
    let arms := branches.map fun b =>
      let head := match b.pat with
        | .wildcard => " | _"
        | .strLit s' => s!" | \"{escape s'}\""
        | .ctor name args =>
          let binders := if args.isEmpty then ""
                         else " " ++ String.intercalate " " args
          s!" | .{name}{binders}"
      s!"{head} => {emitExpr b.body}"
    s!"(match {emitExpr scr} with{String.join arms})"
  | .dotE e f     => s!"({emitExpr e}).{f}"
  | .awaitE e     => emitExpr e            -- no async on the Lean side
  | .arrE xs      =>
    "[" ++ String.intercalate ", " (xs.map emitExpr) ++ "]"
  | .objE _       => "(panic! \"object literal: unsupported in LeanEmit\")"
  | .idxE e ix    => s!"(({emitExpr e}).get! {emitExpr ix})"
  | .nullE        => "(panic! \"null: unsupported in LeanEmit\")"
  | .boolE b      => if b then "true" else "false"
  | .unopE op e   => s!"({op}({emitExpr e}))"
  | .newE _ _     => "(panic! \"new: JS-only construct\")"
  | .optDotE _ _  => "(panic! \"?.: JS-only construct\")"
  | .assignE _ _  => "(panic! \"<- assignment: JS-only construct\")"
  | .seqE _ b     => emitExpr b  -- comma expr — just keep the result
  | .recordLitE cls fields =>
    /- `({ k1 := v1, k2 := v2 } : Cls)` — Lean record syntax with the
       expected-type annotation pinned outside the braces. -/
    let entries := String.intercalate ", "
      (fields.map (fun (k, v) => s!"{k} := {emitExpr v}"))
    s!"(\{ {entries} } : {cls})"
where
  escape (s : String) : String :=
    s.replace "\\" "\\\\" |>.replace "\"" "\\\""

/-! ## Top-level definitions -/

private def emitParam (p : Param) : String :=
  let t := p.type?.getD defaultType
  s!"({p.name} : {t})"

/-- Render one `def`. Optional return-type annotation flows through.
    Async defs are emitted as plain `def` (no IO on the Lean side
    for this pure-subset target). -/
def emitDef (name : String) (params : List Param) (retType? : Option String)
    (body : Ast.Expr) : String :=
  let paramText := String.intercalate " " (params.map emitParam)
  let head := if params.isEmpty then s!"def {name}"
              else s!"def {name} {paramText}"
  let retText := retType?.map (s!" : {·}") |>.getD ""
  let tail := if params.length > 0 ∨ retType?.isSome then "" else ""
  s!"partial {head}{retText} :=\n  {emitExpr body}{tail}"

/-- Render an inductive declaration. We list every constructor with
    `arity` arrow-shaped `Int` parameters so the type is concrete
    enough for the Lean elaborator. -/
def emitInductive (name : String) (ctors : List CtorDecl) : String :=
  let header := s!"inductive {name} where"
  let arms := ctors.map fun c =>
    if c.arity == 0 then s!"  | {c.name}"
    else
      let fields := (List.range c.arity).map fun _ => "Int"
      let body := String.intercalate " → " fields
      s!"  | {c.name} ({body})"
  String.intercalate "\n" (header :: arms)
  ++ s!"\n  deriving Inhabited, Repr"

/-- A top-level def → the emitted Lean source for it, or an error
    if the construct can't be expressed on the Lean side. -/
def emitTopDef : TopDef → Except String String
  | .defE name params ret body =>
    .ok (emitDef name params ret body)
  | .asyncDefE name params body =>
    .ok (emitDef name params none body)
  | .indE name ctors =>
    .ok (emitInductive name ctors)
  | .externE _ _ =>
    .error "extern js — no Lean equivalent (program is JS-only)"
  | .classE _ _ =>
    .error "class — not supported in LeanEmit"
  | .instE _ _ _ =>
    .error "instance — not supported in LeanEmit"
  | .importE _ _ =>
    .error "import — JS-only construct"
  | .includeE _ =>
    /- `include` is resolved before LeanEmit runs; nothing to emit. -/
    .ok ""
  | .recordE name fields =>
    /- `structure Name where f1 : T1, f2 : T2, … deriving Inhabited, Repr` -/
    let fieldLines := fields.map (fun (k, t) => s!"  {k} : {t}")
    let body := String.intercalate "\n" fieldLines
    .ok (s!"structure {name} where\n{body}\n  deriving Inhabited, Repr")
  | .exprE _ =>
    .error "top-level expression statement — JS-only construct"

/-! ## Whole-program emit -/

/-- Render every declaration in order, prepending the obvious
    `import` lines so the file stands alone. Returns the source on
    success; the first construct LeanEmit can't handle stops the
    emission with an error. -/
def emitProgram (p : Program) : Except String String := do
  let mut out : Array String := #[
    "-- AUTO-GENERATED by LeanJs.LeanEmit; do not edit by hand."
  ]
  for d in p do
    let txt ← emitTopDef d
    out := out.push txt
  return String.intercalate "\n\n" out.toList

/-! ## `main`-wrapping helper

`lean --run` calls the file's `main : IO Unit`. The user's program
already uses `main` to name its top-level expression — so the
wrapper renames every occurrence of the bare identifier `main` in
the emitted source to `_userMain`, then adds a real `def main :
IO Unit` that prints `_userMain` via `IO.println`. That matches the
node pipeline's `console.log(main)` shape. -/

def wrapForLeanRun (programSource : String) : String :=
  let renamed := programSource
    |>.replace "def main " "def _userMain "
    |>.replace "def main :" "def _userMain :"
    |>.replace " main\n" " _userMain\n"
    |>.replace "(main "  "(_userMain "
  renamed
    ++ "\n\ndef main : IO Unit := IO.println (toString _userMain)"

end LeanJs.LeanEmit
