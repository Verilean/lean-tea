import LeanJs.Ast

/-! # LeanJs.Check — light-weight arity check, runs between parse and codegen

We don't have a type-checker. But we can still catch the cheapest
class of mistakes — calling a known binding with the wrong number of
arguments — without paying for full inference. This pass:

1. Walks the program once to collect a name → arity table, drawing
   from:
   * `def f(a, b) := …`               (arity 2)
   * `async def g(x) := …`             (arity 1)
   * `inductive T where | Ctor(a, b)` (arity 2)
   * `extern js "(a, b) => …" name`   (arity 2, parsed from the JS source)
2. Walks every expression and, when it sees `f(args…)` where `f` is
   a bare variable reference to a known name, verifies the arg count.

Method calls (`obj.method(args)`), constructor calls (`new C(args)`),
and applications of higher-order results are *not* checked — their
true arity isn't visible to us. Wrong-arity calls there will still
fail at JS runtime, which is fine.

`Codegen.compileToString` invokes this pass and fails loudly on a
mismatch, so the offending source never reaches the browser. -/

namespace LeanJs.Check

open LeanJs.Ast

/-! ## Arity extraction from raw FFI source -/

/-- Try to read the arity out of an `extern js "…"` body. We only
    handle the common `(a, b, c) => …` shape — anything else (a
    bare `console.log`, destructuring patterns, default values)
    falls back to `none` and the call site is left unchecked.

    The check is permissive on purpose: a missing arity is *not* an
    error, just an "we couldn't tell, trust the user." -/
private partial def scanArity (cs : List Char) (depth : Nat) (commas : Nat)
    (sawAny : Bool) : Option Nat :=
  match cs with
  | []          => none
  | ')' :: _    =>
    if depth == 0 then
      if sawAny then some (commas + 1) else some 0
    else
      scanArity cs.tail (depth - 1) commas true
  | '(' :: rest => scanArity rest (depth + 1) commas true
  | ',' :: rest =>
    if depth == 0 then scanArity rest 0 (commas + 1) true
    else scanArity rest depth commas true
  | c :: rest   =>
    scanArity rest depth commas (sawAny || !c.isWhitespace)

def arityOfRawJs (raw : String) : Option Nat :=
  let s := raw.trimAscii.toString
  if !s.startsWith "(" then
    /- `x => …` (single-param arrow without parens). If we see an
       ident followed by `=>`, that's arity 1. -/
    let rest := (s.dropWhile (fun c => c.isAlpha || c == '_')).toString
    let post := (rest.dropWhile (·.isWhitespace)).toString
    if post.startsWith "=>" && !s.isEmpty && (s.front.isAlpha || s.front == '_')
    then some 1
    else none
  else
    /- `(a, b, c) => …` — find the matching `)` at depth 1, count
       top-level commas in between. -/
    let body := (s.drop 1).toString
    scanArity body.toList 0 0 false

/-! ## Building the name → arity table -/

abbrev ArityTable := List (String × Nat)
/-- Maps a record name to its declared field-name set. -/
abbrev RecordTable := List (String × List String)

structure Tables where
  arity   : ArityTable := []
  records : RecordTable := []
  deriving Inhabited

private def addDef (t : ArityTable) (name : String) (arity : Nat) : ArityTable :=
  (name, arity) :: t

def buildTables (p : Program) : Tables := Id.run do
  let mut a : ArityTable := []
  let mut r : RecordTable := []
  for d in p do
    match d with
    | .defE name params _ _      => a := addDef a name params.length
    | .asyncDefE name params _   => a := addDef a name params.length
    | .indE _ ctors              =>
      for c in ctors do a := addDef a c.name c.arity
    | .externE name raw          =>
      match arityOfRawJs raw with
      | some n => a := addDef a name n
      | none   => pure ()
    | .recordE name fields       =>
      /- A record name is callable as `Name({…})` — a unary
         constructor for arity purposes. -/
      a := addDef a name 1
      r := (name, fields.map (·.1)) :: r
    | .classE _ _                => pure ()
    | .instE _ _ _               => pure ()
    | .importE _ _               => pure ()
    | .includeE _                => pure ()
    | .exprE _                   => pure ()
  return { arity := a, records := r }

/-- Back-compat alias for callers that still want just the arity table. -/
def buildTable (p : Program) : ArityTable := (buildTables p).arity

/-! ## Walk + check -/

private partial def lookupArity (t : ArityTable) (n : String) : Option Nat :=
  t.find? (·.1 == n) |>.map (·.2)

private def lookupRecord (r : RecordTable) (n : String) : Option (List String) :=
  r.find? (·.1 == n) |>.map (·.2)

/-- Verify a `Cls { f1: v1, … }` construction against the declared
    field set. We check three things: declared set ≡ supplied set
    (no extras, no missing). Errors are specific so the user knows
    what to fix. -/
private def checkRecordLit (decl : List String) (supplied : List String)
    (cls : String) : Except String Unit := do
  /- Missing fields. -/
  for d in decl do
    if !supplied.contains d then
      throw s!"`{cls}`: missing field `{d}`"
  /- Extra / typo'd fields. -/
  for s in supplied do
    if !decl.contains s then
      throw s!"`{cls}`: unknown field `{s}` (expected: {String.intercalate ", " decl})"

mutual

partial def checkExpr (t : Tables) : Ast.Expr → Except String Unit
  | .app f args => do
    /- Direct `name(args)` — verify if we know name's arity. -/
    match f with
    | .var n =>
      match lookupArity t.arity n with
      | some k =>
        if args.length != k then
          throw s!"`{n}` expects {k} argument(s), got {args.length}"
      | none => pure ()
    | _ => pure ()
    /- Recurse into callee and args regardless. -/
    checkExpr t f
    for a in args do checkExpr t a
  | .num _ | .numF _ | .str _ | .var _ | .nullE | .boolE _ => pure ()
  | .binop _ l r       => do checkExpr t l; checkExpr t r
  | .letE _ v b        => do checkExpr t v; checkExpr t b
  | .ifE c th el       => do checkExpr t c; checkExpr t th; checkExpr t el
  | .fnE _ b           => checkExpr t b
  | .matchE s bs       => do
    checkExpr t s
    for b in bs do checkExpr t b.body
  | .dotE e _          => checkExpr t e
  | .awaitE e          => checkExpr t e
  | .arrE xs           => xs.forM (checkExpr t)
  | .objE fs           => fs.forM (fun (_, v) => checkExpr t v)
  | .idxE e ix         => do checkExpr t e; checkExpr t ix
  | .unopE _ e         => checkExpr t e
  | .newE c args       => do checkExpr t c; args.forM (checkExpr t)
  | .optDotE e _       => checkExpr t e
  | .assignE l r       => do checkExpr t l; checkExpr t r
  | .seqE a b          => do checkExpr t a; checkExpr t b
  | .recordLitE cls fields => do
    /- Lookup the record's declared field set. Unknown record names
       error out so the user knows they're constructing something
       that wasn't declared. -/
    match lookupRecord t.records cls with
    | some decl => checkRecordLit decl (fields.map (·.1)) cls
    | none      => throw s!"`{cls}`: not declared as a `record`"
    /- Still recurse into each field value. -/
    fields.forM (fun (_, v) => checkExpr t v)

end

def checkTop (t : Tables) : TopDef → Except String Unit
  | .defE _ _ _ body    => checkExpr t body
  | .asyncDefE _ _ body => checkExpr t body
  | .indE _ _           => pure ()
  | .externE _ _        => pure ()
  | .classE _ _         => pure ()
  | .instE _ _ methods  =>
    methods.forM (fun (_, body) => checkExpr t body)
  | .importE _ _        => pure ()
  | .includeE _         => pure ()
  | .recordE _ _        => pure ()
  | .exprE e            => checkExpr t e

/-- Verify every direct application against the program's arity
    table, plus every record construction against its declared shape.
    Returns the program unchanged on success; the first mismatch
    becomes the error. -/
def check (p : Program) : Except String Program := do
  let t := buildTables p
  for d in p do
    checkTop t d
  return p

end LeanJs.Check
