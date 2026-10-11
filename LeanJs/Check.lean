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
  /-- constructor → (its inductive, arity) -/
  ctors   : List (String × String × Nat) := []
  /-- inductive → its constructors, in declaration order -/
  types   : List (String × List String) := []
  deriving Inhabited

private def addDef (t : ArityTable) (name : String) (arity : Nat) : ArityTable :=
  (name, arity) :: t

def buildTables (p : Program) : Tables := Id.run do
  let mut a : ArityTable := []
  let mut r : RecordTable := []
  let mut cs : List (String × String × Nat) := []
  let mut ts : List (String × List String) := []
  for d in p do
    if let .indE ty ctors := d then
      cs := ctors.map (fun c => (c.name, ty, c.arity)) ++ cs
      ts := (ty, ctors.map (·.name)) :: ts
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
  return { arity := a, records := r, ctors := cs, types := ts }

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

private def editDistance (a b : String) : Nat := Id.run do
  let a := a.toList.toArray
  let b := b.toList.toArray
  let mut prev : Array Nat := Array.range (b.size + 1)
  for i in [1:a.size + 1] do
    let mut cur : Array Nat := #[i]
    for j in [1:b.size + 1] do
      let cost := if a[i-1]! == b[j-1]! then 0 else 1
      cur := cur.push (min (min (prev[j]! + 1) (cur[j-1]! + 1)) (prev[j-1]! + cost))
    prev := cur
  return prev[b.size]!

/-- ` (did you mean `x`?)` for the closest candidate within distance 2. -/
def suggest (name : String) (cands : List String) : String :=
  let scored := cands.map (fun c => (editDistance name c, c))
  match scored.foldl (fun best x => match best with
      | none => some x
      | some b => if x.1 < b.1 then some x else some b) none with
  | some (d, c) => if d ≤ 2 && d < name.length then s!" (did you mean `{c}`?)" else ""
  | none => ""

/-! ## Patterns: constructors and exhaustiveness

Every constructor a pattern names must be declared and given the
right number of fields. A `match` must cover every value: for each
column of the pattern matrix, either some row is irrefutable there or
every constructor of the type is covered (recursively, through the
fields) — Maranget's usefulness check, without guards. String and
number literals never cover their (infinite) type on their own, so a
`match` over them needs a `_` / variable arm. A missing case is
reported as a pattern, e.g. ``missing case `Some(Rect(_, _))` ``. -/

private def irrefutable : Pattern → Bool
  | .wildcard | .var _ => true
  | _ => false

partial def checkPattern (t : Tables) : Pattern → Except String Unit
  | .ctor c args => do
    match t.ctors.find? (·.1 == c) with
    | none => throw s!"unknown constructor `{c}` in pattern{suggest c (t.ctors.map (·.1))}"
    | some (_, _, k) =>
      if args.length != k then
        throw s!"`{c}` has {k} field(s), the pattern gives {args.length}"
    args.forM (checkPattern t)
  | .var x =>
    if (t.ctors.find? (·.1 == x)).isSome then
      throw s!"`{x}` is a constructor: in patterns, constructor names must start with an uppercase letter"
    else pure ()
  | _ => pure ()

/-- A value vector (`n` columns) that no row of `rows` matches, if any. -/
partial def uncovered (t : Tables) (rows : List (List Pattern)) (n : Nat) : Option (List Pattern) :=
  if n == 0 then (if rows.isEmpty then some [] else none)
  else
    let col := rows.filterMap (·.head?)
    let names := col.filterMap fun | .ctor c _ => some c | _ => none
    let dflt := fun (_ : Unit) =>
      (uncovered t (rows.filterMap fun r =>
        if r.head?.all irrefutable then some r.tail else none) (n - 1)).map (.wildcard :: ·)
    match names.head? with
    | none => dflt ()
    | some c0 =>
      match t.ctors.find? (·.1 == c0) with
      | none => none  -- unknown constructor: reported by `checkPattern`
      | some (_, ty, _) =>
        let all := (t.types.find? (·.1 == ty)).map (·.2) |>.getD []
        all.findSome? fun c =>
          let k := ((t.ctors.find? (·.1 == c)).map (·.2.2)).getD 0
          let spec := rows.filterMap fun r =>
            match r.head? with
            | some (.ctor c' args) => if c' == c then some (args ++ r.tail) else none
            | some p => if irrefutable p then some (List.replicate k .wildcard ++ r.tail) else none
            | none => none
          (uncovered t spec (k + n - 1)).map fun w => Pattern.ctor c (w.take k) :: w.drop k

def checkMatch (t : Tables) (bs : List MatchBranch) : Except String Unit := do
  for b in bs do checkPattern t b.pat
  match uncovered t (bs.map (fun b => [b.pat])) 1 with
  | some (w :: _) => throw s!"`match` is missing a case: `{w.show}`"
  | _ => pure ()

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
    checkMatch t bs
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
  | .doBlock body      => checkExpr t body
  | .forE _ iter body  => do checkExpr t iter; checkExpr t body
  | .whileE c body     => do checkExpr t c; checkExpr t body
  | .letMutE _ v b     => do checkExpr t v; checkExpr t b
  | .updE b fs         => do checkExpr t b; fs.forM (fun (_, v) => checkExpr t v)

end

/-! ## Names in scope

Every variable must be bound somewhere: a top-level definition
(`def`, `extern`, constructor, `record`, `class`, an `import` alias),
a parameter, a `let` / `let mut`, a `for` variable, a `match` binder —
or be a standard JavaScript / browser global (`jsGlobals`). Anything
else is a typo or a missing `extern`, reported with the closest known
name.

Top-level objects written as literals (`def P := {x: 0, y: 0}`) also
fix their field set: `P.z` (read or assignment) is an error unless the
literal declares `z`. An empty literal (`def keys := {}`) is treated as
a dictionary and left alone, as is any name a local binding shadows. -/

/-- Standard globals a LeanJs program may use without an `extern`. -/
def jsGlobals : List String := [
  "Math", "Object", "Array", "JSON", "Number", "String", "Boolean", "Symbol", "BigInt",
  "Date", "RegExp", "Error", "TypeError", "RangeError", "Promise", "Map", "Set",
  "WeakMap", "WeakSet", "Reflect", "Proxy", "Intl", "console", "globalThis", "window",
  "document", "navigator", "location", "history", "localStorage", "sessionStorage",
  "fetch", "setTimeout", "clearTimeout", "setInterval", "clearInterval",
  "requestAnimationFrame", "cancelAnimationFrame", "performance", "alert", "confirm",
  "prompt", "Image", "Audio", "URL", "URLSearchParams", "Blob", "File", "FileReader",
  "FormData", "Headers", "Request", "Response", "WebSocket", "EventSource",
  "TextEncoder", "TextDecoder", "crypto", "atob", "btoa", "encodeURIComponent",
  "decodeURIComponent", "encodeURI", "decodeURI", "parseInt", "parseFloat", "isNaN",
  "isFinite", "NaN", "Infinity", "undefined", "structuredClone", "queueMicrotask",
  "ArrayBuffer", "DataView", "Uint8Array", "Int8Array", "Uint16Array", "Int16Array",
  "Uint32Array", "Int32Array", "Float32Array", "Float64Array", "Uint8ClampedArray",
  "BigInt64Array", "BigUint64Array", "Worker", "MutationObserver", "ResizeObserver",
  "IntersectionObserver", "CustomEvent", "Event", "KeyboardEvent", "MouseEvent",
  "HTMLElement", "Node", "process", "require", "module", "exports", "Buffer",
  "this", "arguments"]

/-- Names every top-level form introduces. -/
def topNames (p : Program) : List String := p.toList.flatMap fun
  | .defE n _ _ _ | .asyncDefE n _ _ | .externE n _ | .classE n _ => [n]
  | .recordE n _ => [n]
  | .indE _ ctors => ctors.map (·.name)
  | .importE binds _ => binds.map (fun b => b.namespaceAs.getD b.name)
  | _ => []

/-- Field sets of top-level object literals (non-empty ones). -/
def objectShapes (p : Program) : List (String × List String) := p.toList.filterMap fun
  | .defE n [] _ (.objE fs) => if fs.isEmpty then none else some (n, fs.map (·.1))
  | _ => none

structure Scope where
  globals : List String
  shapes  : List (String × List String)
  locals  : List String := []

private def Scope.bind (sc : Scope) (ns : List String) : Scope :=
  { sc with locals := ns ++ sc.locals }

private def Scope.knows (sc : Scope) (n : String) : Bool :=
  sc.locals.contains n || sc.globals.contains n || jsGlobals.contains n

private def checkField (sc : Scope) (obj : Ast.Expr) (f : String) : Except String Unit :=
  match obj with
  | .var n =>
    if sc.locals.contains n then pure () else
    match sc.shapes.find? (·.1 == n) with
    | some (_, fields) =>
      if fields.contains f then pure ()
      else throw s!"`{n}` has no field `{f}`{suggest f fields} — fields: {String.intercalate ", " fields}"
    | none => pure ()
  | _ => pure ()

partial def checkNames (sc : Scope) : Ast.Expr → Except String Unit
  | .var n =>
    if sc.knows n then pure ()
    else throw s!"unknown identifier `{n}`{suggest n (sc.locals ++ sc.globals)}"
  | .num _ | .numF _ | .str _ | .nullE | .boolE _ => pure ()
  | .app f args => do checkNames sc f; args.forM (checkNames sc)
  | .binop _ l r => do checkNames sc l; checkNames sc r
  | .letE n v b | .letMutE n v b => do checkNames sc v; checkNames (sc.bind [n]) b
  | .ifE c t e => do checkNames sc c; checkNames sc t; checkNames sc e
  | .fnE ps b => checkNames (sc.bind ps) b
  | .matchE scr bs => do
    checkNames sc scr
    for b in bs do checkNames (sc.bind b.pat.binders) b.body
  | .dotE e f | .optDotE e f => do checkField sc e f; checkNames sc e
  | .awaitE e | .unopE _ e | .doBlock e => checkNames sc e
  | .arrE xs => xs.forM (checkNames sc)
  | .objE fs | .recordLitE _ fs => fs.forM (fun (_, v) => checkNames sc v)
  | .idxE e i => do checkNames sc e; checkNames sc i
  | .newE c xs => do checkNames sc c; xs.forM (checkNames sc)
  | .assignE l r | .seqE l r => do checkNames sc l; checkNames sc r
  | .forE v it b => do checkNames sc it; checkNames (sc.bind [v]) b
  | .whileE c b => do checkNames sc c; checkNames sc b
  | .updE b fs => do
    -- updating a known-shape object can't add fields either
    for (f, _) in fs do checkField sc b f
    checkNames sc b
    fs.forM (fun (_, v) => checkNames sc v)

def checkTopNames (sc : Scope) : TopDef → Except String Unit
  | .defE _ ps _ body | .asyncDefE _ ps body => checkNames (sc.bind (ps.map (·.name))) body
  | .instE _ _ methods => methods.forM (fun (_, b) => checkNames sc b)
  | .exprE e => checkNames sc e
  | _ => pure ()

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
  let sc : Scope := { globals := topNames p, shapes := objectShapes p }
  for d in p do
    -- name the enclosing definition: the AST carries no positions
    let whereAt := match d with
      | .defE n _ _ _ | .asyncDefE n _ _ => s!"in `def {n}`: "
      | .instE c ty _ => s!"in `instance {c} {ty}`: "
      | .exprE _ => "in a top-level expression: "
      | _ => ""
    match checkTop t d *> checkTopNames sc d with
    | .ok () => pure ()
    | .error e => throw (whereAt ++ e)
  return p

end LeanJs.Check
