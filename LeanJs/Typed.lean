import LeanJs.Ast

/-! # LeanJs.Typed — type-check pure LeanJs rules with Lean

LeanJs has no type system of its own. For code written as pure rules
(game physics, a combat step: functions from a state to a new state)
this pass gives it one by **translating the program to Lean and
letting Lean's elaborator check it**. Nothing is run; the JavaScript
the browser executes still comes from `LeanJs.Codegen`.

The translation is direct, so the LeanJs source reads as the Lean it
becomes:

  * `record` → `structure`, `inductive` → `inductive` (constructor
    fields carry their types: `Damaged(id : Float, amount : Float)`)
  * `def f(x : Float, p : Player) : Player := …` → the same `def`;
    parameters and the results of integer-valued constants need their
    types (`def HERO_HP : Float := 100`); the rest is inferred
  * `do` blocks → `Id.run do` with `let mut`, `for`, `while`, `if` and
    `match` statements, exactly as Lean writes them
  * `[a, b]` → `#[a, b]` (an `Array`), `xs[i]` → `xs[i]!`,
    `{x: 1}` → `{ x := 1 }` (a structure whose type Lean knows from
    context), `Rec {x: 1}` → `({ x := 1 } : Rec)`
  * `Math.*` and the array methods the rules use (`map`, `filter`,
    `concat`, `includes`, `length`) come from `prelude`

What has no type is refused with a message instead of guessed at:
`null`, `extern js`, `new`, `?.`, assignment to a field, `async`.
Write `Option`-like values as an `inductive` (`| Keep | Face(angle :
Float)`) instead of `null`. -/

namespace LeanJs.Typed

open LeanJs.Ast

/-- Lean's keywords; a LeanJs name that is one is written `«name»`. -/
def leanKeywords : List String := [
  "at", "by", "from", "fun", "have", "show", "open", "end", "in", "do", "then", "else",
  "match", "with", "for", "mut", "return", "where", "deriving", "instance", "structure",
  "def", "let", "if", "unless", "Type", "Prop", "Sort", "macro", "syntax", "namespace",
  "section", "variable", "universe", "theorem", "example", "abbrev", "local", "private",
  "protected", "noncomputable", "partial", "unsafe", "mutual", "inductive", "class",
  "extends", "calc", "suffices", "obtain", "try", "catch", "finally", "break", "continue",
  "nomatch", "nofun", "notation", "infix", "import", "export", "attribute", "include",
  "omit", "opaque", "axiom", "using", "this", "termination_by", "decreasing_by"]

def name (n : String) : String := if leanKeywords.contains n then s!"«{n}»" else n

/-- What the emitter needs to know about the program. -/
structure Ctx where
  /-- constructor → its inductive -/
  ctors : List (String × String) := []

def Ctx.ofProgram (p : Program) : Ctx :=
  { ctors := p.toList.flatMap fun
      | .indE ty cs => cs.map (·.name, ty)
      | _ => [] }

/-- Shared definitions every checked program sees: `Math`, and the
    JavaScript array methods on `Array`. -/
def prelude : String := "
def Array.concat (a b : Array α) : Array α := a ++ b
def Array.includes [BEq α] (a : Array α) (v : α) : Bool := a.contains v
def Array.length (a : Array α) : Nat := a.size
namespace Math
def PI : Float := 3.141592653589793
def sin (x : Float) : Float := Float.sin x
def cos (x : Float) : Float := Float.cos x
def atan2 (y x : Float) : Float := Float.atan2 y x
def floor (x : Float) : Float := Float.floor x
def abs (x : Float) : Float := Float.abs x
def sqrt (x : Float) : Float := Float.sqrt x
def min (a b : Float) : Float := if a ≤ b then a else b
def max (a b : Float) : Float := if a ≤ b then b else a
def hypot (a b : Float) (c : Float := 0) : Float := Float.sqrt (a * a + b * b + c * c)
end Math
"

private def pad (n : Nat) : String := String.ofList (List.replicate n ' ')

private def quote (s : String) : String :=
  "\"" ++ (s.replace "\\" "\\\\" |>.replace "\"" "\\\"" |>.replace "\n" "\\n") ++ "\""

partial def emitPat (cx : Ctx) : Pattern → String
  | .wildcard => "_"
  | .var x => name x
  | .strLit s => quote s
  | .numLit n => if n < 0 then s!"({n})" else toString n
  | .ctor c [] => s!".{c}"
  | .ctor c args => s!"(.{c} " ++ String.intercalate " " (args.map (emitPat cx)) ++ ")"

mutual

/-- An expression. `ind` is the indentation a `do` block inside it
    starts its statements at. -/
partial def emitExpr (cx : Ctx) (ind : Nat) : Ast.Expr → Except String String
  | .num n => .ok (if n < 0 then s!"({n})" else toString n)
  | .numF lit => .ok s!"({lit} : Float)"
  | .str s => .ok (quote s)
  | .boolE b => .ok (if b then "true" else "false")
  | .var x =>
    match cx.ctors.find? (·.1 == x) with
    | some (_, ty) => .ok s!"{ty}.{x}"
    | none => .ok (name x)
  | .app f [] => emitExpr cx ind f
  | .app f args => do
    let f' ← emitExpr cx ind f
    let as ← args.mapM (emitExpr cx ind)
    .ok (s!"({f'} " ++ String.intercalate " " (as.map (s!"({·})")) ++ ")")
  | .binop op l r => do
    .ok s!"({← emitExpr cx ind l} {op} {← emitExpr cx ind r})"
  | .unopE op e => do .ok s!"({op}{← emitExpr cx ind e})"
  | .letE n v b | .letMutE n v b => do
    -- one `let` per line, so an error points at the line it is about
    .ok s!"(let {name n} := {← emitExpr cx ind v}\n{pad ind}{← emitExpr cx ind b})"
  | .ifE _ _ .nullE => .error "an `if` without `else` has no value here"
  | .ifE c t e => do
    .ok s!"(if {← emitExpr cx ind c} then {← emitExpr cx ind t} else {← emitExpr cx ind e})"
  | .fnE ps b => do
    let ps' := if ps.isEmpty then "()" else String.intercalate " " (ps.map name)
    .ok s!"(fun {ps'} => {← emitExpr cx ind b})"
  | .matchE scr bs => do
    let arms ← bs.mapM fun b => do .ok s!" | {emitPat cx b.pat} => {← emitExpr cx ind b.body}"
    .ok s!"(match {← emitExpr cx ind scr} with{String.join arms})"
  | .dotE (.var "Math") f => .ok s!"Math.{f}"
  | .dotE e f => do .ok s!"({← emitExpr cx ind e}).{name f}"
  | .idxE e i => do .ok s!"({← emitExpr cx ind e})[{← emitExpr cx ind i}]!"
  | .arrE xs => do
    let xs' ← xs.mapM (emitExpr cx ind)
    .ok ("#[" ++ String.intercalate ", " xs' ++ "]")
  | .objE fs => do .ok s!"\{ {← fields cx ind fs} }"
  | .recordLitE cls fs => do .ok s!"(\{ {← fields cx ind fs} } : {cls})"
  | .updE b fs => do
    -- the copy has the type of what it copies; saying so up front keeps
    -- Lean from guessing (it may otherwise read `h.down > 0` as `Nat`)
    let b' ← emitExpr cx ind b
    .ok s!"(\{ {b'} with {← fields cx ind fs} } : type_of% {b'})"
  | .doBlock body => do
    let lines ← emitStmts cx (ind + 2) true body
    .ok ("(Id.run do\n" ++ String.intercalate "\n" lines ++ ")")
  | .seqE _ _ | .assignE _ _ | .forE _ _ _ | .whileE _ _ =>
    .error "a statement (`;`, `:=`, `for`, `while`) outside a `do` block"
  | .nullE => .error "`null` has no type: use an `inductive` with an empty case"
  | .newE _ _ => .error "`new` is JavaScript only"
  | .optDotE _ _ => .error "`?.` is JavaScript only"
  | .awaitE _ => .error "`await` is JavaScript only"

partial def fields (cx : Ctx) (ind : Nat) (fs : List (String × Ast.Expr)) : Except String String := do
  let kvs ← fs.mapM fun (k, v) => do .ok s!"{name k} := {← emitExpr cx ind v}"
  .ok (String.intercalate ", " kvs)

/-- The statements of a `do` block, one per line at `ind`. `tail`: the
    last expression is the block's value (`pure …`); otherwise the
    block only runs for its effect on `let mut` locals. -/
partial def emitStmts (cx : Ctx) (ind : Nat) (tail : Bool) : Ast.Expr → Except String (List String)
  | .seqE a b => do .ok ((← emitStmts cx ind false a) ++ (← emitStmts cx ind tail b))
  | .letE n v b => do
    .ok ((pad ind ++ s!"let {name n} := {← emitExpr cx ind v}") :: (← emitStmts cx ind tail b))
  | .letMutE n v b => do
    .ok ((pad ind ++ s!"let mut {name n} := {← emitExpr cx ind v}") :: (← emitStmts cx ind tail b))
  | .doBlock b => emitStmts cx ind tail b
  | e =>
    if tail then do .ok [pad ind ++ s!"pure {← emitExpr cx ind e}"]
    else emitStmt cx ind e

/-- One statement run for its effect. -/
partial def emitStmt (cx : Ctx) (ind : Nat) : Ast.Expr → Except String (List String)
  | .assignE (.var n) r => do .ok [pad ind ++ s!"{name n} := {← emitExpr cx ind r}"]
  | .assignE _ _ => .error "assignment to a field or an element: build a new value with `{ x with … }`"
  | .forE v it b => do
    .ok ((pad ind ++ s!"for {name v} in {← emitExpr cx ind it} do") :: (← block cx (ind + 2) b))
  | .whileE c b => do
    .ok ((pad ind ++ s!"while {← emitExpr cx ind c} do") :: (← block cx (ind + 2) b))
  | .ifE c t e => do
    let head := (pad ind ++ s!"if {← emitExpr cx ind c} then") :: (← block cx (ind + 2) t)
    match e with
    | .nullE => .ok head
    | _ => .ok (head ++ [pad ind ++ "else"] ++ (← block cx (ind + 2) e))
  | .matchE scr bs => do
    let mut out := [pad ind ++ s!"match {← emitExpr cx ind scr} with"]
    for b in bs do
      out := out ++ [pad ind ++ s!"| {emitPat cx b.pat} =>"] ++ (← block cx (ind + 2) b.body)
    .ok out
  | .nullE => .ok [pad ind ++ "pure ()"]
  | e@(.seqE _ _) | e@(.letE _ _ _) | e@(.letMutE _ _ _) | e@(.doBlock _) => emitStmts cx ind false e
  | e => do .ok [pad ind ++ s!"let _ := {← emitExpr cx ind e}"]

/-- A nested statement block; never empty. -/
partial def block (cx : Ctx) (ind : Nat) (e : Ast.Expr) : Except String (List String) := do
  let ls ← emitStmts cx ind false e
  -- a block whose last line binds a name still needs an action
  let last := (ls.getLast?.getD "").trimAsciiStart.toString
  if ls.isEmpty || last.startsWith "let " then .ok (ls ++ [pad ind ++ "pure ()"]) else .ok ls

end

private def emitParam (p : Param) : Except String String :=
  match p.type? with
  | some t => .ok s!"({name p.name} : {t})"
  | none => .error s!"parameter `{p.name}` needs a type"

/-- Does `e` mention `n` (for `partial` on recursive definitions)? -/
partial def mentions (n : String) : Ast.Expr → Bool
  | .var x => x == n
  | .app f xs => mentions n f || xs.any (mentions n)
  | .binop _ l r | .assignE l r | .seqE l r | .idxE l r => mentions n l || mentions n r
  | .letE _ v b | .letMutE _ v b => mentions n v || mentions n b
  | .ifE c t e => mentions n c || mentions n t || mentions n e
  | .fnE _ b | .dotE b _ | .optDotE b _ | .awaitE b | .unopE _ b | .doBlock b => mentions n b
  | .matchE s bs => mentions n s || bs.any (mentions n ·.body)
  | .arrE xs => xs.any (mentions n)
  | .objE fs | .recordLitE _ fs => fs.any (mentions n ·.2)
  | .newE c xs => mentions n c || xs.any (mentions n)
  | .forE _ i b => mentions n i || mentions n b
  | .whileE c b => mentions n c || mentions n b
  | .updE b fs => mentions n b || fs.any (mentions n ·.2)
  | _ => false

def emitTop (cx : Ctx) : TopDef → Except String String
  | .defE n ps ret body => do
    -- `def f() := …` is a value on the Lean side: no `Unit` argument
    let (ps, body) := match ps, body with
      | [], .fnE [] b => ([], b)
      | _, _ => (ps, body)
    let ps' ← ps.mapM emitParam
    let head := String.intercalate " " (s!"def {name n}" :: ps')
    let retText := (ret.map (s!" : {·}")).getD ""
    let kw := if mentions n body then "partial " else ""
    let rhs ← match body with
      | .doBlock b => do .ok ("Id.run do\n" ++ String.intercalate "\n" (← emitStmts cx 2 true b))
      | _ => emitExpr cx 2 body
    .ok s!"{kw}{head}{retText} :=\n  {rhs}"
  | .indE n cs =>
    let arms := cs.map fun c =>
      let fs := c.fields.map fun f => s!" ({name f.name} : {f.type?.getD "_"})"
      s!"  | {c.name}{String.join fs}"
    -- no `BEq`: compare constructors with `match` (JavaScript's `==` on
    -- them compares objects, not values)
    .ok (String.intercalate "\n" (s!"inductive {n} where" :: arms) ++ "\n  deriving Inhabited")
  | .recordE n fs =>
    .ok (String.intercalate "\n" (s!"structure {n} where" :: fs.map (fun (k, t) => s!"  {name k} : {t}"))
      ++ "\n  deriving Inhabited")
  | .includeE _ => .ok ""
  | .externE n _ => .error s!"`extern js {n}`: JavaScript has no type to check"
  | .asyncDefE n _ _ => .error s!"`async def {n}`: not a pure rule"
  | .classE n _ | .instE n _ _ => .error s!"`{n}`: classes are not supported here"
  | .importE _ _ => .error "`import` is JavaScript only"
  | .exprE _ => .error "a top-level expression is not a definition"

/-- The program as one Lean file: the prelude, then every definition
    inside `namespace LeanJsTyped`. -/
def emitProgram (p : Program) : Except String String := do
  let cx := Ctx.ofProgram p
  let mut out : Array String := #[prelude, "namespace LeanJsTyped"]
  for d in p do
    let what := match d with
      | .defE n _ _ _ | .indE n _ | .recordE n _ => s!"`{n}`: "
      | _ => ""
    match emitTop cx d with
    | .ok "" => pure ()
    | .ok s => out := out.push s
    | .error e => throw (what ++ e)
  return String.intercalate "\n\n" (out.push "end LeanJsTyped").toList ++ "\n"

/-- Lean's complaints, each error line followed by the emitted line it
    points at (whose names are the LeanJs ones). -/
def explain (src out : String) : String :=
  let lines := (src.splitOn "\n").toArray
  let shown := (out.splitOn "\n").take 40 |>.map fun l =>
    if (l.splitOn ": error").length > 1 then
      let parts := l.splitOn ":"
      match (parts[1]?.bind (·.toNat?)), (parts[2]?.bind (·.toNat?)) with
      | some k, some c =>
        let text := lines[k - 1]?.getD ""
        -- a long line is shown from a little before the column
        let from_ := if c > 40 then c - 40 else 0
        let piece := String.ofList ((text.toList.drop from_).take 120)
        -- the LeanJs definition the line belongs to
        let owner := ((lines.toList.take k).reverse.findSome? fun t =>
          let t := (t.dropPrefix "partial ").toString
          if t.startsWith "def " || t.startsWith "structure " || t.startsWith "inductive " then
            some (((t.splitOn " ")[1]?).getD "") else none).getD "?"
        s!"in `{owner}`, " ++ l ++ "\n    | " ++ (if from_ > 0 then "…" else "") ++ piece.trimAsciiStart.toString
      | _, _ => l
    else l
  String.intercalate "\n" shown

/-- Elaborate the program with `lean` (found on `PATH`, or `lake env
    lean` inside a Lake project). `.ok` means every definition has a
    type; the error lists Lean's first complaints. -/
def check (p : Program) (leanCmd : String × Array String := ("lake", #["env", "lean"])) :
    IO (Except String Unit) := do
  match emitProgram p with
  | .error e => return .error s!"cannot be typed: {e}"
  | .ok src =>
    IO.FS.withTempDir fun dir => do
      let path := dir / "typed.lean"
      IO.FS.writeFile path src
      let out ← IO.Process.output { cmd := leanCmd.1, args := leanCmd.2.push path.toString }
      if out.exitCode == 0 then return .ok ()
      else return .error (explain src (out.stdout ++ out.stderr))

end LeanJs.Typed
