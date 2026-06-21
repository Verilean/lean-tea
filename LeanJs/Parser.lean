import Std.Internal.Parsec
import Std.Internal.Parsec.String
import LeanJs.Ast

/-! # LeanJs.Parser — `Std.Internal.Parsec.String` combinators

We avoid hand-rolling a tokenizer. Tokens are absorbed inline using
`pchar` / `pstring` plus a shared `ws` whitespace skipper.

Operator precedence climbs: `parseAdd` chains `parseMul`, which
chains `parseApp`, which chains `parseAtom`. Same shape as the
recursive-descent demo from Chapter 7, just expressed via the
combinator library rather than by hand. -/

namespace LeanJs.Parser

open LeanJs.Ast
open Std.Internal Parsec Parsec.String

/-! ## Whitespace + lexemes -/

private partial def whitespace : Parser Unit := do
  let _ ← many (satisfy fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r')
  -- Strip `-- …` line comments and resume.
  if (← optional (attempt (skipString "--"))).isSome then
    let _ ← many (satisfy (· != '\n'))
    let _ ← optional (pchar '\n')
    whitespace

/-- `lex p` runs `p` then eats trailing whitespace. -/
private def lex (p : Parser α) : Parser α := do
  let v ← p
  whitespace
  return v

private def sym (str : String) : Parser Unit := lex (skipString str)

private def keyword (k : String) : Parser Unit := attempt do
  skipString k
  -- Make sure it isn't a prefix of a longer ident (e.g. `let` vs `letter`).
  match ← peek? with
  | some c => if c.isAlphanum || c == '_' then fail s!"unexpected keyword prefix '{k}'" else pure ()
  | none   => pure ()
  whitespace

/-! ## Atoms -/

private def reserved : List String :=
  ["let", "in", "if", "then", "else", "fun", "def",
   "inductive", "where", "match", "with",
   "async", "await", "extern", "js",
   "class", "instance", "record",
   "null", "new", "import", "from", "as", "include",
   "true", "false"]

private def hexDigitValue (c : Char) : Nat :=
  if c.isDigit then c.toNat - '0'.toNat
  else if 'a' ≤ c ∧ c ≤ 'f' then c.toNat - 'a'.toNat + 10
  else if 'A' ≤ c ∧ c ≤ 'F' then c.toNat - 'A'.toNat + 10
  else 0

/-- Number literal — decimal (with optional leading `-` for negation)
    or hex `0xABCD`. Hex is convenient for colour constants in the
    Three.js-flavoured examples; otherwise the parser stays simple. -/
private def number : Parser Expr := attempt do
  let negOpt ← optional (pchar '-')
  /- Try hex form first: `0x` then ≥1 hex digit. -/
  let hexOpt ← optional (attempt do
    skipString "0x"
    let head ← satisfy (fun c => c.isDigit ∨ ('a' ≤ c ∧ c ≤ 'f') ∨ ('A' ≤ c ∧ c ≤ 'F'))
    let rest ← manyChars (satisfy (fun c => c.isDigit ∨ ('a' ≤ c ∧ c ≤ 'f') ∨ ('A' ≤ c ∧ c ≤ 'F')))
    let digits := String.singleton head ++ rest
    let n : Nat := digits.toList.foldl (fun acc c => acc * 16 + hexDigitValue c) 0
    return n)
  whitespace
  match hexOpt with
  | some n =>
    let signed : Int := Int.ofNat n
    return .num (if negOpt.isSome then -signed else signed)
  | none =>
    let head ← satisfy (·.isDigit)
    let intPart ← manyChars (satisfy (·.isDigit))
    let intStr := String.singleton head ++ intPart
    /- Optional fractional part: `.dddd`. If a `.` is followed by a
       digit, this is a float literal — recorded as `.numF` so the
       emitter copies the text verbatim (no rounding). -/
    let fracOpt ← optional (attempt do
      skipChar '.'
      let h ← satisfy (·.isDigit)
      let r ← manyChars (satisfy (·.isDigit))
      return String.singleton h ++ r)
    whitespace
    match fracOpt with
    | some frac =>
      let lit := (if negOpt.isSome then "-" else "") ++ intStr ++ "." ++ frac
      return .numF lit
    | none =>
      let n : Int := intStr.toInt!
      return .num (if negOpt.isSome then -n else n)

private def stringLit : Parser Expr := attempt do
  let _ ← pchar '"'
  let body ← manyChars (satisfy (· != '"'))
  let _ ← pchar '"'
  whitespace
  return .str body

private def identCore : Parser String := attempt do
  let head ← satisfy (fun c => c.isAlpha || c == '_')
  let rest ← manyChars (satisfy (fun c => c.isAlphanum || c == '_'))
  return String.singleton head ++ rest

/-- An identifier that isn't a reserved keyword. -/
private def ident : Parser String := attempt do
  let s ← identCore
  if reserved.contains s then fail s!"reserved word: {s}"
  whitespace
  return s

private def varExpr : Parser Expr := do
  let n ← ident
  return .var n

private def parenExpr (parseExpr : Parser Expr) : Parser Expr := do
  sym "("
  let e ← parseExpr
  sym ")"
  return e

/-! ## let / if / fun / app / arithmetic — mutually recursive -/

mutual

partial def parseExpr : Parser Expr := do
  if let some _ ← peek? then
    (parseLet <|> parseIf <|> parseFun <|> parseMatch
      <|> parseAwait <|> parseAssign)
  else
    fail "empty expression"

/-- Assignment `lhs <- rhs`, right-associative. LHS is whatever
    `parseAdd` produces (typically a `var`, `dotE`, `optDotE`, or
    `idxE`); the parser doesn't enforce that — codegen emits
    whatever the user wrote and JS rejects it at runtime if the LHS
    isn't a reference. Lower-precedence than every binary operator
    so `obj.field <- a || b` reads as expected. RHS is a full
    `parseExpr`, so `x <- if cond then a else b` works. -/
partial def parseAssign : Parser Expr := do
  let head ← parseAdd
  match ← optional (attempt (skipString "<-" <* whitespace)) with
  | some _ =>
    let rhs ← parseExpr
    return .assignE head rhs
  | none   => return head

partial def parseAwait : Parser Expr := attempt do
  keyword "await"
  let e ← parseApp
  return .awaitE e

partial def parseMatch : Parser Expr := attempt do
  keyword "match"
  let scrutinee ← parseExpr
  keyword "with"
  let branches ← many1 parseMatchBranch
  return .matchE scrutinee branches.toList

partial def parseMatchBranch : Parser MatchBranch := attempt do
  sym "|"
  /- Pattern shapes, tried in order:
       1. string literal     `| "loading" => …`
       2. wildcard `_`       `| _      => …`
       3. constructor + args `| Cons(h, t) => …`  /  `| None => …`
     `attempt` on each so a partial match rolls back cleanly. -/
  let pat ← (do
        attempt do
          let _ ← pchar '"'
          let body ← manyChars (satisfy (· != '"'))
          let _ ← pchar '"'
          whitespace
          return Pattern.strLit body)
      <|> (do
        attempt do
          skipChar '_'
          /- Reject if the `_` is the head of an identifier (e.g. `_x`). -/
          match ← peek? with
          | some c => if c.isAlphanum || c == '_' then
                        fail "ident starting with _"
                      else pure ()
          | none   => pure ()
          whitespace
          return Pattern.wildcard)
      <|> (do
        let ctor ← ident
        let args ← (do
          sym "("
          let xs ← sepBy ident (sym ",")
          sym ")"
          return xs.toList) <|> pure []
        return Pattern.ctor ctor args)
  sym "=>"
  let body ← parseAdd  -- nested `match` requires explicit parens
  return .mk pat body

partial def parseLet : Parser Expr := attempt do
  keyword "let"
  let name ← ident
  sym ":="
  let val ← parseExpr
  sym ";"
  let body ← parseExpr
  return .letE name val body

partial def parseIf : Parser Expr := attempt do
  keyword "if"
  let c ← parseExpr
  keyword "then"
  let t ← parseExpr
  keyword "else"
  let f ← parseExpr
  return .ifE c t f

partial def parseFun : Parser Expr := attempt do
  keyword "fun"
  sym "("
  let params ← sepBy ident (sym ",")
  sym ")"
  sym "=>"
  let body ← parseExpr
  return .fnE params.toList body

-- Precedence climb:
--   || (lowest) → && → == != → < <= > >= → + - → * / %  (tightest)
-- `parseAdd` is the historical entry point; we now route it through
-- the lowest-precedence rule so existing callers (parseMatchBranch
-- etc.) compose with the new operators without edits.

partial def parseAdd : Parser Expr := do
  let v ← parseOr
  return v

partial def parseOr : Parser Expr := do
  let head ← parseAnd
  parseOrTail head

partial def parseOrTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "||" <* whitespace))).isSome then
    let rhs ← parseAnd
    parseOrTail (.binop "||" acc rhs)
  else return acc

partial def parseAnd : Parser Expr := do
  let head ← parseEq
  parseAndTail head

partial def parseAndTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "&&" <* whitespace))).isSome then
    let rhs ← parseEq
    parseAndTail (.binop "&&" acc rhs)
  else return acc

partial def parseEq : Parser Expr := do
  let head ← parseCmp
  parseEqTail head

partial def parseEqTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "==" <* whitespace))).isSome then
    let rhs ← parseCmp; parseEqTail (.binop "==" acc rhs)
  else if (← optional (attempt (skipString "!=" <* whitespace))).isSome then
    let rhs ← parseCmp; parseEqTail (.binop "!=" acc rhs)
  else return acc

partial def parseCmp : Parser Expr := do
  let head ← parseAddSub
  parseCmpTail head

partial def parseCmpTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "<=" <* whitespace))).isSome then
    let rhs ← parseAddSub; parseCmpTail (.binop "<=" acc rhs)
  else if (← optional (attempt (skipString ">=" <* whitespace))).isSome then
    let rhs ← parseAddSub; parseCmpTail (.binop ">=" acc rhs)
  else if (← optional (attempt (do
      skipChar '<'
      -- Don't swallow the `<` of `<-` (assignment). Peeking for `-`
      -- after `<` is enough — `<-something` is unambiguous given
      -- LeanJs has no `<-` operator outside assignment.
      match ← peek? with
      | some '-' => fail "looks like <-"
      | _ => whitespace))).isSome then
    let rhs ← parseAddSub; parseCmpTail (.binop "<" acc rhs)
  else if (← optional (attempt (do skipChar '>'; whitespace))).isSome then
    let rhs ← parseAddSub; parseCmpTail (.binop ">" acc rhs)
  else return acc

partial def parseAddSub : Parser Expr := do
  let head ← parseMul
  parseAddTail head

partial def parseAddTail (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '+' => let _ ← sym "+"; let rhs ← parseMul; parseAddTail (.binop "+" acc rhs)
  | some '-' => let _ ← sym "-"; let rhs ← parseMul; parseAddTail (.binop "-" acc rhs)
  | _        => return acc

partial def parseMul : Parser Expr := do
  let head ← parseApp
  parseMulTail head

partial def parseMulTail (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '*' => let _ ← sym "*"; let rhs ← parseApp; parseMulTail (.binop "*" acc rhs)
  | some '/' => let _ ← sym "/"; let rhs ← parseApp; parseMulTail (.binop "/" acc rhs)
  | some '%' => let _ ← sym "%"; let rhs ← parseApp; parseMulTail (.binop "%" acc rhs)
  | _        => return acc

partial def parseApp : Parser Expr := do
  /- Unary prefix `!expr` (logical not). `-expr` is handled by
     `number` for the literal case and at parseAddSub for the
     binary form; standalone unary `-` is rare enough that we don't
     surface it explicitly (use `0 - x`). -/
  if (← optional (attempt (do skipChar '!'; whitespace))).isSome then
    let e ← parseApp
    return .unopE "!" e
  let head ← parseAtom
  let head ← parseAppTail head
  /- Lean-prover-style space-separated application: `f a b` lowers
     to `f(a, b)`. Runs after the JS-style chain (parseAppTail)
     so `f(a)(b)`, `f.x()`, etc. all keep their existing semantics.
     Boundaries: any reserved keyword (`let`, `if`, …), operator,
     or punctuation makes parseAtom fail inside the `attempt`,
     so collection naturally stops at expression edges. -/
  parseSpaceApp head

partial def parseAppTail (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '(' =>
    /- After consuming `(args)` as a call, only chase member-access
       chains (`.x`, `?.x`, `[i]`). A subsequent `(...)` is left
       for parseSpaceApp, which merges it into the existing arg list
       rather than treating it as a separate (curried) call. -/
    sym "("
    let args ← sepBy parseExpr (sym ",")
    sym ")"
    parseAppTailMembers (.app acc args.toList)
  | some '?' =>
    /- Optional chain `?.field`. We peek for `?.` and bail if it's
       just a `?` (which LeanJs doesn't otherwise use). -/
    let opt ← optional (attempt (do skipString "?."; let f ← identCore; return f))
    match opt with
    | none   => return acc  -- `?` without `.field` — leave for outer to error
    | some f =>
      whitespace
      parseAppTail (.optDotE acc f)
  | some '.' =>
    -- `.field` access. Identifier-form names only.
    let _ ← pchar '.'
    let f ← identCore
    whitespace
    parseAppTail (.dotE acc f)
  | some '[' =>
    /- `[` is ambiguous between indexing (`arr[k]`) and Lean-style
       space-application with an array literal (`f [1, 2, 3]`).
       We disambiguate after parsing the first inner expression:
       a trailing `,` means it's an array literal being applied,
       a plain `]` means it's an index. After consuming the call,
       chain only member access (parseAppTailMembers) so a follow-up
       `(...)` becomes a parseSpaceApp arg rather than a curried call. -/
    sym "["
    let first ← parseExpr
    match ← peek? with
    | some ',' =>
      sym ","
      let rest ← sepBy parseExpr (sym ",")
      sym "]"
      let arrArg := Expr.arrE (first :: rest.toList)
      parseAppTailMembers (.app acc [arrArg])
    | _ =>
      sym "]"
      parseAppTailMembers (.idxE acc first)
  | _ => return acc

/-- After a call has been consumed, keep chasing only member-access
    chains (`.x`, `?.x`, `[i]`). A new `(` is left for parseSpaceApp
    so `f(a) (b)` becomes `f(a, b)` rather than the JS-curry
    `f(a)(b)`. The existing code base has no curried-call usage,
    and the Lean-style merge is more useful. -/
partial def parseAppTailMembers (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '?' =>
    let opt ← optional (attempt (do skipString "?."; let f ← identCore; return f))
    match opt with
    | none   => return acc
    | some f =>
      whitespace
      /- After a member access, allow a fresh `(args)` chain via
         parseAppTail — that's how JS method calls work. -/
      parseAppTail (.optDotE acc f)
  | some '.' =>
    let _ ← pchar '.'
    let f ← identCore
    whitespace
    parseAppTail (.dotE acc f)
  | some '[' =>
    sym "["
    let first ← parseExpr
    match ← peek? with
    | some ',' =>
      sym ","
      let rest ← sepBy parseExpr (sym ",")
      sym "]"
      let arrArg := Expr.arrE (first :: rest.toList)
      parseAppTailMembers (.app acc [arrArg])
    | _ =>
      sym "]"
      parseAppTailMembers (.idxE acc first)
  | _ => return acc

/-- Lean-prover-style space-separated application. Greedily consumes
    additional atoms (numbers, strings, idents, paren-exprs, array /
    object literals) as if they were extra call arguments. We wrap
    each candidate in `attempt` so a reserved keyword or operator
    cleanly aborts collection without consuming input. Resulting
    AST: `f a b c` → `.app f [a, b, c]` (single multi-arg node, so
    codegen emits `f(a, b, c)` rather than `f(a)(b)(c)`). -/
partial def parseSpaceApp (head : Expr) : Parser Expr := do
  /- Lean-style space app only makes sense for callable heads. If
     the head is a bare literal (number / string / array / record
     literal / lambda etc.), don't try to apply it — otherwise
         def main := 0
         tick()
     would be misparsed as `0(tick)` and the trailing `()` would
     be left at top level. Restrict to identifiers, member access,
     and existing call results. -/
  let isCallable :=
    match head with
    | .var _      => true
    | .dotE _ _   => true
    | .optDotE _ _ => true
    | .app _ _    => true
    | .idxE _ _   => true
    | _           => false
  if !isCallable then return head
  /- Excluded starters:
     * `[`  — owned by parseAppTail (indexing / array-literal-as-arg
              via the comma trick).
     * `-`  — ambiguous with binary subtract. `f - 1` would otherwise
              be parsed as `f(-1)` because `number` accepts a unary
              `-`. Users who want `f` applied to `-1` must write
              `f (-1)` or `f(-1)`. -/
  let canStartArg ← (do
    match ← peek? with
    | some c => return c != '[' && c != '-'
    | none   => return false)
  if !canStartArg then return head
  /- The arg's own chain is restricted to member access — a stray
     `(` after the atom belongs to the OUTER space-app, not to this
     arg. Using parseAppTail here would over-eat: `add 1 (dbl 3)`
     would treat `(dbl 3)` as call args of `1`. -/
  let argOpt ← optional (attempt do
    let a ← parseAtom
    parseAppTailMembers a)
  match argOpt with
  | none     => return head
  | some arg =>
    let newHead := match head with
      | .app f xs => .app f (xs ++ [arg])
      | _         => .app head [arg]
    parseSpaceApp newHead

partial def parseNew : Parser Expr := attempt do
  keyword "new"
  /- `new Cls(args)` or `new (path.to.Cls)(args)`. We consume the
     constructor name (or paren-expr) via parseApp's atom layer,
     then the call args manually. -/
  let cls ← (do
    /- Atom path only — no fancy chaining inside `new`. -/
    let head ← parseAtom
    /- Allow `.` chains so `new THREE.Vector3()` works. -/
    let rec dots (acc : Expr) : Parser Expr := do
      match ← peek? with
      | some '.' =>
        let _ ← pchar '.'
        let f ← identCore
        whitespace
        dots (.dotE acc f)
      | _ => return acc
    dots head)
  let args ← (do
    sym "("
    let xs ← sepBy parseExpr (sym ",")
    sym ")"
    return xs.toList) <|> pure []
  return .newE cls args

partial def parseNullLit : Parser Expr := attempt do
  keyword "null"
  return .nullE

partial def parseBoolLit : Parser Expr := attempt do
  (do keyword "true"; return .boolE true) <|>
  (do keyword "false"; return .boolE false)

/-- `Cls { f1: v1, f2: v2, … }` — record-literal construction.
    Distinguished from a bare object literal by the leading
    identifier. `attempt` so a plain `ident` (not followed by `{`)
    rolls back and falls through to `varExpr`. -/
partial def parseRecordLit : Parser Expr := attempt do
  let cls ← ident
  sym "{"
  let parseField : Parser (String × Expr) := attempt do
    let k ← ident
    sym ":"
    let v ← parseExpr
    return (k, v)
  let fields ← sepBy parseField (sym ",")
  sym "}"
  return .recordLitE cls fields.toList

partial def parseAtom : Parser Expr :=
  parseNullLit <|> parseBoolLit <|> parseNew
    <|> number <|> stringLit <|> arrayLit <|> objectLit
    <|> parenExpr parseExpr <|> parseRecordLit <|> varExpr

partial def arrayLit : Parser Expr := attempt do
  sym "["
  let xs ← sepBy parseExpr (sym ",")
  sym "]"
  return .arrE xs.toList

partial def objectLit : Parser Expr := attempt do
  sym "{"
  let fields ← sepBy parseObjField (sym ",")
  sym "}"
  return .objE fields.toList

partial def parseObjField : Parser (String × Expr) := attempt do
  -- Key is an unquoted identifier — keeps the parser cheap.
  let k ← ident
  sym ":"
  let v ← parseExpr
  return (k, v)

partial def sepBy (p : Parser α) (sep : Parser Unit) : Parser (Array α) := do
  match ← (optional p) with
  | none   => return #[]
  | some a => sepByLoop p sep #[a]

partial def sepByLoop (p : Parser α) (sep : Parser Unit) (acc : Array α)
    : Parser (Array α) := do
  match ← optional (attempt (do sep; p)) with
  | some b => sepByLoop p sep (acc.push b)
  | none   => return acc

end

/-! ## Top-level defs and programs -/

/-- A formal parameter `name` or `name : Type` (type optional). -/
partial def parseParam : Parser Param := do
  let name ← ident
  let type? ← (attempt do
    sym ":"
    -- Bail out if the `:` was actually the start of `:=`
    match ← peek? with
    | some '=' => fail "param: that was :="
    | _ => let t ← ident; return some t
  ) <|> pure none
  return { name, type? }

partial def parseDef : Parser TopDef := do
  keyword "def"
  let name ← ident
  /- We need to tell `def f` apart from `def f()`:
     * `def f := body`   — a value binding (no parens at all)
     * `def f() := body` — a 0-arity function
     * `def f(a) := body` — a 1-arity function
     Lowering happens here: the 0-arity-function case wraps the body
     in an explicit `fun () =>` so Codegen treats it as a callable. -/
  let parens ← optional (do
    sym "("
    let ps ← sepBy parseParam (sym ",")
    sym ")"
    return ps.toList)
  -- Optional return-type annotation: `def f(...) : Int := …`
  -- `attempt` so that bare `:=` (no return type) doesn't consume the colon.
  let retType? ← (attempt do
    sym ":"
    -- Refuse if the colon was actually the start of `:=`.
    match ← peek? with
    | some '=' => fail "ret-type: that was :="
    | _ => let t ← ident; return some t
  ) <|> pure none
  sym ":="
  let body ← parseExpr
  match parens with
  | some [] => return .defE name [] retType? (.fnE [] body)
  | some ps => return .defE name ps retType? body
  | none    => return .defE name [] retType? body

partial def parseAsyncDef : Parser TopDef := attempt do
  keyword "async"
  keyword "def"
  let name ← ident
  let params ← (do
    sym "("
    let ps ← sepBy parseParam (sym ",")
    sym ")"
    return ps.toList) <|> pure []
  sym ":="
  let body ← parseExpr
  return .asyncDefE name params body

partial def parseExtern : Parser TopDef := attempt do
  keyword "extern"
  keyword "js"
  -- A string literal carrying the raw JS expression.
  let _ ← pchar '"'
  let raw ← manyChars (satisfy (· != '"'))
  let _ ← pchar '"'
  whitespace
  let name ← ident
  return .externE name raw

partial def parseCtorDecl : Parser CtorDecl := attempt do
  sym "|"
  let name ← ident
  let args ← (do
    sym "("
    let xs ← sepBy ident (sym ",")
    sym ")"
    return xs.toList) <|> pure []
  return { name, arity := args.length }

partial def parseInductive : Parser TopDef := attempt do
  keyword "inductive"
  let name ← ident
  keyword "where"
  let ctors ← many1 parseCtorDecl
  return .indE name ctors.toList

partial def parseClass : Parser TopDef := attempt do
  keyword "class"
  let name ← ident
  keyword "where"
  let methods ← many1 ident
  return .classE name methods.toList

partial def parseInstanceMethod : Parser (String × Expr) := attempt do
  let m ← ident
  sym ":="
  let body ← parseExpr
  return (m, body)

partial def parseInstance : Parser TopDef := attempt do
  keyword "instance"
  let cls ← ident
  let ty  ← ident
  keyword "where"
  let head ← parseInstanceMethod
  let rest ← many (attempt (do sym ","; parseInstanceMethod))
  return .instE cls ty (head :: rest.toList)

/-- `import * as Foo from "src"` — namespace import. -/
partial def parseImportStar : Parser TopDef := attempt do
  keyword "import"
  sym "*"
  keyword "as"
  let alias ← ident
  keyword "from"
  let _ ← pchar '"'
  let src ← manyChars (satisfy (· != '"'))
  let _ ← pchar '"'
  whitespace
  return .importE [{ name := "*", namespaceAs := some alias }] src

/-- `import { a, b as c } from "src"` — named imports. -/
partial def parseImportNamed : Parser TopDef := attempt do
  keyword "import"
  sym "{"
  let one : Parser ImportBinding := attempt do
    let n ← ident
    let alias ← (attempt do keyword "as"; let a ← ident; return some a) <|> pure none
    return { name := n, namespaceAs := alias }
  let binds ← sepBy one (sym ",")
  sym "}"
  keyword "from"
  let _ ← pchar '"'
  let src ← manyChars (satisfy (· != '"'))
  let _ ← pchar '"'
  whitespace
  return .importE binds.toList src

partial def parseImportTop : Parser TopDef :=
  parseImportStar <|> parseImportNamed

/-- `include "path.leanjs"` — splice another LeanJs file's top defs
    into this program at the include site. Resolved by
    `LeanJs.Includes.resolve` before Check / Codegen / Eval run. -/
partial def parseIncludeTop : Parser TopDef := attempt do
  keyword "include"
  let _ ← pchar '"'
  let src ← manyChars (satisfy (· != '"'))
  let _ ← pchar '"'
  whitespace
  return .includeE src

/-- `record Name where f1 : T1, f2 : T2, …` — declares a record shape.
    Types are advisory (LeanJs doesn't elaborate them) but field names
    are kept and `Check` verifies every `Name {…}` construction. -/
partial def parseRecord : Parser TopDef := attempt do
  keyword "record"
  let name ← ident
  keyword "where"
  let parseField : Parser (String × String) := attempt do
    let fname ← ident
    sym ":"
    let ftype ← ident
    return (fname, ftype)
  let fields ← sepBy parseField (sym ",")
  return .recordE name fields.toList

/-- A trailing top-level expression statement — handy for boot calls
    like `tick()` that need to run at module load. We *don't* allow
    bare expressions that would shadow a `def` keyword (parseTopDef
    tries everything else first). -/
partial def parseExprTop : Parser TopDef := attempt do
  let e ← parseExpr
  return .exprE e

partial def parseTopDef : Parser TopDef :=
  parseIncludeTop <|> parseImportTop <|> parseRecord <|> parseInductive
   <|> parseExtern <|> parseAsyncDef <|> parseClass <|> parseInstance
   <|> parseDef <|> parseExprTop

partial def parseProgram : Parser Program := do
  whitespace
  let defs ← many parseTopDef
  eof
  return defs

/-! ## Public entry -/

/-- Translate a byte offset back to a `(line, col)` pair by walking
    `src` and counting newlines. Both are 1-indexed; if `off` is past
    EOF we return the last position. -/
def lineColOfOffset (src : String) (off : Nat) : Nat × Nat := Id.run do
  let mut line : Nat := 1
  let mut col  : Nat := 1
  let mut i    : Nat := 0
  let cs := src.toList
  for c in cs do
    if i ≥ off then break
    if c == '\n' then
      line := line + 1
      col  := 1
    else
      col := col + 1
    i := i + 1
  return (line, col)

/-- Post-process a `Parsec` error string of the shape
    `offset N: <message>` (the default `Std.Internal.Parsec` format)
    and prepend a human-readable `(line L, col C)`. Unrecognised
    formats pass through unchanged. -/
private def withLineCol (src : String) : String → String := fun e =>
  /- Strip a leading "offset " then parse digits. -/
  if !e.startsWith "offset " then e else
  let rest := (e.drop 7).toString
  let digits := rest.takeWhile Char.isDigit |>.toString
  match digits.toNat? with
  | none      => e
  | some off  =>
    let (line, col) := lineColOfOffset src off
    let tail := (rest.drop digits.length).toString.dropWhile (fun c => c == ':' || c == ' ')
    s!"error at (line {line}, col {col}, offset {off}): {tail.toString}"

def parseExpressionString (src : String) : Except String Expr :=
  (Parser.run (whitespace *> parseExpr <* eof) src).mapError (withLineCol src)

def parseProgramString (src : String) : Except String Program :=
  (Parser.run parseProgram src).mapError (withLineCol src)

end LeanJs.Parser
