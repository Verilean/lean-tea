import Std.Internal.Parsec
import Std.Internal.Parsec.String
import LeanJs.Ast

/-! # LeanJs.JsParser — JS-flavoured expression syntax → `Ast.Expr`

`LeanJs.Parser` accepts Lean-shaped syntax (`fun (x) => …`,
`if c then a else b`). This module accepts the JS-shaped form
(`(x) => …`, `c ? a : b`) and produces the *same* `Ast.Expr` so the
rest of the pipeline (codegen, eval) doesn't care which side the
source came from.

Scope is deliberately tight — expressions only. Top-level
`const`/`function` declarations land in a follow-up if useful. -/

namespace LeanJs.JsParser

open LeanJs.Ast
open Std.Internal Parsec Parsec.String

/-! ## Whitespace + lexemes (shared shape with `LeanJs.Parser`) -/

private def whitespace : Parser Unit := do
  let _ ← many (satisfy fun c => c == ' ' || c == '\n' || c == '\t' || c == '\r')
  return ()

private def lex (p : Parser α) : Parser α := do
  let v ← p
  whitespace
  return v

private def sym (str : String) : Parser Unit := lex (skipString str)

private def keyword (k : String) : Parser Unit := attempt do
  skipString k
  match ← peek? with
  | some c => if c.isAlphanum || c == '_' then fail "kw boundary" else pure ()
  | none   => pure ()
  whitespace

private def reserved : List String :=
  -- JS-style reserved set. Smaller than the Lean parser's because
  -- we don't recognise `then/else/where/match/with` here.
  ["true", "false", "null"]

private def identCore : Parser String := attempt do
  let head ← satisfy (fun c => c.isAlpha || c == '_' || c == '$')
  let rest ← manyChars (satisfy (fun c => c.isAlphanum || c == '_' || c == '$'))
  return String.singleton head ++ rest

private def ident : Parser String := attempt do
  let s ← identCore
  if reserved.contains s then fail s!"reserved: {s}"
  whitespace
  return s

/-! ## Atoms -/

private def number : Parser Expr := attempt do
  let negOpt ← optional (pchar '-')
  let head ← satisfy (·.isDigit)
  let rest ← manyChars (satisfy (·.isDigit))
  whitespace
  let n : Int := (String.singleton head ++ rest).toInt!
  return .num (if negOpt.isSome then -n else n)

private def stringLit : Parser Expr := attempt do
  let _ ← pchar '"'
  let body ← manyChars (satisfy (· != '"'))
  let _ ← pchar '"'
  whitespace
  return .str body

private def varExpr : Parser Expr := do
  let n ← ident
  return .var n

/-! ## Expression core, mutually recursive -/

mutual

partial def parseExpr : Parser Expr := parseTernary

/-- JS-style `cond ? then : else` chains right-associatively. -/
partial def parseTernary : Parser Expr := do
  let head ← parseOr
  match ← peek? with
  | some '?' =>
    sym "?"
    let t ← parseExpr
    sym ":"
    let f ← parseExpr
    return .ifE head t f
  | _ => return head

partial def parseOr : Parser Expr := do
  let head ← parseAnd
  parseOrTail head
partial def parseOrTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "||" <* whitespace))).isSome then
    let rhs ← parseAnd; parseOrTail (.binop "||" acc rhs)
  else return acc

partial def parseAnd : Parser Expr := do
  let head ← parseEq
  parseAndTail head
partial def parseAndTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "&&" <* whitespace))).isSome then
    let rhs ← parseEq; parseAndTail (.binop "&&" acc rhs)
  else return acc

partial def parseEq : Parser Expr := do
  let head ← parseCmp
  parseEqTail head
partial def parseEqTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "===" <* whitespace))).isSome then
    let rhs ← parseCmp; parseEqTail (.binop "===" acc rhs)
  else if (← optional (attempt (skipString "==" <* whitespace))).isSome then
    let rhs ← parseCmp; parseEqTail (.binop "==" acc rhs)
  else if (← optional (attempt (skipString "!=" <* whitespace))).isSome then
    let rhs ← parseCmp; parseEqTail (.binop "!=" acc rhs)
  else return acc

partial def parseCmp : Parser Expr := do
  let head ← parseAdd
  parseCmpTail head
partial def parseCmpTail (acc : Expr) : Parser Expr := do
  if (← optional (attempt (skipString "<=" <* whitespace))).isSome then
    let rhs ← parseAdd; parseCmpTail (.binop "<=" acc rhs)
  else if (← optional (attempt (skipString ">=" <* whitespace))).isSome then
    let rhs ← parseAdd; parseCmpTail (.binop ">=" acc rhs)
  else if (← optional (attempt (do skipChar '<'; whitespace))).isSome then
    let rhs ← parseAdd; parseCmpTail (.binop "<" acc rhs)
  else if (← optional (attempt (do skipChar '>'; whitespace))).isSome then
    let rhs ← parseAdd; parseCmpTail (.binop ">" acc rhs)
  else return acc

partial def parseAdd : Parser Expr := do
  let head ← parseMul
  parseAddTail head
partial def parseAddTail (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '+' => let _ ← sym "+"; let rhs ← parseMul; parseAddTail (.binop "+" acc rhs)
  | some '-' => let _ ← sym "-"; let rhs ← parseMul; parseAddTail (.binop "-" acc rhs)
  | _ => return acc

partial def parseMul : Parser Expr := do
  let head ← parseApp
  parseMulTail head
partial def parseMulTail (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '*' => let _ ← sym "*"; let rhs ← parseApp; parseMulTail (.binop "*" acc rhs)
  | some '/' => let _ ← sym "/"; let rhs ← parseApp; parseMulTail (.binop "/" acc rhs)
  | some '%' => let _ ← sym "%"; let rhs ← parseApp; parseMulTail (.binop "%" acc rhs)
  | _ => return acc

partial def parseApp : Parser Expr := do
  let head ← parseAtom
  parseAppTail head

partial def parseAppTail (acc : Expr) : Parser Expr := do
  match ← peek? with
  | some '(' =>
    sym "("
    let args ← sepBy parseExpr (sym ",")
    sym ")"
    parseAppTail (.app acc args.toList)
  | some '.' =>
    let _ ← pchar '.'
    let f ← identCore
    whitespace
    parseAppTail (.dotE acc f)
  | some '[' =>
    sym "["
    let i ← parseExpr
    sym "]"
    parseAppTail (.idxE acc i)
  | _ => return acc

partial def parseAtom : Parser Expr :=
  -- An arrow `(x) => body` and a parenthesised expression both start
  -- with `(`. `attempt parseArrow` rolls back if no `=>` follows.
  number <|> stringLit <|> attempt parseArrow
    <|> parseParenOrSeq <|> parseArrayLit <|> parseObjectLit
    <|> varExpr

partial def parseArrow : Parser Expr := attempt do
  sym "("
  let params ← sepBy ident (sym ",")
  sym ")"
  sym "=>"
  let body ← parseExpr
  return .fnE params.toList body

partial def parseParenOrSeq : Parser Expr := attempt do
  sym "("
  let e ← parseExpr
  sym ")"
  return e

partial def parseArrayLit : Parser Expr := attempt do
  sym "["
  let xs ← sepBy parseExpr (sym ",")
  sym "]"
  return .arrE xs.toList

partial def parseObjectLit : Parser Expr := attempt do
  sym "{"
  let fields ← sepBy parseObjField (sym ",")
  sym "}"
  return .objE fields.toList

partial def parseObjField : Parser (String × Expr) := attempt do
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

/-! ## Public entry -/

def parseExpressionString (src : String) : Except String Expr :=
  Parser.run (whitespace *> parseExpr <* eof) src

end LeanJs.JsParser
