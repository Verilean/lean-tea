import LeanTea
import LeanJs.Ast
import LeanJs.Parser
import LeanJs.Eval

/-! # Sheet — a tiny functional spreadsheet.

A 10×10 grid where each cell stores a **LeanJs-subset expression**.
Cell references (`A1`, `B2`, …) are substituted with the referenced
cell's evaluated value before the expression goes through LeanJs's
parser, so `A1 + B1 * 2` is real arithmetic, not a string.

The point: every part of the stack — the editor runtime, the data
store, the formula language, the evaluator — is Lean. There is no
third-party expression engine; `LeanJs.Eval` is the same evaluator
the `leanjs_run` CLI uses.

Out of scope for v1: dependency-graph incremental recomputation
(we always recompute every cell on every edit; cost is microseconds
for 100 cells), multiple sheets, range formulas (`SUM(A1:A10)`),
typed RPC / MCP surfaces. -/

namespace Sheet

open LeanTea LeanTea.Persist

/-! ## Types -/

abbrev CellRef := String

structure Row where
  ref     : CellRef
  formula : String
  deriving Inhabited, Repr

instance : Entity Row where
  table   := "cells"
  ddl     :=
    "CREATE TABLE IF NOT EXISTS cells(" ++
    "ref TEXT PRIMARY KEY," ++
    "formula TEXT NOT NULL)"
  columns := ["ref", "formula"]
  toRow r := #[r.ref, r.formula]
  fromRow row := match row.toList with
    | [r, f] => .ok { ref := r, formula := f }
    | _      => .error s!"Sheet.Row: expected 2 cols, got {row.size}"

structure Model where
  cells    : List (CellRef × String) := []
  selected : CellRef := "A1"

inductive Msg where
  | setCell (ref : CellRef) (formula : String)
  | select  (ref : CellRef)
  | clear   (ref : CellRef)

def update : Msg → Model → Model
  | .setCell r f, m =>
    let cleaned := m.cells.filter (fun (k, _) => k != r)
    { m with cells := if f.isEmpty then cleaned else cleaned ++ [(r, f)] }
  | .select r,   m => { m with selected := r }
  | .clear r,    m => { m with cells := m.cells.filter (fun (k, _) => k != r) }

/-! ## Cell-ref lexing.

Only column letters `A-J` and row digits `1-10` are recognised, so
that arithmetic involving variables named `a + 1` doesn't trip the
lexer. -/

private def isCol (c : Char) : Bool :=
  c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' ||
  c == 'F' || c == 'G' || c == 'H' || c == 'I' || c == 'J'

/-- Walk the char list once: for each char, peek ahead to see if it
    starts a cell ref. Returns the rewritten source. The
    `Char → String → String` callback substitutes a ref's value. -/
partial def substituteRefs (src : String) (subst : CellRef → String) : String :=
  let rec go (chars : List Char) (out : String) : String :=
    match chars with
    | [] => out
    | c :: rest =>
      if isCol c then
        /- Collect digit run. -/
        let digits := rest.takeWhile (·.isDigit)
        let restAfter := rest.drop digits.length
        if digits.isEmpty then go rest (out.push c)
        else
          let ref := String.mk (c :: digits)
          go restAfter (out ++ "(" ++ subst ref ++ ")")
      else go rest (out.push c)
  go src.toList ""

/-! ## Evaluation via LeanJs.Eval. -/

open LeanJs.Eval (Value)

def showValue : Value → String
  | .int n          => toString n
  | .str s          => s
  | .bool b         => if b then "true" else "false"
  | .null           => "null"
  | .ctor n fields  => s!"{n}({fields.length})"
  | .ctorFn n _ _   => s!"<{n}/?>"
  | .foreign raw    => s!"[foreign:{raw}]"
  | .closure ..     => "<fn>"
  | .dict _         => "<dict>"
  | .arr xs         => s!"[{xs.length} items]"

private def looksLikeText (s : String) : Bool :=
  s.length > 0 &&
  let c := s.front
  !(c.isDigit || c == '(' || c == '-' || c == '+' || c == ' ' || isCol c)

partial def evalCell (model : Model) (ref : CellRef) (depth : Nat) : String :=
  if depth > 50 then "#CYCLE" else
  match model.cells.find? (fun (k, _) => k == ref) with
  | none => ""
  | some (_, raw) =>
    let trimmed := raw.trimAscii.toString
    if trimmed.isEmpty then ""
    else if looksLikeText trimmed then trimmed
    else
      let substituted :=
        substituteRefs trimmed (fun r => evalCell model r (depth+1))
      let prog := s!"def main := {substituted}"
      match LeanJs.Parser.parseProgramString prog with
      | .error _ => "#PARSE"
      | .ok p =>
        match LeanJs.Eval.runProgram p with
        | .ok v    => showValue v
        | .error _ => "#EVAL"

/-! ## Rendering -/

private def upperRange : List Char :=
  ['A','B','C','D','E','F','G','H','I','J']

def allRefs : List CellRef := Id.run do
  let mut out : List CellRef := []
  for n in [1 : 11] do
    for c in upperRange do
      out := out ++ [s!"{c}{n}"]
  return out

private def td_  (attrs : Attrs) (children : List Html) : Html := elem "td" attrs children
private def tr_  (attrs : Attrs) (children : List Html) : Html := elem "tr" attrs children
private def tab_ (attrs : Attrs) (children : List Html) : Html := elem "table" attrs children
private def th__ (attrs : Attrs) (children : List Html) : Html := elem "th" attrs children

def view (m : Model) : Html :=
  let formulaOfSelected :=
    (m.cells.find? (fun (k, _) => k == m.selected)).map (·.2) |>.getD ""
  let gridRows : List Html := List.range 10 |>.map fun rIdx =>
    let n := rIdx + 1
    let tds : List Html := upperRange.map fun c =>
      let ref := s!"{c}{n}"
      let value := evalCell m ref 0
      let selectedClass := if ref == m.selected then "cell selected" else "cell"
      let valueClass := if value.startsWith "#" then "cell-value error" else "cell-value"
      td_ [("class", selectedClass)] [
        a_ [("class", "cell-anchor"), ("href", "#"), ("data-msg", s!"select:{ref}")] [
          span_ [("class","cell-ref")] [text ref],
          span_ [("class", valueClass)] [text value]
        ]
      ]
    tr_ [] tds
  div_ [("class","sheet-app")] [
    div_ [("class","sheet-toolbar")] [
      span_ [("class","cell-name")] [text m.selected],
      form_ [("data-msg","set"), ("class","formula-form")] [
        input_ [("type","hidden"),("name","ref"),("value", m.selected)],
        input_ [("type","text"),("name","formula"),
                ("class","formula-input"),
                ("autofocus","autofocus"),
                ("value", formulaOfSelected),
                ("placeholder","= A1 + B1 * 2")],
        button_ [("type","submit"),("class","l primary")] [text "set"]
      ],
      a_ [("class","l ghost"),("href","#"), ("data-msg", s!"clear:{m.selected}")] [text "clear"]
    ],
    tab_ [("class","sheet-grid")] (
      tr_ [("class","row-header")] (
        td_ [("class","corner")] [] ::
        upperRange.map (fun c => th__ [] [text c.toString])
      ) :: gridRows
    )
  ]

/-! ## Codec — short tab-separated wire format. -/

def encodeModel (m : Model) : String :=
  let cellsEnc :=
    m.cells.map (fun (k, v) => s!"{k}|{v}")
      |> String.intercalate ";"
  s!"{m.selected}\t{cellsEnc}"

def decodeModel (s : String) : Option Model :=
  match s.splitOn "\t" with
  | [sel, cellsStr] =>
    let pairs : List (CellRef × String) :=
      if cellsStr.isEmpty then [] else
      cellsStr.splitOn ";" |>.filterMap fun p =>
        match p.splitOn "|" with
        | [k, v] => some (k, v)
        | _      => none
    some { selected := sel, cells := pairs }
  | _ => none

def decodeMsg (s : String) : Option Msg :=
  if s.startsWith "select:" then some (.select (s.drop 7).toString)
  else if s.startsWith "clear:" then some (.clear (s.drop 6).toString)
  else if s.startsWith "set:" then
    let body := (s.drop 4).toString
    let pairs := body.splitOn "&"
    let lookup (k : String) : Option String :=
      pairs.findSome? fun p =>
        let pre := k ++ "="
        if p.startsWith pre then some (p.drop pre.length).toString else none
    match lookup "ref", lookup "formula" with
    | some r, some f => some (.setCell (LeanTea.Rpc.percentDecode r) (LeanTea.Rpc.percentDecode f))
    | _, _ => none
  else none

def app : WebApp Model Msg :=
  { init := { selected := "A1", cells := [] }
    title := "LeanTea Sheet"
    update, view, encodeModel, decodeModel, decodeMsg }

end Sheet
