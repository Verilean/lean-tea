import LeanTea.Markdown.Ast
import LeanTea.Markdown.Inline

/-! # LeanTea.Markdown.Parser — line-based block parser

The parser is a single left-to-right walk over the lines of the
source. For each line it decides which block-shape (heading, bullet,
code fence, …) starts here, then consumes as many subsequent lines as
that block-shape demands. The result is a flat `List Block`.

Inline tokenisation is deferred to `LeanTea.Markdown.Inline.parse`. -/

namespace LeanTea.Markdown.Parser

open LeanTea.Markdown

/-! ## Tiny helpers -/

private def isBlank (line : String) : Bool :=
  line.trimAscii.toString.isEmpty

private def stripTrailingCR (line : String) : String :=
  if line.endsWith "\r" then (line.dropEnd 1).toString else line

private def headingLevel (line : String) : Option (Nat × String) :=
  /- Count leading `#`s (1..6), then require a space. -/
  let cs := line.toList
  let n := cs.takeWhile (· == '#') |>.length
  if n == 0 || n > 6 then none
  else
    let rest := cs.drop n
    match rest with
    | ' ' :: body => some (n, (String.mk body).trimAscii.toString)
    | _           => none

private def isHRule (line : String) : Bool :=
  let trimmed := line.trimAscii.toString
  trimmed == "---" || trimmed == "***" || trimmed == "___"

private def codeFenceLang? (line : String) : Option String :=
  let trimmed := line.trimAscii.toString
  if trimmed.startsWith "```"
  then some (trimmed.drop 3).toString.trimAscii.toString
  else none

private def isBullet (line : String) : Option String :=
  /- `- foo`, `* foo`, or `+ foo`. We don't yet track indentation,
     so nested lists collapse to a single level. -/
  let trimmed := line.trimAscii.toString
  if trimmed.startsWith "- " then some (trimmed.drop 2).toString
  else if trimmed.startsWith "* " then some (trimmed.drop 2).toString
  else if trimmed.startsWith "+ " then some (trimmed.drop 2).toString
  else none

private def isOrdered (line : String) : Option String :=
  /- Match `N. ` or `N) ` for N = 1..9. -/
  let trimmed := line.trimAscii.toString
  let cs := trimmed.toList
  let digits := cs.takeWhile Char.isDigit
  if digits.isEmpty then none else
    let after := cs.drop digits.length
    match after with
    | '.' :: ' ' :: rest => some (String.mk rest)
    | ')' :: ' ' :: rest => some (String.mk rest)
    | _ => none

private def isBlockquote (line : String) : Option String :=
  let trimmed := line.trimAscii.toString
  if trimmed.startsWith "> "  then some (trimmed.drop 2).toString
  else if trimmed == ">"      then some ""
  else none

private def isTablePipeRow (line : String) : Bool :=
  let trimmed := line.trimAscii.toString
  trimmed.startsWith "|" && trimmed.endsWith "|"

private def isTableSeparator (line : String) : Bool :=
  /- `|---|---|` or with colons for alignment. -/
  let trimmed := line.trimAscii.toString
  if !(trimmed.startsWith "|" && trimmed.endsWith "|") then false
  else
    let inner := (trimmed.drop 1).dropEnd 1 |>.toString
    inner.toList.all (fun c => c == '-' || c == '|' || c == ':' || c == ' ')

private def splitTableRow (line : String) : List String :=
  /- Strip leading/trailing `|`, then split on `|`, trim each cell. -/
  let trimmed := (line.trimAscii.toString)
  let inner   := if trimmed.startsWith "|" then (trimmed.drop 1).toString else trimmed
  let inner2  := if inner.endsWith "|" then (inner.dropEnd 1).toString else inner
  (inner2.splitOn "|").map (·.trimAscii.toString)

/-! ## Block walk

We work over a mutable `lines : Array String` + `i : Nat` index. -/

private structure St where
  lines : Array String
  i     : Nat := 0

private def peek (st : St) : Option String :=
  st.lines[st.i]?

private def advance (st : St) : St := { st with i := st.i + 1 }

/-- Greedily read lines while `pred` returns true. Returns the
    consumed lines plus the updated state. -/
private partial def takeWhile (st : St) (pred : String → Bool)
    : Array String × St := Id.run do
  let mut acc : Array String := #[]
  let mut s := st
  while h : s.i < s.lines.size do
    let line := s.lines[s.i]
    if pred line then
      acc := acc.push line
      s := advance s
    else
      break
  return (acc, s)

mutual

/-- Parse a single block starting at the current line. -/
partial def parseBlock (st : St) : Block × St :=
  match peek st with
  | none => (.paragraph [], st)        -- impossible at top level
  | some raw =>
    let line := stripTrailingCR raw
    if isBlank line then
      (.paragraph [], advance st)  -- caller skips empties
    else if isHRule line then
      (.hrule, advance st)
    /- Heading: a single line. -/
    else match headingLevel line with
    | some (n, body) => (.heading n (Inline.parse body), advance st)
    | none =>
    /- Code fence. -/
    match codeFenceLang? line with
    | some lang =>
      Id.run do
        let mut body : String := ""
        let mut s := advance st
        while h : s.i < s.lines.size do
          let l := stripTrailingCR (s.lines[s.i])
          if codeFenceLang? l |>.isSome then
            s := advance s
            break
          body := body ++ l ++ "\n"
          s := advance s
        return (.code lang body, s)
    | none =>
    /- Bullet list — consume consecutive bullet lines. -/
    match isBullet line with
    | some _ =>
      let (lines, s) := takeWhile st (fun l => (isBullet (stripTrailingCR l)).isSome)
      let items : List (List Inline) :=
        lines.toList.map fun l =>
          match isBullet (stripTrailingCR l) with
          | some body => Inline.parse body
          | none      => []
      (.bullets items, s)
    | none =>
    /- Ordered list — same shape as bullets but with `N.` markers. -/
    match isOrdered line with
    | some _ =>
      let (lines, s) := takeWhile st (fun l => (isOrdered (stripTrailingCR l)).isSome)
      let items : List (List Inline) :=
        lines.toList.map fun l =>
          match isOrdered (stripTrailingCR l) with
          | some body => Inline.parse body
          | none      => []
      (.ordered items, s)
    | none =>
    /- Blockquote — consume consecutive `> …` lines, strip the prefix,
       recursively parse the inner lines as their own document. -/
    match isBlockquote line with
    | some _ =>
      let (lines, s) := takeWhile st (fun l => (isBlockquote (stripTrailingCR l)).isSome)
      let inner : List String :=
        lines.toList.map fun l =>
          match isBlockquote (stripTrailingCR l) with
          | some body => body
          | none      => ""
      let innerSt : St := { lines := inner.toArray }
      let inner' := parseBlocks innerSt
      (.blockquote inner', s)
    | none =>
    /- Table — header line, separator, then rows. -/
    if isTablePipeRow line && st.i + 1 < st.lines.size
       && isTableSeparator (stripTrailingCR (st.lines[st.i + 1]!)) then
      Id.run do
        let header := (splitTableRow line).map Inline.parse
        let mut s := { st with i := st.i + 2 }  -- skip header + separator
        let mut rows : List (List (List Inline)) := []
        while h : s.i < s.lines.size do
          let l := stripTrailingCR (s.lines[s.i])
          if isTablePipeRow l then
            let cells := (splitTableRow l).map Inline.parse
            rows := rows ++ [cells]
            s := advance s
          else break
        return (.table header rows, s)
    else
    /- Default — paragraph. Consume consecutive non-blank lines that
       don't start a different block. -/
    let (paraLines, s) := takeWhile st fun l =>
      let l := stripTrailingCR l
      !isBlank l
        && !isHRule l
        && (headingLevel l).isNone
        && (codeFenceLang? l).isNone
        && (isBullet l).isNone
        && (isOrdered l).isNone
        && (isBlockquote l).isNone
        && !isTablePipeRow l
    let body := String.intercalate " " paraLines.toList
    (.paragraph (Inline.parse body), s)

/-- Parse the entire document. -/
partial def parseBlocks (st : St) : List Block := Id.run do
  let mut s := st
  let mut acc : List Block := []
  while h : s.i < s.lines.size do
    let line := stripTrailingCR (s.lines[s.i])
    if isBlank line then
      s := advance s
    else
      let (b, s') := parseBlock s
      acc := acc ++ [b]
      s := s'
  return acc

end

/-- Top-level entry point: split on newlines and walk. -/
def parse (src : String) : Document :=
  let lines := (src.splitOn "\n").toArray
  parseBlocks { lines }

end LeanTea.Markdown.Parser
