import LeanTea.Markdown.Ast

/-! # LeanTea.Markdown.Inline — inline tokenizer

Walks one logical line (no newlines) producing a list of `Inline`
elements. The grammar is intentionally simple — we recognise the
common patterns in a single left-to-right pass, with no backtracking
beyond a fixed lookahead. -/

namespace LeanTea.Markdown.Inline

open LeanTea.Markdown

/-! ## Character-level helpers -/

private def isPunct (c : Char) : Bool :=
  c == '.' || c == ',' || c == '!' || c == '?' || c == ';' || c == ':'

/-- Read characters until `pred` returns true. Returns `(consumed,
    remainder)`. -/
private def takeUntil (s : String) (pred : Char → Bool) : String × String := Id.run do
  let mut acc : String := ""
  let mut rest := s.toList
  while !rest.isEmpty do
    let c := rest.head!
    if pred c then break
    acc := acc.push c
    rest := rest.tail
  return (acc, String.mk rest)

/-- Match a literal prefix. Returns `some rest` on success, `none`
    otherwise. -/
private def stripPrefix (s pfx : String) : Option String :=
  if s.startsWith pfx then some (s.drop pfx.length).toString else none

/-- Try to match a balanced delimiter pair on the *prefix* of `s`,
    where `open` and `close` are non-empty markers. Returns the
    content between them and the remainder of the string after the
    closing marker. -/
private def matchPair (s opn cls : String) : Option (String × String) := do
  let body ← stripPrefix s opn
  -- Find the closing marker. Naive search; emphasis runs are short.
  let cs := body.toList
  let pos? := cs.length.fold (init := none) fun i _ acc =>
    match acc with
    | some _ => acc
    | none   =>
      let rest := String.mk (cs.drop i)
      if rest.startsWith cls then some i else none
  match pos? with
  | none     => none
  | some pos =>
    let content := String.mk (cs.take pos)
    let after   := (String.mk (cs.drop pos)).drop cls.length
    some (content, after.toString)

/-! ## Top-level walk

We greedy-match the longest known marker at the current position:
`**…**`, `*…*`, `` `…` ``, `[label](url)`, `![alt](url)`, trailing
`\` for `<br>`. Anything else becomes a plain text run. -/

mutual

/-- Tokenise the remainder of an inline string. -/
partial def parse (s : String) : List Inline :=
  if s.isEmpty then [] else
  /- Image first because the leading `!` would otherwise be plain text. -/
  match matchPair s "![" "]" with
  | some (alt, rest) =>
    match matchPair rest "(" ")" with
    | some (url, rest') => .image alt url :: parse rest'
    | none              => .text "!" :: parse (s.drop 1).toString
  | none =>
    /- Bold (**…**) — must be tried before italic (*…*). -/
    match matchPair s "**" "**" with
    | some (body, rest) => .bold (parse body) :: parse rest
    | none =>
      match matchPair s "*" "*" with
      | some (body, rest) => .italic (parse body) :: parse rest
      | none =>
        match matchPair s "`" "`" with
        | some (body, rest) => .code body :: parse rest
        | none =>
          match matchPair s "[" "]" with
          | some (label, rest) =>
            match matchPair rest "(" ")" with
            | some (url, rest') => .link (parse label) url :: parse rest'
            | none              => textRun s
          | none => textRun s

/-- Read a plain text run up to the next *potential* marker, then
    recurse. Markers we react to: `*`, `_`, `` ` ``, `[`, `!`, `\`. -/
partial def textRun (s : String) : List Inline :=
  if s.isEmpty then [] else
  let cs := s.toList
  /- Consume at least one character so we make progress on `*` etc.
     when the surrounding context didn't form a valid pair. -/
  let (head, tail) :=
    match cs with
    | []      => ("", "")
    | c :: rs =>
      /- A trailing backslash followed by end-of-line becomes a <br>. -/
      if c == '\\' && rs.isEmpty then ("", "<br/>")
      else (c.toString, String.mk rs)
  if tail == "<br/>" then [.br]
  else
    let (more, rest) := takeUntil tail (fun c =>
      c == '*' || c == '`' || c == '[' || c == '!' || c == '\\')
    .text (head ++ more) :: parse rest

end

end LeanTea.Markdown.Inline
