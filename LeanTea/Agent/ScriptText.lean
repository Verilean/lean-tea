import LeanTea.Agent.Script

/-! # LeanTea.Agent.ScriptText — a script as lines of text

The JSON form is for tools; this one is for people editing a recorded
test. One step per line, `#` starts a note, `## <goal>` heads the steps
recorded for a goal, and a loop's body is indented (only for reading):

```
## Open the quest screen
wait_until "Is the Quest button visible on the screen?" 30s
click 176 711                      # the user pointed here
click "the Start button"
check "Is the quest list shown?"
check not "Is an error dialog shown?"
repeat until "Are all stages cleared?" max 5
  click "the next stage"
  wait 2s
end
```

| line | step |
|---|---|
| `click X Y` | `click_xy` |
| `click "target" [key K]` | `click_described` |
| `click_known K` | `click_known` |
| `wait 500ms` / `wait 2s` | `wait` |
| `wait_until "question" [30s]` | `wait_until` (default 60 s) |
| `wait_for_screen "name" [30s]` | `wait_for_screen` |
| `check ["not"] "question"` | `assert_visual` |
| `check_text "target" "value"` | `assert_text` |
| `check_pixel X Y #rrggbb [tol N]` | `assert_pixel` |
| `key Enter` · `type "text"` · `screenshot ["path"]` | |
| `repeat until "question" [max N]` … `end` | a loop |
| `json {…}` | any step, as its JSON object |

A step with an `expect` is written as `json`, which keeps every field. -/

namespace LeanTea.Agent.Script

open Lean (Json)

/-! ## Writing -/

def quote (s : String) : String :=
  "\"" ++ (s.replace "\\" "\\\\" |>.replace "\"" "\\\"") ++ "\""

def duration (ms : Nat) : String :=
  if ms ≥ 1000 && ms % 1000 == 0 then s!"{ms / 1000}s" else s!"{ms}ms"

/-- The line for an action; `none` when only `json` can say it. -/
def actionText : Action → Option String
  | .clickXy x y => some s!"click {x} {y}"
  | .clickDescribed t k => some (s!"click {quote t}" ++ (k.map (s!" key {quote ·}")).getD "")
  | .clickKnown k => some s!"click_known {quote k}"
  | .wait ms => some s!"wait {duration ms}"
  | .waitUntil q ms => some s!"wait_until {quote q} {duration ms}"
  | .waitForScreen n ms => some s!"wait_for_screen {quote n} {duration ms}"
  | .assertVisual q want => some s!"check {if want then "" else "not "}{quote q}"
  | .assertText t e => some s!"check_text {quote t} {quote e}"
  | .assertPixel x y rgb tol => some s!"check_pixel {x} {y} {rgb} tol {tol}"
  | .key k => some s!"key {k}"
  | .typeText t => some s!"type {quote t}"
  | .screenshot p => some ("screenshot" ++ (p.map (s!" {quote ·}")).getD "")
  | .repeatStart q mx => some s!"repeat until {quote q} max {mx}"
  | .repeatEnd => some "end"
  | .toolCall .. => none

/-- The script as text (`parseText` reads it back). -/
def toText (steps : List Step) : String := Id.run do
  let mut out : Array String := #[]
  let mut goal : Option String := none
  let mut depth := 0
  for st in steps do
    if st.goal != goal then
      if !out.isEmpty then out := out.push ""
      out := out.push (match st.goal with | some g => s!"## {g}" | none => "##")
      goal := st.goal
    if let .repeatEnd := st.act then depth := depth - 1
    let body := match st.expect, actionText st.act with
      | none, some t => t
      | _, _ =>
        -- every field, minus the goal (the heading says it)
        let j := (Script.toJson { name := "", steps := [{ st with goal := none }] }).getObjValD "steps"
        let one : Json := match j with | .arr xs => xs[0]?.getD Json.null | _ => Json.null
        s!"json {one.compress}"
    let indent := String.ofList (List.replicate (2 * depth) ' ')
    let note := if st.note.isEmpty || body.startsWith "json" then "" else s!"  # {st.note}"
    out := out.push (indent ++ body ++ note)
    if let .repeatStart .. := st.act then depth := depth + 1
  return String.intercalate "\n" out.toList ++ (if out.isEmpty then "" else "\n")

/-! ## Reading -/

inductive Tok where
  | word (s : String)
  | str (s : String)
  deriving Inhabited, BEq, Repr

/-- `rrggbb…` after a `#`: a colour, not a note. -/
def isColour (rest : List Char) : Bool :=
  let hex := fun (d : Char) => d.isDigit || ('a' ≤ d.toLower && d.toLower ≤ 'f')
  rest.length ≥ 6 && (rest.take 6).all hex && (rest.drop 6).head?.all (fun d => d == ' ' || d == '\t')

/-- Words and quoted strings, and the note after an unquoted `#` (a
    `#` that starts a word followed by a hex colour is a colour). -/
partial def tokenize (line : String) : Except String (List Tok × String) :=
  let rec go (cs : List Char) (acc : Array Tok) : Except String (List Tok × String) :=
    match cs with
    | [] => .ok (acc.toList, "")
    | c :: rest =>
      if c == ' ' || c == '\t' then go rest acc
      else if c == '#' && !(isColour rest) then
        .ok (acc.toList, (String.ofList rest).trimAscii.toString)
      else if c == '"' then
        let rec str (cs : List Char) (s : Array Char) : Except String (String × List Char) :=
          match cs with
          | [] => .error "a quote is not closed"
          | '\\' :: d :: more => str more (s.push d)
          | '"' :: more => .ok (String.ofList s.toList, more)
          | d :: more => str more (s.push d)
        match str rest #[] with
        | .ok (s, more) => go more (acc.push (.str s))
        | .error e => .error e
      else
        let word := (c :: rest).takeWhile (fun d => d != ' ' && d != '\t')
        go ((c :: rest).drop word.length) (acc.push (.word (String.ofList word)))
  go line.toList #[]

def parseDuration (s : String) : Option Nat :=
  if let some n := s.dropSuffix? "ms" then n.toString.toNat?
  else if let some n := s.dropSuffix? "s" then (n.toString.toNat?).map (· * 1000)
  else s.toNat?

/-- One line (no note, not blank, not a heading) → its action. -/
def lineAction (line : String) (toks : List Tok) : Except String Action := do
  let dur := fun (d : Option Tok) (default : Nat) => match d with
    | some (.word w) => (parseDuration w).getD default
    | _ => default
  match toks with
  | [.word "click", .word x, .word y] =>
    match x.toNat?, y.toNat? with
    | some x, some y => return .clickXy x y
    | _, _ => throw "click X Y wants two numbers"
  | [.word "click", .str t] => return .clickDescribed t
  | [.word "click", .str t, .word "key", .str k] => return .clickDescribed t (some k)
  | [.word "click_known", .str k] | [.word "click_known", .word k] => return .clickKnown k
  | [.word "wait", .word d] =>
    let some ms := parseDuration d | throw "wait wants a time, e.g. wait 500ms or wait 2s"
    return .wait ms
  | .word "wait_until" :: .str q :: rest => return .waitUntil q (dur rest.head? 60000)
  | .word "wait_for_screen" :: .str n :: rest => return .waitForScreen n (dur rest.head? 30000)
  | [.word "check", .str q] => return .assertVisual q true
  | [.word "check", .word "not", .str q] => return .assertVisual q false
  | [.word "check_text", .str t, .str e] => return .assertText t e
  | .word "check_pixel" :: .word x :: .word y :: .word rgb :: rest =>
    let tol := match rest with | [.word "tol", .word n] => n.toNat?.getD 16 | _ => 16
    match x.toNat?, y.toNat? with
    | some x, some y => return .assertPixel x y rgb tol
    | _, _ => throw "check_pixel X Y #rrggbb [tol N]"
  | [.word "key", .word k] | [.word "key", .str k] => return .key k
  | [.word "type", .str t] => return .typeText t
  | [.word "screenshot"] => return .screenshot
  | [.word "screenshot", .str p] => return .screenshot (some p)
  | .word "repeat" :: .word "until" :: .str q :: rest =>
    let mx := match rest with | [.word "max", .word n] => n.toNat?.getD 10 | _ => 10
    return .repeatStart q (max 1 mx)
  | [.word "end"] | [.word "end", .word "repeat"] => return .repeatEnd
  | _ => throw s!"can't read: {line}"

/-- Read the text form back into steps. Errors name the line. -/
def parseText (text : String) : Except String (List Step) := do
  let mut out : Array Step := #[]
  let mut goal : Option String := none
  let mut depth : Int := 0
  for (raw, n) in (text.splitOn "\n").zipIdx do
    let line := raw.trimAscii.toString
    if line.isEmpty then continue
    if line.startsWith "##" then
      let g := (line.drop 2).toString.trimAscii.toString
      goal := if g.isEmpty then none else some g
      continue
    if line.startsWith "#" then continue
    let at_ := s!"line {n + 1}"
    if let some js := line.dropPrefix? "json " then
      let j ← (Json.parse js.toString).mapError (s!"{at_}: {·}")
      let sc ← (Script.fromJson (Json.mkObj [("name", "x"), ("steps", .arr #[j])])).mapError (s!"{at_}: {·}")
      let some st := sc.steps.head? | throw s!"{at_}: empty"
      out := out.push { st with goal := st.goal.orElse fun _ => goal }
      continue
    let (toks, note) ← (tokenize line).mapError (s!"{at_}: {·}")
    let act ← (lineAction line toks).mapError (s!"{at_}: {·}")
    match act with
    | .repeatStart .. => depth := depth + 1
    | .repeatEnd =>
      depth := depth - 1
      if depth < 0 then throw s!"{at_}: `end` without a `repeat until`"
    | _ => pure ()
    out := out.push { act, note, goal }
  if depth != 0 then throw "a `repeat until` has no `end`"
  return out.toList

end LeanTea.Agent.Script
