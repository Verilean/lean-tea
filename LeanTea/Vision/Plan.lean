import LeanTea.Vision.Explore

/-! # LeanTea.Vision.Plan — a broad test instruction as a list of sub-goals

A small model explores well one short goal at a time ("open the Quest
screen and check Quest Select is shown") and loses its way on a broad
one ("clear every section of this chapter"). So a broad instruction is
first turned into a plan: short sub-goals, with loops for "every item"
or "until none is left". The plan is plain text the user can edit
before running it:

```
Open the chapter map
repeat until "Are all sections cleared?" max 5:
  Click the next uncleared section and press Next
  Start the battle and wait for the result
end
```

`qa_studio` then explores each sub-goal in turn (`Explore.explore`)
and records a loop as `repeat` … `end_repeat` steps: the first round is
explored, later rounds replay the recorded steps. -/

namespace LeanTea.Vision.Plan

open Lean (Json)
open LeanTea.Vision
open LeanTea.Vision.Qa
open LeanTea.Llm.Openai (chat userTextAndImage)

inductive Item where
  | goal (text : String)
  /-- Repeat `body` until the yes/no `question` is yes (at most `max`
      rounds). -/
  | loop (question : String) (max : Nat) (body : List Item)
  deriving Inhabited, Repr, BEq

/-! ## Text form -/

partial def render (items : List Item) (indent : String := "") : String :=
  String.intercalate "\n" (items.map fun
    | .goal t => indent ++ t
    | .loop q mx body =>
      s!"{indent}repeat until \"{q}\" max {mx}:\n" ++ render body (indent ++ "  ") ++ s!"\n{indent}end")

/-- `repeat until "<question>" [max N]:` → `(question, max)`. -/
def parseRepeat (line : String) : Option (String × Nat) := do
  let rest ← (line.dropPrefix? "repeat until").map (·.toString.trimAscii.toString)
  let rest := if rest.endsWith ":" then (rest.dropEnd 1).toString.trimAscii.toString else rest
  let (q, after) ←
    if rest.startsWith "\"" then
      match (rest.drop 1).toString.splitOn "\"" with
      | q :: tail => some (q, String.intercalate "\"" tail)
      | [] => none
    else
      match rest.splitOn " max " with
      | [q] => some (q, "")
      | q :: tail => some (q, " max " ++ String.intercalate " max " tail)
      | [] => none
  let mx := match (after.trimAscii.toString.dropPrefix? "max").map (·.toString.trimAscii.toString.toNat?) with
    | some (some n) => n
    | _ => 10
  if q.trimAscii.isEmpty then none else some (q.trimAscii.toString, max 1 mx)

/-- Parse the text form. Blank lines and `#` comments are skipped;
    indentation is only for reading (`end` closes a loop). -/
def parse (text : String) : Except String (List Item) := do
  -- a stack of open loops: (question, max, items so far)
  let mut stack : List (String × Nat × Array Item) := []
  let mut top : Array Item := #[]
  for (raw, n) in (text.splitOn "\n").zipIdx do
    let line := raw.trimAscii.toString
    if line.isEmpty || line.startsWith "#" then continue
    if line.startsWith "repeat until" then
      let some (q, mx) := parseRepeat line
        | throw s!"line {n + 1}: write it as repeat until \"<yes/no question>\" max <N>:"
      stack := (q, mx, top) :: stack
      top := #[]
    else if line == "end" || line == "end repeat" then
      match stack with
      | (q, mx, outer) :: rest =>
        top := outer.push (.loop q mx top.toList)
        stack := rest
      | [] => throw s!"line {n + 1}: `end` without a `repeat until`"
    else
      top := top.push (.goal line)
  if !stack.isEmpty then throw "a `repeat until` has no `end`"
  return top.toList

/-! ## Asking the model for a plan -/

partial def fromJson : Json → List Item
  | .arr xs => xs.toList.flatMap fun
    | .str t =>
      let t := t.trimAscii.toString
      -- "Repeat until all … are done" as a plain line, next to a real block
      if t.isEmpty || t.toLower.startsWith "repeat" then [] else [.goal t]
    | j@(.obj _) =>
      let q := (j.getObjValD "repeat_until").getStr?.toOption.getD ""
      let mx := match j.getObjValD "max" with
        | .num n => n.mantissa.toNat / 10 ^ n.exponent
        | _ => 10
      if q.isEmpty then [] else [.loop q (max 1 mx) (fromJson (j.getObjValD "steps"))]
    | _ => []
  | _ => []

def prompt (instruction : String) (notes : List String) : String :=
  let noteText := if notes.isEmpty then "" else
    "Notes:\n" ++ String.intercalate "\n" (notes.map ("- " ++ ·)) ++ "\n"
  -- a worked example from another kind of app: without one a 4B model
  -- echoes the format's placeholder as a one-line plan
  s!"You are writing a UI test plan for the app in the screenshot.\nInstruction: {instruction}\n{noteText}" ++
  "Write the plan as a list of short sub-goals in order, from the screen shown to the end of the instruction. " ++
  "A sub-goal is one or two clicks and what to check after them.\n" ++
  "If the instruction says to do something for every item or until nothing is left, put the sub-goals that are " ++
  "repeated in a repeat block, with a yes/no question that is YES when the repetition is finished.\n" ++
  "Example, for the instruction \"Delete every draft email\" on a mail app:\n" ++
  "{\"plan\": [\"Open the Drafts folder and check that the draft list is shown\", " ++
  "{\"repeat_until\": \"Is the draft list empty?\", \"max\": 20, \"steps\": [\"Open the first draft\", " ++
  "\"Click Delete and check that the draft list is shown again\"]}]}\n" ++
  "Reply with ONLY the JSON object for the instruction above."

/-- A plan for `instruction`, made from the current screen. -/
def make (c : Ctx) (img : Image) (instruction : String) (notes : List String := []) : IO (List Item) := do
  let res ← chat c.vlm.cfg {
    model := c.vlm.model,
    messages := [userTextAndImage (prompt instruction notes) (← img.dataUrl)],
    temperature := some 0.0, maxTokens := some 600 }
  let text := stripThink res.content
  let some obj := Explore.extractObject text
    | throw <| IO.userError s!"no plan in the reply: {text.take 200}"
  let j ← IO.ofExcept (Json.parse obj)
  let items := fromJson (j.getObjValD "plan")
  if items.isEmpty then throw <| IO.userError s!"empty plan: {text.take 200}"
  return items

end LeanTea.Vision.Plan
