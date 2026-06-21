import LeanTea
import English.QuizData

open LeanTea

structure Flat where
  rule     : String
  written  : String
  tts      : String
  katakana : String
  note     : String

def flatten (ls : List QuizData.Lesson) : List Flat :=
  ls.foldr (fun l acc =>
    (l.examples.map (fun e =>
      { rule := l.rule, written := e.written, tts := e.tts,
        katakana := e.katakana, note := e.note })) ++ acc) []

def items : List Flat := flatten QuizData.lessons

structure Model where
  idx     : Nat
  correct : Nat
  shown   : Bool
  done    : Bool

inductive Msg where
  | show
  | next
  | mark (good : Bool)
  | quit
  | noop

def card? (i : Nat) : Option Flat := items[i]?

def view (m : Model) : String :=
  if m.done then
    s!"おつかれさまでした。\n正解: {m.correct} / {items.length}\n"
  else
  match card? m.idx with
  | none => s!"終了。正解: {m.correct} / {items.length}\n"
  | some c =>
    let header := s!"問 {m.idx + 1} / {items.length}   [{c.rule}]"
    let body :=
      if m.shown then
        s!"  書き言葉: {c.written}\n  発音    : {c.tts}\n  カタカナ: {c.katakana}\n  メモ    : {c.note}\n\n  [o] 正解だった   [x] 間違えた   [q] 終了"
      else
        s!"  書き言葉: {c.written}\n\n  どう発音される？\n  [s] 答えを見る   [q] 終了"
    s!"┌─ Sound-Change Quiz ────────────────────────┐\n│ {header}\n├────────────────────────────────────────────┤\n{body}\n└────────────────────────────────────────────┘"

def parse (line : String) : Msg :=
  match line with
  | "s" => Msg.show
  | "o" => Msg.mark true
  | "x" => Msg.mark false
  | "q" => Msg.quit
  | _   => Msg.noop

def update : Msg → Model → Model × Cmd Msg
  | .show, m => ({ m with shown := true }, Cmd.none)
  | .mark good, m =>
      let next := m.idx + 1
      let done := next >= items.length
      let correct := if good then m.correct + 1 else m.correct
      ({ idx := next, correct := correct, shown := false, done := done }, Cmd.none)
  | .next, m => ({ m with idx := m.idx + 1, shown := false }, Cmd.none)
  | .quit, m => ({ m with done := true }, Cmd.none)
  | .noop, m => (m, Cmd.none)

def app : App Model Msg :=
  { init   := ({ idx := 0, correct := 0, shown := false, done := items.isEmpty }, Cmd.none)
    update := update
    view   := view
    subs   := fun _ => Sub.onStdin parse
    isDone := fun m => m.done }

def main : IO Unit := LeanTea.run app
