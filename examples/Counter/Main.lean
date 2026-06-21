import LeanTea

open LeanTea

structure Model where
  count : Int
  done  : Bool

inductive Msg where
  | inc
  | dec
  | quit
  | noop

def view (m : Model) : String :=
  s!"┌─ LeanTea Counter ────────────┐\n│  count = {m.count}\n│\n│  [+] inc   [-] dec   [q] quit\n└──────────────────────────────┘"

def parse (line : String) : Msg :=
  match line with
  | "+" => Msg.inc
  | "-" => Msg.dec
  | "q" => Msg.quit
  | _   => Msg.noop

def update : Msg → Model → Model × Cmd Msg
  | .inc, m  => ({ m with count := m.count + 1 }, Cmd.none)
  | .dec, m  => ({ m with count := m.count - 1 }, Cmd.none)
  | .quit, m => ({ m with done := true }, Cmd.none)
  | .noop, m => (m, Cmd.none)

def app : App Model Msg :=
  { init   := ({ count := 0, done := false }, Cmd.none)
    update := update
    view   := view
    subs   := fun _ => Sub.onStdin parse
    isDone := fun m => m.done }

def main : IO Unit := LeanTea.run app
