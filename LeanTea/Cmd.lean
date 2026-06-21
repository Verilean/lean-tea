namespace LeanTea

/-- A `Cmd msg` is a (possibly empty) bundle of side effects, each of which
    yields a message that is dispatched to `update`. Modelled after Elm's
    `Cmd`. -/
structure Cmd (msg : Type) where
  effects : List (IO (Option msg))

namespace Cmd

def none {msg : Type} : Cmd msg := ⟨[]⟩

def batch {msg : Type} (xs : List (Cmd msg)) : Cmd msg :=
  ⟨xs.foldr (fun c acc => c.effects ++ acc) []⟩

/-- Lift a pure message into a Cmd. -/
def msg {α : Type} (m : α) : Cmd α := ⟨[pure (Option.some m)]⟩

/-- Run an arbitrary IO action and ignore its result. -/
def perform {msg : Type} (act : IO Unit) : Cmd msg :=
  ⟨[act *> pure Option.none]⟩

/-- Run an IO action whose result becomes a message. -/
def task {msg : Type} (act : IO msg) : Cmd msg := ⟨[Option.some <$> act]⟩

end Cmd
end LeanTea
