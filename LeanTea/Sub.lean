namespace LeanTea

/-- A subscription describes input the runtime should listen for and
    convert into messages. This minimal version supports two kinds:
    stdin lines and periodic tick events. -/
inductive Source where
  | stdin
  | tick (ms : Nat)
  deriving Repr, BEq

structure Sub (msg : Type) where
  /-- For each source, a function turning a payload (the raw stdin line or
      the elapsed tick count) into an optional message. `Option.none`
      ignores the event. -/
  handlers : List (Source × (String → Option msg))

namespace Sub

def none {msg : Type} : Sub msg := ⟨[]⟩

def onStdin {msg : Type} (f : String → msg) : Sub msg :=
  ⟨[(Source.stdin, fun s => Option.some (f s))]⟩

def onTick {msg : Type} (ms : Nat) (f : Nat → msg) : Sub msg :=
  ⟨[(Source.tick ms, fun s => Option.some (f s.toNat!))]⟩

def batch {msg : Type} (xs : List (Sub msg)) : Sub msg :=
  ⟨xs.foldr (fun s acc => s.handlers ++ acc) []⟩

end Sub
end LeanTea
