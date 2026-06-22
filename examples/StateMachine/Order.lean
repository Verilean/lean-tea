import LeanTea.StateMachine

/-! # examples/StateMachine/Order.lean — worked example for
    `LeanTea.StateMachine`.

Demonstrates the state-machine pattern with a five-state order
lifecycle. Imported by `examples/Tests/SecuritySpec.lean` so the
`SecuritySpec` runner asserts the legal transitions round-trip and
the illegal-transition compile error is documented in source. -/

namespace OrderSM

inductive State where
  | draft
  | submitted
  | paid
  | shipped
  | cancelled
  deriving DecidableEq, Repr

/-- Allowed lifecycle arrows. **The set of constructors is the spec**:
    nothing outside this list can build a `Transition`, so unauthorised
    state changes don't compile.

      * draft → submitted   (operator submits the cart)
      * submitted → paid    (payment provider confirms)
      * paid → shipped      (warehouse confirms handoff)
      * <any> → cancelled   (admin override; uses a polymorphic
                              source state) -/
inductive Transition : State → State → Type where
  | submit : Transition .draft     .submitted
  | pay    : Transition .submitted .paid
  | ship   : Transition .paid      .shipped
  | cancel : {s : State} → Transition s .cancelled
  deriving Repr

/-- Per-state payload. Fields only populated once the corresponding
    transition has fired — the optionality is documented, not
    type-enforced, to keep the example readable. -/
structure Order (s : State) where
  id       : Nat
  total    : Int := 0
  paidAmt  : Int := 0
  shippedAt: Nat := 0
  deriving Repr

/-- The *only* function that produces a new-state order. Pattern-
    matching exhaustively across the GADT constructors means the
    compiler tells you what to do for each new transition you add. -/
def Order.apply : Transition s s' → Order s → Order s'
  | .submit, o =>
    { id := o.id, total := o.total }
  | .pay,    o =>
    { id := o.id, total := o.total, paidAmt := o.total }
  | .ship,   o => Id.run do
    /- A real app would call `nowSec` here; for the smoke we use a
       fixed timestamp so the assertion is deterministic. -/
    return { id := o.id, total := o.total, paidAmt := o.paidAmt, shippedAt := 1_700_000_000 }
  | .cancel, o =>
    { id := o.id }

/-! ## Worked example -/

/-- A fresh order. -/
def fresh (id : Nat) (total : Int) : Order .draft :=
  { id, total }

end OrderSM
