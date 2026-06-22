/-! # LeanTea.StateMachine — type-level state-machine proofs

Closes **"invalid state transitions"** (paying a draft order, shipping
a cancelled one, double-spending a balance). The pattern Lean 4
gives you that mainstream languages can't:

  * `State : Type` is your domain's lifecycle (`draft / submitted /
    paid / shipped / cancelled`).
  * `Transition : State → State → Type` is a **GADT** whose
    constructors encode the *allowed* arrows (`submit : Transition
    .draft .submitted`).
  * A function that "applies" a transition is shaped
    `applyTransition : Transition s s' → Order s → Order s'`. The
    only way to reach `Order .shipped` is through the chain of
    legal arrows; **the type system rejects every other path**.

This module ships the **shape** (no concrete domain). Apps define
their own `State` enum + `Transition` GADT + payload structure.
The worked example lives in `examples/StateMachine/Order.lean` and
shows that the compiler refuses an "untyped" draft → paid jump.

> Why not ship a generic state machine library? Domain state names
> (`draft`, `submitted`, `paid`) only make sense in context, and
> Lean's dependent types make the per-domain definition tiny —
> ten lines for a five-state machine. -/

namespace LeanTea.StateMachine

/-! ## The shape

Every state machine in a LeanTEA app follows three steps. Here's
the abstract pattern with `S` parameterised — concrete apps drop
in their own `S` and constructors. -/

/-- (Phantom) **Statelet** — what an `Entity s` carries beyond the
    state tag. Apps usually drop a concrete `structure Order` in
    here, parameterised by the state. -/
abbrev Entity (S : Type) (carrier : S → Type) (s : S) : Type := carrier s

/-! ### Convenience builder

The pattern compiles to four lines per state machine. Spelled out:

```lean
-- 1. enumerate the lifecycle states
inductive OrderState where
  | draft | submitted | paid | shipped | cancelled
  deriving DecidableEq

-- 2. enumerate the *allowed* arrows as a GADT
inductive OrderTransition : OrderState → OrderState → Type where
  | submit : OrderTransition .draft     .submitted
  | pay    : OrderTransition .submitted .paid
  | ship   : OrderTransition .paid      .shipped
  | cancel : {s : OrderState} → OrderTransition s .cancelled  -- from any state

-- 3. carry per-state payload (or just use `Unit`)
structure Order (s : OrderState) where
  id    : Nat
  total : Int := 0           -- meaningful when s ≥ .submitted
  paid  : Int := 0           -- meaningful when s ≥ .paid

-- 4. one applier — the only way to change state
def Order.apply : OrderTransition s s' → Order s → Order s'
  | .submit, o => { id := o.id, total := o.total }
  | .pay,    o => { id := o.id, total := o.total, paid := o.total }
  | .ship,   o => { id := o.id, total := o.total, paid := o.paid }
  | .cancel, o => { id := o.id }
```

Try writing `Order.apply (.pay, draftOrder)` — the type checker
refuses, because `.pay : OrderTransition .submitted .paid` doesn't
match the `OrderTransition .draft ?` slot. The error is the
guarantee: the bug class can't be expressed. -/

end LeanTea.StateMachine
