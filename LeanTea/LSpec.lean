/-! # LeanTea.LSpec — a minimal LSpec-shaped test harness

The Lean ecosystem has a real LSpec library (lecopivo/LSpec). We
opt for a tiny in-house version with the same surface so the doc
chapter that uses it doesn't carry a network-fetched dependency in
the build.

API:

```lean
def specs : LSpec :=
  group "addition" [
    it "is commutative"        (1 + 2 = 2 + 1),
    it "with zero is identity" (0 + 5 = 5)
  ]

def main := lspecIO specs
```

`group` and `it` are values; `lspecIO` runs them, prints a tree of
pass/fail markers, and exits non-zero if anything failed. The exit
code makes the harness suitable for CI gates like
`tools/run-docs.sh` and a forthcoming `tools/run-tests.sh`. -/

namespace LeanTea.LSpec

/-! ## Tree of specs -/

inductive LSpec where
  /-- One named assertion. -/
  | it    (name : String) (passed : Bool) (detail : String := "")
  /-- A grouping with a label. -/
  | group (name : String) (children : List LSpec)
  deriving Inhabited

/-- Constructor alias mirroring LSpec's surface. -/
def it (name : String) (passed : Bool) (detail : String := "") : LSpec :=
  .it name passed detail

/-- Constructor alias mirroring LSpec's surface. -/
def group (name : String) (children : List LSpec) : LSpec :=
  .group name children

/-! ## Reporter -/

/-- (passed, failed). -/
structure Counts where
  passed : Nat := 0
  failed : Nat := 0
  deriving Inhabited

private partial def go (depth : Nat) (s : LSpec) : StateT Counts IO Unit := do
  let indent := String.mk (List.replicate (depth * 2) ' ')
  match s with
  | .it name passed detail =>
    if passed then
      IO.println s!"{indent}✓ {name}"
      modify fun c => { c with passed := c.passed + 1 }
    else
      IO.println s!"{indent}✗ {name}"
      if !detail.isEmpty then IO.println s!"{indent}  → {detail}"
      modify fun c => { c with failed := c.failed + 1 }
  | .group name children =>
    IO.println s!"{indent}● {name}"
    for c in children do go (depth + 1) c

/-- Run a spec tree, print results, exit non-zero on any failure. -/
def lspecIO (s : LSpec) : IO UInt32 := do
  let ((), counts) ← (go 0 s).run {}
  IO.println ""
  IO.println s!"  {counts.passed} passed, {counts.failed} failed"
  return if counts.failed == 0 then 0 else 1

end LeanTea.LSpec
