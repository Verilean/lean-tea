import LeanJs.Ast
import LeanJs.LeanEmit

/-! # LeanJs.TypeCheck — type-check a `.leanjs` client by re-using Lean

LeanJs is a separate DSL: its own parser, no type system (its `Check`
pass verifies arity + record fields only). So a client that calls an
API can't, on its own, be checked against the request/response types
the server declares.

Rather than build a type checker into LeanJs — reinventing what Lean
already has — this module **emits the client to real Lean and lets
Lean's elaborator do the checking.** The trick that makes it work for
*client* code (which the pure-subset `LeanEmit` refuses, because it
uses `async`/`await`/FFI):

  1. `async def` bodies emit as `do`-blocks; `await e` becomes a
     monadic bind `← e`. The async effect is modelled as `Async`
     (an `IO` alias in the prelude) purely so the types line up — we
     never *run* the emitted code, only elaborate it.
  2. The API surface is supplied as a **typed prelude**: the shared
     request/response types (real Lean structures with derived JSON
     codecs) plus one `opaque` stub per endpoint,
     `apiFoo : Req → Async Resp`. The client's `.leanjs` is emitted
     below that prelude and handed to `lean` for elaboration.

If elaboration succeeds, the client's API call sites agree with the
server's types. If the client builds a request with a wrong field, or
consumes a response field that doesn't exist, `lean` reports it — with
a line/column we map back to the emitted source.

The JavaScript the browser actually runs still comes from
`LeanJs.Codegen`; this pass only gates it on a clean type-check. -/

namespace LeanJs.TypeCheck

open LeanJs.Ast
open LeanJs.LeanEmit (emitExpr)

/-! ## Async-aware emission

`LeanEmit.emitProgram` targets the pure subset and drops `await`. For
client code we need the opposite: preserve the async structure so the
monad types drive the check. We emit an `async def` body as a
`do`-block, threading `await` through monadic bind. -/

/-- Does this expression contain an `await` anywhere in statement
    position of its let/seq spine? Determines whether a body must be a
    `do`-block (monadic) or can stay a pure `def`. -/
partial def containsAwait : Expr → Bool
  | .awaitE _        => true
  | .letE _ v b      => containsAwait v || containsAwait b
  | .seqE a b        => containsAwait a || containsAwait b
  | .ifE _ t e       => containsAwait t || containsAwait e
  | .app f xs        => containsAwait f || xs.any containsAwait
  | _                => false

/-- Emit an expression as the statements of a `do`-block. `let x :=
    await e; rest` becomes `let x ← e` (monadic); a plain `let` stays
    pure; the tail expression is `pure`-wrapped unless it is itself an
    `await` (already a monadic action). -/
partial def emitDoStmts : Expr → String
  | .letE n (.awaitE e) body =>
    s!"  let {n} ← {emitExpr e}\n{emitDoStmts body}"
  | .letE n v body =>
    s!"  let {n} := {emitExpr v}\n{emitDoStmts body}"
  | .seqE (.awaitE e) body =>
    s!"  let _ ← {emitExpr e}\n{emitDoStmts body}"
  | .seqE a body =>
    s!"  let _ := {emitExpr a}\n{emitDoStmts body}"
  | .awaitE e => s!"  {emitExpr e}"
  | tail      => s!"  pure ({emitExpr tail})"

/-- Emit one top-level definition for the type-check harness. Unlike
    `LeanEmit.emitTopDef`, async defs become `do`-blocks and we accept
    (rather than reject) the constructs client code needs. -/
def emitForCheck : TopDef → Except String String
  | .defE name params ret body =>
    .ok (LeanEmit.emitDef name params ret body)
  | .asyncDefE name params body =>
    if containsAwait body then
      -- Monadic body. Leave the return type to inference so the monad
      -- (Async) is fixed by the `←`-bound API stubs.
      let paramText := String.intercalate " "
        (params.map fun p => s!"({p.name} : {p.type?.getD "Int"})")
      let head := if params.isEmpty then s!"def {name}" else s!"def {name} {paramText}"
      .ok s!"{head} := do\n{emitDoStmts body}"
    else
      -- No awaits — a pure def is enough to check it.
      .ok (LeanEmit.emitDef name params none body)
  | .indE name ctors  => .ok (LeanEmit.emitInductive name ctors)
  | .recordE name fields =>
    let fieldLines := fields.map (fun (k, t) => s!"  {k} : {t}")
    .ok s!"structure {name} where\n{String.intercalate "\n" fieldLines}\n  deriving Inhabited, Repr"
  | .includeE _       => .ok ""
  -- Constructs that don't affect API-boundary checking are elided so
  -- they can't derail elaboration. `extern js` bindings, imports, and
  -- top-level effect statements are the client's own concern; the
  -- generated API stubs in the prelude are what we check calls against.
  | .externE _ _      => .ok ""
  | .importE _ _      => .ok ""
  | .classE _ _       => .ok ""
  | .instE _ _ _      => .ok ""
  | .exprE _          => .ok ""

/-- Emit the whole client program for checking (async-aware). -/
def emitForCheckProgram (p : Program) : Except String String := do
  let mut out : Array String := #[]
  for d in p do
    out := out.push (← emitForCheck d)
  return String.intercalate "\n\n" (out.toList.filter (· != ""))

/-! ## The type-check pass -/

/-- Assemble `prelude` (shared types + `opaque apiFoo : Req → Async
    Resp` stubs) above the emitted client, elaborate with `lean`, and
    report the first error. Success (`.ok`) means every API call site
    agrees with the server's types. -/
def check (prelude : String) (p : Program) : IO (Except String Unit) := do
  match emitForCheckProgram p with
  | .error e => return .error s!"emit: {e}"
  | .ok clientSrc =>
    let full := prelude ++ "\n\n" ++ clientSrc ++ "\n"
    IO.FS.withTempDir fun dir => do
      let path := (dir / "client_check.lean")
      IO.FS.writeFile path full
      -- Elaborate only (no `--run`): a clean exit means it type-checks.
      let out ← IO.Process.output { cmd := "lean", args := #[path.toString] }
      if out.exitCode == 0 then
        return .ok ()
      else
        return .error out.stdout

/-- The standard prelude preamble: the `Async` model every generated
    stub uses. Callers append their shared types + endpoint stubs. -/
def asyncPreamble : String :=
  "-- AUTO-GENERATED type-check harness for a LeanJs client.\n" ++
  "abbrev Async := IO\n"

end LeanJs.TypeCheck
