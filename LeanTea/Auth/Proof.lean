import LeanTea.Auth
import LeanTea.Net.Http

/-! # LeanTea.Auth.Proof — type-level authorisation proofs

The compiler refuses to call a handler that needs `Proof .admin` from a
path that doesn't carry one. The only function that can mint a proof is
`Proof.issue`, which verifies the request's session against the auth
store. **Drop the proof argument from a handler signature and the call
site stops compiling.**

This is the framework's `SECURITY.md` Primitive 3. Trusted core is
~50 LOC. -/

namespace LeanTea.Auth.Proof

open LeanTea LeanTea.Auth
open LeanTea.Net.Http (Request Response)

/-- Roles the framework knows about. `owner` is dependent on a runtime
    resource id — see `Proof.issueOwner` for the dependent-type punchline. -/
inductive Capability where
  | guest
  | user
  | admin
  | owner (resourceId : String)
  deriving DecidableEq, Repr, Inhabited

/-- Pretty-print for logs / error messages. -/
def Capability.toString : Capability → String
  | .guest => "guest"
  | .user  => "user"
  | .admin => "admin"
  | .owner r => s!"owner({r})"

instance : ToString Capability := ⟨Capability.toString⟩

/-- `dominates a b` iff a session with capability `a` is allowed to act
    as `b`. Lattice: admin > user > guest; `owner r` only dominates
    itself (you can't generalise from one resource to another). -/
def dominates : Capability → Capability → Bool
  | _, .guest          => true   -- everyone can act as guest
  | .admin, _          => true   -- admin dominates everything
  | .user, .user       => true
  | .owner a, .owner b => a == b
  | _, _               => false

/-- Type-class lattice for static widening. A `Proof c1` can be
    weakened to a `Proof c2` whenever the instance is in scope.

    We expose the instances that don't depend on a runtime resource id
    so widenings (`Proof .admin → Proof .user`) are trivial at the
    call site. Owner widenings happen via `issueOwner` directly. -/
class HasCapability (c1 c2 : Capability) : Prop where intro : True

instance : HasCapability c c := ⟨trivial⟩
instance : HasCapability .admin .user  := ⟨trivial⟩
instance : HasCapability .admin .guest := ⟨trivial⟩
instance : HasCapability .user  .guest := ⟨trivial⟩

/-- The unforgeable certificate that an authorised principal exists.
    `subject` identifies *who* the principal is (email / user id). -/
structure Proof (c : Capability) where
  /- private constructor — only `issue` / `issueOwner` can fabricate one. -/
  private mk ::
  subject : String

/-- Widen a proof to a smaller capability. Trivial at runtime; the
    typeclass lookup is what actually enforces correctness. -/
def Proof.weaken {c1 c2 : Capability} [HasCapability c1 c2]
    (p : Proof c1) : Proof c2 := ⟨p.subject⟩

/-! ## Issuance — the single trusted entry point. -/

/-- Look up `name=value` in a `cookie` header. -/
private def cookieLookup (cookies : String) (name : String) : Option String :=
  let pre := name ++ "="
  cookies.splitOn ";"
    |>.findSome? fun raw =>
      let trimmed := raw.trimAscii.toString
      if trimmed.startsWith pre then some (trimmed.drop pre.length).toString
      else none

/-- Verify the request's session covers capability `c`. `resolveRole`
    lets the caller map an authenticated `Session` to the framework's
    `Capability` (so apps stay in control of role storage). On
    success returns `Proof c`. -/
def Proof.issue (auth : AuthStore) (req : Request) (c : Capability)
    (resolveRole : Session → Capability) : IO (Except String (Proof c)) := do
  let cookies := req.header? "cookie" |>.getD ""
  let token := cookieLookup cookies "sid" |>.getD ""
  if token.isEmpty then return .error "no sid cookie"
  let now ← nowSec
  match ← auth.findSession token now with
  | none => return .error "invalid or expired session"
  | some s =>
    let userCap := resolveRole s
    if dominates userCap c then
      return .ok ⟨s.email⟩
    else
      return .error s!"insufficient capability: have {userCap}, need {c}"

/-- Dependent-type version for the `owner` capability. The resulting
    proof's type is `Proof (.owner resourceId)` — the runtime resource
    id is reflected in the static type. A handler that takes a
    `Proof (.owner id)` argument can only be called with a proof whose
    resource id matches `id` (modulo the resolver's correctness).

    `checkOwnership` is the caller's authoritative check (e.g.
    `SELECT 1 FROM docs WHERE id=? AND owner=?`). -/
def Proof.issueOwner (auth : AuthStore) (req : Request)
    (resourceId : String)
    (checkOwnership : Session → String → IO Bool)
    : IO (Except String (Proof (.owner resourceId))) := do
  let cookies := req.header? "cookie" |>.getD ""
  let token := cookieLookup cookies "sid" |>.getD ""
  if token.isEmpty then return .error "no sid cookie"
  let now ← nowSec
  match ← auth.findSession token now with
  | none => return .error "invalid or expired session"
  | some s =>
    if ← checkOwnership s resourceId then
      return .ok ⟨s.email⟩
    else
      return .error s!"not owner of {resourceId}"

/-! ## Endpoint integration

The framework's untyped `LeanTea.Rpc.Endpoint` doesn't know about
capabilities. `AuthRoute` is a thin typed wrapper that pairs an
endpoint + the capability it requires + a handler that *demands the
proof in its signature*. Dispatch runs `Proof.issue` once and only
calls the handler on success. -/

/-- An authorised route. The `c : Capability` parameter is in the type
    so different capability requirements give different `AuthRoute`
    types — exactly what makes "drop the proof argument" a compile
    error rather than a silent miss. -/
structure AuthRoute (c : Capability) where
  path    : String
  method  : String := "GET"
  /- The handler takes the proof. Demand the proof argument in the
     type and the compiler enforces it at every call site. -/
  handler : Proof c → Request → IO Response

/-- Sigma-wrapped routing list — capability is erased at the list
    level but preserved per-element. Pattern-match in dispatch and you
    get the runtime `Capability` to feed `Proof.issue`. -/
inductive AnyAuthRoute where
  | mk : (c : Capability) → AuthRoute c → AnyAuthRoute

/-- Smart constructor — Lean's elaborator picks `c` from the
    `AuthRoute`'s type. -/
def AnyAuthRoute.of {c : Capability} (r : AuthRoute c) : AnyAuthRoute := .mk c r

/-- Dispatch a list of `AnyAuthRoute` over a request, threading the
    auth store + role resolver. On capability failure returns 403. -/
def dispatchAuthorized
    (auth : AuthStore) (resolveRole : Session → Capability)
    (routes : List AnyAuthRoute)
    (fallback : Request → IO Response := fun _ => return Response.notFound)
    : Request → IO Response := fun req => do
  for ⟨c, r⟩ in routes do
    if r.path == req.path && r.method == req.method then
      match ← Proof.issue auth req c resolveRole with
      | .ok p    => return ← r.handler p req
      | .error e => return Response.text 403 s!"forbidden: {e}"
  fallback req

end LeanTea.Auth.Proof
