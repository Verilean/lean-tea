# Security — Secure by Construction

LeanTEA's *killer* property is that the compiler doesn't only check
that your code runs; it checks that whole **classes of vulnerabilities
can't even be expressed**. This file is the design document for that
property — what's shipped, what's planned, what the threat model is,
and how the type-level primitives work.

> **Status (2026-06-21)**: TEA / Persist / Rpc / Mcp surface shipped;
> **`Proof of Authorization` and `SafeQuery` v1 shipped** (see
> Primitives 1 and 3 below); `SafeHtml` and the State Machine Proof
> primitives are the remaining items. See [ROADMAP.md](ROADMAP.md) for
> sequencing.

## Threat model

We aim to make **four specific classes of vulnerability unrepresentable
in user code that compiles**:

1. **SQL / command injection** — concatenating user input into a query.
2. **Reflected / stored XSS** — putting untrusted text on the page
   without escaping.
3. **Authorization bypass** — a handler runs without confirming the
   caller's role.
4. **Invalid state transitions** — paying out below zero, double-spending,
   accepting an order from a deleted account.

Out of scope:

- Bugs inside the auth middleware itself (we narrow this gap by keeping
  the trusted core ~50 lines)
- OS / runtime / DB vulnerabilities
- Side channels (timing, cache)
- Cryptographic primitive flaws (we use stdlib + audited C)

The argument is the classic Phantom Type one: we don't prove the *whole
system* is safe, we prove that **anything user code does at the
framework boundary** is. That alone collapses the audit cost an order
of magnitude.

## Primitive 1 · `SafeQuery` — SQL injection elimination *(shipped)*

### Problem

Today the framework's untyped `Repo.query` accepts a raw `String`:

```lean
let xs ← repo.query s!"SELECT * FROM users WHERE id = {userInput}" #[]
```

This is a footgun and exactly the shape LLM-generated code reaches for.
`SafeQuery` makes that shape unrepresentable: the new typed surface
won't accept a `String` at all.

### How it works

The typed AST lives in `LeanTea.Persist.SafeQuery`. Every value enters
through a smart constructor that demands a typed `Col E α` and a
`ToValue α` instance:

```lean
structure Col (E : Type) (α : Type) [Entity E] where
  name : String

-- Smart constructor — the only path from a typed value into a Where.
def Col.eq {E α} [Entity E] [ToValue α] (c : Col E α) (v : α) : Where E := …
```

The `Where` inductive's leaf constructors (`.eq`, `.inList`, `.like`, …)
are `private` to the `SafeQuery.lean` file. A misguided
`Where.eq "email" rawUserInput` from anywhere else in the codebase is a
**`Unknown constant` compile error**. The only paths in are
`Col.eq`, `Col.lt`, `Col.inList`, `Col.like` (typed), or the audited
`.trusted` escape hatch.

```lean
namespace UserCols
  def email   : Col User String := ⟨"email"⟩
  def deleted : Col User Bool   := ⟨"deleted"⟩
end UserCols

-- Build a typed predicate and run it.
let q : Select User :=
  { where_  := .and (UserCols.email.eq "alice@x.com")
                    (.not (UserCols.deleted.eq true)),
    orderBy := [],
    limit   := some 10 }
let rows ← SafeQuery.run users q
```

### What ships in v1

- `Where E` AST with `eq`, `ne`, `lt`, `gt`, `inList`, `like` (prefix /
  suffix / contains), `isNull`, `and`, `or`, `not`.
- `Select E` with typed `where_` / `orderBy` / `limit` / `offset`.
- `Update E` with `[ UserCols.name .= "x", UserCols.flag .= true ]`
  list literal — the `.=` operator builds a `SetClause E`.
- `Delete E`.
- `SafeQuery.run` / `.count` / `.update` / `.delete` runners.
- `inList` with **variable-length placeholders** rendered positionally;
  empty list folds to `1=0` to avoid the `IN ()` syntax error.
- `.trusted decl_name% "…raw SQL…"` audited escape hatch — `decl_name%`
  resolves to the fully-qualified name of the enclosing declaration,
  so `grep -rn '\.trusted'` lists every audit point.

### Deliberately out of scope for v1

- **JOIN** across multiple entities. Rationale: a typed JOIN builder
  inflates the AST and the user-visible API by ~3×; for v1 we chose
  the FP-developer-friendly story "90% of CRUD is typed; complex
  multi-table SQL is `.trusted` with an audit tag." Audit cost
  collapses to `grep`. Revisit for v1.1.
- **`SUM` / `MAX` / `GROUP BY`** — the `Repo.count` shortcut covers the
  pagination case (`Repo.count : Where E → IO Nat`). Other aggregates
  go through `.trusted`.

### Trusted core

`SafeQuery.lean` is 260 LOC total; the trusted core (the
`renderWhere` / `Select.render` / `Update.render` / `Delete.render`
fragment) is ~80 LOC. The smart constructors and `Repo.run`-style
runners are thin shells around it. The whole module is reviewable in
under an hour.

### Worked example

`examples/Smoke/SafeQuery.lean` runs eight scenarios end-to-end and
shows the emitted SQL + bound parameters for each.

## Primitive 2 · `SafeHtml` — XSS elimination

### Problem

`LeanTea.Html` is *almost* there — `Html.text` already escapes strings,
and the typed constructors generate well-formed markup. The hole is
attribute values:

```lean
a_ [("href", userInput)] [text "click"]   -- userInput could be `javascript:…`
```

### Design

```lean
/-- Strings that have passed through a sanitiser. -/
structure SafeAttr where
  private mk :: value : String

namespace SafeAttr
  /-- URL attribute — only `https://`, `http://`, `mailto:`, `#`, `/` schemes. -/
  def url (raw : String) : Except String SafeAttr := …
  /-- Plain text attribute — strips `<`, `>`, quotes. -/
  def text (raw : String) : SafeAttr := …
  /-- Numeric attribute. -/
  def num (n : Int) : SafeAttr := ⟨toString n⟩
end SafeAttr
```

Then:

```lean
a_ [("href", urlSafe.value)] [Html.text userInput]
```

`Html.attr` is changed from `(String × String)` to `(String × SafeAttr)`.
The old form becomes a compile error unless the value is a string
literal (which we audit specially — same `Lean.Name` trick).

### What it changes

- `Html.attr` requires `SafeAttr` instead of `String`.
- `Html.text` escapes the same as today (no API change).
- Old code mechanically migrates: `("href", x)` → `("href", ← SafeAttr.url x)`.

### Effort

~80 LOC for the type + sanitisers; ~120 LOC migrating the existing
typed Html DSL call sites.

## Primitive 3 · `Proof of Authorization` *(shipped)*

The killer demo. A handler that requires admin access *cannot be
called* without first proving the caller is an admin. The proof is a
value that only the auth middleware can mint.

### Capability lattice

```lean
inductive Capability where
  | guest
  | user
  | admin
  | owner (resourceId : String)   -- dependent on a runtime resource id
  deriving DecidableEq

structure Proof (c : Capability) where
  private mk :: subject : String

/-- Lattice: prove `c1 ≤ c2` to widen a proof. -/
class HasCapability (c1 c2 : Capability) : Prop
instance : HasCapability .admin .user  := ⟨⟩
instance : HasCapability .admin .guest := ⟨⟩
instance : HasCapability .user  .guest := ⟨⟩
instance : HasCapability c      c      := ⟨⟩

def Proof.weaken {c1 c2 : Capability} [HasCapability c1 c2]
    (p : Proof c1) : Proof c2 := ⟨p.subject⟩
```

### Issuance (the single trusted entry point)

```lean
/-- Only function that can fabricate a `Proof`. Verifies session +
    role from `AuthStore`. ~30 LOC of trusted code. -/
def Proof.issue (auth : AuthStore) (req : Request) (c : Capability)
    : IO (Except String (Proof c)) := …
```

### Usage — static capabilities

```lean
def handleAdminDelete (proof : Proof .admin) (id : Nat) (store : Store)
    : IO Response := do
  /- Reachable only by paths where `proof : Proof .admin` exists.
     Drop the parameter and the call site won't compile. -/
  store.shapes.delete id
  return Response.text 200 s!"deleted {id} by {proof.subject}"
```

### Usage — dynamic capabilities (the dependent-type punchline)

The naïve API `Proof (.owner "doc-42")` doesn't generalise. The right
shape uses Lean's dependent types:

```lean
def handleEdit (id : String) (proof : Proof (.owner id))
    (newText : String) : IO Response := …
```

The type of `proof` **depends on the runtime value of `id`**. The path
that extracts `id` from the URL and the DB row that confirms ownership
must agree on the same `id` — *the type system makes that consistency
a compile-time invariant*.

Neither TypeScript nor Rust can express this — they lack `Π`-types.
Haskell's `singletons` library gets close but needs heavyweight
encoding. Lean 4 expresses it directly.

### Endpoint integration (avoiding 200× boilerplate)

Don't make every handler call `Proof.issue` manually. Lift it into the
`Endpoint` record:

```lean
structure Endpoint (req res : Type) (c : Capability) where
  path    : String
  method  : Method
  handler : Proof c → req → IO res
```

The router runs `Proof.issue` once per request, returns 403 on failure,
and only then dispatches:

```lean
def dispatch (auth : AuthStore) (e : Endpoint req res c)
    (req : Request) : IO Response := do
  match ← Proof.issue auth req c with
  | .error _ => return Response.forbidden
  | .ok p =>
    let parsed ← parseRequest e req
    let result ← e.handler p parsed
    return renderResponse result
```

### The heterogeneous-list problem

A naïve `List (Endpoint req res c)` won't hold endpoints with different
capabilities. Three Lean-idiomatic answers:

**Option A — Sigma type (recommended)**

```lean
abbrev SomeEndpoint := Σ c, Endpoint req res c

def routes : List SomeEndpoint := [
  ⟨.guest, listShapesEp⟩,
  ⟨.user,  addShapeEp⟩,
  ⟨.admin, deleteShapeEp⟩
]
```

The capability is erased from the static type but preserved at runtime
in the Sigma's first component. Dispatch reads `c` off the Sigma and
calls `Proof.issue` with it. Simplest, least clever, no macros.

**Option B — `HList` over an indexed family**

```lean
inductive Routes : List Capability → Type where
  | nil  : Routes []
  | cons : Endpoint req res c → Routes cs → Routes (c :: cs)
```

Preserves capabilities at the type level — useful if a metaprogram
wants to enumerate them, less useful for the running router.

**Option C — Existential wrapper**

```lean
inductive AnyEndpoint where
  | mk : (c : Capability) → Endpoint req res c → AnyEndpoint
```

Equivalent to A in expressive power, slightly less ergonomic to
destructure.

**Decision**: ship Option A. It's the minimum Lean machinery for the
job and matches the way the rest of `LeanTea.Rpc` already works (lists
of records).

## Primitive 4 · `State Machine Proof` (deferred)

Most interesting at the domain layer (transactions, orders). Lean's
inductive types + dependent types make the shape obvious:

```lean
inductive OrderState where | draft | submitted | paid | shipped
inductive Transition : OrderState → OrderState → Type where
  | submit  : Transition .draft     .submitted
  | pay     : Transition .submitted .paid
  | ship    : Transition .paid      .shipped
```

A function `apply : Transition s s' → Order s → IO (Order s')` then
makes invalid transitions a compile error. Out of scope for the first
milestone — domain-shaped, not framework-shaped.

## Trusted Computing Base

The proofs above hold only if the trusted core is correct. We commit
to keeping it small and audit-able:

| Module | Approximate LOC | What it asserts |
|---|---|---|
| `LeanTea.Auth.AuthStore` | ~80 | Sessions match exactly one row, expiry is enforced |
| `LeanTea.Auth.Proof.issue` | ~30 | Builds `Proof c` only after verifying the session has role ≥ c |
| `LeanTea.Persist.SafeQuery.render` | ~50 | Parameter binding is positional + escaped |
| `LeanTea.Html.SafeAttr` | ~80 | Sanitisers reject URLs / attribute values that violate the scheme allow-list |
| **App-supplied `checkOwnership`** | varies | Authoritative owner check passed into `Proof.issueOwner`. Each app owns this and must guard it as carefully as the auth core — a `fun _ _ => return true` here defeats the entire owner-proof story. |
| **App-supplied `resolveRole`** | varies | Maps a `Session` to a static `Capability`. A misconfigured `resolveRole` that returns `.admin` for everyone defeats the static-capability story. Treat the role-lookup table as a security-critical asset. |

~240 LOC total. Reviewable in an afternoon. The interesting property:
**anything you build on top is automatically safe**, because the type
system rejects every call that bypasses these primitives.

## Open questions

1. **`SafeQuery` for joins** — the AST above is single-table. JOINs
   need a more elaborate `From` clause; do we ship a typed JOIN builder
   or accept `.trusted` for joins in the v1?
2. **PathParam parsing** — currently the URL `id` is `String`. For
   the dependent owner pattern to typecheck, we may need the framework
   to thread a `Decidable (parsedId = proofId)` proof. The simpler
   path: keep `id : String` throughout and rely on `Proof.issue`
   verifying ownership against the same string.
3. **CSP / HSTS headers** — automatic enforcement (similar to how
   Yesod injects security headers) belongs in the framework, not in
   user code. Likely a one-time wiring in `WebApp.run`.
4. **Mutation in the typeclass** — `SafeQuery` covers SELECT well;
   UPDATE / DELETE need a parallel design where row identifiers are
   typed.

These will be resolved during PoC implementation — see
[ROADMAP.md](ROADMAP.md) for sequencing.

## Selling it

The pitch isn't "we have type safety" — every modern language has that.
The pitch is:

> The compiler rejects code that violates these four properties.
> Compile succeeds → those four bug classes are gone, at no test cost.
> The auditor can verify the property by reading 240 lines.

Lean 4 specifically is the only host language where this is currently
buildable. Coq is too academic for industrial uptake; F\* is
research-grade; Idris is unsupported; ATS is unmaintained. Lean 4 has
LSP, an active community, a polished package manager (Lake), and a
maintainer (Microsoft Research → independent Lean FRO).

That gap is the moat.
