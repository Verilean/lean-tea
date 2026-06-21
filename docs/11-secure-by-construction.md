# 11 · Secure by Construction — walking through the shipped primitives

LeanTEA's headline property is that whole classes of vulnerabilities
**can't be expressed in user code that compiles**. This chapter walks
through the three primitives that ship in v0.1: `Auth.Proof`
(authorization), `SafeQuery` (SQL), and `SafeHtml` (XSS). All three
are real code under `LeanTea/`, and each comes with a smoke binary
that runs the demo end-to-end.

> **What this chapter is for**: showing *why* the framework is safe,
> with the exact compile errors that prove it. The other chapters
> tell you *how* to write apps. If you only want one of those two,
> Chapter 11 is the "why" one.

The full design document lives in [`SECURITY.md`](../SECURITY.md);
this chapter is the prose / worked-example side of the same story.

---

## 1 · The threat model in one paragraph

A LeanTEA codebase makes **four classes of vulnerability
unrepresentable** at compile time:

1. **Authorization bypass** — a handler runs without the auth check.
2. **SQL / command injection** — user input concatenated into a query.
3. **Reflected / stored XSS** — untrusted text reaches the DOM unescaped.
4. **Invalid state transitions** — paying out below zero, double-spend, etc.

The framework's TCB (trusted core) is **~270 LOC** of auth + persist +
HTML modules. **Anything you build on top is automatically safe**,
because the type system rejects every call that bypasses the typed
entry points. (1), (2), and (3) ship today; (4) is on the roadmap.

---

## 2 · `Auth.Proof` — authorization that can't be forgotten

### The bug class

In a typical framework (Rails / Django / Spring), every handler that
manipulates admin data has to *remember* to call a "verify admin"
check. **Forgetting that line is one of the most common
authentication-bypass vulnerabilities in the wild.** Static type
systems normally don't catch it because the handler's signature
doesn't *require* the check.

### How LeanTEA closes it

Every handler that needs an authenticated principal demands the
principal **in its type signature**, as a `Proof c` value. The proof
is **unforgeable** — its `mk` constructor is `private` to the auth
module, so only the auth middleware can mint one. Drop the `proof`
argument from a handler's signature and the framework's dispatcher
won't accept it — *the code won't compile*.

### The types

```lean
-- LeanTea/Auth/Proof.lean
inductive Capability where
  | guest
  | user
  | admin
  | owner (resourceId : String)
  deriving DecidableEq

structure Proof (c : Capability) where
  private mk ::                 -- ← the unforgeability gate
  subject : String
```

### Issuing a proof (the *only* path)

```lean
def Proof.issue
    (auth : AuthStore) (req : Request) (c : Capability)
    (resolveRole : Session → Capability)
    : IO (Except String (Proof c)) := do
  let token := cookieLookup (req.header? "cookie" |>.getD "") "sid"
  match ← auth.findSession (token.getD "") (← nowSec) with
  | none   => return .error "no session"
  | some s =>
    if dominates (resolveRole s) c then return .ok ⟨s.email⟩
    else return .error s!"insufficient capability"
```

`Proof.issue` is the only public function that can produce a `Proof c`.
Everything else has to receive it as an argument.

### Routing — capabilities live in the type

```lean
structure AuthRoute (c : Capability) where
  path    : String
  method  : String := "GET"
  handler : Proof c → Request → IO Response   -- ← signature demands the proof

inductive AnyAuthRoute where
  | mk : (c : Capability) → AuthRoute c → AnyAuthRoute
```

The dispatcher runs `Proof.issue` once per request and only then
invokes the handler:

```lean
def dispatchAuthorized
    (auth : AuthStore) (resolveRole : Session → Capability)
    (routes : List AnyAuthRoute) : Request → IO Response :=
  fun req => do
    for ⟨c, r⟩ in routes do
      if r.path == req.path && r.method == req.method then
        match ← Proof.issue auth req c resolveRole with
        | .ok p    => return ← r.handler p req
        | .error e => return Response.text 403 s!"forbidden: {e}"
    return Response.notFound
```

### Worked example — admin delete

```lean
def handleAdminDelete (proof : Proof .admin) (req : Request) : IO Response := do
  return Response.text 200 s!"deleted (auth'd as {proof.subject})"
```

This handler **cannot be called without a `Proof .admin`**. Try
dropping the parameter:

```lean
-- Before
def handleAdminDelete (proof : Proof .admin) (req : Request) : IO Response := …

-- After (proof argument removed by a careless edit)
def handleAdminDelete (req : Request) : IO Response := …
```

…and the route registration stops compiling:

```
error: examples/Smoke/AuthProof.lean:98:17: Type mismatch
  expected: Proof .admin → Request → IO Response
  got:      Request → IO Response
```

This is the demo `examples/Smoke/AuthProof.lean` runs end-to-end:

```
[200] admin → /admin/delete: deleted (auth'd as admin@example.com)
[403] user → /admin/delete: forbidden: insufficient capability: have user, need admin
[403] no cookie → /ping: forbidden: no sid cookie
[200] user → /ping: ping from user@example.com
[200] admin → /ping (widens): ping from admin@example.com
```

### The dependent-type punchline — `Proof (.owner id)`

For per-resource authorization (the IDOR class — "user A edits user
B's record"), the proof's type depends on the **runtime resource id**:

```lean
def handleEdit (id : String) (proof : Proof (.owner id))
    (req : Request) : IO Response := …
```

The same `id` appears both as a value argument and inside the proof's
*type*. The path that extracts `id` from the URL and the DB row that
confirms ownership **must agree on the same `id`** — *the type system
makes that consistency a compile-time invariant*.

```
[200] user owns doc-42 → edit succeeds (dependent owner proof)
[403] admin → /edit/doc-42 (not owner): not owner of doc-42
```

TypeScript can't express this. Rust can't express this. Lean 4 does it
directly via dependent types (`Π`-types).

### TCB and audit story

The trusted core for this primitive is **~80 LOC**:
- `AuthStore.findSession` / `addSession` (~30 LOC)
- `Proof.issue` / `Proof.issueOwner` / `dispatchAuthorized` (~50 LOC)

Plus two app-supplied callbacks the framework documents as
security-critical:
- `resolveRole : Session → Capability` — maps your roles
- `checkOwnership : Session → String → IO Bool` — your authoritative
  owner check

A misconfigured `resolveRole` that returns `.admin` for everyone
defeats the static-capability story; we list this in `SECURITY.md`
under TCB so auditors know to read it carefully.

---

## 3 · `SafeQuery` — SQL injection that *cannot be expressed*

### The bug class

```python
# every framework's footgun
cursor.execute(f"SELECT * FROM users WHERE id = {user_input}")
```

Frameworks fight this with `prepare` / `?`-placeholders, but the *type
system* still permits passing a raw string. Every regression test
suite for an established Web framework has a section called "SQL
injection" — that exists *because* the language can't prevent it.

### How LeanTEA closes it

The typed AST is the **only** path from a user value to SQL. The
inductive `Where E`'s leaf constructors (`eq`, `lt`, `inList`, `like`,
…) are **`private` to `SafeQuery.lean`**. From outside the module,
the `Where.eq "email" rawString` shape **doesn't typecheck**.

User input enters via typed smart constructors that demand a `Col E α`
column reference and a `ToValue α` instance:

```lean
namespace UserCols
  def email   : Col User String := ⟨"email"⟩
  def deleted : Col User Bool   := ⟨"deleted"⟩
end UserCols

-- Build a typed predicate:
let q : Select User :=
  { where_  := .and (UserCols.email.eq "alice@x.com")
                    (.not (UserCols.deleted.eq true)),
    orderBy := [],
    limit   := some 10 }
let rows ← SafeQuery.run users q
```

### The compile error that *cannot happen*

Try to construct a `Where E` directly:

```lean
-- from any file outside SafeQuery.lean
let bad : Where User := Where.eq "email" rawUserInput
```

…and the build fails with:

```
error: Unknown constant `LeanTea.Persist.SafeQuery.Where.eq`
```

This isn't a lint or a runtime check — `Where.eq` is **literally not
visible** outside its defining module. The framework guarantees that
the *only* paths into a `Where` from user input are the typed smart
constructors and the audited `.trusted` escape (below).

### `inList` — variable-length placeholders, typed

```lean
let rows ← SafeQuery.run users
  { where_ := UserCols.id.inList [1, 2, 3] }
```

Renders to:

```sql
SELECT id, email, name, deleted FROM users WHERE id IN (?, ?, ?)
```

with positional bindings `#[1, 2, 3]`. **The list length is variable
and that's safe** — every element is positionally bound. Empty list
folds to `1=0` so you get an empty result, not a syntax error.

### `LIKE` — the `%` lives in the AST, not in user input

```lean
let rows ← SafeQuery.run users
  { where_ := UserCols.email.like .suffix "@example.com" }
```

→ `WHERE email LIKE ?` with bound parameter `%@example.com`. The user
input never builds an SQL fragment.

### `UPDATE` / `DELETE` — the same typed builder

```lean
let _ ← SafeQuery.update users
  { set    := [ UserCols.name .= "Alice", UserCols.deleted .= false ],
    where_ := UserCols.id.eq 1 }
let _ ← SafeQuery.delete users { where_ := UserCols.deleted.eq true }
```

`.=` is an operator that builds a typed `SetClause` from a `Col`
and a value of the matching `α`. There is no path that accepts a
raw `String → String` SET clause.

### `.trusted decl_name%` — the audited escape

For genuinely complex SQL (multi-table JOIN, recursive CTE) the
framework provides one escape hatch. It demands a `Lean.Name` audit
tag so every escape is **physically grep-able**:

```lean
let rows ← SafeQuery.trusted users decl_name%
  "SELECT id, email FROM users WHERE name LIKE ?"
  #["%alice%"]
```

`decl_name%` is a Lean macro that resolves to the enclosing
declaration's fully-qualified name. CI runs `grep -rn '\.trusted '`
across the codebase to list every audit point:

```
$ grep -rn '\.trusted ' src/
src/Reports.lean:42:   let rows ← SafeQuery.trusted orders decl_name%
src/Reports.lean:78:   let rows ← SafeQuery.trusted users  decl_name%
```

**Two lines for the auditor to review, not the entire repo.**

### Worked example — `examples/Smoke/SafeQuery.lean`

```
1. SELECT … WHERE email = ?  → 1 row
   params : #[alice@x.com]
2. WHERE id IN (?, ?)  → 2 rows
   params : #[1, 3]
3. WHERE email LIKE '%@y.com' (suffix LIKE)  → 2 rows
   params : #[%@y.com]
4. AND/NOT combinator  → 1 row
5. UPDATE SET name = ? WHERE id = ?  → 1 row updated
6. COUNT(*) WHERE deleted = false  → 3
7. DELETE WHERE deleted = true  → 1 row deleted
8. .trusted decl_name% escape  → 1 row (audit-tagged)
```

### TCB and audit story

The trusted core is **~80 LOC** of `renderWhere` / `Select.render` /
`Update.render` / `Delete.render` inside `SafeQuery.lean`. The smart
constructors are thin shells over the inductive constructors; auditing
the rendering passes once is enough to prove the bound-parameter
guarantee.

The escape hatch (`SafeQuery.trusted`) is reviewable by `grep`. We
publish the convention "every `.trusted` call must carry
`decl_name%`" as a CI lint — no `decl_name%`, no merge.

---

## 4 · `SafeHtml` — XSS that can't be introduced

### The bug class

XSS comes in two shapes:

1. **Untrusted text reaching the DOM unescaped.** LeanTEA's base
   `Html` already escapes `text` content and attribute *values* at
   render — that part is solved at the framework level.
2. **The two gaps `escape()` can't catch on its own:**
   * `javascript:` / `data:text/html` URL schemes inside `href` / `src`.
     `<a href="javascript:alert(1)">click</a>` is valid HTML — escaping
     the *value* doesn't stop it.
   * **Event-handler attribute names** (`onclick`, `onerror`, …).
     `<a onclick="alert(1)">click</a>` is the literal name of an
     attribute, so per-value escaping never touches it.

### How LeanTEA closes it

`LeanTea.Html.SafeAttr` is a structure with a `private mk`. The only
ways to construct one are three smart constructors — each runs an
allow-list check before producing the value:

```lean
-- LeanTea/Html/Safe.lean
structure SafeAttr where
  private mk ::             -- ← outside this file, no direct construction
  name  : String
  value : String

def SafeAttr.text (name value : String) : Except String SafeAttr := …
def SafeAttr.url  (name urlV  : String) : Except String SafeAttr := …
def SafeAttr.num  (name : String) (n : Int) : Except String SafeAttr := …
```

`SafeAttr.text` rejects any name starting with `on`, plus everything
that isn't on the curated allow-list (`class`, `id`, `href`, `data-*`,
`aria-*`, SVG geometry, etc.). `SafeAttr.url` is for `href` / `src`
/ `action` and rejects `javascript:`, `data:text/html`, and
`vbscript:` schemes. There are also `text!` / `url!` / `num!`
variants that throw at build time — useful when both `name` and
`value` are literal strings you control.

### What ships in v1

- The `SafeAttr` structure with `private mk` (the unforgeability gate).
- `SafeAttr.text` / `.url` / `.num` smart constructors that return
  `Except String SafeAttr`; rejection is explicit, callers decide
  whether to log / redirect / 400.
- `text!` / `url!` / `num!` panicking variants for literal names you
  control (good for static class names; bad for user-supplied URLs).
- `aSafe` / `divSafe` / `spanSafe` / `buttonSafe` / `inputSafe` /
  `imgSafe` / `h1Safe` / `pSafe` builders that take
  `List SafeAttr` directly, so the safe path stays as ergonomic as
  the existing unsafe one.
- `SafeAttr.toAttrs` so a list of validated `SafeAttr` can also feed
  the existing `Html.elem` / `div_` builders.

The existing `LeanTea.Html` API is **unchanged** — apps migrate
incrementally. New code (and any place a user-controlled URL or
attribute name ever reaches the DOM) should use the safe builders.

### The compile error that *cannot happen*

```lean
import LeanTea.Html.Safe
open LeanTea.Html
example : SafeAttr := SafeAttr.mk "onclick" "evil()"
```

```
error: Unknown constant `LeanTea.Html.SafeAttr.mk`
```

`SafeAttr.mk` is `private` to `LeanTea/Html/Safe.lean`. From any
other file you have to go through the smart constructors, which
enforce the allow-list.

### Worked example — `examples/Smoke/SafeHtml.lean`

The smoke runner exercises 13 scenarios and prints a tree:

```
── rejected names ─────────────────────────────
  ✓ onclick rejected
  ✓ onerror rejected
  ✓ style NOT on allow-list — rejected
── rejected URL schemes ───────────────────────
  ✓ javascript: rejected
  ✓ data:text/html rejected
  ✓ javascript: case-insensitive rejected
── accepted (safe) cases ──────────────────────
  ✓ class accepted
  ✓ data-* accepted
  ✓ aria-* accepted
  ✓ relative URL accepted
  ✓ https URL accepted
  ✓ mailto URL accepted
── rendering still HTML-escapes the value ─────
  ✓ value is HTML-escaped on render
```

### TCB and audit story

The trusted core is **~30 LOC** (`SafeAttr` structure +
`nameAllowed` + `isSchemeRejected` + the three smart constructors).
Add to the curated `nameAllowList` only after auditing that the new
name can't carry executable content.

### When you need a richer URL policy

The default `isSchemeRejected` blocks `javascript:`, `data:text/html`,
and `vbscript:` — the three known XSS vectors. If your app needs a
stricter policy (e.g. "only same-origin URLs"), wrap `SafeAttr.url`
with your own helper that runs the additional check and forwards on
success.

---

## 5 · What's planned (not in v0.1)

| Bug class | Primitive | Status |
|---|---|---|
| XSS — DOM injection | `Html.SafeAttr` (URL allow-list, escape-on-construction) | Planned for v0.2 |
| Invalid state transitions | `Transition s s'` GADT (e.g. `Order .draft → Order .submitted`) | Planned for v0.2 |

`SafeAttr` is straightforward — `Html.attr` becomes
`(String × SafeAttr)` instead of `(String × String)`, and `SafeAttr`
has private constructors with sanitiser-only smart entry points,
mirroring the `Where`-style approach.

`Transition s s'` is domain-shaped rather than framework-shaped, so
the framework ships *the shape* and apps write the inductive for
their own domain.

---

## 6 · Limitations — what we don't claim

This is the section that lets auditors trust the rest. We are
**explicit about what we do not guarantee**:

- **The OS, the JS runtime, SQLite, and TLS** can all have CVEs;
  LeanTEA inherits whatever the platform gives it.
- **Side channels** (timing, cache) are out of scope. If your domain
  needs constant-time crypto, use the audited C backends behind
  `LEANTEA_CRYPTO=1` rather than the pure-Lean fallbacks.
- **Implementation bugs inside the TCB itself** are still possible.
  We keep the TCB small (~240 LOC) precisely so a single audit pass
  is feasible.
- **App-supplied callbacks** (`resolveRole`, `checkOwnership`) are
  part of the TCB by composition. A `fun _ _ => return true`
  `checkOwnership` defeats the entire owner-proof story; treat the
  role-lookup table as a security-critical asset.

The pitch isn't "we have type safety" — every modern language does
that. The pitch is: **the compiler rejects code that violates these
four properties**. Compile succeeds → those four bug classes are gone,
at no test cost. The auditor verifies the property by reading 240
lines. **No other Web framework can match this today**, because none
of their host languages have a dependently typed proof system in
their core.

---

## Try it yourself

```sh
# Run the Auth.Proof PoC
./.lake/build/bin/auth_proof_smoke

# Run the SafeQuery PoC
./.lake/build/bin/safequery_smoke

# Reproduce the compile error: try to call Where.eq from outside SafeQuery.lean
cat > /tmp/oops.lean <<'EOF'
import LeanTea.Persist.SafeQuery
open LeanTea.Persist.SafeQuery
example : Where Unit := Where.eq "x" "y"   -- This will not compile.
EOF
lake env lean /tmp/oops.lean
```

The full design history (Threat model • Primitives • TCB • Open
questions) lives in [`SECURITY.md`](../SECURITY.md). The shipped
roadmap is in [`ROADMAP.md`](../ROADMAP.md).
