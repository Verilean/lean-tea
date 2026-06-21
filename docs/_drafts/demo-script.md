# AI-driven secure development — demo video script (draft)

This is the script for the launch demo. The video is 5 minutes long:
60 s setup, 4 × 60 s scenes that each pit an LLM against a LeanTEA
security primitive, 30 s wrap.

**Pitch in one line**: *"Watch the LLM write a bug, watch the compiler
refuse to ship it, watch the LLM fix it — all without a human in the
loop."*

The whole flow runs through the Chrome-CDP MCP (so the LLM is talking
to the same Lean toolchain you'd use locally). No editing of the LLM's
output; we want viewers to see the unscripted retry loop.

---

## Cast

- **The LLM**: Claude (Anthropic, via Claude Code). Could substitute
  GPT, Gemini, Llama — the story is the same. Always shown live with
  the cursor blinking and the compile-error feedback driving its next
  attempt.
- **The Compiler**: Lean 4, surfaced through the LSP error pane and
  through `lake build` exit codes. The "second player" of every scene.
- **The Human**: just a narrator. Never edits code. Hits Enter, reads.

---

## 0:00 — 0:60 · Setup

**Visual**: split-screen. Left: VS Code with `examples/MyShop/Serve.lean`
open. Right: a terminal tail of `lake build --watch`. The framework is
LeanTEA; we've stubbed a tiny e-commerce server with three private
helpers (sessions, an order store, a user store) and **one missing
endpoint**.

**Narration (15 s)**:
> "I asked Claude to add an admin-only DELETE endpoint to this Lean
> server. Forget about prompt engineering. Forget about RLHF.
> The compiler is going to do the security review."

**Visual (15 s)**: zoom into `examples/MyShop/Serve.lean` — show the
existing routes:

```lean
def routes : List AnyAuthRoute := [
  AnyAuthRoute.of (c := .guest) listEndpoint,
  AnyAuthRoute.of (c := .user)  buyEndpoint,
  -- TODO: admin can delete any order
]
```

**Narration (30 s)** while Claude's prompt scrolls in the editor pane:
> "The prompt: *'Add an admin_delete endpoint at POST /admin/delete that
> deletes an order by id. Must follow the existing pattern.'* That's it.
> No mention of authentication. No mention of authorization. No mention
> of SQL injection."

---

## 1:00 — 2:00 · Scene 1 · Authorization bypass

**LLM action**: Claude writes a handler that looks reasonable:

```lean
def adminDelete (req : Request) : IO Response := do
  let id := lookupNat req.query "id" 0
  store.orders.delete id
  return Response.text 200 s!"deleted {id}"
```

…then wires it into `routes` as `.guest`:

```lean
AnyAuthRoute.of (c := .guest) {
  path := "/admin/delete", method := "POST", handler := adminDelete
}
```

**Compile output (cut to terminal)**:

```
error: examples/MyShop/Serve.lean:42:24: Type mismatch
  expected: Proof .guest → Request → IO Response
  got:      Request → IO Response
```

**Narration**: "The router's signature **demands** a `Proof` argument.
The LLM dropped it. **Compiler rejects.**"

**LLM action**: re-tries — now adds the `proof` arg:

```lean
def adminDelete (proof : Proof .guest) (req : Request) : IO Response := …
```

**Compile output**: build passes — but watch:

```
$ curl -X POST http://localhost:8001/admin/delete?id=42 -b sid=tok-guest
deleted 42
```

A guest just deleted an order! Show the failure. Then we change one
character in the LLM's prompt: `.guest` → `.admin`.

**Compile output**: build passes. Now:

```
$ curl -X POST http://localhost:8001/admin/delete?id=42 -b sid=tok-guest
forbidden: insufficient capability: have guest, need admin
$ curl -X POST http://localhost:8001/admin/delete?id=42 -b sid=tok-admin
deleted 42 (auth'd as admin@x.com)
```

**Narration (close of scene)**: "The compiler can enforce *which kind of
proof* the handler demands. We picked `Proof .admin`. Now no path
without an admin session can even **reach** this handler. The proof is
unforgeable — `private mk`. We didn't write a single test."

---

## 2:00 — 3:00 · Scene 2 · SQL injection

**Visual**: zoom into the same file. Claude is now asked: *"Add a search
endpoint at /api/search?q=… that returns products whose name contains
the query."*

**LLM action** — predictable first try:

```lean
def search (req : Request) : IO Response := do
  let q := lookupParam req.query "q" |>.getD ""
  let rows ← products.query s!"SELECT * FROM products WHERE name LIKE '%{q}%'" #[]
  return Response.text 200 (renderRows rows)
```

**Compile output**:

```
error: examples/MyShop/Serve.lean:60:20: Unknown identifier `Repo.query`
  hint: untyped query is gone in v0.2; use SafeQuery
```

(We've removed the old `Repo.query` in favour of `SafeQuery.run`.)

**LLM action**: switches to `SafeQuery.run` but tries to inline the
parameter:

```lean
let rows ← SafeQuery.run products
  { where_ := Where.like "name" .contains q }
```

**Compile output**:

```
error: examples/MyShop/Serve.lean:60:24: Unknown constant `LeanTea.Persist.SafeQuery.Where.like`
  hint: `Where`'s leaf constructors are private. Use `Col.like`.
```

**LLM action**: reaches for the smart constructor:

```lean
let rows ← SafeQuery.run products
  { where_ := ProductCols.name.like .contains q }
```

**Compile output**: green. Run:

```
$ curl 'http://localhost:8001/api/search?q=widget'
[{"id":1,"name":"Widget"},{"id":7,"name":"Widget Pro"}]
$ curl "http://localhost:8001/api/search?q=widget%25%27%3B+DROP+TABLE+products%3B+--"
[]
```

**Narration**: "Last query: `widget'; DROP TABLE products; --`. The DB
is fine. The `%` and the quote both went through `?` parameter binding —
they're text, not SQL. **We didn't write a single test.**"

---

## 3:00 — 4:00 · Scene 3 · Owner check (dependent types)

**Visual**: extend the search demo. *"Add an /api/edit/:id endpoint
that lets the owner of an order edit its note."*

**LLM action** — first attempt forgets the owner check:

```lean
def editNote (id : String) (req : Request) : IO Response := do
  store.orders.updateNote id (lookupParam req.query "note" |>.getD "")
  return Response.text 200 "ok"
```

…and registers it as `.user`. Compile passes, but the wrong user can
edit anyone's order.

We **change the route's capability** to `.owner id` — the dependent
form:

```lean
AnyAuthRoute.of (c := .owner id) {
  path := "/api/edit", method := "POST",
  handler := fun proof req => editNote id proof req
}
```

**Compile output**:

```
error: examples/MyShop/Serve.lean:78:42: function expected at
  c := .owner id
  but the LHS expects a Capability, and `id` is unbound here
```

**Visual**: pause. Show the diff that lifts `id` from a runtime URL
parameter into the route's *type-level* capability. The handler now
takes `Proof (.owner id)`.

**LLM action**: re-writes the dispatcher to *thread `id` through*:

```lean
def editHandler (id : String) (proof : Proof (.owner id))
    (req : Request) : IO Response := …
```

**Narration**: "The proof's type carries the runtime ID. If the
authentication middleware verifies ownership of order 42, the proof can
only be passed to a handler whose argument has type `Proof (.owner
"42")`. The path that extracts `id` from the URL and the DB row that
confirms ownership **must agree on the same `id`** — *the type system
makes that consistency a compile-time invariant*. **It's not an
`if`-statement at runtime — the type signature is what proves the ids
match.** TypeScript can't do this. Rust can't do this. Lean can."

---

## 4:00 — 4:30 · Scene 4 · The audit grep

**Visual**: terminal.

```
$ grep -rn '\.trusted ' lean-tea/examples/
examples/MyShop/Reports.lean:42:  let rows ← SafeQuery.trusted products decl_name%
examples/MyShop/Reports.lean:78:  let rows ← SafeQuery.trusted orders   decl_name%
```

**Narration (15 s)**: "Two places in the entire codebase touch raw
SQL. Both are tagged with `decl_name%` so the audit log says exactly
which function. The auditor's review surface is **these two files**.
Not 'the whole repo, please be thorough.' Two files."

**Visual (15 s)**: switch to `Reports.lean:42` and show the trusted
call — a complex 4-table JOIN that the typed builder doesn't yet
support, with a clear comment explaining why.

---

## 4:30 — 5:00 · Wrap

**Narration**:
> "Four scenes. Four classes of bug that other frameworks fight at
> review time, test time, or — usually — at incident time. LeanTEA
> compiles them away. The LLM wrote every line you saw. The compiler
> rejected every line that wasn't safe. **We didn't write a single
> *security* test** — what the compiler proves, we don't have to test.
> Happy-path / business-logic tests still matter, just at a fraction
> of the volume because the negative space is gone.
>
> Try it: `lake new my-app && cd my-app`, then add LeanTEA from git.
> The full guide is at SECURITY.md in the repo."

**Final shot**: GitHub URL on screen, with the SECURITY.md heading
visible.

---

## Filming notes

- Real Claude session via `claude --mcp`. No editing of LLM output.
  If a take goes off the rails, restart from the prompt.
- Lean 4 LSP errors must be readable on screen — use the 16pt
  `JetBrains Mono` setup we already use in the framework videos.
- Keep the terminal output to `lake build` errors only — no spurious
  warnings. Set `LEAN_LSP_SILENT=1`.
- One single take per scene. The retry loop **is the demo**.

## Open questions before recording

- Should Scene 1 also show the **`Sigma` type** wrapping in the routes
  list (`AnyAuthRoute.mk c r`) for type-aware viewers? Probably no —
  pacing matters more than completeness.
- Worth opening with a 10-second `lean --version` shot to confirm
  there's no special toolchain? Probably yes for credibility.
- Closing CTA — link to README or directly to SECURITY.md? Latter,
  because that's where the technical hook is.
