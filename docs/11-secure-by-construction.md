# 11 · Secure by Construction — walking through the shipped primitives

LeanTEA's headline property is that whole classes of vulnerabilities
**can't be expressed in user code that compiles**. This chapter walks
through the eight primitives that ship in v0.1:

| § | Primitive | Bug class | IPA category | OWASP |
|---|---|---|---|---|
| 2 | `Auth.Proof` | Authorization bypass / IDOR | アクセス制御 §3.7 | A01 |
| 3 | `Persist.SafeQuery` | SQL injection | SQL インジェクション §3.1 | A03 |
| 4 | `Html.SafeAttr` | Reflected / stored XSS | XSS §3.5 | A03 |
| 5 | `Net.SafePath` | Path traversal | パス・トラバーサル §3.4 | A01 |
| 6 | `Os.SafeCmd` | OS command injection | OS コマンド・インジェクション §3.3 | A03 |
| 7 | `Response.setHeader` + `defaultSecurityHeaders` | HTTP header injection + clickjacking + MIME sniff | HTTP ヘッダ §3.6 + クリックジャッキング §3.10 | A03 + A05 |
| 8 | `Net.SafeRedirect` | Open redirect | オープンリダイレクト §3.9 | A01 |

All eight are real code under `LeanTea/`, and each comes with a smoke
binary that runs the demo end-to-end. The IPA column maps to 「安全
なウェブサイトの作り方」 (IPA Japan's de facto enterprise web-app
audit standard); the OWASP column maps to OWASP Top 10 2021.

> **What this chapter is for**: showing *why* the framework is safe,
> with the exact compile errors that prove it. The other chapters
> tell you *how* to write apps. If you only want one of those two,
> Chapter 11 is the "why" one.

The full design document lives in [`SECURITY.md`](../SECURITY.md);
this chapter is the prose / worked-example side of the same story.

---

## 1 · The threat model in one paragraph

A LeanTEA codebase makes **eight classes of vulnerability
unrepresentable** at compile time:

1. **Authorization bypass** — a handler runs without the auth check.
2. **SQL injection** — user input concatenated into a query.
3. **XSS via attribute name / URL scheme** — untrusted text reaches
   the DOM via `onclick=…` or `href="javascript:…"`.
4. **Path traversal** — `?file=../../etc/passwd`.
5. **OS command injection** — `IO.Process.run` called via a shell.
6. **HTTP header injection** — `\r\n` smuggled into `Location:`.
7. **Open redirect** — `?next=https://evil.example`.
8. **Clickjacking + MIME sniffing** — missing `X-Frame-Options` /
   `X-Content-Type-Options`.

The framework's TCB (trusted core) is **~480 LOC** of auth + persist +
HTML + path + cmd + header + redirect modules. **Anything you build
on top is automatically safe**, because the type system rejects every
call that bypasses the typed entry points.

The pattern is the same in every case:

> A `Safe*` structure with `private mk` → the only way to construct
> one is the smart constructor → the smart constructor runs the
> allow-list / scheme check / normalisation → user code that goes
> around the smart constructor doesn't compile.

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

This is the demo `examples/Tests/PersistSpec.lean` (`AuthProofGroup`) runs end-to-end:

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

### Worked example — `examples/Tests/SecuritySpec.lean` (SafeHtml group)

The smoke runner exercises 13 scenarios — all `✓`:

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

---

## 5 · `SafePath` — paths that can't escape their workspace

### The bug class

```lean
-- THE BUG
let raw := req.query.find "file" |>.getD ""    -- user input
let path := workspace ++ "/" ++ raw            -- "/srv/uploads/../../../etc/passwd"
IO.FS.readFile path                            -- 💥
```

Path traversal sits behind half of all "file read" CVEs (`?file=../`,
`?include=../../wp-config.php`, `?theme=../../etc/passwd`). The
framework can't tell whether a `String` is a user-controlled path
vs. a developer-controlled one — until you tag it.

### How LeanTEA closes it

`LeanTea.Net.SafePath` is a structure with `private mk`. The only
public constructor `SafePath.under` takes a workspace + raw path,
normalises `.` / `..`, and **refuses to return** a path that resolves
outside the workspace. NUL bytes (the classic libc truncation trick:
`foo.txt\0.png`) are rejected up front.

```lean
structure SafePath where
  private mk ::
  value : String              -- absolute, normalised, under workspace

def SafePath.under (workspace path : String) : Except String SafePath
```

### The compile error that *cannot happen*

```lean
example : SafePath := SafePath.mk "/etc/passwd"
-- error: SafePath.mk is private to LeanTea.Net.SafePath
```

`SafePath.mk` is module-private. The reader (or future code-reviewer)
who sees a `SafePath` knows the only path it could have come from is
`SafePath.under` — and that already ran the workspace check.

### Sibling-prefix attack — why we compare `/foo/bar/` and not `/foo/bar`

A naive prefix check on `/srv/uploads` lets `/srv/uploads-attacker/x`
slip past. The smart constructor compares against `workspace ++ "/"`,
so sibling-prefixes are explicitly rejected. The smoke test covers
this case (`✓ sibling-prefix rejected`).

### Worked example — `examples/Tests/SecuritySpec.lean` (SafePath group)

```
── accepted paths under workspace ─────────────
  ✓ relative joined under ws
  ✓ nested relative (/srv/uploads/sub/dir/b.txt)
  ✓ absolute already under ws
  ✓ `.` / `..` normalised (/srv/uploads/a/c.txt)
  ✓ `.` resolves to ws root
── rejected paths that escape workspace ───────
  ✓ `..` rejected
  ✓ deep `..` rejected
  ✓ absolute outside ws rejected
  ✓ sibling-prefix rejected
── rejected: NUL byte (libc truncation guard) ─
  ✓ NUL byte rejected
```

### TCB and audit story

**~30 LOC** (`normalise` + `under`). The `normalise` function is
idempotent and operates purely on `splitOn "/"` segments — auditing
once is enough. The framework explicitly does not follow symlinks
(no `realpath`): if your workspace itself contains a symlink to
`/etc`, that is an operator-deployment issue, not a framework
guarantee. We document this limitation in the module's docstring.

---

## 6 · `SafeCmd` — `IO.Process.run` that can't get shell-injected

### The bug class

```lean
-- THE BUG
let cmd := "convert " ++ userInput ++ " out.png"
IO.Process.run { cmd := "sh", args := #["-c", cmd] }
-- userInput = "in.png; rm -rf /" → game over
```

Lean's `IO.Process.run` already takes `args : Array String` and
`execvp`s directly — using it correctly is structurally safe. The
ergonomic temptation is to drop down to `sh -c` for "just one pipe"
and lose the guarantee.

### How LeanTEA closes it

`LeanTea.Os.SafeCmd` is a structure with `private mk` whose `args`
is a `List String` (never concatenated). The smart constructor
`SafeCmd.exec` refuses programs whose basename is a shell
(`sh`, `bash`, `zsh`, `dash`, `pwsh`, `cmd`, …) — those need the
grep-able audit escape `SafeCmd.shell`.

```lean
structure SafeCmd where
  private mk ::
  cmd  : String
  args : List String

def SafeCmd.exec (cmd : String) (args : List String) : Except String SafeCmd
def SafeCmd.shell (script : String) : SafeCmd      -- audit-escape, grep for it
```

### Basename detection — `/usr/bin/bash` is still a shell

The detection looks at the basename (last `/`-separated segment),
so `/usr/bin/bash` and `/bin/zsh` are both refused. NUL bytes in
either `cmd` or any `args` element are rejected up front.

### Worked example — `examples/Tests/SecuritySpec.lean` (SafeCmd group)

```
── accepted argv-style commands ───────────────
  ✓ echo accepted
  ✓ /usr/bin/env accepted
  ✓ ls accepted
── rejected shells (by basename) ──────────────
  ✓ sh rejected
  ✓ bash rejected
  ✓ /usr/bin/bash rejected (basename)
  ✓ /bin/zsh rejected (basename)
── end-to-end: spawn echo via SafeCmd.output ──
  ✓ spawn + stdout matches
```

### TCB and audit story

**~30 LOC**. The shell allow-list (`sh`, `bash`, `zsh`, `dash`,
`ksh`, `csh`, `tcsh`, `fish`, `pwsh`, `powershell`, `cmd`,
`cmd.exe`) is conservative; any name added needs a code review.
`SafeCmd.shell` is the grep-able audit point — every intentional
shell use sticks out as `SafeCmd.shell` in `grep -rn`.

---

## 7 · `Response.setHeader` + `defaultSecurityHeaders` — header injection & clickjacking

### The bug class — header injection

```lean
-- THE BUG
let next := req.query.find "next" |>.getD "/"      -- attacker: "/x\r\nSet-Cookie: ev=1"
Response.redirect next
-- Browser sees:  Location: /x\r\n
--                Set-Cookie: ev=1\r\n
```

A CRLF sneaks a second header line in. Same pattern affects mail-
agent code that takes user-controlled `Subject:` / `To:` strings.

### How LeanTEA closes it

`Response.setHeader` refuses any header name or value that contains
`\r`, `\n`, or `\u0000`. `withCookie` and `redirect` route through
the same guard. The default constructor for `Response` accepts
`headers : Array (String × String)` so old code keeps working — but
*every shipped helper* now passes through `setHeader!`.

```lean
def Response.setHeader (r : Response) (name value : String)
  : Except String Response
def Response.setHeader! (r : Response) (name value : String) : Response
```

### The bug class — clickjacking & MIME sniffing

These aren't about input-validation — they're about *headers you
forgot to add*. `X-Frame-Options: DENY` stops your login page from
being iframed into `evil.example`. `X-Content-Type-Options: nosniff`
stops the browser from MIME-guessing a CSV as HTML.

### How LeanTEA closes it

`Response.defaultSecurityHeaders` is a single call that adds the
baseline. The default is locked-down (`X-Frame-Options: DENY`);
apps that intentionally embed themselves set
`frameOptions := none` and configure a CSP `frame-ancestors`
directive instead.

```lean
def Response.defaultSecurityHeaders
    (r : Response) (frameOptions : Option String := some "DENY")
    : Response :=
  -- adds:  X-Frame-Options, X-Content-Type-Options,
  --        Referrer-Policy, Permissions-Policy
```

### Worked example — `examples/Tests/SecuritySpec.lean` (SafeHeader group)

```
── CR/LF/NUL in header name or value rejected ─
  ✓ CRLF in name rejected
  ✓ CRLF in value rejected
  ✓ LF-only in value rejected
  ✓ NUL in value rejected
── defaultSecurityHeaders applies the baseline ─
  ✓ X-Frame-Options: DENY present
  ✓ X-Content-Type-Options: nosniff present
  ✓ Referrer-Policy: no-referrer present
  ✓ Permissions-Policy present
── Response.redirect strips CR/LF defence in depth ─
  ✓ injected `\r\nset-cookie:` removed
```

### TCB and audit story

**~10 LOC** (`hasHeaderInjection` + the two `setHeader` shells).
`defaultSecurityHeaders` adds another ~10 LOC of pure header-list
mutation. The list of "shipped baseline headers" is in the
docstring; auditors review it once.

---

## 8 · `SafeRedirect` — open redirect that needs an allow-list

### The bug class

```lean
-- THE BUG
let next := req.query.find "next" |>.getD "/"     -- attacker: "?next=https://evil.example"
Response.redirect next
```

The attacker phishes a link that *looks* like it lives on `your-app.com`
because the URL bar shows your domain. After login, the browser
redirects to `evil.example`, where credentials get re-prompted on a
look-alike form. Open redirect is the OAuth callback footgun and the
single most common "post-login open-redirect → phishing" CWE.

### How LeanTEA closes it

`LeanTea.Net.SafeRedirect.to` requires the caller to declare which
origins are acceptable. The smart constructor:

* accepts safe relative paths (`/dashboard`)
* **rejects** protocol-relative URLs (`//evil.example`)
* **rejects** `javascript:` / `data:` / `vbscript:` / `file:` schemes
* accepts absolute URLs only if the origin appears in the allow-list

```lean
structure SafeRedirect where
  private mk ::
  location : String

def SafeRedirect.to (trustedOrigins : List String) (loc : String)
  : Except String SafeRedirect
def SafeRedirect.toForced (loc : String) : SafeRedirect   -- grep-able escape
```

### Sibling-prefix attack again — `https://app.example.evil.com`

A naive `startsWith` would let `https://app.example.evil.com` slip
past an allow-list entry of `https://app.example`. The smart
constructor compares against `origin ++ "/"`, so sibling-prefix
domains are rejected. The smoke covers this (`✓ sibling-prefix
origin rejected`).

### Worked example — `examples/Tests/SecuritySpec.lean` (SafeRedirect group)

```
── accepted: relative paths ──────────────────
  ✓ /dashboard
  ✓ path+query
── accepted: trusted origins ─────────────────
  ✓ exact origin (https://app.example)
  ✓ path under trusted origin (https://app.example/foo/bar)
── rejected: protocol-relative ───────────────
  ✓ //evil.example rejected
  ✓ /\evil.example rejected
── rejected: dangerous schemes ───────────────
  ✓ javascript: rejected
  ✓ data: rejected
  ✓ vbscript: rejected
  ✓ file: rejected
── rejected: origin not on allow-list ────────
  ✓ evil origin rejected
  ✓ sibling-prefix origin rejected
```

### TCB and audit story

**~30 LOC**. `SafeRedirect.toForced` is the grep-able audit escape
— every intentional non-allow-listed redirect is one `grep -rn
SafeRedirect.toForced` away.

---

## 9 · What's planned (not in v0.1)

| Bug class | Primitive | Status |
|---|---|---|
| Invalid state transitions | `Transition s s'` GADT (e.g. `Order .draft → Order .submitted`) | Planned for v0.2 |
| CSRF | `Form.csrf` token wired through `<form>` builder + middleware | Partial (cookie infra ships; helper TBD) |
| CSP | `Response.csp` typed builder | Planned for v0.2 |

`Transition s s'` is domain-shaped rather than framework-shaped, so
the framework ships *the shape* and apps write the inductive for
their own domain.

---

## 10 · Limitations — what we don't claim

This is the section that lets auditors trust the rest. We are
**explicit about what we do not guarantee**:

- **The OS, the JS runtime, SQLite, and TLS** can all have CVEs;
  LeanTEA inherits whatever the platform gives it.
- **Side channels** (timing, cache) are out of scope. If your domain
  needs constant-time crypto, use the audited C backends behind
  `LEANTEA_CRYPTO=1` rather than the pure-Lean fallbacks.
- **Implementation bugs inside the TCB itself** are still possible.
  We keep the TCB small (~480 LOC across all eight primitives)
  precisely so a single audit pass is feasible.
- **App-supplied callbacks** (`resolveRole`, `checkOwnership`,
  `trustedOrigins`) are part of the TCB by composition. A
  `fun _ _ => return true` `checkOwnership` defeats the entire
  owner-proof story; an over-broad `trustedOrigins := ["https://"]`
  defeats `SafeRedirect`. Treat the role-lookup table and the
  redirect allow-list as security-critical assets.
- **`SafePath` does not follow symlinks.** If your workspace contains
  a symlink to `/etc`, that escape is on the operator. Best-effort
  guarantee, documented in the module.
- **`SafeCmd.shell` is allowed** — it has to be, for any framework
  that ships at all. The guarantee is that every call site is
  grep-able under one fixed identifier.

The pitch isn't "we have type safety" — every modern language does
that. The pitch is: **the compiler rejects code that violates these
eight properties**. Compile succeeds → those eight bug classes are
gone, at no test cost. The auditor verifies the property by reading
480 lines. **No other Web framework can match this today**, because
none of their host languages have a dependently typed proof system
in their core.

---

## Try it yourself

```sh
# SQLite-backed integration: Store + Query DSL + Migrate +
# Auth.Proof + SafeQuery, 32 LSpec assertions in one binary.
./.lake/build/bin/persist_spec

# Construction-time guarantees: SafeHtml + SafePath + SafeCmd +
# SafeHeader + SafeRedirect, 60 LSpec assertions in one binary.
./.lake/build/bin/security_spec

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
