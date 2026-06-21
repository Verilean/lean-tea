# 4 · Persist — typed SQLite tables

`LeanTea.Persist` is a tiny analogue of Haskell's `persistent`. Each
table is a Lean `structure` plus a row codec; the framework hands you
typed CRUD against SQLite. No ORM magic, no migration framework with a
mind of its own — just enough to keep `.lean` and `.sqlite` in sync.

> **Worried about SQL injection?** The framework has a typed query
> AST that **makes injection unrepresentable in user code**. The
> `Where E` inductive has `private` leaf constructors — there's no
> path from a raw `String` into a `Where` clause. See
> **[Chapter 11 · Secure by Construction](11-secure-by-construction.md)**
> for the walk-through (`LeanTea/Persist/SafeQuery.lean` is the
> code, `examples/Smoke/SafeQuery.lean` is the live demo). This
> chapter covers the *untyped* `Entity` / `Repo` baseline; reach for
> `SafeQuery` the moment user input touches the query.

## Smallest example

```lean
import LeanTea.Persist.Store

open LeanTea.Persist

structure NoteRow where
  title : String
  body  : String
  ts    : Nat   -- unix timestamp
  deriving Inhabited, Repr

instance : Entity NoteRow where
  table := "notes"
  ddl :=
    "CREATE TABLE IF NOT EXISTS notes(" ++
    "id INTEGER PRIMARY KEY AUTOINCREMENT," ++
    "title TEXT NOT NULL," ++
    "body  TEXT NOT NULL," ++
    "ts    INTEGER NOT NULL)"
  columns := ["title", "body", "ts"]
  toRow   n := #[n.title, n.body, toString n.ts]
  fromRow r := match r.toList with
    | [_id, t, b, ts] =>
      match ts.toNat? with
      | some ts => .ok { title := t, body := b, ts := ts }
      | none    => .error "ts: int parse"
    | _ => .error "expected 4 columns"
```

That's the whole declaration. Now open a DB and start writing rows:

```lean
let db ← Sqlite.open' ".leantea-state/notes.sqlite"
let notes : Repo NoteRow := Repo.new db
notes.migrate                                  -- runs the DDL once

-- insert (typed — fields and types match the structure)
let id ← notes.insert { title := "Hi", body := "hello", ts := 0 }

-- read every row
let rows ← notes.all
```

The codec lives in *your* code. Lean's elaborator catches a field
mismatch the second you change the structure.

## Reading and writing rows — untyped vs typed

`Repo` ships **two query paths**, with a clear prescription for when
to use which.

### The rule (start here)

> **If any user-controllable string ever reaches the query — use
> `SafeQuery`. Always. No exceptions.**
>
> If the query is hand-written with no external input — admin
> scripts, one-shot data fixes, migrations, internal cron jobs —
> `Repo.query` / `Repo.execRaw` is fine.
>
> If the typed builder doesn't cover the shape you need (JOIN, CTE,
> window function, aggregate beyond `count`) — use
> `SafeQuery.trusted decl_name%`. It's the same audited escape, with
> a grep-able tag.

### Decision tree

```
                       ┌─ user input ever in the query?
                       │
                  ┌─── yes ───┐                    ┌─── no ───┐
                  │           │                    │          │
                  ▼           ▼                    ▼          ▼
        does the typed     SafeQuery        is the SQL    Repo.query
        builder cover   ←── (default)      hand-typed,    / execRaw
        the shape?      │                  no external    (acceptable —
                        │                  data?           hand SQL with
            ┌── yes ────┘                                  literals)
            │
            ▼                                  ──┐
       SafeQuery.run /                            │
       .update / .delete                          │
                                                  │
            no                                    │
            │                                     │
            ▼                                     │
       SafeQuery.trusted decl_name%   ← also audited via grep -rn '\.trusted '
       (JOIN / CTE / aggregates)                  ─┘
```

The reason for the rule: at the moment a value of unknown provenance
touches a SQL string, you've opened the injection door. The framework
makes that **impossible at the type level** by routing through
`SafeQuery` — the `Where` AST has `private` leaf constructors, so
calling `Where.eq "email" rawString` from outside doesn't even
typecheck. The full mechanism (private constructors, the compile
error you'd see) is in
**[Chapter 11 · Secure by Construction](11-secure-by-construction.md)**.

The rest of this section shows both APIs side by side so you can
match shapes when reading existing code, and gives you the typed
recipes you'll be writing 95% of the time.

### The untyped baseline (`Repo.query` / `Repo.execRaw`)

```lean
-- SELECT — pass raw SQL + positional params
let recent ← notes.query
  "SELECT * FROM notes WHERE title LIKE ? ORDER BY ts DESC LIMIT ?"
  #["%hi%", "10"]

-- UPDATE — same shape
let _ ← notes.execRaw
  "UPDATE notes SET body = ? WHERE id = ?"
  #["edited", "42"]

-- DELETE
let _ ← notes.execRaw "DELETE FROM notes WHERE ts < ?" #["1000"]
```

This works. It's also exactly the shape every web framework has, and
every web framework occasionally ships a CVE because someone wrote:

```lean
-- ⚠️ Don't.  String-concat user input directly into the WHERE clause.
let bad ← notes.query
  s!"SELECT * FROM notes WHERE title LIKE '%{userInput}%'" #[]
```

The compiler can't tell `userInput` from a safe literal — `s!".."` is
just string interpolation. **This compiles**. The CVE is one careless
edit away. `Repo.query` is provided because some queries (recursive
CTEs, complex JOINs) the typed builder doesn't yet cover, but if you
have any user-supplied data flowing into the query, this is the wrong
tool.

### The typed alternative (`SafeQuery`)

The same queries, built from a typed AST. Every value enters via a
smart constructor that demands a typed `Col E α` column reference and
a value of the matching `α`. The leaf constructors of `Where E` are
**`private`** to `SafeQuery.lean` — there is **no path from a raw
`String` into a `Where` clause**.

```lean
import LeanTea.Persist.SafeQuery
open LeanTea.Persist.SafeQuery

-- Column references — once per app, then reused everywhere.
namespace NoteCols
  def id    : Col NoteRow Nat    := ⟨"id"⟩
  def title : Col NoteRow String := ⟨"title"⟩
  def body  : Col NoteRow String := ⟨"body"⟩
  def ts    : Col NoteRow Nat    := ⟨"ts"⟩
end NoteCols

-- SELECT — typed Where + orderBy + limit
let recent ← SafeQuery.run notes
  { where_  := NoteCols.title.like .contains "hi",
    orderBy := [],
    limit   := some 10 }

-- UPDATE — `.=` is the typed setter, takes Col + value of the matching α
let _ ← SafeQuery.update notes
  { set    := [ NoteCols.body .= "edited" ],
    where_ := NoteCols.id.eq 42 }

-- DELETE
let _ ← SafeQuery.delete notes
  { where_ := NoteCols.ts.lt 1000 }

-- COUNT (the only aggregate the v1 covers — pagination friendly)
let stale ← SafeQuery.count notes (NoteCols.ts.lt 1000)
```

### Side-by-side

| Concern | Untyped (`Repo.query`) | Typed (`SafeQuery`) |
|---|---|---|
| User input → WHERE | Programmer must remember `?` placeholders + manually escape | Impossible at the type level — only typed `Col α + value α` enters |
| Misspelt column | Runtime error after `SELECT * FROM notes WHERE titel = ?` returns 0 rows | Compile error — `NoteCols.titel` is undefined |
| Wrong-typed value | Runtime conversion error or silent wrong result | Compile error — `NoteCols.id.eq "hello"` fails: `Nat` ≠ `String` |
| Variable-length `IN (?, ?, …)` | Hand-build the placeholder list, easy to off-by-one | `.inList [1, 2, 3]` — rendered & bound positionally for you |
| `LIKE 'prefix%'` | `'%' ++ user ++ '%'` — exact spot for an injection | `Col.like .contains user` — `%` lives in the AST, value is bound |
| Migration to a renamed column | Silent failure (`SELECT * FROM notes WHERE old_name = ?`) | Compile error — `Col` reference no longer matches |
| Code review surface | Every `Repo.query` is a potential injection site | Every `SafeQuery.trusted` is grep-able with `decl_name%` |
| Aggregate `SUM`, JOINs | Hand SQL | Use `.trusted` (audited escape, ~2 lines per app) |

### When the typed builder doesn't cover it — `.trusted`

For multi-table JOINs, recursive CTEs, window functions: the typed
builder doesn't (yet) emit those, so the framework provides an audited
escape:

```lean
let rows ← SafeQuery.trusted notes decl_name%
  "SELECT n.* FROM notes n JOIN tags t ON t.note = n.id WHERE t.label = ?"
  #["urgent"]
```

`decl_name%` is a Lean macro that resolves to the calling
declaration's fully-qualified name. CI runs `grep -rn '\.trusted '`
to enumerate every audit point. **Two lines for the auditor to read,
not the entire repo.**

The full security story (private leaf constructors, the
"impossible at the type level" compile errors, the ~240-LOC trusted
core) is **[Chapter 11 · Secure by Construction](11-secure-by-construction.md)**.

### Skip the codec — JSON blob columns

Hand-writing `toRow` / `fromRow` is fine for 3 fields; at 10+ it gets
noisy. The pragmatic escape hatch: store the whole row as a single
JSON-blob column and let Lean's stdlib derive the codec.

```lean
structure SettingsRow where
  userId : String
  theme  : String
  lang   : String
  fontSz : Nat
  flags  : List String
  deriving ToJson, FromJson, Inhabited, Repr

instance : Entity SettingsRow where
  table   := "settings"
  ddl     := "CREATE TABLE IF NOT EXISTS settings(userId TEXT PRIMARY KEY, data TEXT NOT NULL)"
  columns := ["userId", "data"]
  toRow   s := #[s.userId, (toJson s).compress]
  fromRow r := match r.toList with
    | [_uid, json] =>
      match Lean.Json.parse json with
      | .ok j   => Lean.fromJson? (α := SettingsRow) j
      | .error e => .error e
    | _ => .error "expected 2 columns"
```

You give up SQL-side querying of individual fields, but you keep
schema evolution painless: add a `flags` field, redeploy, old rows
read fine if the new field has a default.

The Sheet store uses the per-column path because cells are queried
by `ref` (`A1`, `B2`, …) directly. A settings table or audit log
doesn't care — JSON blob is fine.

## The `Store` aggregate

Bundle every Repo your app uses into a `Store` record. Sheet does this
(see `examples/Sheet/App.lean`):

```lean
structure Store where
  db    : Db
  cells : Repo Sheet.Row     -- ref : String, formula : String

def Store.open (path : String) : IO Store := do
  let db ← Sqlite.open' path
  let cells : Repo Sheet.Row := Repo.new db
  cells.migrate
  return { db, cells }
```

Then high-level operations are methods on `Store`:

```lean
def Store.setCell (s : Store) (ref : Sheet.CellRef) (formula : String)
    : IO Nat := do
  /- Idempotent insert; we replace by primary key (`ref`). -/
  s.cells.insert { ref, formula }
```

Side-effecting domain operations land on `Store`. The pure
`WebApp.update` function never imports `Persist`.

## Migrations

Manual. Edit your `ddl`, write a one-shot SQL script, run it. No
framework magic.

The argument: most schema changes are non-trivial enough that hiding
them behind `auto-migrate` is dangerous. Keep every schema change as
either a `CREATE TABLE IF NOT EXISTS` (safe to re-run) or a
`migrations/0NN-…sql` script you run by hand.
`LeanTea.Persist.Migrate` ships a small versioned-script runner if you
want to formalise that.

## Composable backends

`LeanTea.Persist.Backend` lets you stack a shard layer or a memcached
read-through cache on top of a `Repo`. None of the shipped examples
need it; it exists for when you do.

`LeanTea.Persist.Mysql` is the MySQL counterpart (opt-in via
`LEANTEA_MYSQL=1` at build time, see `lakefile.lean` for the FFI
wiring). The same `Entity / Repo` typeclass surface works against
either backend.

## When to *not* use this

- **Anything multi-tenant with strict isolation** — SQLite is single
  file. Use Postgres + a proper migration tool.
- **High-concurrency writes** — SQLite is single-writer. The user-paced
  apps shipped here never bite this; a WebSocket game with hundreds of
  concurrent writes would.
- **Anything you'd query analytically** — SQLite is fine for app state;
  for cross-user analytics, dump and load elsewhere.

For everything else the **typed Entity + manual migrations + Store
aggregate** pattern carries an app cleanly from prototype to small
production.
