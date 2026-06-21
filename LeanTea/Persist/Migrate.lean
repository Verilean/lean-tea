import LeanTea.Persist.Backend

/-! # Schema migrations

Versioned migration runner that targets the `Backend` interface, so
the same code drives sqlite-only apps and the sharded / cached stacks
from `Backend.lean`.

The runner keeps state in a single bookkeeping table — by default
`schema_migrations(version INTEGER PRIMARY KEY, description TEXT,
applied_at INTEGER)`. On `run`:

1. Ensure the bookkeeping table exists (idempotent DDL).
2. Read the highest applied version.
3. Filter migrations to those with `version > current`, sort by
   version, apply each `up` closure in order.
4. Append a row for every successful migration.

If a migration's `up` throws, later migrations don't run; the
bookkeeping table already records the ones that did, so the next
invocation picks up from there. There is no implicit transaction
wrapping each migration — DDL across most engines isn't
transactional anyway, so we deliberately do not pretend otherwise.
The `down?` reverse closure is optional and only used by an explicit
`rollback` call.

## How "real" frameworks do it

| Framework | Mechanism |
|---|---|
| Persistent (Haskell) | `mkMigrate` diffs entity defs vs `information_schema` and emits SQL. Mostly automatic for additive changes. |
| Diesel (Rust) | File-based `migrations/<ts>_<name>/up.sql` + `down.sql`. CLI runs pending. |
| ActiveRecord (Rails) | Ruby DSL describing changes; numbered files. `schema.rb` snapshots the current state. |
| Alembic (Python) | Like Diesel but in Python with autogeneration from SQLAlchemy models. |
| Liquibase / Flyway | Engine-agnostic SQL files + a `databasechangelog` audit table. |

The common shape is "versioned, append-only files + a bookkeeping
table". This module implements that shape with closures instead of
files, so a migration can do any IO it likes — not just DDL. -/

namespace LeanTea.Persist.Migrate

structure Migration where
  /-- Strictly increasing, gaps allowed. Conventional: timestamps
      like 20260615120000 or sequential 1/2/3. -/
  version     : Nat
  description : String := ""
  up          : Backend → IO Unit
  /-- Optional reverse migration. Used by `rollback`; if `none`, the
      migration is considered destructive / one-way. -/
  down?       : Option (Backend → IO Unit) := none
  deriving Inhabited

/-- Custom bookkeeping table name. Override if `schema_migrations`
    clashes with an existing table. -/
structure Config where
  tableName : String := "schema_migrations"
  deriving Inhabited

namespace Config

def ddl (c : Config) : String :=
  s!"CREATE TABLE IF NOT EXISTS {c.tableName}(" ++
  "version INTEGER PRIMARY KEY," ++
  "description TEXT NOT NULL DEFAULT ''," ++
  "applied_at INTEGER NOT NULL)"

def selectMaxVersion (c : Config) : String :=
  s!"SELECT COALESCE(MAX(version), 0) FROM {c.tableName}"

def insertSql (c : Config) : String :=
  s!"INSERT INTO {c.tableName}(version, description, applied_at) VALUES (?, ?, ?)"

def deleteSql (c : Config) : String :=
  s!"DELETE FROM {c.tableName} WHERE version = ?"

end Config

/-- Pull the current schema version (0 if the bookkeeping table is
    empty or just created). -/
def currentVersion (b : Backend) (cfg : Config := {}) : IO Nat := do
  let _ ← b.exec cfg.ddl #[]
  let rows ← b.query cfg.selectMaxVersion #[]
  return (rows[0]?.bind (·[0]?)).bind (·.toNat?) |>.getD 0

/-- Run every migration with `version > current`, in ascending order
    of version. Returns `(applied count, new highest version)`. -/
def run (b : Backend) (ms : List Migration) (cfg : Config := {})
    : IO (Nat × Nat) := do
  let _ ← b.exec cfg.ddl #[]
  let current ← currentVersion b cfg
  let pending : Array Migration :=
    (ms.filter (fun m => m.version > current)).toArray.qsort
      (fun a b => a.version < b.version)
  if pending.isEmpty then
    return (0, current)
  IO.eprintln s!"migrate: current = {current}, pending = {pending.size}"
  let mut applied := 0
  for m in pending do
    IO.eprintln s!"migrate: applying v{m.version} — {m.description}"
    m.up b
    let now ← IO.monoMsNow
    let _ ← b.exec cfg.insertSql
      #[toString m.version, m.description, toString (now / 1000)]
    applied := applied + 1
  let last := (pending[pending.size - 1]!).version
  return (applied, last)

/-- Reverse one migration: invoke its `down?` closure (if any) and
    drop the corresponding bookkeeping row. Returns `true` if a
    rollback happened. -/
def rollback (b : Backend) (ms : List Migration) (cfg : Config := {})
    : IO Bool := do
  let current ← currentVersion b cfg
  if current == 0 then return false
  match ms.find? (·.version == current) with
  | none =>
    IO.eprintln s!"migrate: no Migration found for current version {current}"
    return false
  | some m =>
    match m.down? with
    | none =>
      IO.eprintln s!"migrate: v{m.version} has no down — not reversible"
      return false
    | some down =>
      IO.eprintln s!"migrate: rolling back v{m.version}"
      down b
      let _ ← b.exec cfg.deleteSql #[toString m.version]
      return true

/-- Pretty-print the applied / pending state without running anything. -/
def status (b : Backend) (ms : List Migration) (cfg : Config := {}) : IO Unit := do
  let current ← currentVersion b cfg
  let sorted := (ms.toArray.qsort (fun a b => a.version < b.version)).toList
  IO.println s!"current version: {current}"
  for m in sorted do
    let mark := if m.version ≤ current then "✓" else "·"
    IO.println s!"  {mark} v{m.version} — {m.description}"

end LeanTea.Persist.Migrate
