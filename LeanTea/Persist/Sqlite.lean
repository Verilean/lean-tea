/-! # Minimal Lean bindings to libsqlite3

The C wrapper lives in `c/leantea_sqlite.c`. Lake compiles it via the
`leantea_sqlite_o` target declared in `lakefile.lean`, and the
`leantea_sqlite` extern lib links `libsqlite3` into the final binary.

All values are exchanged as `String`. Integer / numeric columns must
be parsed on the Lean side; this keeps the FFI surface tiny. -/

namespace LeanTea.Sqlite

/-- Opaque handle to a sqlite3 database. Backed by a `sqlite3*`
    pointer with an external finalizer that calls `sqlite3_close`. -/
opaque DbPointed : NonemptyType
def Db : Type := DbPointed.type
instance : Nonempty Db := DbPointed.property

@[extern "leantea_sqlite_open"]
opaque open' (path : @& String) : IO Db

@[extern "leantea_sqlite_close"]
opaque close (db : @& Db) : IO Unit

/-- Execute one or more semicolon-separated SQL statements that take
    no parameters and return no rows. Useful for DDL and ad-hoc DML. -/
@[extern "leantea_sqlite_exec"]
opaque exec (db : @& Db) (sql : @& String) : IO Unit

/-- Execute a prepared statement with text-bound parameters. Returns
    `sqlite3_changes` (rows affected). -/
@[extern "leantea_sqlite_execp"]
opaque execp (db : @& Db) (sql : @& String) (params : @& Array String) : IO Nat

/-- Run a query with text-bound parameters and collect all rows. Each
    cell is returned as a `String`; numerics come through as their
    sqlite text representation. -/
@[extern "leantea_sqlite_query"]
opaque query (db : @& Db) (sql : @& String) (params : @& Array String)
  : IO (Array (Array String))

end LeanTea.Sqlite
