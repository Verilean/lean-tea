import LeanTea.Persist.Backend

/-! # PostgreSQL bindings (libpq)

Mirrors `LeanTea.Mysql`'s surface so a Persistent-style app can swap
backends with one line:

```lean
-- before
let db ← Sqlite.open' "app.sqlite"
-- after
let pg ← Postgres.open' "postgresql://app:secret@127.0.0.1/app"
```

The connection string follows libpq's `PQconnectdb` syntax:

  * URI form:    `postgresql://user:pass@host:port/dbname?option=value`
  * Keyword form: `host=127.0.0.1 user=app password=secret dbname=app`

Both are accepted by libpq — we just pass through.

## Placeholder normalisation

The shared `Backend` interface (and every other LeanTEA driver) uses
`?` placeholders. PostgreSQL natively wants `$1`, `$2`, … — the C
wrapper rewrites `?` to `$N` before calling `PQexecParams`, so user
code keeps writing portable SQL.

## Conditional compilation

`c/leantea_postgres.c` is always built, but real libpq calls are
gated behind the `LEANTEA_HAVE_POSTGRES` macro. Without it, every
operation throws an `IO.userError`. Enable the real driver:

```
LEANTEA_POSTGRES=1 lake build
```

The lake target detects the env var, adds `-DLEANTEA_HAVE_POSTGRES`
during compile, and asks `pg_config` for include + link flags. -/

namespace LeanTea.Postgres

opaque ConnPointed : NonemptyType
def Conn : Type := ConnPointed.type
instance : Nonempty Conn := ConnPointed.property

@[extern "leantea_pg_open"]
opaque open' (connStr : @& String) : IO Conn

@[extern "leantea_pg_close"]
opaque close (c : @& Conn) : IO Unit

@[extern "leantea_pg_execp"]
opaque execp (c : @& Conn) (sql : @& String) (params : @& Array String) : IO Nat

@[extern "leantea_pg_query"]
opaque query (c : @& Conn) (sql : @& String) (params : @& Array String)
    : IO (Array (Array String))

/-- Conn → Backend, same shape as `Sqlite.Db.toBackend` and
    `Mysql.Conn.toBackend`. Plug into a sharded / cached stack
    interchangeably. -/
def Conn.toBackend (c : Conn) : LeanTea.Persist.Backend := {
  exec  := fun sql ps => execp c sql ps,
  query := fun sql ps => query c sql ps,
  close := close c
}

end LeanTea.Postgres
