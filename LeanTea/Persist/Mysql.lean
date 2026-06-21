import LeanTea.Persist.Backend

/-! # MySQL bindings (libmysqlclient)

Mirrors the `LeanTea.Sqlite` surface: open a connection, run text
SQL with `?` placeholders, get back `Array (Array String)`. Then
`Mysql.Conn.toBackend` plugs the result into the composable
`Backend` stack alongside SQLite and the cache decorators.

Conditional compilation: the C wrapper (`c/leantea_mysql.c`) is
always built, but real libmysqlclient calls are gated behind the
`LEANTEA_HAVE_MYSQL` macro. Build with that macro defined (and
`-lmysqlclient` linked) to get a working driver. Without it, every
operation returns an `IO.userError` directing the user to rebuild.

Build with MySQL:

  LEANTEA_MYSQL=1 lake build

The lake target detects the env var and adds the `-DLEANTEA_HAVE_MYSQL`
flag plus the right include and link options from `mysql_config`. -/

namespace LeanTea.Mysql

opaque ConnPointed : NonemptyType
def Conn : Type := ConnPointed.type
instance : Nonempty Conn := ConnPointed.property

@[extern "leantea_mysql_open"]
opaque open' (host : @& String) (port : UInt32)
    (user : @& String) (password : @& String) (db : @& String) : IO Conn

@[extern "leantea_mysql_close"]
opaque close (c : @& Conn) : IO Unit

@[extern "leantea_mysql_execp"]
opaque execp (c : @& Conn) (sql : @& String) (params : @& Array String) : IO Nat

@[extern "leantea_mysql_query"]
opaque query (c : @& Conn) (sql : @& String) (params : @& Array String)
    : IO (Array (Array String))

/-- Conn → Backend. The shape matches `Sqlite.Db.toBackend` so a
    sharded / cached stack can mix sqlite and mysql shards
    transparently. -/
def Conn.toBackend (c : Conn) : LeanTea.Persist.Backend := {
  exec  := fun sql ps => execp c sql ps,
  query := fun sql ps => query c sql ps,
  close := close c
}

end LeanTea.Mysql
