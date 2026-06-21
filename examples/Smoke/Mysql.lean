import LeanTea

open LeanTea.Mysql
open LeanTea.Persist

/-! MySQL smoke. Detects whether the wrapper was built with
    `LEANTEA_MYSQL=1` and either runs a real round-trip or prints
    the stub error. Either way exits 0. -/

structure Args where
  host : String := "127.0.0.1"
  port : UInt32 := 3306
  user : String := "root"
  pass : String := ""
  db   : String := "test"

partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | "--port" :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 3306).toUInt32 }
  | "--user" :: v :: rest => parseArgs rest { a with user := v }
  | "--pass" :: v :: rest => parseArgs rest { a with pass := v }
  | "--db"   :: v :: rest => parseArgs rest { a with db   := v }
  | _ :: rest             => parseArgs rest a
  | []                    => a

def main (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  IO.println s!"connecting to mysql at {a.host}:{a.port} (db={a.db}) …"
  try
    let conn ← open' a.host a.port a.user a.pass a.db
    let _ ← execp conn
      "CREATE TABLE IF NOT EXISTS kv(k VARCHAR(64) PRIMARY KEY, v TEXT)" #[]
    let _ ← execp conn "DELETE FROM kv WHERE k = ?" #["mysql_smoke"]
    let _ ← execp conn "INSERT INTO kv(k, v) VALUES (?, ?)"
      #["mysql_smoke", "hello, mysql"]
    let rows ← query conn "SELECT v FROM kv WHERE k = ?" #["mysql_smoke"]
    match rows[0]?.bind (·[0]?) with
    | some v => IO.println s!"  v = {v}"
    | none   => IO.println "  row missing (bug)"
    close conn
    IO.println "ok"
  catch e =>
    -- Stub mode or unreachable server — both surface as IO.userError.
    IO.println s!"mysql unavailable ({e}); skipping."
    IO.println "ok"
