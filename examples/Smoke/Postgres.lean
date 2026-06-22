import LeanTea
import LeanTea.Persist.Postgres
import LeanTea.LSpec

/-! # postgres_smoke — round-trip the Postgres driver against a live DB

Needs `LEANTEA_POSTGRES=1` at build time and a live PostgreSQL
reachable via `PG_CONN_STR` (default
`postgresql://postgres:postgres@127.0.0.1:5432/postgres`). The CI
workflow spins up a `postgres:16` service container with that
default URL.

```sh
# Local quick run:
docker run -d --rm --name leantea-pg -p 5432:5432 \
  -e POSTGRES_PASSWORD=postgres postgres:16
LEANTEA_POSTGRES=1 lake build postgres_smoke
./.lake/build/bin/postgres_smoke
```

The smoke creates a fresh table, inserts via `?`-placeholder SQL
(which the C wrapper rewrites to `$1` for libpq), reads it back,
updates one row, deletes another, drops the table. -/

open LeanTea LeanTea.LSpec
open LeanTea.Postgres

def main : IO Unit := do
  let connStr := (← IO.getEnv "PG_CONN_STR").getD
    "postgresql://postgres:postgres@127.0.0.1:5432/postgres"
  IO.println s!"postgres_smoke — {connStr}"
  let c ← open' connStr

  /- Always start from a clean slate. -/
  let _ ← execp c "DROP TABLE IF EXISTS leantea_smoke" #[]
  let _ ← execp c
    "CREATE TABLE leantea_smoke (id INTEGER PRIMARY KEY, name TEXT NOT NULL, n INTEGER)"
    #[]

  /- INSERT × 4 — `?` placeholders, params as Array String. -/
  let _ ← execp c
    "INSERT INTO leantea_smoke (id, name, n) VALUES (?, ?, ?)"
    #["1", "alice", "100"]
  let _ ← execp c
    "INSERT INTO leantea_smoke (id, name, n) VALUES (?, ?, ?)"
    #["2", "bob",   "200"]
  let _ ← execp c
    "INSERT INTO leantea_smoke (id, name, n) VALUES (?, ?, ?)"
    #["3", "carol", "300"]
  let _ ← execp c
    "INSERT INTO leantea_smoke (id, name, n) VALUES (?, ?, ?)"
    #["4", "dave",  "400"]

  /- SELECT all. -/
  let all ← query c "SELECT id, name, n FROM leantea_smoke ORDER BY id" #[]

  /- SELECT WHERE name = ?  (parametrised). -/
  let oneRows ← query c
    "SELECT id, name FROM leantea_smoke WHERE name = ?"
    #["bob"]

  /- UPDATE returning affected count. -/
  let upd ← execp c "UPDATE leantea_smoke SET n = ? WHERE id = ?" #["999", "1"]
  let afterUpd ← query c "SELECT n FROM leantea_smoke WHERE id = ?" #["1"]

  /- DELETE returning affected count. -/
  let del ← execp c "DELETE FROM leantea_smoke WHERE id = ?" #["4"]
  let afterDel ← query c "SELECT COUNT(*) FROM leantea_smoke" #[]

  /- Mismatched param count → IO error. -/
  let mismatch ← (try
                    let _ ← execp c "SELECT * FROM leantea_smoke WHERE id = ?" #[]
                    pure false
                  catch _ => pure true)

  /- Cleanup. -/
  let _ ← execp c "DROP TABLE leantea_smoke" #[]
  close c

  let spec : LSpec := group "Postgres round-trip" [
    it "SELECT returns 4 rows"
      (all.size == 4),
    it "row 0 columns are id, name, n"
      (match all[0]? with
       | some r => r.size == 3 && r[0]! == "1" && r[1]! == "alice" && r[2]! == "100"
       | none => false),
    it "parametrised WHERE name = 'bob' returns 1 row"
      (oneRows.size == 1
        && (oneRows[0]?.bind (·[1]?)) == some "bob"),
    it "UPDATE returns affected = 1"
      (upd == 1),
    it "UPDATE actually took"
      (match afterUpd[0]? with
       | some r => r[0]? == some "999"
       | none => false),
    it "DELETE returns affected = 1"
      (del == 1),
    it "after delete, 3 rows remain"
      (match afterDel[0]? with
       | some r => r[0]? == some "3"
       | none => false),
    it "wrong placeholder count throws IO error"
      mismatch
  ]
  let code ← lspecIO (group "LeanTEA Postgres" [spec])
  if code != 0 then IO.Process.exit code.toUInt8
