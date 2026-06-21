import LeanTea

open LeanTea.Persist LeanTea.Persist.Migrate

/-! Migration smoke test. Three migrations applied in order; running
    twice should leave the second pass with 0 to apply. -/

def m1 : Migration := {
  version := 1, description := "create users",
  up := fun b => do
    let _ ← b.exec
      "CREATE TABLE users(id INTEGER PRIMARY KEY, name TEXT NOT NULL)" #[]
}

def m2 : Migration := {
  version := 2, description := "create posts",
  up := fun b => do
    let _ ← b.exec
      "CREATE TABLE posts(id INTEGER PRIMARY KEY, user_id INTEGER, body TEXT)" #[]
  down? := some fun b => do
    let _ ← b.exec "DROP TABLE posts" #[]
    return ()
}

def m3 : Migration := {
  version := 3, description := "add users.email",
  up := fun b => do
    let _ ← b.exec
      "ALTER TABLE users ADD COLUMN email TEXT NOT NULL DEFAULT ''" #[]
}

def migrations : List Migration := [m1, m2, m3]

def main : IO Unit := do
  let path := "/tmp/leantea_migrate.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let db ← LeanTea.Sqlite.open' path
  let backend := Db.toBackend db

  IO.println "-- initial status --"
  Migrate.status backend migrations

  IO.println "-- first run --"
  let (n, lastV) ← Migrate.run backend migrations
  IO.println s!"applied={n}, now at v{lastV}"

  IO.println "-- second run (idempotent) --"
  let (n, lastV) ← Migrate.run backend migrations
  IO.println s!"applied={n}, now at v{lastV}"

  IO.println "-- rollback v3 (no down — should fail) --"
  let r ← Migrate.rollback backend migrations
  IO.println s!"rolled back: {r}"

  IO.println "-- rollback v3 doesn't have down? — remove it manually --"
  -- Manually drop the v3 record to get back to v2, then rollback v2.
  let _ ← backend.exec "DELETE FROM schema_migrations WHERE version = 3" #[]
  let _ ← backend.exec "ALTER TABLE users DROP COLUMN email" #[]
  let r ← Migrate.rollback backend migrations
  IO.println s!"rolled back v2 (has down?): {r}"

  IO.println "-- status after rollback --"
  Migrate.status backend migrations
  IO.println "ok"
