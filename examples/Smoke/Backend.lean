import LeanTea

open LeanTea.Persist LeanTea.Persist.Backend
open LeanTea.Sqlite

/-! Backend smoke test: two SQLite "shards" + an LRU cache stacked
    via decorators. Walks through:

    * each write is routed to one shard based on the first param
    * cache misses fall through to the right shard
    * cache hits skip the inner backend
    * writes punch the cache so we don't serve stale rows -/

def main : IO Unit := do
  let pathA := "/tmp/leantea_backend_a.sqlite"
  let pathB := "/tmp/leantea_backend_b.sqlite"
  for p in [pathA, pathB] do
    IO.FS.removeFile p |>.catchExceptions (fun _ => pure ())

  let dbA ← LeanTea.Sqlite.open' pathA
  let dbB ← LeanTea.Sqlite.open' pathB
  let ddl := "CREATE TABLE IF NOT EXISTS kv(user_id TEXT, key TEXT, value TEXT)"
  LeanTea.Sqlite.exec dbA ddl
  LeanTea.Sqlite.exec dbB ddl

  -- Build the stack: shard by params[0] (user_id), then cache reads.
  let sharded : Backend :=
    Backend.shardByParamNat #[Db.toBackend dbA, Db.toBackend dbB] 0
  let cache ← Cache.lru 64
  let backend := sharded.cached cache

  /- 1. Writes go to whichever shard the hash chooses. -/
  let _ ← backend.exec
    "INSERT INTO kv(user_id, key, value) VALUES (?, ?, ?)" #["1", "name", "alice"]
  let _ ← backend.exec
    "INSERT INTO kv(user_id, key, value) VALUES (?, ?, ?)" #["2", "name", "bob"]
  let _ ← backend.exec
    "INSERT INTO kv(user_id, key, value) VALUES (?, ?, ?)" #["3", "name", "carol"]
  let _ ← backend.exec
    "INSERT INTO kv(user_id, key, value) VALUES (?, ?, ?)" #["4", "name", "dave"]

  /- 2. Read by user_id — the shard router picks the right DB. -/
  let row1 ← backend.query
    "SELECT value FROM kv WHERE user_id = ? AND key = ?" #["1", "name"]
  let row2 ← backend.query
    "SELECT value FROM kv WHERE user_id = ? AND key = ?" #["2", "name"]
  let row3 ← backend.query
    "SELECT value FROM kv WHERE user_id = ? AND key = ?" #["3", "name"]
  IO.println s!"user 1 → {row1.map (·.toList)}"
  IO.println s!"user 2 → {row2.map (·.toList)}"
  IO.println s!"user 3 → {row3.map (·.toList)}"

  /- 3. Distribution sanity check: count what landed on each shard. -/
  let countA ← LeanTea.Sqlite.query dbA "SELECT COUNT(*) FROM kv" #[]
  let countB ← LeanTea.Sqlite.query dbB "SELECT COUNT(*) FROM kv" #[]
  let nA := (countA[0]?.bind (·[0]?)).getD "?"
  let nB := (countB[0]?.bind (·[0]?)).getD "?"
  IO.println s!"shard A row count = {nA}"
  IO.println s!"shard B row count = {nB}"

  /- 4. Cache hit demo. After an exec the cache is cleared; the
        first read for "1" populates it, the second hits cache. -/
  let _ ← backend.exec
    "INSERT INTO kv(user_id, key, value) VALUES (?, ?, ?)" #["1", "city", "tokyo"]
  let q := "SELECT value FROM kv WHERE user_id = ? AND key = ?"
  let _ ← backend.query q #["1", "city"]    -- populates cache
  let key := cacheKey q #["1", "city"]
  -- Confirm it landed in cache:
  match ← cache.get key with
  | some _ => IO.println "cache populated ✓"
  | none   => IO.println "cache miss (unexpected)"

  /- 5. Cache invalidation on next write. We keep user_id as
        params[0] so the shard router routes consistently — production
        apps that shard usually adopt the same convention (or push
        explicit routing into application code). -/
  let _ ← backend.exec
    "DELETE FROM kv WHERE user_id = ? AND key = ?" #["1", "city"]
  let _ ← backend.exec
    "INSERT INTO kv(user_id, key, value) VALUES (?, ?, ?)"
    #["1", "city", "nagoya"]
  match ← cache.get key with
  | some _ => IO.println "cache NOT cleared after write (bug)"
  | none   => IO.println "cache cleared on write ✓"

  /- 6. New read picks up the fresh value. -/
  let after ← backend.query q #["1", "city"]
  IO.println s!"after update → {after.map (·.toList)}"

  IO.println "ok"
where
  cacheKey (sql : String) (ps : Array String) : String :=
    sql ++ "\x01" ++ String.intercalate "\x02" ps.toList
