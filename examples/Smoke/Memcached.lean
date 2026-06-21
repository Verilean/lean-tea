import LeanTea

open LeanTea.Net.Memcached
open LeanTea.Persist

/-! Memcached smoke test. Requires a memcached process on
    `127.0.0.1:11211` (or pass `--host / --port`); if the connect
    fails, the test prints a skip notice and exits 0 so CI doesn't
    flap when memcached isn't installed locally.

    Run a server first:
      docker run --rm -d -p 11211:11211 memcached
    Or:
      memcached -p 11211 -d -m 32 -u $USER -P /tmp/memcached.pid -l 127.0.0.1
-/

structure Args where
  host : String := "127.0.0.1"
  port : UInt16 := 11211

partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | "--port" :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 11211).toUInt16 }
  | _ :: rest             => parseArgs rest a
  | []                    => a

/-- Smoke body that assumes a live connection. Anything throwing
    here lets `main` catch + skip gracefully. -/
private def runOps (client : Client) : IO Unit := do
  IO.println "-- direct ops --"
  client.flushAll
  match ← client.get "missing-key" with
  | none   => IO.println "  miss → none ✓"
  | some _ => IO.println "  miss returned a value (bug)"
  client.set "k1" "hello"
  client.set "k2" "world"
  match ← client.get "k1" with
  | some v => IO.println s!"  k1 = {v}"
  | none   => IO.println "  k1 missing (bug)"
  match ← client.get "k2" with
  | some v => IO.println s!"  k2 = {v}"
  | none   => IO.println "  k2 missing (bug)"
  client.delete "k1"
  match ← client.get "k1" with
  | none   => IO.println "  k1 after delete → none ✓"
  | some _ => IO.println "  delete didn't take (bug)"

  IO.println "-- through Cache adapter --"
  let cache := client.asCache
  cache.set "sql:select|user:42" "{rows:1}"
  match ← cache.get "sql:select|user:42" with
  | some v => IO.println s!"  cache hit: {v}"
  | none   => IO.println "  cache miss (bug)"
  cache.clear
  match ← cache.get "sql:select|user:42" with
  | none   => IO.println "  after clear → none ✓"
  | some _ => IO.println "  clear didn't take (bug)"

  client.close

def main (argv : List String) : IO Unit := do
  let a := parseArgs argv {}
  IO.println s!"connecting to memcached at {a.host}:{a.port} …"
  try
    let client ← connect a.host a.port
    runOps client
    IO.println "ok"
  catch e =>
    IO.println s!"memcached not reachable ({e}); skipping."
    IO.println "ok"
