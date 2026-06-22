import LeanTea
import LeanTea.Net.Valkey
import LeanTea.LSpec

/-! # valkey_smoke — round-trip the Valkey client against a live server

Defaults to `127.0.0.1:6379` (override via `VALKEY_HOST` /
`VALKEY_PORT` env). The CI workflow spins up Valkey as a service.
Locally:

```sh
docker run -d --rm -p 6379:6379 valkey/valkey:latest
./.lake/build/bin/valkey_smoke
```

Covers PING / SET / GET / TTL / DEL / INCR / FLUSHDB. -/

open LeanTea LeanTea.LSpec
open LeanTea.Net.Valkey (Client connect)

def main : IO Unit := do
  let host := (← IO.getEnv "VALKEY_HOST").getD "127.0.0.1"
  let port : UInt16 :=
    match (← IO.getEnv "VALKEY_PORT").bind String.toNat? with
    | some n => n.toUInt16
    | none   => 6379
  IO.println s!"valkey_smoke — {host}:{port}"
  let c ← LeanTea.Net.Valkey.connect host port

  /- Make sure we start clean. -/
  c.flushDb

  let pong       ← c.ping
  /- SET + GET round-trip. -/
  c.set "k1" "value-of-k1"
  let got1       ← c.get "k1"
  /- Missing key returns none. -/
  let gotMissing ← c.get "nope"
  /- SET with TTL still works; we don't assert the actual TTL here. -/
  c.set "k2" "ephemeral" (ttl := 60)
  let got2       ← c.get "k2"
  /- INCR / INCRBY. -/
  c.set "counter" "10"
  let inc1       ← c.incr "counter"
  let inc2       ← c.incrBy "counter" 5
  /- DEL returns 1 for an existing key, 0 for missing. -/
  let del1       ← c.del "k1"
  let del0       ← c.del "k1"

  c.close

  let spec : LSpec := group "Valkey round-trip" [
    it "PING returns PONG"                 (pong == "PONG"),
    it "SET+GET round-trip"                (got1 == some "value-of-k1"),
    it "GET missing key → none"            (gotMissing == none),
    it "SET with TTL still readable"       (got2 == some "ephemeral"),
    it "INCR returns new value 11"         (inc1 == 11),
    it "INCRBY 5 returns 16"               (inc2 == 16),
    it "DEL existing returns 1"            (del1 == 1),
    it "DEL absent returns 0"              (del0 == 0)
  ]
  let code ← lspecIO (group "LeanTEA Valkey" [spec])
  if code != 0 then IO.Process.exit code.toUInt8
