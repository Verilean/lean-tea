import LeanTea.Net.Server
import LeanTea.Net.FastServer
import LeanTea.Net.ReactorServer

/-! # LeanTea.Net.Backend — pick an HTTP backend at boot

Three server flavours ship in the framework and they all take the
same `Handler = Request → IO Response`. This module exposes them
through a single `Backend` enum + `Backend.serve` dispatcher so an
app's `main` can pick one via config (env var, CLI flag) without
touching handler code.

## The three flavours, one more time

| variant       | throughput   | idle-conn ceiling     | best for                  |
|---------------|--------------|------------------------|---------------------------|
| `.libuv`      | ~6 k RPS     | libuv (10 k+ idle)    | LLM proxy, WS, SSE, chat  |
| `.fast N`     | ~64 k RPS    | `LEAN_NUM_THREADS`    | short-request web APIs    |
| `.reactor`    | ~72 k RPS    | fd limit (100 k+)     | **default** for HTTP APIs |

## Typical use

```lean
open LeanTea LeanTea.Net.Http LeanTea.Net.Backend

def main : IO Unit := do
  let backend ← Backend.fromEnv (default := .reactor)
  backend.serve 8080 "0.0.0.0" myHandler
```

Then:

```sh
./app                                  # reactor (default)
LEANTEA_HTTP_BACKEND=libuv    ./app    # libuv Server (long-lived conns)
LEANTEA_HTTP_BACKEND=fast     ./app    # FastServer, 8 workers
LEANTEA_HTTP_BACKEND=fast:16  ./app    # FastServer, 16 workers
```

Keep in mind that the reactor runs the Lean handler synchronously on
the event-loop thread — a slow handler stalls the loop. If your app
needs to make outbound HTTP / DB calls per request, either use
`.libuv` (which yields on `.block`) or `IO.asTask` the slow work off. -/

namespace LeanTea.Net.Backend

open LeanTea.Net.Http

/-- Which HTTP server to run. See the file doc for trade-offs. -/
inductive Backend where
  /-- `LeanTea.Net.Server.serveConcurrent` — libuv-backed, low
      throughput but supports arbitrarily many idle keep-alive
      connections. -/
  | libuv
  /-- `LeanTea.Net.FastServer.serve` — POSIX-native FFI, N accept
      workers bound with `SO_REUSEPORT`, one OS thread per active
      connection. Very high throughput; total concurrent-conn count
      capped at `LEAN_NUM_THREADS`. -/
  | fast (workers : Nat)
  /-- `LeanTea.Net.ReactorServer.serve` — non-blocking kqueue/epoll
      event loop. Highest throughput on this box (matches nginx) and
      scales cleanly to 10 k+ idle connections. Default. -/
  | reactor
  deriving Repr

/-- Dispatch to the underlying server. `host` is only consumed by
    the libuv variant; the FFI variants always bind `INADDR_ANY`. -/
def serve (b : Backend) (port : UInt16) (host : String) (handler : Handler)
    : IO Unit :=
  match b with
  | .libuv         => LeanTea.Net.Server.serveConcurrent port host handler
  | .fast workers  => LeanTea.Net.FastServer.serve port workers handler
  | .reactor       => LeanTea.Net.ReactorServer.serve port handler

/-- Parse a spec like `libuv` / `fast` / `fast:16` / `reactor` into a
    `Backend`. Unknown strings return `none` so callers can fall back
    to a default or fail loudly. -/
def parse? (s : String) : Option Backend :=
  match s.trim.toLower with
  | "libuv"   => some .libuv
  | "reactor" => some .reactor
  | "fast"    => some (.fast 8)
  | other =>
    if other.startsWith "fast:" then
      let n := (other.drop 5).toNat?.getD 8
      some (.fast n)
    else
      none

/-- Read `LEANTEA_HTTP_BACKEND` from the environment and parse it.
    Falls back to `default` (reactor unless overridden) when the var
    is unset or unparseable. -/
def fromEnv (default : Backend := .reactor) : IO Backend := do
  match ← IO.getEnv "LEANTEA_HTTP_BACKEND" with
  | none   => return default
  | some s =>
    match parse? s with
    | some b => return b
    | none   =>
      IO.eprintln s!"LEANTEA_HTTP_BACKEND={s} not recognised; falling back to {repr default}"
      return default

end LeanTea.Net.Backend
