# LeanTea.Net.Server — perf, and what it says about the architecture

Two rounds of numbers so the shape of the story is clear:

1. **Round 1 (baseline).** No keep-alive, single-request-per-TCP-connection.
   Peak ~6.6 k RPS at `LEAN_NUM_THREADS=1`; adding threads *hurt*.
2. **Round 2 (after HTTP/1.1 keep-alive + `noDelay`).** The peak
   moves to `LEAN_NUM_THREADS=16` and RPS scales roughly with
   `sqrt(threads)` up to that ceiling. Absolute peak is comparable
   to round 1 — the ceiling is bounded by a single `accept` thread,
   not by codec, allocation, or scheduler overhead.

Both peaks land around **6-7 k RPS on all three test routes**.
nginx on the same box for a static file of the same size does
80-120 k RPS; a tuned Haskell warp does 40-80 k. This framework is
therefore **1-2 orders of magnitude below line-rate for a plain
HTTP workload**, and that's fine to say out loud — the pitch has
always been "pure Lean, no external HTTP dep," not "beats nginx."
Reaching linear scaling to core count is a plumbing job (SO_REUSEPORT
+ per-worker accept sockets) that needs a socket-option API in
`Std.Net`; a note at the bottom of this file describes the work.

## Method

`examples/BenchServer/Main.lean` exposes three routes so we can
separate framework overhead from handler cost:

| route | shape |
|---|---|
| `GET /health` | returns the four-byte `"OK"`. Closest to "framework overhead only". |
| `GET /json`   | returns a five-field JSON via `Response.json` (through `Lean.Json.compress`). |
| `POST /echo`  | round-trips the request body. Exercises body read + response send. |

Load generator: **Apache Bench (ab)** — universal, one dependency,
same tool on every dev machine. `ab -q -k -c 64 -n 50000` per data
point. `-k` requests HTTP/1.1 keep-alive on the client so we're
measuring per-request cost, not per-TCP-connection cost.

Server: `bench_server` uses `LeanTea.Net.Server.serveConcurrent`,
which fans every accepted connection out through `IO.asTask`. The
number of Lean task worker threads is controlled by the
`LEAN_NUM_THREADS` environment variable — we vary it across
`{1, 2, 4, 8, 16}` to observe scaling.

```sh
# Reproduce
lake build bench_server
./bench/run.sh health "1 2 4 8 16"
./bench/run.sh json   "1 2 4 8 16"
./bench/run.sh echo   "1 2 4 8 16"
```

Host: **Apple M-series laptop, 16 cores, 48 GB RAM, macOS 25.5**.

## Round 2 — with keep-alive

### GET /health (4-byte response body)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | 1 933    | 31       | 46       | 33.10    |
| 2                | 2 485    | 25       | 38       | 25.75    |
| 4                | 3 420    | 18       | 29       | 18.71    |
| 8                | 4 469    | 14       | 25       | 14.32    |
| 16               | **6 218** | 10       | 17       | 10.29    |

### GET /json (Response.json with 5 fields)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | 2 006    | 29       | 43       | 31.90    |
| 2                | 2 542    | 24       | 37       | 25.17    |
| 4                | 3 543    | 18       | 27       | 18.07    |
| 8                | 4 414    | 14       | 25       | 14.50    |
| 16               | **6 004** | 10       | 19       | 10.66    |

### POST /echo (5-byte body, round-tripped)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | 1 976    | 30       | 43       | 32.40    |
| 2                | 2 573    | 24       | 37       | 24.87    |
| 4                | 3 411    | 18       | 29       | 18.77    |
| 8                | 4 420    | 14       | 25       | 14.48    |
| 16               | **5 958** | 10       | 19       | 10.74    |

## Round 1 — no keep-alive (pre-change reference)

Kept in case anyone needs to reproduce the old behaviour or wants
to see the effect of the keep-alive change. Every entry here was
one-TCP-connection-per-request.

### GET /health (4-byte response body)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | **6 657** | 9        | 12       | 9.61     |
| 2                | 5 950    | 11       | 13       | 10.76    |
| 4                | 5 663    | 11       | 15       | 11.30    |
| 8                | 5 717    | 11       | 16       | 11.20    |
| 16               | 5 656    | 11       | 15       | 11.32    |

Adding threads made RPS *worse* — the task-spawn cost per short-lived
connection was larger than the parallelism benefit. That regression
disappeared once the connection was kept open across requests.

## What the two rounds mean side by side

The two extremes measure different things:

- **Round 1 (no keep-alive)** is dominated by `accept + shutdown`
  system calls. Each request pays for a fresh TCP three-way
  handshake plus the four-way close. With `LEAN_NUM_THREADS=1` all
  connection work happens on a single Lean task worker, no
  scheduler coordination, no cross-thread cache thrash — throughput
  peaks. Adding threads adds coordination cost without adding
  parallelism (the accept thread is still the bottleneck).
- **Round 2 (keep-alive)** amortises TCP setup across many
  requests, and now the per-request work is small enough that a
  single worker holds many connections' worth of state. `T=1`
  serialises 64 in-flight connections onto one worker → 1.9 k RPS.
  `T=16` spreads them → 6.2 k RPS.

Same absolute ceiling in both regimes because the accept loop is
still a single OS thread. Neither round frees us from that.

## Why the ceiling is at ~6-7 k RPS

The accept loop looks like this today (`LeanTea/Net/Server.lean`):

```lean
partial def serveLoopConcurrent (server : Socket.Server) (handler : Handler)
    : IO Unit := do
  let client ← (server.accept).block
  let _ ← IO.asTask (handleConn handler client)
  serveLoopConcurrent server handler
```

A **single OS thread** calls `accept()` in a loop and hands each
connection to a Lean `Task`. Even with keep-alive, that thread
still has to run the accept syscall for every new connection, and
`ab -k` reuses connections but does open one per concurrency
level. So the ceiling on new-connection rate is set by that lone
thread.

For a workload that's "many short-lived connections" this makes
the framework CPU-bound on one core.

## What this means for the front page

The README's Yesod / Servant framing is about **API ergonomics**,
not throughput, and none of the perf work below invalidates that
framing. But some claims that were on the front page are ambition,
not measured reality:

- "on par with nginx / on par with wai" — **removed**. Nothing in
  this file supports it.
- "pure-Lean HTTP/1.1" — **kept**. Zero external deps, buildable
  anywhere Lean builds, tiny binary. The trade is throughput; the
  win is deployability + auditability.

## What we still need to break the ceiling

- **`SO_REUSEPORT` + one accept loop per worker.** The single-thread
  accept bottleneck is the reason we can't go past ~6-7 k RPS. Modern
  nginx and Rust's `tokio::listen` scale by binding N listener
  sockets to the same port with `SO_REUSEPORT`; the kernel round-
  robins accept()s across them. Requires exposing the socket
  option through `Std.Net` (not exposed in Lean 4.31 today) and
  spawning N accept loops instead of one.
- **Better task placement.** `IO.asTask` chooses which worker
  runs the handler; with 64 persistent connections on 16 workers
  we'd like each worker to own its share instead of the scheduler
  redistributing on every wake. Requires a `spawnOn` variant, or
  hand-writing a pool that picks its own connections off a
  `Std.Channel`.

Both are work with real API surface and are staged behind Round 2.
The point of this file is that the front page will stop implying
we can already do that.
