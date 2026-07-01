# LeanTea.Net.Server — perf, and what it says about the architecture

Short answer: **the current `serveConcurrent` doesn't scale past one
thread on this hardware.** Raw throughput is around 6-7 k RPS on all
three test handlers, and giving it more cores actually *lowers* RPS
slightly. This document has the numbers and a brief interpretation
so the pitch on the front page stays honest.

## Method

`examples/BenchServer/Main.lean` exposes three routes so we can
separate framework overhead from handler cost:

| route | shape |
|---|---|
| `GET /health` | returns the four-byte `"OK"`. Closest to "framework overhead only". |
| `GET /json`   | returns a five-field JSON via `Response.json` (through `Lean.Json.compress`). |
| `POST /echo`  | round-trips the request body. Exercises body read + response send. |

Load generator: **Apache Bench (ab)** — universal, one dependency,
same tool on every dev machine.

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
Concurrency = 64 in-flight connections, N = 50 000 requests per
data point. `ab -q -c 64 -n 50000`.

## Results

### GET /health (4-byte response body)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | **6 657** | 9        | 12       | 9.61     |
| 2                | 5 950     | 11       | 13       | 10.76    |
| 4                | 5 663     | 11       | 15       | 11.30    |
| 8                | 5 717     | 11       | 16       | 11.20    |
| 16               | 5 656     | 11       | 15       | 11.32    |

### GET /json (Response.json with 5 fields)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | **6 431** | 10       | 11       | 9.95     |
| 2                | 5 961     | 11       | 15       | 10.74    |
| 4                | 5 467     | 12       | 15       | 11.71    |
| 8                | 5 453     | 12       | 16       | 11.74    |
| 16               | 5 680     | 11       | 14       | 11.27    |

### POST /echo (5-byte body, round-tripped)

| LEAN_NUM_THREADS | RPS      | p50 (ms) | p99 (ms) | avg (ms) |
|-----------------:|---------:|---------:|---------:|---------:|
| 1                | **6 663** | 9        | 12       | 9.61     |
| 2                | 6 069     | 10       | 16       | 10.55    |
| 4                | 5 594     | 11       | 13       | 11.44    |
| 8                | 5 691     | 11       | 15       | 11.25    |
| 16               | 5 691     | 11       | 13       | 11.25    |

## Interpretation

Three things worth pointing at, in decreasing order of "makes the
front page inaccurate":

1. **Peak is at N=1 thread, not the core count.** Adding worker
   threads costs a few hundred RPS. That's the opposite of the
   scaling story that "Rust axum vs Haskell warp vs nginx" charts
   set up.

2. **JSON encoding doesn't visibly cost anything.** `/json` and
   `/health` are within noise of each other. So the bottleneck isn't
   codec or allocation — it's whatever the accept-and-dispatch loop
   is doing.

3. **Absolute ceiling ~6-7 k RPS.** nginx on the same box, serving
   static text of the same size, will do 80-120 k RPS. A tuned
   Haskell warp will do 40-80 k. So the framework is *nowhere near*
   line-rate for a plain-HTTP workload.

Why? The accept loop looks like this today
(`LeanTea/Net/Server.lean:70`):

```lean
partial def serveLoopConcurrent (server : Socket.Server) (handler : Handler)
    : IO Unit := do
  let client ← (server.accept).block
  let _ ← IO.asTask (handleConn handler client)
  serveLoopConcurrent server handler
```

That's a **single OS thread** calling `accept()` in a loop, then
handing each connection to a Lean `Task`. Every connection therefore
serialises on the accept, and every `handleConn` is a short computation
whose overhead (allocating the Task, waking a worker, marshalling
the response) is comparable to its useful work. With small handlers
that never yield, giving the scheduler more workers just adds
coordination cost — hence the mild regression as `LEAN_NUM_THREADS`
grows.

## What this means for the framing

The README's Yesod / Servant framing is about **API ergonomics**, not
throughput, and none of the perf work below invalidates that framing.
But some claims currently on the front page are ambition, not
measured reality:

- "on par with nginx / on par with wai" — **remove until real**.
  Nothing in this file supports either.
- "pure-Lean HTTP/1.1" — **fine, keep**. Zero external deps, buildable
  anywhere Lean builds, tiny binary. The trade is throughput; the
  win is deployability + auditability.

## What we'd have to change to scale

Not in this commit — this doc is the honest baseline. Design notes
for the next round:

- **SO_REUSEPORT + one accept loop per worker.** Multiple listener
  sockets on the same port, kernel round-robins. Every worker owns
  its own accept, no serialisation. This is how nginx, envoy, and
  Rust's reactor pattern (tokio + `axum::serve`) get linear scaling.
  Requires exposing the socket option via the Lean stdlib and
  spawning N accept loops instead of one.
- **Handler in-place instead of `IO.asTask` per connection.**
  For tiny handlers the task-spawn is pure overhead. A synchronous
  variant that runs the handler on the accept thread would probably
  peak higher on the current 1-thread numbers. `serveConcurrent`
  becomes the right choice only when a handler can genuinely block
  (LLM turn, DB query, external API call).
- **Keep-alive.** Every request in the current loop is one TCP
  connection: `accept → parse → send → shutdown`. Adding HTTP/1.1
  keep-alive removes the TCP + task cost from all but the first
  request in a session and typically 5-10×'s RPS on this class
  of benchmark.

All three are follow-up work with real API surface. The point of
this file is that the front page will stop implying otherwise.
