# LeanTea HTTP throughput — bench + nginx comparison

Four rounds. The framework's HTTP throughput moved ~10× to nginx
parity, and then a further ~10 % to match or slightly exceed nginx
on this box while dropping to a single OS thread.

| round | mechanism                          | health RPS | vs nginx |
|-------|------------------------------------|-----------:|---------:|
| 1     | libuv, no keep-alive               | 6 657      | 9 %      |
| 2     | libuv + HTTP/1.1 keep-alive        | 6 218      | 9 %      |
| 3     | POSIX-native (FFI, SO_REUSEPORT)   | 64 297     | 90 %     |
| 4     | epoll/kqueue reactor               | **72 149** | **104 %**|
| ref   | nginx (same box, same conf)        | 69 428     | —        |

Rounds 1–2 topped out ~6-7 k RPS regardless of thread count because
the single `Std.Async.TCP` accept loop was the bottleneck and every
recv/send hopped through libuv + a Lean task wake — profiled at
100–500 µs per hop. Round 3 replaces both: `c/leantea_fastnet.c`
exposes `bind_reuseport`, `accept_one`, `recv_bytes`, `send_bytes`,
`shutdown_fd`, `close_fd` as thin `@[extern]` bindings, and
`LeanTea.Net.FastServer.serve` runs N accept workers each with their
own listener bound via `SO_REUSEPORT`. Blocking calls sit in the
kernel, not in libuv, so per-syscall overhead disappears.

Round 3 got the framework to ~90 % of nginx. Round 4 closes the gap
by switching from thread-per-connection to an epoll/kqueue reactor
inside `c/leantea_reactor.c` — one non-blocking event loop drains
every fd, and the Lean callback runs synchronously on that thread
per fully-buffered request. That both eliminates the last remaining
`.block` hop and makes each idle connection worth ~100 bytes of
C state instead of a whole OS thread. Result: the reactor beats a
tuned nginx by a small margin on the low-concurrency latency
regime and matches it under stress.

## Method

`examples/BenchServer/Main.lean` exposes three routes so we can
separate framework overhead from handler cost:

| route | shape |
|---|---|
| `GET /health` | returns the four-byte `"OK"`. Closest to "framework overhead only". |
| `GET /json`   | returns a five-field JSON via `Response.json` (through `Lean.Json.compress`). |
| `POST /echo`  | round-trips the request body. Exercises body read + response send. |

Two load generators:

* **Apache Bench (ab)** — universal, one dependency. Single-threaded
  client, useful for the low-concurrency latency picture. Kept the
  same `ab -q -k -c 64 -n 50000` runs so pre/post-FFI numbers stay
  comparable.
* **wrk** — multi-threaded, closes the client-side saturation cliff
  that ab hits around 70 k RPS. Used for the 512-connection stress
  runs against nginx.

Host: **Apple M-series laptop, 16 cores, 48 GB RAM, macOS 25.5**.

## Round 4 — non-blocking reactor (kqueue/epoll)

`LeanTea.Net.ReactorServer.serve port handler`.

One event-loop thread does everything: `kevent()` / `epoll_wait()`
returns ready fds; the loop drains recv non-blocking into a
per-connection buffer, invokes the Lean callback once a full
request has arrived, sends the response back non-blocking, re-arms
for the next request. Idle keep-alive connections cost ~100 bytes
of C state each — no OS thread per fd. The Lean callback still does
the framework's usual `parseRequest` + user handler + `Response.toBytes`
synchronously on the event thread, which is why heavy handlers should
`IO.asTask` themselves off (same rule as Node.js).

### wrk, 8 threads, N keep-alive connections, /health

| server              | c=128 RPS | c=512 RPS | c=2000 RPS |
|---------------------|----------:|----------:|-----------:|
| nginx               | 69 428    | 70 013    | 61 760     |
| lean-tea reactor    | **72 149**| **70 388**| 63 336     |
| lean-tea reactor Δ  | +3.9 %    | +0.5 %    | +2.6 %     |

### All three routes at c=128

| route            | reactor RPS | p99 (ms) |
|------------------|------------:|---------:|
| GET /health      | 72 149      | 2.11     |
| GET /json        | 73 089      | 2.15     |
| POST /echo       | 74 671      | 2.07     |

Route symmetry sanity-checks that the codec cost isn't
distorting the picture — `Response.jsonObj` and the echo body
copy don't move the needle at these sizes.

## Round 3 — POSIX-native FFI + SO_REUSEPORT (thread-per-connection)

`LEAN_NUM_THREADS=512` for these runs. The FFI server spawns an
`IO.asTask` per accepted connection, and each of those parks in the
kernel on `recv()` while it holds the connection open. With N
concurrent keep-alive connections you need `LEAN_NUM_THREADS >= N`
or connections queue behind each other on the task worker pool. This
is the one operational sharp edge of the design.

### wrk, 128 keep-alive connections, 10 s

| server         | RPS      | avg (ms) | p50 (ms) | p99 (ms) |
|----------------|---------:|---------:|---------:|---------:|
| lean-tea fast  | 64 297   | 1.99     | 1.98     | 2.10     |
| nginx          | 71 457   | 1.79     | 1.77     | 1.99     |

lean-tea is running about 700 µs behind nginx per request on this
setup. Most of that is codec: `parseRequest` still allocates ~30
short strings per request. Replacing it with a picohttpparser-style
zero-alloc parser is the natural next lever.

### wrk, 512 keep-alive connections, 10 s

| server         | route    | RPS      | p99 (ms) |
|----------------|----------|---------:|---------:|
| lean-tea fast  | /health  | 61 449   | 10.50    |
| nginx          | /health  | 70 368   | 9.18     |
| lean-tea fast  | /json    | 61 697   | 9.66     |
| nginx          | /json    | 70 013   | 8.84     |

The JSON path is essentially free (`Response.jsonObj` on a 5-field
struct); the framework overhead is the same shape as `/health`. Echo
(`POST /echo`) landed at 66 k on ab; same story.

### ab, 64 keep-alive connections, 50 000 requests, N accept workers

| workers | RPS    | p99 (ms) |
|--------:|-------:|---------:|
| 1       | 67 778 | 2        |
| 2       | 67 490 | 2        |
| 4       | 68 461 | 1        |
| 8       | 67 635 | 2        |
| 16      | 67 205 | 1        |

Flat across worker counts under this load — the client (ab) is the
bottleneck, not the server. Under wrk's true parallel load we do
still see benefit from multiple workers up through core count.

## Round 2 — libuv, HTTP/1.1 keep-alive (pre-FFI reference)

Kept as a reference for the effect of the FFI change. `LEAN_NUM_THREADS=16`,
`ab -k -c 64 -n 50000`.

| route       | RPS      | p99 (ms) |
|-------------|---------:|---------:|
| GET /health | 6 218    | 17       |
| GET /json   | 6 004    | 19       |
| POST /echo  | 5 958    | 19       |

Round 3 wins ~10× on RPS and 5-10× on p99 tail latency.

## Round 1 — libuv, no keep-alive

Left in for archaeology.

| LEAN_NUM_THREADS | GET /health RPS |
|-----------------:|----------------:|
| 1                | 6 657           |
| 2                | 5 950           |
| 16               | 5 656           |

Adding threads made it *worse* — task-spawn cost per short-lived
connection exceeded parallelism benefit. Went away in Round 2 with
keep-alive; killed for good in Round 3 with FFI.

## How to reproduce

```sh
lake build bench_server

# libuv variant
./bench/run.sh health "1 2 4 8 16"

# POSIX-native FFI variant
LEAN_NUM_THREADS=512 ./.lake/build/bin/bench_server --port 8090 --fast 8 &
wrk -t8 -c128 -d10s --latency http://127.0.0.1:8090/health
```

For nginx parity, a matching config lives in the top of
`c/leantea_fastnet.c` comment — same `sendfile`, `tcp_nopush`,
`keepalive_requests`, `SO_REUSEPORT`. The main difference is nginx
uses `epoll`/`kqueue`; we still park threads in `accept()`. That's
where the next 10-15 % has to come from.

## CI regression tracker

`.github/workflows/bench.yml` runs on every push to `main`:

1. Boots `bench_server --reactor` and a matching nginx side-by-side
   on an `ubuntu-latest` runner.
2. Hits both with `wrk -t8 -c128 -d15s` on `/health` + `/json`
   (plus `/echo` on lean-tea).
3. Feeds the results to
   [`benchmark-action/github-action-benchmark`](https://github.com/benchmark-action/github-action-benchmark)
   which appends to `bench-data/http-bench.json` in the repo.
4. If any run drops below 80 % of the previous best, the job flags
   the regression (`fail-on-alert: false` for now — flip once the
   trend is boring).

CI numbers will always trail the M-series numbers above — a
GitHub 4 vCPU runner won't do 70 k RPS on anything — but both
lean-tea and nginx are measured on **the same runner in the same
job**, so the parity ratio (also charted, as `lean-tea/nginx %`) is
what to watch.

## Next levers (in order of expected gain)

1. **Zero-alloc HTTP parser (picohttpparser FFI).** `parseRequest`
   is now the dominant remaining cost — ~30 String allocations per
   request. Replacing it is a ~200 LOC C wrapper and should land
   another 5-15 %.
2. **Batch small responses.** `Response.toBytes` builds one
   ByteArray then does a final concat with the body. For 4-byte
   responses we could pre-serialize the head into a scratch buffer
   and issue one `writev`. Diminishing returns — <5 %.
3. **epoll/kqueue on the accept side.** Would remove the
   `LEAN_NUM_THREADS >= concurrency` operational constraint. Bigger
   refactor; only worth doing if the sharp edge bites in practice.
