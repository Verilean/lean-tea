#!/usr/bin/env bash
# bench/run.sh — scale-with-cores harness for LeanTea.Net.Server
#
# For each value of LEAN_NUM_THREADS in the list, spawn bench_server,
# hit it with `ab -c $conc -n $req $route`, record RPS + latency,
# tear it down, and print a summary table.
#
# Usage:
#   ./bench/run.sh [ROUTE] [THREADS_LIST]
#     ROUTE          — one of  health | json | echo   (default: health)
#     THREADS_LIST   — space-separated list of LEAN_NUM_THREADS values
#                       (default: "1 2 4 8 16")
#
# Prereqs:
#   - bench_server built:  `lake build bench_server`
#   - Apache Bench (ab) on PATH  (`brew install httpd` or system pkg)
#
# The concurrency is fixed at 64 in-flight connections so we're
# measuring server-side scaling, not client-side saturation. If you
# want to push harder, edit CONCURRENCY / REQUESTS below.

set -euo pipefail

ROUTE=${1:-health}
THREADS=${2:-"1 2 4 8 16"}
PORT=${PORT:-8080}
CONCURRENCY=${CONCURRENCY:-64}
REQUESTS=${REQUESTS:-50000}
SERVER=./.lake/build/bin/bench_server

case "$ROUTE" in
  health) URL="http://127.0.0.1:${PORT}/health"; METHOD=GET  ;;
  json)   URL="http://127.0.0.1:${PORT}/json";   METHOD=GET  ;;
  echo)   URL="http://127.0.0.1:${PORT}/echo";   METHOD=POST ;;
  *) echo "unknown route: $ROUTE" >&2; exit 2 ;;
esac

if [[ ! -x "$SERVER" ]]; then
  echo "bench_server not built. run: lake build bench_server" >&2
  exit 1
fi

# Warm the executable page cache once so the first run isn't skewed.
LEAN_NUM_THREADS=1 "$SERVER" --port "$PORT" >/dev/null 2>&1 &
warm_pid=$!
sleep 0.5
curl -s "http://127.0.0.1:${PORT}/health" >/dev/null || true
kill "$warm_pid" 2>/dev/null || true
wait "$warm_pid" 2>/dev/null || true

printf "\n== lean-tea bench (%s, c=%d, n=%d) ==\n" "$ROUTE" "$CONCURRENCY" "$REQUESTS"
printf "%-4s  %-10s  %-10s  %-10s  %-10s\n" "T" "RPS" "p50(ms)" "p99(ms)" "avg(ms)"
printf "%s\n" "$(printf '%.0s-' {1..56})"

for T in $THREADS; do
  LEAN_NUM_THREADS="$T" "$SERVER" --port "$PORT" >/dev/null 2>&1 &
  pid=$!
  # Wait for the socket to be listening.
  for _ in {1..30}; do
    if nc -z 127.0.0.1 "$PORT" 2>/dev/null; then break; fi
    sleep 0.1
  done

  # For POST /echo we need a body. `-k` requests HTTP/1.1 keep-alive
  # so we're measuring per-request cost, not per-TCP-connection cost.
  if [[ "$METHOD" = POST ]]; then
    tmp=$(mktemp)
    printf 'hello' > "$tmp"
    out=$(ab -q -k -c "$CONCURRENCY" -n "$REQUESTS" -p "$tmp" -T application/octet-stream "$URL" 2>&1) || true
    rm -f "$tmp"
  else
    out=$(ab -q -k -c "$CONCURRENCY" -n "$REQUESTS" "$URL" 2>&1) || true
  fi

  # Extract metrics from ab output.
  rps=$(printf "%s\n" "$out" | awk '/Requests per second:/ {print $4; exit}')
  avg=$(printf "%s\n" "$out" | awk '/Time per request:/ && /concurrent requests/{ next } /Time per request:/ {print $4; exit}')
  p50=$(printf "%s\n" "$out" | awk '/^\s*50%/  {print $2; exit}')
  p99=$(printf "%s\n" "$out" | awk '/^\s*99%/  {print $2; exit}')

  printf "%-4s  %-10s  %-10s  %-10s  %-10s\n" "$T" "${rps:--}" "${p50:--}" "${p99:--}" "${avg:--}"

  kill "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
  sleep 0.2
done
