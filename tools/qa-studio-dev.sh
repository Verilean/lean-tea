#!/usr/bin/env bash
# Run qa_studio and rebuild + restart it whenever its sources change.
#
#   tools/qa-studio-dev.sh --url https://example.com/game --profile ~/.cache/qa-profile --clip auto --headed
#
# What survives a restart:
#   - the browser: it runs on its own (--browser-port, default 9333), so
#     the game stays logged in and on the same screen;
#   - the recorded steps, goals and chat (--dir/state.json);
#   - the open studio tab: it reconnects by itself.
# page.html is re-read on every load (--dev), so UI edits need only a
# reload. A run in progress when the restart happens is lost.
# If a build fails the running studio is left alone.
# To close the kept browser: pkill -f remote-debugging-port=9333
set -u
cd "$(dirname "$0")/.."

port="${QA_BROWSER_PORT:-9333}"
watch=(LeanTea examples/QaStudio tools/browser-bridge/bridge.js)
stamp="$(mktemp -t qa-studio-dev)"
pid=""

start() {
  ./.lake/build/bin/qa_studio --dev --browser-port "$port" "$@" &
  pid=$!
}
stop() {
  if [ -n "$pid" ]; then kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null; pid=""; fi
}
trap 'stop; rm -f "$stamp"; exit 0' INT TERM

touch "$stamp"
lake build qa_studio || exit 1
start "$@"
while true; do
  sleep 1
  changed="$(find "${watch[@]}" -newer "$stamp" -type f \( -name '*.lean' -o -name '*.js' \) 2>/dev/null | head -1)"
  [ -z "$changed" ] && continue
  touch "$stamp"
  echo "qa-studio-dev: $changed changed — rebuilding"
  if lake build qa_studio; then
    stop
    start "$@"
  else
    echo "qa-studio-dev: build failed — the running studio is unchanged"
  fi
done
