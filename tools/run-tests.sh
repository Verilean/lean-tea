#!/usr/bin/env bash
# Build and run every LSpec-based spec binary. Currently just
# `leanjs_spec`; new specs will be picked up automatically as we
# add them. Exits non-zero on any test failure so CI / a release
# checklist can use this as a single gate.
set -euo pipefail

cd "$(dirname "$0")/.."

specs=()
while IFS= read -r line; do
  specs+=("$line")
done < <(grep -oE 'lean_exe [a-zA-Z_]+_spec' lakefile.lean | awk '{print $2}' | sort -u)

if [ ${#specs[@]} -eq 0 ]; then
  echo "no _spec binaries found in lakefile.lean" >&2
  exit 1
fi

echo "→ building ${#specs[@]} spec target(s) …"
lake build "${specs[@]}" > /tmp/lean-elm-specs-build.log 2>&1 || {
  cat /tmp/lean-elm-specs-build.log
  echo "build failed" >&2
  exit 1
}
echo "  ok"
echo

failed=0
for spec in "${specs[@]}"; do
  echo "═════════════════════════════════════════════════════════════════"
  echo " $spec"
  echo "═════════════════════════════════════════════════════════════════"
  if ! ./.lake/build/bin/"$spec"; then
    failed=1
    echo "  $spec exited non-zero" >&2
  fi
  echo
done

if [ $failed -ne 0 ]; then
  echo "some specs failed" >&2
  exit 1
fi
echo "all specs ok"
