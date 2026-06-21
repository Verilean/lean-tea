#!/usr/bin/env bash
# Build and run every doc_chXX binary, in order. Exits non-zero if
# any chapter prints an error or returns a bad exit code. This is
# what the user means by "run the docs before release" — the prose
# in docs/ cannot drift from the runnable code under examples/Docs.
set -euo pipefail

cd "$(dirname "$0")/.."

# Collect every doc_chNN lean_exe entry from lakefile.lean and run it
# in numeric order.
chapters=()
while IFS= read -r line; do
  chapters+=("$line")
done < <(grep -oE 'lean_exe doc_ch[0-9]+' lakefile.lean | awk '{print $2}' | sort -u)

if [ ${#chapters[@]} -eq 0 ]; then
  echo "no doc chapters found in lakefile.lean" >&2
  exit 1
fi

echo "→ building ${#chapters[@]} chapters …"
lake build "${chapters[@]}" > /tmp/lean-elm-docs-build.log 2>&1 || {
  cat /tmp/lean-elm-docs-build.log
  echo "build failed" >&2
  exit 1
}
echo "  ok"
echo

failed=0
for ch in "${chapters[@]}"; do
  echo "═════════════════════════════════════════════════════════════════"
  echo " $ch"
  echo "═════════════════════════════════════════════════════════════════"
  if ! ./.lake/build/bin/"$ch"; then
    failed=1
    echo "  $ch exited non-zero" >&2
  fi
  echo
done

if [ $failed -ne 0 ]; then
  echo "some chapters failed" >&2
  exit 1
fi
echo "all chapters ok"
