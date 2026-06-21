import LeanTea

open LeanTea.Persist

def main : IO Unit := do
  let path := "/tmp/leantea_smoke.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let s ← Store.open path
  IO.println s!"opened {path}"

  s.setPref "diff" "2"
  s.setPref "name" "junji"
  s.addScore "sc" 3 4 1700000000
  s.addScore "dict" 5 8 1700000010
  s.addScore "vocab" 30 40 1700000020
  s.markToday "2026-06-15" "shadow"
  s.markToday "2026-06-15" "dict"
  s.markToday "2026-06-14" "vocab"

  let diff ← s.getPref "diff"
  IO.println s!"pref diff = {diff}"
  let nm ← s.getPref "name"
  IO.println s!"pref name = {nm}"

  let scores ← s.recentScores
  IO.println s!"scores ({scores.size}):"
  for r in scores do
    IO.println s!"  {r.mode} {r.correct}/{r.total} @ {r.ts}"

  let today ← s.todayModes "2026-06-15"
  IO.println s!"today 2026-06-15 modes: {today}"

  let days ← s.daysWithEntries
  IO.println s!"days with entries: {days}"

  IO.println "ok"
