import LeanTea

/-! Smoke test for the Persistent-style typed query DSL.

Round-trips the existing `ScoreRow` entity through the new
`Repo.select / count / updateWhere / deleteWhere` ops plus operator
syntax for filters. -/

open LeanTea.Persist

/-! Column references for ScoreRow. -/
namespace ScoreRow
def modeC    : Col ScoreRow String := col "mode"    id
def correctC : Col ScoreRow Nat    := col "correct" toString
def totalC   : Col ScoreRow Nat    := col "total"   toString
def tsC      : Col ScoreRow Nat    := col "ts"      toString
end ScoreRow

def main : IO Unit := do
  let path := "/tmp/leantea_query_smoke.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let s ← Store.open path
  IO.println s!"opened {path}"

  s.addScore "sc"    3   4 1700000000
  s.addScore "dict"  5   8 1700000010
  s.addScore "vocab" 30 40 1700000020
  s.addScore "dict"  8   8 1700000030

  /- 1. Filter compile test (no DB) -/
  let f := ScoreRow.modeC ==. "dict"
       &&. ScoreRow.correctC >. 6
       ||. ScoreRow.totalC <. 10
  let (sql, ps) := f.compile
  IO.println s!"compile sql    : {sql}"
  IO.println s!"compile params : {ps}"

  /- 2. select via the DSL -/
  let dictRows ← s.history.select <|
    (Select.empty.where_ (ScoreRow.modeC ==. "dict")).orderBy ScoreRow.tsC.asc
  IO.println s!"dict rows ({dictRows.size}):"
  for r in dictRows do
    IO.println s!"  {r.mode} {r.correct}/{r.total} @ {r.ts}"

  /- 3. count -/
  let n ← s.history.count (ScoreRow.modeC ==. "dict")
  IO.println s!"dict count = {n}"

  /- 4. updateWhere — bump all 'sc' rows to ts = 9 -/
  let touched ← s.history.updateWhere (ScoreRow.modeC ==. "sc")
    [ScoreRow.tsC =. 9, ScoreRow.totalC =. 99]
  IO.println s!"updateWhere touched = {touched}"
  let scRows ← s.history.select <| Select.empty.where_ (ScoreRow.modeC ==. "sc")
  for r in scRows do
    IO.println s!"  after update: {r.mode} {r.correct}/{r.total} @ {r.ts}"

  /- 5. deleteWhere — wipe vocab rows -/
  let removed ← s.history.deleteWhere (ScoreRow.modeC ==. "vocab")
  IO.println s!"deleteWhere removed = {removed}"
  let all ← s.history.select Select.empty
  IO.println s!"surviving rows = {all.size}"

  IO.println "ok"
