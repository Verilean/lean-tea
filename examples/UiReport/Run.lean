import LeanTea
import Lean.Data.Json

/-! # ui_report — turn a ui_script manifest into a portable HTML page

```
./ui_report                                # newest run under ~/.cache/leantea-agent/runs/
./ui_report path/to/run.json               # specific manifest
./ui_report --diff base.json head.json     # side-by-side (Phase 2)
```

The runner already writes `~/.cache/leantea-agent/runs/<name>.<ts>.json`
per script execution. This exe turns one of those into a single HTML
file with the tree, per-step evidence screenshots embedded inline, and
a verdict badge — same shape as the terminal tree, but clickable.

Output lands in `~/.cache/leantea-agent/reports/` next to the runs
dir, so the user can `open ~/.cache/leantea-agent/reports/<name>.html`
to inspect failures without scrolling a log. -/

open Lean (Json)
open LeanTea.Llm.Openai (base64Encode)

namespace UiReport

/-! ## Manifest accessors

We don't decode the manifest into a Lean struct because the runner
already owns the schema (`LeanTea.Agent.Script.ScriptResult.toJson`)
and re-deriving types here would just create drift. Plain Json
lookups are good enough — the manifest is small. -/

private def getStr? (j : Json) (k : String) : Option String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption)

private def getBool? (j : Json) (k : String) : Option Bool :=
  match j.getObjVal? k with
  | .ok (.bool b) => some b
  | _             => none

private def getNat? (j : Json) (k : String) : Option Nat :=
  match j.getObjVal? k with
  | .ok (.num n) => some n.mantissa.toNat
  | _            => none

private def getArr (j : Json) (k : String) : Array Json :=
  match (j.getObjVal? k).toOption.bind (·.getArr?.toOption) with
  | some arr => arr
  | none     => #[]

/-! ## HTML helpers -/

/-- Minimal HTML escape. Don't try to be clever — the script's notes
    might have `<` / `>` / `&` in them. -/
private def escape (s : String) : String :=
  s.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "\"" "&quot;"
   |>.replace "'" "&#39;"

private def fmtMs (ms : Nat) : String :=
  if ms < 1000 then s!"{ms}ms"
  else
    let s := ms / 1000
    let m := (ms % 1000) / 100
    s!"{s}.{m}s"

/-! ## Per-step rendering -/

/-- Read a PNG off disk, base64-encode it, inline as a `data:` URL.
    Returns the empty string if the file is missing (we still render
    the row; an absent screenshot becomes a placeholder note). -/
private def embedImage (path : String) : IO String := do
  if ← System.FilePath.pathExists path then
    let bytes ← IO.FS.readBinFile path
    return s!"data:image/png;base64,{base64Encode bytes}"
  else
    return ""

private def renderStep (step : Json) : IO String := do
  let idx      := getNat? step "index"     |>.getD 0
  let action   := getStr? step "action"    |>.getD "?"
  let expect   := getStr? step "expect"
  let observed := getStr? step "observed"
  let ok       := getBool? step "ok"       |>.getD false
  let dur      := getNat? step "durationMs" |>.getD 0
  let evidence := getStr? step "evidence"
  let err      := getStr? step "error"

  let img ← match evidence with
    | some p => embedImage p
    | none   => pure ""

  let cls := if ok then "ok" else "fail"
  let mark := if ok then "✓" else "✗"

  let expectHtml := match expect with
    | some e => s!"<div class=\"row\"><span class=\"k\">expect</span><span class=\"v\">{escape e}</span></div>"
    | none   => ""
  let observedHtml := match observed with
    | some o => s!"<div class=\"row\"><span class=\"k\">observed</span><span class=\"v\">{escape o}</span></div>"
    | none   => ""
  let errorHtml := match err with
    | some e =>
      let cleaned := (e.replace "\n" "<br>")
      s!"<div class=\"row err\"><span class=\"k\">error</span><span class=\"v\">{escape cleaned}</span></div>"
    | none   => ""
  let imgHtml :=
    if img.isEmpty then "<div class=\"row k\">(no screenshot)</div>"
    else s!"<div class=\"shot\"><img src=\"{img}\" loading=\"lazy\"/></div>"
  let evidencePath := match evidence with
    | some p => s!"<div class=\"row\"><span class=\"k\">📸</span><span class=\"v path\">{escape p}</span></div>"
    | none   => ""

  return s!"
<li class=\"step {cls}\">
  <div class=\"head\">
    <span class=\"idx\">[{idx}]</span>
    <span class=\"mark\">{mark}</span>
    <span class=\"action\">{escape action}</span>
    <span class=\"dur\">{fmtMs dur}</span>
  </div>
  <div class=\"detail\">
    {expectHtml}
    {observedHtml}
    {errorHtml}
    {evidencePath}
    {imgHtml}
  </div>
</li>
"

/-! ## Full page -/

private def css : String := "
:root {
  --bg: #f6f7f9;
  --fg: #1f2330;
  --muted: #6b7280;
  --ok: #16a34a;
  --fail: #dc2626;
  --skip: #9ca3af;
  --line: #e5e7eb;
  --shadow: 0 1px 2px rgba(0,0,0,.06), 0 4px 12px rgba(0,0,0,.04);
}
* { box-sizing: border-box; }
body {
  font: 14px/1.5 -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
  color: var(--fg);
  background: var(--bg);
  margin: 0; padding: 24px;
}
h1 { font-size: 20px; margin: 0 0 6px; }
.meta { color: var(--muted); margin-bottom: 16px; }
.verdict { padding: 2px 10px; border-radius: 999px; color: #fff; font-weight: 600; font-size: 12px; }
.verdict.PASS { background: var(--ok); }
.verdict.FAIL { background: var(--fail); }
.steps { list-style: none; padding: 0; margin: 0; }
.step {
  background: #fff;
  border: 1px solid var(--line);
  border-radius: 8px;
  margin: 10px 0;
  padding: 12px 14px;
  box-shadow: var(--shadow);
}
.step.ok   { border-left: 4px solid var(--ok); }
.step.fail { border-left: 4px solid var(--fail); }
.head { display: flex; gap: 12px; align-items: baseline; }
.head .idx  { color: var(--muted); font-family: ui-monospace, monospace; min-width: 36px; }
.head .mark { font-size: 16px; min-width: 16px; }
.step.ok   .head .mark { color: var(--ok); }
.step.fail .head .mark { color: var(--fail); }
.head .action { font-family: ui-monospace, monospace; flex: 1; }
.head .dur { color: var(--muted); font-variant-numeric: tabular-nums; }
.detail { margin-top: 10px; padding-left: 48px; }
.row { display: flex; gap: 10px; margin: 4px 0; }
.row .k { color: var(--muted); min-width: 80px; font-family: ui-monospace, monospace; font-size: 12px; }
.row .v { font-family: ui-monospace, monospace; word-break: break-word; }
.row .v.path { color: var(--muted); }
.row.err .v { color: var(--fail); white-space: pre-wrap; }
.shot { margin-top: 12px; }
.shot img {
  max-width: 100%; height: auto;
  border-radius: 6px; border: 1px solid var(--line);
  display: block;
  background: repeating-conic-gradient(#eee 0% 25%, #ddd 0% 50%) 0 0 / 16px 16px;
}
.shot img:hover { cursor: zoom-in; }
.footer { color: var(--muted); margin-top: 24px; font-size: 12px; }
"

/-! ## Single-run page -/

def renderManifest (manifest : Json) : IO String := do
  let name    := getStr? manifest "script"      |>.getD "(unnamed)"
  let descr   := getStr? manifest "description" |>.getD ""
  let passed  := getBool? manifest "passed"     |>.getD false
  let totalMs := getNat? manifest "totalMs"     |>.getD 0
  let steps   := getArr manifest "steps"
  let skipped := getNat? manifest "skipped"     |>.getD 0
  let verdict := if passed then "PASS" else "FAIL"

  let stepsHtml ← (steps.mapM renderStep).map (·.toList)
  let stepsBlock := String.intercalate "" stepsHtml

  let skippedRow :=
    if skipped > 0 then
      s!"<li class=\"step\" style=\"border-left-color: var(--skip)\"><div class=\"head\"><span class=\"idx\">⊘</span><span class=\"action\">{skipped} step(s) skipped after failure</span></div></li>"
    else ""

  let skippedSuffix := if skipped > 0 then s!" · {skipped} skipped" else ""
  let schemaName := getStr? manifest "schema" |>.getD "?"
  return s!"<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>{escape name} · ui_script</title>
<style>{css}</style>
</head>
<body>
<h1>{escape name} <span class=\"verdict {verdict}\">{verdict}</span></h1>
<div class=\"meta\">total {fmtMs totalMs} · {steps.size} step(s){skippedSuffix}</div>
<div class=\"meta\">{escape descr}</div>
<ol class=\"steps\">
{stepsBlock}
{skippedRow}
</ol>
<div class=\"footer\">generated by ui_report · schema {escape schemaName}</div>
</body>
</html>
"

/-! ## Diff page (two runs side by side)

Renders the two manifests as parallel columns so a reviewer can scan
top-to-bottom and see what changed between a known-good baseline and
the current run. Cross-step `observed` differences are highlighted
on the head side; everything else is the same per-step rendering as
the single-run page. -/

private def renderStepPair (b? h? : Option Json) : IO String := do
  /- Determine if the two sides agree on the headline status. If
     they don't, we tag the row so CSS can highlight it. -/
  let same :=
    match b?, h? with
    | some b, some h =>
      let bo := getBool? b "ok"
      let ho := getBool? h "ok"
      let bv := getStr?  b "observed"
      let hv := getStr?  h "observed"
      bo == ho && bv == hv
    | none, none => true
    | _, _       => false
  let rowCls := if same then "pair" else "pair diff"
  let cellOf : Option Json → IO String
    | some s => renderStep s
    | none   => pure "<li class=\"step skip\"><div class=\"head\"><span class=\"idx\">⊘</span><span class=\"action\">(not in this run)</span></div></li>"
  let bHtml ← cellOf b?
  let hHtml ← cellOf h?
  return s!"<div class=\"{rowCls}\"><div class=\"col base\"><ul class=\"steps\">{bHtml}</ul></div><div class=\"col head\"><ul class=\"steps\">{hHtml}</ul></div></div>"

def renderDiff (base : Json) (head : Json) : IO String := do
  let nameB    := getStr?  base "script" |>.getD "(base)"
  let nameH    := getStr?  head "script" |>.getD "(head)"
  let passedB  := getBool? base "passed" |>.getD false
  let passedH  := getBool? head "passed" |>.getD false
  let totalMsB := getNat?  base "totalMs" |>.getD 0
  let totalMsH := getNat?  head "totalMs" |>.getD 0
  let stepsB   := getArr base "steps"
  let stepsH   := getArr head "steps"
  let verdictB := if passedB then "PASS" else "FAIL"
  let verdictH := if passedH then "PASS" else "FAIL"
  let maxN     := max stepsB.size stepsH.size

  let mut rows : List String := []
  for i in [0:maxN] do
    let b? := stepsB[i]?
    let h? := stepsH[i]?
    rows := rows ++ [← renderStepPair b? h?]

  let diffCss := "
.pairs { padding: 0; }
.pair { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin: 6px 0; }
.pair.diff { background: #fff5f0; border-left: 4px solid #f97316; padding-left: 8px; }
.pair .col { min-width: 0; }
.colhdr { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; font-family: ui-monospace, monospace; color: var(--muted); margin: 6px 0 4px; }
"

  let body := String.intercalate "" rows
  return s!"<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>diff {escape nameB} vs {escape nameH} · ui_report</title>
<style>{css}{diffCss}</style>
</head>
<body>
<h1>diff <span class=\"verdict {verdictB}\">{verdictB}</span> → <span class=\"verdict {verdictH}\">{verdictH}</span></h1>
<div class=\"meta\">{escape nameB} ({fmtMs totalMsB}) → {escape nameH} ({fmtMs totalMsH})</div>
<div class=\"colhdr\"><div>base</div><div>head</div></div>
<div class=\"pairs\">
{body}
</div>
<div class=\"footer\">generated by ui_report --diff</div>
</body>
</html>
"

/-! ## CLI -/

private def listRuns : IO (Array String) := do
  let dir ← LeanTea.Agent.Memory.agentDir
  let runsDir := dir / "runs"
  if ! (← runsDir.pathExists) then return #[]
  let entries ← System.FilePath.readDir runsDir
  let paths := entries.filterMap fun e =>
    let p := e.path.toString
    if p.endsWith ".json" then some p else none
  return paths

private def newestRun : IO (Option String) := do
  let runs ← listRuns
  if runs.isEmpty then return none
  /- Filenames are `<name>.<monotonic-ms>.json`, so sort lexically
     gives us the most recent (monotonic ms is monotonically increasing). -/
  let sorted := runs.qsort (· > ·)
  return sorted[0]?

private def loadManifest (path : String) : IO Json := do
  let raw ← IO.FS.readFile path
  match Json.parse raw with
  | .ok j    => return j
  | .error e => throw <| IO.userError s!"bad manifest JSON at {path}: {e}"

def main (rawArgs : List String) : IO Unit := do
  let dir ← LeanTea.Agent.Memory.agentDir
  let reportsDir := dir / "reports"
  IO.FS.createDirAll reportsDir

  match rawArgs with
  | "--diff" :: basePath :: headPath :: _ =>
    let base ← loadManifest basePath
    let head ← loadManifest headPath
    let html ← renderDiff base head
    let baseStem := (System.FilePath.mk basePath).fileStem.getD "base"
    let headStem := (System.FilePath.mk headPath).fileStem.getD "head"
    let stem := s!"{baseStem}__vs__{headStem}"
    let outPath := reportsDir / s!"{stem}.html"
    IO.FS.writeFile outPath html
    IO.println outPath.toString
  | _ =>
    let manifestPath ← match rawArgs with
      | []      =>
        match ← newestRun with
        | some p => pure p
        | none   =>
          IO.eprintln "ui_report: no runs found under ~/.cache/leantea-agent/runs/"
          IO.Process.exit 2
      | p :: _  => pure p
    let manifest ← loadManifest manifestPath
    let html ← renderManifest manifest
    let stem := (System.FilePath.mk manifestPath).fileStem.getD "report"
    let outPath := reportsDir / s!"{stem}.html"
    IO.FS.writeFile outPath html
    IO.println outPath.toString

end UiReport

def main (args : List String) : IO Unit := UiReport.main args
