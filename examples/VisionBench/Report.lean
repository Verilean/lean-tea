import LeanTea

/-! # vision_bench_report — results.jsonl → comparison HTML

```
$ ./.lake/build/bin/vision_bench_report bench/vision/runs/*.jsonl \
    [--out bench/vision/report.html] [--data data]
```

Rolls every run file up per (model, strategy): locate hit rate (overall,
per target-size bucket, per screen kind), centre error, assert accuracy
and false-positive rate, OCR exact/CER, latency. Pass/fail against the
selection gate is shown per model, plus a gallery of locate misses with
the ground-truth (green) and predicted (red) boxes overlaid. Image paths
are relative to the report (`--data`, default `data`). -/

namespace VisionBench.Report

open Lean (Json)

structure Args where
  inputs : List String := []
  out    : String := "bench/vision/report.html"
  data   : String := "data"

partial def parseArgs : List String → Args → Args
  | "--out" :: v :: r, a  => parseArgs r { a with out := v }
  | "--data" :: v :: r, a => parseArgs r { a with data := v }
  | f :: r, a => parseArgs r { a with inputs := a.inputs ++ [f] }
  | [], a => a

def str (j : Json) (k : String) : String := (j.getObjValD k).getStr?.toOption.getD ""
def num (j : Json) (k : String) : Float :=
  match j.getObjValD k with | .num n => n.toFloat | _ => 0
def bool (j : Json) (k : String) : Bool :=
  match j.getObjValD k with | .bool b => b | _ => false
def floats (j : Json) (k : String) : Array Float :=
  ((j.getObjValD k).getArr?.toOption.getD #[]).map (fun x => match x with | .num n => n.toFloat | _ => 0)

def pct (n d : Nat) : String :=
  if d == 0 then "–" else
    let p := (n.toFloat * 1000 / d.toFloat).round / 10
    s!"{p}%"

def quantile (xs : Array Float) (q : Float) : Float :=
  if xs.isEmpty then 0 else
    let s := xs.qsort (· < ·)
    s[min (s.size - 1) (q * (s.size - 1).toFloat).round.toUInt64.toNat]!

def r1 (f : Float) : String := toString ((f * 10).round / 10)

def esc (s : String) : String :=
  s.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;" |>.replace "\"" "&quot;"

def buckets : List (String × Float × Float) :=
  [("<16", 0, 16), ("16–23", 16, 24), ("24–31", 24, 32), ("32–47", 32, 48), ("≥48", 48, 1e9)]

def kinds : List String := ["form", "icons", "grid", "canvas", "tui"]

structure Row where
  model    : String
  strategy : String
  locs     : Array Json
  deriving Inhabited

def hits (xs : Array Json) : Nat := (xs.filter (bool · "hit")).size

/-- Locate hit rate for targets in a size bucket. -/
def bucketHit (xs : Array Json) (lo hi : Float) : Nat × Nat :=
  let ys := xs.filter (fun j => let s := num j "size"; s >= lo && s < hi)
  (hits ys, ys.size)

def gate (row : Row) : Bool × String :=
  let (h24, n24) := row.locs.foldl (fun (h, n) j =>
    if num j "size" >= 24 then (h + (if bool j "hit" then 1 else 0), n + 1) else (h, n)) (0, 0)
  let (h16, n16) := bucketHit row.locs 16 24
  let ok24 := n24 > 0 && h24.toFloat / n24.toFloat >= 0.95
  let ok16 := n16 == 0 || h16.toFloat / n16.toFloat >= 0.85
  (ok24 && ok16, s!"≥24px {pct h24 n24} (≥95%), 16–23px {pct h16 n16} (≥85%)")

def css : String := "
:root{--bg:#fafafa;--fg:#1c1f24;--muted:#6b7280;--line:#e3e5e9;--card:#fff;--ok:#16794c;--bad:#b42318;--acc:#2563eb}
@media (prefers-color-scheme:dark){:root{--bg:#121417;--fg:#e7e9ee;--muted:#9aa1ad;--line:#2b3038;--card:#1a1d22;--ok:#4ade80;--bad:#f87171;--acc:#7aa2ff}}
body{margin:0;padding:24px 16px 64px;background:var(--bg);color:var(--fg);font:14px/1.5 -apple-system,'Hiragino Sans',sans-serif}
main{max-width:1200px;margin:0 auto}h1{font-size:22px;margin:0 0 4px}h2{font-size:17px;margin:32px 0 8px}
.sub{color:var(--muted);margin-bottom:16px}.wrap{overflow-x:auto}
table{border-collapse:collapse;background:var(--card);border:1px solid var(--line);font-variant-numeric:tabular-nums;width:100%}
th,td{padding:6px 10px;border-bottom:1px solid var(--line);text-align:right;white-space:nowrap}
th:first-child,td:first-child,th:nth-child(2),td:nth-child(2){text-align:left}
th{color:var(--muted);font-weight:600;font-size:12px}
.ok{color:var(--ok);font-weight:600}.bad{color:var(--bad);font-weight:600}
.gal{display:grid;grid-template-columns:repeat(auto-fill,minmax(360px,1fr));gap:12px}
.fig{background:var(--card);border:1px solid var(--line);border-radius:8px;overflow:hidden}
.fig .cap{padding:6px 10px;font-size:12px;color:var(--muted)}.fig .cap b{color:var(--fg)}
.view{position:relative;width:100%;aspect-ratio:1/1;overflow:hidden}
.view img,.view svg{position:absolute;left:0;top:0}
"

/-- A zoomed 320×320 window around the gt box, gt green, pred red. -/
def figure (dataDir : String) (j : Json) : String :=
  let gt := floats j "gt"
  let pr := floats j "pred"
  let cx := gt[0]! + gt[2]! / 2
  let cy := gt[1]! + gt[3]! / 2
  let span : Float := 320
  let x0 := max 0 (min (1280 - span) (cx - span / 2))
  let y0 := max 0 (min (800 - span) (cy - span / 2))
  let box (b : Array Float) (col : String) : String :=
    if b.size < 4 then "" else
    s!"<rect x='{r1 b[0]!}' y='{r1 b[1]!}' width='{r1 (max 2 b[2]!)}' height='{r1 (max 2 b[3]!)}' fill='none' stroke='{col}' stroke-width='2' vector-effect='non-scaling-stroke'/>"
  let dot := if pr.size < 4 then "" else
    s!"<circle cx='{r1 (pr[0]! + pr[2]! / 2)}' cy='{r1 (pr[1]! + pr[3]! / 2)}' r='3' fill='#e11d48'/>"
  let predTxt := if pr.size < 4 then "unparsed" else s!"err {r1 (num j "err")}px"
  s!"<div class='fig'><div class='view'>" ++
  s!"<svg viewBox='{r1 x0} {r1 y0} {span} {span}' width='100%' height='100%' preserveAspectRatio='none'>" ++
  s!"<image href='{dataDir}/{esc (str j "image")}' x='0' y='0' width='1280' height='800'/>" ++
  box gt "#16a34a" ++ box pr "#e11d48" ++ dot ++ "</svg></div>" ++
  s!"<div class='cap'><b>{esc (str j "query")}</b><br>{str j "model"} · {str j "strategy"} · {str j "kind"} · {r1 (num j "size")}px · {predTxt}</div></div>"

def main (argv : List String) : IO UInt32 := do
  let a := parseArgs argv {}
  if a.inputs.isEmpty then
    IO.eprintln "usage: vision_bench_report runs/*.jsonl [--out report.html]"
    return 1
  let mut all : Array Json := #[]
  for f in a.inputs do
    for line in (← IO.FS.lines f) do
      if let .ok j := Json.parse line then all := all.push j
  let models := all.foldl (fun acc j =>
    let m := str j "model"; if m.isEmpty || acc.contains m then acc else acc.push m) #[]
  let frameOf (m : String) : String :=
    (all.find? (fun j => str j "model" == m && str j "meta" == "frames")).map (str · "frame") |>.getD "?"
  let recs := all.filter (fun j => str j "meta" == "")
  let sel (m s t : String) := recs.filter (fun j => str j "model" == m && str j "strategy" == s && str j "task" == t)
  let rows : Array Row := models.foldl (fun acc m =>
    ["single", "zoom"].foldl (fun acc s =>
      let ls := sel m s "locate"
      if ls.isEmpty then acc else acc.push { model := m, strategy := s, locs := ls }) acc) #[]
  let mut h := "<!doctype html><html lang='ja'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>"
  h := h ++ s!"<title>VLM Bench Report</title><style>{css}</style></head><body><main>"
  h := h ++ s!"<h1>VLM grounding bench</h1><div class='sub'>{recs.size} scored answers · {models.size} models · sources: {esc (", ".intercalate a.inputs)}</div>"
  -- Locate summary
  h := h ++ "<h2>Locate — predicted centre inside the target box</h2><div class='wrap'><table><tr><th>model</th><th>strategy</th><th>frame</th><th>n</th><th>hit</th>"
  for (b, _, _) in buckets do h := h ++ s!"<th>{b}</th>"
  h := h ++ "<th>err p50</th><th>err p90</th><th>unparsed</th><th>ms p50</th><th>ms p90</th><th>gate</th></tr>"
  for row in rows do
    let errs := (row.locs.filter (bool · "parsed")).map (num · "err")
    let ms := row.locs.map (num · "ms")
    let unparsed := (row.locs.filter (!bool · "parsed")).size
    let (ok, why) := gate row
    h := h ++ s!"<tr><td>{esc row.model}</td><td>{row.strategy}</td><td>{frameOf row.model}</td><td>{row.locs.size}</td><td><b>{pct (hits row.locs) row.locs.size}</b></td>"
    for (_, lo, hi) in buckets do
      let (n, d) := bucketHit row.locs lo hi
      h := h ++ s!"<td>{pct n d}</td>"
    h := h ++ s!"<td>{r1 (quantile errs 0.5)}</td><td>{r1 (quantile errs 0.9)}</td><td>{pct unparsed row.locs.size}</td>"
    h := h ++ s!"<td>{(quantile ms 0.5).round}</td><td>{(quantile ms 0.9).round}</td>"
    h := h ++ s!"<td class='{if ok then "ok" else "bad"}' title='{esc why}'>{if ok then "PASS" else "FAIL"}</td></tr>"
  h := h ++ "</table></div>"
  -- Per kind
  h := h ++ "<h2>Locate hit rate by screen kind</h2><div class='wrap'><table><tr><th>model</th><th>strategy</th>"
  for k in kinds do h := h ++ s!"<th>{k}</th>"
  h := h ++ "</tr>"
  for row in rows do
    h := h ++ s!"<tr><td>{esc row.model}</td><td>{row.strategy}</td>"
    for k in kinds do
      let ys := row.locs.filter (str · "kind" == k)
      h := h ++ s!"<td>{pct (hits ys) ys.size}</td>"
    h := h ++ "</tr>"
  h := h ++ "</table></div>"
  -- Assert + OCR
  h := h ++ "<h2>Assert (yes/no) and OCR</h2><div class='wrap'><table><tr><th>model</th><th>frame</th><th>assert n</th><th>accuracy</th><th>false yes</th><th>false no</th><th>no answer</th><th>assert ms p50</th><th>ocr n</th><th>exact</th><th>CER mean</th><th>ocr ms p50</th></tr>"
  for m in models do
    let as := sel m "single" "assert"
    let oc := sel m "single" "ocr"
    let correct := (as.filter (bool · "correct")).size
    let negs := as.filter (!bool · "expect")
    let poss := as.filter (bool · "expect")
    let fy := (negs.filter (fun j => j.getObjValD "got" == .bool true)).size
    let fn := (poss.filter (fun j => j.getObjValD "got" == .bool false)).size
    let na := (as.filter (fun j => j.getObjValD "got" == .null)).size
    let exact := (oc.filter (bool · "exact")).size
    let cer := if oc.isEmpty then 0 else (oc.foldl (fun s j => s + num j "cer") 0) / oc.size.toFloat
    h := h ++ s!"<tr><td>{esc m}</td><td>{frameOf m}</td><td>{as.size}</td><td><b>{pct correct as.size}</b></td><td>{pct fy negs.size}</td><td>{pct fn poss.size}</td><td>{na}</td><td>{(quantile (as.map (num · "ms")) 0.5).round}</td>"
    h := h ++ s!"<td>{oc.size}</td><td><b>{pct exact oc.size}</b></td><td>{r1 (cer * 100)}%</td><td>{(quantile (oc.map (num · "ms")) 0.5).round}</td></tr>"
  h := h ++ "</table></div>"
  -- OCR misses
  let ocrMiss := recs.filter (fun j => str j "task" == "ocr" && !bool j "exact")
  if !ocrMiss.isEmpty then
    h := h ++ "<h2>OCR misses (first 40)</h2><div class='wrap'><table><tr><th>model</th><th>kind</th><th>expected</th><th>got</th></tr>"
    for j in ocrMiss.toList.take 40 do
      h := h ++ s!"<tr><td>{esc (str j "model")}</td><td>{str j "kind"}</td><td>{esc (str j "text")}</td><td>{esc (str j "got")}</td></tr>"
    h := h ++ "</table></div>"
  -- Miss gallery, per row
  for row in rows do
    let misses := row.locs.filter (!bool · "hit")
    if misses.isEmpty then continue
    h := h ++ s!"<h2>Misses — {esc row.model} / {row.strategy} ({misses.size})</h2><div class='gal'>"
    for j in misses.toList.take 24 do h := h ++ figure a.data j
    h := h ++ "</div>"
  h := h ++ "</main></body></html>"
  IO.FS.writeFile a.out h
  IO.println s!"wrote {a.out}"
  return 0

end VisionBench.Report

def main (argv : List String) : IO UInt32 := VisionBench.Report.main argv
