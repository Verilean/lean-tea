import LeanTea

/-! # vision_bench_run — score VLMs on the labelled dataset

```
$ ./.lake/build/bin/vision_bench_run \
    --models qwen/qwen3-vl-4b,google/gemma-3-4b \
    [--strategies single,zoom] [--tasks locate,assert,ocr] \
    [--limit 20]            # per (kind, task); 0 = all
    [--data bench/vision/data] [--out bench/vision/runs/<stamp>.jsonl]
```

Per model:
1. `locate/single` on every sampled locate item, recording raw coords.
2. Pick the `CoordFrame` with the best hit rate (or use `model=frame`
   from `--models` to force one) — this is how a model's convention
   is discovered rather than assumed.
3. `locate/zoom` with that frame (the coarse pass hits the cache).
4. `assert` and `ocr`.

One JSON line per (model, strategy, sample); `vision_bench_report`
turns them into the comparison page. VLM answers are cached under
`bench/vision/cache/`, so re-runs and metric tweaks are free. -/

namespace VisionBench.Run

open Lean (Json)
open LeanTea.Vision

structure Args where
  data       : String := "bench/vision/data"
  out?       : Option String := none
  models     : List String := []
  strategies : List String := ["single", "zoom"]
  tasks      : List String := ["locate", "assert", "ocr"]
  limit      : Nat := 20
  baseUrl    : String := "http://127.0.0.1:11211/v1"
  zoom       : Nat := 3
  maxTokens  : Nat := 256

partial def parseArgs : List String → Args → Args
  | "--data" :: v :: r, a       => parseArgs r { a with data := v }
  | "--out" :: v :: r, a        => parseArgs r { a with out? := some v }
  | "--models" :: v :: r, a     => parseArgs r { a with models := v.splitOn "," }
  | "--strategies" :: v :: r, a => parseArgs r { a with strategies := v.splitOn "," }
  | "--tasks" :: v :: r, a      => parseArgs r { a with tasks := v.splitOn "," }
  | "--limit" :: v :: r, a      => parseArgs r { a with limit := v.toNat! }
  | "--base-url" :: v :: r, a   => parseArgs r { a with baseUrl := v }
  | "--zoom" :: v :: r, a       => parseArgs r { a with zoom := v.toNat! }
  | "--max-tokens" :: v :: r, a => parseArgs r { a with maxTokens := v.toNat! }
  | _ :: r, a => parseArgs r a
  | [], a => a

structure Sample where
  id    : String
  image : String
  kind  : String
  task  : String
  json  : Json
  deriving Inhabited

def Sample.str (s : Sample) (k : String) : String :=
  (s.json.getObjValD k).getStr?.toOption.getD ""

def jsonFloat (j : Json) : Float :=
  match j with
  | .num n => n.toFloat
  | _ => 0

def Sample.bbox? (s : Sample) : Option Rect :=
  match (s.json.getObjValD "bbox").getArr? with
  | .ok #[x, y, w, h] => some { x := jsonFloat x, y := jsonFloat y, w := jsonFloat w, h := jsonFloat h }
  | _ => none

def Sample.expect (s : Sample) : Bool :=
  match s.json.getObjValD "expect" with | .bool b => b | _ => false

def loadSamples (path : System.FilePath) : IO (Array Sample) := do
  let mut out := #[]
  for line in (← IO.FS.lines path) do
    if line.isEmpty then continue
    let j ← IO.ofExcept (Json.parse line)
    let g k := (j.getObjValD k).getStr?.toOption.getD ""
    out := out.push { id := g "id", image := g "image", kind := g "kind", task := g "task", json := j }
  return out

/-- Deterministic subsample: up to `n` per (kind, task), ordered by id hash. -/
def subsample (xs : Array Sample) (n : Nat) : Array Sample := Id.run do
  if n == 0 then return xs
  let sorted := xs.qsort (fun a b => hash a.id < hash b.id)
  let mut counts : Std.HashMap (String × String) Nat := {}
  let mut out := #[]
  for s in sorted do
    let k := (s.kind, s.task)
    let c := counts.getD k 0
    if c < n then
      out := out.push s
      counts := counts.insert k (c + 1)
  return out

def levenshtein (a b : List Char) : Nat := Id.run do
  let bArr := b.toArray
  let mut prev : Array Nat := Array.range (bArr.size + 1)
  for ca in a do
    let mut cur : Array Nat := #[prev[0]! + 1]
    for j in [0:bArr.size] do
      let cost := if ca == bArr[j]! then 0 else 1
      cur := cur.push (min (min (prev[j+1]! + 1) (cur[j]! + 1)) (prev[j]! + cost))
    prev := cur
  return prev[bArr.size]!

def fl (f : Float) : Json :=
  match Json.parse (toString ((f * 10).round / 10)) with
  | .ok j => j
  | .error _ => Json.num 0

def rectJson (r : Rect) : Json := Json.arr #[fl r.x, fl r.y, fl r.w, fl r.h]

def rawJson : Option RawCoords → Json
  | some (.box a b c d) => Json.arr #[fl a, fl b, fl c, fl d]
  | some (.point a b)   => Json.arr #[fl a, fl b]
  | none => Json.null

/-- Locate metrics: hit = predicted centre inside the gt box. -/
def scoreLocate (gt : Rect) (pred? : Option Rect) : List (String × Json) :=
  match pred? with
  | none => [("parsed", .bool false), ("hit", .bool false)]
  | some p =>
    let dx := p.cx - gt.cx
    let dy := p.cy - gt.cy
    [("parsed", .bool true), ("pred", rectJson p),
     ("hit", .bool (gt.contains p.cx p.cy)),
     ("err", fl (Float.sqrt (dx * dx + dy * dy))), ("iou", fl (gt.iou p))]

structure Ctx where
  args  : Args
  out   : IO.FS.Handle
  data  : System.FilePath
  images : IO.Ref (Std.HashMap String Image)

def Ctx.image (c : Ctx) (name : String) : IO Image := do
  if let some img := (← c.images.get).get? name then return img
  let img ← Image.load (c.data / name)
  c.images.modify (·.insert name img)
  return img

def emit (c : Ctx) (fields : List (String × Json)) : IO Unit := do
  c.out.putStrLn (Json.mkObj fields).compress
  c.out.flush

def common (model strategy : String) (s : Sample) : List (String × Json) :=
  [("model", .str model), ("strategy", .str strategy), ("id", .str s.id),
   ("kind", .str s.kind), ("task", .str s.task), ("image", .str s.image)]

def runModel (c : Ctx) (spec : String) (samples : Array Sample) : IO Unit := do
  let (model, forced?) := match spec.splitOn "=" with
    | [m, f] => (m, CoordFrame.ofName? f)
    | _ => (spec, none)
  let base : Vlm := {
    cfg := { baseUrl := c.args.baseUrl, timeoutSec := some 600 },
    model, maxTokens := c.args.maxTokens, cacheDir? := some ("bench/vision/cache" : System.FilePath) }
  IO.println s!"══ {model}"
  -- Warm-up so JIT model load time doesn't pollute latency numbers.
  try
    let _ ← ({ base with cacheDir? := none } : Vlm).call "Say OK." (← (← c.image samples[0]!.image).dataUrl)
  catch e => IO.eprintln s!"  warm-up failed: {e}"
  let locs := samples.filter (·.task == "locate")
  -- 1. single pass, raw coords kept so every frame can be scored.
  let mut singles : Array (Sample × Located × Nat × Nat) := #[]
  if c.args.tasks.contains "locate" then
    let mut i := 0
    for s in locs do
      let img ← c.image s.image
      let r ← try locate base img (s.str "query")
              catch e => pure { rect? := none, raw? := none, answer := s!"ERROR: {e}", ms := 0 }
      singles := singles.push (s, r, img.width, img.height)
      i := i + 1
      if i % 20 == 0 then IO.println s!"  locate/single {i}/{locs.size}"
  -- 2. frame discovery.
  let hitsFor (f : CoordFrame) : Nat := singles.foldl (fun n (s, r, w, h) =>
    match s.bbox?, r.raw? with
    | some gt, some raw => let p := raw.toRect f w h; if gt.contains p.cx p.cy then n + 1 else n
    | _, _ => n) 0
  let frameHits := CoordFrame.all.map (fun f => (f, hitsFor f))
  let best := frameHits.foldl (fun acc x => if x.2 > acc.2 then x else acc) (CoordFrame.guess model, 0)
  let frame := forced?.getD best.1
  IO.println s!"  frame hits: {frameHits.map (fun (f, n) => s!"{f.name}={n}")} → {frame.name}"
  emit c [("model", .str model), ("meta", .str "frames"), ("frame", .str frame.name),
    ("frameHits", Json.mkObj (frameHits.map (fun (f, n) => (f.name, Json.num (Int.ofNat n)))))]
  let v := { base with frame }
  if c.args.strategies.contains "single" then
    for (s, r, w, h) in singles do
      let gt := s.bbox?.getD default
      let pred? := r.raw?.map (·.toRect frame w h)
      emit c (common model "single" s ++ [("query", .str (s.str "query")), ("gt", rectJson gt),
        ("size", fl (min gt.w gt.h)), ("raw", rawJson r.raw?), ("ms", .num (Int.ofNat r.ms)),
        ("answer", .str r.answer)] ++ scoreLocate gt pred?)
  -- 3. zoom-refine.
  if c.args.strategies.contains "zoom" && c.args.tasks.contains "locate" then
    let mut i := 0
    for s in locs do
      let img ← c.image s.image
      let gt := s.bbox?.getD default
      let r ← try locateZoom v img (s.str "query") c.args.zoom
              catch e => pure { rect? := none, raw? := none, answer := s!"ERROR: {e}", ms := 0 }
      emit c (common model "zoom" s ++ [("query", .str (s.str "query")), ("gt", rectJson gt),
        ("size", fl (min gt.w gt.h)), ("ms", .num (Int.ofNat r.ms)), ("answer", .str r.answer)]
        ++ scoreLocate gt r.rect?)
      i := i + 1
      if i % 20 == 0 then IO.println s!"  locate/zoom {i}/{locs.size}"
  -- 4. assert / ocr.
  if c.args.tasks.contains "assert" then
    for s in samples.filter (·.task == "assert") do
      let img ← c.image s.image
      let r ← try ask v img (s.str "question")
              catch e => pure { yes? := none, answer := s!"ERROR: {e}", ms := 0 }
      emit c (common model "single" s ++ [("question", .str (s.str "question")),
        ("expect", .bool s.expect),
        ("got", match r.yes? with | some b => .bool b | none => .null),
        ("correct", .bool (r.yes? == some s.expect)),
        ("ms", .num (Int.ofNat r.ms)), ("answer", .str r.answer)])
    IO.println "  assert done"
  if c.args.tasks.contains "ocr" then
    for s in samples.filter (·.task == "ocr") do
      let img ← c.image s.image
      let gt := s.bbox?.getD default
      let want := s.str "text"
      let r ← try readText v img gt catch e => pure { text := s!"ERROR: {e}", ms := 0 }
      let got := r.text
      let d := levenshtein want.toList got.toList
      emit c (common model "single" s ++ [("text", .str want), ("got", .str got),
        ("exact", .bool (want == got)),
        ("cer", fl (d.toFloat / (max 1 want.length).toFloat)),
        ("ms", .num (Int.ofNat r.ms))])
    IO.println "  ocr done"

def main (argv : List String) : IO UInt32 := do
  let a := parseArgs argv {}
  if a.models.isEmpty then
    IO.eprintln "usage: vision_bench_run --models m1[=frame],m2 [--limit N] [--strategies single,zoom] [--tasks locate,assert,ocr]"
    return 1
  let data : System.FilePath := a.data
  let samples := subsample (← loadSamples (data / "labels.jsonl")) a.limit
  let samples := samples.filter (a.tasks.contains ·.task)
  let stamp ← IO.monoMsNow
  let outPath : System.FilePath := a.out?.getD s!"bench/vision/runs/run-{stamp}.jsonl"
  if let some p := outPath.parent then IO.FS.createDirAll p
  let h ← IO.FS.Handle.mk outPath .append
  IO.println s!"{samples.size} samples, models={a.models} → {outPath}"
  let c : Ctx := { args := a, out := h, data, images := ← IO.mkRef {} }
  for m in a.models do
    runModel c m samples
  IO.println s!"done → {outPath}"
  return 0

end VisionBench.Run

def main (argv : List String) : IO UInt32 := VisionBench.Run.main argv
