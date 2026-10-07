import LeanTea

/-! # vision_bench_gen — build the labelled screenshot dataset

Renders `examples/VisionBench/gen.html` in headless Chromium for each
`kind × seed`, saves the screenshot, and appends the page's ground
truth to `labels.jsonl`:

```
$ lake build vision_bench_gen
$ ./.lake/build/bin/vision_bench_gen [--out bench/vision/data] [--seeds 12]
                                     [--kinds form,icons,grid,canvas,tui]
```

Each label line: `{id, image, kind, theme, task, query|question|text,
bbox?, expect?, size?}` — bboxes are `[x, y, w, h]` in screenshot px. -/

namespace VisionBench.Gen

open Lean (Json)
open LeanTea.Browser

structure Args where
  out   : String := "bench/vision/data"
  seeds : Nat := 12
  kinds : List String := ["form", "icons", "grid", "canvas", "tui"]

partial def parseArgs : List String → Args → Args
  | "--out" :: v :: rest, a   => parseArgs rest { a with out := v }
  | "--seeds" :: v :: rest, a => parseArgs rest { a with seeds := v.toNat! }
  | "--kinds" :: v :: rest, a => parseArgs rest { a with kinds := v.splitOn "," }
  | _ :: rest, a => parseArgs rest a
  | [], a => a

def main (argv : List String) : IO UInt32 := do
  let a := parseArgs argv {}
  let cwd ← IO.currentDir
  let page := cwd / "examples" / "VisionBench" / "gen.html"
  unless ← page.pathExists do
    IO.eprintln s!"missing {page} — run from the repo root"
    return 1
  let outDir : System.FilePath := a.out
  IO.FS.createDirAll outDir
  let labelsPath := outDir / "labels.jsonl"
  let h ← IO.FS.Handle.mk labelsPath .write
  let mut n := 0
  let s ← Session.spawn
  try
    let _ ← s.open 1280 800 (headless := some true)
    for kind in a.kinds do
      for seed in List.range a.seeds do
        let seed := seed + 1
        let _ ← s.navigate s!"file://{page}?kind={kind}&seed={seed}"
        let j ← s.evaluate "JSON.stringify({meta: window.__meta, samples: window.__samples})"
        let j ← match j.getStr? with
          | .ok str => IO.ofExcept (Json.parse str)
          | .error _ => throw <| IO.userError s!"{kind}/{seed}: no samples"
        let image := s!"{kind}-{seed}.png"
        let abs := (← IO.FS.realPath outDir) / image
        let _ ← s.screenshot (outputPath := some abs.toString)
        let theme := (j.getObjValD "meta").getObjValD "theme"
        let samples := (j.getObjValD "samples").getArr?.toOption.getD #[]
        let mut i := 0
        for smp in samples do
          let base := match smp with | .obj kvs => kvs.toList | _ => []
          let line := Json.mkObj ([
            ("id", Json.str s!"{kind}-{seed}-{i}"), ("image", Json.str image),
            ("kind", Json.str kind), ("theme", theme)] ++ base)
          h.putStrLn line.compress
          i := i + 1
        n := n + samples.size
        IO.println s!"  {image}: {samples.size} samples"
    s.close
  catch e =>
    s.close
    throw e
  h.flush
  IO.println s!"wrote {n} samples → {labelsPath}"
  return 0

end VisionBench.Gen

def main (argv : List String) : IO UInt32 := VisionBench.Gen.main argv
