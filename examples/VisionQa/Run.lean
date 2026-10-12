import LeanTea

/-! # vision_qa — run a UI script with a local VLM as the eyes

Same JSON script format as `ui_script` (see `LeanTea.Agent.Script`),
plus the vision actions (`click_described`, `assert_visual`,
`assert_text`, `assert_pixel`, `key`, `type`). Runs against any
`LeanTea.Vision.Driver`:

```
$ vision_qa script.json --target browser --url http://127.0.0.1:8003/ [--headed]
$ vision_qa script.json --target desktop                # LEANTEA_DESKTOP=1 build
$ vision_qa script.json --target tmux --tmux-target qa:0.0
      [--model qwen/qwen3-vl-4b] [--frame norm1000] [--zoom 3]
      [--evidence-dir DIR] [--cache DIR] [--profile DIR] [--clip auto|SELECTOR|x,y,w,h]
```

Each step's screenshot is kept as evidence; the run manifest lands in
`~/.cache/leantea-agent/runs/` so `ui_report` renders it. Exit 3 on
the first failing step (also appended to `escalations.jsonl`).

`click_described` with a `key` caches the clicked point in
`ui-map.json` once the step passes, so the next run clicks it
directly — the VLM is only consulted again if the key is removed. -/

namespace VisionQa

open Lean (Json)
open LeanTea.Agent.Script
open LeanTea.Vision
open LeanTea.Vision.Qa

structure Args where
  script?     : Option String := none
  target      : String := "browser"
  url?        : Option String := none
  tmuxTarget  : String := ""
  headed      : Bool := false
  model       : String := "qwen/qwen3-vl-4b"
  frame?      : Option CoordFrame := none
  zoom        : Nat := 3
  evidenceDir : String := "/tmp/vision-qa"
  cache?      : Option String := none
  baseUrl     : String := "http://127.0.0.1:11211/v1"
  profile?    : Option String := none
  clip?       : Option String := none

partial def parseArgs : List String → Args → Args
  | "--target" :: v :: r, a       => parseArgs r { a with target := v }
  | "--url" :: v :: r, a          => parseArgs r { a with url? := some v }
  | "--tmux-target" :: v :: r, a  => parseArgs r { a with tmuxTarget := v }
  | "--headed" :: r, a            => parseArgs r { a with headed := true }
  | "--model" :: v :: r, a        => parseArgs r { a with model := v }
  | "--frame" :: v :: r, a        => parseArgs r { a with frame? := CoordFrame.ofName? v }
  | "--zoom" :: v :: r, a         => parseArgs r { a with zoom := v.toNat! }
  | "--evidence-dir" :: v :: r, a => parseArgs r { a with evidenceDir := v }
  | "--cache" :: v :: r, a        => parseArgs r { a with cache? := some v }
  | "--base-url" :: v :: r, a     => parseArgs r { a with baseUrl := v }
  | "--profile" :: v :: r, a      => parseArgs r { a with profile? := some v }
  | "--clip" :: v :: r, a         => parseArgs r { a with clip? := some v }
  | p :: r, a => parseArgs r (if a.script?.isNone then { a with script? := some p } else a)
  | [], a => a

def main (argv : List String) : IO UInt32 := do
  let a := parseArgs argv {}
  let some path := a.script?
    | IO.eprintln "usage: vision_qa <script.json> --target browser|desktop|tmux [--url U] [--tmux-target T] [--model M]"
      return 2
  let script ← IO.ofExcept (Script.fromJson (← IO.ofExcept (Json.parse (← IO.FS.readFile path))))
  let dir : System.FilePath := a.evidenceDir
  IO.FS.createDirAll dir
  let vlm : Vlm := { cfg := { baseUrl := a.baseUrl, timeoutSec := some 300 }, model := a.model,
                     frame := a.frame?.getD (CoordFrame.guess a.model),
                     cacheDir? := a.cache?.map (·) }
  let session? ← if a.target == "desktop" then pure none else do
    let s ← LeanTea.Browser.Session.spawn
    let _ ← s.open 1280 800 (headless := some !a.headed) (profile := a.profile?)
    pure (some s)
  let drv ← match a.target, session? with
    | "desktop", _ => Driver.desktop
    | "tmux", some s => pure (Driver.tmux a.tmuxTarget s dir)
    | _, some s => match a.clip? with
      | some spec => (Driver.browser s).clipTo (browserRegion s spec)
      | none => pure (Driver.browser s)
    | t, none => throw <| IO.userError s!"unknown target {t}"
  if let (some url, some s) := (a.url?, session?) then
    if a.target == "browser" then let _ ← s.navigate url
  IO.eprintln s!"vision_qa: {script.name} ({script.steps.length} steps) target={drv.name} model={vlm.model} frame={vlm.frame.name}"
  let c : Ctx := { drv, vlm, zoom := a.zoom, dir, script, pending := ← IO.mkRef none,
                   lastPoint := ← IO.mkRef none }
  let res ← try
      runScript c fun idx r =>
        IO.eprintln s!"  [{idx + 1}/{script.steps.length}] {if r.ok then "✓" else "✗"} {r.observed.getD ""}{r.error.getD ""}"
    finally if let some s := session? then s.close
  IO.eprintln ""
  IO.eprintln res.renderTree
  return if res.passed then 0 else 3

end VisionQa

def main (argv : List String) : IO UInt32 := VisionQa.main argv
