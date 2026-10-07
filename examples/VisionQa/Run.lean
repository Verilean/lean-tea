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
      [--evidence-dir DIR] [--cache DIR]
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
  | p :: r, a => parseArgs r (if a.script?.isNone then { a with script? := some p } else a)
  | [], a => a

structure Ctx where
  drv    : Driver
  vlm    : Vlm
  zoom   : Nat
  dir    : System.FilePath
  script : Script
  /-- Points clicked by `click_described` this step, to cache on success. -/
  pending : IO.Ref (Option (String × Nat × Nat × String))

def Ctx.shot (c : Ctx) (idx : Nat) (tag : String) : IO (System.FilePath × Image) := do
  let p := c.dir / s!"{c.script.name}.step{idx + 1}.{tag}.png"
  c.drv.screenshot p
  return (p, ← Image.load p)

def parseHex (s : String) : Option (Nat × Nat × Nat) := do
  let h := (s.dropWhile (· == '#')).toString
  guard (h.length == 6)
  let hexv (c : Char) : Option Nat :=
    if c.isDigit then some (c.toNat - '0'.toNat)
    else if 'a' ≤ c.toLower && c.toLower ≤ 'f' then some (c.toLower.toNat - 'a'.toNat + 10) else none
  let ds ← h.toList.mapM hexv
  match ds with
  | [a, b, c, d, e, f] => some (a * 16 + b, c * 16 + d, e * 16 + f)
  | _ => none

def num? (j : Json) (k : String) : Option Nat :=
  match j.getObjValD k with | .num n => some n.mantissa.toNat | _ => none

/-- Run one action; returns (observation for the audit log, evidence path). -/
def runAction (c : Ctx) (idx : Nat) : Action → IO (Option String × Option String)
  | .clickXy x y => do c.drv.click x y; return (none, none)
  | .clickKnown k => do
    let v ← LeanTea.Agent.Memory.recall k
    let (some x, some y) := (num? v "x", num? v "y")
      | throw <| IO.userError s!"click_known: no x/y cached for {k}"
    c.drv.click x y
    return (some s!"({x}, {y})", none)
  | .wait ms => do IO.sleep ms.toUInt32; return (none, none)
  | .screenshot saveAs => do
    let p : System.FilePath := saveAs.getD (c.dir / s!"{c.script.name}.step{idx + 1}.png").toString
    c.drv.screenshot p
    return (none, some p.toString)
  | .key k => do c.drv.key k; return (none, none)
  | .typeText t => do c.drv.typeText t; return (none, none)
  | .toolCall t .. => throw <| IO.userError s!"tool_call {t}: not supported by vision_qa (use ui_script)"
  | .clickDescribed target key? => do
    if let some k := key? then
      let v ← LeanTea.Agent.Memory.recall k
      if let (some x, some y) := (num? v "x", num? v "y") then
        c.drv.click x y
        return (some s!"cached {k} → ({x}, {y})", none)
    let (p, img) ← c.shot idx "before"
    let r ← locateZoom c.vlm img target c.zoom
    let some rect := r.rect?
      | throw <| IO.userError s!"click_described: could not locate {target}: {r.answer}"
    let x := rect.cx.round.toUInt64.toNat
    let y := rect.cy.round.toUInt64.toNat
    c.drv.click x y
    if let some k := key? then c.pending.set (some (k, x, y, target))
    return (some s!"({x}, {y}) in {r.ms}ms", some p.toString)
  | .assertVisual q want => do
    let (p, img) ← c.shot idx "assert"
    let v ← ask c.vlm img q
    if v.yes? != some want then
      throw <| IO.userError s!"assert_visual: wanted {if want then "yes" else "no"}, VLM said: {v.answer}"
    return (some (if want then "yes" else "no"), some p.toString)
  | .assertText target eq => do
    let (p, img) ← c.shot idx "assert"
    let r ← locateZoom c.vlm img target c.zoom
    let some rect := r.rect?
      | throw <| IO.userError s!"assert_text: could not locate {target}: {r.answer}"
    let got := (← readText c.vlm img rect).text
    if got != eq then
      throw <| IO.userError s!"assert_text: {target} reads \"{got}\", wanted \"{eq}\""
    return (some got, some p.toString)
  | .assertPixel x y rgb tol => do
    let (p, img) ← c.shot idx "assert"
    let some (er, eg, eb) := parseHex rgb
      | throw <| IO.userError s!"assert_pixel: bad colour {rgb}"
    let (r, g, b, _) := img.pixel x y
    let d (a : UInt8) (e : Nat) : Nat := if a.toNat > e then a.toNat - e else e - a.toNat
    let got := s!"rgb({r},{g},{b})"
    if d r er > tol || d g eg > tol || d b eb > tol then
      throw <| IO.userError s!"assert_pixel: ({x}, {y}) is {got}, wanted {rgb} ±{tol}"
    return (some got, some p.toString)
  | .waitForScreen screen timeoutMs => do
    let t0 ← IO.monoMsNow
    let mut last := "unknown"
    repeat
      let (p, img) ← c.shot idx "wait"
      last ← classify c.vlm img [screen]
      if last == screen then return (some last, some p.toString)
      if (← IO.monoMsNow) - t0 > timeoutMs then break
      IO.sleep 1000
    throw <| IO.userError s!"wait_for_screen: timed out waiting for {screen} (last: {last})"

def isAssertion : Action → Bool
  | .assertVisual .. | .assertText .. | .assertPixel .. | .screenshot .. | .waitForScreen .. => true
  | _ => false

def runStep (c : Ctx) (idx : Nat) (step : Step) : IO StepResult := do
  let t0 ← IO.monoMsNow
  c.pending.set none
  try
    let (obs, ev) ← runAction c idx step.act
    -- Let the UI settle, then capture the post-action state as evidence.
    let ev ← if isAssertion step.act then pure ev else do
      IO.sleep 300
      let (p, img) ← c.shot idx "after"
      if let some expected := step.expect then
        let got ← classify c.vlm img [expected]
        if got != expected then
          throw <| IO.userError s!"expect mismatch: wanted {expected}, classifier said {got}"
      pure (some p.toString)
    if let some (k, x, y, target) ← c.pending.get then
      LeanTea.Agent.Memory.remember k (Json.mkObj [
        ("x", .num (Int.ofNat x)), ("y", .num (Int.ofNat y)),
        ("target", .str target), ("source", .str s!"vision_qa/{c.vlm.model}")])
    return { step, ok := true, observed := obs, evidencePath := ev,
             durationMs := (← IO.monoMsNow) - t0 }
  catch e =>
    return { step, ok := false, error := some (toString e),
             durationMs := (← IO.monoMsNow) - t0 }

def runScript (c : Ctx) : IO ScriptResult := do
  let t0 ← IO.monoMsNow
  let mut results := #[]
  let mut passed := true
  for (step, idx) in c.script.steps.zipIdx do
    let r ← runStep c idx step
    results := results.push r
    IO.eprintln s!"  [{idx + 1}/{c.script.steps.length}] {if r.ok then "✓" else "✗"} {r.observed.getD ""}{r.error.getD ""}"
    if !r.ok then
      passed := false
      LeanTea.Agent.Memory.escalate s!"vision_qa: {c.script.name} step {idx + 1} failed: {r.error.getD ""}"
        (Json.mkObj [("script", .str c.script.name), ("step", .num (Int.ofNat (idx + 1))),
                     ("evidence", .str (r.evidencePath.getD ""))])
      break
  let totalMs := (← IO.monoMsNow) - t0
  let skipped := c.script.steps.length - results.size
  let res : ScriptResult := { script := c.script, steps := results, passed, totalMs, skipped }
  let runs := (← LeanTea.Agent.Memory.agentDir) / "runs"
  IO.FS.createDirAll runs
  let path := runs / s!"{c.script.name}.{← IO.monoMsNow}.json"
  IO.FS.writeFile path (res.toJson.pretty 2)
  return { res with reportPath := some path.toString }

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
    let _ ← s.open 1280 800 (headless := some !a.headed)
    pure (some s)
  let drv ← match a.target, session? with
    | "desktop", _ => Driver.desktop
    | "tmux", some s => pure (Driver.tmux a.tmuxTarget s dir)
    | _, some s => pure (Driver.browser s)
    | t, none => throw <| IO.userError s!"unknown target {t}"
  if let (some url, some s) := (a.url?, session?) then
    if a.target == "browser" then let _ ← s.navigate url
  IO.eprintln s!"vision_qa: {script.name} ({script.steps.length} steps) target={drv.name} model={vlm.model} frame={vlm.frame.name}"
  let c : Ctx := { drv, vlm, zoom := a.zoom, dir, script, pending := ← IO.mkRef none }
  let res ← try runScript c finally if let some s := session? then s.close
  IO.eprintln ""
  IO.eprintln res.renderTree
  return if res.passed then 0 else 3

end VisionQa

def main (argv : List String) : IO UInt32 := VisionQa.main argv
