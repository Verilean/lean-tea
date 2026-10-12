import LeanTea.Agent.Script
import LeanTea.Agent.Memory
import LeanTea.Vision.Ground
import LeanTea.Vision.Driver

/-! # LeanTea.Vision.Qa — run a UI script with a local VLM as the eyes

The execution kernel behind `vision_qa` (CLI) and `qa_studio` (chat
UI): every `LeanTea.Agent.Script` action, including the vision ones
(`click_described`, `assert_visual`, `assert_text`, `assert_pixel`,
`key`, `type`), against any `LeanTea.Vision.Driver`. Each step keeps
its screenshot as evidence; `runScript` writes the run manifest to
`~/.cache/leantea-agent/runs/` and escalates the first failure. -/

namespace LeanTea.Vision.Qa

open Lean (Json)
open LeanTea.Agent.Script
open LeanTea.Vision

structure Ctx where
  drv    : Driver
  vlm    : Vlm
  zoom   : Nat
  dir    : System.FilePath
  script : Script
  /-- Points clicked by `click_described` this step, to cache on success. -/
  pending : IO.Ref (Option (String × Nat × Nat × String))
  /-- Where the last click landed (for UIs that mark it on the screenshot). -/
  lastPoint : IO.Ref (Option (Nat × Nat))

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
  | .clickXy x y => do c.drv.click x y; c.lastPoint.set (some (x, y)); return (none, none)
  | .clickKnown k => do
    let v ← LeanTea.Agent.Memory.recall k
    let (some x, some y) := (num? v "x", num? v "y")
      | throw <| IO.userError s!"click_known: no x/y cached for {k}"
    c.drv.click x y
    c.lastPoint.set (some (x, y))
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
        c.lastPoint.set (some (x, y))
        return (some s!"cached {k} → ({x}, {y})", none)
    -- up to 3 looks, 600 ms apart: blinking prompts ("TAP TO START") and
    -- brief transitions can hide the target in a single screenshot
    let mut found : Option (System.FilePath × Rect × Located) := none
    let mut lastAnswer := ""
    for k in [0:3] do
      if k > 0 then IO.sleep 600
      let (p, img) ← c.shot idx "before"
      let r ← locateZoom c.vlm img target c.zoom
      lastAnswer := r.answer
      if let some rect := r.rect? then
        found := some (p, rect, r)
        break
    let some (p, rect, r) := found
      | throw <| IO.userError s!"click_described: could not locate {target}: {lastAnswer}"
    let x := rect.cx.round.toUInt64.toNat
    let y := rect.cy.round.toUInt64.toNat
    c.drv.click x y
    c.lastPoint.set (some (x, y))
    if let some k := key? then c.pending.set (some (k, x, y, target))
    return (some s!"({x}, {y}) in {r.ms}ms", some p.toString)
  | .assertVisual q want => do
    -- a "yes" check gets 3 looks 600 ms apart (blinking / settling
    -- screens); a "no" check must hold on the first look
    let tries := if want then 3 else 1
    let mut last : Option (System.FilePath × Verdict) := none
    for k in [0:tries] do
      if k > 0 then IO.sleep 600
      let (p, img) ← c.shot idx "assert"
      let v ← ask c.vlm img q
      last := some (p, v)
      if v.yes? == some want then break
    let some (p, v) := last | throw <| IO.userError "assert_visual: no screenshot"
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
  | .waitUntil q timeoutMs => do
    let t0 ← IO.monoMsNow
    let mut last := ""
    repeat
      let (p, img) ← c.shot idx "wait"
      let v ← ask c.vlm img q
      last := v.answer
      if v.yes? == some true then return (some s!"yes after {(← IO.monoMsNow) - t0}ms", some p.toString)
      if (← IO.monoMsNow) - t0 > timeoutMs then break
      IO.sleep 1000
    throw <| IO.userError s!"wait_until: still no after {timeoutMs}ms — {q} (last answer: {last})"
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

/-- How many sampled pixels (every 2nd in each direction) differ between
    two screenshots; the sample count if the sizes differ. Small on
    purpose: six password dots or a caret must still register. -/
def changedPixels (a b : Image) : Nat := Id.run do
  if a.width != b.width || a.height != b.height then return a.width * a.height / 4
  let mut diff := 0
  for y in [0:a.height:2] do
    for x in [0:a.width:2] do
      if a.pixel x y != b.pixel x y then diff := diff + 1
  return diff

/-- `changedPixels` restricted to a window (clamped to the image). -/
def changedPixelsIn (a b : Image) (x0 y0 w h : Nat) : Nat := Id.run do
  if a.width != b.width || a.height != b.height then return w * h / 4
  let mut diff := 0
  for y in [y0:min (y0 + h) a.height:2] do
    for x in [x0:min (x0 + w) a.width:2] do
      if a.pixel x y != b.pixel x y then diff := diff + 1
  return diff

/-- Restrict a driver to a region of its screen: screenshots are cropped
    to it and clicks are offset into it, so the model only ever sees
    (and points into) e.g. the game's canvas. `region` is asked on every
    screenshot, so a layout that moves is followed; `none` = whole screen. -/
def _root_.LeanTea.Vision.Driver.clipTo (d : Driver) (region : IO (Option (Nat × Nat × Nat × Nat))) : IO Driver := do
  let origin ← IO.mkRef ((0, 0) : Nat × Nat)
  return { d with
    name := d.name ++ "+clip"
    screenshot := fun p => do
      d.screenshot p
      match ← region with
      | some (x, y, w, h) =>
        if w > 0 && h > 0 then
          let img ← Image.load p
          (img.crop x y w h).save p
          origin.set (x, y)
        else origin.set (0, 0)
      | none => origin.set (0, 0)
    click := fun x y => do
      let (ox, oy) ← origin.get
      d.click (ox + x) (oy + y) }

/-- Resolve a `--clip` spec in a browser page:
    `"x,y,w,h"` (fixed), `"auto"` (the largest visible iframe / canvas /
    embed — a browser game's frame), or a CSS selector. Asked per
    screenshot; `none` when nothing matches (whole page). -/
def browserRegion (s : LeanTea.Browser.Session) (spec : String) : IO (Option (Nat × Nat × Nat × Nat)) := do
  match spec.splitOn "," |>.map (·.trimAscii.toString.toNat?) with
  | [some x, some y, some w, some h] => return some (x, y, w, h)
  | _ =>
    let sel := if spec == "auto" then "" else spec.replace "\\" "\\\\" |>.replace "'" "\\'"
    let js := "(() => { const vw = innerWidth, vh = innerHeight; let best = null, area = 0;" ++
      s!" const els = '{sel}' ? [document.querySelector('{sel}')] : document.querySelectorAll('iframe,canvas,embed,object');" ++
      " for (const e of els) { if (!e) continue; const r = e.getBoundingClientRect();" ++
      " const x = Math.max(0, r.left), y = Math.max(0, r.top), w = Math.min(vw, r.right) - x, h = Math.min(vh, r.bottom) - y;" ++
      " if (w > 0 && h > 0 && w * h > area) { area = w * h; best = [x, y, w, h].map(Math.round); } }" ++
      " return best; })()"
    match ← s.evaluate js with
    | .arr #[x, y, w, h] =>
      match x.getNat?, y.getNat?, w.getNat?, h.getNat? with
      | .ok x, .ok y, .ok w, .ok h => return some (x, y, w, h)
      | _, _, _, _ => return none
    | _ => return none

/-- Share of sampled pixels that changed between two screenshots taken
    `gapMs` apart, repeated until it falls below `quiet` or `maxMs`
    passes. Returns the last ratio: ≥ `quiet` means the page never
    settled (an animated game screen). -/
def settle (c : Ctx) (maxMs : Nat := 5000) (quiet : Float := 0.01) (gapMs : Nat := 350) : IO Float := do
  let t0 ← IO.monoMsNow
  let a := c.dir / "settle.a.png"
  let b := c.dir / "settle.b.png"
  c.drv.screenshot a
  let mut last := 1.0
  repeat
    IO.sleep gapMs.toUInt32
    c.drv.screenshot b
    let (ia, ib) := (← Image.load a, ← Image.load b)
    let samples := max 1 ((ib.width / 2) * (ib.height / 2))
    last := (changedPixels ia ib).toFloat / samples.toFloat
    if last < quiet || (← IO.monoMsNow) - t0 ≥ maxMs then break
    IO.FS.writeBinFile a (← IO.FS.readBinFile b)
  return last

def isAssertion : Action → Bool
  | .assertVisual .. | .assertText .. | .assertPixel .. | .screenshot .. | .waitForScreen .. | .waitUntil .. => true
  | _ => false

def runStep (c : Ctx) (idx : Nat) (step : Step) : IO StepResult := do
  let t0 ← IO.monoMsNow
  c.pending.set none
  try
    let (obs, ev) ← runAction c idx step.act
    -- Let the UI settle, then capture the post-action state as evidence.
    let ev ← if isAssertion step.act then pure ev else do
      IO.sleep 150
      -- wait for transitions / loading to finish (bounded: animated
      -- screens never go fully quiet)
      let _ ← settle c
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

/-- Run every step, stopping at the first failure. `onStep` sees each
    result as it lands (the CLI prints it, `qa_studio` streams it). -/
def runScript (c : Ctx) (onStep : Nat → StepResult → IO Unit := fun _ _ => pure ()) : IO ScriptResult := do
  let t0 ← IO.monoMsNow
  let mut results := #[]
  let mut passed := true
  for (step, idx) in c.script.steps.zipIdx do
    let r ← runStep c idx step
    results := results.push r
    onStep idx r
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

end LeanTea.Vision.Qa
