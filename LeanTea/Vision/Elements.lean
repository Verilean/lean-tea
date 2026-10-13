import LeanTea.Vision.Ground

/-! # LeanTea.Vision.Elements — the screen as a list of things to click

A small VLM reads a screen well (where the buttons are, what they say)
and decides badly (what to click next, whether a goal is done). So the
studio asks it only to *read*: every button, tab and icon with its box,
and shows that list to the user, who picks what to click or check.

Qwen-VL tends to repeat the last line of a long list until it runs out
of tokens, so the parser drops duplicates and the token budget is kept
to about `maxItems` lines. -/

namespace LeanTea.Vision.Elements

open LeanTea.Vision

structure Element where
  label : String
  rect  : Rect
  /-- Mean brightness: an element behind a dialog is dimmed. -/
  luma  : Float := 0
  deriving Inhabited, Repr

def prompt : String :=
  "List the buttons, tabs, menu items and icons a player can press on this screen, each once. " ++
  "Do NOT list plain text, numbers, dates or decorations. Label each with the text written on it " ++
  "(in the screen's language), or a short description for an icon. " ++
  "Reply with ONLY a JSON array: [{\"label\": \"...\", \"bbox_2d\": [x1, y1, x2, y2]}, ...]."

/-- The `"label": "…"` value at the start of `part`, if any. -/
private def labelIn (part : String) : Option String := do
  let afterColon ← match part.splitOn ":" with
    | _ :: rest => some (String.intercalate ":" rest)
    | [] => none
  match afterColon.splitOn "\"" with
  | _ :: v :: _ => let v := v.trimAscii.toString; if v.isEmpty then none else some v
  | _ => none

/-- Elements in a model answer (possibly cut off mid-array), in screen
    pixels. Repeats — same label at about the same place — are dropped. -/
def parse (answer : String) (frame : CoordFrame) (w h : Nat) : List Element := Id.run do
  let parts := ((stripThink answer).splitOn "\"label\"").drop 1
  let mut out : Array Element := #[]
  for part in parts do
    let some label := labelIn part | continue
    let some after := (part.splitOn "bbox_2d")[1]? | continue
    let [a, b, c, d] := (scanNumbers after.toList).take 4 | continue
    let r := (RawCoords.box a b c d).toRect frame w h
    if r.w < 2 || r.h < 2 then continue
    if out.any (fun e => e.label == label && e.rect.iou r > 0.5) then continue
    out := out.push { label, rect := r }
  return out.toList

/-- The same element found twice (in two overlapping tiles). -/
def Element.same (a b : Element) : Bool :=
  a.label == b.label && (a.rect.iou b.rect > 0.3 || a.rect.contains b.rect.cx b.rect.cy)

/-- Quarters of a `w`×`h` screen, overlapping by `pad` px so an element
    on a seam is whole in one of them: `(x, y, w, h)`. -/
def tiles (w h : Nat) (pad : Nat := 60) : List (Nat × Nat × Nat × Nat) :=
  let tw := min w (w / 2 + pad)
  let th := min h (h / 2 + pad)
  [(0, 0), (w - tw, 0), (0, h - th), (w - tw, h - th)].map fun (x, y) => (x, y, tw, th)

/-- Read the screen, a quarter at a time: on the whole screen a 4B model
    fills its list with whatever repeats most (a column of times, a row
    of dates) and misses the menus; on a quarter it lists what can be
    pressed. Also returns the raw answers (to show when nothing could be
    read from them). -/
def list (v : Vlm) (img : Image) (maxItems : Nat := 30) : IO (List Element × String) := do
  let v := { v with maxTokens := maxItems * 28 }
  let mut out : Array Element := #[]
  let mut answers : Array String := #[]
  for (x, y, w, h) in tiles img.width img.height do
    let tile := img.crop x y w h
    let a ← v.call prompt (← tile.dataUrl)
    answers := answers.push a.text
    for e in parse a.text v.frame w h do
      let e := { e with rect := { e.rect with x := e.rect.x + x.toFloat, y := e.rect.y + y.toFloat } }
      if out.any (·.same e) then continue
      out := out.push { e with luma := meanLuma img e.rect }
  -- reading order: top to bottom, then left to right
  let rows := out.qsort fun a b =>
    let ra := (a.rect.cy / 40).floor
    let rb := (b.rect.cy / 40).floor
    ra < rb || (ra == rb && a.rect.cx < b.rect.cx)
  return (rows.toList, String.intercalate "\n" answers.toList)

end LeanTea.Vision.Elements
