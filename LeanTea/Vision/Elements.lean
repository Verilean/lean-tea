import LeanTea.Vision.Ground
import LeanTea.Vision.Ocr

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
  /-- `button` (the model says it can be pressed) or `text` (only OCR
      found it: a label, or a button the model missed). -/
  kind  : String := "button"
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

/-- Top to bottom (in 40 px rows), then left to right. -/
def readingOrder (es : List Element) : List Element :=
  (es.toArray.qsort fun a b =>
    let ra := (a.rect.cy / 40).floor
    let rb := (b.rect.cy / 40).floor
    ra < rb || (ra == rb && a.rect.cx < b.rect.cx)).toList

/-- OCR lines as elements of kind `text`. -/
def ofOcr (lines : List Ocr.Line) : List Element :=
  readingOrder (lines.map fun l => { label := l.text, rect := l.rect, kind := "text" })

/-- The model's buttons, then the OCR text no button covers (labels, and
    buttons the model missed). A button keeps the model's label: it
    reads a stylised label better in context than OCR does. -/
def merge (buttons : List Element) (lines : List Ocr.Line) : List Element :=
  let covered := fun (l : Ocr.Line) => buttons.any fun b =>
    b.rect.contains l.rect.cx l.rect.cy || b.rect.iou l.rect > 0.3
  readingOrder buttons ++ ofOcr (lines.filter (!covered ·))

/-- An `n`×`n` grid over a `w`×`h` screen, tiles overlapping by about
    `pad` px so an element on a seam is whole in one of them, row by row:
    `(x, y, w, h)`. -/
def tiles (w h : Nat) (n : Nat := 2) (pad : Nat := 60) : List (Nat × Nat × Nat × Nat) :=
  let n := max 1 n
  let tw := min w (w / n + pad)
  let th := min h (h / n + pad)
  let at_ := fun (i span t : Nat) => if n == 1 then 0 else i * (span - t) / (n - 1)
  (List.range n).flatMap fun j => (List.range n).map fun i => (at_ i w tw, at_ j h th, tw, th)

/-- Read the screen in an `grid`×`grid` set of tiles. On the whole screen
    a 4B model fills its list with whatever repeats most (a column of
    times, a row of dates) and misses the menus; the smaller the tile, the
    more it lists (a game home screen with 26 things to press: whole 1,
    2×2 16, 3×3 23). Also returns the raw answers (to show when nothing
    could be read from them). -/
def list (v : Vlm) (img : Image) (grid : Nat := 2) (maxItems : Nat := 30) (stop : IO Bool := pure false) :
    IO (List Element × String) := do
  let v := { v with maxTokens := maxItems * 28 }
  let mut out : Array Element := #[]
  let mut answers : Array String := #[]
  for (x, y, w, h) in tiles img.width img.height grid (if grid ≥ 3 then 40 else 60) do
    if ← stop then break
    let tile := img.crop x y w h
    let a ← v.call prompt (← tile.dataUrl)
    answers := answers.push a.text
    for e in parse a.text v.frame w h do
      let e := { e with rect := { e.rect with x := e.rect.x + x.toFloat, y := e.rect.y + y.toFloat } }
      if out.any (·.same e) then continue
      out := out.push { e with luma := meanLuma img e.rect }
  return (readingOrder out.toList, String.intercalate "\n" answers.toList)

end LeanTea.Vision.Elements
