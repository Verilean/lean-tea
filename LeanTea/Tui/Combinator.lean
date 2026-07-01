import LeanTea.Tui.Core

/-! # LeanTea.Tui.Combinator — vbox / hbox / border / padding / withStyle

Layout combinators that take widgets and produce widgets. Each
combinator's `render` lays out children inside the allocated
`width × height`. Each combinator's `onKey` is dispatched by the
app — combinators themselves don't consume keys (children do).

`vbox` and `hbox` allocate space:

  * Children with a `prefHeight` (vbox) or `prefWidth` (hbox) get
    exactly that much (clamped to remaining space).
  * Whatever's left is split evenly among unconstrained children.

This is not flex-grow / flex-shrink — too much machinery for a TUI
of this scale. Use `prefHeight` for headers/footers and let the
single "content area" eat the remainder.
-/

namespace LeanTea.Tui

/-! ## vbox -/

/-- Allocate space (height for vbox, width for hbox) to children.
    Children with a `Some n` pref get exactly that much (clamped to
    remaining); the rest is split evenly among `None` children. -/
private def splitV (total : Nat) (prefs : List (Option Nat)) : List Nat := Id.run do
  let mut allocated : Array Nat := #[]
  let mut used : Nat := 0
  let mut unspecified : Nat := 0
  for p in prefs do
    match p with
    | some n =>
      let take := if used + n > total then total - used else n
      allocated := allocated.push take
      used := used + take
    | none =>
      allocated := allocated.push 0
      unspecified := unspecified + 1
  let remaining := if used > total then 0 else total - used
  let each := if unspecified > 0 then remaining / unspecified else 0
  let mut extra := if unspecified > 0 then remaining % unspecified else 0
  let mut out : Array Nat := #[]
  let mut i : Nat := 0
  for p in prefs do
    match p with
    | some _ =>
      out := out.push allocated[i]!
    | none =>
      let bonus := if extra > 0 then 1 else 0
      if extra > 0 then extra := extra - 1
      out := out.push (each + bonus)
    i := i + 1
  return out.toList

/-- Pad / crop a Box to exactly (`width × height`). Used by the box
    layouts to enforce that each child occupies its allocation even
    when the child's own renderer ignores the size hint. -/
private def fitBox (w h : Nat) (b : Box) : Box := Id.run do
  -- Truncate / pad height
  let mut rows := b.cells
  if rows.size > h then rows := rows.extract 0 h
  while rows.size < h do
    rows := rows.push (Array.replicate b.width ({} : Cell))
  -- Truncate / pad each row's width
  let mut fixed : Array (Array Cell) := #[]
  for row in rows do
    let mut r := row
    if r.size > w then r := r.extract 0 w
    while r.size < w do r := r.push {}
    fixed := fixed.push r
  return { width := w, height := h, cells := fixed }

def vbox (children : List (Widget m)) : Widget m := {
  render := fun w h _focused => Id.run do
    let heights := splitV h (children.map (·.prefHeight))
    let mut acc : Box := Box.empty w 0
    let mut idx := 0
    for child in children do
      let ch := heights[idx]!
      if ch > 0 then
        let raw := child.render w ch false
        let fitted := fitBox w ch raw
        acc := Box.vcat acc fitted
      idx := idx + 1
    if acc.height < h then
      acc := Box.vcat acc (Box.empty w (h - acc.height))
    return acc,
  onKey := fun _ => none,
  children := children
}

/-! ## hbox -/

def hbox (children : List (Widget m)) : Widget m := {
  render := fun w h _focused => Id.run do
    let widths := splitV w (children.map (·.prefWidth))
    let mut acc : Box := Box.empty 0 h
    let mut idx := 0
    for child in children do
      let cw := widths[idx]!
      if cw > 0 then
        let raw := child.render cw h false
        let fitted := fitBox cw h raw
        acc := Box.hcat acc fitted
      idx := idx + 1
    if acc.width < w then
      acc := Box.hcat acc (Box.empty (w - acc.width) h)
    return acc,
  onKey := fun _ => none,
  children := children
}

/-! ## padding -/

def padding (p : Nat) (child : Widget m) : Widget m := {
  render := fun w h focused => Id.run do
    if w < 2*p || h < 2*p then
      return Box.empty w h
    else
      let innerW := w - 2*p
      let innerH := h - 2*p
      let inner := child.render innerW innerH focused
      let frame := Box.empty w h
      return Box.overlay frame inner p p,
  onKey := child.onKey,
  focusId := child.focusId,
  children := [child]
}

/-! ## border -/

inductive BorderStyle where
  | none | line | doubleLine | dashed
  deriving BEq

private def borderChars (s : BorderStyle) : Char × Char × Char × Char × Char × Char :=
  match s with
  | .none       => (' ', ' ', ' ', ' ', ' ', ' ')
  | .line       => ('┌', '┐', '└', '┘', '─', '│')
  | .doubleLine => ('╔', '╗', '╚', '╝', '═', '║')
  | .dashed     => ('+', '+', '+', '+', '-', '|')

def border (style : BorderStyle := .line) (child : Widget m) : Widget m := {
  render := fun w h focused => Id.run do
    if w < 2 || h < 2 then
      return child.render w h focused
    let (tl, tr, bl, br, hChar, vChar) := borderChars style
    let lineStyle : Style := if focused then { fg := .cyan, bold := true } else {}
    -- Top row: tl + hChar*(w-2) + tr
    let topRow := #[{ ch := tl, style := lineStyle : Cell }]
                  ++ Array.replicate (w - 2) ({ ch := hChar, style := lineStyle } : Cell)
                  ++ #[{ ch := tr, style := lineStyle : Cell }]
    let botRow := #[{ ch := bl, style := lineStyle : Cell }]
                  ++ Array.replicate (w - 2) ({ ch := hChar, style := lineStyle } : Cell)
                  ++ #[{ ch := br, style := lineStyle : Cell }]
    -- Middle rows: vChar + (w-2) cells + vChar (cells from child)
    let inner := child.render (w - 2) (h - 2) focused
    let mut rows : Array (Array Cell) := #[topRow]
    for r in [:h - 2] do
      let innerRow := if r < inner.height then inner.cells[r]! else Array.replicate (w - 2) ({} : Cell)
      let row := #[{ ch := vChar, style := lineStyle : Cell }]
                 ++ innerRow
                 ++ #[{ ch := vChar, style := lineStyle : Cell }]
      rows := rows.push row
    rows := rows.push botRow
    return { width := w, height := h, cells := rows },
  onKey := child.onKey,
  focusId := child.focusId,
  children := [child]
}

/-! ## withStyle — paint every cell the child produces -/

def withStyle (s : Style) (child : Widget m) : Widget m := {
  render := fun w h focused =>
    let inner := child.render w h focused
    let cells := inner.cells.map (fun row => row.map (fun c => { c with style := s }))
    { inner with cells := cells },
  onKey := child.onKey,
  focusId := child.focusId,
  children := [child]
}

/-! ## text — non-interactive label (1 row) -/

def text (s : String) (style : Style := {}) : Widget m := {
  render := fun w _h _focused => Box.text w s style,
  prefHeight := some 1
}

/-! ## blank — empty filler -/

def blank : Widget m := {
  render := fun w h _ => Box.empty w h
}

end LeanTea.Tui
