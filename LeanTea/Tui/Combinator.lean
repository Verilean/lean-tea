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

Every combinator also fills in the two hooks the app needs beyond
drawing: `place` (where each child sits, for routing mouse events to
the widget under the pointer) and `renderIn` (render with a focus id,
so the focused widget is highlighted however deeply it is nested).

`hsplit` / `vsplit` draw a divider the user can drag, and `scroll`
is a viewport with wheel + scrollbar support. Both keep their
position in the app's state and report changes as messages, like
every other widget here.
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

/-- Offsets of consecutive spans: `[3, 2, 4]` → `[0, 3, 5]`. -/
private def offsets (sizes : List Nat) : List Nat :=
  (sizes.foldl (fun (acc, pos) n => (acc.push pos, pos + n)) ((#[] : Array Nat), 0)).1.toList

/-- Stack children top to bottom, rendering each with `draw`. -/
private def vboxDraw (children : List (Widget m)) (draw : Widget m → Nat → Nat → Box)
    (w h : Nat) : Box := Id.run do
  let heights := splitV h (children.map (·.prefHeight))
  let mut acc : Box := Box.empty w 0
  for (child, ch) in children.zip heights do
    if ch > 0 then
      acc := Box.vcat acc (fitBox w ch (draw child w ch))
  if acc.height < h then
    acc := Box.vcat acc (Box.empty w (h - acc.height))
  return acc

def vbox (children : List (Widget m)) : Widget m := {
  render := fun w h _focused => vboxDraw children (fun c w h => c.render w h false) w h,
  renderIn := some fun w h f => vboxDraw children (fun c w h => c.renderFocused w h f) w h,
  place := fun w h =>
    let heights := splitV h (children.map (·.prefHeight))
    (offsets heights).zip heights |>.map fun (r, ch) =>
      { row := r, col := 0, width := w, height := ch },
  onKey := fun _ => none,
  children := children
}

/-! ## hbox -/

private def hboxDraw (children : List (Widget m)) (draw : Widget m → Nat → Nat → Box)
    (w h : Nat) : Box := Id.run do
  let widths := splitV w (children.map (·.prefWidth))
  let mut acc : Box := Box.empty 0 h
  for (child, cw) in children.zip widths do
    if cw > 0 then
      acc := Box.hcat acc (fitBox cw h (draw child cw h))
  if acc.width < w then
    acc := Box.hcat acc (Box.empty (w - acc.width) h)
  return acc

def hbox (children : List (Widget m)) : Widget m := {
  render := fun w h _focused => hboxDraw children (fun c w h => c.render w h false) w h,
  renderIn := some fun w h f => hboxDraw children (fun c w h => c.renderFocused w h f) w h,
  place := fun w h =>
    let widths := splitV w (children.map (·.prefWidth))
    (offsets widths).zip widths |>.map fun (c, cw) =>
      { row := 0, col := c, width := cw, height := h },
  onKey := fun _ => none,
  children := children
}

/-! ## padding -/

private def paddingDraw (p : Nat) (draw : Nat → Nat → Box) (w h : Nat) : Box :=
  if w < 2*p || h < 2*p then Box.empty w h
  else Box.overlay (Box.empty w h) (draw (w - 2*p) (h - 2*p)) p p

def padding (p : Nat) (child : Widget m) : Widget m := {
  render := fun w h focused => paddingDraw p (fun w h => child.render w h focused) w h,
  renderIn := some fun w h f => paddingDraw p (fun w h => child.renderFocused w h f) w h,
  place := fun w h =>
    if w < 2*p || h < 2*p then [{ row := 0, col := 0, width := 0, height := 0 }]
    else [{ row := p, col := p, width := w - 2*p, height := h - 2*p }],
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

private def borderDraw (style : BorderStyle) (focused : Bool) (draw : Nat → Nat → Box)
    (w h : Nat) : Box := Id.run do
  if w < 2 || h < 2 then
    return draw w h
  let (tl, tr, bl, br, hChar, vChar) := borderChars style
  let lineStyle : Style := if focused then { fg := .cyan, bold := true } else {}
  let edge := fun (l r : Char) =>
    #[{ ch := l, style := lineStyle : Cell }]
      ++ Array.replicate (w - 2) ({ ch := hChar, style := lineStyle } : Cell)
      ++ #[{ ch := r, style := lineStyle : Cell }]
  let inner := draw (w - 2) (h - 2)
  let mut rows : Array (Array Cell) := #[edge tl tr]
  for r in [:h - 2] do
    let innerRow := if r < inner.height then inner.cells[r]! else Array.replicate (w - 2) ({} : Cell)
    rows := rows.push (#[{ ch := vChar, style := lineStyle : Cell }] ++ innerRow
                       ++ #[{ ch := vChar, style := lineStyle : Cell }])
  rows := rows.push (edge bl br)
  return { width := w, height := h, cells := rows }

def border (style : BorderStyle := .line) (child : Widget m) : Widget m := {
  render := fun w h focused => borderDraw style focused (fun w h => child.render w h focused) w h,
  renderIn := some fun w h f =>
    borderDraw style (child.focusId == f && !f.isEmpty) (fun w h => child.renderFocused w h f) w h,
  place := fun w h =>
    if w < 2 || h < 2 then [{ row := 0, col := 0, width := w, height := h }]
    else [{ row := 1, col := 1, width := w - 2, height := h - 2 }],
  onKey := child.onKey,
  focusId := child.focusId,
  children := [child]
}

/-! ## withStyle — paint every cell the child produces -/

private def restyle (s : Style) (b : Box) : Box :=
  { b with cells := b.cells.map (fun row => row.map (fun c => { c with style := s })) }

def withStyle (s : Style) (child : Widget m) : Widget m := {
  render := fun w h focused => restyle s (child.render w h focused),
  renderIn := some fun w h f => restyle s (child.renderFocused w h f),
  place := fun w h => [{ row := 0, col := 0, width := w, height := h }],
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

/-! ## hsplit / vsplit — two panes with a draggable divider

`ratio` is the first pane's share in per-mille (0–1000) and lives in
the app's state. Pressing the divider and dragging reports the new
ratio via `onResize`; the app stores it and the next frame uses it.
Each pane keeps at least one cell. -/

private def clampRatio (r : Int) : Nat := (max 0 (min 1000 r)).toNat

/-- First-pane size for a split of `total` cells (one goes to the divider). -/
private def splitAt (ratio total : Nat) : Nat :=
  if total < 3 then total / 2
  else max 1 (min (total - 2) (total * (min ratio 1000) / 1000))

private def hsplitDraw (ratio : Nat) (a b : Widget m) (draw : Widget m → Nat → Nat → Box)
    (w h : Nat) : Box :=
  let lw := splitAt ratio w
  let rw := w - lw - (if w ≥ 3 then 1 else 0)
  let bar := Box.filled (if w ≥ 3 then 1 else 0) h '│' { fg := .bright .black }
  Box.hcat (Box.hcat (fitBox lw h (draw a lw h)) bar) (fitBox rw h (draw b rw h))

def hsplit (ratio : Nat) (onResize : Nat → m) (a b : Widget m) : Widget m := {
  render := fun w h _ => hsplitDraw ratio a b (fun c w h => c.render w h false) w h,
  renderIn := some fun w h f => hsplitDraw ratio a b (fun c w h => c.renderFocused w h f) w h,
  place := fun w h =>
    let lw := splitAt ratio w
    [{ row := 0, col := 0, width := lw, height := h },
     { row := 0, col := lw + 1, width := w - lw - (if w ≥ 3 then 1 else 0), height := h }],
  onMouse := fun w _ ev =>
    let onBar := ev.col == splitAt ratio w
    match ev.kind, ev.button with
    | .press, .left => if onBar then some (onResize ratio) else none
    | .drag, .left  => if w == 0 then none else some (onResize (clampRatio ((ev.col * 1000 + w / 2) / w)))
    | _, _ => none,
  children := [a, b]
}

private def vsplitDraw (ratio : Nat) (a b : Widget m) (draw : Widget m → Nat → Nat → Box)
    (w h : Nat) : Box :=
  let th := splitAt ratio h
  let bh := h - th - (if h ≥ 3 then 1 else 0)
  let bar := Box.filled w (if h ≥ 3 then 1 else 0) '─' { fg := .bright .black }
  Box.vcat (Box.vcat (fitBox w th (draw a w th)) bar) (fitBox w bh (draw b w bh))

def vsplit (ratio : Nat) (onResize : Nat → m) (a b : Widget m) : Widget m := {
  render := fun w h _ => vsplitDraw ratio a b (fun c w h => c.render w h false) w h,
  renderIn := some fun w h f => vsplitDraw ratio a b (fun c w h => c.renderFocused w h f) w h,
  place := fun w h =>
    let th := splitAt ratio h
    [{ row := 0, col := 0, width := w, height := th },
     { row := th + 1, col := 0, width := w, height := h - th - (if h ≥ 3 then 1 else 0) }],
  onMouse := fun _ h ev =>
    let onBar := ev.row == splitAt ratio h
    match ev.kind, ev.button with
    | .press, .left => if onBar then some (onResize ratio) else none
    | .drag, .left  => if h == 0 then none else some (onResize (clampRatio ((ev.row * 1000 + h / 2) / h)))
    | _, _ => none,
  children := [a, b]
}

/-! ## scroll — a viewport onto a taller child

The child is rendered at `contentHeight` rows (default: its
`prefHeight`, else the viewport height) and rows `offset ..` are
shown, with a one-column scrollbar on the right. The wheel scrolls by
`wheelStep` rows and pressing / dragging the scrollbar jumps; every
change is reported as the new offset via `onScroll`. `offset` is
clamped, so the app can store whatever it gets. -/

private def maxOffset (content h : Nat) : Nat := content - h

private def scrollDraw (offset content : Nat) (child : Widget m) (draw : Widget m → Nat → Nat → Box)
    (w h : Nat) : Box := Id.run do
  if w < 2 then return Box.empty w h
  let cw := w - 1
  let total := max content h
  let off := min offset (maxOffset total h)
  let full := fitBox cw total (draw child cw total)
  let view : Box := { width := cw, height := h, cells := full.cells.extract off (off + h) }
  -- scrollbar thumb: size and position proportional to the visible part
  let thumb := max 1 (h * h / total)
  let top := if total == h then 0 else (h - thumb) * off / (total - h)
  let bar : Box := {
    width := 1, height := h,
    cells := (Array.range h).map fun r =>
      if top ≤ r && r < top + thumb then #[{ ch := '┃', style := { fg := .white } }]
      else #[{ ch := '│', style := { fg := .bright .black } }] }
  return Box.hcat view bar

def scroll (offset : Nat) (onScroll : Nat → m) (child : Widget m)
    (contentHeight : Option Nat := none) (wheelStep : Nat := 3) : Widget m :=
  let content := fun h => max h (contentHeight.getD (child.prefHeight.getD h))
  {
    render := fun w h _ => scrollDraw offset (content h) child (fun c w h => c.render w h false) w h,
    renderIn := some fun w h f =>
      scrollDraw offset (content h) child (fun c w h => c.renderFocused w h f) w h,
    place := fun w h =>
      let off := min offset (maxOffset (content h) h)
      [{ row := -(off : Int), col := 0, width := w - 1, height := content h }],
    onMouse := fun w h ev =>
      let maxOff := maxOffset (content h) h
      let off := min offset maxOff
      let jump := fun (row : Int) =>
        if h ≤ 1 then 0 else (max 0 (min (h - 1 : Int) row)).toNat * maxOff / (h - 1)
      match ev.kind with
      | .scrollUp   => if off == 0 then none else some (onScroll (off - min off wheelStep))
      | .scrollDown => if off == maxOff then none else some (onScroll (min maxOff (off + wheelStep)))
      | .press => if ev.col + 1 == w && ev.button == .left then some (onScroll (jump ev.row)) else none
      | .drag  => if ev.button == .left then some (onScroll (jump ev.row)) else none
      | _ => none,
    children := [child]
  }

end LeanTea.Tui
