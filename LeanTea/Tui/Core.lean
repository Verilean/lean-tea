/-! # LeanTea.Tui.Core — Box, Cell, Style, Key, Widget

A tiny brick-style TUI substrate. The whole rendering model is:

  * `Box` — a width × height grid of `Cell`s, where each `Cell` is a
    `Char` + a `Style`.
  * `Widget m` — a renderer `Nat → Nat → Bool → Box` (width, height,
    focused?) plus an `onKey` that translates a `Key` into a message
    of type `m` (or `none` to ignore).
  * Layout combinators (`vbox`, `hbox`, `border`, `padding`,
    `withStyle`) and elements (`button`, `input`, `panel`, `text`)
    live in sibling files.

Everything is pure — the IO side (read keystroke, write to terminal,
clear screen) lives only in `LeanTea.Tui.App`. That separation is
what lets the same widget tree be rendered and key-driven by
`LeanTea.Tui.Test` without a real TTY.

## Why not just hand-roll ANSI

We tried that in `examples/LlmChatTui` and ran into the same trap
the ChuHan `button` kit fixed: every UI element wires render +
event handling separately and you eventually forget one. With
widgets you compose `button("primary", .quit, "/quit")` and the
key handling, style, and focus highlight come along for free.
-/

namespace LeanTea.Tui

/-! ## Style — color + attribute bits -/

inductive Color where
  | default
  | black | red | green | yellow | blue | magenta | cyan | white
  | bright (c : Color)
  deriving Inhabited, BEq, Repr

structure Style where
  fg     : Color := .default
  bg     : Color := .default
  bold   : Bool := false
  dim    : Bool := false
  inverse : Bool := false
  deriving Inhabited, BEq, Repr

def Style.default : Style := {}

/-! ## Cell + Box

`Box.cells[r][c]` is row r, column c (both 0-indexed). Box has fixed
size; combinators that change size return a new Box. Out-of-range
access via `cellAt` returns a default ' ' cell so renderers can
overlay without bounds checks at every step. -/

structure Cell where
  ch    : Char := ' '
  style : Style := {}
  /-- Right half of a double-width character in the cell to its left.
      The renderer skips it (the terminal already advanced two columns);
      `toRows` drops it so text assertions see the real string. -/
  cont  : Bool := false
  deriving Inhabited, BEq

/-! ## Display width

Terminals give East Asian wide / fullwidth characters (CJK, kana,
Hangul, fullwidth forms) and most emoji two columns. Combining marks,
zero-width spaces, variation selectors and C0 controls take none; a
`Cell` holds a single `Char`, so those are dropped rather than
attached to the previous cell. Ranges follow Markus Kuhn's `wcwidth`
plus the Unicode emoji blocks. -/

private def wideRanges : Array (Nat × Nat) := #[
  (0x1100, 0x115F), (0x231A, 0x231B), (0x2329, 0x232A), (0x23E9, 0x23EC),
  (0x23F0, 0x23F0), (0x23F3, 0x23F3), (0x25FD, 0x25FE), (0x2614, 0x2615),
  (0x2648, 0x2653), (0x267F, 0x267F), (0x2693, 0x2693), (0x26A1, 0x26A1),
  (0x26AA, 0x26AB), (0x26BD, 0x26BE), (0x26C4, 0x26C5), (0x26CE, 0x26CE),
  (0x26D4, 0x26D4), (0x26EA, 0x26EA), (0x26F2, 0x26F3), (0x26F5, 0x26F5),
  (0x26FA, 0x26FA), (0x26FD, 0x26FD), (0x2705, 0x2705), (0x270A, 0x270B),
  (0x2728, 0x2728), (0x274C, 0x274C), (0x274E, 0x274E), (0x2753, 0x2755),
  (0x2757, 0x2757), (0x2795, 0x2797), (0x27B0, 0x27B0), (0x27BF, 0x27BF),
  (0x2B1B, 0x2B1C), (0x2B50, 0x2B50), (0x2B55, 0x2B55),
  (0x2E80, 0x303E), (0x3041, 0x33FF), (0x3400, 0x4DBF), (0x4E00, 0x9FFF),
  (0xA000, 0xA4CF), (0xA960, 0xA97F), (0xAC00, 0xD7A3), (0xF900, 0xFAFF),
  (0xFE10, 0xFE19), (0xFE30, 0xFE6F), (0xFF00, 0xFF60), (0xFFE0, 0xFFE6),
  (0x16FE0, 0x18AFF), (0x1B000, 0x1B2FF), (0x1F004, 0x1F004), (0x1F0CF, 0x1F0CF),
  (0x1F18E, 0x1F18E), (0x1F191, 0x1F19A), (0x1F200, 0x1F251), (0x1F300, 0x1F320),
  (0x1F32D, 0x1F335), (0x1F337, 0x1F37C), (0x1F37E, 0x1F393), (0x1F3A0, 0x1F3CA),
  (0x1F3CF, 0x1F3D3), (0x1F3E0, 0x1F3F0), (0x1F3F4, 0x1F3F4), (0x1F3F8, 0x1F43E),
  (0x1F440, 0x1F440), (0x1F442, 0x1F4FC), (0x1F4FF, 0x1F53D), (0x1F54B, 0x1F54E),
  (0x1F550, 0x1F567), (0x1F57A, 0x1F57A), (0x1F595, 0x1F596), (0x1F5A4, 0x1F5A4),
  (0x1F5FB, 0x1F64F), (0x1F680, 0x1F6C5), (0x1F6CC, 0x1F6CC), (0x1F6D0, 0x1F6D2),
  (0x1F6D5, 0x1F6D7), (0x1F6EB, 0x1F6EC), (0x1F6F4, 0x1F6FC), (0x1F7E0, 0x1F7EB),
  (0x1F90C, 0x1F93A), (0x1F93C, 0x1F945), (0x1F947, 0x1F9FF), (0x1FA70, 0x1FAFF),
  (0x20000, 0x2FFFD), (0x30000, 0x3FFFD)]

private def zeroRanges : Array (Nat × Nat) := #[
  (0x0300, 0x036F), (0x0483, 0x0489), (0x0591, 0x05BD), (0x0610, 0x061A),
  (0x064B, 0x065F), (0x0E31, 0x0E31), (0x0E34, 0x0E3A), (0x0E47, 0x0E4E),
  (0x1AB0, 0x1AFF), (0x1DC0, 0x1DFF), (0x200B, 0x200F), (0x2028, 0x202E),
  (0x2060, 0x2064), (0x20D0, 0x20FF), (0x3099, 0x309A), (0xFE00, 0xFE0F),
  (0xFE20, 0xFE2F), (0xFEFF, 0xFEFF), (0xE0100, 0xE01EF)]

private def inRanges (rs : Array (Nat × Nat)) (n : Nat) : Bool := Id.run do
  -- binary search over sorted, non-overlapping ranges
  let mut lo := 0
  let mut hi := rs.size
  while lo < hi do
    let mid := (lo + hi) / 2
    let (a, b) := rs[mid]!
    if n < a then hi := mid
    else if n > b then lo := mid + 1
    else return true
  return false

/-- Columns `c` occupies on a terminal: 0, 1 or 2. -/
def charWidth (c : Char) : Nat :=
  let n := c.toNat
  if n < 0x20 || (0x7F ≤ n && n < 0xA0) then 0
  else if n < 0x300 then 1
  else if inRanges zeroRanges n then 0
  else if inRanges wideRanges n then 2
  else 1

/-- Columns a string occupies on a terminal. -/
def displayWidth (s : String) : Nat :=
  s.foldl (fun acc c => acc + charWidth c) 0

structure Box where
  width  : Nat
  height : Nat
  /-- `cells[r][c]` for 0 ≤ r < height, 0 ≤ c < width. -/
  cells  : Array (Array Cell)
  deriving Inhabited

namespace Box

/-- An empty box of given size filled with the default cell. -/
def empty (width height : Nat) : Box := {
  width, height,
  cells := Array.replicate height (Array.replicate width ({} : Cell))
}

/-- A box filled with one character + style. -/
def filled (width height : Nat) (ch : Char) (style : Style := {}) : Box := {
  width, height,
  cells := Array.replicate height (Array.replicate width { ch, style })
}

/-- A single row from a string. Truncates or pads to `width` columns.
    A wide character takes two cells (the second marked `cont`); one
    that would straddle the right edge is cut, not split. -/
def textRow (width : Nat) (s : String) (style : Style := {}) : Array Cell := Id.run do
  let mut row : Array Cell := #[]
  for c in s.toList do
    let cw := charWidth c
    if cw == 0 then continue
    if row.size + cw > width then break
    row := row.push { ch := c, style := style }
    if cw == 2 then row := row.push { ch := ' ', style := style, cont := true }
  while row.size < width do
    row := row.push { ch := ' ', style := style }
  return row

/-- One-line text box. -/
def text (width : Nat) (s : String) (style : Style := {}) : Box := {
  width, height := 1,
  cells := #[textRow width s style]
}

/-- Get cell at (r, c). Out of range → default cell. -/
def cellAt (b : Box) (r c : Nat) : Cell :=
  match b.cells[r]? with
  | none => {}
  | some row => row[c]?.getD {}

/-- Overlay `over` onto `under` starting at (top, left). Cells of
    `over` whose `ch` is the substitute-character `'\u0000'` mean
    "transparent" and don't paint. -/
def overlay (under over : Box) (top left : Nat) : Box := Id.run do
  let mut cells := under.cells
  for r in [:over.height] do
    let dstRow := top + r
    if dstRow ≥ under.height then break
    let row := cells[dstRow]!
    let mut newRow := row
    for c in [:over.width] do
      let dstCol := left + c
      if dstCol ≥ under.width then break
      let cell := (over.cells[r]!)[c]!
      if cell.ch != '\u0000' then
        newRow := newRow.set! dstCol cell
    cells := cells.set! dstRow newRow
  return { under with cells := cells }

/-- Glue two boxes side by side. Heights are unified by padding the
    shorter one at the bottom with default cells. -/
def hcat (a b : Box) : Box := Id.run do
  let h := if a.height > b.height then a.height else b.height
  let w := a.width + b.width
  let mut rows : Array (Array Cell) := #[]
  for r in [:h] do
    let aRow := if r < a.height then a.cells[r]! else Array.replicate a.width ({} : Cell)
    let bRow := if r < b.height then b.cells[r]! else Array.replicate b.width ({} : Cell)
    rows := rows.push (aRow ++ bRow)
  return { width := w, height := h, cells := rows }

/-- Stack two boxes vertically. Widths are unified by padding the
    narrower one on the right with default cells. -/
def vcat (a b : Box) : Box := Id.run do
  let w := if a.width > b.width then a.width else b.width
  let pad := fun (row : Array Cell) (rowW : Nat) =>
    if rowW < w then row ++ Array.replicate (w - rowW) ({} : Cell) else row
  let mut rows : Array (Array Cell) := #[]
  for row in a.cells do rows := rows.push (pad row a.width)
  for row in b.cells do rows := rows.push (pad row b.width)
  return { width := w, height := a.height + b.height, cells := rows }

/-- Flatten the box to a list of plain text rows (no style info).
    Used by the test framework for substring assertions. -/
def toRows (b : Box) : List String :=
  b.cells.toList.map (fun row =>
    String.ofList ((row.toList.filter (!·.cont)).map (fun c => c.ch)))

/-- All rows joined with '\n'. -/
def toString (b : Box) : String :=
  String.intercalate "\n" b.toRows

/-- Does any row contain `needle` as a substring? Test convenience. -/
def contains (b : Box) (needle : String) : Bool :=
  b.toRows.any (fun row => (row.splitOn needle).length > 1)

end Box

/-! ## Key — what `onKey` receives -/

inductive Key where
  | char    (c : Char)
  | enter
  | tab
  | shiftTab
  | esc
  | backspace
  | delete
  | up | down | left | right
  | home | end_
  | ctrl    (c : Char)
  | pageUp | pageDown
  deriving Inhabited, BEq, Repr

/-! ## Mouse — what `onMouse` receives

Coordinates are relative to the receiving widget's top-left corner
(0-based). They are `Int` because a drag that started inside a widget
keeps being delivered to it after the pointer leaves, so the position
can be negative or past the widget's size. -/

inductive MouseButton where
  | left | middle | right | none
  deriving Inhabited, BEq, Repr

inductive MouseKind where
  /-- Button went down. -/
  | press
  /-- Button came up (delivered to the widget that got the press). -/
  | release
  /-- Moved with a button held (delivered to the widget that got the press). -/
  | drag
  /-- Moved with no button held (only if the terminal reports it). -/
  | move
  | scrollUp | scrollDown
  deriving Inhabited, BEq, Repr

structure Mouse where
  kind   : MouseKind
  button : MouseButton := .left
  row    : Int
  col    : Int
  shift  : Bool := false
  alt    : Bool := false
  ctrl   : Bool := false
  deriving Inhabited, BEq, Repr

/-- A child's placement inside its parent, relative to the parent's
    top-left corner. `row` can be negative (a scrolled-off child). -/
structure Rect where
  row    : Int
  col    : Int
  width  : Nat
  height : Nat
  deriving Inhabited, BEq, Repr

def Rect.contains (r : Rect) (row col : Int) : Bool :=
  r.row ≤ row && row < r.row + r.height && r.col ≤ col && col < r.col + r.width

/-! ## Widget

A widget is fully described by its renderer + key handler. `focusId`
is `none` when the widget can't receive focus; otherwise the app's
focus traversal will visit it. -/

structure Widget (msg : Type) where
  /-- Render to a Box that fits exactly the given width / height.
      `focused` is whether this widget currently holds focus, so it
      can paint itself differently (e.g. inverse colour highlight). -/
  render    : Nat → Nat → Bool → Box
  /-- Key handling. `none` = "I don't consume this key, let the app
      route it (e.g. Tab to next widget)". -/
  onKey     : Key → Option msg := fun _ => none
  /-- Used by the app's focus traversal. `""` = not focusable. -/
  focusId   : String := ""
  /-- Natural size hint. The app honours it inside vbox/hbox if it
      can — otherwise it stretches to fit. `none` = "as big as you'll
      give me". -/
  prefWidth  : Option Nat := none
  prefHeight : Option Nat := none
  /-- Direct child widgets, in render order. Combinators (`vbox`,
      `hbox`, `border`, …) set this so the App's focus dispatch can
      walk the tree to locate the widget that owns the current focus
      id. Leaf widgets leave it empty. -/
  children   : List (Widget msg) := []
  /-- Mouse handling: widget width, height, then the event in local
      coordinates. `none` lets the event bubble to the parent (press
      and scroll only; drag/release go to the pressed widget alone). -/
  onMouse    : Nat → Nat → Mouse → Option msg := fun _ _ _ => none
  /-- Where each of `children` sits for a given (width, height), in
      the same order. Combinators set this so clicks can be routed to
      the widget under the pointer; leaves leave it empty. -/
  place      : Nat → Nat → List Rect := fun _ _ => []
  /-- Focus-aware renderer: (width, height, focused id). Combinators
      set it so focus highlighting reaches nested widgets; `none` =
      use `render` with this widget's own focused flag. -/
  renderIn   : Option (Nat → Nat → String → Box) := none

namespace Widget

/-- Walk the tree depth-first and apply `key` to the widget whose
    `focusId` matches `focused`. Returns the first `some msg` found,
    `none` if no focused widget consumes the key. -/
partial def dispatchKey (w : Widget m) (focused : String) (key : Key) : Option m :=
  if w.focusId == focused && !focused.isEmpty then
    w.onKey key
  else
    let rec go : List (Widget m) → Option m
      | [] => none
      | c :: rest =>
        match dispatchKey c focused key with
        | some m => some m
        | none   => go rest
    go w.children

/-- All focus ids in the tree, depth-first, in render order.
    Used by the app to derive the Tab cycle order without the
    caller having to declare it. -/
partial def focusables (w : Widget m) : List String :=
  let here := if w.focusId.isEmpty then [] else [w.focusId]
  here ++ (w.children.flatMap focusables)

/-- Render with focus propagation: every widget whose `focusId`
    equals `focusId` renders with `focused = true`, however deep it
    sits in the combinator tree. -/
partial def renderFocused (w : Widget m) (width height : Nat) (focusId : String) : Box :=
  match w.renderIn with
  | some r => r width height focusId
  | none   => w.render width height (w.focusId == focusId && !focusId.isEmpty)

/-- Path (child indices from the root) of the deepest widget under
    `(row, col)`, given the root is laid out at `width × height`. -/
partial def hitPath (w : Widget m) (width height : Nat) (row col : Int) : List Nat :=
  let rects := w.place width height
  let rec pick : List (Widget m) → List Rect → Nat → List Nat
    | c :: cs, r :: rs, i =>
      if r.contains row col then i :: hitPath c r.width r.height (row - r.row) (col - r.col)
      else pick cs rs (i + 1)
    | _, _, _ => []
  pick w.children rects 0

/-- The widgets along `path` with their absolute placement, root
    first. Stops early if the tree no longer has that shape. -/
partial def chain (w : Widget m) (width height : Nat) (path : List Nat) : List (Widget m × Rect) :=
  let here := (w, { row := 0, col := 0, width, height : Rect })
  match path with
  | [] => [here]
  | i :: rest =>
    match w.children[i]?, (w.place width height)[i]? with
    | some c, some r =>
      here :: (chain c r.width r.height rest).map (fun (cw, cr) =>
        (cw, { cr with row := cr.row + r.row, col := cr.col + r.col }))
    | _, _ => [here]

/-- What a mouse event did: the message (if any widget consumed it)
    and the focus id to move to (deepest focusable under a press). -/
structure MouseResult (m : Type) where
  msg?   : Option m := none
  focus? : Option String := none

/-- Deliver `ev` (root coordinates) along `path`. With `bubble`, the
    deepest widget tries first and unhandled events walk up to the
    root; without it only the widget at the end of `path` sees it. -/
def routeMouse (w : Widget m) (width height : Nat) (path : List Nat) (ev : Mouse)
    (bubble : Bool := true) : MouseResult m := Id.run do
  let ch := (chain w width height path).reverse
  let targets := if bubble then ch else ch.take 1
  let mut res : MouseResult m := {}
  if ev.kind == .press then
    res := { res with focus? := (ch.find? (fun (c, _) => !c.focusId.isEmpty)).map (·.1.focusId) }
  for (c, r) in targets do
    let ev' := { ev with row := ev.row - r.row, col := ev.col - r.col }
    if let some msg := c.onMouse r.width r.height ev' then
      return { res with msg? := some msg }
  return res

end Widget

end LeanTea.Tui
