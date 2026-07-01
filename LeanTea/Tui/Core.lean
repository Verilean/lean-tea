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
  deriving Inhabited, BEq

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

/-- A single row from a string. Truncates or pads to `width`. -/
def textRow (width : Nat) (s : String) (style : Style := {}) : Array Cell := Id.run do
  let chars := s.toList
  let mut row : Array Cell := #[]
  for c in chars do
    if row.size < width then row := row.push { ch := c, style := style }
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
    String.mk (row.toList.map (fun c => c.ch)))

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
  deriving Inhabited, BEq, Repr

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

/-- Render with focus propagation: each focusable child gets
    `focused = true` only if its id matches `focusId`. Combinators'
    `render` only sees `focused = false` since combinators themselves
    aren't focusable. -/
partial def renderFocused (w : Widget m) (width height : Nat) (focusId : String) : Box :=
  if w.children.isEmpty then
    -- Leaf widget: just render with the right focused flag.
    w.render width height (w.focusId == focusId && !focusId.isEmpty)
  else
    -- Combinator: render via its layout but with children that
    -- propagate focus themselves. We do this by building a
    -- "shadow" widget tree where each child's render closure is
    -- replaced with one that knows about focus. Simpler: just
    -- call the combinator's render which already calls child
    -- renders — but with no awareness of focus. So we'd need
    -- the combinator's render to accept a focus-resolver.
    --
    -- For v0 we cheat: combinators (vbox/hbox/border) call their
    -- children's `render` directly without focus. Until that's
    -- refactored, we approximate by walking the children and
    -- compositing in App.runWith. So this function is mostly a
    -- placeholder for future use.
    w.render width height (w.focusId == focusId && !focusId.isEmpty)

end Widget

end LeanTea.Tui
