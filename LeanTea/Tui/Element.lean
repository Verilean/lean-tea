import LeanTea.Tui.Core
import LeanTea.Tui.Combinator

/-! # LeanTea.Tui.Element — button / input / panel / listView

Concrete focusable widgets. Each follows the same kind-driven idea
as the ChuHan button kit:

  * `kind` selects style (and, for buttons, the SFX in the GUI case)
  * `msg` is what's emitted on activation
  * `label` / `value` / `items` is the payload

A widget's `focusId` is what the app's focus traversal sees; we
use a structured `prefix-id` string so it stays unique even when
the same widget kind appears multiple times in the same view.

## Why a single Element file (not one-per-widget)

Each widget is ~25 lines; the count is bounded. Splitting per file
adds 4-5 imports per app build. Once we cross ~6 widgets I'll
re-split.
-/

namespace LeanTea.Tui

/-! ## button — single-line activation -/

inductive ButtonKind where
  | primary | secondary | danger | ghost
  deriving BEq

private def buttonStyle (kind : ButtonKind) (focused : Bool) : Style :=
  match kind, focused with
  | .primary,   true  => { fg := .black, bg := .cyan, bold := true }
  | .primary,   false => { fg := .cyan, bold := true }
  | .secondary, true  => { fg := .black, bg := .white }
  | .secondary, false => { fg := .white }
  | .danger,    true  => { fg := .black, bg := .red, bold := true }
  | .danger,    false => { fg := .red, bold := true }
  | .ghost,     true  => { fg := .yellow, inverse := true }
  | .ghost,     false => { fg := .yellow, dim := true }

def button (id : String) (kind : ButtonKind) (label : String) (msg : m) : Widget m :=
  let labelLen := label.length
  let render := fun w _h focused =>
    let chrome := if focused then "▶ " else "  "
    let inner := chrome ++ label
    Box.text w inner (buttonStyle kind focused)
  {
    render := render,
    onKey := fun
      | .enter => some msg
      | .char ' ' => some msg
      | _ => none,
    focusId := id,
    prefHeight := some 1,
    prefWidth := some (labelLen + 2)
  }

/-! ## input — single-line text entry

The widget renders the current value and the caret when focused. It
fires one `msg` per keystroke; the parent state holds the actual
string and is the source of truth — this keeps focus & state
ownership clean.

Common pattern: parent decides how to wire each key via `dispatch`. -/

structure InputBindings (m : Type) where
  onChar      : Char → m
  onBackspace : m
  onSubmit    : m
  onEsc       : Option m := none

def input (id : String) (value : String) (bindings : InputBindings m) : Widget m :=
  let render := fun w _h focused =>
    let caret := if focused then "│" else " "
    let visible := value ++ caret
    -- truncate from the left if value overflows so caret stays on screen
    let display :=
      if visible.length > w then
        let drop := visible.length - w
        (visible.drop drop).toString
      else visible
    let style : Style := if focused then { bg := .blue, fg := .white } else { dim := true }
    Box.text w display style
  {
    render := render,
    onKey := fun
      | .char c    => some (bindings.onChar c)
      | .backspace => some bindings.onBackspace
      | .enter     => some bindings.onSubmit
      | .esc       => bindings.onEsc
      | _          => none,
    focusId := id,
    prefHeight := some 1
  }

/-! ## panel — bordered container with a title -/

def panel (title : String) (style : BorderStyle := .line) (body : Widget m) : Widget m :=
  let titleBar : Widget m := {
    render := fun w _h focused =>
      let mark := if focused then " ◆ " else " · "
      let line := mark ++ title
      let s : Style := if focused then { fg := .cyan, bold := true } else { fg := .white, bold := true }
      Box.text w line s,
    prefHeight := some 1
  }
  border style (vbox [titleBar, body])

/-! ## listView — scrollable item list with cursor

Items are plain strings (formatted by the caller). The selected
index is owned by the parent state. The list renders a window of
items around the selection. -/

def listView (id : String) (items : List String) (selected : Nat)
    (onUp onDown : m) (onSelect : m) : Widget m :=
  let render := fun w h focused => Id.run do
    if items.isEmpty || h == 0 then
      return Box.empty w h
    -- Window: keep selected in view.
    let total := items.length
    let start :=
      if selected >= h then min (selected - h + 1) (total - 1) else 0
    let window := (items.drop start).take h
    let mut rows : Array (Array Cell) := #[]
    let mut idx := start
    for itm in window do
      let isSel := idx == selected
      let chrome := if isSel then "▶ " else "  "
      let line := chrome ++ itm
      let s : Style :=
        if isSel && focused then { fg := .black, bg := .cyan }
        else if isSel then { bold := true }
        else {}
      rows := rows.push (Box.textRow w line s)
      idx := idx + 1
    -- Pad bottom.
    while rows.size < h do
      rows := rows.push (Array.replicate w ({} : Cell))
    return { width := w, height := h, cells := rows }
  {
    render := render,
    onKey := fun
      | .up    => some onUp
      | .down  => some onDown
      | .enter => some onSelect
      | _ => none,
    focusId := id
  }

end LeanTea.Tui
