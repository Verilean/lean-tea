import LeanTea
import LeanTea.Tui

/-! # examples/Tests/TuiSpec.lean — pure tests for the LeanTea.Tui widget kit

Every widget is a pure function (`Nat → Nat → Bool → Box`) plus a
pure key handler (`Key → Option msg`). This spec exercises the
widget kit end-to-end without spinning up a real terminal:

  * Layout — `Box.text`, `vcat`, `hcat`, `vbox`, `hbox` all produce
    the expected sizes and contents.
  * Combinators — `border` adds the right corners and edges,
    `padding` clears the margins.
  * Elements — `button` renders its label, `input` reflects its
    value, `listView` highlights the selected row.
  * App loop — feeding a sequence of `Key`s into a `Session` walks
    the state machine the same way the real `App.run` does.

The whole file completes in well under a second because there's no
IO. Add a regression case here before changing layout heuristics.
-/

open LeanTea LeanTea.LSpec
open LeanTea.Tui

/-! ## Group 1 — Box primitives -/

namespace BoxGroup

def emptyHasSize : Bool :=
  let b := Box.empty 5 3
  b.width == 5 && b.height == 3 && b.cells.size == 3

def textTruncatesToWidth : Bool :=
  let b := Box.text 5 "Hello, World"
  (b.toRows.head?.getD "") == "Hello"

def textPadsToWidth : Bool :=
  let b := Box.text 8 "hi"
  (b.toRows.head?.getD "") == "hi      "

def vcatStacks : Bool :=
  let top := Box.text 6 "top"
  let bot := Box.text 6 "bottom"
  let s := Box.vcat top bot
  s.height == 2 && s.contains "top" && s.contains "bottom"

def hcatJoins : Bool :=
  let l := Box.text 4 "abcd"
  let r := Box.text 4 "WXYZ"
  let s := Box.hcat l r
  s.width == 8 && (s.toRows.head?.getD "") == "abcdWXYZ"

def containsFindsSubstring : Bool :=
  let b := Box.vcat (Box.text 10 "alpha") (Box.text 10 "beta")
  b.contains "alpha" && b.contains "beta" && !b.contains "gamma"

def run : IO LSpec := do
  return group "Tui — Box primitives" [
    it "Box.empty has the requested size" emptyHasSize,
    it "Box.text truncates to width"      textTruncatesToWidth,
    it "Box.text right-pads to width"     textPadsToWidth,
    it "Box.vcat stacks vertically"       vcatStacks,
    it "Box.hcat joins horizontally"      hcatJoins,
    it "Box.contains finds substrings"    containsFindsSubstring
  ]

end BoxGroup

/-! ## Group 2 — Combinators -/

namespace CombinatorGroup

def borderDrawsCorners : Bool :=
  let inner := Box.empty 8 3
  let widget : Widget Unit := {
    render := fun _ _ _ => inner,
    prefWidth := some 8, prefHeight := some 3
  }
  let bordered := border .line widget
  let b := bordered.render 10 5 false
  (b.cellAt 0 0).ch == '┌' &&
  (b.cellAt 0 9).ch == '┐' &&
  (b.cellAt 4 0).ch == '└' &&
  (b.cellAt 4 9).ch == '┘'

def paddingClearsMargin : Bool :=
  let widget : Widget Unit := text "X"
  let padded := padding 1 widget
  let b := padded.render 5 3 false
  (b.cellAt 1 1).ch == 'X' && (b.cellAt 0 0).ch == ' '

def vboxAllocatesPrefHeight : Bool :=
  let header : Widget Unit := { render := fun w _ _ => Box.text w "HDR",
                                prefHeight := some 2 }
  let body   : Widget Unit := { render := fun w h _ => Box.filled w h '.' }
  let root := vbox [header, body]
  let b := root.render 10 5 false
  b.contains "HDR" && ((b.cellAt 4 0).ch == '.')

def hboxAllocatesPrefWidth : Bool :=
  let left  : Widget Unit := { render := fun _ h _ => Box.filled 3 h 'L', prefWidth := some 3 }
  let right : Widget Unit := { render := fun w h _ => Box.filled w h 'R' }
  let root := hbox [left, right]
  let b := root.render 10 1 false
  ((b.cellAt 0 0).ch == 'L') && ((b.cellAt 0 5).ch == 'R')

def run : IO LSpec := do
  return group "Tui — Combinators" [
    it "border draws the right corners"   borderDrawsCorners,
    it "padding clears the margin"        paddingClearsMargin,
    it "vbox honours prefHeight"          vboxAllocatesPrefHeight,
    it "hbox honours prefWidth"           hboxAllocatesPrefWidth
  ]

end CombinatorGroup

/-! ## Group 3 — Elements -/

namespace ElementGroup

inductive Msg where | clicked deriving Inhabited

def buttonShowsLabel : Bool :=
  let w : Widget Msg := button "btn" .primary "Save" .clicked
  let b := w.render 12 1 false
  b.contains "Save"

def buttonFiresOnEnter : Bool :=
  let w : Widget Msg := button "btn" .primary "Save" .clicked
  match w.onKey .enter with
  | some .clicked => true
  | _ => false

def buttonIgnoresOtherKeys : Bool :=
  let w : Widget Msg := button "btn" .primary "Save" .clicked
  match w.onKey .esc with
  | none => true
  | _    => false

inductive InpMsg where
  | typed (c : Char) | back | submit
  deriving Inhabited

def inputShowsValue : Bool :=
  let w : Widget InpMsg := input "x" "hello" {
    onChar := .typed, onBackspace := .back, onSubmit := .submit
  }
  let b := w.render 12 1 false
  b.contains "hello"

def inputSendsCharOnKey : Bool :=
  let w : Widget InpMsg := input "x" "" {
    onChar := .typed, onBackspace := .back, onSubmit := .submit
  }
  match w.onKey (.char 'a') with
  | some (.typed 'a') => true
  | _ => false

inductive LMsg where
  | up | down | go
  deriving Inhabited

def listShowsItems : Bool :=
  let w : Widget LMsg := listView "lst" ["one", "two", "three"] 0
    LMsg.up LMsg.down LMsg.go
  let b := w.render 20 3 false
  b.contains "one" && b.contains "two" && b.contains "three"

def listHighlightsSelection : Bool :=
  let w : Widget LMsg := listView "lst" ["one", "two", "three"] 1
    LMsg.up LMsg.down LMsg.go
  let b := w.render 20 3 false
  -- The selected row should be prefixed with the ▶ marker.
  (b.toRows[1]?.getD "").startsWith "▶ two"

def run : IO LSpec := do
  return group "Tui — Elements" [
    it "button renders its label"        buttonShowsLabel,
    it "button fires onEnter"            buttonFiresOnEnter,
    it "button ignores unrelated keys"   buttonIgnoresOtherKeys,
    it "input echoes its value"          inputShowsValue,
    it "input fires onChar per keystroke" inputSendsCharOnKey,
    it "listView renders each item"      listShowsItems,
    it "listView highlights selection"   listHighlightsSelection
  ]

end ElementGroup

/-! ## Group 4 — App + Session round-trip (the test framework itself) -/

namespace AppGroup

inductive CMsg where
  | inc | dec | quit
  deriving Inhabited

structure Counter where
  count : Int := 0
  quit  : Bool := false
  deriving Inhabited

def view (s : Counter) : Widget CMsg :=
  vbox [
    text s!"count = {s.count}",
    hbox [
      button "minus" .secondary "[-]" .dec,
      button "plus"  .primary   "[+]" .inc,
      button "quit"  .danger    "[q]" .quit
    ]
  ]

def update (m : CMsg) (s : Counter) : Counter :=
  match m with
  | .inc  => { s with count := s.count + 1 }
  | .dec  => { s with count := s.count - 1 }
  | .quit => { s with quit := true }

def app : Tui.App Counter CMsg := {
  init := {},
  view := view,
  update := update,
  quitWhen := (·.quit)
}

def fresh : Session Counter CMsg :=
  mkSession app 30 4 ["minus", "plus", "quit"]

def initialRenderShowsZero : Bool :=
  fresh.containsText "count = 0"

def enterOnInitialFocusFiresMinus : Bool :=
  (fresh.sendKey .enter).state.count == -1

def tabAdvancesFocus : Bool :=
  (fresh.sendKey .tab).focused == "plus"

def enterAfterTabFiresPlus : Bool :=
  ((fresh.sendKey .tab).sendKey .enter).state.count == 1

def quitTriggersQuitWhen : Bool :=
  (((fresh.sendKey .tab).sendKey .tab).sendKey .enter).isQuit

def run : IO LSpec := do
  return group "Tui — App + Session" [
    it "initial render shows count=0"          initialRenderShowsZero,
    it "Enter on initial focus fires .dec"     enterOnInitialFocusFiresMinus,
    it "Tab advances focus"                    tabAdvancesFocus,
    it "Enter after Tab fires the next button" enterAfterTabFiresPlus,
    it "the quit button trips quitWhen"        quitTriggersQuitWhen
  ]

end AppGroup

/-! ## main -/

def main : IO Unit := do
  let b ← BoxGroup.run
  let c ← CombinatorGroup.run
  let e ← ElementGroup.run
  let a ← AppGroup.run
  let tree := group "LeanTea.Tui widget kit" [b, c, e, a]
  let code ← lspecIO tree
  if code != 0 then IO.Process.exit code.toUInt8
