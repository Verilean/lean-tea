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
  * Raw-mode output — `renderBoxAnsi` replayed on a raw-mode terminal
    model puts every row at column 1 (issue #22).
  * Wide characters — display width, cell layout and rendering of
    CJK / emoji.
  * Input decoding — keys, UTF-8 and SGR mouse reports from raw bytes.
  * Mouse — clicks routed to the widget under the pointer (and
    focusing it), split dividers dragged, viewports scrolled, focus
    highlight reaching nested widgets.
  * Incremental repaint — `renderDiff` brings the screen to the same
    state as a full repaint while writing only changed spans.

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

/-! ## Group 5 — ANSI output on a raw-mode terminal (issue #22)

`App.runWith` puts the tty in raw mode, which disables output
post-processing: `\n` moves the cursor down but NOT back to column 1.
`rawScreen` replays `renderBoxAnsi` output on a tiny terminal model with
exactly those semantics (CUP/`H` positions, `\r` returns, `\n` only goes
down, SGR ignored), so a renderer that relies on `\n` produces the
"staircase" from the issue and fails here. -/

namespace RawRenderGroup

/-- Replay `out` on a `rows × cols` raw-mode screen; returns the rows. -/
def rawScreen (out : String) (rows cols : Nat) : Array String := Id.run do
  let mut grid : Array (Array Char) := Array.replicate rows (Array.replicate cols ' ')
  let mut r := 0
  let mut c := 0
  let mut cs := out.toList
  while !cs.isEmpty do
    match cs with
    | '\x1b' :: '[' :: rest =>
      let params := rest.takeWhile (fun ch => ch.isDigit || ch == ';' || ch == '?')
      match rest.drop params.length with
      | fin :: more =>
        if fin == 'H' then
          let ns := (String.ofList params).splitOn ";" |>.map (fun x => (x.toNat?.getD 1) - 1)
          r := ns.getD 0 0
          c := ns.getD 1 0
        else if fin == 'J' then
          grid := Array.replicate rows (Array.replicate cols ' ')
        cs := more
      | [] => cs := []
    | '\r' :: rest => c := 0; cs := rest
    | '\n' :: rest => r := r + 1; cs := rest          -- raw mode: no implicit CR
    | ch :: rest =>
      if r < rows && c < cols then grid := grid.set! r (grid[r]!.set! c ch)
      c := c + 1
      cs := rest
    | [] => pure ()
  return grid.map (fun row => String.ofList row.toList)

def hashes (n : Nat) : String := String.ofList (List.replicate n '#')
def pad (s : String) (n : Nat) : String := s ++ String.ofList (List.replicate (n - s.length) ' ')

/-- The issue's repro: a 20×4 box of `#`. -/
def issueBox : Box := Box.filled 20 4 '#'

/-- Mixed styles mid-row, so style escapes sit between cells. -/
def styledBox : Box :=
  (Box.filled 12 3 '.').overlay (Box.text 4 "AB" { fg := .red, bold := true }) 1 5

def run : IO LSpec := do
  let scr := rawScreen (renderBoxAnsi issueBox) 6 70
  let styled := rawScreen (renderBoxAnsi styledBox) 4 20
  return group "Tui — raw-mode ANSI rendering (#22)" [
    it "every row of the 20×4 box starts at column 1"
      ((List.range 4).all (fun i => scr[i]! == pad (hashes 20) 70)),
    it "nothing is drawn below the box"
      (scr[4]! == pad "" 70 && scr[5]! == pad "" 70),
    it "styled cells land in the right place"
      (styled[0]! == pad "............" 20 && styled[1]! == pad ".....AB  ..." 20 &&
       styled[2]! == pad "............" 20),
    it "no bare newline (would scroll the last row)"
      (!(renderBoxAnsi issueBox).contains '\n')
  ]

end RawRenderGroup

/-! ## Group 6 — Wide characters -/

namespace WideGroup

/-- Strip CSI sequences, leaving only the printed characters. -/
def printed (out : String) : String := Id.run do
  let mut res := ""
  let mut cs := out.toList
  while !cs.isEmpty do
    match cs with
    | '\x1b' :: '[' :: rest =>
      let body := rest.takeWhile (fun c => !(c.toNat ≥ 0x40 && c.toNat ≤ 0x7E))
      cs := rest.drop (body.length + 1)
    | c :: rest => res := res.push c; cs := rest
    | [] => pure ()
  return res

def run : IO LSpec := do
  let row := Box.textRow 5 "あいx"
  let cut := Box.textRow 3 "あい"
  -- an overlay that overwrites the left half of a wide char leaves an orphan
  let orphan := (Box.text 4 "あい").overlay (Box.text 1 "Z") 0 2
  return group "Tui — wide characters" [
    it "charWidth: ASCII 1, kana/kanji/fullwidth/emoji 2"
      (charWidth 'a' == 1 && charWidth 'あ' == 2 && charWidth '漢' == 2 &&
       charWidth 'Ａ' == 2 && charWidth '😀' == 2 && charWidth '─' == 1),
    it "charWidth: combining marks and controls 0"
      (charWidth '\u0301' == 0 && charWidth '\u200B' == 0 && charWidth '\x07' == 0),
    it "displayWidth sums columns" (displayWidth "aあ😀" == 5),
    it "textRow puts a cont cell after each wide char"
      (row.size == 5 && !row[0]!.cont && row[1]!.cont && row[2]!.ch == 'い' && row[3]!.cont &&
       row[4]!.ch == 'x'),
    it "a wide char that would straddle the edge is cut" (cut.size == 3 && cut[2]!.ch == ' ' && !cut[2]!.cont),
    it "toRows hides cont cells" (Box.toRows { width := 5, height := 1, cells := #[row] } == ["あいx"]),
    it "renderBoxAnsi writes each wide char once" (printed (renderBoxAnsi (Box.text 5 "あいx")) == "あいx"),
    it "orphaned half is painted as a space" (printed (renderBoxAnsi orphan) == "あZ "),
    it "button width counts columns" ((button "b" .primary "保存" ()).prefWidth == some 6)
  ]

end WideGroup

/-! ## Group 7 — Input decoding -/

namespace InputGroup

def keys (s : String) : Array Event := Input.decodeString s
def m (kind : MouseKind) (row col : Int) (button : MouseButton := .left) : Event :=
  .mouse { kind, button, row, col }

def run : IO LSpec := do
  let aBytes := "あ".toUTF8.toList
  let (part, rest) := Input.decode (aBytes.take 2)
  let (whole, rest2) := Input.decode (rest ++ aBytes.drop 2)
  let (escHeld, escRest) := Input.decode [0x1b]
  return group "Tui — input decoding" [
    it "arrows (CSI and SS3)" (keys "\x1b[A\x1b[D\x1bOB" == #[.key .up, .key .left, .key .down]),
    it "modifier params are accepted" (keys "\x1b[1;5C" == #[.key .right]),
    it "~ keys: Home/End/Delete/PageUp/PageDown"
      (keys "\x1b[1~\x1b[4~\x1b[3~\x1b[5~\x1b[6~" ==
        #[.key .home, .key .end_, .key .delete, .key .pageUp, .key .pageDown]),
    it "C0: Enter, Tab, Backspace, Ctrl" (keys "\r\t\x7f\x01" == #[.key .enter, .key .tab, .key .backspace, .key (.ctrl 'a')]),
    it "UTF-8 text is one char per code point" (keys "aあ😀" == #[.key (.char 'a'), .key (.char 'あ'), .key (.char '😀')]),
    it "a UTF-8 char split across reads waits for the rest"
      (part.isEmpty && rest.length == 2 && whole == #[.key (.char 'あ')] && rest2.isEmpty),
    it "lone ESC is held until final" (escHeld.isEmpty && escRest == [0x1b] && (Input.decode [0x1b] true).1 == #[.key .esc]),
    it "ESC + key = Esc then the key" (keys "\x1bx" == #[.key .esc, .key (.char 'x')]),
    it "SGR press / release, 1-based → 0-based"
      (keys "\x1b[<0;5;3M\x1b[<0;5;3m" == #[m .press 2 4, m .release 2 4]),
    it "SGR right button and drag"
      (keys "\x1b[<2;1;1M\x1b[<32;6;3M" == #[m .press 0 0 .right, m .drag 2 5]),
    it "SGR wheel" (keys "\x1b[<64;2;2M\x1b[<65;2;2M" == #[m .scrollUp 1 1 .none, m .scrollDown 1 1 .none]),
    it "SGR modifiers"
      (keys "\x1b[<20;1;1M" == #[.mouse { kind := .press, row := 0, col := 0, shift := true, ctrl := true }]),
    it "unknown sequences are dropped, not leaked" (keys "\x1b[99Xa" == #[.key (.char 'a')])
  ]

end InputGroup

/-! ## Group 8 — Mouse routing -/

namespace MouseGroup

open AppGroup in
def counter : Session Counter CMsg := mkSession app 30 4 ["minus", "plus", "quit"]

inductive SMsg where
  | ratio (r : Nat) | top (r : Nat) | scrolled (o : Nat) | picked (i : Nat) | up | down
  deriving Inhabited

structure SState where
  ratio  : Nat := 500
  top    : Nat := 500
  offset : Nat := 0
  picked : Option Nat := none
  sel    : Nat := 0
  deriving Inhabited

def lines : List (Widget SMsg) :=
  (List.range 20).map fun i => button s!"row{i}" .ghost s!"line {i}" (SMsg.picked i)

def splitApp : Tui.App SState SMsg := {
  init := {},
  view := fun s => hsplit s.ratio .ratio (text "LEFT") (text "RIGHT"),
  update := fun msg s => match msg with
    | .ratio r => { s with ratio := r } | _ => s
}

def vsplitApp : Tui.App SState SMsg := {
  init := {},
  view := fun s => vsplit s.top .top (text "TOP") (text "BOTTOM"),
  update := fun msg s => match msg with
    | .top r => { s with top := r } | _ => s
}

def scrollApp : Tui.App SState SMsg := {
  init := {},
  view := fun s => scroll s.offset .scrolled (vbox lines) (contentHeight := some 20),
  update := fun msg s => match msg with
    | .scrolled o => { s with offset := o }
    | .picked i => { s with picked := some i }
    | _ => s
}

def listApp : Tui.App SState SMsg := {
  init := {},
  view := fun s => border .line (listView "lst" ((List.range 10).map (s!"item {·}")) s.sel .up .down (.picked s.sel)
    (onPick := some .picked)),
  update := fun msg s => match msg with
    | .picked i => { s with picked := some i, sel := i }
    | .down => { s with sel := s.sel + 1 }
    | .up => { s with sel := s.sel - 1 }
    | _ => s
}

def run : IO LSpec := do
  let sp := mkSession splitApp 20 3
  let dragged := sp.drag 0 10 0 5
  let missed := sp.drag 0 3 0 15
  let vs := mkSession vsplitApp 10 9
  let sc := mkSession scrollApp 10 5
  let sc3 := sc.scroll 2 2
  let lst := mkSession listApp 20 7
  return group "Tui — mouse" [
    it "focus highlight reaches a button nested in vbox/hbox" (counter.containsText "▶ [-]"),
    it "click runs the button under the pointer and focuses it"
      (let c := counter.click 1 6; c.state.count == 1 && c.focused == "plus" && c.containsText "▶ [+]"),
    it "click on plain text changes nothing" (let c := counter.click 0 2; c.state.count == 0 && c.focused == "minus"),
    it "raw SGR bytes drive the same path"
      ((counter.sendBytes "\x1b[<0;7;2M\x1b[<0;7;2m").state.count == 1),
    it "hsplit: divider sits at the ratio" ((sp.rowAt 0).startsWith "LEFT      │RIGHT"),
    it "hsplit: dragging the divider resizes"
      (dragged.state.ratio == 250 && (dragged.rowAt 0).startsWith "LEFT │RIGHT"),
    it "hsplit: a drag that starts off the divider does nothing" (missed.state.ratio == 500),
    it "vsplit: drag the divider down"
      (let v := vs.drag 4 0 6 0; v.state.top == 667 && v.rowAt 6 == "──────────"),
    it "resize keeps the ratio" (((sp.resize 40 3).rowAt 0).startsWith "LEFT                │RIGHT"),
    it "scroll: wheel moves the viewport by 3" (sc3.state.offset == 3 && (sc3.rowAt 0).startsWith "  line 3"),
    it "scroll: wheel up at the top is ignored" ((sc.scroll 1 1 (up := true)).state.offset == 0),
    it "scroll: click lands on the scrolled child" ((sc3.click 1 2).state.picked == some 4),
    it "scroll: dragging the scrollbar jumps"
      ((sc.drag 0 9 4 9).state.offset == 15 && ((sc.drag 0 9 4 9).rowAt 4).startsWith "  line 19"),
    it "listView: click picks the row (inside a border)"
      (let l := lst.click 3 4; l.state.picked == some 2 && l.focused == "lst"),
    it "listView: wheel sends down" ((lst.scroll 2 4).state.sel == 1)
  ]

end MouseGroup

/-! ## Group 9 — Incremental repaint -/

namespace DiffGroup

open RawRenderGroup

def before : Box := Box.filled 12 3 '.'
def after : Box := before.overlay (Box.text 3 "XYZ" { bold := true }) 1 4

def run : IO LSpec := do
  let d := renderDiff before after
  let full := rawScreen (renderBoxAnsi after) 3 12
  let patched := rawScreen (renderBoxAnsi before ++ d) 3 12
  return group "Tui — incremental repaint" [
    it "diff + old screen = full repaint of the new box" (patched == full),
    it "only the changed span is written" ((d.splitOn "\x1b[2;5H").length == 2 && !(d.contains '.')),
    it "unchanged box writes nothing" (renderDiff after after == ""),
    it "size change clears and repaints" ((renderDiff before (Box.filled 5 2 '#')).startsWith "\x1b[2J")
  ]

end DiffGroup

/-! ## main -/

def main : IO Unit := do
  let b ← BoxGroup.run
  let c ← CombinatorGroup.run
  let e ← ElementGroup.run
  let a ← AppGroup.run
  let r ← RawRenderGroup.run
  let wd ← WideGroup.run
  let i ← InputGroup.run
  let mo ← MouseGroup.run
  let d ← DiffGroup.run
  let tree := group "LeanTea.Tui widget kit" [b, c, e, a, r, wd, i, mo, d]
  let code ← lspecIO tree
  if code != 0 then IO.Process.exit code.toUInt8
