import LeanTea.Tui.Core
import LeanTea.Tui.Combinator
import LeanTea.Tui.Input

/-! # LeanTea.Tui.App — the IO side: terminal setup, repaint, input loop

The pure widget tree from `Core` + `Combinator` + `Element` only
becomes a running app via `App.run`. The app:

  1. Switches the terminal into raw mode + alt-screen, hides the
     cursor and (unless `RunOpts.mouse := false`) turns on SGR mouse
     reporting.
  2. Renders the root widget at the terminal's size (or the fixed
     `RunOpts.width/height`) and paints it. Later frames only repaint
     the cells that changed.
  3. Decodes input (`LeanTea.Tui.Input`) and routes each event with
     `App.handle`:
       - Tab / Shift-Tab cycle focus (`RunOpts.focusOrder`, or every
         focusable id in the tree when that is empty).
       - Keys go to the focused widget's `onKey`.
       - A mouse press goes to the deepest widget under the pointer
         (bubbling up until one handles it) and focuses the deepest
         focusable one; drag and release go to the widget that got
         the press, so dragging a split divider keeps working when the
         pointer leaves it.
       - A returned `msg` goes through `update`.
  4. Wakes up every 100 ms without input to fire `onTick` when
     `tickMs` is set, and re-measures the terminal every 500 ms (one
     `stty size`, ~15 ms) to follow window resizes.
  5. Repeats until `App.quitWhen state` is true or Ctrl+C.

Everything goes through `stty` and stdin/stdout — no C — so an app
also runs under `lake env lean --run`. Terminal teardown lives in a
`try ... finally`, so a crash leaves the user's shell intact.
-/

namespace LeanTea.Tui

/-! ## App definition -/

structure App (s : Type) (m : Type) where
  init     : s
  view     : s → Widget m
  update   : m → s → s
  /-- The app exits cleanly the first time `quitWhen state` is true. -/
  quitWhen : s → Bool := fun _ => false
  /-- Widget id to focus initially. Empty = first id in `RunOpts.focusOrder`. -/
  initialFocus : String := ""
  /-- Live-monitor support. When set, the key read is bounded to this many
      milliseconds; if no key arrives the runtime fires `onTick` (an IO
      refresh of the state, e.g. re-query a database) and repaints. `none`
      = purely input-driven (blocks until a key is pressed). -/
  tickMs   : Option Nat := none
  /-- Called on each tick to refresh state from the outside world (IO). -/
  onTick   : s → IO s := pure

structure RunOpts where
  /-- Fixed screen width. `0` = follow the terminal (re-measured
      while the app runs, so resizing the window re-lays out). -/
  width      : Nat := 0
  /-- Fixed screen height. `0` = follow the terminal. -/
  height     : Nat := 0
  /-- Explicit focus order, in tab cycle order. Empty = every
      `focusId` in the view, in render order. -/
  focusOrder : List String := []
  /-- Report mouse clicks, drags and the wheel to the app. While on,
      most terminals select text with Shift (Option on iTerm2) + drag. -/
  mouse      : Bool := true

/-! ## ANSI primitives -/

private def esc (s : String) : String := s!"\x1b[{s}"

private def clearScreen : String := esc "2J" ++ esc "H"
private def hideCursor  : String := esc "?25l"
private def showCursor  : String := esc "?25h"
private def altScreenOn  : String := esc "?1049h"
private def altScreenOff : String := esc "?1049l"
/-- Press/release (1000), drag motion (1002), SGR coordinates (1006). -/
private def mouseOn      : String := esc "?1000h" ++ esc "?1002h" ++ esc "?1006h"
private def mouseOff     : String := esc "?1006l" ++ esc "?1002l" ++ esc "?1000l"
private def reset        : String := esc "0m"

private def colorCode (c : Color) (bg : Bool) : String :=
  let base := if bg then 40 else 30
  match c with
  | .default  => esc s!"{base + 9}m"
  | .black    => esc s!"{base + 0}m"
  | .red      => esc s!"{base + 1}m"
  | .green    => esc s!"{base + 2}m"
  | .yellow   => esc s!"{base + 3}m"
  | .blue     => esc s!"{base + 4}m"
  | .magenta  => esc s!"{base + 5}m"
  | .cyan     => esc s!"{base + 6}m"
  | .white    => esc s!"{base + 7}m"
  | .bright cc =>
    let bbase := if bg then 100 else 90
    match cc with
    | .black   => esc s!"{bbase + 0}m"
    | .red     => esc s!"{bbase + 1}m"
    | .green   => esc s!"{bbase + 2}m"
    | .yellow  => esc s!"{bbase + 3}m"
    | .blue    => esc s!"{bbase + 4}m"
    | .magenta => esc s!"{bbase + 5}m"
    | .cyan    => esc s!"{bbase + 6}m"
    | .white   => esc s!"{bbase + 7}m"
    | _        => esc s!"{base + 9}m"

private def styleCode (s : Style) : String := Id.run do
  let mut out := reset
  out := out ++ colorCode s.fg false
  out := out ++ colorCode s.bg true
  if s.bold    then out := out ++ esc "1m"
  if s.dim     then out := out ++ esc "2m"
  if s.inverse then out := out ++ esc "7m"
  return out

/-- ANSI for columns `c0 ..< c1` of `row`, starting with a cursor
    move to (`r`, `c0`). A double-width character is written once and
    its `cont` cell skipped; a half that lost its partner (cropped by a
    layout or overwritten by an overlay) is painted as a space so the
    rest of the row stays aligned. -/
private def emitSpan (row : Array Cell) (r c0 c1 : Nat) : String := Id.run do
  let mut out := esc s!"{r + 1};{c0 + 1}H"
  let mut curStyle : Style := { fg := .red }  -- non-default so first cell forces an emit
  let mut c := c0
  while c < c1 do
    let cell := row[c]!
    if cell.style != curStyle then
      out := out ++ styleCode cell.style
      curStyle := cell.style
    if charWidth cell.ch == 2 && !cell.cont then
      if (row[c + 1]?.map (·.cont)).getD false then
        out := out.push cell.ch
        c := c + 2
      else
        out := out.push ' '
        c := c + 1
    else
      out := out.push (if cell.cont || charWidth cell.ch == 0 then ' ' else cell.ch)
      c := c + 1
  return out ++ reset

/-- Render a Box to one ANSI string, batching consecutive cells of
    the same style so we don't emit a fresh escape per character.

    Each row starts with an explicit cursor move (`ESC[<row>;1H`)
    instead of following a `\n`: `runWith` puts the tty in raw mode,
    which turns off output post-processing (`opost`/`onlcr`), so a
    bare `\n` only moves down and the next row would start where the
    previous one ended (issue #22). Absolute positioning also means
    the last row never scrolls the screen. -/
def renderBoxAnsi (box : Box) : String := Id.run do
  let mut out : String := reset
  for r in [:box.height] do
    out := out ++ emitSpan box.cells[r]! r 0 box.width
  return out ++ reset

/-- ANSI that turns a screen showing `prev` into one showing `box`:
    for each changed row, only the span from the first to the last
    changed cell (widened so it never starts or ends inside a
    double-width character). A size change repaints everything. -/
def renderDiff (prev box : Box) : String := Id.run do
  if prev.width != box.width || prev.height != box.height then
    return clearScreen ++ renderBoxAnsi box
  let mut out : String := ""
  for r in [:box.height] do
    let a := prev.cells[r]!
    let b := box.cells[r]!
    if a == b then continue
    let diff := (List.range box.width).filter (fun c => a[c]? != b[c]?)
    let mut first := diff.head?.getD 0
    let mut last := diff.getLast?.getD 0
    let isCont := fun (c : Nat) => (b[c]?.map (·.cont)).getD false || (a[c]?.map (·.cont)).getD false
    if first > 0 && isCont first then first := first - 1
    if isCont (last + 1) then last := last + 1
    out := out ++ emitSpan b r first (last + 1)
  return out

/-! ## Focus cycling -/

private def cycleNext (order : List String) (cur : String) : String :=
  let rec loop : List String → Option String → String
    | [], _ => order.head?.getD ""
    | x :: xs, none => loop xs (if x == cur then some x else none)
    | x :: _,  some _ => x
  match order with
  | [] => ""
  | first :: _ =>
    if cur.isEmpty then first
    else loop order none

private def cyclePrev (order : List String) (cur : String) : String :=
  cycleNext order.reverse cur

/-! ## Event handling (pure — shared with `LeanTea.Tui.Test`) -/

/-- The part of the loop state that events change. -/
structure Loop (s : Type) where
  state   : s
  focused : String := ""
  /-- Path of the widget that got the last mouse press while a button
      is held; drag/release go there. -/
  capture : Option (List Nat) := none
  /-- Ctrl+C was pressed. -/
  quit    : Bool := false

/-- Apply one event to the loop state, for a screen of `width × height`.
    `focusOrder` empty = derive it from the view. -/
def App.handle (app : App s m) (focusOrder : List String) (width height : Nat)
    (st : Loop s) (ev : Event) : Loop s :=
  let w := app.view st.state
  let apply := fun (st : Loop s) (msg? : Option m) =>
    match msg? with
    | some msg => { st with state := app.update msg st.state }
    | none => st
  match ev with
  | .key k =>
    let order := if focusOrder.isEmpty then w.focusables else focusOrder
    match k with
    | .ctrl 'c' => { st with quit := true }
    | .tab      => { st with focused := cycleNext order st.focused }
    | .shiftTab => { st with focused := cyclePrev order st.focused }
    | _         => apply st (w.dispatchKey st.focused k)
  | .mouse e =>
    match e.kind with
    | .press =>
      let path := w.hitPath width height e.row e.col
      let r := w.routeMouse width height path e
      apply { st with capture := some path, focused := r.focus?.getD st.focused } r.msg?
    | .drag | .release =>
      let st' := { st with capture := if e.kind == .release then none else st.capture }
      match st.capture with
      | some path => apply st' (w.routeMouse width height path e (bubble := false)).msg?
      | none => st'
    | _ =>
      let path := w.hitPath width height e.row e.col
      apply st (w.routeMouse width height path e).msg?

/-! ## Raw terminal mode (POSIX, via stty) -/

/-- Run an stty command, tolerating failure (e.g. no controlling TTY): the
    app then simply runs without raw mode instead of crashing. -/
private def stty (arg : String) : IO Unit := do
  try
    let _ ← IO.Process.run { cmd := "sh", args := #["-c", s!"stty {arg} < /dev/tty"] }
    pure ()
  catch _ => pure ()

/-- Raw mode with a read timeout: `min 0 time 1` makes a 1-byte read
    return after 100 ms even with no input, so the loop can notice a
    resize or fire a tick. -/
private def sttyRawTimedOn : IO Unit := stty "raw -echo min 0 time 1"

private def sttyRawOff : IO Unit := stty "-raw echo"

/-- `(cols, rows)` of the controlling terminal, if there is one. -/
def terminalSize : IO (Option (Nat × Nat)) := do
  try
    -- via /dev/tty: a child's inherited stdin is not the terminal here
    let out ← IO.Process.output { cmd := "sh", args := #["-c", "stty size < /dev/tty"] }
    match out.stdout.trimAscii.toString.splitOn " " with
    | [r, c] =>
      match r.toNat?, c.toNat? with
      | some r, some c => return if r > 0 && c > 0 then some (c, r) else none
      | _, _ => return none
    | _ => return none
  catch _ => return none

/-! ## Main loop -/

partial def App.runWith (app : App s m) (opts : RunOpts) : IO Unit := do
  let stdout ← IO.getStdout
  let stdin ← IO.getStdin
  let measure : IO (Nat × Nat) := do
    if opts.width > 0 && opts.height > 0 then return (opts.width, opts.height)
    let (tw, th) := (← terminalSize).getD (100, 30)
    return (if opts.width > 0 then opts.width else tw, if opts.height > 0 then opts.height else th)
  stdout.putStr (altScreenOn ++ hideCursor ++ clearScreen ++ (if opts.mouse then mouseOn else ""))
  stdout.flush
  sttyRawTimedOn
  let mut st : Loop s := {
    state := app.init,
    focused := if app.initialFocus.isEmpty then opts.focusOrder.head?.getD "" else app.initialFocus }
  if st.focused.isEmpty then
    st := { st with focused := ((app.view st.state).focusables.head?).getD "" }
  try
    let mut (width, height) ← measure
    let mut prev : Option Box := none
    let mut pending : List UInt8 := []
    let mut dirty := true
    let mut lastTick ← IO.monoMsNow
    let mut lastMeasure := lastTick
    while !st.quit && !(app.quitWhen st.state) do
      if dirty then
        let box := (app.view st.state).renderFocused width height st.focused
        stdout.putStr (match prev with
          | some p => renderDiff p box
          | none => clearScreen ++ renderBoxAnsi box)
        stdout.flush
        prev := some box
        dirty := false
      -- one byte at a time: returns at once while a sequence is arriving,
      -- empty after the 100 ms `stty time` when there is no input
      let b ← stdin.read 1
      let (evs, rest) :=
        if b.size > 0 then Input.decode (pending ++ b.toList)
        else if pending.isEmpty then (#[], [])
        else Input.decode pending (final := true)
      pending := rest
      for ev in evs do
        st := app.handle opts.focusOrder width height st ev
        dirty := true
      if b.size == 0 then
        let now ← IO.monoMsNow
        if let some ms := app.tickMs then
          if now - lastTick ≥ ms then
            st := { st with state := ← app.onTick st.state }
            lastTick := now
            dirty := true
        if now - lastMeasure ≥ 500 then
          lastMeasure := now
          let (w', h') ← measure
          if w' != width || h' != height then
            width := w'
            height := h'
            dirty := true
  finally
    sttyRawOff
    stdout.putStr ((if opts.mouse then mouseOff else "") ++ showCursor ++ altScreenOff ++ reset)
    stdout.flush

def App.run (app : App s m) : IO Unit :=
  App.runWith app {}

end LeanTea.Tui
