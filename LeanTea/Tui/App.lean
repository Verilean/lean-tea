import LeanTea.Tui.Core
import LeanTea.Tui.Combinator

/-! # LeanTea.Tui.App — the IO side: terminal setup, repaint, key read loop

The pure widget tree from `Core` + `Combinator` + `Element` only
becomes a running app via `App.run`. The app:

  1. Switches the terminal into raw mode + alt-screen + hides cursor.
  2. Renders the root widget to a `Box`, emits ANSI to repaint.
  3. Reads one keystroke (`Key`), routes it:
       - Tab / Shift-Tab cycle focus across `RunOpts.focusOrder`.
       - Anything else is passed to the root widget's `onKey`. If it
         returns a `msg`, the app calls `update` and re-renders.
  4. Repeats until `App.quitWhen state` returns true.

Terminal teardown lives in a `try ... finally`, so a crash leaves
the user's shell intact.

## Known wart of v0

Focus traversal asks the caller to pass `RunOpts.focusOrder`
explicitly. Combinators (`vbox`, `hbox`) don't expose children, so
the framework can't auto-detect focusable ids from the tree. A
future revision will introduce a `Widget.focusables` visitor and
drop the manual list.
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
  width      : Nat := 100
  height     : Nat := 30
  /-- Explicit focus order, in tab cycle order. Must include every
      `focusId` you want Tab to reach. -/
  focusOrder : List String := []

/-! ## ANSI primitives -/

private def esc (s : String) : String := s!"\x1b[{s}"

private def clearScreen : String := esc "2J" ++ esc "H"
private def cursorHome  : String := esc "H"
private def hideCursor  : String := esc "?25l"
private def showCursor  : String := esc "?25h"
private def altScreenOn  : String := esc "?1049h"
private def altScreenOff : String := esc "?1049l"
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

/-- Render a Box to one ANSI string, batching consecutive cells of
    the same style so we don't emit a fresh escape per character. -/
def renderBoxAnsi (box : Box) : String := Id.run do
  let mut out : String := cursorHome ++ reset
  let mut curStyle : Style := { fg := .red }  -- non-default so first cell forces an emit
  for r in [:box.height] do
    let row := box.cells[r]!
    for c in [:box.width] do
      let cell := row[c]!
      if cell.style != curStyle then
        out := out ++ styleCode cell.style
        curStyle := cell.style
      out := out.push cell.ch
    out := out ++ reset ++ "\n"
    curStyle := {}
  return out ++ reset

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

/-! ## Raw terminal mode (POSIX, via stty) -/

/-- Run an stty command, tolerating failure (e.g. no controlling TTY): the
    app then simply runs without raw mode instead of crashing. -/
private def stty (arg : String) : IO Unit := do
  try
    let _ ← IO.Process.run { cmd := "sh", args := #["-c", s!"stty {arg} < /dev/tty"] }
    pure ()
  catch _ => pure ()

private def sttyRawOn : IO Unit := stty "raw -echo"

/-- Raw mode with a read timeout: `min 0 time <deci>` makes a 1-byte read
    return after `deci` × 100ms even with no input, so the loop can tick. -/
private def sttyRawTimedOn (deci : Nat) : IO Unit := stty s!"raw -echo min 0 time {deci}"

private def sttyRawOff : IO Unit := stty "-raw echo"

/-! ## Key reading -/

/-- Like `readKey` but returns `none` when the (timed) read yields no byte,
    so a ticking loop can tell "timed out, fire a tick" from a real key. -/
partial def readKey? (stdin : IO.FS.Stream) : IO (Option Key) := do
  let b0 ← stdin.read 1
  if b0.size == 0 then return none
  return some (← readKeyFrom stdin b0[0]!)
where
  readKeyFrom (stdin : IO.FS.Stream) (c0 : UInt8) : IO Key := do
    if c0 == 0x1b then
      let b1 ← stdin.read 1
      if b1.size == 0 then return Key.esc
      if b1[0]! == 0x5b /- [ -/ then
        let b2 ← stdin.read 1
        if b2.size == 0 then return Key.esc
        match b2[0]! with
        | 0x41 => return Key.up
        | 0x42 => return Key.down
        | 0x43 => return Key.right
        | 0x44 => return Key.left
        | 0x46 => return Key.end_
        | 0x48 => return Key.home
        | 0x5a => return Key.shiftTab
        | _ => return Key.esc
      else return Key.esc
    else if c0 == 0x09 then return Key.tab
    else if c0 == 0x0d || c0 == 0x0a then return Key.enter
    else if c0 == 0x7f || c0 == 0x08 then return Key.backspace
    else if c0 < 0x20 then return Key.ctrl (Char.ofNat (c0.toNat + 0x60))
    else return Key.char (Char.ofNat c0.toNat)

partial def readKey (stdin : IO.FS.Stream) : IO Key := do
  let b0 ← stdin.read 1
  if b0.size == 0 then return Key.esc
  let c0 := b0[0]!
  if c0 == 0x1b then
    let b1 ← stdin.read 1
    if b1.size == 0 then return Key.esc
    let c1 := b1[0]!
    if c1 == 0x5b /- [ -/ then
      let b2 ← stdin.read 1
      if b2.size == 0 then return Key.esc
      match b2[0]! with
      | 0x41 /- A -/ => return Key.up
      | 0x42 /- B -/ => return Key.down
      | 0x43 /- C -/ => return Key.right
      | 0x44 /- D -/ => return Key.left
      | 0x46 /- F -/ => return Key.end_
      | 0x48 /- H -/ => return Key.home
      | 0x5a /- Z -/ => return Key.shiftTab
      | _ => return Key.esc
    else
      return Key.esc
  else if c0 == 0x09 then return Key.tab
  else if c0 == 0x0d || c0 == 0x0a then return Key.enter
  else if c0 == 0x7f || c0 == 0x08 then return Key.backspace
  else if c0 < 0x20 then
    let letter := Char.ofNat (c0.toNat + 0x60)
    return Key.ctrl letter
  else
    return Key.char (Char.ofNat c0.toNat)

/-! ## Main loop -/

partial def App.runWith (app : App s m) (opts : RunOpts) : IO Unit := do
  let stdout ← IO.getStdout
  let stdin ← IO.getStdin
  stdout.putStr (altScreenOn ++ hideCursor ++ clearScreen)
  stdout.flush
  match app.tickMs with
  | some ms => sttyRawTimedOn (max 1 (ms / 100))
  | none    => sttyRawOn
  let mut state := app.init
  let mut focused :=
    if app.initialFocus.isEmpty then opts.focusOrder.head?.getD "" else app.initialFocus
  try
    let mut running := true
    while running && !(app.quitWhen state) do
      let w := app.view state
      let box := w.render opts.width opts.height (focused == w.focusId)
      stdout.putStr (renderBoxAnsi box)
      stdout.flush
      -- ticking apps use a timed read (none = timed out -> refresh via onTick)
      let key? ← match app.tickMs with
        | some _ => readKey? stdin
        | none   => some <$> readKey stdin
      match key? with
      | none     => state ← app.onTick state
      | some key =>
        match key with
        | .ctrl 'c' => running := false
        | .tab      => focused := cycleNext opts.focusOrder focused
        | .shiftTab => focused := cyclePrev opts.focusOrder focused
        | _ =>
          match w.dispatchKey focused key with
          | none => pure ()
          | some msg => state := app.update msg state
  finally
    sttyRawOff
    stdout.putStr (showCursor ++ altScreenOff ++ reset)
    stdout.flush

def App.run (app : App s m) : IO Unit :=
  App.runWith app {}

end LeanTea.Tui
