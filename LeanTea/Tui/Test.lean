import LeanTea.Tui.Core
import LeanTea.Tui.Combinator
import LeanTea.Tui.App

/-! # LeanTea.Tui.Test — pure render + key dispatch, no TTY

The widget kit's main payoff: every part of `App.run`'s loop except
the IO bits (terminal modes, stdin reads, stdout writes) is a pure
function. This file exposes those pure pieces as a testable harness:

  * `mkSession init view update` — wrap an app
  * `Session.render` — produce the current `Box` for inspection
  * `Session.sendKey` — fire a synthetic `Key` and return the new state
  * `Session.click` / `Session.drag` / `Session.scroll` — synthetic
    mouse input, routed exactly like the real loop (`App.handle`)
  * `Session.resize` — lay the same state out at another size
  * `Session.containsText` / `Session.rowAt` — text assertions

The same widgets can be unit-tested without running a real terminal.
Combined with LSpec we can keep regression tests of layout +
keybindings cheap.

## Pattern

```
let app : App Counter Msg := { init, view, update }
let s := mkSession app { width := 20, height := 3 }
let s' := s.sendKey .enter
assert (s'.state.count == 1)
let s'' := s'.sendKey (.char 'q')
assert (s''.box.contains "1")
```
-/

namespace LeanTea.Tui

/-! ## Session — the test harness -/

structure Session (s : Type) (m : Type) where
  app     : App s m
  width   : Nat := 80
  height  : Nat := 24
  state   : s
  focused : String := ""
  /-- Per-session focus traversal order. Same shape as `RunOpts.focusOrder`. -/
  focusOrder : List String := []
  /-- Widget holding the current mouse press (see `Loop.capture`). -/
  capture : Option (List Nat) := none

/-- Build a Session from an App. -/
def mkSession (app : App s m) (width : Nat := 80) (height : Nat := 24)
    (focusOrder : List String := []) : Session s m := {
  app := app,
  width := width,
  height := height,
  state := app.init,
  focused := if app.initialFocus.isEmpty then focusOrder.head?.getD "" else app.initialFocus,
  focusOrder := focusOrder
}

namespace Session

/-- Render the current state to a `Box`. -/
def render (s : Session a m) : Box :=
  (s.app.view s.state).renderFocused s.width s.height s.focused

/-- Flatten the rendered Box to a single string with rows separated
    by '\n'. Convenient for snapshot tests. -/
def text (s : Session a m) : String :=
  s.render.toString

/-- Does any rendered row contain `needle`? -/
def containsText (s : Session a m) (needle : String) : Bool :=
  s.render.contains needle

/-- Get row `r` as plain text. Useful when ordering matters. -/
def rowAt (s : Session a m) (r : Nat) : String :=
  (s.render.toRows[r]?).getD ""

/-- Feed one input event through the same `App.handle` the real loop
    uses. Ctrl+C has no effect here (there is no loop to stop). -/
def sendEvent (s : Session a m) (ev : Event) : Session a m :=
  let st := s.app.handle s.focusOrder s.width s.height
    { state := s.state, focused := s.focused, capture := s.capture } ev
  { s with state := st.state, focused := st.focused, capture := st.capture }

/-- Send a single key and return the new Session. -/
def sendKey (s : Session a m) (key : Key) : Session a m :=
  s.sendEvent (.key key)

/-- Raw bytes as the terminal would send them (keys, UTF-8 text, SGR
    mouse reports), decoded by `Input.decode`. -/
def sendBytes (s : Session a m) (bytes : String) : Session a m :=
  (Input.decodeString bytes).foldl (fun acc ev => acc.sendEvent ev) s

/-- Press and release the left button at screen (`row`, `col`), 0-based. -/
def click (s : Session a m) (row col : Nat) (button : MouseButton := .left) : Session a m :=
  (s.sendEvent (.mouse { kind := .press, button, row, col })).sendEvent
    (.mouse { kind := .release, button, row, col })

/-- Press at `from`, drag through to `to`, release there. -/
def drag (s : Session a m) (fromRow fromCol toRow toCol : Nat) : Session a m :=
  let s := s.sendEvent (.mouse { kind := .press, row := fromRow, col := fromCol })
  let s := s.sendEvent (.mouse { kind := .drag, row := toRow, col := toCol })
  s.sendEvent (.mouse { kind := .release, row := toRow, col := toCol })

/-- One wheel notch at (`row`, `col`): `up := false` scrolls down. -/
def scroll (s : Session a m) (row col : Nat) (up : Bool := false) : Session a m :=
  s.sendEvent (.mouse { kind := if up then .scrollUp else .scrollDown, button := .none, row, col })

/-- Same state, laid out at a new screen size. -/
def resize (s : Session a m) (width height : Nat) : Session a m :=
  { s with width, height }

/-- Send a list of keys in sequence. -/
def sendKeys (s : Session a m) (keys : List Key) : Session a m :=
  keys.foldl (fun acc k => acc.sendKey k) s

/-- Type a literal string as character keys. -/
def typeString (s : Session a m) (text : String) : Session a m :=
  s.sendKeys (text.toList.map Key.char)

/-- Has the app's quit condition fired? -/
def isQuit (s : Session a m) : Bool := s.app.quitWhen s.state

end Session

end LeanTea.Tui
