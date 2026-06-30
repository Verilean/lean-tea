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
  let w := s.app.view s.state
  w.render s.width s.height (s.focused == w.focusId)

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

/-- Cycle one position forward through a focus list. Returns the
    first element when `cur` isn't found or is empty. -/
private partial def cycleFwd (order : List String) (cur : String) : String :=
  match order with
  | [] => ""
  | first :: _ =>
    if cur.isEmpty then first
    else
      let rec go : List String → String
        | []      => first
        | [_]     => first
        | a :: b :: rest =>
          if a == cur then b else go (b :: rest)
      go order

/-- Send a single key and return the new Session. Same dispatch
    logic as `App.runWith` but pure — no IO. -/
def sendKey (s : Session a m) (key : Key) : Session a m :=
  match key with
  | .tab      => { s with focused := cycleFwd s.focusOrder s.focused }
  | .shiftTab => { s with focused := cycleFwd s.focusOrder.reverse s.focused }
  | _ =>
    let w := s.app.view s.state
    match w.dispatchKey s.focused key with
    | none     => s
    | some msg => { s with state := s.app.update msg s.state }

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
