import LeanTea

/-! # examples/Tetris/Main.lean — Tetris in Lean, TUI flavour

A worked example showing what the framework's TEA shape buys you
once you bolt on **raw-mode stdin** + **concurrent tick gravity**.
The runtime that ships in `LeanTea.Runtime` is intentionally minimal
(line-buffered stdin, no timers) — Tetris demonstrates the next
step: a small inline extension that the framework will probably
absorb as `LeanTea.Tui.Raw` once the API settles.

## What ships here (~400 LOC, all in this file)

  * The seven canonical tetrominoes, each with their four rotations
  * A 10×20 board (the "Guideline" sizes)
  * `Model / Msg / update / view` split: pure game logic in
    `update`, terminal I/O confined to `main`
  * Raw-mode tty toggling via `stty(1)` (Unix only — works on macOS
    + Linux; Windows would need `SetConsoleMode` and is out of
    scope for the example)
  * A background tick task that pushes a `.tick` message every
    `gravityMs ms`, racing against single-key input on stdin
  * Standard scoring: 100 / 300 / 500 / 800 per 1 / 2 / 3 / 4
    lines, level-up every 10 lines

## Controls

  * ← / →   move left / right        (arrow keys, escape sequences)
  * ↑       rotate clockwise
  * ↓       soft drop one row
  * space   hard drop (slam to bottom)
  * `h`     hold-piece (swap with stash; once per piece)
  * `q`     quit

## Run it

```sh
lake build tetris
./.lake/build/bin/tetris
```

A failed game still calls `stty sane` on exit (via the wrapped
guard); if you Ctrl-C out and your terminal is left in raw mode,
`reset` puts it back. -/

open LeanTea

/-! ## Pieces -/

inductive Piece where
  | I | O | T | S | Z | J | L
  deriving Repr, BEq, Inhabited

namespace Piece

def all : List Piece := [.I, .O, .T, .S, .Z, .J, .L]

/-- ANSI 256-colour code for the block. Matches modern Tetris
    palette: cyan I, yellow O, magenta T, green S, red Z, blue J,
    orange L. -/
def color : Piece → Nat
  | .I => 51 | .O => 226 | .T => 201 | .S => 46
  | .Z => 196 | .J => 21 | .L => 208

/-- The four-cell shapes for each rotation. Each shape is a list of
    `(dx, dy)` offsets relative to the piece's pivot. Rotations are
    pre-baked — no rotation matrix at runtime. The pivots match the
    Super Rotation System spawn orientation. -/
def shapes : Piece → Array (List (Int × Int))
  -- I — horizontal bar, then column, then horizontal (lower), then column
  | .I => #[
      [(0,1), (1,1), (2,1), (3,1)],
      [(2,0), (2,1), (2,2), (2,3)],
      [(0,2), (1,2), (2,2), (3,2)],
      [(1,0), (1,1), (1,2), (1,3)]
    ]
  -- O — square; rotation is a no-op
  | .O => #[
      [(0,0), (1,0), (0,1), (1,1)],
      [(0,0), (1,0), (0,1), (1,1)],
      [(0,0), (1,0), (0,1), (1,1)],
      [(0,0), (1,0), (0,1), (1,1)]
    ]
  -- T
  | .T => #[
      [(0,1), (1,1), (2,1), (1,0)],
      [(1,0), (1,1), (1,2), (2,1)],
      [(0,1), (1,1), (2,1), (1,2)],
      [(1,0), (1,1), (1,2), (0,1)]
    ]
  -- S
  | .S => #[
      [(0,1), (1,1), (1,0), (2,0)],
      [(1,0), (1,1), (2,1), (2,2)],
      [(0,2), (1,2), (1,1), (2,1)],
      [(0,0), (0,1), (1,1), (1,2)]
    ]
  -- Z
  | .Z => #[
      [(0,0), (1,0), (1,1), (2,1)],
      [(2,0), (1,1), (2,1), (1,2)],
      [(0,1), (1,1), (1,2), (2,2)],
      [(1,0), (0,1), (1,1), (0,2)]
    ]
  -- J
  | .J => #[
      [(0,0), (0,1), (1,1), (2,1)],
      [(1,0), (2,0), (1,1), (1,2)],
      [(0,1), (1,1), (2,1), (2,2)],
      [(1,0), (1,1), (0,2), (1,2)]
    ]
  -- L
  | .L => #[
      [(2,0), (0,1), (1,1), (2,1)],
      [(1,0), (1,1), (1,2), (2,2)],
      [(0,1), (1,1), (2,1), (0,2)],
      [(0,0), (1,0), (1,1), (1,2)]
    ]

end Piece

/-! ## Model -/

abbrev Board := Array (Array (Option Piece))

def boardWidth  : Nat := 10
def boardHeight : Nat := 20

def emptyRow : Array (Option Piece) :=
  Array.replicate boardWidth Option.none

def emptyBoard : Board :=
  Array.replicate boardHeight emptyRow

structure Falling where
  piece     : Piece
  /-- Top-left corner of the 4×4 bounding box. -/
  x         : Int
  y         : Int
  /-- 0–3. -/
  rotation  : Nat
  deriving Repr, Inhabited

structure Model where
  board     : Board
  falling   : Falling
  next      : Piece
  hold      : Option Piece := Option.none
  /-- One hold per piece; reset on lock. -/
  canHold   : Bool := true
  score     : Nat := 0
  lines     : Nat := 0
  level     : Nat := 1
  gameOver  : Bool := false
  done      : Bool := false
  /-- Source of (deterministic) randomness for piece bag. -/
  rng       : Nat := 1
  deriving Inhabited

inductive Msg where
  | tick
  | left | right
  | rotateCw
  | softDrop
  | hardDrop
  | hold
  | quit
  | noop
  deriving Repr

/-! ## Random piece bag -/

/-- Linear-congruential PRNG. Plenty for piece-bag selection;
    deterministic so replays are reproducible. -/
private def lcgStep (s : Nat) : Nat := (s * 1103515245 + 12345) % 0x80000000

def Model.nextPiece (m : Model) : Piece × Nat :=
  let s := lcgStep m.rng
  let p := Piece.all[s % 7]!
  (p, s)

/-! ## Geometry helpers -/

def Falling.cells (f : Falling) : List (Int × Int) :=
  let offs := f.piece.shapes[f.rotation % 4]!
  offs.map fun (dx, dy) => (f.x + dx, f.y + dy)

def Model.cellAt (m : Model) (x y : Int) : Option Piece :=
  if x < 0 ∨ y < 0 ∨ x ≥ boardWidth ∨ y ≥ boardHeight then
    Option.some Piece.I    -- treat OOB as a wall
  else
    (m.board[y.toNat]!)[x.toNat]!

def Model.collides (m : Model) (f : Falling) : Bool :=
  f.cells.any fun (x, y) =>
    if y < 0 then false   -- spawn area above visible board
    else (m.cellAt x y).isSome

/-- Lock the current piece into the board (returns a new board). -/
def Model.lockBoard (m : Model) : Board := Id.run do
  let mut b := m.board
  for (x, y) in m.falling.cells do
    if y ≥ 0 && y < boardHeight && x ≥ 0 && x < boardWidth then
      let row := b[y.toNat]!.set! x.toNat (Option.some m.falling.piece)
      b := b.set! y.toNat row
  return b

/-- Find which rows in `b` are fully populated. -/
def fullRows (b : Board) : List Nat := Id.run do
  let mut acc : List Nat := []
  for i in [0:boardHeight] do
    if (b[i]!).all (·.isSome) then acc := i :: acc
  return acc.reverse

/-- Drop `rows` from the board; pad the top with empty rows. -/
def clearRows (b : Board) (rows : List Nat) : Board := Id.run do
  let mut survivors : Array (Array (Option Piece)) := #[]
  for i in [0:boardHeight] do
    if !rows.contains i then survivors := survivors.push b[i]!
  let dropped := boardHeight - survivors.size
  let mut out : Array (Array (Option Piece)) := #[]
  for _ in [:dropped] do out := out.push emptyRow
  for r in survivors do out := out.push r
  return out

def scoreForLines (n : Nat) (level : Nat) : Nat :=
  let base := match n with
    | 0 => 0 | 1 => 100 | 2 => 300 | 3 => 500 | _ => 800
  base * level

/-! ## Spawn / lock cycle -/

def Model.spawn (m : Model) : Model :=
  let f : Falling :=
    { piece := m.next, x := 3, y := -1, rotation := 0 }
  if m.collides f then { m with gameOver := true }
  else
    let (p2, s2) := m.nextPiece
    { m with falling := f, next := p2, rng := s2, canHold := true }

/-- Lock the piece into the board, clear filled rows, update score
    + level, and spawn the next piece. -/
def Model.lockAndAdvance (m : Model) : Model :=
  let b := m.lockBoard
  let full := fullRows b
  let b' := clearRows b full
  let cleared := full.length
  let newLines := m.lines + cleared
  let newLevel := max 1 ((newLines / 10) + 1)
  let m' : Model :=
    { m with board := b', lines := newLines, level := newLevel,
             score := m.score + scoreForLines cleared m.level }
  m'.spawn

/-! ## Update -/

private def shifted (f : Falling) (dx dy : Int) : Falling :=
  { f with x := f.x + dx, y := f.y + dy }

private def rotated (f : Falling) : Falling :=
  { f with rotation := (f.rotation + 1) % 4 }

def update : Msg → Model → Model × Cmd Msg
  | _, m@{ gameOver := true, .. } => (m, Cmd.none)
  | .quit,     m => ({ m with done := true }, Cmd.none)
  | .noop,     m => (m, Cmd.none)
  | .left,     m =>
    let f' := shifted m.falling (-1) 0
    if m.collides f' then (m, Cmd.none)
    else ({ m with falling := f' }, Cmd.none)
  | .right,    m =>
    let f' := shifted m.falling 1 0
    if m.collides f' then (m, Cmd.none)
    else ({ m with falling := f' }, Cmd.none)
  | .rotateCw, m =>
    let f' := rotated m.falling
    if m.collides f' then
      /- Wall-kick: try ±1 horizontal. -/
      let alt1 := shifted f' 1 0
      if !m.collides alt1 then ({ m with falling := alt1 }, Cmd.none)
      else
        let alt2 := shifted f' (-1) 0
        if !m.collides alt2 then ({ m with falling := alt2 }, Cmd.none)
        else (m, Cmd.none)
    else ({ m with falling := f' }, Cmd.none)
  | .softDrop, m =>
    let f' := shifted m.falling 0 1
    if m.collides f' then
      (m.lockAndAdvance, Cmd.none)
    else ({ m with falling := f', score := m.score + 1 }, Cmd.none)
  | .hardDrop, m => Id.run do
    /- Slam down until we collide; lock at the deepest legal pos. -/
    let mut f := m.falling
    let mut drops : Nat := 0
    while true do
      let f' := shifted f 0 1
      if m.collides f' then break
      f := f'
      drops := drops + 1
    return ({ m with falling := f, score := m.score + 2 * drops }.lockAndAdvance, Cmd.none)
  | .tick, m =>
    /- Gravity. Same as soft-drop but no per-row score and no lock-
       on-touch grace period. -/
    let f' := shifted m.falling 0 1
    if m.collides f' then (m.lockAndAdvance, Cmd.none)
    else ({ m with falling := f' }, Cmd.none)
  | .hold, m =>
    if !m.canHold then (m, Cmd.none)
    else
      let current := m.falling.piece
      match m.hold with
      | none =>
        /- Stash current, draw next; consume our hold token. -/
        let (p2, s2) := m.nextPiece
        let f : Falling := { piece := m.next, x := 3, y := -1, rotation := 0 }
        ({ m with hold := some current, falling := f, next := p2, rng := s2, canHold := false },
         Cmd.none)
      | some held =>
        let f : Falling := { piece := held, x := 3, y := -1, rotation := 0 }
        ({ m with hold := some current, falling := f, canHold := false }, Cmd.none)

/-! ## Rendering -/

namespace Tetris

/-- One-character mnemonic for the sidebar / next preview. -/
def pieceLabel : Piece → String
  | .I => "I"  | .O => "O"  | .T => "T"  | .S => "S"
  | .Z => "Z"  | .J => "J"  | .L => "L"

end Tetris

private def colorOn (c : Nat) : String := s!"\x1b[38;5;{c}m"
private def reset : String := "\x1b[0m"

/-- Block cell — two columns wide so the playfield is square-ish. -/
private def cellGlyph : Option Piece → String
  | none   => "  "
  | some p => colorOn p.color ++ "██" ++ reset

def view (m : Model) : String := Id.run do
  /- Compose a snapshot board with the falling piece overlaid. -/
  let mut board := m.board
  for (x, y) in m.falling.cells do
    if y ≥ 0 && y < boardHeight && x ≥ 0 && x < boardWidth then
      board := board.set! y.toNat
        ((board[y.toNat]!).set! x.toNat (some m.falling.piece))
  let mut out := "\x1b[2J\x1b[H"          -- clear + home
  out := out ++ "LeanTEA Tetris\n"
  out := out ++ s!"score {m.score}   lines {m.lines}   level {m.level}\n\n"
  let topBar := "╔" ++ String.mk (List.replicate (boardWidth * 2) '═') ++ "╗"
  out := out ++ topBar ++ "    next:  " ++ Tetris.pieceLabel m.next ++ "\n"
  for y in [0:boardHeight] do
    out := out ++ "║"
    for x in [0:boardWidth] do
      out := out ++ cellGlyph (board[y]!)[x]!
    out := out ++ "║"
    /- Sidebar lines. -/
    match y with
    | 0 => out := out ++ "    hold:  " ++
                  (m.hold.map Tetris.pieceLabel |>.getD "—")
    | 2 => out := out ++ "    controls:"
    | 3 => out := out ++ "      ← →  move"
    | 4 => out := out ++ "      ↑    rotate"
    | 5 => out := out ++ "      ↓    soft drop"
    | 6 => out := out ++ "      ␣    hard drop"
    | 7 => out := out ++ "      h    hold"
    | 8 => out := out ++ "      q    quit"
    | _ => pure ()
    out := out ++ "\n"
  let botBar := "╚" ++ String.mk (List.replicate (boardWidth * 2) '═') ++ "╝"
  out := out ++ botBar ++ "\n"
  if m.gameOver then out := out ++ "\n*** GAME OVER ***   press q to exit\n"
  return out

/-! ## Raw-mode TTY helpers

`stty(1)` is on every macOS + Linux dev box. We:

  * Save the current settings (`stty -g`)
  * Switch to raw mode (`-icanon -echo`) on startup
  * Restore on exit (handles normal quit; Ctrl-C still ungraceful
    — the operator can `reset` to recover)

Reading from stdin then returns single bytes; arrow keys arrive as
3-byte ESC sequences (`\x1b[A`, `\x1b[B`, etc.) which the input
parser handles below. -/

namespace Tty

/-- stty with inherited stdin so it sees the parent's terminal.
    `IO.Process.output` connects stdin to /dev/null, which makes
    stty silently no-op against a non-tty fd — that's the
    previous bug that ate every keypress. -/
private def sttyInherit (args : Array String) : IO Unit := do
  let child ← IO.Process.spawn {
    cmd := "stty", args,
    stdin := .inherit, stdout := .inherit, stderr := .inherit
  }
  let _ ← child.wait

/-- stty `-g` captures the current settings (so we can restore on
    exit). Stdin is inherited; stdout is piped so we can read the
    saved-settings string. -/
private def sttySaveCurrent : IO String := do
  let child ← IO.Process.spawn {
    cmd := "stty", args := #["-g"],
    stdin := .inherit, stdout := .piped, stderr := .inherit
  }
  let out ← child.stdout.readToEnd
  let _ ← child.wait
  return out.trimAscii.toString

def saveAndRaw : IO String := do
  let saved ← sttySaveCurrent
  sttyInherit #["-icanon", "-echo"]
  return saved

def restore (saved : String) : IO Unit :=
  sttyInherit #[saved]

/-- Read one byte from stdin and return it. Blocks until a key is
    pressed. -/
def readByte : IO (Option UInt8) := do
  let h ← IO.getStdin
  let bs ← h.read 1
  if bs.size == 0 then return none
  return some (bs.get! 0)

end Tty

/-- Map a raw byte sequence to a `Msg`. Arrow keys are 3 bytes
    starting with `\x1b[`; everything else is a single key. -/
def parseInput (h : IO.FS.Stream) : IO Msg := do
  let b1 ← h.read 1
  if b1.size == 0 then return Msg.noop
  let c := b1.get! 0
  if c == 0x1b then
    /- Possible ESC sequence — read two more bytes (best-effort). -/
    let _ ← h.read 1
    let b3 ← h.read 1
    if b3.size == 0 then return Msg.quit
    let code := b3.get! 0
    return (match code with
      | 0x41 /- A -/ => Msg.rotateCw
      | 0x42 /- B -/ => Msg.softDrop
      | 0x43 /- C -/ => Msg.right
      | 0x44 /- D -/ => Msg.left
      | _ => Msg.noop)
  else if c == 0x71 /- q -/ then return Msg.quit
  else if c == 0x68 /- h -/ then return Msg.hold
  else if c == 0x20 /- space -/ then return Msg.hardDrop
  else if c == 0x6a /- j (soft drop alias) -/ then return Msg.softDrop
  else if c == 0x6c /- l (right alias) -/ then return Msg.right
  else if c == 0x6b /- k (rotate alias) -/ then return Msg.rotateCw
  else if c == 0x73 /- s (left alias for ergonomic right-hand) -/ then return Msg.softDrop
  else return Msg.noop

/-! ## Main loop

Two concurrent sources feed `update`:

  1. **Stdin** — `parseInput` on the main thread. Blocks until a
     keypress.
  2. **Tick** — a background `IO.asTask` sleeps `gravityMs` then
     posts a `tick` to a shared queue.

We multiplex them by polling the queue between renders. -/

def gravityMs (level : Nat) : Nat :=
  /- Standard "every level shaves 50ms, floor at 100". -/
  match level with
  | 0 | 1 => 800
  | 2 => 720
  | 3 => 630
  | 4 => 550
  | 5 => 470
  | 6 => 380
  | 7 => 300
  | 8 => 220
  | 9 => 130
  | _ => 100

/-- Atomically push a message into the shared queue. `IO.Ref.modify`
    is atomic per-call in Lean 4, so two background tasks appending
    is race-free. -/
private def pushMsg (queue : IO.Ref (Array Msg)) (m : Msg) : IO Unit :=
  queue.modify (·.push m)

/-- Background tick task: pushes `Msg.tick` into the shared queue
    every `gravityMs ms`. Stops when `done` flag is set. -/
private partial def tickLoop (queue : IO.Ref (Array Msg)) (done : IO.Ref Bool)
    (level : IO.Ref Nat) : IO Unit := do
  let isDone ← done.get
  if isDone then return
  let lv ← level.get
  IO.sleep (gravityMs lv).toUInt32
  pushMsg queue Msg.tick
  tickLoop queue done level

/-- Background input task: pushes parsed keypresses into the queue. -/
private partial def inputLoop (queue : IO.Ref (Array Msg)) (done : IO.Ref Bool) : IO Unit := do
  let isDone ← done.get
  if isDone then return
  let h ← IO.getStdin
  let m ← parseInput h
  pushMsg queue m
  inputLoop queue done

/-- Drain the message queue: swap in an empty array and return the
    previous contents. Two separate `.modify` calls would race; the
    `swap` shape inside one `modifyGet` is atomic. -/
private def drainQueue (queue : IO.Ref (Array Msg)) : IO (Array Msg) :=
  queue.modifyGet (fun q => (q, #[]))

partial def gameLoop
    (queue : IO.Ref (Array Msg))
    (doneRef : IO.Ref Bool)
    (levelRef : IO.Ref Nat)
    (m : Model) : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStr (view m)
  stdout.flush
  if m.done then
    doneRef.set true
    return ()
  /- Wait briefly so the loop doesn't busy-spin. The two tasks
     above will push messages into the queue. -/
  IO.sleep 20
  let msgs ← drainQueue queue
  /- Fold every queued message through update. -/
  let mut m' := m
  for msg in msgs do
    let (mm, _cmd) := update msg m'
    m' := mm
  /- Sync the gravity ref so the tick loop picks up the new level. -/
  levelRef.set m'.level
  gameLoop queue doneRef levelRef m'

def main : IO Unit := do
  /- Seed the RNG and spawn the first piece. -/
  let seed ← IO.monoMsNow
  let initRng := seed % 0x80000000
  let (firstPiece, s1) := (Piece.all[initRng % 7]!, lcgStep initRng)
  let (nextPiece, s2) := (Piece.all[s1 % 7]!, lcgStep s1)
  let model0 : Model := {
    board := emptyBoard,
    falling := { piece := firstPiece, x := 3, y := -1, rotation := 0 },
    next := nextPiece,
    rng := s2
  }
  /- Set raw mode; restore on exit. -/
  let saved ← Tty.saveAndRaw
  let queue ← IO.mkRef (#[] : Array Msg)
  let doneRef ← IO.mkRef false
  let levelRef ← IO.mkRef model0.level
  let _ ← IO.asTask (prio := Task.Priority.dedicated)
            (tickLoop queue doneRef levelRef)
  let _ ← IO.asTask (prio := Task.Priority.dedicated)
            (inputLoop queue doneRef)
  try
    gameLoop queue doneRef levelRef model0
  catch e =>
    IO.eprintln s!"tetris crashed: {e}"
  Tty.restore saved
  /- Make sure the cursor is visible + at the bottom of the board. -/
  let h ← IO.getStdout
  h.putStr "\x1b[?25h\n"
  h.flush
