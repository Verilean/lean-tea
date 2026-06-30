import Lean.Data.Json

/-! # examples/MetaOrchestrator/Zellij.lean — thin wrapper over `zellij action`

The orchestrator drives a *target pane* (typically the one running
`claude` / Claude Code) by shelling out to `zellij action` and
parsing its output. We never hold a long-lived connection — each
operation is one fork/exec, which keeps the wrapper deterministic
and observable. Zellij itself is the persistent state.

## Pane targeting

`zellij action` subcommands all take `--pane-id <PANE_ID>`. The id
is what `zellij` calls a *stable* id: `terminal_3`, `plugin_2`, or
the bare integer (`3` is equivalent to `terminal_3`).

To find the id you want at startup, the user typically calls
`zellij list-aliases` once and pins it via `--target-pane terminal_X`
to our exe. Auto-detection (active pane id) is a follow-up.

## Why we don't use the Zellij plugin API

A Wasm plugin would be more "native" but locks us out of using the
existing `LeanTea.Cloud.Gemini` (which calls curl) and the SQLite
state store. The shell-out cost (~10ms / call) is negligible at
poll intervals measured in seconds.
-/

namespace MetaOrchestrator.Zellij

/-- Dump the *visible viewport* of a pane (no scrollback) to stdout
    without ANSI escapes. Result is the plain-text screen. Empty
    string on failure. -/
def dumpScreen (paneId : String) : IO String := do
  let out ← IO.Process.output {
    cmd := "zellij"
    args := #["action", "dump-screen", "--pane-id", paneId]
  }
  if out.exitCode == 0 then return out.stdout
  else
    IO.eprintln s!"zellij dump-screen failed (exit {out.exitCode}): {out.stderr}"
    return ""

/-- Same as `dumpScreen` but includes the full scrollback. -/
def dumpScreenFull (paneId : String) : IO String := do
  let out ← IO.Process.output {
    cmd := "zellij"
    args := #["action", "dump-screen", "--pane-id", paneId, "--full"]
  }
  if out.exitCode == 0 then return out.stdout
  else
    IO.eprintln s!"zellij dump-screen --full failed (exit {out.exitCode}): {out.stderr}"
    return ""

/-- Type a string into the target pane (as if the user typed it).
    Does NOT press Enter — call `pressEnter` separately for that
    so we can pipeline multi-line input. -/
def writeChars (paneId : String) (text : String) : IO Unit := do
  let out ← IO.Process.output {
    cmd := "zellij"
    args := #["action", "write-chars", "--pane-id", paneId, text]
  }
  if out.exitCode != 0 then
    IO.eprintln s!"zellij write-chars failed (exit {out.exitCode}): {out.stderr}"

/-- Send a single raw byte (e.g. `\r` for Enter, `\u{1b}` for Esc).
    `zellij action write` takes bytes as separate decimal args. -/
def writeBytes (paneId : String) (bytes : List UInt8) : IO Unit := do
  let byteArgs := bytes.map (toString ·.toNat) |>.toArray
  let out ← IO.Process.output {
    cmd := "zellij"
    args := #["action", "write", "--pane-id", paneId] ++ byteArgs
  }
  if out.exitCode != 0 then
    IO.eprintln s!"zellij write failed (exit {out.exitCode}): {out.stderr}"

/-- Press Enter in the target pane. Use after `writeChars` to submit. -/
def pressEnter (paneId : String) : IO Unit :=
  writeBytes paneId [0x0d]  -- CR

/-- Type `text` then Enter. The most common combo. -/
def submit (paneId : String) (text : String) : IO Unit := do
  writeChars paneId text
  pressEnter paneId

/-- A FNV-1a hash of the pane text. We use it for cheap stall detection
    (same hash N polls in a row = nothing's happening). Not cryptographic. -/
def cheapHash (s : String) : UInt64 := Id.run do
  let mut h : UInt64 := 0xcbf29ce484222325
  for c in s.toList do
    h := h ^^^ c.toNat.toUInt64
    h := h * 0x100000001b3
  return h

end MetaOrchestrator.Zellij
