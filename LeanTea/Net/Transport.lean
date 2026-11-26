import Std.Async.TCP
import Std.Async.Timer
import Std.Sync.CancellationToken
import LeanTea.Net.Unix

/-! # LeanTea.Net.Transport — one connected byte stream, TCP or Unix

The HTTP server loop, the WebSocket server and the proxy read and write
through this, so the same code serves a TCP socket (libuv) and a Unix
domain socket (`LeanTea.Net.Unix`, blocking). -/

namespace LeanTea.Net

open Std.Async
open Std.Async.TCP

structure Transport where
  /-- One receive. `none` when the peer closed, on an error, or once
      `deadline` (monotonic ms, `IO.monoMsNow`; 0 = no deadline) passes. -/
  recv     : (deadline : Nat) → IO (Option ByteArray)
  send     : ByteArray → IO Unit
  /-- Stop sending; the peer then reads end of stream. -/
  shutdownWrite : IO Unit
  /-- Done with the connection, both ways. -/
  close    : IO Unit
  /-- The user id of the peer, on a Unix socket. -/
  peerUid  : Option Nat := none

namespace Transport

/-- A TCP connection accepted or opened through `Std.Async.TCP`.

    `close` cancels a token every `recv` also waits on, so a task waiting
    to receive returns `none` at once instead of waiting for the peer: a
    relay that closes both ends when one side finishes relies on it.
    (`Std.Async.TCP` has no close; the shutdown ends sending, and the
    socket itself goes when it is no longer referenced.) -/
def ofTcp (client : Socket.Client) : IO Transport := do
  let closed ← Std.CancellationToken.new
  return {
    recv := fun deadline => do
      if ← closed.isCancelled then return none
      try
        let now ← IO.monoMsNow
        if deadline != 0 && now ≥ deadline then return none
        let wait : Async (Option ByteArray) := do
          let stop := Selectable.case closed.selector (fun _ => pure none)
          let data := Selectable.case (client.recvSelector 65536) pure
          if deadline == 0 then
            Selectable.one #[data, stop]
          else
            let timer ← Selector.sleep (Std.Time.Millisecond.Offset.ofNat (deadline - now))
            -- The selector cancels whichever cases lose.
            Selectable.one #[data, stop, .case timer (fun _ => pure none)]
        wait.block
      catch _ => return none
    send := fun b => do
      if ← closed.isCancelled then throw (IO.userError "send: connection closed")
      (client.send b).block
    shutdownWrite := try (client.shutdown).block catch _ => pure ()
    close := do
      if ← closed.isCancelled then return
      closed.cancel
      try (client.shutdown).block catch _ => pure () }

/-- State of a Unix descriptor shared by `recv`, `send` and `close`. -/
private structure FdState where
  closed   : Bool := false
  users    : Nat := 0
  released : Bool := false

/-- A connected Unix socket descriptor; `close` releases it.

    `close` may run while another task waits in `recv` (a relay closes
    both ends when one side finishes). It shuts the socket down, which
    wakes that reader, and the descriptor is released only once no
    task is inside `recv` or `send`: a descriptor number closed under a
    waiting task could be reused by another connection. -/
def ofUnix (fd : UInt32) (peerUid : Option Nat := none) : IO Transport := do
  let st ← IO.mkRef ({} : FdState)
  let release : IO Unit := do
    let go ← st.modifyGet fun s =>
      if s.closed && s.users == 0 && !s.released then (true, { s with released := true })
      else (false, s)
    if go then try Unix.close fd catch _ => pure ()
  -- `recv` and `send` hold the descriptor between `enter` and `leave`.
  let enter : IO Bool := st.modifyGet fun s =>
    if s.closed then (false, s) else (true, { s with users := s.users + 1 })
  let leave : IO Unit := do
    st.modify fun s => { s with users := s.users - 1 }
    release
  let recvOnce (deadline : Nat) : IO (Option ByteArray) := do
    let timeout ← if deadline == 0 then pure 0 else do
      let now ← IO.monoMsNow
      pure (if now ≥ deadline then 0 else deadline - now)
    if deadline != 0 && timeout == 0 then return none
    try
      let b ← Unix.recv fd 65536 timeout.toUInt32
      return if b.isEmpty then none else some b
    catch _ => return none
  return {
    recv := fun deadline => do
      if !(← enter) then return none
      try recvOnce deadline finally leave
    send := fun b => do
      if !(← enter) then throw (IO.userError "send: connection closed")
      try Unix.send fd b finally leave
    shutdownWrite := do
      if ← enter then
        try Unix.shutdown fd 1 catch _ => pure ()
        leave
    close := do
      if ← st.modifyGet (fun s => (s.closed, { s with closed := true })) then return
      try Unix.shutdown fd 2 catch _ => pure ()
      release
    peerUid }

/-- Connect to the Unix socket at `path`. -/
def connectUnix (path : String) : IO Transport := do
  ofUnix (← Unix.connect path)

end Transport

end LeanTea.Net
