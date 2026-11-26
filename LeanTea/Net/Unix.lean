/-! # LeanTea.Net.Unix — Unix domain stream sockets

Blocking bindings over `c/leantea_unix.c` (`Std.Async` has no Unix
sockets). Every call that waits (`accept`, `recv`) parks an OS thread,
so run them on dedicated tasks (`IO.asTask (prio := .dedicated)`), as
`LeanTea.Net.Server.serveUnix` does. Descriptors are plain `UInt32`s:
close them yourself. -/

namespace LeanTea.Net.Unix

/-- Bind a stream socket at `path` with file mode `mode` and listen. A
    stale socket at `path` is replaced; any other file there is an error.
    The mode is set before listening, so nobody connects through a looser
    mode first. -/
@[extern "lean_unix_listen"]
opaque listen (path : @& String) (mode : UInt32) : IO UInt32

@[extern "lean_unix_accept"]
opaque accept (fd : UInt32) : IO UInt32

@[extern "lean_unix_connect"]
opaque connect (path : @& String) : IO UInt32

/-- One receive of up to `max` bytes. Waits at most `timeoutMs` (0: no
    limit) and then fails. Empty: the peer closed. -/
@[extern "lean_unix_recv"]
opaque recv (fd : UInt32) (max : UInt32) (timeoutMs : UInt32) : IO ByteArray

@[extern "lean_unix_send"]
opaque send (fd : UInt32) (bytes : @& ByteArray) : IO Unit

/-- `how`: 0 reading, 1 writing, 2 both. -/
@[extern "lean_unix_shutdown"]
opaque shutdown (fd : UInt32) (how : UInt8) : IO Unit

@[extern "lean_unix_close"]
opaque close (fd : UInt32) : IO Unit

/-- The user id of the process at the other end of a connected socket. -/
@[extern "lean_unix_peer_uid"]
opaque peerUid (fd : UInt32) : IO UInt32

end LeanTea.Net.Unix
