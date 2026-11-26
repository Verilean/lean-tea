/-! # LeanTea.Net.Sock — blocking POSIX stream sockets (Unix and TCP)

Bindings over `c/leantea_sock.c`. `Std.Async` has no Unix sockets, and
its TCP sockets cannot be closed while a send waits for a peer that
stopped reading. With these descriptors `shutdown` from another thread
wakes a thread blocked in `send` or `recv`, so a relay can always end.

Every call that waits (`accept`, `recv`, `send`) parks an OS thread, so
run them on dedicated tasks (`IO.asTask (prio := .dedicated)`), as
`LeanTea.Net.Server.serveUnix` and `serveThreads` do. Descriptors are
plain `UInt32`s: close them yourself (`Transport.ofFd` does). -/

namespace LeanTea.Net.Sock

/-- Bind a stream socket at `path` with file mode `mode` and listen. A
    stale socket at `path` is replaced; any other file there is an error.
    The mode is set before listening, so nobody connects through a looser
    mode first. -/
@[extern "lean_sock_listen_unix"]
opaque listenUnix (path : @& String) (mode : UInt32) : IO UInt32

@[extern "lean_sock_connect_unix"]
opaque connectUnix (path : @& String) : IO UInt32

/-- Bind and listen on an IPv4 address (dotted quad). -/
@[extern "lean_sock_listen_tcp"]
opaque listenTcp (host : @& String) (port : UInt16) : IO UInt32

@[extern "lean_sock_connect_tcp"]
opaque connectTcp (host : @& String) (port : UInt16) : IO UInt32

@[extern "lean_sock_accept"]
opaque accept (fd : UInt32) : IO UInt32

/-- One receive of up to `max` bytes. Waits at most `timeoutMs` (0: no
    limit) and then fails. Empty: the peer closed. -/
@[extern "lean_sock_recv"]
opaque recv (fd : UInt32) (max : UInt32) (timeoutMs : UInt32) : IO ByteArray

/-- Send everything. Fails if the peer is gone, or (with a send timeout)
    when it reads nothing for that long. -/
@[extern "lean_sock_send"]
opaque send (fd : UInt32) (bytes : @& ByteArray) : IO Unit

/-- Make a `send` that cannot progress for `ms` fail (0: no limit). -/
@[extern "lean_sock_set_send_timeout"]
opaque setSendTimeout (fd : UInt32) (ms : UInt32) : IO Unit

/-- `how`: 0 reading, 1 writing, 2 both. Wakes threads blocked on `fd`. -/
@[extern "lean_sock_shutdown"]
opaque shutdown (fd : UInt32) (how : UInt8) : IO Unit

@[extern "lean_sock_close"]
opaque close (fd : UInt32) : IO Unit

/-- The user id of the process at the other end of a Unix socket. -/
@[extern "lean_sock_peer_uid"]
opaque peerUid (fd : UInt32) : IO UInt32

end LeanTea.Net.Sock
