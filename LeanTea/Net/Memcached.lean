import Std.Async.TCP
import Std.Net
import LeanTea.Persist.Backend

/-! # Memcached text-protocol client

A minimal client implementing the four operations the framework
needs to turn memcached into a `LeanTea.Persist.Cache`:

  * `get key`
  * `set key value [exptime]`
  * `delete key`
  * `flushAll`

Built directly on `Std.Internal.Async.TCP`, no FFI required. The
text protocol is documented at
<https://github.com/memcached/memcached/blob/master/doc/protocol.txt>;
we implement only the lines needed for cache-aside.

The `Client.asCache` adapter feeds memcached straight into the
`Backend.cached` decorator. Memcached caps keys at 250 bytes and
disallows whitespace / control bytes, so the adapter hashes the
incoming key (FNV-1a 64) to a 16-char hex string before sending. -/

namespace LeanTea.Net.Memcached

open Std.Async
open Std.Net
open Std.Async.TCP

structure Client where
  socket : Socket.Client

/-! ## Address parsing — copied from Net/Server.lean so this module
    doesn't drag in the whole server runtime as a dep. -/

private def parseIPv4 (s : String) : IPv4Addr :=
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  match (s.splitOn ".").map (·.toNat?.getD 0) with
  | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
  | _ => zero

def connect (host : String := "127.0.0.1") (port : UInt16 := 11211) : IO Client := do
  let socket ← Socket.Client.mk
  let addr : SocketAddress := .v4 { addr := parseIPv4 host, port := port }
  (socket.connect addr).block
  return { socket }

def Client.close (c : Client) : IO Unit :=
  (c.socket.shutdown).block

/-! ## Wire I/O -/

private def Client.send (c : Client) (bs : ByteArray) : IO Unit :=
  (c.socket.send bs).block

/-- Read chunks until `acc` ends with `terminator` (or EOF). Memcached
    responses are always line-oriented and terminate with `\r\n`-based
    sentinels, so this is enough. -/
private partial def Client.recvUntil
    (c : Client) (acc : ByteArray) (terminator : ByteArray) : IO ByteArray := do
  if acc.size ≥ terminator.size then
    let tail := acc.extract (acc.size - terminator.size) acc.size
    if tail == terminator then return acc
  match (← (c.socket.recv? 4096).block) with
  | none       => return acc
  | some chunk => c.recvUntil (acc ++ chunk) terminator

/-- ByteArray index-of, returning the offset of the first occurrence
    of `needle` in `hay`, or `none`. -/
private partial def indexOf? (hay needle : ByteArray) : Option Nat :=
  if needle.size == 0 || hay.size < needle.size then none else
  let rec go (i : Nat) : Option Nat :=
    if i + needle.size > hay.size then none
    else
      let slice := hay.extract i (i + needle.size)
      if slice == needle then some i else go (i + 1)
  go 0

/-! ## Operations -/

/-- `get` returns the stored value or `none` if the key is absent. -/
def Client.get (c : Client) (key : String) : IO (Option String) := do
  c.send (s!"get {key}\r\n".toUTF8)
  let endMarker := "END\r\n".toUTF8
  let bs ← c.recvUntil .empty endMarker
  let valuePrefix := "VALUE ".toUTF8
  if bs.size < valuePrefix.size then return none
  if bs.extract 0 valuePrefix.size != valuePrefix then return none
  -- Parse the VALUE header line: "VALUE <key> <flags> <bytes>\r\n"
  let crlf := "\r\n".toUTF8
  match indexOf? bs crlf with
  | none => return none
  | some headerEnd =>
    let header := String.fromUTF8! (bs.extract 0 headerEnd)
    -- header == "VALUE <key> <flags> <bytes>"
    let parts := (header.splitOn " ")
    match parts.reverse with
    | bytesS :: _ =>
      match bytesS.toNat? with
      | none   => return none
      | some n =>
        let dataStart := headerEnd + crlf.size
        if dataStart + n > bs.size then return none
        let data := bs.extract dataStart (dataStart + n)
        return some (String.fromUTF8! data)
    | _ => return none

/-- `set` writes `value` for `key` with `exptime` seconds of TTL
    (0 = never expire from this client's perspective, subject to
    server LRU). -/
def Client.set (c : Client) (key value : String) (exptime : Nat := 0) : IO Unit := do
  let bs := value.toUTF8
  let header := s!"set {key} 0 {exptime} {bs.size}\r\n".toUTF8
  c.send (header ++ bs ++ "\r\n".toUTF8)
  let _ ← c.recvUntil .empty "\r\n".toUTF8
  return ()

def Client.delete (c : Client) (key : String) : IO Unit := do
  c.send (s!"delete {key}\r\n".toUTF8)
  let _ ← c.recvUntil .empty "\r\n".toUTF8
  return ()

def Client.flushAll (c : Client) : IO Unit := do
  c.send "flush_all\r\n".toUTF8
  let _ ← c.recvUntil .empty "\r\n".toUTF8
  return ()

/-! ## Cache adapter -/

private def fnvHex (s : String) : String := Id.run do
  let n := LeanTea.Persist.Backend.fnv1a64 s
  let digits := "0123456789abcdef".toList.toArray
  let mut out : String := ""
  let mut x : UInt64 := n
  for _ in [:16] do
    let nibble : Nat := (x % 16).toNat
    out := (digits[nibble]!.toString) ++ out
    x := x / 16
  return out

/-- Bridge memcached to the `Cache` interface used by
    `Backend.cached`. The incoming cache key is hashed to a
    16-character hex string so it satisfies memcached's 250-byte +
    no-whitespace restriction. -/
def Client.asCache (c : Client) : LeanTea.Persist.Cache := {
  get    := fun k => c.get (fnvHex k),
  set    := fun k v => c.set (fnvHex k) v,
  delete := fun k => c.delete (fnvHex k),
  clear  := c.flushAll
}

end LeanTea.Net.Memcached
