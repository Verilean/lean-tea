import Std.Internal.Async.TCP
import Std.Net
import LeanTea.Persist.Backend

/-! # LeanTea.Net.Valkey — pure-Lean Valkey / Redis client (RESP-2)

Valkey is the open-source fork of Redis maintained by the Linux
Foundation; both speak the same wire protocol (RESP), so this
client talks to either. Built directly on `Std.Internal.Async.TCP`
— no FFI required.

We implement just the slice of RESP-2 the framework needs for
Cache and counter use cases:

  * `PING`
  * `GET key`
  * `SET key value [EX seconds]`
  * `DEL key …`
  * `INCR key` / `INCRBY key delta`
  * `FLUSHDB`

RESP-3 (`HELLO`, attribute frames, push frames) is out of scope —
RESP-2 is what every Redis 5+ and every Valkey understands by
default.

`Client.asCache` plugs the connection straight into the
`LeanTea.Persist.Cache` shape used by `Backend.cached`. -/

namespace LeanTea.Net.Valkey

open Std.Internal.IO Async
open Std.Net
open Std.Internal.IO.Async.TCP

structure Client where
  socket : Socket.Client

/-! ## Address parsing — same shape as `Net.Memcached`. -/

private def parseIPv4 (s : String) : IPv4Addr :=
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  match (s.splitOn ".").map (·.toNat?.getD 0) with
  | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
  | _ => zero

def connect (host : String := "127.0.0.1") (port : UInt16 := 6379) : IO Client := do
  let socket ← Socket.Client.mk
  let addr : SocketAddress := .v4 { addr := parseIPv4 host, port }
  (socket.connect addr).block
  return { socket }

def Client.close (c : Client) : IO Unit :=
  (c.socket.shutdown).block

/-! ## Wire I/O -/

private def Client.send (c : Client) (bs : ByteArray) : IO Unit :=
  (c.socket.send bs).block

/-- Read up to `n` more bytes into `acc`, returning what we have. -/
private partial def Client.recvAtLeast
    (c : Client) (acc : ByteArray) (n : Nat) : IO ByteArray := do
  if acc.size ≥ n then return acc
  match (← (c.socket.recv? 4096).block) with
  | none       => return acc
  | some chunk => c.recvAtLeast (acc ++ chunk) n

/-- ByteArray index-of (first occurrence). -/
private partial def indexOf? (hay needle : ByteArray) : Option Nat :=
  if needle.size == 0 || hay.size < needle.size then none else
  let rec go (i : Nat) : Option Nat :=
    if i + needle.size > hay.size then none
    else
      let slice := hay.extract i (i + needle.size)
      if slice == needle then some i else go (i + 1)
  go 0

private def CRLF : ByteArray := "\r\n".toUTF8

/-- Read up to the next CRLF terminator, returning the line (without
    the terminator) and the buffered tail past CRLF. -/
private partial def Client.recvLine (c : Client) (acc : ByteArray)
    : IO (ByteArray × ByteArray) := do
  match indexOf? acc CRLF with
  | some i =>
    let line := acc.extract 0 i
    let rest := acc.extract (i + 2) acc.size
    return (line, rest)
  | none =>
    match (← (c.socket.recv? 4096).block) with
    | none       => return (acc, .empty)
    | some chunk => c.recvLine (acc ++ chunk)

/-! ## RESP request encoder

We use the **inline command** form for the most common ops (PING /
GET / DEL / INCR …) because Valkey accepts it and it keeps the
client tiny. For SET we use the **multi-bulk array** form so the
value can contain spaces, newlines, etc. -/

/-- Encode an Array of strings as a RESP-2 multi-bulk array. -/
private def encodeMulti (parts : Array String) : ByteArray := Id.run do
  let mut out := s!"*{parts.size}\r\n".toUTF8
  for p in parts do
    let bs := p.toUTF8
    out := out ++ s!"${bs.size}\r\n".toUTF8 ++ bs ++ "\r\n".toUTF8
  return out

/-! ## RESP response parser

We only need three reply shapes:

  * `+OK\r\n`            — simple string
  * `-ERR …\r\n`         — error (we propagate as `IO.userError`)
  * `:N\r\n`             — integer
  * `$N\r\n<N bytes>\r\n` — bulk string (or `$-1\r\n` for null)

Anything else triggers a "RESP: unexpected reply" error so callers
notice quickly when we point the client at the wrong port. -/

inductive Reply where
  | simple (s : String)
  | integer (n : Int)
  | bulk (s : Option String)         -- none = `$-1` (nil)
  | error (msg : String)
  deriving Repr

/-- Read one RESP reply from the wire. Leaves any trailing bytes in
    the returned ByteArray for the next call. -/
partial def Client.readReply (c : Client) (acc : ByteArray) : IO (Reply × ByteArray) := do
  let (line, rest) ← c.recvLine acc
  if line.size == 0 then
    throw <| IO.userError "Valkey: empty reply"
  let tag := line.get! 0
  let body := String.fromUTF8! (line.extract 1 line.size)
  if tag == '+'.toUInt8 then
    return (Reply.simple body, rest)
  else if tag == '-'.toUInt8 then
    return (Reply.error body, rest)
  else if tag == ':'.toUInt8 then
    match body.toInt? with
    | some n => return (Reply.integer n, rest)
    | none   => throw <| IO.userError s!"Valkey: bad integer `{body}`"
  else if tag == '$'.toUInt8 then
    match body.toInt? with
    | none   => throw <| IO.userError s!"Valkey: bad bulk header `{body}`"
    | some n =>
      if n < 0 then return (Reply.bulk none, rest)
      let want : Nat := n.toNat + 2     -- payload + trailing CRLF
      let full ← c.recvAtLeast rest want
      if full.size < want then
        throw <| IO.userError "Valkey: short bulk"
      let data := full.extract 0 n.toNat
      let tail := full.extract want full.size
      return (Reply.bulk (some (String.fromUTF8! data)), tail)
  else
    throw <| IO.userError s!"Valkey: unexpected reply tag `{Char.ofNat tag.toNat}{body}`"

/-! ## High-level ops -/

/-- Send one command and read the reply. Convenience for the
    one-call/one-reply commands; not for pipelining. -/
private def Client.command (c : Client) (parts : Array String) : IO Reply := do
  c.send (encodeMulti parts)
  let (r, _tail) ← c.readReply .empty
  return r

/-- `PING` returns "PONG" on a healthy connection. -/
def Client.ping (c : Client) : IO String := do
  match ← c.command #["PING"] with
  | .simple s    => return s
  | .bulk (some s) => return s
  | .error e     => throw <| IO.userError s!"PING: {e}"
  | r            => throw <| IO.userError s!"PING: unexpected reply {repr r}"

/-- `GET key` — returns `none` when the key is absent. -/
def Client.get (c : Client) (key : String) : IO (Option String) := do
  match ← c.command #["GET", key] with
  | .bulk x      => return x
  | .error e     => throw <| IO.userError s!"GET {key}: {e}"
  | r            => throw <| IO.userError s!"GET {key}: unexpected reply {repr r}"

/-- `SET key value [EX seconds]`. `ttl=0` means no expiry. -/
def Client.set (c : Client) (key value : String) (ttl : Nat := 0) : IO Unit := do
  let parts :=
    if ttl == 0 then #["SET", key, value]
    else #["SET", key, value, "EX", toString ttl]
  match ← c.command parts with
  | .simple _    => return ()
  | .error e     => throw <| IO.userError s!"SET {key}: {e}"
  | r            => throw <| IO.userError s!"SET {key}: unexpected reply {repr r}"

/-- `DEL key` — returns the number of keys actually removed. -/
def Client.del (c : Client) (key : String) : IO Nat := do
  match ← c.command #["DEL", key] with
  | .integer n   => return n.toNat
  | .error e     => throw <| IO.userError s!"DEL {key}: {e}"
  | r            => throw <| IO.userError s!"DEL {key}: unexpected reply {repr r}"

/-- `INCR key` — atomic ++, returns the new value. -/
def Client.incr (c : Client) (key : String) : IO Int := do
  match ← c.command #["INCR", key] with
  | .integer n   => return n
  | .error e     => throw <| IO.userError s!"INCR {key}: {e}"
  | r            => throw <| IO.userError s!"INCR {key}: unexpected reply {repr r}"

/-- `INCRBY key delta` — atomic add, returns the new value. -/
def Client.incrBy (c : Client) (key : String) (delta : Int) : IO Int := do
  match ← c.command #["INCRBY", key, toString delta] with
  | .integer n   => return n
  | .error e     => throw <| IO.userError s!"INCRBY {key} {delta}: {e}"
  | r            => throw <| IO.userError s!"INCRBY {key}: unexpected reply {repr r}"

/-- `FLUSHDB` — wipes the current database. -/
def Client.flushDb (c : Client) : IO Unit := do
  match ← c.command #["FLUSHDB"] with
  | .simple _    => return ()
  | .error e     => throw <| IO.userError s!"FLUSHDB: {e}"
  | r            => throw <| IO.userError s!"FLUSHDB: unexpected reply {repr r}"

/-! ## Cache adapter -/

/-- Bridge the Valkey connection to the `Cache` interface used by
    `Backend.cached`. Unlike memcached, Valkey allows arbitrary
    bytes in keys; no hashing is required. -/
def Client.asCache (c : Client) : LeanTea.Persist.Cache := {
  get    := fun k => c.get k,
  set    := fun k v => c.set k v,
  delete := fun k => do let _ ← c.del k
  clear  := c.flushDb
}

end LeanTea.Net.Valkey
