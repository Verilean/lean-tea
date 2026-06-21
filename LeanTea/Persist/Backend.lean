import LeanTea.Persist.Sqlite
import LeanTea.Persist.Store
import Std.Data.HashMap

/-! # Composable storage backend

A `Backend` is a record of three closures: `exec`, `query`, `close`.
Concrete drivers (SQLite, MySQL) and decorators (sharding, caching)
all live behind the same interface, so an app can stack them
without the call sites caring:

```lean
let shardA ← Sqlite.open' "user_0_to_999.db"
let shardB ← Sqlite.open' "user_1000_to_1999.db"
let cache  ← Cache.lru 1024
let backend :=
  (Backend.shardByParam #[shardA.toBackend, shardB.toBackend] 0).cached cache
```

Existing `Repo`-based code keeps compiling because the `Sqlite.Db`
constructors stay around. The Backend stack is purely additive. -/

namespace LeanTea.Persist

open LeanTea.Sqlite

/-! ## Backend interface -/

/-- All persistence sits behind this triple. Closures are passed
    `String` SQL + a positional parameter array — the same shape that
    sqlite / mysql naturally accept. -/
structure Backend where
  exec  : String → Array String → IO Nat
  query : String → Array String → IO (Array (Array String))
  close : IO Unit := pure ()
  deriving Inhabited

/-! ## SQLite adapter -/

/-- Lift a `Sqlite.Db` to a `Backend`. The closures call into the
    existing FFI so behaviour is identical to using the raw `Db`. -/
def Db.toBackend (db : Db) : Backend := {
  exec  := fun sql ps => execp db sql ps,
  query := fun sql ps => LeanTea.Sqlite.query db sql ps,
  close := LeanTea.Sqlite.close db
}

/-! ## Sharding -/

namespace Backend

/-- Route requests across `children` by inspecting `(sql, params)`.
    `route` returns a 0-based shard index; out-of-range values are
    folded with `mod children.size`. -/
def shard (children : Array Backend)
    (route : String → Array String → Nat) : Backend :=
  let pick (sql : String) (ps : Array String) : Backend :=
    let idx := (route sql ps) % children.size
    children[idx]!
  {
    exec  := fun sql ps => (pick sql ps).exec sql ps,
    query := fun sql ps => (pick sql ps).query sql ps,
    close := do
      for c in children do c.close
  }

/-- Cheap stable hash for sharding by a single param value. Folds
    UTF-8 bytes with 64-bit FNV-1a so the result is stable across
    runs and platforms. -/
def fnv1a64 (s : String) : UInt64 := Id.run do
  let mut h : UInt64 := 14695981039346656037  -- FNV offset basis
  for b in s.toUTF8 do
    h := h ^^^ b.toUInt64
    h := h * 1099511628211  -- FNV prime
  return h

/-- Shard by `params[idx]`. The "shard by user_id" recipe — just
    point at whichever param holds the user identifier. -/
def shardByParam (children : Array Backend) (idx : Nat) : Backend :=
  shard children fun _sql ps =>
    let v := ps[idx]?.getD ""
    (fnv1a64 v).toNat

/-- Shard by `params[idx]` parsed as a number; falls back to hashing
    the string if it isn't numeric. Useful when ids are already ints
    and you want a perfectly even split. -/
def shardByParamNat (children : Array Backend) (idx : Nat) : Backend :=
  shard children fun _sql ps =>
    let v := ps[idx]?.getD ""
    match v.toNat? with
    | some n => n
    | none   => (fnv1a64 v).toNat

end Backend

/-! ## Cache interface

`Cache` is the same record-of-closures trick as `Backend`. Concrete
caches (in-process LRU, memcached, redis) all just produce one of
these. Values are `String`-typed so caches that go over the network
have a natural transport. -/

structure Cache where
  get    : String → IO (Option String)
  set    : String → String → IO Unit
  delete : String → IO Unit
  clear  : IO Unit := pure ()
  deriving Inhabited

/-! ### In-process LRU implementation

A simple bounded map with insertion-order eviction. Plenty for tests
and single-process workloads; for multi-process / multi-host caching,
plug in memcached via `LeanTea.Net.Memcached.asCache`. -/

namespace Cache

private structure LruState where
  map      : Std.HashMap String String
  /-- Recency queue, head = oldest. Bounded by `capacity`. -/
  order    : Array String
  capacity : Nat

private def emptyLru (cap : Nat) : LruState :=
  { map := {}, order := #[], capacity := cap }

private def LruState.touch (s : LruState) (k : String) : LruState :=
  { s with order := (s.order.filter (· != k)).push k }

private def LruState.evict (s : LruState) : LruState :=
  if s.order.size ≤ s.capacity then s
  else Id.run do
    let mut s' := s
    while s'.order.size > s'.capacity do
      match s'.order[0]? with
      | none      => break
      | some k    =>
        s' := { s' with
          map := s'.map.erase k,
          order := s'.order.extract 1 s'.order.size }
    return s'

/-- Bounded LRU cache backed by `IO.Ref`. The cache is local to the
    process so it cannot help across instances; use `Memcached.asCache`
    for that. -/
def lru (capacity : Nat := 1024) : IO Cache := do
  let st ← IO.mkRef (emptyLru capacity)
  return {
    get := fun k => do
      let s ← st.get
      match s.map[k]? with
      | none   => return none
      | some v =>
        st.set (s.touch k)
        return some v,
    set := fun k v => do
      st.modify fun s =>
        let touched := s.touch k
        let withK   := { touched with map := touched.map.insert k v }
        withK.evict,
    delete := fun k => do
      st.modify fun s =>
        { s with map := s.map.erase k, order := s.order.filter (· != k) },
    clear := do
      st.modify fun s => { s with map := {}, order := #[] }
  }

end Cache

/-! ## Cached backend decorator

Reads consult `cache` first, falling through to `inner` on miss and
filling. Writes punch the cache so the next read sees fresh state.
"Punch the whole cache on any write" is correctness-safe; smarter
key-by-table invalidation can come later. -/

namespace Backend

/-! ### Row encoding

`Array (Array String)` ⇄ `String` so the cache layer can store and
retrieve row sets through any String-keyed cache. Each cell is
`<n>:<n bytes>`, so embedded delimiters round-trip cleanly. -/

private def encodeCell (s : String) : String :=
  toString s.utf8ByteSize ++ ":" ++ s

private def encodeRow (row : Array String) : String :=
  toString row.size ++ ";" ++
    row.foldl (fun acc c => acc ++ encodeCell c) ""

private def encodeRows (rows : Array (Array String)) : String :=
  toString rows.size ++ "|" ++
    rows.foldl (fun acc r => acc ++ encodeRow r) ""

/-- Read `<n>:<bytes>` starting at byte offset `i`; return the
    decoded cell + the offset just past it, or `none` on malformed
    input. -/
private partial def decodeCell?
    (bs : ByteArray) (i : Nat) : Option (String × Nat) := Id.run do
  let mut j := i
  let mut lenS : String := ""
  while j < bs.size && bs[j]! != ':'.toNat.toUInt8 do
    lenS := lenS.push (Char.ofNat bs[j]!.toNat)
    j := j + 1
  match lenS.toNat? with
  | none   => return none
  | some n =>
    let start := j + 1
    let stop  := start + n
    if stop > bs.size then return none
    let slice := bs.extract start stop
    return some (String.fromUTF8! slice, stop)

private partial def decodeRow?
    (bs : ByteArray) (i : Nat) : Option (Array String × Nat) := Id.run do
  -- Pull "n;" header
  let mut j := i
  let mut nStr : String := ""
  while j < bs.size && bs[j]! != ';'.toNat.toUInt8 do
    nStr := nStr.push (Char.ofNat bs[j]!.toNat)
    j := j + 1
  match nStr.toNat? with
  | none   => return none
  | some n =>
    j := j + 1
    let mut cells : Array String := #[]
    let mut k := 0
    let mut ok := true
    while ok && k < n do
      match decodeCell? bs j with
      | none           => ok := false
      | some (c, j')   => cells := cells.push c; j := j'; k := k + 1
    if ok then return some (cells, j) else return none

private partial def decodeRows? (raw : String) : Option (Array (Array String)) := Id.run do
  let bs := raw.toUTF8
  let mut j := 0
  let mut nStr : String := ""
  while j < bs.size && bs[j]! != '|'.toNat.toUInt8 do
    nStr := nStr.push (Char.ofNat bs[j]!.toNat)
    j := j + 1
  match nStr.toNat? with
  | none   => return none
  | some n =>
    j := j + 1
    let mut rows : Array (Array String) := #[]
    let mut k := 0
    let mut ok := true
    while ok && k < n do
      match decodeRow? bs j with
      | none           => ok := false
      | some (r, j')   => rows := rows.push r; j := j'; k := k + 1
    if ok then return some rows else return none

/-- Cache key. SQL plus params, separated by control bytes so distinct
    param sets get distinct keys. -/
private def cacheKey (sql : String) (ps : Array String) : String :=
  sql ++ "\x01" ++ String.intercalate "\x02" ps.toList

/-- Wrap `inner` with a read-through cache. The cache is queried
    before falling through to `inner`, and any successful `exec` call
    calls the cache's `clear` so stale reads can't survive a write. -/
def cached (inner : Backend) (cache : Cache) : Backend := {
  exec  := fun sql ps => do
    let n ← inner.exec sql ps
    cache.clear
    return n,
  query := fun sql ps => do
    let key := cacheKey sql ps
    match ← cache.get key with
    | some raw =>
      match decodeRows? raw with
      | some rows => return rows
      | none      =>
        let rows ← inner.query sql ps
        cache.set key (encodeRows rows)
        return rows
    | none =>
      let rows ← inner.query sql ps
      cache.set key (encodeRows rows)
      return rows,
  close := inner.close
}

end Backend

/-! ## RepoB — `Repo` against a `Backend`

`RepoB α` is the Backend-backed counterpart of `Repo α`. Existing
`Repo` code keeps working unchanged; new code that wants composable
storage uses `RepoB`. -/

structure RepoB (α : Type) [Entity α] where
  backend : Backend

namespace RepoB
variable {α : Type} [Entity α]

def new (b : Backend) : RepoB α := ⟨b⟩

def migrate (r : RepoB α) : IO Unit := do
  let _ ← r.backend.exec (Entity.ddl α) #[]

def insert (r : RepoB α) (x : α) : IO Nat := do
  let cells := Entity.toRow x
  let cols := String.intercalate "," (Entity.columns α)
  let placeholders := String.intercalate "," (List.replicate cells.size "?")
  let sql := s!"INSERT INTO {Entity.table α}({cols}) VALUES ({placeholders})"
  r.backend.exec sql cells

def query (r : RepoB α) (sql : String) (params : Array String := #[])
    : IO (Array α) := do
  let rows ← r.backend.query sql params
  let mut out : Array α := #[]
  for row in rows do
    match Entity.fromRow row with
    | .ok v => out := out.push v
    | .error e => throw (IO.userError s!"fromRow: {e}")
  return out

def all (r : RepoB α) : IO (Array α) :=
  r.query s!"SELECT * FROM {Entity.table α}"

def execRaw (r : RepoB α) (sql : String) (params : Array String := #[]) : IO Nat :=
  r.backend.exec sql params

def queryRaw (r : RepoB α) (sql : String) (params : Array String := #[])
    : IO (Array (Array String)) :=
  r.backend.query sql params

end RepoB

end LeanTea.Persist
