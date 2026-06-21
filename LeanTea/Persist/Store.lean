import LeanTea.Persist.Sqlite

/-! # Persistent-style typed entity layer

A tiny analogue of Haskell's `persistent`. Each entity is a Lean
structure with a `toRow`/`fromRow` codec, a table name and a small
DDL string. The framework provides typeclass-driven CRUD ops via
`LeanTea.Sqlite`. Conventions:

* All scalar fields are encoded as TEXT (sqlite's flexible typing
  makes this safe). The codec lives entirely in user code so unusual
  types are easy to support.
* The table is created on first connect via `Repo.migrate`. Schema
  changes are intentionally manual — we do not auto-migrate.
* No prepared statement caching yet; for our app's traffic the cost
  is negligible. -/

namespace LeanTea.Persist

open LeanTea.Sqlite

/-- An entity is a record that has a TEXT-encoded row representation
    plus the table name + DDL. Entities are not required to have an
    `id`; insert just appends. Queries that need an `id` should add it
    as a field. -/
class Entity (α : Type) where
  table   : String
  /-- `CREATE TABLE` statement (we leave `IF NOT EXISTS` to the user). -/
  ddl     : String
  /-- Column names that `toRow` produces, in order. They are explicit
      because tables often have extra columns like `id INTEGER PRIMARY
      KEY AUTOINCREMENT` that aren't part of the entity. -/
  columns : List String
  toRow   : α → Array String
  fromRow : Array String → Except String α

/-- A repository binds an `Entity` to a database connection. The
    intended use is `Repo.new db` to obtain a value-level handle. -/
structure Repo (α : Type) [Entity α] where
  db : Db

namespace Repo
variable {α : Type} [Entity α]

def new (db : Db) : Repo α := ⟨db⟩

/-- Run the entity's DDL once, wrapped in `CREATE TABLE IF NOT EXISTS`. -/
def migrate (r : Repo α) : IO Unit := do
  exec r.db (Entity.ddl α)

/-- Insert one row. Column list is inferred from `toRow` length; we
    use the table's natural column order, so the DDL and `toRow` must
    agree (Lean does not have row-polymorphism cheap enough to do this
    automatically). -/
def insert (r : Repo α) (x : α) : IO Nat := do
  let cells := Entity.toRow x
  let cols := String.intercalate "," (Entity.columns α)
  let placeholders := String.intercalate "," (List.replicate cells.size "?")
  let sql := s!"INSERT INTO {Entity.table α}({cols}) VALUES ({placeholders})"
  execp r.db sql cells

/-- Run an arbitrary SQL with the table's full row shape and decode
    each row through `fromRow`. -/
def query (r : Repo α) (sql : String) (params : Array String := #[])
  : IO (Array α) := do
  let rows ← Sqlite.query r.db sql params
  let mut out : Array α := #[]
  for row in rows do
    match Entity.fromRow row with
    | .ok v => out := out.push v
    | .error e => throw (IO.userError s!"fromRow: {e}")
  return out

def all (r : Repo α) : IO (Array α) := do
  query r s!"SELECT * FROM {Entity.table α}"

def deleteAll (r : Repo α) : IO Unit := do
  exec r.db s!"DELETE FROM {Entity.table α}"

/-- Run free-form SQL that doesn't return entity rows. -/
def execRaw (r : Repo α) (sql : String) (params : Array String := #[]) : IO Nat := do
  execp r.db sql params

def queryRaw (r : Repo α) (sql : String) (params : Array String := #[])
  : IO (Array (Array String)) := do
  Sqlite.query r.db sql params

end Repo

/-! ## Sample entities for the english app

These are kept close to the framework so the existing `serve.py`
behavior can be ported to pure Lean with little ceremony. -/

structure ScoreRow where
  mode    : String
  correct : Nat
  total   : Nat
  ts      : Nat   -- unix timestamp
  deriving Inhabited, Repr

structure PrefRow where
  key   : String
  value : String
  deriving Inhabited, Repr

structure DailyRow where
  day  : String   -- "YYYY-MM-DD"
  mode : String
  deriving Inhabited, Repr

instance : Entity ScoreRow where
  table := "history"
  ddl :=
    "CREATE TABLE IF NOT EXISTS history(" ++
    "id INTEGER PRIMARY KEY AUTOINCREMENT," ++
    "mode TEXT NOT NULL," ++
    "correct INTEGER NOT NULL," ++
    "total INTEGER NOT NULL," ++
    "ts INTEGER NOT NULL)"
  columns := ["mode", "correct", "total", "ts"]
  toRow s := #[s.mode, toString s.correct, toString s.total, toString s.ts]
  fromRow row :=
    match row.toList with
    | [_id, m, c, t, ts] =>
      match c.toNat?, t.toNat?, ts.toNat? with
      | some c, some t, some ts => .ok { mode := m, correct := c, total := t, ts := ts }
      | _, _, _ => .error "ScoreRow: integer parse failed"
    | _ => .error s!"ScoreRow: expected 5 columns, got {row.size}"

instance : Entity PrefRow where
  table := "prefs"
  ddl :=
    "CREATE TABLE IF NOT EXISTS prefs(" ++
    "key TEXT PRIMARY KEY," ++
    "value TEXT)"
  columns := ["key", "value"]
  toRow p := #[p.key, p.value]
  fromRow row :=
    match row.toList with
    | [k, v] => .ok { key := k, value := v }
    | _ => .error s!"PrefRow: expected 2 columns, got {row.size}"

instance : Entity DailyRow where
  table := "daily_log"
  ddl :=
    "CREATE TABLE IF NOT EXISTS daily_log(" ++
    "day TEXT NOT NULL," ++
    "mode TEXT NOT NULL," ++
    "PRIMARY KEY (day, mode))"
  columns := ["day", "mode"]
  toRow d := #[d.day, d.mode]
  fromRow row :=
    match row.toList with
    | [d, m] => .ok { day := d, mode := m }
    | _ => .error s!"DailyRow: expected 2 columns, got {row.size}"

/-- Per-word spaced-repetition state. `correct` / `wrong` are
    running totals; `last_seen` is a unix timestamp; `streak` is the
    current correct-in-a-row count (resets to 0 on a miss). The
    weight = `wrong + 2 - streak` (clamped ≥ 1) feeds the picker. -/
structure VocabRow where
  word     : String
  correct  : Nat
  wrong    : Nat
  streak   : Nat
  lastSeen : Nat
  deriving Inhabited, Repr

instance : Entity VocabRow where
  table := "vocab_progress"
  ddl :=
    "CREATE TABLE IF NOT EXISTS vocab_progress(" ++
    "word TEXT PRIMARY KEY," ++
    "correct INTEGER NOT NULL DEFAULT 0," ++
    "wrong INTEGER NOT NULL DEFAULT 0," ++
    "streak INTEGER NOT NULL DEFAULT 0," ++
    "last_seen INTEGER NOT NULL DEFAULT 0)"
  columns := ["word", "correct", "wrong", "streak", "last_seen"]
  toRow v := #[v.word, toString v.correct, toString v.wrong,
               toString v.streak, toString v.lastSeen]
  fromRow row :=
    match row.toList with
    | [w, c, wr, st, ls] =>
      match c.toNat?, wr.toNat?, st.toNat?, ls.toNat? with
      | some c, some wr, some st, some ls =>
        .ok { word := w, correct := c, wrong := wr, streak := st, lastSeen := ls }
      | _, _, _, _ => .error "VocabRow: integer parse failed"
    | _ => .error s!"VocabRow: expected 5 columns, got {row.size}"

/-! ## High-level helpers used by the app -/

structure Store where
  db        : Db
  history   : Repo ScoreRow
  prefs     : Repo PrefRow
  dailyLog  : Repo DailyRow
  vocab     : Repo VocabRow

def Store.open (path : String) : IO Store := do
  let db ← Sqlite.open' path
  let history  : Repo ScoreRow := Repo.new db
  let prefs    : Repo PrefRow  := Repo.new db
  let dailyLog : Repo DailyRow := Repo.new db
  let vocab    : Repo VocabRow := Repo.new db
  history.migrate
  prefs.migrate
  dailyLog.migrate
  vocab.migrate
  return { db, history, prefs, dailyLog, vocab }

def Store.setPref (s : Store) (key value : String) : IO Unit := do
  let _ ← s.prefs.execRaw
    "INSERT INTO prefs(key, value) VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value=excluded.value"
    #[key, value]

def Store.getPref (s : Store) (key : String) : IO (Option String) := do
  let rows ← s.prefs.queryRaw "SELECT value FROM prefs WHERE key = ?" #[key]
  match rows.toList with
  | (row :: _) => return row[0]?
  | _          => return none

def Store.addScore (s : Store) (mode : String) (correct total ts : Nat) : IO Unit := do
  let _ ← s.history.insert {
    mode := mode, correct := correct, total := total, ts := ts }

def Store.recentScores (s : Store) (limit : Nat := 200) : IO (Array ScoreRow) := do
  s.history.query s!"SELECT * FROM history ORDER BY id DESC LIMIT {limit}"

def Store.clearScores (s : Store) : IO Unit := s.history.deleteAll

def Store.markToday (s : Store) (day mode : String) : IO Unit := do
  let _ ← s.dailyLog.execRaw
    "INSERT OR IGNORE INTO daily_log(day, mode) VALUES (?, ?)" #[day, mode]

def Store.todayModes (s : Store) (day : String) : IO (Array String) := do
  let rows ← s.dailyLog.queryRaw
    "SELECT mode FROM daily_log WHERE day = ?" #[day]
  return rows.filterMap (fun r => r[0]?)

def Store.daysWithEntries (s : Store) : IO (Array String) := do
  let rows ← s.dailyLog.queryRaw
    "SELECT DISTINCT day FROM daily_log ORDER BY day DESC" #[]
  return rows.filterMap (fun r => r[0]?)

/-! ## Vocabulary progress (per-word SRS) -/

/-- Record one answer for `word`. UPSERT-shaped — first time a word is
    seen, it's inserted at zero. Streak counts correct-in-a-row and
    resets on a miss; weight derivations elsewhere rely on it. -/
def Store.vocabRecord (s : Store) (word : String) (won : Bool) (ts : Nat) : IO Unit := do
  let _ ← s.vocab.execRaw
    ("INSERT INTO vocab_progress(word, correct, wrong, streak, last_seen) " ++
     "VALUES (?, ?, ?, ?, ?) " ++
     "ON CONFLICT(word) DO UPDATE SET " ++
     "  correct = correct + ?, " ++
     "  wrong   = wrong   + ?, " ++
     "  streak  = CASE WHEN ? = 1 THEN streak + 1 ELSE 0 END, " ++
     "  last_seen = ?")
    #[word,
      (if won then "1" else "0"),
      (if won then "0" else "1"),
      (if won then "1" else "0"),
      toString ts,
      (if won then "1" else "0"),
      (if won then "0" else "1"),
      (if won then "1" else "0"),
      toString ts]

def Store.vocabAll (s : Store) : IO (Array VocabRow) := do
  s.vocab.query "SELECT * FROM vocab_progress"

/-- Words still needing review — picks the top `pool` worst offenders
    (by `wrong - streak`, with `wrong > streak` so mastered words drop
    out), then randomises the order so the user doesn't keep seeing
    the single worst word on every review turn. Returns at most
    `limit` entries from the shuffled pool. -/
def Store.vocabWeakest (s : Store) (limit : Nat := 8)
    (pool : Nat := 16) : IO (Array VocabRow) := do
  let sql :=
    "SELECT * FROM (" ++
    "  SELECT * FROM vocab_progress " ++
    "  WHERE wrong > 0 AND wrong > streak " ++
    "  ORDER BY (wrong - streak) DESC, wrong DESC " ++
    s!"  LIMIT {pool}" ++
    s!") ORDER BY RANDOM() LIMIT {limit}"
  s.vocab.query sql

/-- Best-mastered words (long correct streak, no recent misses). The
    UI uses this to show "もう覚えた" praise and to deprioritise these
    words in the picker. Randomised within the top pool so the
    occasional refresh question isn't always the same word. -/
def Store.vocabStrongest (s : Store) (limit : Nat := 4)
    (pool : Nat := 20) : IO (Array VocabRow) := do
  let sql :=
    "SELECT * FROM (" ++
    "  SELECT * FROM vocab_progress " ++
    "  WHERE streak >= 2 " ++
    "  ORDER BY streak DESC, correct DESC " ++
    s!"  LIMIT {pool}" ++
    s!") ORDER BY RANDOM() LIMIT {limit}"
  s.vocab.query sql

/-- Set of words the user has *ever* answered (whether right or
    wrong). The session builder uses this to find the "new" words. -/
def Store.vocabSeen (s : Store) : IO (Array String) := do
  let rows ← s.vocab.queryRaw "SELECT word FROM vocab_progress" #[]
  return rows.filterMap (fun r => r[0]?)

end LeanTea.Persist
