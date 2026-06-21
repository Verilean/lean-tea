import LeanTea

/-! # Chapter 2 — Persistence, typed

We define an entity (a Lean struct that knows how to round-trip
through SQLite), open a database, and exercise the typed query DSL:
inserts, filtered selects with operator syntax, updates by predicate,
deletes by predicate, counts.

Run:

    lake exe doc_ch02

The binary creates a fresh /tmp database, performs every operation
the chapter narrates, and prints the result of each step. If the
prose in `docs/02-persistence.md` claims something happens, the
binary's output is the receipt. -/

open LeanTea LeanTea.Persist

/-! ## The entity

A `Todo` is a row. The `Entity` typeclass instance describes how to
serialize it to/from a SQL row plus the column list and DDL. We do
not use string-stitched SQL anywhere outside of the DDL itself — the
query DSL handles the rest. -/

structure Todo where
  id    : Nat
  title : String
  done  : Bool
  prio  : Nat   -- 0 = low, 1 = med, 2 = high
  deriving Inhabited, Repr

instance : Entity Todo where
  table := "tasks"
  ddl :=
    "CREATE TABLE IF NOT EXISTS tasks(" ++
    "id INTEGER PRIMARY KEY AUTOINCREMENT," ++
    "title TEXT NOT NULL," ++
    "done INTEGER NOT NULL DEFAULT 0," ++
    "prio INTEGER NOT NULL DEFAULT 0)"
  columns := ["title", "done", "prio"]
  toRow t := #[t.title, if t.done then "1" else "0", toString t.prio]
  fromRow row :=
    match row.toList with
    | [idS, title, doneS, prioS] =>
      match idS.toNat?, prioS.toNat? with
      | some idN, some prioN =>
        .ok {
          id := idN, title,
          done := (doneS == "1"),
          prio := prioN
        }
      | _, _ => .error "Todo: parse failed"
    | _ => .error s!"Todo: expected 4 columns, got {row.size}"

/-! ## Typed column references

`Col E α` is the typed column accessor used by the query DSL. The
name is the literal SQL column name; the encoder turns the Lean
value into the textual representation the FFI sends. Booleans become
"1"/"0" so they line up with SQLite's integer convention. -/

namespace Todo
private def stringEnc (s : String) : String := s
def idC    : Col Todo Nat    := col "id"    toString
def titleC : Col Todo String := col "title" stringEnc
def doneC  : Col Todo Bool   := col "done"  (fun b => if b then "1" else "0")
def prioC  : Col Todo Nat    := col "prio"  toString
end Todo

/-! ## Walk through the operations -/

def dump (label : String) (rows : Array Todo) : IO Unit := do
  IO.println s!"  {label} ({rows.size}):"
  for t in rows do
    let mark := if t.done then "✓" else "·"
    IO.println s!"    {mark} #{t.id} [p{t.prio}] {t.title}"

def main : IO Unit := do
  let path := "/tmp/leantea_doc_ch02.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  IO.println "== Chapter 2 — Persistence =="
  IO.println ""

  let db ← LeanTea.Sqlite.open' path
  let tasks : Repo Todo := Repo.new db
  tasks.migrate

  /- Insert three rows. `insert` returns the affected count from
     sqlite (always 1 for a single INSERT); the AUTOINCREMENT id is
     fetched separately. We don't need the id here. -/
  IO.println "step 1: insert three tasks"
  let _ ← tasks.insert { id := 0, title := "ship docs",  done := false, prio := 2 }
  let _ ← tasks.insert { id := 0, title := "buy milk",   done := false, prio := 0 }
  let _ ← tasks.insert { id := 0, title := "send email", done := true,  prio := 1 }

  /- Filtered select — typed predicates compose via `&&.` / `||.`.
     `Select.empty.where_` reads top-to-bottom; the next chapter will
     introduce a shorter alias once the patterns are clear. -/
  IO.println "step 2: select open tasks ordered by prio desc"
  let open' ← tasks.select <|
    (Select.empty.where_ (Todo.doneC ==. false)).orderBy Todo.prioC.desc
  dump "open" open'

  /- count is its own primitive so the entity decoder doesn't need to
     understand COUNT(*). -/
  let nHigh ← tasks.count (Todo.prioC ==. 2)
  IO.println s!"step 3: count high-priority = {nHigh}"

  /- Update by predicate. Multiple SET assignments share the WHERE
     clause; the result is the affected row count. -/
  IO.println "step 4: mark 'buy milk' done"
  let touched ← tasks.updateWhere (Todo.titleC ==. "buy milk")
    [Todo.doneC =. true]
  IO.println s!"  touched = {touched}"

  /- Re-select. The previous row from the open list should now be
     gone. -/
  let stillOpen ← tasks.select <|
    Select.empty.where_ (Todo.doneC ==. false)
  dump "still open" stillOpen

  IO.println "step 5: delete every done task"
  let removed ← tasks.deleteWhere (Todo.doneC ==. true)
  IO.println s!"  removed = {removed}"

  let all ← tasks.select Select.empty
  dump "after delete" all
  IO.println ""
  IO.println "ok"
