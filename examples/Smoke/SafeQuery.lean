import LeanTea
import LeanTea.Persist.SafeQuery

/-! # safequery_smoke — exercise `LeanTea.Persist.SafeQuery`

See SECURITY.md (Primitive 1) for the design rationale. -/

open LeanTea
open LeanTea.Persist (Entity Repo)
open LeanTea.Persist.Repo (migrate insert)
open LeanTea.Persist.SafeQuery
open LeanTea.Sqlite

/-- Local alias so we can write `SafeQuery.run users q` even though
    `LeanTea.Persist.SafeQuery` is the namespace path. -/
abbrev SafeQuery.run     := @LeanTea.Persist.SafeQuery.run
abbrev SafeQuery.count   := @LeanTea.Persist.SafeQuery.count
abbrev SafeQuery.update  := @LeanTea.Persist.SafeQuery.update
abbrev SafeQuery.delete  := @LeanTea.Persist.SafeQuery.delete
abbrev SafeQuery.trusted := @LeanTea.Persist.SafeQuery.trusted

/-! ## Test fixture — a tiny `users` table. -/

structure User where
  id      : Nat
  email   : String
  name    : String
  deleted : Bool
  deriving Inhabited, Repr

instance : Entity User where
  table   := "users"
  ddl     :=
    "CREATE TABLE IF NOT EXISTS users(" ++
    "id INTEGER PRIMARY KEY," ++
    "email TEXT NOT NULL," ++
    "name TEXT NOT NULL," ++
    "deleted INTEGER NOT NULL DEFAULT 0)"
  columns := ["id", "email", "name", "deleted"]
  toRow u := #[toString u.id, u.email, u.name, if u.deleted then "1" else "0"]
  fromRow row :=
    match row.toList with
    | [i, e, n, d] =>
      match i.toNat? with
      | some id => .ok { id, email := e, name := n, deleted := d == "1" }
      | none    => .error "User.id: int parse"
    | _ => .error s!"User: expected 4 cols, got {row.size}"

namespace UserCols
  def id      : Col User Nat    := ⟨"id"⟩
  def email   : Col User String := ⟨"email"⟩
  def name    : Col User String := ⟨"name"⟩
  def deleted : Col User Bool   := ⟨"deleted"⟩
end UserCols

/-! ## Runner. -/

private def tempDbPath : IO String := do
  let tmp ← IO.getEnv "TMPDIR" >>= fun e => pure (e.getD "/tmp")
  return s!"{tmp}/safequery_smoke.sqlite"

def main : IO Unit := do
  let dbPath ← tempDbPath
  try IO.FS.removeFile dbPath catch _ => pure ()
  let db ← Sqlite.open' dbPath
  let users : Repo User := Repo.new db
  users.migrate

  /- Seed. -/
  let _ ← users.insert { id := 1, email := "alice@x.com", name := "Alice", deleted := false }
  let _ ← users.insert { id := 2, email := "bob@x.com",   name := "Bob",   deleted := false }
  let _ ← users.insert { id := 3, email := "carol@y.com", name := "Carol", deleted := false }
  let _ ← users.insert { id := 4, email := "dave@y.com",  name := "Dave",  deleted := true  }

  IO.println "── 1. SELECT … WHERE email = ? (typed eq) ───────────"
  let q1 : Select User := { where_ := UserCols.email.eq "alice@x.com" }
  let (sql, params) := q1.render
  IO.println s!"  SQL    : {sql}"
  IO.println s!"  params : {params}"
  let rows ← SafeQuery.run users q1
  for u in rows do IO.println s!"  → {u.email}"

  IO.println "── 2. WHERE id IN (?, ?, ?) (variable-length) ───────"
  let q2 : Select User := { where_ := UserCols.id.inList [1, 3] }
  let (sql, params) := q2.render
  IO.println s!"  SQL    : {sql}"
  IO.println s!"  params : {params}"
  let rows ← SafeQuery.run users q2
  for u in rows do IO.println s!"  → #{u.id} {u.name}"

  IO.println "── 3. WHERE email LIKE '%@y.com' (suffix LIKE) ──────"
  let q3 : Select User := { where_ := UserCols.email.like .suffix "@y.com" }
  let (sql, params) := q3.render
  IO.println s!"  SQL    : {sql}"
  IO.println s!"  params : {params}"
  let rows ← SafeQuery.run users q3
  for u in rows do IO.println s!"  → {u.email}"

  IO.println "── 4. AND / NOT combiner ─────────────────────────────"
  let q4 : Select User := {
    where_ := .and (UserCols.email.like .suffix "@y.com")
                   (.not (UserCols.deleted.eq true))
  }
  let (sql, params) := q4.render
  IO.println s!"  SQL    : {sql}"
  IO.println s!"  params : {params}"
  let rows ← SafeQuery.run users q4
  for u in rows do IO.println s!"  → {u.email} deleted={u.deleted}"

  IO.println "── 5. UPDATE … SET name = ? WHERE id = ? ────────────"
  let u5 : Update User := {
    set := [ UserCols.name .= "Alice Renamed" ],
    where_ := UserCols.id.eq 1
  }
  let n5 ← SafeQuery.update users u5
  IO.println s!"  rows updated: {n5}"
  let after ← SafeQuery.run users { where_ := UserCols.id.eq 1 }
  for u in after do IO.println s!"  after: {u.name}"

  IO.println "── 6. COUNT(*) WHERE deleted = false ─────────────────"
  let n ← SafeQuery.count users (UserCols.deleted.eq false)
  IO.println s!"  live users: {n}"

  IO.println "── 7. DELETE WHERE deleted = true ───────────────────"
  let n7 ← SafeQuery.delete users { where_ := UserCols.deleted.eq true }
  IO.println s!"  rows deleted: {n7}"
  let remaining ← SafeQuery.count users .trueP
  IO.println s!"  remaining users: {remaining}"

  IO.println "── 8. .trusted audit-tagged escape ──────────────────"
  /- The audit tag is `decl_name%` at the call site, so a quick
     grep -rn 'trusted' lean-tea/ lists every place hand-SQL
     is used. -/
  let weird ← SafeQuery.trusted users decl_name%
    "SELECT id, email, name, deleted FROM users WHERE email LIKE ?"
    #["%alice%"]
  for u in weird do IO.println s!"  trusted hit: {u.email}"

  IO.println "safequery_smoke: done"
