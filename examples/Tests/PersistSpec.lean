import LeanTea
import LeanTea.Auth.Proof
import LeanTea.Persist.SafeQuery

/-! # examples/Tests/PersistSpec.lean — one binary for the SQLite-backed
    integration tests.

Consolidates the per-module smoke binaries (Sqlite, Query, Migrate,
AuthProof, SafeQuery) into a single LSpec runner so CI runs **one
step** instead of five. Each group uses its own temp DB file under
`/tmp/leantea_persistspec_*.sqlite` so groups don't trip over each
other.

`LeanTea.Persist` (legacy Query DSL) and `LeanTea.Persist.SafeQuery`
both define `Col` / `Select`, so we keep them in their own namespaces
to avoid `Ambiguous term` errors. -/

open LeanTea LeanTea.LSpec
open LeanTea.Net.Http (Request Response)

/-! ## Group 1 — `Store` round-trips (was `sqlite_smoke`). -/

namespace SqliteGroup

open LeanTea.Persist

def run : IO LSpec := do
  let path := "/tmp/leantea_persistspec_sqlite.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let s ← Store.open path

  s.setPref "diff" "2"
  s.setPref "name" "junji"
  s.addScore "sc" 3 4 1700000000
  s.addScore "dict" 5 8 1700000010
  s.addScore "vocab" 30 40 1700000020
  s.markToday "2026-06-15" "shadow"
  s.markToday "2026-06-15" "dict"
  s.markToday "2026-06-14" "vocab"

  let diff   ← s.getPref "diff"
  let name   ← s.getPref "name"
  let nope   ← s.getPref "missing"
  let scores ← s.recentScores
  let today  ← s.todayModes "2026-06-15"
  let days   ← s.daysWithEntries

  return group "Store roundtrips" [
    it "setPref+getPref roundtrips a value"      (diff == some "2"),
    it "second pref independent"                 (name == some "junji"),
    it "missing pref → none"                     (nope == none),
    it "three scores recorded"                   (scores.size == 3),
    it "todayModes returns both modes for 06-15" (today.size == 2),
    it "daysWithEntries covers both days"        (days.size == 2)
  ]

end SqliteGroup

/-! ## Group 2 — Persistent-style query DSL (was `query_smoke`). -/

namespace QueryGroup

open LeanTea.Persist

def modeC    : Col ScoreRow String := col "mode"    id
def correctC : Col ScoreRow Nat    := col "correct" toString
def totalC   : Col ScoreRow Nat    := col "total"   toString
def tsC      : Col ScoreRow Nat    := col "ts"      toString

def run : IO LSpec := do
  let path := "/tmp/leantea_persistspec_query.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let s ← Store.open path

  s.addScore "sc"    3   4 1700000000
  s.addScore "dict"  5   8 1700000010
  s.addScore "vocab" 30 40 1700000020
  s.addScore "dict"  8   8 1700000030

  let f := modeC ==. "dict"
       &&. correctC >. 6
       ||. totalC <. 10
  let (sql, _ps) := f.compile

  let dictRows  ← s.history.select <|
    (Select.empty.where_ (modeC ==. "dict")).orderBy tsC.asc
  let dictCount ← s.history.count (modeC ==. "dict")
  let touched   ← s.history.updateWhere (modeC ==. "sc")
    [tsC =. 9, totalC =. 99]
  let scRows    ← s.history.select <| Select.empty.where_ (modeC ==. "sc")
  let removed   ← s.history.deleteWhere (modeC ==. "vocab")
  let all       ← s.history.select Select.empty

  return group "Persist.Query DSL" [
    it "compile renders parametrised SQL"        (sql.length > 0 && !sql.contains '\''),
    it "select WHERE mode = 'dict' → 2 rows"     (dictRows.size == 2),
    it "count WHERE mode = 'dict' → 2"           (dictCount == 2),
    it "updateWhere mode='sc' → 1 touched"       (touched == 1),
    it "updated row has ts=9 + total=99"
      (scRows.size == 1 && scRows[0]!.ts == 9 && scRows[0]!.total == 99),
    it "deleteWhere mode='vocab' → 1 removed"    (removed == 1),
    it "three rows remain after delete"          (all.size == 3)
  ]

end QueryGroup

/-! ## Group 3 — Migration runner (was `migrate_smoke`). -/

namespace MigrateGroup

open LeanTea.Persist LeanTea.Persist.Migrate

def mig1 : Migration := {
  version := 1, description := "create users",
  up := fun b => do
    let _ ← b.exec
      "CREATE TABLE users(id INTEGER PRIMARY KEY, name TEXT NOT NULL)" #[]
}

def mig2 : Migration := {
  version := 2, description := "create posts",
  up := fun b => do
    let _ ← b.exec
      "CREATE TABLE posts(id INTEGER PRIMARY KEY, user_id INTEGER, body TEXT)" #[]
  down? := some fun b => do
    let _ ← b.exec "DROP TABLE posts" #[]
    return ()
}

def mig3 : Migration := {
  version := 3, description := "add users.email",
  up := fun b => do
    let _ ← b.exec
      "ALTER TABLE users ADD COLUMN email TEXT NOT NULL DEFAULT ''" #[]
}

def run : IO LSpec := do
  let path := "/tmp/leantea_persistspec_migrate.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let db ← LeanTea.Sqlite.open' path
  let backend := Db.toBackend db
  let migs : List Migration := [mig1, mig2, mig3]

  let (n1, v1) ← Migrate.run backend migs
  let (n2, v2) ← Migrate.run backend migs

  return group "Migration runner" [
    it "first run applies 3 migrations"                  (n1 == 3 && v1 == 3),
    it "second run is idempotent (0 applied, still v3)" (n2 == 0 && v2 == 3)
  ]

end MigrateGroup

/-! ## Group 4 — Auth.Proof + dependent owner proof (was `auth_proof_smoke`). -/

namespace AuthProofGroup

open LeanTea.Auth LeanTea.Auth.Proof
open LeanTea.Persist
open LeanTea.Sqlite

def resolveRole : Session → Capability
  | s => if s.email == "admin@example.com" then .admin
         else if s.email == "user@example.com" then .user
         else .guest

def handlePing (proof : Proof .user) (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 s!"ping from {proof.subject}"

def handleAdminDelete (proof : Proof .admin) (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 s!"deleted (auth'd as {proof.subject})"

def handleOwnerEdit (id : String) (proof : Proof (.owner id))
    (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 s!"edited {id} (owner = {proof.subject})"

def run : IO LSpec := do
  let path := "/tmp/leantea_persistspec_auth.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let db ← Sqlite.open' path
  let auth ← AuthStore.attach db
  let now ← nowSec
  auth.addSession {
    token := "tok-admin", email := "admin@example.com",
    name := "Admin", picture := "",
    createdAt := now, expiresAt := now + 3600 }
  auth.addSession {
    token := "tok-user", email := "user@example.com",
    name := "User", picture := "",
    createdAt := now, expiresAt := now + 3600 }

  let pingRoute : AuthRoute .user := {
    path := "/ping", method := "GET", handler := handlePing }
  let adminRoute : AuthRoute .admin := {
    path := "/admin/delete", method := "POST", handler := handleAdminDelete }
  let routes : List AnyAuthRoute := [
    AnyAuthRoute.of (c := .user)  pingRoute,
    AnyAuthRoute.of (c := .admin) adminRoute
  ]
  let dispatch := dispatchAuthorized auth resolveRole routes
  let mkReq (path method cookie : String) : Request := {
    method, path, query := "",
    headers := #[("cookie", cookie)],
    body := .empty
  }

  let r1 ← dispatch (mkReq "/admin/delete" "POST" "sid=tok-admin")
  let r2 ← dispatch (mkReq "/admin/delete" "POST" "sid=tok-user")
  let r3 ← dispatch (mkReq "/ping"         "GET"  "")
  let r4 ← dispatch (mkReq "/ping"         "GET"  "sid=tok-user")
  let r5 ← dispatch (mkReq "/ping"         "GET"  "sid=tok-admin")

  let owners : List (String × String) := [("doc-42", "user@example.com")]
  let checkOwnership : Session → String → IO Bool := fun s rid =>
    return (owners.any fun (r, o) => r == rid && o == s.email)
  let ownerReqOK := mkReq "/edit/doc-42" "POST" "sid=tok-user"
  let ownerReqNo := mkReq "/edit/doc-42" "POST" "sid=tok-admin"
  let okEdit ← Proof.issueOwner auth ownerReqOK "doc-42" checkOwnership
  let noEdit ← Proof.issueOwner auth ownerReqNo "doc-42" checkOwnership
  let okStatus ← match okEdit with
    | .ok p    => do let resp ← handleOwnerEdit "doc-42" p ownerReqOK; pure resp.status
    | .error _ => pure 0
  let nonOwnerRefused := match noEdit with | .ok _ => false | .error _ => true

  return group "Auth.Proof + dependent owner proof" [
    it "admin → /admin/delete = 200"        (r1.status == 200),
    it "user  → /admin/delete = 403"        (r2.status == 403),
    it "no cookie → /ping = 403"            (r3.status == 403),
    it "user  → /ping = 200"                (r4.status == 200),
    it "admin → /ping (widens) = 200"       (r5.status == 200),
    it "owner can edit own doc"             (okStatus == 200),
    it "non-owner refused at Proof.issue"   nonOwnerRefused
  ]

end AuthProofGroup

/-! ## Group 5 — SafeQuery typed-SQL (was `safequery_smoke`). -/

namespace SafeQueryGroup

open LeanTea.Persist (Entity Repo)
open LeanTea.Persist.Repo (migrate insert)
open LeanTea.Persist.SafeQuery
open LeanTea.Sqlite

structure User where
  id      : Nat
  email   : String
  name    : String
  deleted : Bool
  deriving Inhabited, Repr

instance : Entity User where
  table   := "sq_users"
  ddl     :=
    "CREATE TABLE IF NOT EXISTS sq_users(" ++
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

def idC      : Col User Nat    := ⟨"id"⟩
def emailC   : Col User String := ⟨"email"⟩
def nameC    : Col User String := ⟨"name"⟩
def deletedC : Col User Bool   := ⟨"deleted"⟩

abbrev SQrun     := @LeanTea.Persist.SafeQuery.run
abbrev SQcount   := @LeanTea.Persist.SafeQuery.count
abbrev SQupdate  := @LeanTea.Persist.SafeQuery.update
abbrev SQdelete  := @LeanTea.Persist.SafeQuery.delete
abbrev SQtrusted := @LeanTea.Persist.SafeQuery.trusted

def run : IO LSpec := do
  let path := "/tmp/leantea_persistspec_safequery.sqlite"
  IO.FS.removeFile path |>.catchExceptions (fun _ => pure ())
  let db ← Sqlite.open' path
  let users : Repo User := Repo.new db
  users.migrate
  let _ ← users.insert { id := 1, email := "alice@x.com", name := "Alice", deleted := false }
  let _ ← users.insert { id := 2, email := "bob@x.com",   name := "Bob",   deleted := false }
  let _ ← users.insert { id := 3, email := "carol@y.com", name := "Carol", deleted := false }
  let _ ← users.insert { id := 4, email := "dave@y.com",  name := "Dave",  deleted := true  }

  let q1 : Select User := { where_ := emailC.eq "alice@x.com" }
  let (sql1, params1) := q1.render
  let rows1 ← SQrun users q1
  let q2 : Select User := { where_ := idC.inList [1, 3] }
  let rows2 ← SQrun users q2
  let q3 : Select User := { where_ := emailC.like .suffix "@y.com" }
  let rows3 ← SQrun users q3
  let q4 : Select User := {
    where_ := .and (emailC.like .suffix "@y.com") (.not (deletedC.eq true)) }
  let rows4 ← SQrun users q4
  let n5 ← SQupdate users { set := [ nameC .= "Alice Renamed" ],
                            where_ := idC.eq 1 }
  let after ← SQrun users { where_ := idC.eq 1 }
  let live ← SQcount users (deletedC.eq false)
  let n7 ← SQdelete users { where_ := deletedC.eq true }
  let remaining ← SQcount users .trueP
  let weird ← SQtrusted users decl_name%
    "SELECT id, email, name, deleted FROM sq_users WHERE email LIKE ?"
    #["%alice%"]

  return group "SafeQuery typed SQL" [
    it "rendered SQL is parametrised (no inline literal)"
      (sql1.length > 0 && !sql1.contains '\'' && params1.size == 1),
    it "WHERE email = ? returns Alice"
      (rows1.size == 1 && rows1[0]!.email == "alice@x.com"),
    it "WHERE id IN (1,3) returns 2 rows"        (rows2.size == 2),
    it "LIKE suffix '@y.com' returns 2 rows"     (rows3.size == 2),
    it "AND NOT deleted → 1 row (Carol)"
      (rows4.size == 1 && rows4[0]!.name == "Carol"),
    it "UPDATE SET name WHERE id=1 → 1 updated"
      (n5 == 1 && after.size == 1 && after[0]!.name == "Alice Renamed"),
    it "COUNT non-deleted = 3"                   (live == 3),
    it "DELETE deleted=true → 1 row removed"     (n7 == 1),
    it "remaining row count = 3"                 (remaining == 3),
    it ".trusted decl_name% escape hits alice"   (weird.size == 1)
  ]

end SafeQueryGroup

/-! ## Entry point. -/

def main : IO Unit := do
  let s1 ← SqliteGroup.run
  let s2 ← QueryGroup.run
  let s3 ← MigrateGroup.run
  let s4 ← AuthProofGroup.run
  let s5 ← SafeQueryGroup.run
  let tree := group "LeanTEA persistence integration" [s1, s2, s3, s4, s5]
  let code ← lspecIO tree
  if code != 0 then IO.Process.exit code.toUInt8
