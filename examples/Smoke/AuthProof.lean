import LeanTea
import LeanTea.Auth.Proof

/-! # auth_proof_smoke — exercise `LeanTea.Auth.Proof`

This file is also the worked example referenced from
`SECURITY.md §"Primitive 3 · Proof of Authorization"`. It exists to:

1. Prove the framework compiles when handlers demand `Proof c` in
   their signature.
2. Show that **removing the `proof` argument from a handler signature
   is a compile error** because the dispatch site can't satisfy the
   modified handler type.
3. Exercise the dependent-type `Proof (.owner id)` form.

We don't spin up a real HTTP server — instead we fabricate a few
`Request` values, mint sessions in an on-disk SQLite, and assert the
dispatcher's behaviour. -/

open LeanTea LeanTea.Auth LeanTea.Auth.Proof
open LeanTea.Net.Http (Request Response)
open LeanTea.Persist

/-! ## Test fixture — sessions in a temp DB. -/

private def tempDbPath : IO String := do
  let tmp ← IO.getEnv "TMPDIR" >>= fun e => pure (e.getD "/tmp")
  return s!"{tmp}/auth_proof_smoke.sqlite"

/-- Map session email → capability. In a real app you'd look this up
    in a `user_roles` table; for the smoke we hard-code two emails. -/
def resolveRole : Session → Capability
  | s => if s.email == "admin@example.com" then .admin
         else if s.email == "user@example.com" then .user
         else .guest

/-! ## Handlers — note that the *type signature* demands the proof. -/

/-- Public route — anyone (even guests) can call it. No proof needed. -/
def handlePublic (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 "public ok"

/-- Admin-only route. Drop the `proof` parameter from the signature
    and `adminRoute` below stops compiling — the dispatcher expects a
    `Proof .admin → Request → IO Response` shape. -/
def handleAdminDelete (proof : Proof .admin) (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 s!"deleted (auth'd as {proof.subject})"

/-- User-or-better route. -/
def handleUserPing (proof : Proof .user) (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 s!"ping from {proof.subject}"

/-- Dependent-type owner route. The `id` parameter is reflected into
    the proof's type — the dispatcher had to verify ownership *of
    that specific id* to call this handler. -/
def handleOwnerEdit (id : String) (proof : Proof (.owner id))
    (req : Request) : IO Response := do
  let _ := req
  return Response.text 200 s!"edited {id} (auth'd as owner = {proof.subject})"

/-- The proof carries a type, but `subject` is just a string. We can
    *weaken* an admin proof to a user proof for free, because the
    `HasCapability` instance is in scope. -/
example (p : Proof .admin) : Proof .user := p.weaken
example (p : Proof .admin) : Proof .guest := p.weaken
example (p : Proof .user)  : Proof .guest := p.weaken

/-! ## Smoke runner. -/

def main : IO Unit := do
  /- 1. Wipe and re-open the test DB. -/
  let dbPath ← tempDbPath
  try IO.FS.removeFile dbPath catch _ => pure ()
  let db ← Sqlite.open' dbPath
  let auth ← AuthStore.attach db

  /- 2. Mint two sessions. -/
  let now ← nowSec
  auth.addSession {
    token := "tok-admin", email := "admin@example.com",
    name := "Admin", picture := "",
    createdAt := now, expiresAt := now + 3600 }
  auth.addSession {
    token := "tok-user", email := "user@example.com",
    name := "User", picture := "",
    createdAt := now, expiresAt := now + 3600 }

  /- 3. Build a routing table mixing capabilities (Sigma-wrapped). -/
  let routes : List AnyAuthRoute := [
    AnyAuthRoute.of (c := .user) {
      path := "/ping", method := "GET",
      handler := handleUserPing },
    AnyAuthRoute.of (c := .admin) {
      path := "/admin/delete", method := "POST",
      handler := handleAdminDelete }
  ]
  let dispatch := dispatchAuthorized auth resolveRole routes

  /- 4. Helper that runs a request and prints status + body. -/
  let mkReq (path method cookie : String) : Request := {
    method, path, query := "",
    headers := #[("cookie", cookie)],
    body := .empty
  }
  let report (label : String) (resp : Response) : IO Unit := do
    let body := match String.fromUTF8? resp.body with
                | some s => s | none => "<binary>"
    IO.println s!"  [{resp.status}] {label}: {body}"

  IO.println "── static-capability dispatch ─────────────────────"
  report "admin → /admin/delete"
    (← dispatch (mkReq "/admin/delete" "POST" "sid=tok-admin"))
  report "user → /admin/delete (should 403)"
    (← dispatch (mkReq "/admin/delete" "POST" "sid=tok-user"))
  report "no cookie → /ping (should 403)"
    (← dispatch (mkReq "/ping" "GET" ""))
  report "user → /ping"
    (← dispatch (mkReq "/ping" "GET" "sid=tok-user"))
  report "admin → /ping (widens)"
    (← dispatch (mkReq "/ping" "GET" "sid=tok-admin"))

  IO.println "── dependent owner proof ───────────────────────────"
  /- Resource "doc-42" — `checkOwnership` is the authoritative check.
     In a real app it would `SELECT 1 FROM docs WHERE id=? AND owner=?`. -/
  let owners : List (String × String) := [
    ("doc-42", "user@example.com")
  ]
  let checkOwnership : Session → String → IO Bool := fun s rid =>
    return (owners.any fun (r, o) => r == rid && o == s.email)
  let req := mkReq "/edit/doc-42" "POST" "sid=tok-user"
  match ← Proof.issueOwner auth req "doc-42" checkOwnership with
  | .ok p =>
    let resp ← handleOwnerEdit "doc-42" p req
    report "user owns doc-42 → edit succeeds" resp
  | .error e => IO.println s!"  [403] owner check failed: {e}"

  /- Wrong user trying to edit. -/
  let req2 := mkReq "/edit/doc-42" "POST" "sid=tok-admin"
  match ← Proof.issueOwner auth req2 "doc-42" checkOwnership with
  | .ok _    => IO.println "  [?] admin shouldn't own doc-42"
  | .error e => IO.println s!"  [403] admin → /edit/doc-42 (not owner): {e}"

  IO.println "── compile-time guard demo ─────────────────────────"
  IO.println "  Try removing `(proof : Proof .admin)` from handleAdminDelete"
  IO.println "  and rebuild: AuthRoute.handler expects exactly that shape."

  IO.println "auth_proof_smoke: done"
