import LeanTea
import LeanTea.Auth.Idp

/-! # auth_idp_serve — standalone in-process IdP exe for AuthSpec

Runs `LeanTea.Auth.Idp.OAuth2` on a fixed port (default 18765,
override via env `AUTH_TEST_PORT`). `AuthSpec` spawns this binary
as a subprocess, polls until /authorize is responding, runs the
test round-trip, and `IO.Process.Child.kill`s the child on
teardown.

Why a separate process rather than `IO.asTask`? In-process,
spawned-via-asTask servers share the runtime's libuv loop with the
main thread; when the main thread blocks on `IO.Process.output`
(curl, used by `exchangeCode`) the server task stops accepting and
the whole round-trip deadlocks. Splitting them into two OS
processes avoids the shared scheduler entirely. -/

open LeanTea.Auth.Idp

def aliceUser : OAuth2.User := {
  sub := "alice-sub-001", email := "alice@example.com",
  name := "Alice Example",
  picture := "https://idp.example.com/alice.png"
}

def aliceClient : OAuth2.Client := {
  clientId := "test-client", clientSecret := "shh",
  /- The redirect URI is opaque to the IdP — it just echoes the
     query parameter back. AuthSpec uses port 18766 to make the
     intent clear ("the bogus SP callback"), but never serves
     anything there. -/
  redirectUri := "http://127.0.0.1:18766/cb",
  user := aliceUser
}

def main : IO Unit := do
  let port : UInt16 := match (← IO.getEnv "AUTH_TEST_PORT").bind String.toNat? with
    | some n => n.toUInt16
    | none   => 18765
  let codes  ← IO.mkRef ([] : List OAuth2.IssuedCode)
  let tokens ← IO.mkRef ([] : List OAuth2.IssuedToken)
  let st : OAuth2.State := { cfg := { clients := [aliceClient] }, codes, tokens }
  LeanTea.Net.Server.serve port "127.0.0.1" (OAuth2.handler st)
