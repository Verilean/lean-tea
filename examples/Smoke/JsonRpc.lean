import LeanTea

open Lean (Json)
open LeanTea LeanTea.JsonRpc

/-! Smoke test: stand up a JSON-RPC server with two methods, then
    call them through the curl-backed client. Also exercises schema
    validation failures. -/

/-- Method 1: `ping` — no params, returns `{pong: true, n: 42}`. -/
def pingMethod : Method := {
  name := "ping",
  description := "Health-check; returns pong",
  params := .object [] [],
  result := .object [("pong", .bool_), ("n", .number)]
                   ["pong", "n"]
}

/-- Method 2: `add` — required `a`, `b` numbers, returns their sum. -/
def addMethod : Method := {
  name := "add",
  description := "Sum two numbers",
  params := .object [("a", .number), ("b", .number)] ["a", "b"],
  result := .number
}

def server : Server := {
  routes := [
    { method := pingMethod,
      handler := fun _ => return Json.mkObj [("pong", Json.bool true), ("n", Json.num 42)] },
    { method := addMethod,
      handler := fun args => do
        let a : Int := (args.getObjVal? "a").toOption.bind (·.getInt?.toOption) |>.getD 0
        let b : Int := (args.getObjVal? "b").toOption.bind (·.getInt?.toOption) |>.getD 0
        return Json.num (a + b) }
  ]
}

def main : IO Unit := do
  let port : UInt16 := 19099
  let url := s!"http://127.0.0.1:{port}/rpc"
  -- Background thread runs the server until main exits.
  let _ ← IO.asTask (Net.Server.serve port "127.0.0.1" (Server.toHandler server))
  IO.sleep 250

  IO.println "== valid ping =="
  match ← call url "ping" Json.null with
  | .ok r    => IO.println s!"  result: {r.compress}"
  | .error e => IO.println s!"  ERROR {e.code}: {e.message}"

  IO.println "== valid add(3, 4) =="
  match ← call url "add" (Json.mkObj [("a", Json.num 3), ("b", Json.num 4)]) with
  | .ok r    => IO.println s!"  result: {r.compress}"
  | .error e => IO.println s!"  ERROR {e.code}: {e.message}"

  IO.println "== invalid add (missing b) — schema should reject =="
  match ← call url "add" (Json.mkObj [("a", Json.num 1)]) with
  | .ok r    => IO.println s!"  unexpected result: {r.compress}"
  | .error e => IO.println s!"  rejected (good): {e.code} {e.message}"

  IO.println "== invalid add (b is string) =="
  match ← call url "add"
      (Json.mkObj [("a", Json.num 1), ("b", Json.str "oops")]) with
  | .ok r    => IO.println s!"  unexpected result: {r.compress}"
  | .error e => IO.println s!"  rejected (good): {e.code} {e.message}"

  IO.println "== unknown method =="
  match ← call url "nope" Json.null with
  | .ok _    => IO.println "  unexpected"
  | .error e => IO.println s!"  rejected (good): {e.code} {e.message}"

  IO.println "ok"
