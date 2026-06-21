import LeanTea

open Lean (Json)
open LeanTea.JsonRpc

def main (args : List String) : IO Unit := do
  let url : String :=
    match args with
    | "--url" :: u :: _ => u
    | _ => "http://127.0.0.1:19099/rpc"

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
  | .ok r    => IO.println s!"  unexpected: {r.compress}"
  | .error e => IO.println s!"  rejected (good): {e.code} {e.message}"

  IO.println "== invalid add (b is string) =="
  match ← call url "add" (Json.mkObj [("a", Json.num 1), ("b", Json.str "x")]) with
  | .ok r    => IO.println s!"  unexpected: {r.compress}"
  | .error e => IO.println s!"  rejected (good): {e.code} {e.message}"

  IO.println "== unknown method =="
  match ← call url "nope" Json.null with
  | .ok _    => IO.println "  unexpected"
  | .error e => IO.println s!"  rejected (good): {e.code} {e.message}"

  IO.println "ok"
