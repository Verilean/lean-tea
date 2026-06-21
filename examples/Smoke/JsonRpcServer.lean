import LeanTea

open Lean (Json)
open LeanTea LeanTea.JsonRpc

/-- Standalone JSON-RPC server with two methods, used by the smoke
    test for the curl-backed client. -/

def pingMethod : Method := {
  name := "ping",
  description := "Health-check",
  params := .object [] [],
  result := .object [("pong", .bool_), ("n", .number)] ["pong", "n"]
}

def addMethod : Method := {
  name := "add",
  description := "Sum two numbers",
  params := .object [("a", .number), ("b", .number)] ["a", "b"],
  result := .number
}

def server : Server := {
  routes := [
    { method := pingMethod,
      handler := fun _ =>
        return Json.mkObj [("pong", Json.bool true), ("n", Json.num 42)] },
    { method := addMethod,
      handler := fun args => do
        let a : Int := (args.getObjVal? "a").toOption.bind (·.getInt?.toOption) |>.getD 0
        let b : Int := (args.getObjVal? "b").toOption.bind (·.getInt?.toOption) |>.getD 0
        return Json.num (a + b) }
  ]
}

def main (args : List String) : IO Unit := do
  let port : UInt16 :=
    match args with
    | "--port" :: v :: _ => (v.toNat?.getD 19099).toUInt16
    | _ => 19099
  Net.Server.serve port "127.0.0.1" (Server.toHandler server)
