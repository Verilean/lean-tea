import LeanTea

open LeanTea.Net.Http LeanTea.Net.Server

def handler : Handler := fun req => do
  IO.eprintln s!"{req.method} {req.path} q={req.query} body={req.body.size}B"
  let resp := match req.path with
    | "/"      => Response.html 200 "<h1>hello from Lean</h1><p>It works.</p>"
    | "/echo"  =>
      let s := match String.fromUTF8? req.body with
        | some s => s
        | none => "(binary)"
      Response.text 200 s!"you sent: {s}"
    | _        => Response.notFound
  return resp

def main : IO Unit := serve (handler := handler) (port := 8765)
