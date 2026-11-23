import LeanTea

/-! # ws_echo_serve — WebSocket server used by `websocket_spec`

* `GET /`         plain HTTP on the same port (`hello`)
* `ws  /echo`     echoes every text/binary message
* `ws  /deny`     refused with 403 by the router
* `ws  /bye`      sends `bye`, then closes with 1000

```
ws_echo_serve --port 8766
``` -/

open LeanTea.Net.Http LeanTea.Net.Server LeanTea.Net.WebSocketServer

partial def echo (c : Conn) : IO Unit := do
  match ← c.recv with
  | some m => c.send m; echo c
  | none => pure ()

def router : Router := fun req => pure <|
  match req.path with
  | "/echo" => some (.ok echo)
  | "/deny" => some (.error (Response.text 403 "no\n"))
  | "/bye"  => some (.ok fun c => do c.sendText "bye"; c.close 1000 "done")
  | _ => none

def main (args : List String) : IO Unit := do
  let port := match args with
    | ["--port", p] => p.toNat?.getD 8766
    | _ => 8766
  serveConcurrent port.toUInt16 "127.0.0.1" (fun _ => pure (Response.text 200 "hello\n")) (ws := router)
