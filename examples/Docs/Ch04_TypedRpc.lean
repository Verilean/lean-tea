import LeanTea

/-! # Chapter 4 — One declaration drives the wire

Typed RPC is where lean-elm earns its name. You write one `Endpoint`
record per route; the framework feeds it to:

  * `Rpc.dispatch` / `chainWith`  → server-side routing
  * `Rpc.clientLib`               → generated JS client functions

So the handler signature, the client call site, the URL, the HTTP
method, and the parameter list all stay in sync because they come
from one source. Renaming an endpoint is a one-line edit; deleting
one breaks the build at every call site.

This chapter walks two endpoints (`ping`, `double`) and shows three
artefacts the same declaration produces:

1. The generated client JS (`apiPing`, `apiDouble`)
2. A browser-side `Block` that calls them — *also* built via the
   typed JS DSL, so the prose is one composition end-to-end
3. The matching server-side `Route` list

Run:

    lake exe doc_ch04

The output prints all three artefacts; verifying they line up is the
verification. -/

open LeanTea LeanTea.Rpc
open LeanTea.Js LeanTea.Js.E LeanTea.Js.S
open Lean (Json)

namespace Ch04

/-! ## 1. Endpoint declarations -/

/-- A no-arg health check that returns JSON `{ok: true}`. -/
def ping : Endpoint := {
  name := "apiPing", path := "/api/ping", method := "GET",
  params := [], output := .json
}

/-- A one-arg endpoint that doubles its input. POST + form carrier
    is the right call for write-shaped endpoints; for a pure read,
    `Carrier.query` would put the arg in the URL. -/
def double : Endpoint := {
  name := "apiDouble", path := "/api/double", method := "POST",
  params := ["n"], carrier := .form, output := .text
}

def endpoints : List Endpoint := [ping, double]

/-! ## 2. Server handlers — they receive params in declaration order -/

def handlePing : Handler := fun _ =>
  return (Json.mkObj [("ok", Json.bool true)]).compress

def handleDouble : Handler := fun ps => do
  let n : Int := (ps[0]?.bind String.toInt?).getD 0
  return toString (n * 2)

def routes : List Route := [
  { ep := ping,   handler := handlePing },
  { ep := double, handler := handleDouble }
]

/-! ## 3. A browser-side block that calls the generated client

`apiPing` and `apiDouble` come from `Rpc.clientLib endpoints`; we
just call them as plain identifiers, the same way the live Canvas
client calls `apiAddShape` and friends. -/

def browserBlock : Block := [
  afn "demo" [] [
    constV "health" (await_ (call (i "apiPing") [])),
    doE (mcall (i "console") "log" [s "ping ->", i "health"]),
    constV "doubled" (await_ (call (i "apiDouble") [s "21"])),
    doE (mcall (i "console") "log" [s "double(21) ->", i "doubled"])
  ],
  doE (call (i "demo") [])
]

end Ch04

def main : IO Unit := do
  IO.println "== Chapter 4 — One declaration drives the wire =="
  IO.println ""

  IO.println "── 1. Generated client JS (from endpoints) ───────────────"
  IO.println (Rpc.clientLib Ch04.endpoints).render
  IO.println ""

  IO.println "── 2. Browser-side caller, also built via the JS DSL ─────"
  IO.println Ch04.browserBlock.render
  IO.println ""

  IO.println "── 3. Server-side: one Endpoint, one handler ─────────────"
  for r in Ch04.routes do
    IO.println s!"  {r.ep.method} {r.ep.path}  →  {r.ep.name}"

  IO.println ""
  IO.println "── Sanity: run the handlers in-process ───────────────────"
  let pong ← Ch04.handlePing []
  let twice ← Ch04.handleDouble ["7"]
  IO.println s!"  ping     → {pong}"
  IO.println s!"  double 7 → {twice}"
  IO.println ""
  IO.println "ok"
