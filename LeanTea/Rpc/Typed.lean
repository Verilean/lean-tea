import LeanTea.Net.Http
import LeanTea.Js
import Lean.Data.Json
import Lean.Data.Json.FromToJson

/-! # LeanTea.Rpc.Typed — end-to-end typed RPC (Servant, done properly)

The legacy `LeanTea.Rpc` is stringly-typed: an `Endpoint` carries a
`List String` of parameter names and a `Handler := List String → IO
String`. Nothing checks that the server decodes what the client sent,
or that the client consumes what the server returns — the wire format
can drift silently on either side.

This module makes request and response *types* part of the endpoint:

```lean
structure SetCellReq  where ref : String; formula : String
  deriving Lean.ToJson, Lean.FromJson
structure SetCellResp where ok : Bool;   value : String
  deriving Lean.ToJson, Lean.FromJson

def setCell : Endpoint SetCellReq SetCellResp :=
  { name := "apiSetCell", path := "/api/set" }
```

`Endpoint α β` is the single source of truth. The server handler is
`α → IO β` — no manual string decoding. Dispatch decodes the request
body JSON to `α` (400 on failure) and encodes the `β` result. The
request/response types are ordinary Lean structures with derived JSON
codecs, so both sides reference the same definition and the wire shape
cannot drift.

The **client** side (browser) is handled by the LeanJs type-check
bridge in `LeanJs/Rpc.lean`: the same `Endpoint α β` generates a typed
client stub `apiSetCell : SetCellReq → Async SetCellResp` whose call
sites are checked against the shared types by re-using Lean's own
elaborator. This module owns the server half + the shared endpoint
type; the client half builds on it. -/

namespace LeanTea.Rpc.Typed

open LeanTea.Net.Http
open Lean (Json ToJson FromJson toJson fromJson?)

/-! ## The typed endpoint -/

/-- Where the request payload lives on the wire. Typed RPC defaults to
    a JSON body (the natural carrier for a structured `α`); `query` is
    available for read-shaped GETs whose `α` is a flat record of
    strings/numbers. -/
inductive Carrier
  | jsonBody   -- `α` serialized as a JSON request body
  | query      -- `α`'s fields as `?k=v` (flat records only)
  deriving BEq, Repr, Inhabited

/-- An endpoint typed by its request (`α`) and response (`β`). The
    types are phantom — they carry no runtime data — but they thread
    through `serve` and the client-stub generator so both sides are
    checked against the same `α` / `β`.

    `reqType` / `respType` are the type names as text. They're
    redundant with `α` / `β` for the server (which uses the real
    types), but the client-side artifacts are generated *source*
    (a JS function and a Lean type-check stub), so they need the names
    as strings. Keeping them on the endpoint is what makes one
    declaration drive server + client JS + client type-check. -/
structure Endpoint (α β : Type) where
  /-- Client stub function name (also used for docs / the `.d.ts`). -/
  name     : String
  path     : String
  reqType  : String
  respType : String
  method   : String := "POST"
  carrier  : Carrier := .jsonBody
  deriving Inhabited, Repr

/-! ## Server side

`serve` turns a typed handler `α → IO β` into a type-erased `Route`.
The erasure lets a heterogeneous `List Route` (endpoints with different
`α`/`β`) share one dispatcher, while each individual `serve` call stays
fully type-checked. -/

/-- A dispatch-ready, type-erased route. -/
structure Route where
  path   : String
  method : String
  run    : Net.Http.Handler

/-- Decode the request body as `α`. For `jsonBody`, parse the body as
    JSON then `fromJson?`. Returns a 400 response text on failure. -/
private def decodeRequest [FromJson α] (ep : Endpoint α β) (req : Request)
    : Except String α :=
  match ep.carrier with
  | .jsonBody =>
    let bodyStr := (String.fromUTF8? req.body).getD ""
    match Json.parse bodyStr with
    | .error e => .error s!"invalid JSON body: {e}"
    | .ok j    => (fromJson? j : Except String α)
  | .query =>
    -- Query carrier: the whole query string is treated as a JSON
    -- object `{"k":"v",...}` after a tiny transform. Kept minimal —
    -- flat string/number records only.
    let pairs := (req.query.splitOn "&").filterMap fun kv =>
      match kv.splitOn "=" with
      | [k, v] => some (k, Json.str v)
      | _      => none
    (fromJson? (Json.mkObj pairs) : Except String α)

/-- Mount a typed handler as a `Route`. The handler works entirely in
    typed Lean: `α → IO β`. Decode failures become `400`; the `β`
    result is JSON-encoded with a `200`. -/
def serve [FromJson α] [ToJson β] (ep : Endpoint α β) (h : α → IO β) : Route :=
  { path := ep.path, method := ep.method,
    run := fun req => do
      match decodeRequest ep req with
      | .error e => return Response.jsonError 400 s!"{ep.name}: {e}"
      | .ok a =>
        let b ← h a
        return Response.jsonObj 200 b }

/-- Route a request through `routes`, falling back to `fallback` (404
    by default) when nothing matches. -/
def dispatch (routes : List Route)
    (fallback : Net.Http.Handler := fun _ => return Response.notFound)
    : Net.Http.Handler := fun req => do
  for r in routes do
    if r.path == req.path && r.method == req.method then
      return ← r.run req
  fallback req

/-- Like `dispatch` but chains to an existing `Net.Http.Handler`
    (static files, auth callbacks, the legacy string RPC) for
    unmatched requests. -/
def chainWith (routes : List Route) (next : Net.Http.Handler)
    : Net.Http.Handler := fun req => do
  for r in routes do
    if r.path == req.path && r.method == req.method then
      return ← r.run req
  next req

/-! ## Client side — generated from the same `Endpoint`

Two artifacts come out of one endpoint, both referring to the same
request/response types:

  * `clientFn` — the JavaScript the browser runs: `async function
    apiFoo(req) { … fetch … JSON.stringify(req) … return r.json() }`.
  * `stubDecl` — a Lean `opaque apiFoo : Req → Async Resp` line for the
    LeanJs type-check harness (`LeanJs.TypeCheck`), so a `.leanjs`
    client that calls `apiFoo` is checked against the server's types.

Because both are generated from `ep.reqType` / `ep.respType`, the
wire contract, the runtime call, and the compile-time check can't
disagree. -/

open LeanTea.Js.E LeanTea.Js.S in
/-- The browser client function for this endpoint. Serializes the whole
    request value as a JSON body and parses the JSON response. -/
def Endpoint.clientFn (ep : Endpoint α β) : LeanTea.Js.Stmt :=
  let opts := obj [
    ("method",  s ep.method),
    ("body",    mcall (i "JSON") "stringify" [i "req"]),
    ("headers", obj [("content-type", s "application/json")])
  ]
  let body : LeanTea.Js.Block := [
    constV "r" (await_ (call (i "fetch") [s ep.path, opts])),
    retE (await_ (mcall (i "r") "json"))
  ]
  afn ep.name ["req"] body

/-- The Lean type-check stub for this endpoint: `opaque apiFoo :
    Req → Async Resp`. Fed to `LeanJs.TypeCheck.check` as part of the
    prelude so the client's calls to `apiFoo` are type-checked against
    `Req` / `Resp`. -/
def Endpoint.stubDecl (ep : Endpoint α β) : String :=
  s!"opaque {ep.name} : {ep.reqType} → Async {ep.respType}"

end LeanTea.Rpc.Typed
