# 5 · RPC — one typed declaration drives the wire

When you have endpoints whose request and response shapes have to agree
between the Lean server and the browser client, you reach for
`LeanTea.Rpc.Typed`. The request and response **types** live in the
endpoint, and both sides are checked against them — a wrong field is a
compile error, not a runtime surprise.

**One `Endpoint α β` per route** drives three artefacts:

1. the **server handler** — typed `α → IO β`, no manual decoding
2. the **browser JS client** (`apiSetCell`, …), generated from the endpoint
3. the **client type-check** — a `.leanjs` client is elaborated against
   the same `α` / `β`

Add or change an endpoint in one place; a mismatch surfaces at compile
time on whichever side is out of date.

## Smallest example

Define the wire types once (real Lean structures with derived JSON
codecs), then the endpoint:

```lean
import LeanTea.Rpc.Typed
open LeanTea.Rpc.Typed

structure SetCellReq  where ref : String; formula : String
  deriving Lean.ToJson, Lean.FromJson
structure SetCellResp where ok : Bool;   value : String
  deriving Lean.ToJson, Lean.FromJson

def setCell : Endpoint SetCellReq SetCellResp :=
  { name := "apiSetCell", path := "/api/set",
    reqType := "SetCellReq", respType := "SetCellResp" }
```

Server — the handler works entirely in typed Lean:

```lean
def handleSetCell (r : SetCellReq) : IO SetCellResp :=
  return { ok := true, value := s!"{r.ref}={r.formula}" }

def app : Handler := dispatch [ serve setCell handleSetCell ]
```

`serve` decodes the request body to `SetCellReq` via the derived codec
(a malformed or wrong-shape body → `400`, since the wire carries
untrusted bytes) and encodes the `SetCellResp` result. The handler only
ever sees a valid, typed request.

Client JS, generated from the same endpoint:

```lean
let rpcClient := LeanTea.Js.renderBlock [setCell.clientFn]
-- async function apiSetCell(req){ … JSON.stringify(req) … r.json() }
-- splice into page.html via {{rpcClient}}
```

## Both sides, one type — enforced

`LeanJs.TypeCheck` type-checks the browser client against the same
types. LeanJs has no type system of its own, so instead of building
one, it emits the client to real Lean and lets Lean's elaborator do the
check (`async`→`do`, `await`→`←`); the shared types are `import`ed, not
re-declared, so there is a single definition.

```text
let req  := SetCellReq { ref: r, formula: f };
let resp := await apiSetCell(req);
resp.value      -- ✓ SetCellResp has `value`
resp.valueX     -- ✗ compile error: SetCellResp.valueX doesn't exist
```

`examples/Tests/TypedRpcSpec.lean` is the end-to-end proof: one
endpoint, a server round-trip (200 / 400 / 404), a client that
type-checks, and a client with a mistyped field that is rejected.

## Legacy stringly RPC

The older `LeanTea.Rpc` (`Endpoint` with `params : List String`,
`Handler := List String → IO String`) still ships for back-compat. It
generates a client the same way but performs no type-checking — prefer
`LeanTea.Rpc.Typed` for new code.

## Scope

This is intentionally narrow. If you want gRPC, use gRPC. If you want
GraphQL, use GraphQL. `LeanTea.Rpc.Typed` is the "one typed declaration
drives the wire" pattern for the common REST-shaped case.

## JSON-RPC sibling

`LeanTea.JsonRpc` is the JSON-RPC 2.0 envelope shared by every MCP
server (Chapter 8). It's structurally similar — one tool declaration
per logical method, one dispatch table — but the wire format is the
JSON-RPC `{jsonrpc:"2.0", method, params, id}` shape rather than a
REST surface. `LeanTea.Mcp` builds the stdio + HTTP transports on top
of it.

## MCP integration

Sheet exposes a curated subset of its `Rpc` endpoints as MCP tools at
`POST /mcp` via `LeanTea.Mcp.Handler` (Chapter 8). The tool list is
hand-written because tool descriptions matter for LLMs — they're not
just type signatures, they're the model's *only* documentation.

## When to *not* use this

- **Endpoint count is < 5** and stable — the boilerplate of `Endpoint`
  outweighs the savings. Hand-write the routes in `handler`.
- **You need streaming responses** — RPC is request/response. For SSE
  (LLM streaming, live updates), add a separate handler outside the
  Rpc surface.
- **You're calling a third-party API** — `Rpc` is for *your* endpoints.
  For OpenAI / LM Studio / a Postgres REST shim, write a normal Lean
  client (see `LeanTea.Llm.Openai` for the streaming-OpenAI pattern).

The remaining chapters cover the pieces beyond the Elm-style + RPC
core: the LeanJs subset, the `.html` template engine, and the MCP
servers.
