# 5 · RPC — one declaration drives the wire

When you have *fifteen* endpoints whose names, parameters, and JSON
shapes have to agree between Lean and JS, you reach for `Rpc`.

`LeanTea.Rpc` is the pattern Sheet uses for its `/api/*` surface.
**One `Endpoint` record per route**, three artefacts read from it:

1. the **server's router** (which Lean handler runs)
2. the **typed JS client** (`apiSetCell`, `apiClear`, …)
3. the **discovery doc** at `/api/_endpoints` (for tooling)

When you add a new endpoint you touch *one* place; the other two update
for free.

## Smallest example

```lean
import LeanTea.Rpc

open LeanTea.Rpc

-- An endpoint is a record. Args type, return type, handler.
def setCell : Endpoint (kind : String) × (x y : Int) → IO Nat :=
  { name    := "set_cell",
    path    := "/api/set",
    method  := .get,
    handler := fun (kind, x, y) => do
      let id ← store.setCell { kind, x, y, … }
      return id }

def all : List Endpoint := [setCell, moveCell, clearCell, …]
```

Then in your server's handler (`examples/Sheet/Serve.lean`):

```lean
def handler (store : Store) : Handler :=
  Rpc.chainWith (SheetRpc.routes store) fun req => do
    -- residual non-API routes go here
    return Response.notFound
```

And in the page shell that's served to the browser:

```lean
let rpcClient := (LeanTea.Rpc.clientLib SheetRpc.all).render
-- splice into page.html via {{rpcClient}}
```

The browser now has `await apiSetCell("A1", "42")` available.
Add a new endpoint to the `all` list and a new `apiX` appears in the
client by the next reload.

## Discovery

`GET /api/_endpoints` returns a JSON document listing every endpoint,
its path, method, args, and return type. Useful for:

- Hooking up an MCP-style tool layer
- Generating an OpenAPI spec downstream
- Tooling that wants to know what endpoints exist before calling them

## Type story

Args are encoded as URL query parameters (for GET) or
`application/x-www-form-urlencoded` body (for POST). Return values
come back as JSON in the response body. The client lib handles
encoding both sides. For now, the supported types are `String`,
`Int`, `Nat`, `Bool`, plus arrays of those. For richer shapes
(objects), declare a Lean structure and add a `ToJsExpr` /
`FromJsExpr` instance.

This is intentionally narrow. If you want gRPC, use gRPC. If you want
GraphQL, use GraphQL. `LeanTea.Rpc` is the "one declaration drives the
wire" pattern for the simple case.

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
