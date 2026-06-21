import Lean.Data.Json
import LeanTea.Net.Http
import LeanTea.Net.Server

/-! # LeanTea.Mcp — common surface for our MCP servers

Every MCP server in `examples/*Mcp/Serve.lean` was independently
hand-rolling the same JSON-RPC envelope + content shapes + stdio
loop + HTTP handler. This module collapses that to a single
contract — supply `initializeResult`, `toolsList`, and `callTool`,
get both transports for free.

```
def main : IO Unit := do
  let cfg ← IO.mkRef defaultConfig
  let h : LeanTea.Mcp.Handler := {
    initializeResult := myInitializeResult,
    toolsList        := myToolsList,
    callTool         := callTool cfg
  }
  match mode with
  | "stdio" => h.serveStdio
  | _       => h.serveHttp port host
```
-/

namespace LeanTea.Mcp

open Lean (Json)
open LeanTea.Net.Http (Request Response)
open LeanTea.Net.Server (serve)

/-- Type alias for the HTTP handler type (Request → IO Response). -/
abbrev HttpHandler := LeanTea.Net.Http.Handler

/-! ## JSON-RPC envelopes -/

/-- `{ jsonrpc, id, result }` — success envelope. -/
def jsonOk (id : Json) (result : Json) : Json :=
  Json.mkObj [("jsonrpc", "2.0"), ("id", id), ("result", result)]

/-- `{ jsonrpc, id, error: { code, message } }` — failure envelope. -/
def jsonErr (id : Json) (code : Int) (msg : String) : Json :=
  Json.mkObj [("jsonrpc", "2.0"), ("id", id),
    ("error", Json.mkObj [("code", Json.num code), ("message", Json.str msg)])]

/-! ## MCP `content` shapes — the protocol's tool-result vocabulary. -/

/-- A single `text` content item wrapped in MCP's `{ content: [...] }`. -/
def textContent (s : String) : Json :=
  Json.mkObj [("content", Json.arr #[
    Json.mkObj [("type", Json.str "text"), ("text", Json.str s)]])]

/-- Error variant of `textContent` (sets `isError: true`). -/
def errContent (s : String) : Json :=
  Json.mkObj [("isError", Json.bool true), ("content", Json.arr #[
    Json.mkObj [("type", Json.str "text"), ("text", Json.str s)]])]

/-- `{ content: [image, text] }` — image plus an optional caption. -/
def imageContent (mime base64 caption : String) : Json :=
  Json.mkObj [("content", Json.arr #[
    Json.mkObj [("type", Json.str "image"), ("data", Json.str base64),
                ("mimeType", Json.str mime)],
    Json.mkObj [("type", Json.str "text"), ("text", Json.str caption)]])]

/-! ## Schema construction helpers. -/

/-- One property entry for a tool's input-schema. -/
def argSchema (name typ desc : String) : String × Json :=
  (name, Json.mkObj [("type", Json.str typ), ("description", Json.str desc)])

/-- Wrap a `(name, description, properties, required)` quadruple into
    MCP's `tools/list` JSON shape. -/
def toolDef (name desc : String) (props : Array (String × Json))
    (required : Array String) : Json :=
  Json.mkObj [
    ("name", Json.str name),
    ("description", Json.str desc),
    ("inputSchema", Json.mkObj [
      ("type", Json.str "object"),
      ("properties", Json.mkObj props.toList),
      ("required", Json.arr (required.map Json.str))
    ])
  ]

/-- Boilerplate `initialize` response. Most servers don't need to
    customise this; supply a custom `serverInfo` if you do. -/
def defaultInitializeResult (name : String) (version : String := "0.1.0") : Json :=
  Json.mkObj [
    ("protocolVersion", Json.str "2024-11-05"),
    ("capabilities", Json.mkObj [
      ("tools", Json.mkObj [("listChanged", Json.bool false)])
    ]),
    ("serverInfo", Json.mkObj [
      ("name", Json.str name),
      ("version", Json.str version)
    ])
  ]

/-! ## The handler contract -/

/-- Supply these three values and you get stdio + HTTP transports
    for free. `callTool` is invoked with the JSON `arguments` object
    from `tools/call`; return a `content`-shaped `Json` (build via
    `textContent` / `errContent` / `imageContent`). -/
structure Handler where
  initializeResult : Json
  toolsList        : Json
  callTool         : String → Json → IO Json

/-! ## Internal dispatch — shared by both transports. -/

private def lookupStr (j : Json) (key : String) (default := "") : String :=
  (j.getObjVal? key).toOption.bind (·.getStr?.toOption) |>.getD default

private def lookupJson (j : Json) (key : String) (default := Json.null) : Json :=
  (j.getObjVal? key).toOption.getD default

private def dispatchOnce (h : Handler) (j : Json) : IO (Option Json) := do
  let id     := lookupJson j "id"
  let method := lookupStr j "method"
  let params := lookupJson j "params" (Json.mkObj [])
  if method.startsWith "notifications/" then return none
  let resp ← match method with
    | "initialize" => pure (jsonOk id h.initializeResult)
    | "tools/list" => pure (jsonOk id h.toolsList)
    | "tools/call" =>
      let name := lookupStr params "name"
      let args := lookupJson params "arguments" (Json.mkObj [])
      let res ← h.callTool name args
      pure (jsonOk id res)
    | "" => pure (jsonErr id (-32600) "missing method")
    | _  => pure (jsonErr id (-32601) s!"method not found: {method}")
  return some resp

/-! ## HTTP transport -/

def handleMcp (h : Handler) (req : Request) : IO Response := do
  let body := match String.fromUTF8? req.body with
    | some b => b | none => ""
  match Json.parse body with
  | .error e =>
    return Response.html 200 (jsonErr Json.null (-32700) s!"parse error: {e}").compress
  | .ok j =>
    match ← dispatchOnce h j with
    | none => return Response.text 204 ""
    | some result =>
      return {
        status := 200,
        headers := #[("content-type", "application/json"), ("cache-control", "no-store")],
        body := result.compress.toUTF8
      }

def httpHandler (h : Handler) : HttpHandler := fun req => do
  match req.path, req.method with
  | "/mcp", "POST" => handleMcp h req
  | "/mcp", "GET"  => return Response.html 200 "<h1>mcp</h1>"
  | _, _           => return Response.notFound

/-! ## stdio transport -/

partial def stdioLoop (h : Handler) : IO Unit := do
  let stdin  ← IO.getStdin
  let stdout ← IO.getStdout
  let mut active := true
  while active do
    let line ← stdin.getLine
    if line.isEmpty then active := false
    else
      let trimmed := line.trimAscii.toString
      if trimmed.isEmpty then continue
      match Json.parse trimmed with
      | .error e =>
        stdout.putStr ((jsonErr Json.null (-32700) s!"parse error: {e}").compress ++ "\n")
        stdout.flush
      | .ok j =>
        match ← dispatchOnce h j with
        | none      => pure ()
        | some resp => stdout.putStr (resp.compress ++ "\n"); stdout.flush

/-! ## Top-level launcher conveniences. -/

def Handler.serveHttp (h : Handler) (port : UInt16 := 8000) (host : String := "0.0.0.0") : IO Unit := do
  serve port host (httpHandler h)

def Handler.serveStdio (h : Handler) : IO Unit := stdioLoop h

end LeanTea.Mcp
