import Lean.Data.Json
import LeanTea.Net.Http
import LeanTea.Net.Server
import LeanTea.Crypto.Password

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

/-- Who may call the HTTP transport. MCP tools often run commands or
    drive a browser, so an open endpoint is remote code execution:

    * `token`: when set, every request needs `Authorization: Bearer
      <token>` (compared in constant time).
    * Requests carrying an `Origin` header (browsers always send one on
      cross-site `fetch`) are refused unless the origin is in
      `allowedOrigins`. MCP clients are not browsers and send none.
    * A `POST` must be `Content-Type: application/json`, which a page
      cannot send cross-site without a CORS preflight we never answer.
    * When `loopbackOnly`, the `Host` header must name the loopback
      interface, so a DNS-rebinding page that resolves its own name to
      127.0.0.1 is refused. -/
structure Access where
  token          : Option String := none
  allowedOrigins : List String := []
  loopbackOnly   : Bool := true
  deriving Inhabited

private def isLoopbackHost (h : String) : Bool :=
  let name :=
    if h.startsWith "[" then ((h.drop 1).takeWhile (· != ']')).toString
    else ((h.splitOn ":").head?.getD "")
  let name := name.toLower
  name == "localhost" || name == "127.0.0.1" || name == "::1"

/-- `none` when `req` may reach the MCP handler, else the refusal. Public
    for testing. -/
def Access.check (a : Access) (req : Request) : Option Response :=
  let deny (status : Nat) (msg : String) := some (Response.text status (msg ++ "\n"))
  if a.loopbackOnly && !isLoopbackHost ((req.header? "host").getD "") then
    deny 403 "mcp: Host must be localhost"
  -- The Origin check only refuses: an allowed origin still needs the
  -- token, since any non-browser client can send any Origin.
  else if (req.header? "origin").any (!a.allowedOrigins.contains ·) then
    deny 403 "mcp: cross-origin requests are not accepted"
  else match a.token with
    | some t =>
      let auth := (req.header? "authorization").getD ""
      -- RFC 7235: the scheme is case-insensitive.
      let (scheme, cred) := match auth.splitOn " " with
        | s :: rest => (s.toLower, (" ".intercalate rest).trimAscii.toString)
        | [] => ("", "")
      let want := t.toUTF8
      let got := cred.toUTF8
      if !t.isEmpty && scheme == "bearer" && want.size == got.size &&
          LeanTea.Crypto.Password.constantTimeEq want got then none
      else deny 401 "mcp: missing or wrong bearer token"
    | none => none

/-- `Access.check` ran first: refuse, or require JSON on `POST`. -/
private def jsonPost? (req : Request) : Bool :=
  let ct := ((req.header? "content-type").getD "").toLower
  ((ct.splitOn ";").head?.getD "").trimAscii.toString == "application/json"

def httpHandler (h : Handler) (access : Access := {}) : HttpHandler := fun req => do
  if let some refusal := access.check req then return refusal
  match req.path, req.method with
  | "/mcp", "POST" =>
    if jsonPost? req then handleMcp h req
    else return Response.text 415 "mcp: Content-Type must be application/json\n"
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

/-- The bearer token from `LEANTEA_MCP_TOKEN`, or from the file named by
    `LEANTEA_MCP_TOKEN_FILE` (trailing whitespace dropped). -/
def tokenFromEnv : IO (Option String) := do
  if let some t := (← IO.getEnv "LEANTEA_MCP_TOKEN").map (·.trimAscii.toString) |>.filter (!·.isEmpty) then
    return some t
  match (← IO.getEnv "LEANTEA_MCP_TOKEN_FILE").filter (!·.isEmpty) with
  | some path =>
    let t := (← IO.FS.readFile path).trimAscii.toString
    if t.isEmpty then throw <| IO.userError s!"mcp: {path} is empty"
    return some t
  | none => return none

/-- Where `serveHttp` will listen and with what access, or why it
    refuses: a host that is not an IPv4 address (the server would
    otherwise fall back to every interface), or a non-loopback address
    without a token. Public for testing. -/
def listenPlan (host : String) (access : Access) (token : Option String)
    : Except String (String × Access) := do
  let some ip := LeanTea.Net.Server.parseIPv4? host
    | throw s!"mcp: {host} is not an IPv4 address; use 127.0.0.1 (or 0.0.0.0 with a token)"
  let loopback := ip.octets[0] == 127
  if token.any (·.isEmpty) then throw "mcp: the token is empty"
  if !loopback && token.isNone then
    throw s!"mcp: refusing to listen on {host} without a token; set LEANTEA_MCP_TOKEN or LEANTEA_MCP_TOKEN_FILE (or listen on 127.0.0.1)"
  let addr := if host == "localhost" then "127.0.0.1" else host
  return (addr, { access with token, loopbackOnly := access.loopbackOnly && loopback })

/-- Serve the HTTP transport on `host:port` (default loopback only).
    The token comes from `LEANTEA_MCP_TOKEN` / `LEANTEA_MCP_TOKEN_FILE`
    unless `access` sets one. Listening on anything but loopback without
    a token is refused: anyone who can reach the port could call every
    tool. -/
def Handler.serveHttp (h : Handler) (port : UInt16 := 8000) (host : String := "127.0.0.1")
    (access : Access := {}) : IO Unit := do
  let token ← match access.token with
    | some t => pure (some t)
    | none => tokenFromEnv
  let (addr, access) ← IO.ofExcept (listenPlan host access token)
  serve port addr (httpHandler h access)

def Handler.serveStdio (h : Handler) : IO Unit := stdioLoop h

end LeanTea.Mcp
