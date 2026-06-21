import Lean.Data.Json
import LeanTea.Net.Http
import LeanTea.Net.Server
import LeanTea.Auth

/-! # JSON-RPC 2.0 + a tiny JSON Schema

Three things in one module:

1. `Schema` — a runtime description of expected JSON shapes used to
   validate JSON-RPC arguments before they reach a handler. Just
   enough to cover what real method declarations need (string,
   number, bool, object, array, nullable, enum).
2. `Server` — register methods with their schema and handler; emit
   a `Net.Http.Handler` that speaks JSON-RPC 2.0 over POST.
3. `Client` — shell-out-to-curl outbound RPC, so a lean-elm app can
   call other JSON-RPC services (own or third-party) by name. -/

namespace LeanTea.JsonRpc

open Lean (Json)

/-! ## Schema -/

inductive Schema where
  | string_
  | number
  | bool_
  | nullT
  | any
  | enum (alts : List String)
  /-- Array with a homogeneous element schema. -/
  | array (item : Schema)
  /-- Object with named fields. Fields not listed in `required` may
      be missing; unknown fields are ignored. -/
  | object (fields : List (String × Schema)) (required : List String := [])
  /-- Union (e.g. `Schema.nullable s = Schema.union [s, .nullT]`). -/
  | union (alts : List Schema)
  deriving Inhabited

namespace Schema

def nullable (s : Schema) : Schema := .union [s, .nullT]

/-- Tiny tag-only label so error messages don't dump huge structures. -/
def reprOf : Schema → String
  | .string_       => "string"
  | .number        => "number"
  | .bool_         => "bool"
  | .nullT         => "null"
  | .any           => "any"
  | .enum _        => "enum"
  | .array _       => "array"
  | .object _ _    => "object"
  | .union _       => "union"

mutual

partial def validateAt (path : String) : Schema → Json → Except String Unit
  | .string_, .str _   => .ok ()
  | .number,  .num _   => .ok ()
  | .bool_,   .bool _  => .ok ()
  | .nullT,   .null    => .ok ()
  | .any,     _        => .ok ()
  | .enum alts, .str s =>
    if alts.contains s then .ok ()
    else .error s!"{path}: {s} not in {alts}"
  | .array item, .arr js => validateArr path item js 0
  | .object fields required, j@(.obj _) =>
    match validateReq path j required with
    | .error e => .error e
    | .ok _    => validateFields path j fields
  | .union alts, v => validateUnion path alts v
  | s, v => .error s!"{path}: expected {reprOf s}, got {v.compress}"

partial def validateArr (path : String) (item : Schema)
    (js : Array Json) (i : Nat) : Except String Unit :=
  if h : i < js.size then
    match validateAt s!"{path}[{i}]" item js[i] with
    | .ok _    => validateArr path item js (i + 1)
    | .error e => .error e
  else .ok ()

partial def validateFields (path : String) (j : Json)
    : List (String × Schema) → Except String Unit
  | [] => .ok ()
  | (k, sub) :: rest =>
    match j.getObjVal? k with
    | .ok v =>
      match validateAt s!"{path}.{k}" sub v with
      | .ok _    => validateFields path j rest
      | .error e => .error e
    | .error _ => validateFields path j rest

partial def validateReq (path : String) (j : Json)
    : List String → Except String Unit
  | [] => .ok ()
  | r :: rest =>
    match j.getObjVal? r with
    | .ok _    => validateReq path j rest
    | .error _ => .error s!"{path}: missing required field {r}"

partial def validateUnion (path : String) : List Schema → Json → Except String Unit
  | [], _      => .error s!"{path}: no union alt matched"
  | a :: rest, v =>
    match validateAt path a v with
    | .ok _    => .ok ()
    | .error _ => validateUnion path rest v

end

/-- Walk the schema against a `Json` value. Reports the first error
    encountered, with a JSON-path-ish breadcrumb so users can find
    the offending field. -/
def validate (s : Schema) (j : Json) : Except String Unit := validateAt "$" s j

/-- Convert to a draft-07-ish JSON Schema object so MCP / tool
    discovery responses don't need a parallel encoding. -/
partial def toJson : Schema → Json
  | .string_         => Json.mkObj [("type", "string")]
  | .number          => Json.mkObj [("type", "number")]
  | .bool_           => Json.mkObj [("type", "boolean")]
  | .nullT           => Json.mkObj [("type", "null")]
  | .any             => Json.mkObj []
  | .enum alts       => Json.mkObj [("enum", Json.arr (alts.map Json.str).toArray)]
  | .array item      =>
    Json.mkObj [("type", "array"), ("items", toJson item)]
  | .object fields required =>
    let props := fields.map fun (k, s) => (k, toJson s)
    Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj props),
      ("required", Json.arr (required.map Json.str).toArray)
    ]
  | .union alts      =>
    Json.mkObj [("anyOf", Json.arr (alts.map toJson).toArray)]

end Schema

/-! ## Method declarations -/

structure Method where
  name        : String
  description : String := ""
  params      : Schema := .object [] []
  result      : Schema := .any

/-! ## Server -/

structure Route where
  method  : Method
  /-- Receives the already-validated `params` value and produces a
      `result` JSON. Errors should `throw` an `IO.Error`; the server
      catches and emits a JSON-RPC error envelope. -/
  handler : Json → IO Json

structure Server where
  routes : List Route := []

namespace Server

def addRoute (s : Server) (r : Route) : Server :=
  { s with routes := s.routes ++ [r] }

private def errObj (id : Json) (code : Int) (msg : String) : Json :=
  Json.mkObj [
    ("jsonrpc", "2.0"), ("id", id),
    ("error", Json.mkObj [
      ("code", Json.num code), ("message", Json.str msg)])
  ]

private def okObj (id : Json) (result : Json) : Json :=
  Json.mkObj [
    ("jsonrpc", "2.0"), ("id", id), ("result", result)
  ]

/-- Look up a route by JSON-RPC method name. -/
def routeFor? (s : Server) (name : String) : Option Route :=
  s.routes.find? (fun r => r.method.name == name)

/-- Dispatch a single parsed JSON-RPC request. Returns `none` if the
    incoming message was a notification (no `id`, no response per
    spec). -/
def dispatch (s : Server) (req : Json) : IO (Option Json) := do
  let id := (req.getObjVal? "id").toOption.getD Json.null
  let method : String :=
    match (req.getObjVal? "method").toOption.bind (·.getStr?.toOption) with
    | some m => m
    | none   => ""
  let params := (req.getObjVal? "params").toOption.getD (Json.mkObj [])

  -- Notifications (no `id`) get no response per spec.
  let isNotification := req.getObjVal? "id" |>.toOption |>.isNone

  if method.isEmpty then
    if isNotification then return none
    else return some (errObj id (-32600) "missing method")

  match s.routeFor? method with
  | none =>
    if isNotification then return none
    else return some (errObj id (-32601) s!"method not found: {method}")
  | some r =>
    -- Validate params against the method's schema before handing off.
    match Schema.validate r.method.params params with
    | .error e =>
      if isNotification then return none
      else return some (errObj id (-32602) s!"invalid params: {e}")
    | .ok _ =>
      try
        let res ← r.handler params
        if isNotification then return none
        else return some (okObj id res)
      catch ex =>
        if isNotification then return none
        else return some (errObj id (-32603) s!"internal error: {ex}")

/-- Wrap the server as a `Net.Http.Handler` that listens on `path`
    for POST and emits JSON-RPC responses. Anything else returns
    `Response.notFound` so the result composes with other handlers
    via `Rpc.chainWith` / similar wrappers. -/
def toHandler (s : Server) (path : String := "/rpc") : Net.Http.Handler :=
  fun req => do
    if req.path != path then return Net.Http.Response.notFound
    if req.method != "POST" then
      return Net.Http.Response.notFound
    let body := match String.fromUTF8? req.body with
      | some b => b | none => ""
    match Json.parse body with
    | .error e =>
      let env := errObj Json.null (-32700) s!"parse error: {e}"
      return {
        status := 200,
        headers := #[("content-type", "application/json"),
                     ("cache-control", "no-store")],
        body := env.compress.toUTF8
      }
    | .ok j =>
      match ← s.dispatch j with
      | none =>
        return { status := 204, headers := #[], body := .empty }
      | some env =>
        return {
          status := 200,
          headers := #[("content-type", "application/json"),
                       ("cache-control", "no-store")],
          body := env.compress.toUTF8
        }

end Server

/-! ## Client

Outbound JSON-RPC over HTTP(S). Shells out to `curl(1)` so the same
implementation handles plain HTTP, HTTPS, redirects, and auth
headers without needing TLS in the Lean stdlib (which we don't
have).

The transport is identical to `LeanTea.Auth.curlPost`; we just send
JSON instead of form data. -/

structure CallError where
  code    : Int
  message : String
  data?   : Option Json := none
  deriving Inhabited

/-- Issue one JSON-RPC call. Returns the parsed `result` on success
    or a `CallError` matching the JSON-RPC error envelope. Network /
    parse failures come back as code `0` / `-32700`. -/
def call (url : String) (method : String) (params : Json := Json.null)
    (headers : Array String := #[]) (id : Json := Json.num 1)
    : IO (Except CallError Json) := do
  let mut payload : List (String × Json) := [
    ("jsonrpc", Json.str "2.0"),
    ("method", Json.str method),
    ("id", id)
  ]
  -- Only include `params` if non-null so calls to strict servers
  -- that reject null `params` still work.
  if params != Json.null then
    payload := payload ++ [("params", params)]
  let body := (Json.mkObj payload).compress
  let allHeaders : Array String :=
    #["content-type: application/json"] ++ headers
  -- Use Auth's curlPost but swap the content-type header. The
  -- function already accepts an extra `headers` array, so we just
  -- override there.
  let mut args : Array String := #[
    "-sS", "--max-time", "30",
    "-X", "POST",
    "-w", "\n___STATUS:%{http_code}",
    "-d", body, url
  ]
  for h in allHeaders do args := args ++ #["-H", h]
  let out ← IO.Process.output { cmd := "curl", args := args }
  let raw := out.stdout
  let (responseBody, status) :=
    match raw.splitOn "\n___STATUS:" with
    | [b, codeS] => (b, codeS.trimAscii.toString.toNat?.getD 0)
    | _          => (raw, 0)
  if status == 0 then
    return .error { code := 0, message := "network error" }
  match Json.parse responseBody with
  | .error e =>
    return .error { code := -32700, message := s!"parse error: {e}" }
  | .ok j =>
    match j.getObjVal? "error" with
    | .ok errJ =>
      let code : Int :=
        match errJ.getObjVal? "code" with
        | .ok (.num n) => n.toFloat.toInt32.toInt
        | _            => -32603
      let msg : String :=
        match (errJ.getObjVal? "message").toOption.bind (·.getStr?.toOption) with
        | some m => m
        | none   => "(no message)"
      let data? := (errJ.getObjVal? "data").toOption
      return .error { code, message := msg, data? := data? }
    | .error _ =>
      match j.getObjVal? "result" with
      | .ok r     => return .ok r
      | .error _  =>
        return .error { code := -32603, message := "no result/error in response" }

/-- Convenience: call a method that ignores its result (e.g. a
    notification request). Throws an `IO.userError` on transport
    failure. -/
def notify (url : String) (method : String) (params : Json := Json.null)
    (headers : Array String := #[]) : IO Unit := do
  let _ ← call url method params headers Json.null
  pure ()

end LeanTea.JsonRpc
