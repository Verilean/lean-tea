import LeanTea.Net.HttpClient
import LeanTea.Net.WebSocket
import LeanTea.Json.Helpers
import Lean.Data.Json

/-! # LeanTea.Cdp — minimal Chrome DevTools Protocol client

A shared driver used by both `ChromeCdpMcp.Serve` (the LLM-facing MCP
server) and `LeanTea.WebSpec` (the typed deterministic E2E test
framework). Lists tabs via the `/json` REST endpoint and dials
per-target WebSocket connections for one command at a time.

Connections are intentionally one-shot — the MCP server is stateless
and WebSpec wants to fail cleanly between assertions. Pooling can
be added later if a benchmark says so. -/

namespace LeanTea.Cdp

open LeanTea.Net.WebSocket
open Lean (Json)

/-! ## REST + WebSocket plumbing -/

/-- GET the URL and parse the body as JSON. -/
def httpGetJson (url : String) : IO Json := do
  let parsed ← match LeanTea.Net.HttpClient.parseUrl url with
    | some u => pure u
    | none => throw <| IO.userError s!"cdp: bad URL: {url}"
  let resp ← LeanTea.Net.HttpClient.request "GET" parsed
  if resp.status >= 400 then
    throw <| IO.userError s!"cdp GET {url}: {resp.status}\n{resp.bodyText}"
  match Json.parse resp.bodyText with
  | .ok j    => return j
  | .error e => throw <| IO.userError s!"cdp: bad JSON from {url}: {e}"

/-- Look up the `webSocketDebuggerUrl` for `targetId` by hitting
    `/json` and finding the matching record. -/
def wsUrlOfTarget (base : String) (targetId : String) : IO String := do
  let j ← httpGetJson (base ++ "/json")
  let arr := match j.getArr? with | .ok a => a | .error _ => #[]
  let found := arr.findSome? fun t =>
    if t.getStrD "id" == targetId then t.getStrOpt "webSocketDebuggerUrl"
    else none
  match found with
  | some u => return u
  | none   => throw <| IO.userError s!"cdp: target {targetId} not found"

/-- Send one CDP command on a fresh WebSocket; wait for the matching
    response; close. Returns the `result` field of the response, or
    throws on `error`. -/
partial def cdpCommand (wsUrl : String) (method : String)
    (params : Json := Json.mkObj []) : IO Json := do
  let conn ← connect wsUrl
  let req := Json.mkObj [
    ("id",     Json.num 1),
    ("method", Json.str method),
    ("params", params)
  ]
  sendText conn req.compress
  let rec waitFor (depth : Nat) : IO Json := do
    if depth > 200 then throw <| IO.userError s!"cdp: too many events while waiting for {method}"
    let raw ← recvText conn
    match Json.parse raw with
    | .error e => throw <| IO.userError s!"cdp: bad JSON: {e}\n{raw}"
    | .ok j =>
      if j.getNatD "id" == 1 then
        match (j.getObjVal? "error").toOption with
        | some e =>
          close conn
          throw <| IO.userError s!"cdp error: {e.compress}"
        | none   =>
          close conn
          return (j.getObjVal? "result").toOption.getD (Json.mkObj [])
      else waitFor (depth + 1)
  waitFor 0

/-! ## Target discovery + creation -/

structure TargetInfo where
  id    : String
  title : String
  url   : String
  wsUrl : String
  deriving Inhabited, Repr

private def targetOfJson (t : Json) : TargetInfo := {
  id    := t.getStrD "id",
  title := t.getStrD "title",
  url   := t.getStrD "url",
  wsUrl := t.getStrOpt "webSocketDebuggerUrl" |>.getD ""
}

/-- List every open tab. -/
def listTargets (base : String) : IO (Array TargetInfo) := do
  let j ← httpGetJson (base ++ "/json")
  let arr := match j.getArr? with | .ok a => a | .error _ => #[]
  return arr.map targetOfJson

/-- First tab matching a URL substring; fail if none found. -/
def findTargetByUrl (base : String) (substr : String) : IO TargetInfo := do
  let ts ← listTargets base
  match ts.find? (·.url.splitOn substr |>.length |> (· > 1)) with
  | some t => return t
  | none   => throw <| IO.userError s!"cdp: no tab matches URL substring '{substr}'"

/-- Open a new tab via the `/json/new?url=…` REST endpoint. -/
def newTarget (base : String) (url : String) : IO TargetInfo := do
  let j ← httpGetJson s!"{base}/json/new?{url}"
  return targetOfJson j

/-- Close the tab via `/json/close/<id>`. -/
def closeTarget (base : String) (targetId : String) : IO Unit := do
  let _ ← httpGetJson s!"{base}/json/close/{targetId}"

end LeanTea.Cdp
