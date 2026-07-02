import LeanTea
import LeanJs.Parser
import LeanJs.TypeCheck

/-! # TypedRpcSpec — end-to-end typed RPC

Proves the two halves of `LeanTea.Rpc.Typed`, both driven from a single
`Endpoint SetCellReq SetCellResp`:

  * **Server** — a typed handler `SetCellReq → IO SetCellResp`,
    codec-mediated dispatch: valid JSON round-trips, malformed or
    wrong-shape bodies become 400s, unmatched paths 404.
  * **Client** — a `.leanjs` client is type-checked against the *same*
    request/response types via `LeanJs.TypeCheck` (emit-to-Lean): a
    correct client passes, a client with a mistyped request field is
    rejected.

The client cases spawn `lean` to elaborate the emitted harness, the
same way `leanjs_spec` spawns `lean --run`. -/

open LeanTea.LSpec
open LeanTea.Rpc.Typed

/-- The one shared definition of the wire types. -/
structure SetCellReq where
  ref : String
  formula : String
  deriving Lean.ToJson, Lean.FromJson, Repr, Inhabited

structure SetCellResp where
  ok : Bool
  value : String
  deriving Lean.ToJson, Lean.FromJson, Repr, Inhabited

/-- The one endpoint that drives server + client. -/
def setCell : Endpoint SetCellReq SetCellResp :=
  { name := "apiSetCell", path := "/api/set",
    reqType := "SetCellReq", respType := "SetCellResp" }

def handleSetCell (r : SetCellReq) : IO SetCellResp :=
  return { ok := true, value := s!"{r.ref}={r.formula}" }

def app : LeanTea.Net.Http.Handler :=
  dispatch [ serve setCell handleSetCell ]

private def post (path body : String) : LeanTea.Net.Http.Request :=
  { method := "POST", path, query := "", headers := #[],
    body := body.toUTF8, version := "HTTP/1.1" }

/-- Client type-check harness prelude: the shared types + the endpoint
    stub, all generated so client calls are checked against the server
    types. -/
private def checkPrelude : String :=
  LeanJs.TypeCheck.asyncPreamble ++
  "structure SetCellReq where\n  ref : String\n  formula : String\n  deriving Inhabited, Repr\n" ++
  "structure SetCellResp where\n  ok : Bool\n  value : String\n  deriving Inhabited, Repr\n" ++
  setCell.stubDecl ++ "\n"

private def clientOk : String :=
  "async def save(r: String, f: String) :=\n" ++
  "  let req := SetCellReq { ref: r, formula: f };\n" ++
  "  let resp := await apiSetCell(req);\n  resp.value"

private def clientBadField : String :=
  "async def save(r: String, f: String) :=\n" ++
  "  let req := SetCellReq { ref: r, formla: f };\n" ++      -- typo'd field
  "  let resp := await apiSetCell(req);\n  resp.value"

private def typeChecks (src : String) : IO Bool := do
  match LeanJs.Parser.parseProgramString src with
  | .error _ => return false
  | .ok prog =>
    match ← LeanJs.TypeCheck.check checkPrelude prog with
    | .ok _    => return true
    | .error _ => return false

def run : IO LSpec := do
  -- Server side.
  let okResp    ← app (post "/api/set" "{\"ref\":\"A1\",\"formula\":\"42\"}")
  let badJson   ← app (post "/api/set" "{ not json")
  let wrongShp  ← app (post "/api/set" "{\"ref\":\"A1\"}")
  let missing   ← app (post "/nope" "{}")
  -- Client side (spawns `lean`).
  let clientGood ← typeChecks clientOk
  let clientBad  ← typeChecks clientBadField
  let jsGen := LeanTea.Js.renderBlock [setCell.clientFn]
  return group "typed RPC (one Endpoint, both sides)" [
    group "server" [
      it "valid request → 200"        (okResp.status == 200),
      it "valid body round-trips"     (String.fromUTF8! okResp.body == "{\"ok\":true,\"value\":\"A1=42\"}"),
      it "malformed JSON → 400"       (badJson.status == 400),
      it "wrong-shape JSON → 400"     (wrongShp.status == 400),
      it "unmatched path → 404"       (missing.status == 404)
    ],
    group "client (type-checked vs the same types)" [
      it "correct client type-checks"       clientGood,
      it "mistyped request field rejected"  (!clientBad)
    ],
    group "client JS generated from the endpoint" [
      it "emits an apiSetCell function"  (jsGen.splitOn "apiSetCell" |>.length |> (· > 1)),
      it "serializes the request as JSON" (jsGen.splitOn "JSON.stringify" |>.length |> (· > 1))
    ]
  ]

def main : IO Unit := do
  let code ← lspecIO (← run)
  if code != 0 then IO.Process.exit code.toUInt8
