import Lean.Data.Json
import Lean.Data.Json.FromToJson

/-! # Tests.TypedRpcApi — the shared wire types for TypedRpcSpec

Defined once, in a real Lean module. The server handler uses these
types directly; the LeanJs client type-check harness `import`s this
module so the client is checked against the *same* definitions — no
second copy to drift. -/

structure SetCellReq where
  ref : String
  formula : String
  deriving Lean.ToJson, Lean.FromJson, Repr, Inhabited

structure SetCellResp where
  ok : Bool
  value : String
  deriving Lean.ToJson, Lean.FromJson, Repr, Inhabited
