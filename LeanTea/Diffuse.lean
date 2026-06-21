import LeanTea.Net.HttpClient
import Lean.Data.Json

/-! # LeanTea.Diffuse — HTTP client for the diffusers sidecar

Lighter-weight alternative to `LeanTea.Comfy`. The sidecar at
`tools/diffuse-server/diffuse_server.py` exposes one-shot
`/txt2img` and `/img2img` endpoints that return PNG bytes
directly — no workflow-JSON construction, no polling loop, no
two-step audio_query dance.

```
let png ← LeanTea.Diffuse.txt2img cfg {
  prompt   := "tropical beach at golden hour",
  ckpt     := some "chroma-unlocked-v50.safetensors",
  steps    := some 4,
  cfg      := some 1.0,
  width    := some 512,
  height   := some 512,
  ...
}
```

The sidecar keeps the last-used Pipeline in memory, so successive
calls with the same `ckpt` skip the cold-load entirely.
-/

namespace LeanTea.Diffuse

open Lean (Json)

structure Config where
  baseUrl    : String := "http://127.0.0.1:8189"
  /-- Single-request timeout. First call after sidecar start may
      include a 5-30 s cold-load; subsequent calls just take the
      generation time itself. -/
  timeoutS   : Nat := 300
  deriving Inhabited

/-- One generation request. All fields except `prompt` are optional
    and fall back to sidecar defaults. -/
structure GenRequest where
  prompt    : String
  negative  : Option String := none
  ckpt      : Option String := none
  steps     : Option Nat    := none
  cfg       : Option Float  := none
  width     : Option Nat    := none
  height    : Option Nat    := none
  seed      : Option Nat    := none
  refImage  : Option String := none  -- data URL for img2img
  denoise   : Option Float  := none
  deriving Inhabited

private def floatJ (f : Float) : Json :=
  match Json.parse (toString f) with
  | .ok j    => j
  | .error _ => Json.num 0

private def natJ (n : Nat) : Json := Json.num (Int.ofNat n)

private def requestToJson (r : GenRequest) : Json :=
  let base : List (String × Json) := [("prompt", Json.str r.prompt)]
  let withNeg := match r.negative with
    | some s => base ++ [("negative", Json.str s)] | none => base
  let withCkpt := match r.ckpt with
    | some s => withNeg ++ [("ckpt", Json.str s)] | none => withNeg
  let withSteps := match r.steps with
    | some n => withCkpt ++ [("steps", natJ n)] | none => withCkpt
  let withCfg := match r.cfg with
    | some f => withSteps ++ [("cfg", floatJ f)] | none => withSteps
  let withW := match r.width with
    | some n => withCfg ++ [("width", natJ n)] | none => withCfg
  let withH := match r.height with
    | some n => withW ++ [("height", natJ n)] | none => withW
  let withSeed := match r.seed with
    | some n => withH ++ [("seed", natJ n)] | none => withH
  let withRef := match r.refImage with
    | some s => withSeed ++ [("refImage", Json.str s)] | none => withSeed
  let withDen := match r.denoise with
    | some f => withRef ++ [("denoise", floatJ f)] | none => withRef
  Json.mkObj withDen

private def post (cfg : Config) (path : String) (body : String) : IO ByteArray := do
  let url := s!"{cfg.baseUrl}{path}"
  let parsed ← match LeanTea.Net.HttpClient.parseUrl url with
    | some u => pure u
    | none   => throw <| IO.userError s!"diffuse: bad URL {url}"
  let headers := #[("Content-Type", "application/json")]
  let resp ← LeanTea.Net.HttpClient.request "POST" parsed body.toUTF8 headers
  if resp.status >= 400 then
    throw <| IO.userError s!"diffuse: {path} returned {resp.status}: {resp.bodyText}"
  return resp.body

/-- Text-to-image. Returns PNG bytes. -/
def txt2img (cfg : Config) (req : GenRequest) : IO ByteArray :=
  post cfg "/txt2img" (requestToJson req).compress

/-- Image-to-image with a base64 data URL reference. The sidecar
    decodes the URL itself; we just pipe it through. -/
def img2img (cfg : Config) (req : GenRequest) : IO ByteArray :=
  post cfg "/img2img" (requestToJson req).compress

/-- One call that picks the right endpoint based on whether
    `refImage` is set. Convenience for callers that already
    handle both shapes uniformly. -/
def generate (cfg : Config) (req : GenRequest) : IO ByteArray :=
  match req.refImage with
  | some _ => img2img cfg req
  | none   => txt2img cfg req

end LeanTea.Diffuse
