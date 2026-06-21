import LeanTea
import Lean.Data.Json

/-! # comfyui_mcp_serve — MCP server that drives a local ComfyUI

ComfyUI exposes an HTTP + WebSocket API designed for headless
batch use, so there's no reason to automate its web UI when we can
just POST a workflow JSON and pull the resulting image back. This
exe wraps that API as MCP tools.

```
comfyui_mcp_serve --port 8011        # HTTP, curl-friendly
comfyui_mcp_serve                    # stdio, for MCP clients
```

Default target is `http://127.0.0.1:8188`; override with the
`COMFYUI_URL` env var or `--comfy http://host:port`.

Tools:

* `comfyui_status` — GET `/system_stats`; smoke-check connection.
* `comfyui_models(type?)` — list installed models per category
  (`checkpoints`, `loras`, `vae`, …). Empty list = need to install.
* `comfyui_txt2img(prompt, negative?, width?, height?, steps?, cfg?,
  seed?, ckpt?)` — one-shot text-to-image. Builds a standard
  workflow, submits, polls until done, returns the PNG as an MCP
  `image` content block.
* `comfyui_submit_workflow(workflow)` — for advanced users with a
  hand-tuned graph; bypass the txt2img scaffold and submit raw.
* `comfyui_wait(prompt_id, timeoutMs?)` — poll history until the
  job finishes or the timeout trips.

For game assets specifically: pair the txt2img tool with the
`ui_remember` shared map (already mounted by every MCP we ship) and
freeze your favourite prompt / seed combos under keys like
`assets.character.idle_front` for reproducible regeneration. -/

open LeanTea LeanTea.Net.Server
open LeanTea.Net.Http (Request Response Handler)
open Lean (Json)

/-! Aliases — we use both `Http.Response` (the server-side response
    we send back to MCP callers) and `HttpClient.Response` (what
    we get from ComfyUI). Keeping them straight prevents the
    "Ambiguous term" frustration. -/
abbrev ClientResponse := LeanTea.Net.HttpClient.Response

namespace ComfyuiMcp

/-! ## Config — `IO.Ref` so HTTP overrides survive across requests. -/

abbrev ComfyConfig := IO.Ref String

private def defaultBaseUrl : IO String := do
  return (← IO.getEnv "COMFYUI_URL").getD "http://127.0.0.1:8188"

/-! ## Low-level HTTP — small layer over `LeanTea.Net.HttpClient`. -/

private def httpGet (url : String) : IO ClientResponse := do
  match LeanTea.Net.HttpClient.parseUrl url with
  | none   => throw <| IO.userError s!"comfyui: bad URL: {url}"
  | some u => LeanTea.Net.HttpClient.request "GET" u

private def httpGetJson (url : String) : IO Json := do
  let resp ← httpGet url
  if resp.status >= 400 then
    throw <| IO.userError s!"comfyui GET {url}: {resp.status}\n{resp.bodyText}"
  match Json.parse resp.bodyText with
  | .ok j    => return j
  | .error e => throw <| IO.userError s!"comfyui: bad JSON from {url}: {e}\n{resp.bodyText}"

private def httpPostJson (url : String) (body : Json) : IO Json := do
  let raw ← LeanTea.Net.HttpClient.postJsonText url body.compress
  match Json.parse raw with
  | .ok j    => return j
  | .error e => throw <| IO.userError s!"comfyui: bad JSON from POST {url}: {e}\n{raw}"

/-! ## ComfyUI API wrappers -/

def systemStats (base : String) : IO Json :=
  httpGetJson (base ++ "/system_stats")

def listModels (base : String) (modelType : String) : IO (Array String) := do
  let j ← httpGetJson (base ++ "/models/" ++ modelType)
  match j.getArr? with
  | .ok arr  => return arr.filterMap (·.getStr?.toOption)
  | .error _ => return #[]

/-! ### Workflow builders

For a normal one-shot txt2img we need 7 nodes: load checkpoint →
clip-encode positive + negative → empty latent → ksampler → vae
decode → save image. The shape below mirrors what the web UI
produces, just with our parameters substituted in. -/

def buildTxt2ImgWorkflow (prompt negative ckpt : String)
    (w h steps : Nat) (cfg : Float) (seed : Nat)
    (sampler scheduler : String) : Json :=
  let mkNode (cls : String) (inputs : Json) : Json :=
    Json.mkObj [("class_type", Json.str cls), ("inputs", inputs)]
  let floatJ (f : Float) : Json :=
    match Json.parse (toString f) with
    | .ok j    => j
    | .error _ => Json.num 0
  let natJ (n : Nat) : Json := Json.num (Int.ofNat n)
  Json.mkObj [
    ("4", mkNode "CheckpointLoaderSimple"
      (Json.mkObj [("ckpt_name", Json.str ckpt)])),
    ("5", mkNode "EmptyLatentImage"
      (Json.mkObj [("width", natJ w), ("height", natJ h),
                   ("batch_size", natJ 1)])),
    ("6", mkNode "CLIPTextEncode"
      (Json.mkObj [("text", Json.str prompt),
                   ("clip", Json.arr #[Json.str "4", Json.num 1])])),
    ("7", mkNode "CLIPTextEncode"
      (Json.mkObj [("text", Json.str negative),
                   ("clip", Json.arr #[Json.str "4", Json.num 1])])),
    ("3", mkNode "KSampler"
      (Json.mkObj [
        ("seed",         natJ seed),
        ("steps",        natJ steps),
        ("cfg",          floatJ cfg),
        ("sampler_name", Json.str sampler),
        ("scheduler",    Json.str scheduler),
        ("denoise",      floatJ 1.0),
        ("model",        Json.arr #[Json.str "4", Json.num 0]),
        ("positive",     Json.arr #[Json.str "6", Json.num 0]),
        ("negative",     Json.arr #[Json.str "7", Json.num 0]),
        ("latent_image", Json.arr #[Json.str "5", Json.num 0])
      ])),
    ("8", mkNode "VAEDecode"
      (Json.mkObj [("samples", Json.arr #[Json.str "3", Json.num 0]),
                   ("vae",     Json.arr #[Json.str "4", Json.num 2])])),
    ("9", mkNode "SaveImage"
      (Json.mkObj [("filename_prefix", Json.str "leantea"),
                   ("images", Json.arr #[Json.str "8", Json.num 0])]))
  ]

/-- Some checkpoints prefer particular sampler/scheduler/cfg/steps
    combinations. Sniff the filename to suggest sensible defaults
    so a single `comfyui_txt2img` call works across SDXL, FLUX
    schnell, and FLUX dev without the caller having to remember
    each model's quirks. -/
private def hasSub (hay needle : String) : Bool :=
  /- Quick substring check by splitting; we only call this on
     filenames so the O(n*m) cost is irrelevant. -/
  (hay.splitOn needle).length > 1

private def suggestDefaults (ckpt : String) : (String × String × Float × Nat) :=
  let lc := ckpt.toLower
  if hasSub lc "flux" && hasSub lc "schnell" then
    -- Flux schnell: distilled, no real CFG, 4-step
    ("euler", "simple", 1.0, 4)
  else if hasSub lc "chroma" then
    -- Chroma (FLUX dev fine-tune): real CFG, 28 steps tuned for speed
    -- (official workflow uses 45, but 28 is the sweet spot for most
    -- chibi/portrait work and runs ~1.6× faster). `beta` scheduler
    -- matches the official; `euler` for the sampler.
    ("euler", "beta", 4.5, 28)
  else if hasSub lc "flux" then
    -- Flux dev: needs FluxGuidance for CFG, 20 steps roughly
    ("euler", "simple", 1.0, 20)
  else if hasSub lc "turbo" then
    -- SDXL Turbo: 1-4 steps, low cfg
    ("euler", "normal", 1.5, 4)
  else
    -- SD 1.5 / SDXL default
    ("euler", "normal", 7.0, 20)

/-! ### Chroma workflow

Chroma (`lodestones/Chroma`) ships as a UNet only — no CLIP, no VAE
baked into the safetensors — so the standard `CheckpointLoaderSimple`
chain blows up with "clip input is invalid: None". The official
recipe is:

  * `ChromaDiffusionLoader` (from the `ComfyUI_FluxMod` custom node)
    on the UNet under `models/diffusion_models/`
  * `CLIPLoader` with `t5xxl_fp8_e4m3fn.safetensors` only — no clip_l
  * `VAELoader` with `ae.safetensors`
  * `ChromaPaddingRemoval` on each conditioning (positive + negative)
  * `KSampler` with `scheduler := beta`, `steps := 45`, `cfg := 4.5`

Defaults below mirror the workflow in the Chroma HF repo. The user
controls everything via the same `comfyui_txt2img` tool — we just
sniff the checkpoint name and route to the right builder. -/

def buildChromaWorkflow (prompt negative ckpt t5 vae : String)
    (w h steps : Nat) (cfg : Float) (seed : Nat) (scheduler : String)
    (weightDtype : String := "default") : Json :=
  let mkNode (cls : String) (inputs : Json) : Json :=
    Json.mkObj [("class_type", Json.str cls), ("inputs", inputs)]
  let floatJ (f : Float) : Json :=
    match Json.parse (toString f) with
    | .ok j    => j
    | .error _ => Json.num 0
  let natJ (n : Nat) : Json := Json.num (Int.ofNat n)
  /- Uses the built-in `UNETLoader` instead of FluxMod's
     `ChromaDiffusionLoader` — the latter's `pick_operations` call
     broke against ComfyUI ≥ 0.25 (`scaled_fp8` keyword removed),
     and Chroma loads cleanly through stock UNETLoader anyway since
     it's structurally a FLUX dev derivative.

     `weightDtype`:
     - `"default"`     — bf16/fp16 per the file; works on MPS & CUDA.
     - `"fp8_e4m3fn"`  — ~1.5–2× faster on CUDA, but **MPS panics**
                         with "doesn't support that dtype" (as of
                         ComfyUI 0.25 / PyTorch 2.x).
     - `"fp8_e4m3fn_fast"` / `"fp8_e5m2"` — CUDA only.
     Pick via env (`LEANTEA_CHROMA_DTYPE=fp8_e4m3fn`) on Linux/CUDA. -/
  Json.mkObj [
    ("25", mkNode "UNETLoader"
      (Json.mkObj [("unet_name",    Json.str ckpt),
                   ("weight_dtype", Json.str weightDtype)])),
    ("6",  mkNode "CLIPLoader"
      (Json.mkObj [("clip_name", Json.str t5),
                   ("type", Json.str "stable_diffusion"),
                   ("device", Json.str "default")])),
    ("11", mkNode "VAELoader"
      (Json.mkObj [("vae_name", Json.str vae)])),
    ("14", mkNode "EmptyLatentImage"
      (Json.mkObj [("width", natJ w), ("height", natJ h),
                   ("batch_size", natJ 1)])),
    ("4",  mkNode "CLIPTextEncode"
      (Json.mkObj [("text", Json.str prompt),
                   ("clip", Json.arr #[Json.str "6", Json.num 0])])),
    ("5",  mkNode "CLIPTextEncode"
      (Json.mkObj [("text", Json.str negative),
                   ("clip", Json.arr #[Json.str "6", Json.num 0])])),
    ("9",  mkNode "KSampler"
      (Json.mkObj [
        ("seed",         natJ seed),
        ("steps",        natJ steps),
        ("cfg",          floatJ cfg),
        ("sampler_name", Json.str "euler"),
        ("scheduler",    Json.str scheduler),
        ("denoise",      floatJ 1.0),
        ("model",        Json.arr #[Json.str "25", Json.num 0]),
        ("positive",     Json.arr #[Json.str "4",  Json.num 0]),
        ("negative",     Json.arr #[Json.str "5",  Json.num 0]),
        ("latent_image", Json.arr #[Json.str "14", Json.num 0])
      ])),
    ("10", mkNode "VAEDecode"
      (Json.mkObj [("samples", Json.arr #[Json.str "9",  Json.num 0]),
                   ("vae",     Json.arr #[Json.str "11", Json.num 0])])),
    ("19", mkNode "SaveImage"
      (Json.mkObj [("filename_prefix", Json.str "leantea-chroma"),
                   ("images", Json.arr #[Json.str "10", Json.num 0])]))
  ]

/-- Submit a workflow. Returns the `prompt_id` ComfyUI assigns. -/
def submit (base : String) (workflow : Json) : IO String := do
  let body := Json.mkObj [
    ("prompt",    workflow),
    ("client_id", Json.str "leantea")
  ]
  let resp ← httpPostJson (base ++ "/prompt") body
  match resp.getObjVal? "prompt_id" with
  | .ok (.str id) => return id
  | _             =>
    /- ComfyUI also surfaces validation errors under `error` /
       `node_errors`; bubble them up verbatim so the LLM / user can
       see exactly which node tripped. -/
    throw <| IO.userError s!"comfyui submit failed:\n{resp.compress}"

/-- Poll `/history/{id}` until the entry has an `outputs` field or
    we exceed `timeoutMs`. Returns the entry. -/
partial def waitFor (base : String) (promptId : String) (timeoutMs : Nat)
    : IO Json := do
  let start ← IO.monoMsNow
  let rec loop : Unit → IO Json := fun _ => do
    let now ← IO.monoMsNow
    if now - start > timeoutMs then
      throw <| IO.userError
        s!"comfyui: timed out after {(now - start)}ms waiting for prompt {promptId}"
    let j ← httpGetJson s!"{base}/history/{promptId}"
    /- Response shape: `{ "<prompt_id>": { ... } }`. -/
    match j.getObjVal? promptId with
    | .ok entry =>
      if (entry.getObjVal? "outputs").toOption.isSome then return entry
      /- Still running — sleep half a second and retry. -/
      IO.sleep 500
      loop ()
    | .error _ =>
      IO.sleep 500
      loop ()
  loop ()

/-- Extract the first saved image filename from a history entry.
    `outputs` is `{node_id: {images: [{filename, subfolder, type}, …]}}`,
    so we walk every node's `images` list and take the first hit. -/
private def firstImageOf (entry : Json) : Option (String × String × String) := Id.run do
  let outputsJ := (entry.getObjVal? "outputs").toOption.getD Json.null
  let nodeOuts : List Json := match outputsJ with
    | .obj kvs => kvs.toList.map (·.snd)
    | _        => []
  for nodeOut in nodeOuts do
    match (nodeOut.getObjVal? "images").toOption.bind (·.getArr?.toOption) with
    | some arr =>
      match arr[0]? with
      | some img =>
        let name := (img.getObjVal? "filename").toOption.bind (·.getStr?.toOption) |>.getD ""
        let sub  := (img.getObjVal? "subfolder").toOption.bind (·.getStr?.toOption) |>.getD ""
        let typ  := (img.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD "output"
        if !name.isEmpty then return some (name, sub, typ)
      | none => pure ()
    | none => pure ()
  return none

/-- Fetch a generated image as bytes via `/view`. -/
def fetchImage (base filename subfolder type_ : String) : IO ByteArray := do
  let url := s!"{base}/view?filename={filename}&subfolder={subfolder}&type={type_}"
  let resp ← httpGet url
  if resp.status >= 400 then
    throw <| IO.userError s!"comfyui /view: {resp.status}"
  return resp.body

/-! ## MCP shapes — see `LeanTea.Mcp` for the shared implementation. -/

open LeanTea.Mcp (jsonOk jsonErr textContent errContent imageContent
                  argSchema toolDef defaultInitializeResult)

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "comfyui_status"
        "Smoke-check the ComfyUI HTTP endpoint. Returns the system_stats JSON."
        #[] #[],
      toolDef "comfyui_models"
        ("List installed models. `type` defaults to `checkpoints`; "
         ++ "valid values are `checkpoints`, `loras`, `vae`, "
         ++ "`text_encoders`, `diffusion_models`, `controlnet`, "
         ++ "`upscale_models`, etc.")
        #[ argSchema "type" "string" "model category" ] #[],
      toolDef "comfyui_txt2img"
        ("Run one text-to-image generation. Builds the standard "
         ++ "CheckpointLoaderSimple → CLIP → EmptyLatent → KSampler "
         ++ "→ VAE → SaveImage workflow, submits, waits, returns the "
         ++ "PNG as an MCP `image` content block.\n\n"
         ++ "Required: `prompt`, `ckpt` (checkpoint filename — "
         ++ "list with `comfyui_models`). Optional: `negative`, "
         ++ "`width` (default 512), `height` (512), `steps` (20), "
         ++ "`cfg` (7.0), `seed` (random-ish), `timeoutMs` (180000).")
        #[ argSchema "prompt"   "string" "positive prompt",
           argSchema "negative" "string" "negative prompt (defaults to a sensible \"bad anatomy\" boilerplate)",
           argSchema "ckpt"     "string" "checkpoint filename",
           argSchema "width"    "number" "image width (multiple of 8)",
           argSchema "height"   "number" "image height (multiple of 8)",
           argSchema "steps"    "number" "sampling steps",
           argSchema "cfg"      "number" "classifier-free guidance scale",
           argSchema "seed"     "number" "RNG seed (set explicitly for reproducible asset regen)",
           argSchema "timeoutMs"  "number" "max wait for completion",
           argSchema "outputPath" "string" "(optional) absolute path to also save the PNG bytes" ]
        #["prompt", "ckpt"],
      toolDef "comfyui_submit_workflow"
        ("Submit a raw ComfyUI workflow graph. Returns the prompt_id; "
         ++ "pair with `comfyui_wait` for completion. Use this when "
         ++ "`comfyui_txt2img`'s scaffold is too narrow (img2img, "
         ++ "ControlNet, multi-pass, …) — copy a working JSON out of "
         ++ "the ComfyUI web UI's Save (API Format).")
        #[ ("workflow", Json.mkObj [
             ("type", Json.str "object"),
             ("description", Json.str "raw workflow graph")
           ]) ]
        #["workflow"],
      toolDef "comfyui_wait"
        "Wait for a submitted prompt to finish; returns the history entry."
        #[ argSchema "prompt_id" "string" "id returned by submit",
           argSchema "timeoutMs" "number" "default 180000" ]
        #["prompt_id"],
      toolDef "ui_recall"
        "Same shared `ui-map.json` as the browser / desktop MCPs. Stash prompts + seeds under keys like `assets.character.idle_front`."
        #[ argSchema "key" "string" "asset key" ] #["key"],
      toolDef "ui_remember"
        "Save a prompt/seed combo (or any freeform value) to the shared map."
        #[ argSchema "key" "string" "asset key",
           ("value", Json.mkObj [("type", Json.str "object"),
                                  ("description", Json.str "freeform")]) ]
        #["key", "value"],
      toolDef "ui_list" "List shared map keys." #[] #[]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-comfyui-mcp"

/-! ## Args extraction -/

private def getStr (args : Json) (k : String) : Except String String :=
  match args.getObjVal? k with
  | .ok v => v.getStr?
  | .error e => .error e

private def getStrOpt (args : Json) (k : String) (default : String := "") : String :=
  match args.getObjVal? k with
  | .ok v => match v.getStr? with | .ok s => s | _ => default
  | _ => default

private def getNatOpt (args : Json) (k : String) (default : Nat := 0) : Nat :=
  match args.getObjVal? k with
  | .ok (.num n) => n.mantissa.toNat
  | _            => default

private def getFloatOpt (args : Json) (k : String) (default : Float := 0) : Float :=
  match args.getObjVal? k with
  | .ok (.num n) =>
    /- JsonNumber → Float via toString round-trip is good enough
       for cfg / denoise / temperature where we don't need exact
       binary fidelity. -/
    n.toFloat
  | _            => default

/-! ## Tool dispatch -/

def callTool (cfg : ComfyConfig) (name : String) (args : Json) : IO Json := do
  try
    let base ← cfg.get
    match name with
    | "comfyui_status" =>
      let j ← systemStats base
      return textContent j.compress
    | "comfyui_models" =>
      let t := getStrOpt args "type" "checkpoints"
      let ms ← listModels base t
      let arr := Json.arr (ms.map Json.str)
      return textContent arr.compress
    | "comfyui_txt2img" =>
      match getStr args "prompt", getStr args "ckpt" with
      | .ok prompt, .ok ckpt =>
        let negative := getStrOpt args "negative"
          "low quality, blurry, jpeg artifacts, watermark, text"
        let isChroma := hasSub ckpt.toLower "chroma"
        let (defSampler, defScheduler, defCfg, defSteps) := suggestDefaults ckpt
        /- Chroma is heavy enough that 512×512 is the sane sprite
           default; bigger resolutions blow the wall-clock past a
           minute fast. Other models keep the 1024×1024 default. -/
        let defWidth  : Nat := if isChroma then 512 else 1024
        let defHeight : Nat := if isChroma then 512 else 1024
        let w     := match getNatOpt args "width"  with | 0 => defWidth  | n => n
        let h     := match getNatOpt args "height" with | 0 => defHeight | n => n
        let steps := match getNatOpt args "steps"  with | 0 => defSteps | n => n
        let cfgScale :=
          let raw := getFloatOpt args "cfg" 0
          if raw == 0 then defCfg else raw
        let sampler   := getStrOpt args "sampler"   defSampler
        let scheduler := getStrOpt args "scheduler" defScheduler
        let seed ← match getNatOpt args "seed" with
          | 0 => do let t ← IO.monoMsNow; pure (t % 2147483647)
          | n => pure n
        let timeoutMs := match getNatOpt args "timeoutMs" with
          | 0 => 180000 | n => n
        let t5  := getStrOpt args "t5"  "t5xxl_fp8_e4m3fn.safetensors"
        let vae := getStrOpt args "vae" "ae.safetensors"
        /- Route Chroma through the UNet/CLIP/VAE split workflow;
           everything else stays on the all-in-one CheckpointLoader
           path. -/
        let chromaDtype ← match ← IO.getEnv "LEANTEA_CHROMA_DTYPE" with
          | some v => pure v
          | none   => pure "default"
        let wf :=
          if isChroma then
            buildChromaWorkflow prompt negative ckpt t5 vae
              w h steps cfgScale seed scheduler chromaDtype
          else
            buildTxt2ImgWorkflow prompt negative ckpt
              w h steps cfgScale seed sampler scheduler
        let kindTag := if isChroma then "(chroma)" else ""
        IO.eprintln s!"comfyui: ckpt={ckpt} {kindTag} {w}x{h} sampler={sampler} scheduler={scheduler} cfg={cfgScale} steps={steps} seed={seed}"
        let id ← submit base wf
        IO.eprintln s!"comfyui: submitted prompt {id}, polling for up to {timeoutMs}ms"
        let entry ← waitFor base id timeoutMs
        match firstImageOf entry with
        | none =>
          return errContent s!"comfyui: prompt {id} finished but no images in outputs\n{entry.compress}"
        | some (filename, subfolder, type_) =>
          let bytes ← fetchImage base filename subfolder type_
          /- Optional outputPath — when present the file is also
             written to disk so a ui_script step can produce a
             usable asset without a base64 decode dance. -/
          let outPathSuffix ← match getStrOpt args "outputPath" "" with
            | "" => pure ""
            | p  =>
              IO.FS.writeBinFile p bytes
              pure s!" → {p}"
          let b64 := LeanTea.Llm.Openai.base64Encode bytes
          return imageContent "image/png" b64
            s!"{w}×{h} generated ({bytes.size} bytes), prompt_id={id}, seed={seed}, ckpt={ckpt}{outPathSuffix}"
      | .error e, _ => return errContent s!"prompt: {e}"
      | _, .error e => return errContent s!"ckpt: {e}"
    | "comfyui_submit_workflow" =>
      let wf := (args.getObjVal? "workflow").toOption.getD Json.null
      let id ← submit base wf
      return textContent s!"submitted prompt_id={id}"
    | "comfyui_wait" =>
      match getStr args "prompt_id" with
      | .error e => return errContent e
      | .ok id =>
        let timeoutMs := match getNatOpt args "timeoutMs" with
          | 0 => 180000 | n => n
        let entry ← waitFor base id timeoutMs
        return textContent entry.compress
    | "ui_recall" =>
      match getStr args "key" with
      | .error e => return errContent e
      | .ok k =>
        let v ← LeanTea.Agent.Memory.recall k
        return textContent v.compress
    | "ui_remember" =>
      match getStr args "key" with
      | .error e => return errContent e
      | .ok k =>
        let v := (args.getObjVal? "value").toOption.getD Json.null
        LeanTea.Agent.Memory.remember k v
        return textContent s!"remembered {k}"
    | "ui_list" =>
      let ks ← LeanTea.Agent.Memory.keys
      return textContent (Json.arr (ks.map Json.str)).compress
    | _ => return errContent s!"unknown tool: {name}"
  catch e =>
    return errContent s!"{name}: {e}"

/-! ## Transports — supplied by `LeanTea.Mcp.Handler`. -/

private structure Args where
  mode  : String := "stdio"
  port  : UInt16 := 8011
  host  : String := "0.0.0.0"
  comfy : Option String := none

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest      => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest      => parseArgs rest { a with mode := "http" }
  | "--port" :: v :: rest  => parseArgs rest { a with mode := "http",
                                                       port := (v.toNat?.getD 8011).toUInt16 }
  | "--host" :: v :: rest  => parseArgs rest { a with host := v }
  | "--comfy" :: v :: rest => parseArgs rest { a with comfy := some v }
  | _ :: rest              => parseArgs rest a
  | []                     => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  let base ← match a.comfy with
    | some u => pure u
    | none   => defaultBaseUrl
  let cfg ← IO.mkRef base
  IO.eprintln s!"comfyui-mcp: ComfyUI at {base}"
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool cfg
  }
  match a.mode with
  | "http" =>
    IO.eprintln s!"comfyui-mcp: POST http://{a.host}:{a.port}/mcp"
    mcpHandler.serveHttp a.port a.host
  | _ =>
    IO.eprintln "comfyui-mcp: stdio mode"
    mcpHandler.serveStdio

end ComfyuiMcp

def main (args : List String) : IO Unit := ComfyuiMcp.serveMain args
