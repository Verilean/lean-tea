import LeanTea.Net.HttpClient
import Lean.Data.Json

/-! # LeanTea.Comfy — minimal ComfyUI HTTP client

ComfyUI exposes a JSON-over-HTTP API on `127.0.0.1:8188` (default).
We only need three calls to drive a queued image generation:

* `POST /prompt`             — queue a workflow, returns a prompt_id
* `GET  /history/<id>`       — poll until the workflow appears with
                               `outputs.<node>.images[]`
* `GET  /view?filename=...`  — fetch the rendered PNG bytes

This module is content-agnostic: callers hand in the workflow JSON
they want executed. A workflow is whatever ComfyUI's UI exports via
"Save (API Format)" — typically a node graph keyed by string node-ids.
We do not introspect the graph; we just substitute a `{{PROMPT}}`
placeholder anywhere in the serialised workflow before submission.
-/

namespace LeanTea.Comfy

open Lean (Json)

structure Config where
  /-- Base URL of the ComfyUI server. No trailing slash. -/
  baseUrl   : String := "http://127.0.0.1:8188"
  /-- Polling cadence for /history. -/
  pollMs    : Nat := 1500
  /-- Cap on total polling time. ComfyUI workflows for Flux on a
      decent GPU finish in ~10-30 s; bump up for CPU or SDXL. -/
  timeoutS  : Nat := 180
  deriving Inhabited

/-- Substitute `{{PROMPT}}` (and any other `{{KEY}}` pairs the caller
    passes) in a workflow JSON string. We do this textually so the
    caller's workflow file can stay readable; ComfyUI accepts the
    re-serialised JSON identically. -/
def fillTemplate (tpl : String) (subs : List (String × String)) : String :=
  subs.foldl (fun acc (k, v) =>
    -- JSON-quote the substitution so newlines / quotes in the prompt
    -- don't break the workflow JSON.
    let quoted := (Json.str v).compress  -- includes surrounding ""
    let inner  := quoted.toRawSubstring.drop 1 |>.dropRight 1 |>.toString
    acc.replace ("{{" ++ k ++ "}}") inner) tpl

/-- Queue a workflow. Returns the prompt_id ComfyUI assigns. -/
def submitPrompt (cfg : Config) (workflow : Json) : IO String := do
  let clientId := "lean-elm"
  let body := (Json.mkObj [
    ("prompt",    workflow),
    ("client_id", Json.str clientId)
  ]).compress
  let raw ← LeanTea.Net.HttpClient.postJsonText s!"{cfg.baseUrl}/prompt" body
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"comfy: bad submit response: {e}\n{raw}"
  | .ok j =>
    match (j.getObjVal? "prompt_id").toOption.bind (·.getStr?.toOption) with
    | some id => return id
    | none =>
      let errStr := (j.getObjVal? "error").toOption.map (·.compress) |>.getD raw
      throw <| IO.userError s!"comfy: submit rejected: {errStr}"

structure OutputImage where
  filename  : String
  subfolder : String
  type      : String := "output"
  deriving Inhabited, Repr

private def parseImages (j : Json) : List OutputImage :=
  match j.getArr? with
  | .error _ => []
  | .ok arr =>
    arr.toList.filterMap fun img =>
      let filename  := (img.getObjVal? "filename").toOption.bind (·.getStr?.toOption) |>.getD ""
      let subfolder := (img.getObjVal? "subfolder").toOption.bind (·.getStr?.toOption) |>.getD ""
      let typ       := (img.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD "output"
      if filename.isEmpty then none
      else some { filename, subfolder, type := typ }

/-- Extract every `outputs.<node>.images[]` entry from a /history slot. -/
private def collectImages (slot : Json) : List OutputImage :=
  match (slot.getObjVal? "outputs").toOption with
  | none => []
  | some outs =>
    match outs.getObj? with
    | .error _ => []
    | .ok kv =>
      kv.toArray.toList.flatMap fun (_, nodeOut) =>
        match (nodeOut.getObjVal? "images").toOption with
        | none => []
        | some imgs => parseImages imgs

/-- Poll `/history/<id>` until images appear (or the timeout fires).
    ComfyUI returns `{}` while queued / running, and an object keyed
    by prompt_id once the workflow completes. -/
partial def waitForResult (cfg : Config) (promptId : String)
    : IO (List OutputImage) := do
  let deadline := cfg.timeoutS * 1000
  let rec loop (elapsed : Nat) : IO (List OutputImage) := do
    if elapsed >= deadline then
      throw <| IO.userError s!"comfy: timed out after {cfg.timeoutS}s on prompt {promptId}"
    let url : LeanTea.Net.HttpClient.Url ← match LeanTea.Net.HttpClient.parseUrl s!"{cfg.baseUrl}/history/{promptId}" with
      | some u => pure u
      | none   => throw <| IO.userError s!"comfy: bad baseUrl {cfg.baseUrl}"
    let resp ← LeanTea.Net.HttpClient.request "GET" url
    if resp.status >= 400 then
      throw <| IO.userError s!"comfy: /history returned {resp.status}: {resp.bodyText}"
    let txt := resp.bodyText
    let images :=
      match Json.parse txt with
      | .error _ => []
      | .ok j =>
        match (j.getObjVal? promptId).toOption with
        | none      => []
        | some slot => collectImages slot
    if !images.isEmpty then
      return images
    IO.sleep cfg.pollMs.toUInt32
    loop (elapsed + cfg.pollMs)
  loop 0

/-- Upload an image to ComfyUI's `input/` directory via the
    multipart `/upload/image` endpoint. Returns the saved filename
    (typically the requested name, possibly with a numeric suffix
    if `overwrite=false` and a name collision occurs). Used by
    img2img / ControlNet / IPAdapter workflows that need to point
    LoadImage at a freshly-uploaded reference. -/
def uploadImage (cfg : Config) (bytes : ByteArray) (filename : String) : IO String := do
  let boundary := "----leantea-comfy-upload"
  let crlf := "\r\n"
  let part1 :=
    s!"--{boundary}{crlf}" ++
    s!"Content-Disposition: form-data; name=\"image\"; filename=\"{filename}\"{crlf}" ++
    s!"Content-Type: image/png{crlf}{crlf}"
  let part2 :=
    s!"{crlf}--{boundary}{crlf}" ++
    s!"Content-Disposition: form-data; name=\"overwrite\"{crlf}{crlf}" ++
    "true"
  let trailer := s!"{crlf}--{boundary}--{crlf}"
  let body := part1.toUTF8 ++ bytes ++ part2.toUTF8 ++ trailer.toUTF8
  let url : LeanTea.Net.HttpClient.Url ← match LeanTea.Net.HttpClient.parseUrl s!"{cfg.baseUrl}/upload/image" with
    | some u => pure u
    | none   => throw <| IO.userError s!"comfy: bad baseUrl {cfg.baseUrl}"
  let headers := #[("Content-Type", s!"multipart/form-data; boundary={boundary}")]
  let resp ← LeanTea.Net.HttpClient.request "POST" url body headers
  if resp.status >= 400 then
    throw <| IO.userError s!"comfy /upload/image returned {resp.status}: {resp.bodyText}"
  match Json.parse resp.bodyText with
  | .error e => throw <| IO.userError s!"comfy upload: bad JSON {e}\n{resp.bodyText}"
  | .ok j =>
    match (j.getObjVal? "name").toOption.bind (·.getStr?.toOption) with
    | some n => return n
    | none => throw <| IO.userError s!"comfy upload: no `name` in response: {resp.bodyText}"

/-- Download an output PNG. ComfyUI's `/view` returns raw image bytes. -/
def fetchImage (cfg : Config) (img : OutputImage) : IO ByteArray := do
  let q := s!"filename={img.filename}&subfolder={img.subfolder}&type={img.type}"
  let url : LeanTea.Net.HttpClient.Url ← match LeanTea.Net.HttpClient.parseUrl s!"{cfg.baseUrl}/view?{q}" with
    | some u => pure u
    | none   => throw <| IO.userError s!"comfy: bad baseUrl {cfg.baseUrl}"
  let resp ← LeanTea.Net.HttpClient.request "GET" url
  if resp.status >= 400 then
    throw <| IO.userError s!"comfy: /view returned {resp.status}: {resp.bodyText}"
  return resp.body

/-- End-to-end convenience: substitute a `{{PROMPT}}` placeholder,
    queue, poll, fetch the first output image as bytes. -/
def runWorkflow (cfg : Config) (workflowTpl : String)
    (subs : List (String × String)) : IO ByteArray := do
  let filled := fillTemplate workflowTpl subs
  let wf ← match Json.parse filled with
    | .error e => throw <| IO.userError s!"comfy: workflow not valid JSON after substitution: {e}"
    | .ok j    => pure j
  let id ← submitPrompt cfg wf
  let imgs ← waitForResult cfg id
  match imgs with
  | []      => throw <| IO.userError "comfy: workflow finished with no images"
  | img :: _ => fetchImage cfg img

end LeanTea.Comfy
