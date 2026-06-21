import Lean.Data.Json
import LeanTea.Net.Http
import LeanTea.Net.HttpClient

/-! # LeanTea.Llm.Openai — OpenAI-compatible client

Talks to anything that speaks the OpenAI Chat-Completions API. Tested
against a local LM Studio at `http://127.0.0.1:11211/v1/`.

Two entry points:

* `chat`        — blocking, returns the full assistant response.
* `chatStream`  — streaming via Server-Sent Events; per-token deltas
                  go to a user callback as they arrive.

Both honour the multi-modal content shape: a `Message`'s content can
be either a single text string or a list of typed blocks
(`text` + `image_url`), so vision models work without a separate API.

The HTTP transport is `curl` spawned via `IO.Process`. Sticking with
curl means we get HTTPS, keep-alive, and (most importantly) honest
streaming line buffering for free. -/

namespace LeanTea.Llm.Openai

open Lean (Json)

/-! ## Types -/

/-- One block inside a multi-modal message. `text` is the common case;
    `image data:`-URLs or `https:`-URLs go through `image_url`. -/
inductive ContentBlock where
  /-- A plain text run. -/
  | text     (s : String) : ContentBlock
  /-- An image, identified by URL. Pass a `data:image/...;base64,...`
      URL to inline an image you have as bytes. -/
  | imageUrl (url : String) (detail : Option String := none) : ContentBlock
  deriving Inhabited, Repr

/-- A chat message — role plus content. We accept either a single
    string or a list of blocks; the encoder picks the right JSON
    shape automatically. -/
structure Message where
  /-- `"system"`, `"user"`, `"assistant"`, or `"tool"`. -/
  role    : String
  /-- Either a string or a `List ContentBlock`. The string form is
      always allowed; the block form is needed for vision. -/
  content : Sum String (List ContentBlock)
  /-- Optional name for tool messages. -/
  name?   : Option String := none

instance : Inhabited Message :=
  ⟨{ role := "user", content := .inl "" }⟩

/-- A chat request. Mirrors the OpenAI body shape; only the fields we
    actually use are surfaced. Anything else can be added later. -/
structure ChatRequest where
  model        : String
  messages     : List Message
  temperature  : Option Float := none
  maxTokens    : Option Nat   := none
  /-- `top_p` sampling. -/
  topP         : Option Float := none
  /-- A list of stop strings. -/
  stop         : List String  := []
  /-- Whether to stream the response. Set automatically by
      `chatStream`; callers of `chat` should leave it `false`. -/
  stream       : Bool         := false
  deriving Inhabited

/-! ## JSON encoding -/

private def blockToJson : ContentBlock → Json
  | .text s          => Json.mkObj [
      ("type", Json.str "text"),
      ("text", Json.str s)
    ]
  | .imageUrl url det =>
    let urlObj :=
      match det with
      | some d => Json.mkObj [("url", Json.str url), ("detail", Json.str d)]
      | none   => Json.mkObj [("url", Json.str url)]
    Json.mkObj [
      ("type",      Json.str "image_url"),
      ("image_url", urlObj)
    ]

private def messageToJson (m : Message) : Json :=
  let contentJson :=
    match m.content with
    | .inl s   => Json.str s
    | .inr bs  => Json.arr (bs.toArray.map blockToJson)
  let base : List (String × Json) := [
    ("role",    Json.str m.role),
    ("content", contentJson)
  ]
  let withName :=
    match m.name? with
    | some n => base ++ [("name", Json.str n)]
    | none   => base
  Json.mkObj withName

private def floatJson (f : Float) : Json :=
  /- `Json.num` takes a `JsonNumber`; we encode by reading back via
     `toString`, which is good enough for the tiny temperature /
     top_p range we care about. -/
  match Json.parse (toString f) with
  | .ok j => j
  | .error _ => Json.num 0

def requestToJson (r : ChatRequest) : Json := Id.run do
  let mut fields : List (String × Json) := [
    ("model",    Json.str r.model),
    ("messages", Json.arr (r.messages.toArray.map messageToJson))
  ]
  if let some t   := r.temperature then fields := fields ++ [("temperature", floatJson t)]
  if let some tp  := r.topP        then fields := fields ++ [("top_p",       floatJson tp)]
  if let some mx  := r.maxTokens   then fields := fields ++ [("max_tokens",  Json.num (Int.ofNat mx))]
  if !r.stop.isEmpty then
    fields := fields ++ [("stop", Json.arr (r.stop.toArray.map Json.str))]
  if r.stream then fields := fields ++ [("stream", Json.bool true)]
  return Json.mkObj fields

/-! ## Client config -/

structure Config where
  /-- Base URL — for LM Studio default it's `http://127.0.0.1:11211/v1`.
      No trailing slash; the route is appended directly. -/
  baseUrl : String := "http://127.0.0.1:11211/v1"
  /-- Optional API key. LM Studio ignores it; OpenAI proper requires it. -/
  apiKey? : Option String := none
  /-- Total request timeout in seconds. LM Studio inference can run
      long, so we default high. Set `none` for no limit. -/
  timeoutSec : Option Nat := some 600
  deriving Inhabited

private def curlAuthArgs (cfg : Config) : Array String :=
  match cfg.apiKey? with
  | none => #[]
  | some k => #["-H", s!"Authorization: Bearer {k}"]

private def curlTimeoutArgs (cfg : Config) : Array String :=
  match cfg.timeoutSec with
  | none   => #[]
  | some n => #["--max-time", toString n]

/-! ## Non-streaming chat

Run `curl` with `--data-binary @-` so the JSON body goes over stdin
(avoids argv-size limits) and `--silent --show-error` so error output
isn't swallowed. -/

/-- One choice in the non-streaming response. We only return the
    final assistant text; the full Json is available via `chatRaw`. -/
structure ChatResult where
  content : String
  /-- Finish reason as reported by the server (e.g. `"stop"`, `"length"`). -/
  finish  : String := ""
  /-- The full raw response Json, for callers that want token counts
      or tool calls. -/
  raw     : Json
  deriving Inhabited

private def authHeaders (cfg : Config) : Array (String × String) :=
  match cfg.apiKey? with
  | none   => #[]
  | some k => #[("Authorization", s!"Bearer {k}")]

/-- Non-streaming chat: pure-Lean HTTP, no curl. The socket transport
    has no argv-size limit, so vision payloads (~1 MB base64) just go
    over the wire. For HTTPS endpoints (OpenAI proper) this client
    doesn't apply — see `chatStream` for the curl-backed path that
    still works for both. -/
def chatRaw (cfg : Config) (req : ChatRequest) : IO Json := do
  let body := (requestToJson { req with stream := false }).compress
  let url  := s!"{cfg.baseUrl}/chat/completions"
  let raw ← LeanTea.Net.HttpClient.postJsonText url body (authHeaders cfg)
  match Json.parse raw with
  | .error e => throw <| IO.userError s!"openai: bad JSON response: {e}\n{raw}"
  | .ok j    => return j

/-- High-level `chat` — runs the request and pulls out the first
    choice's text. -/
def chat (cfg : Config) (req : ChatRequest) : IO ChatResult := do
  let body := (requestToJson { req with stream := false }).compress
  let url  := s!"{cfg.baseUrl}/chat/completions"
  let stdout ← LeanTea.Net.HttpClient.postJsonText url body (authHeaders cfg)
  let j ←
    match Json.parse stdout with
    | .error e => throw <| IO.userError s!"openai: bad JSON response: {e}\n{stdout}"
    | .ok j    => pure j
  /- Defensive defaults — if the server returned an error envelope
     instead of choices, surface the `error.message`. -/
  if let .ok errObj := j.getObjVal? "error" then
    let msg := (errObj.getObjVal? "message").toOption.bind (·.getStr?.toOption) |>.getD ""
    throw <| IO.userError s!"openai: {msg}"
  let choices ← match j.getObjVal? "choices" with
    | .ok cs => match cs.getArr? with
                | .ok xs => pure xs
                | .error e => throw <| IO.userError s!"choices: {e}"
    | .error e => throw <| IO.userError s!"choices: {e}"
  let first ← match choices.toList with
    | (c :: _) => pure c
    | []       => throw <| IO.userError "openai: empty choices"
  let msgObj := (first.getObjVal? "message").toOption.getD (Json.mkObj [])
  let txt   := (msgObj.getObjVal? "content").toOption.bind (·.getStr?.toOption) |>.getD ""
  let fin   := (first.getObjVal? "finish_reason").toOption.bind (·.getStr?.toOption) |>.getD ""
  return { content := txt, finish := fin, raw := j }

/-! ## Streaming

Server-Sent Events from `/v1/chat/completions?stream=true` look like:

```
data: {"choices":[{"delta":{"content":"Hello"},"index":0, …}]}

data: {"choices":[{"delta":{"content":" world"},"index":0, …}]}

data: [DONE]
```

We spawn `curl` with `--no-buffer`, read its stdout line-by-line, and
call the user's `onDelta` for every `delta.content`. `[DONE]` ends
the loop. -/

/-- Pull the next-token text out of one parsed SSE chunk. Returns
    `none` when the chunk has no `delta.content` (some chunks only
    carry a `finish_reason`). -/
private def deltaContent (j : Json) : Option String := do
  let choices ← j.getObjVal? "choices" |>.toOption
  let arr     ← choices.getArr? |>.toOption
  let first   ← arr[0]?
  let delta   ← first.getObjVal? "delta" |>.toOption
  let content ← delta.getObjVal? "content" |>.toOption
  content.getStr?.toOption

/-- Spawn curl, stream the response, hand each delta token to
    `onDelta`. Returns the concatenated text once `[DONE]` arrives. -/
partial def chatStream (cfg : Config) (req : ChatRequest)
    (onDelta : String → IO Unit) : IO String := do
  let body := (requestToJson { req with stream := true }).compress
  let url  := s!"{cfg.baseUrl}/chat/completions"
  let args :=
    #["-sS", "--no-buffer", "-X", "POST",
      "-H", "Content-Type: application/json",
      "--data-binary", body]
    ++ curlAuthArgs cfg
    ++ curlTimeoutArgs cfg
    ++ #[url]
  let child ← IO.Process.spawn {
    cmd    := "curl",
    args,
    stdin  := .null,
    stdout := .piped,
    stderr := .piped
  }
  let stdout := child.stdout
  let mut acc : String := ""
  let mut done : Bool := false
  while !done do
    let line ← stdout.getLine
    if line.isEmpty then
      done := true                  -- EOF
    else
      let trimmed := line.trimAscii.toString
      if trimmed.startsWith "data:" then
        let payload := (trimmed.drop 5).toString.trimAscii.toString
        if payload == "[DONE]" then
          done := true
        else
          match Json.parse payload with
          | .error _ => pure ()
          | .ok j   =>
            match deltaContent j with
            | some s => acc := acc ++ s; onDelta s
            | none   => pure ()
  let exitCode ← child.wait
  if exitCode != 0 then
    let err ← child.stderr.readToEnd
    throw <| IO.userError s!"curl (streaming): {err}"
  return acc

/-! ## Convenience builders -/

/-- Wrap a plain string as a `user` message. -/
def userText (s : String) : Message :=
  { role := "user", content := .inl s }

/-- A vision-flavoured `user` message — one text run plus one image
    URL (data: or http:). -/
def userTextAndImage (txt url : String) : Message :=
  { role := "user",
    content := .inr [.text txt, .imageUrl url] }

/-- A `system` prompt. -/
def system (s : String) : Message :=
  { role := "system", content := .inl s }

/-! ## Base64 + data URL helper

A bare-bones encoder so callers can turn an image-on-disk into a
`data:image/...;base64,...` URL without shelling out. Wide enough for
vision payloads up to a few MB; for *huge* bodies you'd want a
streaming version but vision images are typically well under that. -/

private def b64Table : ByteArray :=
  String.toUTF8 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

private def b64Pad : UInt8 := '='.toNat.toUInt8

/-- RFC-4648 base64 encode. No line wrapping. -/
def base64Encode (bs : ByteArray) : String := Id.run do
  let n := bs.size
  let mut out : ByteArray := ByteArray.empty
  let mut i := 0
  while i < n do
    let b0 := bs[i]!.toNat
    let b1 := if i + 1 < n then bs[i+1]!.toNat else 0
    let b2 := if i + 2 < n then bs[i+2]!.toNat else 0
    let v  := (b0 <<< 16) ||| (b1 <<< 8) ||| b2
    out := out.push b64Table[(v >>> 18) &&& 0x3f]!
    out := out.push b64Table[(v >>> 12) &&& 0x3f]!
    out := out.push (if i + 1 < n then b64Table[(v >>> 6) &&& 0x3f]! else b64Pad)
    out := out.push (if i + 2 < n then b64Table[v &&& 0x3f]!        else b64Pad)
    i := i + 3
  return String.fromUTF8! out

/-- Read a file on disk and return a `data:<mime>;base64,...` URL
    ready to drop into `userTextAndImage`. `mime` is typically
    `image/png`, `image/jpeg`, `image/webp`, … -/
def imageDataUrlFromFile (path : String) (mime : String := "image/png")
    : IO String := do
  let bytes ← IO.FS.readBinFile path
  return s!"data:{mime};base64,{base64Encode bytes}"

private def b64Decode1 (c : UInt8) : Option Nat :=
  let n := c.toNat
  if 65 <= n && n <= 90  then some (n - 65)        -- A-Z
  else if 97 <= n && n <= 122 then some (n - 97 + 26)   -- a-z
  else if 48 <= n && n <= 57  then some (n - 48 + 52)   -- 0-9
  else if n = 43 then some 62  -- '+'
  else if n = 47 then some 63  -- '/'
  else none

/-- Decode standard base64. Whitespace and any non-table
    characters (other than '=' padding) are skipped. -/
def base64Decode (s : String) : Option ByteArray := Id.run do
  let raw := s.toUTF8
  -- Build a cleaned stream of "alphabet-or-equals" bytes.
  let mut cleaned : ByteArray := ByteArray.empty
  for i in [0 : raw.size] do
    let c := raw[i]!
    if c.toNat = 61 then
      cleaned := cleaned.push c  -- preserve '='
    else if (b64Decode1 c).isSome then
      cleaned := cleaned.push c
  if cleaned.size % 4 != 0 then return none
  let mut out : ByteArray := ByteArray.empty
  let mut i := 0
  while i < cleaned.size do
    let c0 := cleaned[i]!
    let c1 := cleaned[i+1]!
    let c2 := cleaned[i+2]!
    let c3 := cleaned[i+3]!
    let v0 := (b64Decode1 c0).getD 0
    let v1 := (b64Decode1 c1).getD 0
    let v2 := (b64Decode1 c2).getD 0
    let v3 := (b64Decode1 c3).getD 0
    let n  := (v0 <<< 18) ||| (v1 <<< 12) ||| (v2 <<< 6) ||| v3
    out := out.push ((n >>> 16) &&& 0xff).toUInt8
    let was3eq := c2.toNat = 61
    let was4eq := c3.toNat = 61
    if !was3eq then out := out.push ((n >>> 8) &&& 0xff).toUInt8
    if !was4eq && !was3eq then out := out.push (n &&& 0xff).toUInt8
    i := i + 4
  return some out

end LeanTea.Llm.Openai
