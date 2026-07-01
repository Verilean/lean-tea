import LeanTea.Cloud.Gemini
import LeanTea.Llm.Openai
import Lean.Data.Json

/-! # examples/MetaOrchestrator/Llm.lean — pluggable LLM backends

The Director's stall loop wants a fast + cheap model (local Gemma on
LM Studio is fine — the input pane snapshot is at most a few kB and
the output is 3 keywords in JSON). Review passes want a heavy model
(Gemini 2.5 Pro's 2 M context lets you drop the whole diff + last
week of decisions into one prompt).

Rather than hard-wiring one client, this file exposes:

  * `Backend` — sum type that names the two providers we care about.
    LMStudio + any OpenAI-compatible endpoint fall under `openaiCompat`;
    Google's REST surface is `gemini`.
  * `Backend.ask` — one signature both backends satisfy: given a
    system + user prompt, return the model's text reply. Used by
    both `Director.decide` and `Director.review`.

Config stores TWO named backends (`decideBackend` + `reviewBackend`)
so per-role routing is one JSON edit — no code change needed. -/

namespace MetaOrchestrator.Llm

open Lean (Json)

/-! ## Backend sum type -/

inductive Backend where
  /-- Anything speaking the OpenAI chat completions dialect. LMStudio,
      Ollama's OpenAI-compat mode, groq, OpenAI proper. `apiKey?` is
      required for the last two; `baseUrl` includes the `/v1` suffix. -/
  | openaiCompat (baseUrl : String) (model : String) (apiKey? : Option String)
  /-- Google Gemini via the v1beta REST surface. Reads `GEMINI_API_KEY`
      from the environment. -/
  | gemini (model : String)
  deriving Inhabited

/-- One round-trip. `system` is the framing prompt (character card,
    audit-eye rubric, etc.); `user` is the concrete input. We
    return the model's text reply verbatim — parsing lives in the
    Director, not here. -/
def Backend.ask (b : Backend) (system : String) (user : String)
    (temperature : Float := 0.4) (maxTokens : Nat := 400) : IO String := do
  match b with
  | .openaiCompat baseUrl model apiKey? =>
    let cfg : LeanTea.Llm.Openai.Config := {
      baseUrl := baseUrl, apiKey? := apiKey?, timeoutSec := some 120
    }
    let sysMsg : LeanTea.Llm.Openai.Message := { role := "system", content := .inl system }
    let userMsg : LeanTea.Llm.Openai.Message := { role := "user", content := .inl user }
    let req : LeanTea.Llm.Openai.ChatRequest := {
      model, messages := [sysMsg, userMsg],
      temperature := some temperature,
      maxTokens := some maxTokens
    }
    let res ← LeanTea.Llm.Openai.chat cfg req
    return res.content
  | .gemini model =>
    let cfg ← LeanTea.Cloud.Gemini.Config.fromEnv!
    let cfg := { cfg with model := model }
    let resp ← LeanTea.Cloud.Gemini.ask cfg user {
      system := some system,
      temperature := some temperature,
      maxOutputTokens := some maxTokens
    }
    return resp.text

/-! ## JSON codec — for Config persistence

We spell out the JSON shape here so the config file reads naturally:

```json
"decideBackend": {"type": "openaiCompat",
                  "baseUrl": "http://127.0.0.1:11211/v1",
                  "model": "local-model"}
"reviewBackend": {"type": "gemini", "model": "gemini-2.5-pro"}
```
-/

private def jstr (j : Json) (k : String) (default : String := "") : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD default

def Backend.toJson (b : Backend) : Json :=
  match b with
  | .openaiCompat baseUrl model apiKey? =>
    let base : List (String × Json) := [
      ("type", Json.str "openaiCompat"),
      ("baseUrl", Json.str baseUrl),
      ("model", Json.str model)
    ]
    let full := match apiKey? with
      | some k => base ++ [("apiKey", Json.str k)]
      | none   => base
    Json.mkObj full
  | .gemini model =>
    Json.mkObj [
      ("type", Json.str "gemini"),
      ("model", Json.str model)
    ]

def Backend.ofJson? (j : Json) : Option Backend :=
  match jstr j "type" with
  | "openaiCompat" =>
    let baseUrl := jstr j "baseUrl"
    let model := jstr j "model"
    if baseUrl.isEmpty || model.isEmpty then none
    else
      let apiKey := jstr j "apiKey"
      some (.openaiCompat baseUrl model (if apiKey.isEmpty then none else some apiKey))
  | "gemini" =>
    let model := jstr j "model"
    if model.isEmpty then none
    else some (.gemini model)
  | _ => none

/-! ## Sensible defaults -/

def Backend.localLmStudio (model : String := "local-model") : Backend :=
  .openaiCompat "http://127.0.0.1:11211/v1" model none

def Backend.geminiPro : Backend := .gemini "gemini-2.5-pro"
def Backend.geminiFlash : Backend := .gemini "gemini-2.5-flash"

/-- Short human-readable form for the TUI status line. -/
def Backend.describe : Backend → String
  | .openaiCompat _ model _ => s!"openai:{model}"
  | .gemini model           => s!"gemini:{model}"

end MetaOrchestrator.Llm
