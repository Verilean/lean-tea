import LeanTea

/-! Smoke test for `LeanTea.Llm.Openai`.

  * Lists available models (sanity)
  * Runs a non-streaming chat
  * Runs a streaming chat, printing each delta as it arrives

Expects a local LM Studio at `http://127.0.0.1:11211/v1`. The model
name comes from $LMSTUDIO_MODEL, defaulting to a small Gemma. -/

open LeanTea.Llm.Openai
open Lean (Json)

private def listModels (cfg : Config) : IO (Array String) := do
  let url := s!"{cfg.baseUrl}/models"
  let out ← IO.Process.output { cmd := "curl", args := #["-sS", url] }
  if out.exitCode != 0 then throw <| IO.userError out.stderr
  match Json.parse out.stdout with
  | .error e => throw <| IO.userError s!"models: bad json: {e}"
  | .ok j    =>
    let data := (j.getObjVal? "data").toOption.getD (Json.arr #[])
    let arr  := data.getArr?.toOption.getD #[]
    return arr.filterMap fun m =>
      (m.getObjVal? "id").toOption.bind (·.getStr?.toOption)

def main : IO Unit := do
  let modelEnv ← IO.getEnv "LMSTUDIO_MODEL"
  let cfg : Config := {
    baseUrl := "http://127.0.0.1:11211/v1",
    apiKey? := none,
    timeoutSec := some 120
  }

  IO.println "── /v1/models ────────────────────────────────────"
  let models ← listModels cfg
  for m in models do IO.println s!"  · {m}"
  IO.println ""

  let model := modelEnv.getD ((models[0]?).getD "google/gemma-3-1b")
  IO.println s!"using model: {model}"
  IO.println ""

  /- 1. Non-streaming. -/
  IO.println "── non-streaming chat ────────────────────────────"
  let req : ChatRequest := {
    model,
    messages := [
      system "You answer briefly. One sentence.",
      userText "What is 21 times 2?"
    ],
    temperature := some 0.2,
    maxTokens := some 64
  }
  let res ← chat cfg req
  IO.println s!"finish={res.finish}"
  IO.println res.content
  IO.println ""

  /- 2. Streaming — flush to stdout as each delta arrives. -/
  IO.println "── streaming chat ────────────────────────────────"
  let _ ← chatStream cfg {
    model,
    messages := [
      system "You are a helpful assistant. Be terse.",
      userText "Count from 1 to 8 separated by spaces."
    ],
    temperature := some 0.2,
    maxTokens := some 64
  } (fun delta => do
       (← IO.getStdout).putStr delta
       (← IO.getStdout).flush)
  IO.println ""
  IO.println ""

  /- 3. Vision — describe a generated test image. The image is a red
     circle on a light checkerboard, so a vision model should mention
     "red", "circle", and maybe the background pattern. -/
  IO.println "── vision chat ───────────────────────────────────"
  let visionModel := (← IO.getEnv "LMSTUDIO_VISION_MODEL").getD model
  IO.println s!"vision model: {visionModel}"
  let imgPath := "examples/Smoke/fixtures/vision-test.png"
  let imgExists ← System.FilePath.pathExists imgPath
  if !imgExists then
    IO.println s!"  ⚠ no {imgPath} — skipping vision pass"
  else
    let dataUrl ← imageDataUrlFromFile imgPath
    IO.println s!"  image: {dataUrl.utf8ByteSize} bytes as data URL"
    let res ← chat cfg {
      model := visionModel,
      messages := [
        userTextAndImage "Describe this image in one short sentence." dataUrl
      ],
      temperature := some 0.2,
      maxTokens := some 64
    }
    IO.println s!"  finish={res.finish}"
    IO.println s!"  → {res.content}"
  IO.println ""

  IO.println "ok"
