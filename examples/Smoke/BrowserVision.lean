import LeanTea

/-! End-to-end demo: drive a real Chromium with `LeanTea.Browser`,
take a screenshot, feed it to a vision model via `LeanTea.Llm.Openai`,
print the model's description of the page.

Useful as a template for "agent loops":
  * the page is the truth
  * the LLM reads it visually
  * the next action is whatever the LLM decides

Expects a vision-capable LM Studio at `http://127.0.0.1:11211/v1`.
Override the target URL with `$BROWSER_URL` (defaults to the GitHub
homepage so the smoke runs without any local server).

```
$ ./.lake/build/bin/browser_vision_smoke
```

Defaults work; pass `BROWSER_URL=http://127.0.0.1:8001/` to point at
the english app. -/

open LeanTea.Browser
open LeanTea.Llm.Openai

def main : IO Unit := do
  let url := (← IO.getEnv "BROWSER_URL").getD "https://example.com/"
  let cfg : Config := {
    baseUrl := "http://127.0.0.1:11211/v1",
    timeoutSec := some 120
  }
  let model := (← IO.getEnv "LMSTUDIO_VISION_MODEL").getD
                 "gemma-3-12b-vision"

  IO.println s!"── opening {url} ───────────────────────────"
  withSession fun s => do
    let nav ← s.navigate url
    IO.println s!"  url   = {nav.url}"
    IO.println s!"  title = {nav.title}"

    IO.println "── screenshot ─────────────────────────────"
    let shot ← s.screenshot
    IO.println s!"  {shot.bytes} bytes, {shot.width}×{shot.height} {shot.mime}"

    IO.println s!"── asking {model} what it sees ────────────"
    let res ← chat cfg {
      model,
      messages := [
        system ("You are looking at a web page screenshot. " ++
                "Describe the visible UI in 1-2 sentences. " ++
                "Mention any obvious buttons or links."),
        userTextAndImage "What does this page look like?" shot.dataUrl
      ],
      temperature := some 0.2,
      maxTokens := some 200
    }
    IO.println s!"  finish={res.finish}"
    IO.println s!"  → {res.content}"

  IO.println "ok"
