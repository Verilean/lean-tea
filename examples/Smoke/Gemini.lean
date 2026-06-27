import LeanTea

/-! Smoke test for `LeanTea.Cloud.Gemini`.

Skips quietly when `GEMINI_API_KEY` is unset so it can sit in CI
without leaking the secret or failing the pipeline.

When the key is present:

  1. `ask`: send "ping; reply OK" against `gemini-2.5-flash-lite`
     (cheapest; this is a wire-up check, not a reasoning test) and
     assert non-empty response text.
  2. `reviewMany`: bundle this very file plus `LeanTea/Cloud/Gemini.lean`
     and request a 1-sentence summary, against the same cheap model.
     Verifies the SafePath workspace plumbing + the multi-file
     bundling shape.

Override the model used for the smoke via `GEMINI_SMOKE_MODEL`
(default `gemini-2.5-flash-lite`). -/

open LeanTea.Cloud.Gemini

def main : IO Unit := do
  match ← Config.fromEnv? with
  | none =>
    IO.println "gemini_smoke: GEMINI_API_KEY not set — skipping (this is fine in CI)"
    return
  | some baseCfg =>
    let model := (← IO.getEnv "GEMINI_SMOKE_MODEL").getD "gemini-2.5-flash-lite"
    let cfg := { baseCfg with model, timeoutSec := 120 }
    IO.println s!"gemini_smoke: model = {cfg.model}"

    IO.println "── ask ──────────────────────────────────────────"
    let r1 ← ask cfg "Reply with the single word OK."
      { temperature := some 0.0, maxOutputTokens := some 16 }
    IO.println s!"text=`{r1.text.trim}`  finish={r1.finishReason}  \
tokens in={r1.usage.input} out={r1.usage.output}"
    if r1.text.isEmpty then
      throw <| IO.userError "gemini_smoke: ask returned empty text"

    IO.println ""
    IO.println "── reviewMany ───────────────────────────────────"
    /- Find the repo root: the smoke binary runs from anywhere, but
       Lake sets cwd to the package root. Bail out cleanly if the
       file isn't there (running this binary by hand from somewhere
       else shouldn't crash CI). -/
    let ws := (← IO.currentDir).toString
    let target := "LeanTea/Cloud/Gemini.lean"
    if ! (← System.FilePath.pathExists (ws ++ "/" ++ target)) then
      IO.println s!"gemini_smoke: {target} not found under {ws} — \
skipping reviewMany (run from repo root for full coverage)"
      return
    let (r2, stats) ← reviewMany cfg ws
      #[target, "examples/Smoke/Gemini.lean"]
      "1 文で全体構造を要約してください。"
      { temperature := some 0.0, maxOutputTokens := some 256 }
    IO.println s!"files={stats.fileCount} bytes={stats.totalBytes} \
finish={r2.finishReason} tokens in={r2.usage.input} out={r2.usage.output}"
    IO.println r2.text
    if r2.text.isEmpty then
      throw <| IO.userError "gemini_smoke: reviewMany returned empty text"
    IO.println ""
    IO.println "gemini_smoke: ✔ both calls succeeded"
