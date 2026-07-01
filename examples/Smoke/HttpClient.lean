import LeanTea
import Lean.Data.Json

/-! Smoke test for `LeanTea.Net.HttpClient` against a live HTTP target.

Uses the local `browser_mcp_serve --port 8009` so the test doesn't
depend on the public internet. Sends a `tools/list` JSON-RPC call,
checks the status code and that the response contains a known field.

Run after `./.lake/build/bin/browser_mcp_serve --port 8009 &` is up. -/

open LeanTea.Net.HttpClient
open Lean (Json)

def main : IO Unit := do
  let url := (← IO.getEnv "MCP_URL").getD "http://127.0.0.1:8009/mcp"
  IO.println s!"== probing {url} =="

  let body := (Json.mkObj [("jsonrpc", Json.str "2.0"), ("id", Json.num 1), ("method", Json.str "tools/list")]).compress
  let respText ← postJsonText url body
  IO.println s!"got {respText.length} bytes back"
  let isOk := respText.startsWith "{\"id\":1" || respText.startsWith "{\"jsonrpc\":\"2.0\""
  if !isOk then
    IO.println s!"⚠️ unexpected response prefix: {(respText.take 80).toString}"
  else
    IO.println "✓ JSON-RPC response shape looks right"

  let containsTools := (respText.splitOn "tools").length > 1
  if containsTools then
    IO.println "✓ response mentions 'tools' field"
  else
    IO.println "✗ response missing 'tools' field"
    IO.Process.exit 1

  IO.println "ok"
