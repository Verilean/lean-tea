import LeanTea

/-! HTTPS smoke: a real TLS GET over the OpenSSL client, no curl.

Build + run (macOS Homebrew openssl@3):
```
LEANTEA_TLS=1 lake build https_smoke
./.lake/build/bin/https_smoke                 # defaults to example.com
./.lake/build/bin/https_smoke https://api.github.com/zen
```
Needs `LEANTEA_TLS=1` (real backend) + `-lssl -lcrypto` (wired via the
exe's weakLinkArgs). Without the flag the client raises a clear IO error. -/

open LeanTea.Net

def main (args : List String) : IO Unit := do
  let url := args.headD "https://example.com/"
  IO.println s!"── HTTPS GET {url} ──────────────────────"
  let resp ← TlsClient.requestUrl "GET" url
  IO.println s!"  status : {resp.status}"
  IO.println s!"  server : {resp.header? "server" |>.getD "(none)"}"
  IO.println s!"  bytes  : {resp.body.size}"
  let text := resp.bodyText
  IO.println "  ── first 320 chars ──"
  IO.println (text.take 320)
  -- scheme dispatcher too
  IO.println "── fetchText (scheme-dispatch) ─────────────"
  let n := (← fetchText url).length
  IO.println s!"  fetched {n} chars via LeanTea.Net.fetchText"
