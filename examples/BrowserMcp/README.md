# browser-mcp — MCP server that drives a real Chromium

Exposes the `LeanTea.Browser` surface (Playwright-backed) as
[Model Context Protocol](https://modelcontextprotocol.io/) tools so any
MCP client (Claude Code, Claude Desktop, Cursor, Zed, …) can navigate
a real browser, click, fill, screenshot, evaluate JS, and read
content. Pair it with a vision LLM for **"see → reason → act"** test
agents.

## Build once

```bash
# from the repo root
lake build browser_mcp_serve
# also: Playwright + Chromium for the Node bridge (one-time)
cd tools/browser-bridge && npm install && npx playwright install chromium
```

## Two transports

| Mode | When |
|---|---|
| **stdio** (default — no args) | MCP clients spawn the binary as a child process |
| **http** (`--port 8009`)      | manual testing with curl |

## Connect to Claude Code (this CLI session)

```bash
claude mcp add browser /Users/junji.hashimoto/git/english_learning/lean-elm/.lake/build/bin/browser_mcp_serve
```

That's it — restart `claude` and you'll see `browser_*` tools in the
catalogue. The binary auto-resolves the Node bridge path relative to
its own location, so no extra config is needed.

To remove later:

```bash
claude mcp remove browser
```

## Connect to Claude Desktop

Edit `~/Library/Application Support/Claude/claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "browser": {
      "command": "/Users/junji.hashimoto/git/english_learning/lean-elm/.lake/build/bin/browser_mcp_serve"
    }
  }
}
```

Restart Claude Desktop. The `browser_*` tools appear in the model's
tool picker.

## Connect to Cursor / Zed / generic MCP client

The binary speaks the standard stdio MCP transport, so any tool that
accepts a `command` (and optionally `args` / `env`) configuration
works. The `command` is the absolute path to `browser_mcp_serve`.

## Tools

| Tool | Purpose |
|---|---|
| `browser_open(width?, height?)` | start / restart the Chromium page |
| `browser_navigate(url)` | open a URL |
| `browser_click(selector)` | click an element |
| `browser_fill(selector, text)` | type into an input |
| `browser_press(key, selector?)` | press a key (`Enter`, `Tab`, …) |
| `browser_wait_for(selector, state?)` | wait until visible / attached / hidden |
| `browser_get_text(selector?)` | innerText (default: `body`) |
| `browser_get_html(selector?)` | innerHTML (default: `body`) |
| `browser_evaluate(expression)` | run JS in the page |
| `browser_screenshot(selector?, fullPage?)` | base64 PNG returned as MCP `image` content |
| `browser_close()` | shut down the session |

`browser_screenshot` returns an `image` content block, so a
vision-capable model can analyse the screenshot directly.

## Testing without an MCP client (curl)

```bash
./.lake/build/bin/browser_mcp_serve --port 8009 &
curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' \
  http://127.0.0.1:8009/mcp | jq .
```

## Bridge path resolution

The Lean binary needs to find `tools/browser-bridge/bridge.js`.
Resolution order:

1. `$LEANTEA_BROWSER_BRIDGE` — explicit override
2. CWD-relative candidates (`tools/browser-bridge/bridge.js`,
   `../tools/…`, `../../tools/…`)
3. Sibling of the running binary (works when the Lake-built
   `browser_mcp_serve` is at its standard `<repo>/.lake/build/bin/`
   path)

If your MCP client config moves the binary somewhere else, set the
env var:

```json
{
  "mcpServers": {
    "browser": {
      "command": "/path/to/browser_mcp_serve",
      "env": {
        "LEANTEA_BROWSER_BRIDGE": "/path/to/lean-elm/tools/browser-bridge/bridge.js"
      }
    }
  }
}
```
