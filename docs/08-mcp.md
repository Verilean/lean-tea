# 8 · MCP servers

LeanTEA ships **six** MCP (Model Context Protocol) servers, each driving
a different capability surface that an LLM client (Claude Code, Claude
Desktop, Cursor, Zed, …) can call as tools:

| Server | Drives | Highlights |
|---|---|---|
| `chrome_cdp_mcp_serve` | A real Chrome via DevTools Protocol (WebSocket) | Attaches to the user's signed-in profile — no Playwright detection |
| `browser_mcp_serve` | Chromium via Playwright | Cross-browser, full Playwright API |
| `comfyui_mcp_serve` | ComfyUI's HTTP + WebSocket API | txt2img, img2img, workflow submission |
| `desktop_mcp_serve` | OS-level mouse + screenshot (macOS Quartz today) | Click any app, including native ones |
| `image_mcp_serve` | HTML/CSS → PNG compositor | Speech bubbles, captioned image overlays |
| Sheet `/mcp` | Adds/moves/edits shapes on the Sheet | Shares the SPA's port |

Every one of them sits on `LeanTea.Mcp.Handler`, a 3-field record. The
JSON-RPC envelope, the stdio loop, the HTTP transport, the content
shapes (text / error / image), the schema builders — all shared.

## The Handler contract

```lean
structure LeanTea.Mcp.Handler where
  initializeResult : Json
  toolsList        : Json
  callTool         : String → Json → IO Json
```

Supply those three and you get **stdio and HTTP transports for free**.

```lean
import LeanTea
open LeanTea.Mcp (textContent errContent argSchema toolDef
                  defaultInitializeResult)

def initializeResult : Json := defaultInitializeResult "my-mcp"

def toolsList : Json :=
  Json.mkObj [("tools", Json.arr #[
    toolDef "greet" "Greet someone by name."
      #[ argSchema "name" "string" "person to greet" ]
      #["name"]
  ])]

def callTool (name : String) (args : Json) : IO Json := do
  match name with
  | "greet" =>
    let who := args.getStrD "name" "stranger"
    return textContent s!"Hello, {who}!"
  | _ => return errContent s!"unknown tool: {name}"

def main : IO Unit := do
  let h : LeanTea.Mcp.Handler := {
    initializeResult, toolsList, callTool
  }
  h.serveStdio   -- or h.serveHttp 8000 "0.0.0.0"
```

That's a complete MCP server. Drop it into `.mcp.json` and the LLM
sees one tool.

## What the shared module gives you

`LeanTea.Mcp` exposes:

- **Envelopes**: `jsonOk`, `jsonErr` (JSON-RPC 2.0)
- **Content shapes**: `textContent`, `errContent`, `imageContent`
- **Schema builders**: `argSchema`, `toolDef`, `defaultInitializeResult`
- **Transports**: `Handler.serveStdio`, `Handler.serveHttp`,
  `handleMcp`, `httpHandler`
- A shared `dispatchOnce` that handles `initialize`, `tools/list`,
  `tools/call`, and `notifications/*`

The five top-level servers + Sheet all share the same dispatch — before
extraction they each duplicated ~80 lines of boilerplate.

## Worked example — Chrome CDP MCP

`examples/ChromeCdpMcp/Serve.lean` drives a real Chrome instance via
the DevTools Protocol. The key insight: Google detects Playwright but
not a vanilla CDP WebSocket attached to the user's own session. So:

1. The user launches Chrome with `--remote-debugging-port=9222
   --user-data-dir=/tmp/chrome-cdp`.
2. `chrome_cdp_mcp_serve` opens a fresh WebSocket per tool call,
   sends one CDP command, reads the matching response, closes.
3. Tools exposed: `chrome_targets`, `chrome_navigate`, `chrome_evaluate`,
   `chrome_screenshot`, `chrome_click`, `chrome_fill`,
   `chrome_wait_for_selector`, `chrome_find_tab`, `chrome_get_html`,
   `chrome_scroll_collect`.

A few design choices worth highlighting:

- **WebSocket is pure Lean** (`LeanTea.Net.WebSocket`, RFC 6455
  handshake via SHA-1, masking, ping/pong handling). No node bridge.
- **Stateless WS per command** keeps the server crash-friendly; the
  only state is the `IO.Ref` holding the workspace path + CDP URL.
- **`outputFile` on every read-shaped tool** writes the result to disk
  instead of returning it inline — large screenshots / DOM dumps
  never enter the LLM client's context budget.
- **`attachFiles` on `chrome_fill`** reads files server-side and
  appends them under the prompt as fenced code blocks. The MCP client
  doesn't waste context reading files first.
- **`chrome_scroll_collect`** drives a virtualised list (Twitter feed,
  Gemini sidebar, infinite scroll) to the bottom and dumps every
  visible item — the one specialised tool that's much faster than a
  generic `chrome_evaluate` round trip per scroll.
- **Workspace path guard**: every `outputFile` / `attachFiles` path is
  validated against `--workspace <root>` so a misbehaving LLM can't
  read `~/.ssh/id_rsa` or write `/etc/passwd`.

## Hooking into a client

Drop the binary path into the client's MCP config. For Claude Code:

```json
{
  "mcpServers": {
    "chrome_cdp": {
      "command": "/path/to/chrome_cdp_mcp_serve",
      "args": ["--stdio", "--workspace", "/Users/you/projects"]
    }
  }
}
```

The same shape works for any other server. Set `args` to whatever
flags the binary takes; common ones are `--stdio` (default), `--http
--port N`, and server-specific URLs (`--cdp http://...`, `--comfy
http://...`).

## HTTP mode for debugging

Every server also accepts `--http --port N` and serves the same
JSON-RPC at `POST /mcp`. Useful for curl-driven smoke tests:

```sh
curl -s -X POST http://127.0.0.1:8014/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' \
  | python3 -m json.tool
```

## When to write your own

The bundled servers cover the common AI-driving surfaces. Write your
own when:

- You're integrating a domain API (a print queue, a build farm, a
  game) the LLM should call directly.
- You want to mix a side-effect with a static asset (Sheet does this
  — same port, SPA at `/`, MCP at `/mcp`).
- You need a transport other than stdio/HTTP — e.g. WebSocket-only —
  in which case copy the dispatch and write your own pump.

For the common case it's three Json values + a `callTool`. The rest is
free.
