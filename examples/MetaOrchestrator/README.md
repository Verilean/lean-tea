# meta_orchestrator — Gemini-driven PM for many Claude Code agents

A small, polling-based meta-agent that watches **N zellij panes**
(each typically running `claude` / Claude Code) and decides when to
nudge each one with a new instruction. Built on:

- **`LeanTea.Cloud.Gemini`** — for the decision LLM
- **`zellij action dump-screen` / `write-chars`** — for pane I/O
- A FNV-1a hash of the pane text — for cheap stall detection
- A JSON config file — list of managed agents, hand-editable, persisted across restarts

## When you want this

You're working towards a long-running objective ("optimise this GPGPU
kernel by 2×", "port library X to Y") that takes many Claude Code
turns. You don't want to babysit each turn but you don't want the
agent to drift. The orchestrator wakes Gemini *only when the pane goes
quiet for `--stall-secs`*, gives Gemini the pane snapshot + the goal +
a short memory of past nudges, and lets it decide:

- `continue`   → don't interrupt
- `instruct`   → write a new prompt into the pane (and press Enter)
- `ask_user`   → escalate to you on stdin

This keeps Gemini calls bounded (no per-poll spend) and keeps the
human in the loop only on real branch points.

## Run

```sh
# Inside a zellij session, in a separate pane from the ones running Claude:
lake build meta_orchestrator
./.lake/build/bin/meta_orchestrator --config meta_orchestrator.config.json
```

The orchestrator boots into a slash-command REPL on stdin. The config
file (created on first `/save`) holds the list of managed agents — on
restart every `enabled: true` agent auto-spawns with the memos it had
last time.

### First-time setup

```
> /add kernel-bwd terminal_18 Optimise FlashAttn v3 bwd SM90 2x throughput
> /add port-cuda  terminal_22 Port the rest of the kernels from CUDA to CK
> /list
> /save
```

Pane id discovery: `zellij action dump-layout` lists every pane id;
`terminal_N` is the stable form.

Required env: `GEMINI_API_KEY` (get one at https://aistudio.google.com/apikey).

### Slash commands

| command | meaning |
|---|---|
| `/list`                   | show every agent + its current status |
| `/add ID PANE GOAL...`    | register and start an agent |
| `/stop ID`                | cooperatively stop the polling loop (config kept) |
| `/start ID`               | resume a stopped/disabled agent |
| `/remove ID`              | drop the agent from the config |
| `/reply ID TEXT...`       | inject a free-text reply (use when the decide backend said `ask_user`) |
| `/review ID`              | run a heavy-weight audit via `reviewBackend` on that agent |
| `/save [PATH]`            | persist the config to PATH (default: `--config` path) |
| `/load PATH`              | replace the in-memory config; running agents keep going until `/stop` |
| `/quit`                   | stop every agent, save the config, exit |

### Config file shape

```json
{
  "agents": [
    {"id": "kernel-bwd", "pane": "terminal_18",
     "goal": "Optimise FlashAttn v3 bwd SM90 2x throughput",
     "stallSec": 30, "pollSec": 5, "enabled": true}
  ],
  "logDir": "./logs",
  "decideBackend": {
    "type": "openaiCompat",
    "baseUrl": "http://127.0.0.1:11211/v1",
    "model": "local-model"
  },
  "reviewBackend": {
    "type": "gemini",
    "model": "gemini-2.5-pro"
  }
}
```

### Two roles, two backends

`decideBackend` runs on every stall — cheap and fast wins here, so
the default is LMStudio on port 11211 with whatever local model is
loaded. Any OpenAI-compatible endpoint works: point `baseUrl` at
Ollama's `/v1`, groq, OpenAI proper (add `"apiKey": "sk-…"`), etc.

`reviewBackend` runs on `/review AGENT_ID` — an on-demand
heavy-weight audit that reads the full memo log + a bigger pane
snapshot and asks for concrete redirection advice. Gemini 2.5 Pro
is the default because the 2 M context lets us drop all of it in
one request.

Backend types:

  | type | fields | notes |
  |---|---|---|
  | `openaiCompat` | `baseUrl` `model` `apiKey?` | LMStudio, Ollama, groq, OpenAI |
  | `gemini` | `model` | reads `GEMINI_API_KEY` from env |

The file is hand-editable. Per-agent memo + decision logs live in
`<logDir>/<agentId>.memos.jsonl` and `<agentId>.decisions.jsonl`.

## File layout

| file | role |
|---|---|
| `Zellij.lean`   | `dumpScreen` / `writeChars` / `submit` over the `zellij action` CLI |
| `Llm.lean`      | `Backend` union (openaiCompat / gemini) + `Backend.ask` — one signature both providers satisfy |
| `Director.lean` | builds the decide + review prompts, parses the verdict JSON |
| `Config.lean`   | `ManagedAgent` / `Config` records + JSON codec + load/save/add/remove |
| `Runtime.lean`  | per-agent polling loop (`IO.asTask`), snapshot for the controller |
| `Tui.lean`      | full-screen TUI on `LeanTea.Tui` — default UI |
| `Main.lean`     | controller — boots agents from the config, then hands off to `Tui.run` (or `--repl` for stdin-only mode) |

## CLI flags

| flag | default | meaning |
|---|---|---|
| `--config PATH` | `meta_orchestrator.config.json` | path to the JSON config file (read on startup, written by `/save`) |
| `--repl`        | off | disable the TUI, use the stdin slash-command REPL instead |

Per-agent knobs (`stallSec`, `pollSec`, `enabled`) live inside the
config file — either set them via `/add ID PANE GOAL...` (uses
defaults) and then hand-edit the JSON, or pre-write the file before
the first run.

## Resume

By default the orchestrator reads `--memo-log` on startup, filters by
session id, and seeds Gemini with the last 10 memos for that session.
Restarting after a crash or after the user-stops-it-for-the-day picks
up where it left off. Session id auto-derives from `sha-ish(--goal)`
so identical goals share memory across runs. Pass `--session FOO` to
fork (same goal, two trial paths) or `--fresh` to start cold.

The previous memo's `afterSummary` is back-filled on each stall —
right now just `"(pane changed)"` or `"(no change in pane)"` based on
the hash diff between the previous decision and the current pane.
A richer summary (last-N-lines of output, error markers) is a follow-up.

## What's deferred

- **Web view**. `Runtime.snapshot` already returns the full agent
  state; a `/web on` slash command that fires up `LeanTea.Net.Server`
  and serves a JSON status feed + accepts the slash commands as
  POSTs is the natural next step.
- **`Stop`-hook integration**. Polling is universal; a Claude Code
  `Stop` hook would let us call Gemini exactly once per agent turn
  instead of approximating via stall detection. Worth doing once the
  hook config is stable.
- **SQLite for memos**. JSONL is fine up to a few MB / agent. Above
  that, indexed queries (by session, by date, by outcome bucket) make
  more sense — `LeanTea.Persist.Query` is the natural fit, the
  current `Director.Memo.toJson` / `ofJson?` codec is the migration
  point.
- **Live TUI refresh while idle**. Snapshots refresh on every
  keystroke; between keystrokes the TUI is static. A `SIGWINCH`-driven
  or timer-driven repaint would show poll-count / status changes
  without user input. Currently non-blocking reads aren't in the
  widget kit.
