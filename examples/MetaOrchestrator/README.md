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
| `/reply ID TEXT...`       | inject a free-text reply (use when Gemini said `ask_user`) |
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
  "geminiModel": "gemini-2.5-pro"
}
```

The file is hand-editable. Per-agent memo + decision logs live in
`<logDir>/<agentId>.memos.jsonl` and `<agentId>.decisions.jsonl`.

## File layout

| file | role |
|---|---|
| `Zellij.lean`   | `dumpScreen` / `writeChars` / `submit` over the `zellij action` CLI |
| `Director.lean` | builds the Gemini prompt, parses the verdict JSON |
| `Config.lean`   | `ManagedAgent` / `Config` records + JSON codec + load/save/add/remove |
| `Runtime.lean`  | per-agent polling loop (`IO.asTask`), snapshot for the controller |
| `Main.lean`     | controller — boots agents from the config, slash-command REPL |

## CLI flags

| flag | default | meaning |
|---|---|---|
| `--config PATH` | `meta_orchestrator.config.json` | path to the JSON config file (read on startup, written by `/save`) |

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

- **TUI**. The REPL is line-mode today. A `Runtime.snapshot` call
  already returns the full per-agent state list, so a TUI repaint
  layer slots in next without touching the runtime.
- **Web view**. Same `Runtime.snapshot` — a `/web on` slash command
  that fires up `LeanTea.Net.Server` and serves a JSON status feed
  + accepts the slash commands as POSTs is the next commit.
- **`Stop`-hook integration**. Polling is universal; a Claude Code
  `Stop` hook would let us call Gemini exactly once per agent turn
  instead of approximating via stall detection. Worth doing once the
  hook config is stable.
- **SQLite for memos**. JSONL is fine up to a few MB / agent. Above
  that, indexed queries (by session, by date, by outcome bucket) make
  more sense — `LeanTea.Persist.Query` is the natural fit, the
  current `Director.Memo.toJson` / `ofJson?` codec is the migration
  point.
