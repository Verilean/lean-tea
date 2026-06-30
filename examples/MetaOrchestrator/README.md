# meta_orchestrator — Gemini-driven PM for Claude Code

A small, polling-based meta-agent that watches a zellij pane (typically
the one running `claude` / Claude Code) and decides when to nudge it
with a new instruction. Built on:

- **`LeanTea.Cloud.Gemini`** — for the decision LLM
- **`zellij action dump-screen` / `write-chars`** — for pane I/O
- A FNV-1a hash of the pane text — for cheap stall detection

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
# Inside a zellij session, in a separate pane from the one running Claude:
lake build meta_orchestrator
./.lake/build/bin/meta_orchestrator \
  --pane terminal_18 \
  --goal "Optimise the FlashAttention v3 backwards kernel for SM90; target 2× throughput on a per-token-streaming workload" \
  --stall-secs 45 \
  --poll-secs 5 \
  --log meta_orchestrator.jsonl
```

Pane id discovery: `zellij action dump-layout` lists every pane id;
`terminal_N` is the stable form.

Required env: `GEMINI_API_KEY` (get one at https://aistudio.google.com/apikey).

## File layout

| file | role |
|---|---|
| `Zellij.lean`   | `dumpScreen` / `writeChars` / `submit` over the `zellij action` CLI |
| `Director.lean` | builds the Gemini prompt, parses the verdict JSON |
| `Main.lean`     | poll loop, stall detection, JSONL audit log |

## Knobs

| flag | default | meaning |
|---|---|---|
| `--pane`       | (required) | zellij pane id to watch + write to |
| `--goal`       | (required) | long-form objective shipped on every Gemini call |
| `--stall-secs` | 30 | pane-text hash must be stable for this long to count as "idle" |
| `--poll-secs`  |  5 | how often to dump-screen |
| `--log`        | `meta_orchestrator.jsonl` | one JSON record per decision |
| `--dry-run`    | off | print decisions instead of writing back |

## What's deferred

- **Multi-pane / multi-agent**. The current loop watches one pane. The
  Zellij/Director split is clean enough that a second loop can run
  beside the first against a different pane — just `IO.asTask` it.
- **TUI**. Logs go to stderr + JSONL. A `textual`-style UI would be
  nice for live decision review, but the JSONL log makes that an
  out-of-process build.
- **`Stop`-hook integration**. Polling is universal; a Claude Code
  `Stop` hook would let us call Gemini exactly once per agent turn
  instead of approximating via stall detection. Worth doing once the
  hook config is stable.
- **Long-term project memory**. The `Director.Memo` history is the
  last 10 decisions only. A SQLite-backed history (via
  `LeanTea.Persist.Query`) would let Gemini reason over weeks of
  iteration.
