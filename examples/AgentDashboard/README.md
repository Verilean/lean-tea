# agent_dashboard_serve

Visual control + telemetry for `LeanTea.Agent.Conductor`. Pair with
`browser_mcp_serve` and a small JSON playbook collection to play a
browser game.

## Try it against the bundled toy game

The toy at `examples/AgentDashboard/game/index.html` is a 3-button
mini-game with reward hierarchy `Big Quest (+10) > Coin (+1) > Rest
(+0.1)`. It exposes `window.__screen` / `__coins` / `__energy` so the
observer can read state cheaply.

```bash
# 1. Serve the toy game on some port (any static file server works)
python3 -m http.server 8090 --directory examples/AgentDashboard/game &

# 2. Launch Chrome with remote debugging (one-time per profile dir)
mkdir -p /tmp/cdp-profile
/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome \
  --remote-debugging-port=9222 --user-data-dir=/tmp/cdp-profile \
  http://localhost:8090/ &

# 3. Boot the dashboard pointed at our example playbooks
mkdir -p examples/AgentDashboard/state/playbooks
cp examples/AgentDashboard/playbooks/*.json \
   examples/AgentDashboard/state/playbooks/

./.lake/build/bin/agent_dashboard_serve \
  --config examples/AgentDashboard/dashboard-config.json \
  --store  examples/AgentDashboard/state \
  --port   8040

# 4. Open the dashboard
open http://localhost:8040/
```

The conductor will:
1. Observe `window.__screen` every 500 ms.
2. Filter playbooks whose `pre.whenScreen` matches the observation.
3. UCB1-rank candidates by `Stats.avgReward + sqrt(2 ln N / n)`,
   using `estReward` as an optimistic prior on cold starts.
4. Run the picked playbook's `script` via the orchestrator's MCP calls.
5. Record outcome + accumulated reward; persist stats to disk.

After ~20 runs the bandit should converge to **Big Quest** when
energy ≥ 3 and **Rest** otherwise — Coin gets squeezed out because
its reward density is dominated by Big Quest.

## Smoke without a browser

```bash
mkdir -p /tmp/dash-smoke/playbooks
cp examples/AgentDashboard/playbooks/*.json /tmp/dash-smoke/playbooks/
./.lake/build/bin/agent_dashboard_serve \
  --config examples/AgentDashboard/dashboard-config-empty.json \
  --store  /tmp/dash-smoke --port 8041
```

The observer returns `"unknown"` every tick, no playbook matches, the
escalator prints `⚠ escalation: reason=no_match` and skips ahead 2 s.
`/api/state`, `/api/playbooks`, `/api/live` all return sane JSON —
useful for verifying wiring without booting Chrome.

## Adding playbooks

A playbook is one JSON file under `<store>/playbooks/`:

```json
{
  "id": "my_routine",
  "name": "human-readable name",
  "description": "...",
  "pre": {"whenScreen": "hub"},
  "estReward": 5.0,
  "maxBurst": 3,
  "timeoutMs": 6000,
  "enabled": true,
  "script": {
    "name": "my_routine",
    "steps": [
      {"act": "click_xy", "x": 100, "y": 200},
      {"act": "wait", "ms": 500},
      {"act": "tool_call",
       "tool": "browser__browser_evaluate",
       "args": {"expression": "({reward: 5})"}}
    ]
  }
}
```

The conductor hot-reloads the playbook directory every tick — drop a
new file in and it shows up in the dashboard on the next refresh.

Step shapes are the same as `LeanTea.Agent.Script.Action`:
`click_xy`, `wait`, `screenshot`, `wait_for_screen`, `tool_call`.
`tool_call` is the escape hatch — invoke any MCP tool with any args.
