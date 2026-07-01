import LeanTea
import Lean.Data.Json

/-! # agent_dashboard_serve — visual control + telemetry for the Conductor

Wraps a `LeanTea.Agent.Conductor` in a single-page Web UI:

  * **Live**       — current playbook, current screenshot, last N runs.
  * **Playbooks**  — every defined playbook with bandit stats
                     (runs / win-rate / avg reward / last run) and a
                     toggle to enable/disable.
  * **Rewards**    — cumulative reward chart over time.

The dashboard owns the orchestrator and the conductor loop (boots
them at startup, spawns the loop in `IO.asTask`). Browser-side state
is read-only polling — every 1s the page re-fetches `/api/live` and
`/api/playbooks`.

```
agent_dashboard_serve \\
  --config examples/AgentDashboard/dashboard-config.json \\
  --store  examples/AgentDashboard/state \\
  --port   8040
```

The config JSON declares: the MCP servers to spawn (typically just
`browser_mcp_serve`), and the **observer** — a JS expression
evaluated against the page each tick whose return value becomes the
screen tag matched against playbook preconditions. -/

open Lean (Json)
open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanTea.Llm.McpOrchestrator
open LeanTea.Agent.Playbook
open LeanTea.Agent.Conductor

namespace AgentDashboard

/-! ## Config -/

structure FileConfig where
  /-- Orchestrator config (model + servers + baseUrl + systemPrompt).
      Reuses the same JSON shape as `llm_chat_*`. -/
  orchestrator : LeanTea.Llm.McpOrchestrator.FileConfig
  /-- JS expression — must return a string the conductor uses as the
      screen tag. Empty falls back to constant `"unknown"`. -/
  observerJs   : String
  /-- Loop tick in ms. -/
  tickMs       : Nat := 250

private def loadFileConfig (path : String) : IO FileConfig := do
  let src ← IO.FS.readFile path
  match Json.parse src with
  | .error e => throw <| IO.userError s!"config {path}: bad JSON: {e}"
  | .ok j =>
    let orchestrator ← LeanTea.Llm.McpOrchestrator.loadConfig path
    let observerJs :=
      (j.getObjVal? "observerJs").toOption.bind (·.getStr?.toOption) |>.getD ""
    let tickMs :=
      match (j.getObjVal? "tickMs").toOption with
      | some (.num n) => n.mantissa.toNat
      | _             => 250
    return { orchestrator, observerJs, tickMs }

/-! ## Observer

We support exactly one kind of observer in v1: evaluate a JS expression
via the browser MCP and treat its return value as the screen tag. Also
take a screenshot every tick so the dashboard can show what the agent
sees. -/

/-- Pull the first `image` content block out of an MCP tool response
    and return it as a `data:` URL, or `""` when there's none. -/
private def extractScreenshot (resp : Json) : String :=
  let content := (resp.getObjVal? "content").toOption.getD (Json.arr #[])
  let arr := (content.getArr?).toOption.getD #[]
  let firstImg := arr.findSome? fun item =>
    let typ := (item.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD ""
    if typ != "image" then none else
      let mime := (item.getObjVal? "mimeType").toOption.bind (·.getStr?.toOption) |>.getD "image/png"
      let data := (item.getObjVal? "data").toOption.bind (·.getStr?.toOption) |>.getD ""
      if data.isEmpty then none else some s!"data:{mime};base64,{data}"
  firstImg.getD ""

/-- Pull the text from a `browser_evaluate` response. -/
private def extractEvalText (resp : Json) : Option String :=
  let content := (resp.getObjVal? "content").toOption.getD (Json.arr #[])
  let arr := (content.getArr?).toOption.getD #[]
  arr.findSome? fun item =>
    let typ := (item.getObjVal? "type").toOption.bind (·.getStr?.toOption) |>.getD ""
    if typ != "text" then none else
      (item.getObjVal? "text").toOption.bind (·.getStr?.toOption)

private def jsObserver (jsExpr : String) : Observer := fun orch => do
  let shot ← (try orch.callTool "browser__browser_screenshot" (Json.mkObj [])
              catch _ => pure Json.null)
  let screenshot := extractScreenshot shot
  let screen ←
    if jsExpr.isEmpty then pure "unknown"
    else do
      let r ← (try orch.callTool "browser__browser_evaluate" (Json.mkObj [
                  ("expression", Json.str jsExpr)])
               catch _ => pure Json.null)
      pure ((extractEvalText r).getD "unknown").trimAscii.toString
  return { screen, screenshot, extra := Json.null }

/-! ## Server context -/

private structure Ctx where
  cfg      : Config
  state    : IO.Ref LiveState
  storeDir : String
  /-- The configured observer JS — surfaced read-only on /api/state. -/
  observerJs : String

/-! ## JSON serialisation -/

private def statsToUiJson (s : Stats) : Json :=
  Json.mkObj [
    ("runs",        Json.num (Int.ofNat s.runs)),
    ("wins",        Json.num (Int.ofNat s.wins)),
    ("losses",      Json.num (Int.ofNat s.losses)),
    ("totalReward", Json.str (toString s.totalReward)),
    ("avgReward",   Json.str (toString s.avgReward)),
    ("winRate",     Json.str (toString s.winRate)),
    ("lastRunMs",   Json.num (Int.ofNat s.lastRunMs)),
    ("recentWins",  Json.arr (s.recentWins.map Json.bool))
  ]

private def playbookToUiJson (pb : Playbook) (s : Stats) : Json :=
  Json.mkObj [
    ("id",          Json.str pb.id),
    ("name",        Json.str pb.name),
    ("description", Json.str pb.description),
    ("whenScreen",  Json.str pb.pre.whenScreen),
    ("estReward",   Json.str (toString pb.estReward)),
    ("enabled",     Json.bool pb.enabled),
    ("maxBurst",    Json.num (Int.ofNat pb.maxBurst)),
    ("stats",       statsToUiJson s)
  ]

private def historyEntryJson (h : HistoryEntry) : Json :=
  Json.mkObj [
    ("ts",         Json.num (Int.ofNat h.ts)),
    ("playbookId", Json.str h.playbookId),
    ("success",    Json.bool h.success),
    ("reward",     Json.str (toString h.reward)),
    ("durationMs", Json.num (Int.ofNat h.durationMs))
  ]

/-! ## Endpoints -/

private def handleState (ctx : Ctx) : IO Response := do
  let paused ← ctx.cfg.paused.get
  let aborted ← ctx.cfg.aborted.get
  let body := Json.mkObj [
    ("paused",     Json.bool paused),
    ("aborted",    Json.bool aborted),
    ("tickMs",     Json.num (Int.ofNat ctx.cfg.tickMs)),
    ("observerJs", Json.str ctx.observerJs),
    ("storeDir",   Json.str ctx.storeDir),
    ("serverCount", Json.num (Int.ofNat ctx.cfg.orch.servers.size)),
    ("toolCount",  Json.num (Int.ofNat ctx.cfg.orch.openAiTools.size))
  ]
  return Response.text 200 body.compress

private def handlePlaybooks (ctx : Ctx) : IO Response := do
  let pbs ← listPlaybooks ctx.storeDir
  let st ← ctx.state.get
  let arr := pbs.map fun pb =>
    let s := (st.stats.get? pb.id).getD {}
    playbookToUiJson pb s
  let body := Json.mkObj [("playbooks", Json.arr arr)]
  return Response.text 200 body.compress

private def handleLive (ctx : Ctx) : IO Response := do
  let st ← ctx.state.get
  let body := Json.mkObj [
    ("current",     Json.str st.current),
    ("screen",      Json.str st.observation.screen),
    ("screenshot",  Json.str st.observation.screenshot),
    ("cumReward",   Json.str (toString st.cumReward)),
    ("burstId",     Json.str st.burstId),
    ("burstCount",  Json.num (Int.ofNat st.burstCount)),
    ("history",     Json.arr (st.history.map historyEntryJson))
  ]
  return Response.text 200 body.compress

private def handleControl (ctx : Ctx) (req : Request) : IO Response := do
  match Json.parse (String.fromUTF8! req.body) with
  | .error e => return Response.json 400 Json.mkObj [("error", Json.str s!"bad json: {e}")]
  | .ok j =>
    let action := (j.getObjVal? "action").toOption.bind (·.getStr?.toOption) |>.getD ""
    match action with
    | "pause"  => ctx.cfg.paused.set true;  return Response.json 200 (Json.mkObj [("ok", Json.bool true)])
    | "resume" => ctx.cfg.paused.set false; return Response.json 200 (Json.mkObj [("ok", Json.bool true)])
    | "abort"  => ctx.cfg.aborted.set true; return Response.json 200 (Json.mkObj [("ok", Json.bool true)])
    | "reset-stats" =>
      ctx.state.modify (fun s => { s with stats := ∅, cumReward := 0.0, history := #[] })
      saveAllStats ctx.storeDir ∅
      return Response.json 200 (Json.mkObj [("ok", Json.bool true)])
    | _ => return Response.json 400 Json.mkObj [("error", Json.str s!"unknown action: {action}")]

private def handleToggle (ctx : Ctx) (req : Request) : IO Response := do
  match Json.parse (String.fromUTF8! req.body) with
  | .error e => return Response.json 400 Json.mkObj [("error", Json.str s!"bad json: {e}")]
  | .ok j =>
    let id := (j.getObjVal? "id").toOption.bind (·.getStr?.toOption) |>.getD ""
    let enabled :=
      match (j.getObjVal? "enabled").toOption with
      | some (.bool b) => b
      | _              => true
    match ← loadPlaybook ctx.storeDir id with
    | none    =>
      let body := (Json.mkObj [("error", Json.str s!"playbook {id} not found")]).compress
      return Response.text 404 body
    | some pb =>
      let pb' := { pb with enabled }
      pb'.save ctx.storeDir
      return Response.json 200 (Json.mkObj [("ok", Json.bool true)])

/-! ## Inline HTML page -/

private def pageHtml : String := "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>agent-dashboard</title>
<style>
  * { box-sizing: border-box; }
  html, body { height: 100%; margin: 0; font: 13px/1.45 ui-sans-serif, system-ui, -apple-system, 'Helvetica Neue', Arial;
               background: #0b0f17; color: #e4e7eb; }
  body { display: flex; flex-direction: column; }
  header {
    background: #111827; border-bottom: 1px solid #1f2937;
    padding: 8px 14px; display: flex; align-items: center; gap: 12px;
  }
  header h1 { font-size: 14px; font-weight: 600; margin: 0; color: #93c5fd; }
  header .meta { color: #6b7280; font-size: 12px; flex: 1; }
  header button {
    background: #374151; color: white; border: 0; border-radius: 6px;
    padding: 6px 10px; font: inherit; cursor: pointer; margin-left: 4px;
  }
  header button.go    { background: #16a34a; }
  header button.pause { background: #ca8a04; }
  header button.stop  { background: #dc2626; }
  header .pill {
    background: #1f2937; color: #93c5fd; padding: 2px 8px; border-radius: 999px;
    font-size: 11px;
  }
  header .pill.paused  { background: #422006; color: #fbbf24; }
  header .pill.aborted { background: #450a0a; color: #ef4444; }
  .tabs {
    background: #0f1422; border-bottom: 1px solid #1f2937;
    display: flex; gap: 0;
  }
  .tab {
    padding: 8px 14px; cursor: pointer; color: #cbd5e1;
    border-bottom: 2px solid transparent;
  }
  .tab.active { color: #93c5fd; border-bottom-color: #2563eb; }
  .pane { flex: 1; overflow: auto; display: none; }
  .pane.active { display: block; }
  /* live pane */
  #livePane { display: grid; grid-template-columns: minmax(0, 1fr) 320px; gap: 14px; padding: 14px; }
  #live-main { display: flex; flex-direction: column; gap: 10px; min-width: 0; }
  .card {
    background: #111827; border: 1px solid #1f2937; border-radius: 8px;
    padding: 10px 14px;
  }
  .card h3 {
    margin: 0 0 6px; font-size: 11px; color: #93c5fd; font-weight: 600;
    text-transform: uppercase; letter-spacing: 0.04em;
  }
  #screenshot {
    width: 100%; min-height: 200px; max-height: 60vh; object-fit: contain;
    background: #0b0f17; border-radius: 6px;
  }
  .kv {
    display: grid; grid-template-columns: 140px 1fr; gap: 4px 12px;
    font-size: 13px; color: #cbd5e1;
  }
  .kv .k { color: #6b7280; }
  .kv .v code { background: #1f2937; padding: 1px 6px; border-radius: 4px; }
  #history table { width: 100%; border-collapse: collapse; font-size: 12px; }
  #history th, #history td { text-align: left; padding: 4px 8px; border-bottom: 1px solid #1f2937; }
  #history th { color: #6b7280; font-weight: 600; font-size: 11px; text-transform: uppercase; }
  #history td.ok  { color: #4ade80; }
  #history td.bad { color: #f87171; }
  /* playbooks pane */
  #playbooksPane { padding: 14px; }
  #playbooksPane table { width: 100%; border-collapse: collapse; }
  #playbooksPane th, #playbooksPane td { padding: 8px 10px; border-bottom: 1px solid #1f2937; text-align: left; }
  #playbooksPane th { color: #6b7280; font-size: 11px; text-transform: uppercase; font-weight: 600; }
  #playbooksPane .id { font-family: ui-monospace, Menlo, monospace; color: #93c5fd; }
  .sparkline { display: inline-flex; gap: 1px; align-items: end; vertical-align: middle; height: 14px; }
  .sparkline span {
    display: inline-block; width: 4px; height: 100%;
    background: #374151; border-radius: 1px;
  }
  .sparkline span.win  { background: #4ade80; }
  .sparkline span.loss { background: #f87171; }
  /* rewards chart */
  #rewardsPane { padding: 14px; }
  #rewardChart {
    width: 100%; height: 280px;
    background: #0f1422; border: 1px solid #1f2937; border-radius: 6px;
  }
</style>
</head>
<body>
<header>
  <h1>agent-dashboard</h1>
  <span class=\"meta\" id=\"meta\">loading…</span>
  <span class=\"pill\" id=\"statusPill\">running</span>
  <button class=\"go\"    id=\"btnResume\">resume</button>
  <button class=\"pause\" id=\"btnPause\">pause</button>
  <button class=\"stop\"  id=\"btnAbort\">abort</button>
  <button id=\"btnReset\">reset stats</button>
</header>
<div class=\"tabs\">
  <div class=\"tab active\" data-pane=\"livePane\">live</div>
  <div class=\"tab\"        data-pane=\"playbooksPane\">playbooks</div>
  <div class=\"tab\"        data-pane=\"rewardsPane\">rewards</div>
</div>
<div id=\"livePane\" class=\"pane active\">
  <div id=\"live-main\">
    <div class=\"card\">
      <h3>screen</h3>
      <div class=\"kv\">
        <div class=\"k\">current playbook</div><div class=\"v\"><code id=\"liveCurrent\">—</code></div>
        <div class=\"k\">observed screen</div><div class=\"v\"><code id=\"liveScreen\">—</code></div>
        <div class=\"k\">cum reward</div><div class=\"v\" id=\"liveCum\">0</div>
        <div class=\"k\">burst</div><div class=\"v\" id=\"liveBurst\">—</div>
      </div>
    </div>
    <div class=\"card\">
      <h3>screenshot</h3>
      <img id=\"screenshot\" alt=\"(no screenshot)\">
    </div>
  </div>
  <div class=\"card\" id=\"history\">
    <h3>recent runs</h3>
    <table>
      <thead><tr><th>when</th><th>playbook</th><th>ok</th><th>reward</th><th>ms</th></tr></thead>
      <tbody id=\"historyRows\"></tbody>
    </table>
  </div>
</div>
<div id=\"playbooksPane\" class=\"pane\">
  <table>
    <thead><tr>
      <th>id</th><th>screen</th><th>est</th>
      <th>runs</th><th>wins</th><th>win-rate</th><th>avg reward</th><th>recent</th><th>enabled</th>
    </tr></thead>
    <tbody id=\"playbookRows\"></tbody>
  </table>
</div>
<div id=\"rewardsPane\" class=\"pane\">
  <canvas id=\"rewardChart\"></canvas>
</div>
<script>
const $ = id => document.getElementById(id);
const meta = $('meta'), statusPill = $('statusPill');
let paused = false, aborted = false;

document.querySelectorAll('.tab').forEach(t => t.addEventListener('click', () => {
  document.querySelectorAll('.tab').forEach(x => x.classList.remove('active'));
  document.querySelectorAll('.pane').forEach(x => x.classList.remove('active'));
  t.classList.add('active');
  $(t.dataset.pane).classList.add('active');
}));

async function api(method, path, body) {
  const init = { method };
  if (body !== undefined) {
    init.headers = {'content-type': 'application/json'};
    init.body = JSON.stringify(body);
  }
  const res = await fetch(path, init);
  return res.json();
}

async function refreshState() {
  const j = await api('GET', '/api/state');
  paused = j.paused; aborted = j.aborted;
  meta.textContent = `${j.serverCount} server(s) · ${j.toolCount} tool(s) · tick=${j.tickMs}ms · observerJs=${j.observerJs || '(none)'}`;
  statusPill.className = 'pill' + (aborted ? ' aborted' : paused ? ' paused' : '');
  statusPill.textContent = aborted ? 'aborted' : paused ? 'paused' : 'running';
}

function sparkline(arr) {
  return '<span class=\"sparkline\">' +
    arr.map(b => `<span class=\"${b ? 'win' : 'loss'}\"></span>`).join('') + '</span>';
}

function relTime(msAgo) {
  if (msAgo < 1500) return 'just now';
  const s = Math.floor(msAgo / 1000);
  if (s < 60) return s + 's ago';
  const m = Math.floor(s / 60);
  if (m < 60) return m + 'm ago';
  const h = Math.floor(m / 60);
  return h + 'h ago';
}

async function refreshLive() {
  const j = await api('GET', '/api/live');
  $('liveCurrent').textContent = j.current || '—';
  $('liveScreen').textContent  = j.screen   || '(unknown)';
  $('liveCum').textContent     = parseFloat(j.cumReward).toFixed(1);
  $('liveBurst').textContent   = j.burstId ? `${j.burstId} (${j.burstCount}x)` : '—';
  const ss = $('screenshot');
  if (j.screenshot) ss.src = j.screenshot;
  else ss.removeAttribute('src');

  const rows = $('historyRows');
  rows.innerHTML = '';
  const now = Date.now();
  /* j.history.ts is monoMsNow — not wall time, so we compare against the latest entry to get a relative-ish ordering. */
  const newest = j.history.length ? j.history[j.history.length - 1].ts : 0;
  for (let i = j.history.length - 1; i >= 0; i--) {
    const h = j.history[i];
    const tr = document.createElement('tr');
    const delta = newest - h.ts;
    tr.innerHTML = `<td>${relTime(delta)}</td>` +
                   `<td class=\"id\">${h.playbookId}</td>` +
                   `<td class=\"${h.success ? 'ok' : 'bad'}\">${h.success ? '✓' : '✗'}</td>` +
                   `<td>${parseFloat(h.reward).toFixed(1)}</td>` +
                   `<td>${h.durationMs}</td>`;
    rows.appendChild(tr);
    if (rows.children.length >= 40) break;
  }
}

async function refreshPlaybooks() {
  const j = await api('GET', '/api/playbooks');
  const rows = $('playbookRows');
  rows.innerHTML = '';
  for (const p of j.playbooks) {
    const tr = document.createElement('tr');
    tr.innerHTML =
      `<td class=\"id\">${p.id}</td>` +
      `<td><code>${p.whenScreen}</code></td>` +
      `<td>${parseFloat(p.estReward).toFixed(1)}</td>` +
      `<td>${p.stats.runs}</td>` +
      `<td>${p.stats.wins}</td>` +
      `<td>${(parseFloat(p.stats.winRate) * 100).toFixed(0)}%</td>` +
      `<td>${parseFloat(p.stats.avgReward).toFixed(2)}</td>` +
      `<td>${sparkline(p.stats.recentWins)}</td>` +
      `<td><input type=\"checkbox\" ${p.enabled ? 'checked' : ''} data-id=\"${p.id}\"></td>`;
    rows.appendChild(tr);
  }
  rows.querySelectorAll('input[type=checkbox]').forEach(cb => {
    cb.onchange = () => api('POST', '/api/playbooks/toggle', {id: cb.dataset.id, enabled: cb.checked});
  });
}

function drawRewards(history) {
  const c = $('rewardChart');
  const dpr = window.devicePixelRatio || 1;
  c.width = c.clientWidth * dpr;
  c.height = c.clientHeight * dpr;
  const ctx = c.getContext('2d');
  ctx.scale(dpr, dpr);
  const W = c.clientWidth, H = c.clientHeight;
  ctx.fillStyle = '#0f1422'; ctx.fillRect(0, 0, W, H);
  if (history.length < 2) {
    ctx.fillStyle = '#6b7280'; ctx.font = '13px system-ui';
    ctx.fillText('(need at least 2 runs to chart)', 12, 22);
    return;
  }
  let cum = 0, points = [];
  for (const h of history) { cum += parseFloat(h.reward); points.push(cum); }
  const max = Math.max(...points, 1);
  ctx.strokeStyle = '#2563eb'; ctx.lineWidth = 2;
  ctx.beginPath();
  points.forEach((v, i) => {
    const x = (i / (points.length - 1)) * (W - 20) + 10;
    const y = H - 10 - (v / max) * (H - 30);
    if (i === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
  });
  ctx.stroke();
  ctx.fillStyle = '#93c5fd'; ctx.font = '12px system-ui';
  ctx.fillText(`cum reward: ${cum.toFixed(1)} over ${points.length} runs`, 12, 18);
}

async function refreshRewards() {
  const j = await api('GET', '/api/live');
  drawRewards(j.history);
}

$('btnPause').onclick  = async () => { await api('POST', '/api/control', {action: 'pause'});  refreshState(); };
$('btnResume').onclick = async () => { await api('POST', '/api/control', {action: 'resume'}); refreshState(); };
$('btnAbort').onclick  = async () => {
  if (!confirm('Abort the conductor? This stops the loop until you restart the server.')) return;
  await api('POST', '/api/control', {action: 'abort'}); refreshState();
};
$('btnReset').onclick  = async () => {
  if (!confirm('Reset all bandit stats? This wipes the history but keeps the playbook definitions.')) return;
  await api('POST', '/api/control', {action: 'reset-stats'}); refreshState(); refreshLive(); refreshPlaybooks();
};

async function tick() {
  await Promise.all([refreshState(), refreshLive(), refreshPlaybooks(), refreshRewards()]);
}
tick();
setInterval(tick, 1000);
</script>
</body>
</html>
"

/-! ## Path matching + handler -/

private def matchPath (path : String) : Option String :=
  if path == "/api/playbooks/toggle" then some "toggle" else none

private def handler (ctx : Ctx) : Handler := fun req => do
  match req.path, req.method with
  | "/", _ => return Response.html 200 pageHtml
  | "/favicon.ico", _ => return { status := 204, headers := #[], body := .empty }
  | "/api/state",     "GET"  => handleState ctx
  | "/api/playbooks", "GET"  => handlePlaybooks ctx
  | "/api/playbooks/toggle", "POST" => handleToggle ctx req
  | "/api/live",      "GET"  => handleLive ctx
  | "/api/control",   "POST" => handleControl ctx req
  | _, _ => return Response.notFound

/-! ## CLI -/

private structure Args where
  port       : UInt16 := 8040
  host       : String := "0.0.0.0"
  configPath : String := ""
  storeDir   : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port"   :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8040).toUInt16 }
  | "--host"   :: v :: rest => parseArgs rest { a with host := v }
  | "--config" :: v :: rest => parseArgs rest { a with configPath := v }
  | "--store"  :: v :: rest => parseArgs rest { a with storeDir := v }
  | _ :: rest               => parseArgs rest a
  | []                      => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  if a.configPath.isEmpty then
    IO.eprintln "usage: agent_dashboard_serve --config <file.json> [--store DIR] [--port N] [--host H]"
    IO.Process.exit 2
  let storeDir :=
    if a.storeDir.isEmpty then "examples/AgentDashboard/state" else a.storeDir
  let fc ← loadFileConfig a.configPath
  IO.eprintln s!"agent-dashboard: spawning {fc.orchestrator.servers.size} MCP server(s)…"
  let orch ← LeanTea.Llm.McpOrchestrator.fromConfig fc.orchestrator
  IO.eprintln s!"agent-dashboard: loaded {orch.openAiTools.size} tool(s)"
  /- Restore stats from disk so a restart picks up where it left off. -/
  let stats0 ← loadAllStats storeDir
  let now0 ← IO.monoMsNow
  let state ← IO.mkRef ({
    current := "", observation := { screen := "(unknown)" },
    history := #[], stats := stats0, startedAtMs := now0,
    cumReward := 0.0, burstId := "", burstCount := 0
  } : LiveState)
  let cfg ← Config.mk' orch (jsObserver fc.observerJs) storeDir
                       defaultEscalator fc.tickMs
  let ctx : Ctx := { cfg, state, storeDir, observerJs := fc.observerJs }
  /- Spawn the conductor loop. -/
  let _ ← IO.asTask (runLoop cfg state)
  IO.eprintln s!"agent-dashboard: conductor running; http://{a.host}:{a.port}/"
  serveConcurrent a.port a.host (handler ctx)

end AgentDashboard

def main (args : List String) : IO Unit := AgentDashboard.serveMain args
