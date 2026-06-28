import LeanTea
import Lean.Data.Json

/-! # llm_chat_web — single-page chat UI talking to LM Studio + MCP

A minimal browser chat:

  * `GET  /`            — single HTML page (inlined; no template file needed)
  * `GET  /api/state`   — return the conversation as JSON
  * `POST /api/send`    — body `{"message": "..."}`; runs one orchestrator
                          turn and returns the new messages

The UI renders bubbles per role with a special tool-call style.
Single in-memory session shared across browser tabs — this is a
single-user developer toy, not a multi-tenant SaaS. -/

open Lean (Json)
open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanTea.Llm.McpOrchestrator

namespace LlmChatWeb

/-! ## State -/

private structure Session where
  history : Array ChatMsg

private def Session.empty : Session := { history := #[] }

/-! ## ChatMsg → JSON for the UI -/

private def msgToUiJson (m : ChatMsg) : Json :=
  let callsJson := Json.arr <| m.toolCalls.map fun c =>
    let fn := (c.getObjVal? "function").toOption.getD Json.null
    let name := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
    let args := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
    Json.mkObj [("name", Json.str name), ("args", Json.str args)]
  Json.mkObj [
    ("role",       Json.str m.role.toString),
    ("text",       Json.str m.text),
    ("toolCalls",  callsJson),
    ("toolName",   Json.str m.toolName)
  ]

private def historyToJson (h : Array ChatMsg) : Json :=
  Json.arr (h.map msgToUiJson)

/-! ## HTML page (inlined) -/

private def pageHtml : String := "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>llm-chat</title>
<style>
  * { box-sizing: border-box; }
  html, body { height: 100%; margin: 0; }
  body {
    font: 14px/1.45 ui-sans-serif, system-ui, -apple-system, 'Helvetica Neue', Arial;
    background: #0b0f17; color: #e4e7eb;
    display: flex; flex-direction: column;
  }
  header {
    background: #111827; border-bottom: 1px solid #1f2937;
    padding: 10px 16px; display: flex; align-items: center; gap: 12px;
  }
  header h1 { font-size: 14px; font-weight: 600; margin: 0; color: #93c5fd; }
  header .meta { color: #6b7280; font-size: 12px; }
  #log {
    flex: 1; overflow-y: auto; padding: 16px;
    display: flex; flex-direction: column; gap: 10px;
  }
  .bubble {
    max-width: 75%; padding: 10px 14px; border-radius: 12px;
    white-space: pre-wrap; word-wrap: break-word;
  }
  .bubble.user      { align-self: flex-end;   background: #1e3a8a; color: #e0e7ff; }
  .bubble.assistant { align-self: flex-start; background: #1f2937; color: #e5e7eb; }
  .toolblock {
    align-self: flex-start; max-width: 90%;
    border-left: 3px solid #fbbf24;
    background: #1c1917; color: #fde68a;
    font: 12px/1.4 ui-monospace, 'SF Mono', Menlo, monospace;
    padding: 6px 10px; border-radius: 4px;
  }
  .toolblock .call { color: #fbbf24; }
  .toolblock .result {
    color: #a3a3a3; margin-top: 2px;
    max-height: 200px; overflow: auto;
    white-space: pre-wrap;
  }
  .status { color: #6b7280; font-style: italic; padding: 4px 8px; }
  footer {
    background: #111827; border-top: 1px solid #1f2937;
    padding: 10px 12px; display: flex; gap: 8px;
  }
  #input {
    flex: 1; background: #0b0f17; color: #e4e7eb;
    border: 1px solid #374151; border-radius: 8px;
    padding: 10px 12px; font: inherit;
  }
  #send {
    background: #2563eb; color: white; border: 0; border-radius: 8px;
    padding: 0 16px; font: inherit; font-weight: 600; cursor: pointer;
  }
  #send:disabled { background: #374151; cursor: progress; }
</style>
</head>
<body>
  <header>
    <h1>llm-chat</h1>
    <span class=\"meta\" id=\"meta\">loading…</span>
  </header>
  <div id=\"log\"></div>
  <footer>
    <textarea id=\"input\" rows=\"1\" placeholder=\"type a message — Enter to send, Shift+Enter for newline\"></textarea>
    <button id=\"send\">send</button>
  </footer>
<script>
const log = document.getElementById('log');
const input = document.getElementById('input');
const send = document.getElementById('send');
const meta = document.getElementById('meta');

const esc = s => s.replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));

function render(messages) {
  log.innerHTML = '';
  for (const m of messages) {
    if (m.role === 'user' || m.role === 'assistant') {
      if (m.text) {
        const div = document.createElement('div');
        div.className = 'bubble ' + m.role;
        div.textContent = m.text;
        log.appendChild(div);
      }
      if (m.toolCalls && m.toolCalls.length) {
        for (const c of m.toolCalls) {
          const tb = document.createElement('div');
          tb.className = 'toolblock';
          tb.innerHTML = `<div class=\"call\">→ ${esc(c.name)}(${esc(c.args)})</div>`;
          log.appendChild(tb);
        }
      }
    } else if (m.role === 'tool') {
      // attach as result of the previous toolblock if possible
      const tbs = log.querySelectorAll('.toolblock');
      const last = tbs[tbs.length - 1];
      if (last) {
        const r = document.createElement('div');
        r.className = 'result';
        r.textContent = m.text;
        last.appendChild(r);
      } else {
        const tb = document.createElement('div');
        tb.className = 'toolblock';
        tb.innerHTML = `<div class=\"call\">↳ ${esc(m.toolName || '(tool)')}</div><div class=\"result\">${esc(m.text)}</div>`;
        log.appendChild(tb);
      }
    }
  }
  log.scrollTop = log.scrollHeight;
}

function showStatus(s) {
  // append/replace a status line at the bottom
  let st = document.querySelector('.status');
  if (!st) {
    st = document.createElement('div');
    st.className = 'status';
    log.appendChild(st);
  }
  st.textContent = s;
  log.scrollTop = log.scrollHeight;
}

function clearStatus() {
  const st = document.querySelector('.status');
  if (st) st.remove();
}

async function loadState() {
  const res = await fetch('/api/state');
  const j = await res.json();
  meta.textContent = `model=${j.model} · ${j.servers} server(s) · ${j.tools} tool(s)`;
  render(j.messages);
}

async function sendMessage() {
  const text = input.value.trim();
  if (!text) return;
  input.value = '';
  send.disabled = true;
  showStatus('thinking…');
  try {
    const res = await fetch('/api/send', {
      method: 'POST',
      headers: {'content-type': 'application/json'},
      body: JSON.stringify({message: text})
    });
    const j = await res.json();
    clearStatus();
    if (j.error) {
      showStatus('error: ' + j.error);
    } else {
      render(j.messages);
    }
  } catch (e) {
    showStatus('network error: ' + e.message);
  } finally {
    send.disabled = false;
    input.focus();
  }
}

send.addEventListener('click', sendMessage);
input.addEventListener('keydown', e => {
  if (e.key === 'Enter' && !e.shiftKey) {
    e.preventDefault();
    sendMessage();
  }
});

loadState();
input.focus();
</script>
</body>
</html>
"

/-! ## Handler -/

private structure Ctx where
  orch    : Orchestrator
  session : IO.Ref Session

private def stateJson (ctx : Ctx) : IO Json := do
  let s ← ctx.session.get
  return Json.mkObj [
    ("model",    Json.str ctx.orch.model),
    ("servers",  Json.num (Int.ofNat ctx.orch.servers.size)),
    ("tools",    Json.num (Int.ofNat ctx.orch.openAiTools.size)),
    ("messages", historyToJson s.history)
  ]

private def handleSend (ctx : Ctx) (req : Request) : IO Response := do
  match Json.parse (String.fromUTF8! req.body) with
  | .error e =>
    let body := Json.mkObj [("error", Json.str s!"bad json: {e}")]
    return Response.text 400 body.compress
  | .ok j =>
    let msg := (j.getObjVal? "message").toOption.bind (·.getStr?.toOption) |>.getD ""
    if msg.isEmpty then
      let body := Json.mkObj [("error", Json.str "message must be a non-empty string")]
      return Response.text 400 body.compress
    let s ← ctx.session.get
    try
      let newMsgs ← ctx.orch.runTurn s.history msg
      let updated := s.history ++ newMsgs
      ctx.session.set { history := updated }
      let body := Json.mkObj [("messages", historyToJson updated)]
      return Response.text 200 body.compress
    catch e =>
      let body := Json.mkObj [("error", Json.str s!"{e}")]
      return Response.text 500 body.compress

private def handler (ctx : Ctx) : Handler := fun req => do
  match req.path, req.method with
  | "/",            _      => return Response.html 200 pageHtml
  | "/api/state",   "GET"  =>
    let body := (← stateJson ctx).compress
    return Response.text 200 body
  | "/api/send",    "POST" => handleSend ctx req
  | "/favicon.ico", _      => return { status := 204, headers := #[], body := .empty }
  | _, _ => return Response.notFound

/-! ## CLI -/

private structure Args where
  port       : UInt16 := 8030
  host       : String := "0.0.0.0"
  configPath : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port" :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8030).toUInt16 }
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | "--config" :: v :: rest => parseArgs rest { a with configPath := v }
  | _ :: rest             => parseArgs rest a
  | []                    => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  if a.configPath.isEmpty then
    IO.eprintln "usage: llm_chat_web --config <file.json> [--port N] [--host H]"
    IO.Process.exit 2
  let fc ← loadConfig a.configPath
  IO.eprintln s!"llm-chat-web: spawning {fc.servers.size} MCP server(s)…"
  let orch ← fromConfig fc
  IO.eprintln s!"llm-chat-web: loaded {orch.openAiTools.size} tool(s); \
serving on http://{a.host}:{a.port}/"
  let session ← IO.mkRef Session.empty
  let ctx : Ctx := { orch, session }
  serve a.port a.host (handler ctx)

end LlmChatWeb

def main (args : List String) : IO Unit := LlmChatWeb.serveMain args
