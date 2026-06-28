import LeanTea
import Lean.Data.Json

/-! # llm_chat_web — single-page chat with sessions sidebar + images

  * Left sidebar — past sessions (newest first); `+ new` button.
  * Main pane — bubbles per role; tool calls collapsed; inline images.
  * Input area — paste / drag-drop / file-picker images.

Routes:

  * `GET  /`                      single HTML page
  * `GET  /api/state`             current model + servers + tools + active session id
  * `GET  /api/sessions`          list every saved session (newest first)
  * `POST /api/sessions/new`      create a fresh session, return its id
  * `GET  /api/sessions/:id`      load full history
  * `DELETE /api/sessions/:id`    remove
  * `POST /api/sessions/:id/send` body `{message, images:[dataUrl,...]}`

Sessions are persisted to `~/.cache/leantea-chat/` (override with
`--store DIR` / `LLM_CHAT_STORE`). Each session is a single JSON
file with the history inlined including base64 image data — fine
for the single-user developer scale this app is built for. -/

open Lean (Json)
open LeanTea LeanTea.Net.Http LeanTea.Net.Server
open LeanTea.Llm.McpOrchestrator

namespace LlmChatWeb

/-! ## ChatMsg → JSON for the UI -/

private def msgToUiJson (m : ChatMsg) : Json :=
  let callsJson := Json.arr <| m.toolCalls.map fun c =>
    let fn := (c.getObjVal? "function").toOption.getD Json.null
    let name := (fn.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
    let args := (fn.getObjVal? "arguments").toOption.bind (·.getStr?.toOption) |>.getD "{}"
    Json.mkObj [("name", Json.str name), ("args", Json.str args)]
  Json.mkObj [
    ("role",      Json.str m.role.toString),
    ("text",      Json.str m.text),
    ("images",    Json.arr (m.images.map Json.str)),
    ("toolCalls", callsJson),
    ("toolName",  Json.str m.toolName)
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
    display: grid; grid-template-columns: 240px 1fr; grid-template-rows: 100%;
  }
  #sidebar {
    background: #0f1422; border-right: 1px solid #1f2937;
    display: flex; flex-direction: column;
  }
  #sidebar header {
    padding: 12px 14px; border-bottom: 1px solid #1f2937;
    display: flex; align-items: center; gap: 8px;
  }
  #sidebar h2 { font-size: 13px; font-weight: 600; margin: 0; color: #93c5fd; flex: 1; }
  #newchat {
    background: #2563eb; color: white; border: 0; border-radius: 6px;
    padding: 5px 9px; font: inherit; cursor: pointer;
  }
  #sessions { flex: 1; overflow-y: auto; padding: 6px; }
  .session {
    padding: 8px 10px; border-radius: 6px; cursor: pointer;
    color: #cbd5e1; font-size: 13px; line-height: 1.3;
    display: flex; align-items: center; gap: 6px;
  }
  .session:hover { background: #1f2937; }
  .session.active { background: #1e3a8a; color: #e0e7ff; }
  .session .title {
    flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  }
  .session .count { color: #6b7280; font-size: 11px; }
  .session .del {
    background: none; border: 0; color: #6b7280; cursor: pointer;
    padding: 0 4px; font-size: 14px;
  }
  .session .del:hover { color: #ef4444; }
  #main { display: flex; flex-direction: column; min-width: 0; }
  #topbar {
    background: #111827; border-bottom: 1px solid #1f2937;
    padding: 10px 16px; display: flex; align-items: center; gap: 12px;
  }
  #topbar h1 { font-size: 14px; font-weight: 600; margin: 0; color: #93c5fd; }
  #topbar .meta { color: #6b7280; font-size: 12px; }
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
  .bubble img {
    max-width: 100%; max-height: 320px; border-radius: 6px;
    display: block; margin-top: 8px;
  }
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
  .toolblock img {
    max-width: 100%; max-height: 320px; border-radius: 6px;
    display: block; margin-top: 6px;
  }
  .status { color: #6b7280; font-style: italic; padding: 4px 8px; }
  footer {
    background: #111827; border-top: 1px solid #1f2937;
    padding: 8px 12px; display: flex; flex-direction: column; gap: 6px;
  }
  #attachments {
    display: flex; gap: 6px; flex-wrap: wrap; min-height: 0;
  }
  .thumb {
    position: relative; width: 64px; height: 64px;
    border-radius: 6px; overflow: hidden; border: 1px solid #374151;
    background: #1f2937;
  }
  .thumb img { width: 100%; height: 100%; object-fit: cover; }
  .thumb .x {
    position: absolute; top: 2px; right: 2px;
    background: rgba(0,0,0,0.6); color: white;
    border: 0; border-radius: 50%; width: 18px; height: 18px;
    cursor: pointer; line-height: 1; font-size: 12px;
  }
  .row { display: flex; gap: 8px; align-items: center; }
  #input {
    flex: 1; background: #0b0f17; color: #e4e7eb;
    border: 1px solid #374151; border-radius: 8px;
    padding: 10px 12px; font: inherit; resize: vertical; min-height: 38px;
  }
  #attach {
    background: #374151; color: #e4e7eb; border: 0; border-radius: 8px;
    padding: 0 12px; height: 38px; font: inherit; cursor: pointer;
  }
  #send {
    background: #2563eb; color: white; border: 0; border-radius: 8px;
    padding: 0 16px; height: 38px; font: inherit; font-weight: 600; cursor: pointer;
  }
  #send:disabled { background: #374151; cursor: progress; }
  body.dragging #main { outline: 3px dashed #2563eb; outline-offset: -16px; }
</style>
</head>
<body>
<aside id=\"sidebar\">
  <header>
    <h2>chats</h2>
    <button id=\"newchat\" title=\"new chat\">+ new</button>
  </header>
  <div id=\"sessions\"></div>
</aside>
<section id=\"main\">
  <div id=\"topbar\">
    <h1>llm-chat</h1>
    <span class=\"meta\" id=\"meta\">loading…</span>
  </div>
  <div id=\"log\"></div>
  <footer>
    <div id=\"attachments\"></div>
    <div class=\"row\">
      <textarea id=\"input\" rows=\"1\" placeholder=\"type a message — Enter to send, Shift+Enter for newline, paste/drop an image to attach\"></textarea>
      <button id=\"attach\" title=\"attach image\">📎</button>
      <input id=\"file\" type=\"file\" accept=\"image/*\" multiple style=\"display:none\">
      <button id=\"send\">send</button>
    </div>
  </footer>
</section>
<script>
const log = document.getElementById('log');
const input = document.getElementById('input');
const send = document.getElementById('send');
const attachBtn = document.getElementById('attach');
const fileIn = document.getElementById('file');
const attachments = document.getElementById('attachments');
const sessionsEl = document.getElementById('sessions');
const newchat = document.getElementById('newchat');
const meta = document.getElementById('meta');

let activeSessionId = null;
let pendingImages = [];

const esc = s => s.replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));

function renderHistory(messages) {
  log.innerHTML = '';
  for (const m of messages) {
    if (m.role === 'user' || m.role === 'assistant') {
      // skip empty user msgs that only carry a synthetic vision attachment
      if (!m.text && (!m.images || !m.images.length) && (!m.toolCalls || !m.toolCalls.length)) continue;
      if (m.text || (m.images && m.images.length)) {
        const div = document.createElement('div');
        div.className = 'bubble ' + m.role;
        if (m.text) div.textContent = m.text;
        for (const url of (m.images || [])) {
          const img = document.createElement('img');
          img.src = url;
          div.appendChild(img);
        }
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
      const tbs = log.querySelectorAll('.toolblock');
      const last = tbs[tbs.length - 1];
      const host = last || (() => {
        const tb = document.createElement('div');
        tb.className = 'toolblock';
        tb.innerHTML = `<div class=\"call\">↳ ${esc(m.toolName || '(tool)')}</div>`;
        log.appendChild(tb);
        return tb;
      })();
      if (m.text) {
        const r = document.createElement('div');
        r.className = 'result';
        r.textContent = m.text;
        host.appendChild(r);
      }
      for (const url of (m.images || [])) {
        const img = document.createElement('img');
        img.src = url;
        host.appendChild(img);
      }
    }
  }
  log.scrollTop = log.scrollHeight;
}

function showStatus(s) {
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

function renderAttachments() {
  attachments.innerHTML = '';
  pendingImages.forEach((url, i) => {
    const t = document.createElement('div');
    t.className = 'thumb';
    t.innerHTML = `<img src=\"${url}\"><button class=\"x\" title=\"remove\">×</button>`;
    t.querySelector('.x').onclick = () => {
      pendingImages.splice(i, 1);
      renderAttachments();
    };
    attachments.appendChild(t);
  });
}

async function loadSessions() {
  const res = await fetch('/api/sessions');
  const j = await res.json();
  sessionsEl.innerHTML = '';
  for (const s of j.sessions) {
    const div = document.createElement('div');
    div.className = 'session' + (s.id === activeSessionId ? ' active' : '');
    div.innerHTML = `<span class=\"title\">${esc(s.name || '(empty)')}</span>` +
                    `<span class=\"count\">${s.count}</span>` +
                    `<button class=\"del\" title=\"delete\">×</button>`;
    div.querySelector('.title').onclick = () => selectSession(s.id);
    div.querySelector('.del').onclick = async (e) => {
      e.stopPropagation();
      if (!confirm('Delete this chat?')) return;
      await fetch('/api/sessions/' + s.id, { method: 'DELETE' });
      if (s.id === activeSessionId) {
        activeSessionId = null;
        renderHistory([]);
      }
      loadSessions();
    };
    sessionsEl.appendChild(div);
  }
}

async function selectSession(id) {
  activeSessionId = id;
  const res = await fetch('/api/sessions/' + id);
  const j = await res.json();
  renderHistory(j.messages);
  loadSessions();
}

async function newSession() {
  const res = await fetch('/api/sessions/new', { method: 'POST' });
  const j = await res.json();
  activeSessionId = j.id;
  pendingImages = [];
  renderAttachments();
  renderHistory([]);
  loadSessions();
}

async function loadState() {
  const res = await fetch('/api/state');
  const j = await res.json();
  meta.textContent = `model=${j.model} · ${j.servers} server(s) · ${j.tools} tool(s)`;
}

function fileToDataUrl(file) {
  return new Promise((resolve, reject) => {
    const r = new FileReader();
    r.onload = () => resolve(r.result);
    r.onerror = reject;
    r.readAsDataURL(file);
  });
}

async function attachFiles(files) {
  for (const f of files) {
    if (!f.type.startsWith('image/')) continue;
    const url = await fileToDataUrl(f);
    pendingImages.push(url);
  }
  renderAttachments();
}

async function sendMessage() {
  const text = input.value.trim();
  if (!text && !pendingImages.length) return;
  if (!activeSessionId) {
    await newSession();
  }
  input.value = '';
  const imgs = pendingImages.slice();
  pendingImages = [];
  renderAttachments();
  send.disabled = true;
  showStatus('thinking…');
  try {
    const res = await fetch('/api/sessions/' + activeSessionId + '/send', {
      method: 'POST',
      headers: {'content-type': 'application/json'},
      body: JSON.stringify({message: text, images: imgs})
    });
    const j = await res.json();
    clearStatus();
    if (j.error) {
      showStatus('error: ' + j.error);
    } else {
      renderHistory(j.messages);
      loadSessions();
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
newchat.addEventListener('click', newSession);
attachBtn.addEventListener('click', () => fileIn.click());
fileIn.addEventListener('change', e => {
  attachFiles(e.target.files);
  e.target.value = '';
});
input.addEventListener('paste', e => {
  const items = e.clipboardData && e.clipboardData.items;
  if (!items) return;
  const files = [];
  for (const it of items) {
    if (it.kind === 'file') {
      const f = it.getAsFile();
      if (f) files.push(f);
    }
  }
  if (files.length) {
    e.preventDefault();
    attachFiles(files);
  }
});
document.addEventListener('dragenter', e => { e.preventDefault(); document.body.classList.add('dragging'); });
document.addEventListener('dragover',  e => { e.preventDefault(); });
document.addEventListener('dragleave', e => {
  if (e.target === document || e.target === document.body) document.body.classList.remove('dragging');
});
document.addEventListener('drop', e => {
  e.preventDefault();
  document.body.classList.remove('dragging');
  if (e.dataTransfer && e.dataTransfer.files) attachFiles(e.dataTransfer.files);
});

loadState();
loadSessions();
input.focus();
</script>
</body>
</html>
"

/-! ## Server state -/

private structure Ctx where
  orch    : Orchestrator
  storeDir : String
  /-- In-memory cache for the active session — avoids re-reading
      the JSON file on every send. -/
  cache   : IO.Ref (Std.HashMap String LeanTea.Llm.ChatStore.Session)

private def Ctx.loadSession (ctx : Ctx) (id : String) : IO (Option LeanTea.Llm.ChatStore.Session) := do
  let m ← ctx.cache.get
  if let some s := m.get? id then return some s
  match ← LeanTea.Llm.ChatStore.load ctx.storeDir id with
  | none   => return none
  | some s =>
    ctx.cache.set (m.insert id s)
    return some s

private def Ctx.saveSession (ctx : Ctx) (s : LeanTea.Llm.ChatStore.Session)
    : IO LeanTea.Llm.ChatStore.Session := do
  let s' ← LeanTea.Llm.ChatStore.save ctx.storeDir s
  let m ← ctx.cache.get
  ctx.cache.set (m.insert s.id s')
  return s'

private def stateJson (ctx : Ctx) : IO Json := do
  return Json.mkObj [
    ("model",   Json.str ctx.orch.model),
    ("servers", Json.num (Int.ofNat ctx.orch.servers.size)),
    ("tools",   Json.num (Int.ofNat ctx.orch.openAiTools.size))
  ]

private def sessionsListJson (ctx : Ctx) : IO Json := do
  let summaries ← LeanTea.Llm.ChatStore.list ctx.storeDir
  return Json.mkObj [
    ("sessions", Json.arr (summaries.map LeanTea.Llm.ChatStore.summaryToJson))
  ]

private def handleNewSession (ctx : Ctx) : IO Response := do
  let s ← LeanTea.Llm.ChatStore.newSession
  let _ ← ctx.saveSession s
  let body := Json.mkObj [("id", Json.str s.id)]
  return Response.text 200 body.compress

private def handleGetSession (ctx : Ctx) (id : String) : IO Response := do
  match ← ctx.loadSession id with
  | none   =>
    let body := Json.mkObj [("error", Json.str s!"session {id} not found")]
    return Response.text 404 body.compress
  | some s =>
    let body := Json.mkObj [
      ("id",       Json.str s.id),
      ("name",     Json.str s.name),
      ("messages", historyToJson s.messages)
    ]
    return Response.text 200 body.compress

private def handleDeleteSession (ctx : Ctx) (id : String) : IO Response := do
  let _ ← LeanTea.Llm.ChatStore.delete ctx.storeDir id
  let m ← ctx.cache.get
  ctx.cache.set (m.erase id)
  return Response.text 200 "{\"ok\":true}"

private def parseImages (j : Json) : Array String :=
  match (j.getObjVal? "images").toOption.bind (·.getArr?.toOption) with
  | some a => a.filterMap (·.getStr?.toOption)
  | none   => #[]

private def handleSend (ctx : Ctx) (id : String) (req : Request) : IO Response := do
  match Json.parse (String.fromUTF8! req.body) with
  | .error e =>
    let body := Json.mkObj [("error", Json.str s!"bad json: {e}")]
    return Response.text 400 body.compress
  | .ok j =>
    let msg := (j.getObjVal? "message").toOption.bind (·.getStr?.toOption) |>.getD ""
    let images := parseImages j
    if msg.isEmpty && images.isEmpty then
      let body := Json.mkObj [("error", Json.str "need either message or images")]
      return Response.text 400 body.compress
    match ← ctx.loadSession id with
    | none =>
      let body := Json.mkObj [("error", Json.str s!"session {id} not found")]
      return Response.text 404 body.compress
    | some s =>
      try
        let newMsgs ← ctx.orch.runTurnFull s.messages msg images
        let updated : LeanTea.Llm.ChatStore.Session :=
          { s with messages := s.messages ++ newMsgs }
        let updated' ← ctx.saveSession updated
        let body := Json.mkObj [("messages", historyToJson updated'.messages)]
        return Response.text 200 body.compress
      catch e =>
        let body := Json.mkObj [("error", Json.str s!"{e}")]
        return Response.text 500 body.compress

/-! ## Routing

We hand-roll path matching for `/api/sessions/<id>` and its `/send`
suffix because the framework's `Handler` is just `Request → IO
Response` — no router. Splitting on `/` and checking the prefix
keeps it minimal. -/

private def matchSessionPath (path : String) : Option (String × Option String) :=
  /- `/api/sessions/ID` → `(ID, none)`
     `/api/sessions/ID/send` → `(ID, some "send")` -/
  let prefix_ := "/api/sessions/"
  if !path.startsWith prefix_ then none
  else
    let rest := (path.drop prefix_.length).toString
    if rest.isEmpty then none
    else
      match rest.splitOn "/" with
      | [id]      => some (id, none)
      | [id, sub] => some (id, some sub)
      | _ => none

private def handler (ctx : Ctx) : Handler := fun req => do
  match req.path, req.method with
  | "/", _ => return Response.html 200 pageHtml
  | "/favicon.ico", _ => return { status := 204, headers := #[], body := .empty }
  | "/api/state",    "GET" =>
    let body := (← stateJson ctx).compress
    return Response.text 200 body
  | "/api/sessions", "GET" =>
    let body := (← sessionsListJson ctx).compress
    return Response.text 200 body
  | "/api/sessions/new", "POST" => handleNewSession ctx
  | p, m =>
    match matchSessionPath p, m with
    | some (id, none), "GET"     => handleGetSession ctx id
    | some (id, none), "DELETE"  => handleDeleteSession ctx id
    | some (id, some "send"), "POST" => handleSend ctx id req
    | _, _ => return Response.notFound

/-! ## CLI -/

private structure Args where
  port       : UInt16 := 8030
  host       : String := "0.0.0.0"
  configPath : String := ""
  storeDir   : String := ""

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port"   :: v :: rest => parseArgs rest { a with port := (v.toNat?.getD 8030).toUInt16 }
  | "--host"   :: v :: rest => parseArgs rest { a with host := v }
  | "--config" :: v :: rest => parseArgs rest { a with configPath := v }
  | "--store"  :: v :: rest => parseArgs rest { a with storeDir := v }
  | _ :: rest               => parseArgs rest a
  | []                      => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  if let some d ← IO.getEnv "LLM_CHAT_STORE" then
    if a.storeDir.isEmpty then a := { a with storeDir := d }
  if a.configPath.isEmpty then
    IO.eprintln "usage: llm_chat_web --config <file.json> \
[--port N] [--host H] [--store DIR]"
    IO.Process.exit 2
  let storeDir ← if a.storeDir.isEmpty then LeanTea.Llm.ChatStore.defaultDir
                 else pure a.storeDir
  let fc ← loadConfig a.configPath
  IO.eprintln s!"llm-chat-web: spawning {fc.servers.size} MCP server(s)…"
  let orch ← fromConfig fc
  IO.eprintln s!"llm-chat-web: loaded {orch.openAiTools.size} tool(s); \
storeDir={storeDir}; serving on http://{a.host}:{a.port}/"
  let cache ← IO.mkRef (∅ : Std.HashMap String LeanTea.Llm.ChatStore.Session)
  let ctx : Ctx := { orch, storeDir, cache }
  serve a.port a.host (handler ctx)

end LlmChatWeb

def main (args : List String) : IO Unit := LlmChatWeb.serveMain args
