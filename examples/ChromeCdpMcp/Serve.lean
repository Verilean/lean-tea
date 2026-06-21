import LeanTea
import LeanTea.Net.WebSocket
import Lean.Data.Json

/-! # chrome_cdp_mcp_serve — MCP server driving a real Chrome via CDP

Talks to a Chrome instance launched with `--remote-debugging-port=9222
--user-data-dir=…`. Lists tabs through the `/json` REST endpoint and
opens a per-tab WebSocket for `Runtime.evaluate` / `Page.navigate`
style commands.

```
chrome_cdp_mcp_serve --port 8014    # HTTP, curl-friendly
chrome_cdp_mcp_serve                # stdio, for MCP clients
```

Tools surfaced:

* `chrome_targets` — list open tabs (id, title, url)
* `chrome_navigate(targetId, url)` — Page.navigate
* `chrome_evaluate(targetId, expression)` — Runtime.evaluate
                                            (returnByValue=true)
* `chrome_screenshot(targetId)` — Page.captureScreenshot → MCP image
* `chrome_click(targetId, selector)` — querySelector(...).click()
* `chrome_fill(targetId, selector, text)` — set input value + dispatch
                                            an `input` event

WebSocket connections are NOT pooled — each tool call dials, runs
one command, closes. Keeps the server stateless; per-tab session
state would matter only if we wanted to subscribe to events.
-/

open LeanTea LeanTea.Net.Server
open LeanTea.Net.Http (Request Response Handler)
open LeanTea.Net.WebSocket
open Lean (Json)

namespace ChromeCdpMcp

/-! ## Config — `IO.Ref` so HTTP overrides survive across requests.

`workspace` constrains every path the tool surface accepts (`outputFile`,
`attachFiles`, …) to live under one root. This is the security boundary —
without it an LLM-driven `attachFiles=['/etc/passwd']` would read & ship
arbitrary host files into a foreign page. -/

structure CdpConfigData where
  baseUrl   : String
  workspace : String

abbrev CdpConfig := IO.Ref CdpConfigData

private def defaultBaseUrl : IO String := do
  return (← IO.getEnv "CHROME_CDP_URL").getD "http://127.0.0.1:9222"

private def defaultWorkspace : IO String := do
  match (← IO.getEnv "CHROME_CDP_WORKSPACE") with
  | some w => return w
  | none   => return (← IO.currentDir).toString

/-- Normalise an absolute path: split on `/`, drop `.` segments, pop
    on `..`. Returns the normalised form. -/
private def normalisePath (raw : String) : String :=
  let parts := raw.splitOn "/"
  let walk (acc : List String) (seg : String) : List String :=
    if seg == "" || seg == "." then acc
    else if seg == ".." then
      match acc with
      | _ :: rest => rest
      | []        => []
    else seg :: acc
  let stack := parts.foldl walk []
  "/" ++ String.intercalate "/" stack.reverse

/-- Reject any path that resolves outside `workspace`. Relative paths
    are resolved against `workspace`. Symlinks aren't followed — best
    effort; if you symlink `/etc/passwd` into the workspace, that's
    on you. -/
private def validatePath (workspace path : String) : Except String String :=
  let abs := if path.startsWith "/" then path else workspace ++ "/" ++ path
  let norm := normalisePath abs
  let wsNorm := normalisePath workspace
  /- `wsNorm ++ "/"` so `/foo/bar/baz` doesn't slip past `/foo/ba`. -/
  if norm == wsNorm || norm.startsWith (wsNorm ++ "/") then .ok norm
  else .error s!"path {path} escapes workspace {wsNorm}"

/-! ## Low-level CDP wrappers -/

private def httpGetJson (url : String) : IO Json := do
  let parsed ← match LeanTea.Net.HttpClient.parseUrl url with
    | some u => pure u
    | none => throw <| IO.userError s!"cdp: bad URL: {url}"
  let resp ← LeanTea.Net.HttpClient.request "GET" parsed
  if resp.status >= 400 then
    throw <| IO.userError s!"cdp GET {url}: {resp.status}\n{resp.bodyText}"
  match Json.parse resp.bodyText with
  | .ok j    => return j
  | .error e => throw <| IO.userError s!"cdp: bad JSON from {url}: {e}"

/-- Look up the `webSocketDebuggerUrl` for a given target id by hitting
    `/json` and finding the matching record. -/
private def wsUrlOfTarget (base : String) (targetId : String) : IO String := do
  let j ← httpGetJson (base ++ "/json")
  let arr := match j.getArr? with | .ok a => a | .error _ => #[]
  let found := arr.findSome? fun t =>
    if t.getStrD "id" == targetId then t.getStrOpt "webSocketDebuggerUrl"
    else none
  match found with
  | some u => return u
  | none   => throw <| IO.userError s!"cdp: target {targetId} not found"

/-- Send one CDP command on a fresh WebSocket; wait for the matching
    response; close. Returns the `result` field of the response, or
    throws on `error`. -/
partial def cdpCommand (wsUrl : String) (method : String)
    (params : Json := Json.mkObj []) : IO Json := do
  let conn ← connect wsUrl
  let req := Json.mkObj [
    ("id",     Json.num 1),
    ("method", Json.str method),
    ("params", params)
  ]
  sendText conn req.compress
  let rec waitFor (depth : Nat) : IO Json := do
    if depth > 200 then throw <| IO.userError s!"cdp: too many events while waiting for {method}"
    let raw ← recvText conn
    match Json.parse raw with
    | .error e => throw <| IO.userError s!"cdp: bad JSON: {e}\n{raw}"
    | .ok j =>
      if j.getNatD "id" == 1 then
        match (j.getObjVal? "error").toOption with
        | some e =>
          close conn
          throw <| IO.userError s!"cdp error: {e.compress}"
        | none   =>
          close conn
          return (j.getObjVal? "result").toOption.getD (Json.mkObj [])
      else waitFor (depth + 1)
  waitFor 0

/-! ## MCP shapes — see `LeanTea.Mcp` for the shared implementation. -/

open LeanTea.Mcp (jsonOk jsonErr textContent errContent imageContent
                  argSchema toolDef defaultInitializeResult)

def toolsList : Json :=
  Json.mkObj [
    ("tools", Json.arr #[
      toolDef "chrome_targets"
        "List open Chrome tabs (id, title, url). Use the id to address subsequent commands."
        #[] #[],
      toolDef "chrome_navigate"
        "Navigate the given tab to a new URL."
        #[ argSchema "targetId" "string" "tab id from chrome_targets",
           argSchema "url"      "string" "destination URL" ]
        #["targetId", "url"],
      toolDef "chrome_evaluate"
        ("Run a JS expression in the tab's page context via "
         ++ "`Runtime.evaluate` with `returnByValue=true`. Returns the "
         ++ "stringified result. If `outputFile` (absolute path) is set, "
         ++ "writes the result there instead of returning it inline — "
         ++ "useful when the result is large (avoids the MCP result-size "
         ++ "cap and keeps the client's context clean).")
        #[ argSchema "targetId"   "string" "tab id",
           argSchema "expression" "string" "JS expression to evaluate",
           argSchema "outputFile" "string" "absolute path: write the result body here instead of returning inline" ]
        #["targetId", "expression"],
      toolDef "chrome_screenshot"
        "Capture a PNG screenshot of the tab's viewport and return it as an MCP image."
        #[ argSchema "targetId" "string" "tab id" ]
        #["targetId"],
      toolDef "chrome_click"
        "Click the first element matching the CSS selector via document.querySelector(...).click()."
        #[ argSchema "targetId" "string" "tab id",
           argSchema "selector" "string" "CSS selector" ]
        #["targetId", "selector"],
      toolDef "chrome_fill"
        ("Fill an input/textarea OR a contenteditable element (e.g. "
         ++ "Quill/ProseMirror rich editors). For `<input>`/`<textarea>` "
         ++ "writes via the prototype setter + `input` event so framework "
         ++ "code (React, Vue, …) registers the change. For contenteditable "
         ++ "uses `execCommand('insertText', …)`. If `attachFiles` is set, "
         ++ "each file is read **server-side** and appended under `text` as "
         ++ "a fenced code block (` ```ext … ``` `) — useful for sending "
         ++ "source files for AI review without spending the MCP client's "
         ++ "context budget on reading them first.")
        #[ argSchema "targetId" "string" "tab id",
           argSchema "selector" "string" "CSS selector for the input/editor",
           argSchema "text"     "string" "text to enter (file contents appended below if attachFiles set)",
           ("attachFiles", Json.mkObj [
             ("type", Json.str "array"),
             ("items", Json.mkObj [("type", Json.str "string")]),
             ("description", Json.str "absolute paths of files to read & append as code blocks under `text`")
           ]) ]
        #["targetId", "selector", "text"],
      toolDef "chrome_wait_for_selector"
        ("Block (server-side) until `selector` resolves on the page, or "
         ++ "`timeoutMs` elapses. Uses `MutationObserver` so it returns "
         ++ "the instant the element appears rather than polling at fixed "
         ++ "intervals. Returns `{found:bool, ms:elapsed}`.")
        #[ argSchema "targetId"  "string"  "tab id",
           argSchema "selector"  "string"  "CSS selector to wait for",
           argSchema "timeoutMs" "integer" "max wait in ms (default 10000)",
           argSchema "visible"   "boolean" "require the element to have non-zero bbox + offsetParent (default true)" ]
        #["targetId", "selector"],
      toolDef "chrome_find_tab"
        ("Find the first open tab matching either a URL substring or a "
         ++ "title substring (or both). Useful after a click opens a new "
         ++ "tab — don't poll `chrome_targets` in a loop.")
        #[ argSchema "urlPattern"   "string" "substring to match against tab URL",
           argSchema "titlePattern" "string" "substring to match against tab title" ]
        #[],
      toolDef "chrome_get_html"
        ("Return the page's HTML, with `script`/`style`/`svg`/`link`/"
         ++ "`meta` removed and most attributes stripped (keeps "
         ++ "id/class/href/src/alt/title/role/name/value/type/data-*/"
         ++ "aria-*). If `selector` is set the dump is restricted to "
         ++ "that subtree. Pair with `outputFile` for big pages.")
        #[ argSchema "targetId"   "string" "tab id",
           argSchema "selector"   "string" "optional CSS selector to limit scope",
           argSchema "outputFile" "string" "absolute path to write HTML to" ]
        #["targetId"],
      toolDef "chrome_scroll_collect"
        ("Auto-scroll `containerSelector` (down/up) and accumulate the "
         ++ "trimmed `textContent` of every element matching `itemSelector` "
         ++ "into a deduped set. Stops when scrollHeight stops growing for "
         ++ "`stableTicks` consecutive ticks or `maxScrolls` reached. If "
         ++ "`outputFile` is set the items are written one-per-line and the "
         ++ "response is just a count; otherwise the JSON array is returned "
         ++ "inline (risk: large results may blow the result-size cap).")
        #[ argSchema "targetId"          "string"  "tab id",
           argSchema "containerSelector" "string"  "CSS selector of the scrollable element",
           argSchema "itemSelector"      "string"  "CSS selector of items whose textContent to collect",
           argSchema "direction"         "string"  "'down' (default) or 'up' — scroll to bottom or top",
           argSchema "maxScrolls"        "integer" "max scroll iterations (default 200)",
           argSchema "stableTicks"       "integer" "consecutive ticks with no scrollHeight growth to stop (default 4)",
           argSchema "intervalMs"        "integer" "wait between scrolls (default 400ms)",
           argSchema "outputFile"        "string"  "absolute path to write one item per line" ]
        #["targetId", "containerSelector", "itemSelector"]
    ])
  ]

def initializeResult : Json := defaultInitializeResult "lean-elm-chrome-cdp-mcp"

/-! ## Tool implementations -/

private def getStr (args : Json) (k : String) : Except String String :=
  match args.getObjVal? k with
  | .ok v => v.getStr?
  | .error e => .error e

private def chromeTargets (base : String) : IO Json := do
  let j ← httpGetJson (base ++ "/json")
  let arr := match j.getArr? with | .ok a => a | .error _ => #[]
  let rows : Array Json := arr.map fun t =>
    Json.mkObj [
      ("id",    Json.str (t.getStrD "id")),
      ("type",  Json.str (t.getStrD "type")),
      ("title", Json.str (t.getStrD "title")),
      ("url",   Json.str (t.getStrD "url"))
    ]
  return textContent (Json.arr rows).compress

private def chromeNavigate (base targetId url : String) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let _ ← cdpCommand ws "Page.navigate"
    (Json.mkObj [("url", Json.str url)])
  return textContent s!"navigated to {url}"

private def chromeEvaluate (base targetId expression : String)
    (outputFile : Option String := none) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let res ← cdpCommand ws "Runtime.evaluate"
    (Json.mkObj [
      ("expression",     Json.str expression),
      ("returnByValue",  Json.bool true),
      ("awaitPromise",   Json.bool true)
    ])
  let inner := res.getJsonD "result" (Json.mkObj [])
  /- Surface either the value (returnByValue case) or the description
     string (objects without a JSON-serialisable representation). -/
  let payload :=
    match (inner.getObjVal? "value").toOption with
    | some v =>
      /- If the JS returned a plain string we write the raw text (no
         surrounding JSON quotes) — that's the common "dump to file"
         path. For anything else (objects, arrays, numbers) we keep the
         compact JSON serialisation. -/
      v.getStr?.toOption.getD v.compress
    | none   =>
      (inner.getStrOpt "description").getD inner.compress
  match outputFile with
  | some path =>
    IO.FS.writeFile path payload
    return textContent s!"wrote {payload.length} chars to {path}"
  | none =>
    return textContent payload

private def chromeScreenshot (base targetId : String) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let res ← cdpCommand ws "Page.captureScreenshot" (Json.mkObj [])
  let b64 := res.getStrD "data"
  return imageContent "image/png" b64 s!"screenshot of {targetId}"

private def jsString (s : String) : String :=
  /- JS-string escape suitable for embedding arbitrary text (including
     source files) as a `"..."` literal in a JS expression. Order
     matters: backslash first, then quotes and control characters.
     Without `\n`/`\r` escaping a multi-line payload becomes a syntax
     error at the literal newline. -/
  let s := s.replace "\\" "\\\\"
  let s := s.replace "\"" "\\\""
  let s := s.replace "\n" "\\n"
  let s := s.replace "\r" "\\r"
  let s := s.replace "\t" "\\t"
  "\"" ++ s ++ "\""

private def chromeClick (base targetId selector : String) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let expr :=
    "(() => { const e = document.querySelector(" ++ jsString selector ++
    "); if (!e) return false; e.click(); return true; })()"
  let res ← cdpCommand ws "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    Json.str expr),
      ("returnByValue", Json.bool true)
    ])
  let inner := res.getJsonD "result" (Json.mkObj [])
  let ok := inner.getBoolD "value"
  return textContent (if ok then s!"clicked {selector}" else s!"no element for {selector}")

/-- Scroll a container repeatedly to flush a virtualised list into the
    DOM, then return / dump every unique `textContent`. Designed for
    Gemini's chat history, Twitter feeds, Slack channels — anywhere
    that lazy-loads on scroll. -/
private def chromeScrollCollect (base targetId containerSel itemSel direction : String)
    (maxScrolls stableTicks intervalMs : Nat)
    (outputFile : Option String) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let scrollTo := if direction == "up" then "0" else "s.scrollHeight"
  let expr :=
    "(async () => {\n" ++
    "  const s = document.querySelector(" ++ jsString containerSel ++ ");\n" ++
    "  if (!s) return JSON.stringify({err: 'no container'});\n" ++
    "  const itemSel = " ++ jsString itemSel ++ ";\n" ++
    "  const seen = new Set();\n" ++
    "  const collapse = t => t.replace(/\\s+/g, ' ').trim();\n" ++
    "  let stable = 0, lastH = 0;\n" ++
    "  for (let i = 0; i < " ++ toString maxScrolls ++ "; i++) {\n" ++
    "    document.querySelectorAll(itemSel).forEach(e => {\n" ++
    "      const t = collapse(e.textContent || '');\n" ++
    "      if (t) seen.add(t);\n" ++
    "    });\n" ++
    "    s.scrollTop = " ++ scrollTo ++ ";\n" ++
    "    await new Promise(r => setTimeout(r, " ++ toString intervalMs ++ "));\n" ++
    "    if (s.scrollHeight === lastH) {\n" ++
    "      stable++;\n" ++
    "      if (stable >= " ++ toString stableTicks ++ ") break;\n" ++
    "    } else { stable = 0; lastH = s.scrollHeight; }\n" ++
    "  }\n" ++
    "  document.querySelectorAll(itemSel).forEach(e => {\n" ++
    "    const t = collapse(e.textContent || '');\n" ++
    "    if (t) seen.add(t);\n" ++
    "  });\n" ++
    "  return JSON.stringify([...seen]);\n" ++
    "})()"
  let res ← cdpCommand ws "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    Json.str expr),
      ("returnByValue", Json.bool true),
      ("awaitPromise",  Json.bool true)
    ])
  let inner := res.getJsonD "result" (Json.mkObj [])
  let raw := inner.getStrD "value" "[]"
  /- The JS returned `JSON.stringify(array)`; parse it back into an
     array of strings. -/
  let items : Array String :=
    match Json.parse raw with
    | .ok j =>
      match j.getArr? with
      | .ok arr => arr.filterMap (·.getStr?.toOption)
      | .error _ => #[]
    | .error _ => #[]
  match outputFile with
  | some path =>
    let body := String.intercalate "\n" items.toList ++ "\n"
    IO.FS.writeFile path body
    return textContent s!"wrote {items.size} items to {path}"
  | none =>
    return textContent raw

/-- Pick a markdown code-fence language tag for a file path. Falls back
    to "" (no language) for unknown extensions. -/
private def langForExt (path : String) : String :=
  let lc := path.toLower
  if      lc.endsWith ".lean" then "lean"
  else if lc.endsWith ".py"   then "python"
  else if lc.endsWith ".ts"   then "ts"
  else if lc.endsWith ".tsx"  then "tsx"
  else if lc.endsWith ".js"   then "js"
  else if lc.endsWith ".jsx"  then "jsx"
  else if lc.endsWith ".md"   then "markdown"
  else if lc.endsWith ".html" then "html"
  else if lc.endsWith ".css"  then "css"
  else if lc.endsWith ".json" then "json"
  else if lc.endsWith ".toml" then "toml"
  else if lc.endsWith ".yaml" || lc.endsWith ".yml" then "yaml"
  else if lc.endsWith ".sh"   then "bash"
  else if lc.endsWith ".rs"   then "rust"
  else if lc.endsWith ".go"   then "go"
  else if lc.endsWith ".c" || lc.endsWith ".h" then "c"
  else if lc.endsWith ".cpp" || lc.endsWith ".cc" || lc.endsWith ".hpp" then "cpp"
  else ""

/-- Read each file and append it under the prompt as a fenced code
    block. Server-side aggregation keeps the file contents off the
    MCP client's context budget. -/
private def assembleWithFiles (text : String) (paths : Array String) : IO String := do
  let mut acc := text
  for path in paths do
    let body ← IO.FS.readFile path
    let lang := langForExt path
    acc := acc ++ s!"\n\n## File: {path}\n\n```{lang}\n{body}\n```\n"
  return acc

private def chromeFill (base targetId selector text : String)
    (attachFiles : Array String := #[]) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let fullText ← assembleWithFiles text attachFiles
  /- Two paths:
     * <input>/<textarea>: write `.value` via the prototype setter so
       React/Vue's value tracker registers the change.
     * contenteditable (rich-textarea / Quill / ProseMirror): the value
       setter doesn't apply — use `execCommand('insertText', …)` after
       clearing the current children. -/
  let expr :=
    "(() => { const e = document.querySelector(" ++ jsString selector ++
    "); if (!e) return false; e.focus(); " ++
    "const isCE = e.isContentEditable || e.getAttribute('contenteditable') === 'true'; " ++
    "const text = " ++ jsString fullText ++ "; " ++
    "if (isCE) { " ++
    /- Don't blow away the children manually — that wipes the caret/
       selection and the next `insertText` silently no-ops. Instead
       `selectAll` highlights the current contents (incl. the `<p><br>`
       placeholder); the first chunk's `insertText` then replaces the
       selection, and subsequent chunks append at the caret. -/
    "  document.execCommand('selectAll', false); " ++
    /- `execCommand('insertText', …)` silently fails on payloads
       larger than ~50 KB in Chrome. Slice into chunks so multi-file
       attachments still go through. -/
    "  const CHUNK = 40000; let inserted = 0; " ++
    "  for (let i = 0; i < text.length; i += CHUNK) { " ++
    "    const ok = document.execCommand('insertText', false, text.slice(i, i + CHUNK)); " ++
    "    if (!ok) return JSON.stringify({err:'insertText chunk failed at ' + i, inserted}); " ++
    "    inserted = i + CHUNK; " ++
    "  } " ++
    "  e.dispatchEvent(new Event('input', { bubbles: true })); " ++
    "  return true; " ++
    "} else { " ++
    "  const setter = Object.getOwnPropertyDescriptor(" ++
    "Object.getPrototypeOf(e), 'value').set; setter.call(e, text); " ++
    "  e.dispatchEvent(new Event('input', { bubbles: true })); " ++
    "  e.dispatchEvent(new Event('change', { bubbles: true })); " ++
    "  return true; " ++
    "} })()"
  let res ← cdpCommand ws "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    Json.str expr),
      ("returnByValue", Json.bool true)
    ])
  let inner := res.getJsonD "result" (Json.mkObj [])
  let ok := inner.getBoolD "value"
  return textContent (if ok then s!"filled {selector} with {fullText.length} chars (attached {attachFiles.size} files)" else s!"no element for {selector}")

/-- Poll for a selector via `MutationObserver` + `requestAnimationFrame`
    server-side. Saves the LLM from re-trying `chrome_evaluate` in a
    loop. Returns `{found:true,ms:…}` or `{found:false,ms:timeout}`. -/
private def chromeWaitForSelector (base targetId selector : String)
    (timeoutMs : Nat) (requireVisible : Bool) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let vis := if requireVisible then "true" else "false"
  let expr :=
    "(async () => {\n" ++
    "  const sel = " ++ jsString selector ++ ";\n" ++
    "  const timeout = " ++ toString timeoutMs ++ ";\n" ++
    "  const requireVisible = " ++ vis ++ ";\n" ++
    "  const start = Date.now();\n" ++
    "  const check = () => {\n" ++
    "    const el = document.querySelector(sel);\n" ++
    "    if (!el) return null;\n" ++
    "    if (!requireVisible) return el;\n" ++
    "    const r = el.getBoundingClientRect();\n" ++
    "    if (r.width > 0 && r.height > 0 && el.offsetParent !== null) return el;\n" ++
    "    return null;\n" ++
    "  };\n" ++
    "  if (check()) return JSON.stringify({found:true, ms: Date.now()-start});\n" ++
    "  return new Promise(resolve => {\n" ++
    "    const obs = new MutationObserver(() => {\n" ++
    "      if (check()) { obs.disconnect(); clearTimeout(t); resolve(JSON.stringify({found:true, ms: Date.now()-start})); }\n" ++
    "    });\n" ++
    "    obs.observe(document.body, {childList:true, subtree:true, attributes:true});\n" ++
    "    const t = setTimeout(() => { obs.disconnect(); resolve(JSON.stringify({found:false, ms: Date.now()-start})); }, timeout);\n" ++
    "  });\n" ++
    "})()"
  let res ← cdpCommand ws "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    Json.str expr),
      ("returnByValue", Json.bool true),
      ("awaitPromise",  Json.bool true)
    ])
  let inner := res.getJsonD "result" (Json.mkObj [])
  let raw := inner.getStrD "value" "{}"
  return textContent raw

/-- Find the first tab whose URL contains `urlPattern` (substring) or
    title contains `titlePattern`. Returns the matching target as the
    same shape `chrome_targets` uses. -/
private def chromeFindTab (base : String)
    (urlPattern titlePattern : Option String) : IO Json := do
  let j ← httpGetJson (base ++ "/json")
  let arr := match j.getArr? with | .ok a => a | .error _ => #[]
  let containsStr (hay : String) (needle : String) : Bool :=
    (hay.splitOn needle).length > 1
  let isHit (t : Json) : Bool :=
    let uOk := (urlPattern.map (containsStr (t.getStrD "url"))).getD true
    let tOk := (titlePattern.map (containsStr (t.getStrD "title"))).getD true
    uOk && tOk
  match arr.find? isHit with
  | some t =>
    let row := Json.mkObj [
      ("id",    Json.str (t.getStrD "id")),
      ("type",  Json.str (t.getStrD "type")),
      ("title", Json.str (t.getStrD "title")),
      ("url",   Json.str (t.getStrD "url"))
    ]
    return textContent row.compress
  | none => return errContent "no matching tab"

/-- Return the page's HTML with noise (script/style/svg/link/meta) stripped.
    If `selector` is set, restrict to that subtree. Long pages should use
    `outputFile` to avoid blowing the MCP cap. -/
private def chromeGetHtml (base targetId : String)
    (selector outputFile : Option String) : IO Json := do
  let ws ← wsUrlOfTarget base targetId
  let selJs := match selector with
               | some s => jsString s
               | none   => "null"
  let expr :=
    "(() => {\n" ++
    "  const sel = " ++ selJs ++ ";\n" ++
    "  const root = sel ? document.querySelector(sel) : document.body;\n" ++
    "  if (!root) return '';\n" ++
    "  const clone = root.cloneNode(true);\n" ++
    "  for (const tag of ['script','style','svg','link','meta','noscript']) {\n" ++
    "    clone.querySelectorAll(tag).forEach(e => e.remove());\n" ++
    "  }\n" ++
    "  clone.querySelectorAll('*').forEach(e => {\n" ++
    "    for (const a of [...e.attributes]) {\n" ++
    "      if (a.name.startsWith('data-') || a.name.startsWith('aria-') || ['id','class','href','src','alt','title','role','name','value','type'].includes(a.name)) continue;\n" ++
    "      e.removeAttribute(a.name);\n" ++
    "    }\n" ++
    "  });\n" ++
    "  return clone.outerHTML;\n" ++
    "})()"
  let res ← cdpCommand ws "Runtime.evaluate"
    (Json.mkObj [
      ("expression",    Json.str expr),
      ("returnByValue", Json.bool true)
    ])
  let inner := res.getJsonD "result" (Json.mkObj [])
  let html := inner.getStrD "value"
  match outputFile with
  | some path =>
    IO.FS.writeFile path html
    return textContent s!"wrote {html.length} chars to {path}"
  | none =>
    return textContent html

/-- Validate an `outputFile` path against the workspace. Returns the
    normalised absolute path on success, or an error `Json` to surface
    to the caller. -/
private def checkPath (ws : String) (path : String) : Except Json String :=
  match validatePath ws path with
  | .ok p    => .ok p
  | .error e => .error (errContent s!"path rejected: {e}")

/-- Same, but for an array of paths (used by `attachFiles`). -/
private def checkPaths (ws : String) (paths : Array String) : Except Json (Array String) := do
  paths.mapM (checkPath ws)

def callTool (cfg : CdpConfig) (name : String) (args : Json) : IO Json := do
  let cd ← cfg.get
  let base := cd.baseUrl
  let ws := cd.workspace
  try
    match name with
    | "chrome_targets" => chromeTargets base
    | "chrome_navigate" =>
      match getStr args "targetId", getStr args "url" with
      | .ok tid, .ok url => chromeNavigate base tid url
      | _, _ => return errContent "chrome_navigate: missing targetId/url"
    | "chrome_evaluate" =>
      match getStr args "targetId", getStr args "expression" with
      | .ok tid, .ok ex =>
        let outFile := (getStr args "outputFile").toOption
        let outFile' ← match outFile with
          | some p => match checkPath ws p with
                      | .ok v => pure (some v)
                      | .error j => return j
          | none => pure none
        chromeEvaluate base tid ex outFile'
      | _, _ => return errContent "chrome_evaluate: missing targetId/expression"
    | "chrome_scroll_collect" =>
      match getStr args "targetId", getStr args "containerSelector", getStr args "itemSelector" with
      | .ok tid, .ok contSel, .ok itemSel =>
        let dir := (getStr args "direction").toOption.getD "down"
        let maxS := args.getNatD "maxScrolls"  200
        let stab := args.getNatD "stableTicks" 4
        let ivl  := args.getNatD "intervalMs"  400
        let outF := (getStr args "outputFile").toOption
        let outF' ← match outF with
          | some p => match checkPath ws p with
                      | .ok v => pure (some v)
                      | .error j => return j
          | none => pure none
        chromeScrollCollect base tid contSel itemSel dir maxS stab ivl outF'
      | _, _, _ => return errContent "chrome_scroll_collect: missing targetId/containerSelector/itemSelector"
    | "chrome_screenshot" =>
      match getStr args "targetId" with
      | .ok tid => chromeScreenshot base tid
      | _ => return errContent "chrome_screenshot: missing targetId"
    | "chrome_click" =>
      match getStr args "targetId", getStr args "selector" with
      | .ok tid, .ok sel => chromeClick base tid sel
      | _, _ => return errContent "chrome_click: missing targetId/selector"
    | "chrome_fill" =>
      match getStr args "targetId", getStr args "selector", getStr args "text" with
      | .ok tid, .ok sel, .ok txt =>
        let files : Array String :=
          (args.getArrD "attachFiles").filterMap (·.getStr?.toOption)
        let files' ← match checkPaths ws files with
                     | .ok v => pure v
                     | .error j => return j
        chromeFill base tid sel txt files'
      | _, _, _ => return errContent "chrome_fill: missing targetId/selector/text"
    | "chrome_wait_for_selector" =>
      match getStr args "targetId", getStr args "selector" with
      | .ok tid, .ok sel =>
        let timeoutMs := args.getNatD "timeoutMs" 10000
        let requireVisible := args.getBoolD "visible" true
        chromeWaitForSelector base tid sel timeoutMs requireVisible
      | _, _ => return errContent "chrome_wait_for_selector: missing targetId/selector"
    | "chrome_find_tab" =>
      let urlPat := (getStr args "urlPattern").toOption
      let titlePat := (getStr args "titlePattern").toOption
      chromeFindTab base urlPat titlePat
    | "chrome_get_html" =>
      match getStr args "targetId" with
      | .ok tid =>
        let selector := (getStr args "selector").toOption
        let outFile := (getStr args "outputFile").toOption
        let outFile' ← match outFile with
          | some p => match checkPath ws p with
                      | .ok v => pure (some v)
                      | .error j => return j
          | none => pure none
        chromeGetHtml base tid selector outFile'
      | _ => return errContent "chrome_get_html: missing targetId"
    | _ => return errContent s!"unknown tool: {name}"
  catch e => return errContent s!"{e}"

/-! ## CLI -/

private structure Args where
  mode      : String := "stdio"
  port      : UInt16 := 8014
  host      : String := "0.0.0.0"
  cdp       : Option String := none
  workspace : Option String := none

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--stdio" :: rest    => parseArgs rest { a with mode := "stdio" }
  | "--http"  :: rest    => parseArgs rest { a with mode := "http" }
  | "--port" :: v :: rest => parseArgs rest { a with mode := "http",
                                                     port := (v.toNat?.getD 8014).toUInt16 }
  | "--host" :: v :: rest => parseArgs rest { a with host := v }
  | "--cdp"  :: v :: rest => parseArgs rest { a with cdp := some v }
  | "--workspace" :: v :: rest => parseArgs rest { a with workspace := some v }
  | _ :: rest             => parseArgs rest a
  | []                    => a

def serveMain (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if let some p ← IO.getEnv "PORT" then
    if let some n := p.toNat? then a := { a with mode := "http", port := n.toUInt16 }
  let base ← match a.cdp with
    | some u => pure u
    | none   => defaultBaseUrl
  let ws ← match a.workspace with
    | some w => pure w
    | none   => defaultWorkspace
  let cfg ← IO.mkRef ({ baseUrl := base, workspace := ws } : CdpConfigData)
  let mcpHandler : LeanTea.Mcp.Handler := {
    initializeResult := initializeResult,
    toolsList        := toolsList,
    callTool         := callTool cfg
  }
  match a.mode with
  | "stdio" =>
    /- stdio MCP transport — silence on stdout aside from JSON-RPC
       frames; logging goes to stderr. -/
    IO.eprintln s!"chrome-cdp-mcp: stdio mode, CDP={base}, workspace={ws}"
    mcpHandler.serveStdio
  | _ =>
    IO.eprintln s!"chrome-cdp-mcp: http://{a.host}:{a.port}/mcp  CDP={base}, workspace={ws}"
    mcpHandler.serveHttp a.port a.host

end ChromeCdpMcp

def main (args : List String) : IO Unit := ChromeCdpMcp.serveMain args
