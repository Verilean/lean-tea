import Lean.Data.Json

/-! # LeanTea.Agent.Memory — JSON-backed UI knowledge base + escalation log

A small budget LLM (Gemma 4 e4b at home) drives the browser, but can't
spot a 30 × 30 px button in a 1280 × 800 screenshot reliably. The fix
is hybrid: a stronger model (or a human) supplies the coordinate of a
verified button once, the cheap model just calls `ui_recall("quest_tab")`
on every later run and clicks there.

Two files under `~/.cache/leantea-agent/`:

* `ui-map.json` — flat key/value store of verified UI elements.
  Values are freeform JSON (`{x, y, notes, …}`) so callers can stash
  whatever metadata they need.

* `escalations.jsonl` — one JSON object per line. The agent writes
  here when it's stuck or sees something unexpected; a human or
  Claude reads the file later and decides what to do.

Both files are atomic at write-time (`.tmp` + rename), so a partial
crash never leaves the map half-serialised. -/

namespace LeanTea.Agent.Memory

open Lean (Json)

/-! ## Paths -/

/-- Resolve `$LEANTEA_AGENT_DIR` or default to `~/.cache/leantea-agent/`.
    Creates the directory if missing — first-run setup should be
    silent. -/
def agentDir : IO System.FilePath := do
  let p ← match ← IO.getEnv "LEANTEA_AGENT_DIR" with
    | some p => pure (System.FilePath.mk p)
    | none   =>
      let home ← IO.getEnv "HOME"
      match home with
      | some h => pure (System.FilePath.mk h / ".cache" / "leantea-agent")
      | none   => pure (System.FilePath.mk "/tmp/leantea-agent")
  IO.FS.createDirAll p
  return p

def mapPath : IO System.FilePath := do
  return (← agentDir) / "ui-map.json"

def escalationsPath : IO System.FilePath := do
  return (← agentDir) / "escalations.jsonl"

/-! ## UI map: read / write / patch -/

private def readMapRaw : IO Json := do
  let p ← mapPath
  if ← p.pathExists then
    let s ← IO.FS.readFile p
    match Json.parse s.trimAscii.toString with
    | .ok j    => return j
    | .error _ => return Json.mkObj []
  else
    return Json.mkObj []

private def writeMapRaw (j : Json) : IO Unit := do
  let p ← mapPath
  let tmp : System.FilePath := p.toString ++ ".tmp"
  IO.FS.writeFile tmp (j.pretty 2)
  IO.FS.rename tmp p

/-- Look up one key. Returns `Json.null` when the key is missing so
    the LLM can detect "not learned yet" without exception handling. -/
def recall (key : String) : IO Json := do
  let m ← readMapRaw
  return (m.getObjVal? key).toOption.getD Json.null

/-- Round-trip a JSON object to a `(String × Json)` list. Used by
    `remember` to read the current map before patching. -/
private def objToList (j : Json) : List (String × Json) :=
  match j with
  | .obj kvs => kvs.toList
  | _        => []

/-- Save / overwrite a key. Idempotent. Value is whatever the LLM
    hands us — we don't validate it so callers stay flexible. -/
def remember (key : String) (value : Json) : IO Unit := do
  let m ← readMapRaw
  let kvs := objToList m
  let kvs' := (kvs.filter (·.fst != key)) ++ [(key, value)]
  writeMapRaw (Json.mkObj kvs')

/-- List every known key — the agent can use this to discover what's
    been mapped without guessing the right key string. -/
def keys : IO (Array String) := do
  let m ← readMapRaw
  return (objToList m).toArray.map (·.fst)

/-! ## Escalation queue (jsonl) -/

/-- Append an escalation event. `reason` is the human-readable summary
    ("stuck after 5 wrong clicks", "unexpected modal"), `context` is
    any extra JSON the agent wants saved (current task, last screen
    URL, last few tool calls). -/
def escalate (reason : String) (context : Json := Json.null) : IO Unit := do
  let p ← escalationsPath
  let now ← (IO.monoMsNow : IO Nat)
  let event := Json.mkObj [
    ("ts",      Json.num (Int.ofNat now)),
    ("reason",  Json.str reason),
    ("context", context)
  ]
  /- One line per event — the file is meant to be tailed / `jq`-ed,
     not parsed as a single document. -/
  let h ← IO.FS.Handle.mk p IO.FS.Mode.append
  h.putStrLn event.compress
  h.flush

end LeanTea.Agent.Memory
