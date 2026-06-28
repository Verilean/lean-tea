import LeanTea.Llm.McpOrchestrator
import Lean.Data.Json

/-! # LeanTea.Llm.ChatStore — JSON-file-per-session chat persistence

Each chat session is one JSON file under a configurable directory
(default `~/.cache/leantea-chat/`). The file format is a small
envelope around the orchestrator's `ChatMsg` history:

```json
{
  "id": "20260628-091500-a1b2",
  "name": "open github and read title",
  "created": 1740000000,
  "updated": 1740001234,
  "messages": [ … ]
}
```

`name` is auto-generated from the first user message when it isn't
set explicitly. Images are stored inline as `data:` URLs — fine for
the scale of a personal-machine LLM-chat app; would not survive at
SaaS multi-tenant scale, but that's not what this is for. -/

namespace LeanTea.Llm.ChatStore

open Lean (Json)
open LeanTea.Llm.McpOrchestrator (ChatMsg Role)

structure Session where
  id      : String
  name    : String := ""
  /-- Unix seconds. -/
  created : Nat := 0
  updated : Nat := 0
  messages : Array ChatMsg := #[]
  deriving Inhabited

/-! ## Default directory -/

private def defaultDirRel : String := ".cache/leantea-chat"

def defaultDir : IO String := do
  let home ← match (← IO.getEnv "HOME") with
    | some h => pure h
    | none   => pure "."
  return home ++ "/" ++ defaultDirRel

private def ensureDir (dir : String) : IO Unit := do
  unless (← System.FilePath.pathExists dir) do
    IO.FS.createDirAll dir

/-! ## ID generation

`<yyyymmdd>-<hhmmss>-<rand4hex>` — sortable by date and visually distinct. -/

private def now? : IO Nat := do
  let ms ← IO.monoMsNow
  /- monoMsNow is monotonic, not wall-clock; we use it for the `created`
     / `updated` fields too — UI never shows it as a real date, only as
     a sort key. Wrapping to seconds keeps the numbers smaller. -/
  return ms / 1000

private def pad2 (n : Nat) : String :=
  let s := toString n
  if s.length < 2 then "0" ++ s else s

private def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat ('0'.toNat + n)
  else Char.ofNat ('a'.toNat + n - 10)

private partial def hex (n : Nat) : String :=
  if n == 0 then "0"
  else
    let rec go (n : Nat) (acc : String) : String :=
      if n == 0 then acc
      else go (n / 16) ((hexDigit (n % 16)).toString ++ acc)
    go n ""

/-- Sortable timestamped session ID using monoMsNow as the time
    source plus a 4-hex random tail for uniqueness across the
    millisecond. Format is `tNNNNNN-XXXX` so file listing sorts in
    creation order. -/
def freshId : IO String := do
  let ms ← IO.monoMsNow
  let r  ← IO.rand 0 0xffff
  return s!"t{ms}-{hex r}"

/-! ## Disk format -/

private def chatMsgToStoreJson (m : ChatMsg) : Json :=
  Json.mkObj [
    ("role",       Json.str m.role.toString),
    ("text",       Json.str m.text),
    ("images",     Json.arr (m.images.map Json.str)),
    ("toolCalls",  Json.arr m.toolCalls),
    ("toolCallId", Json.str m.toolCallId),
    ("toolName",   Json.str m.toolName)
  ]

private def chatMsgFromStoreJson (j : Json) : Option ChatMsg := do
  let roleS ← (j.getObjVal? "role").toOption.bind (·.getStr?.toOption)
  let role : Role := match roleS with
    | "system"    => .system
    | "user"      => .user
    | "assistant" => .assistant
    | "tool"      => .tool
    | _           => .user
  let text := (j.getObjVal? "text").toOption.bind (·.getStr?.toOption) |>.getD ""
  let imagesArr := (j.getObjVal? "images").toOption.bind (·.getArr?.toOption) |>.getD #[]
  let images := imagesArr.filterMap (·.getStr?.toOption)
  let toolCalls := (j.getObjVal? "toolCalls").toOption.bind (·.getArr?.toOption) |>.getD #[]
  let toolCallId := (j.getObjVal? "toolCallId").toOption.bind (·.getStr?.toOption) |>.getD ""
  let toolName := (j.getObjVal? "toolName").toOption.bind (·.getStr?.toOption) |>.getD ""
  return { role, text, images, toolCalls, toolCallId, toolName }

private def sessionToJson (s : Session) : Json :=
  Json.mkObj [
    ("id",       Json.str s.id),
    ("name",     Json.str s.name),
    ("created",  Json.num (Int.ofNat s.created)),
    ("updated",  Json.num (Int.ofNat s.updated)),
    ("messages", Json.arr (s.messages.map chatMsgToStoreJson))
  ]

private def sessionFromJson (j : Json) : Option Session := do
  let id   ← (j.getObjVal? "id").toOption.bind (·.getStr?.toOption)
  let name := (j.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
  let created := (j.getObjVal? "created").toOption.bind (·.getNat?.toOption) |>.getD 0
  let updated := (j.getObjVal? "updated").toOption.bind (·.getNat?.toOption) |>.getD 0
  let msgsJ := (j.getObjVal? "messages").toOption.bind (·.getArr?.toOption) |>.getD #[]
  let messages := msgsJ.filterMap chatMsgFromStoreJson
  return { id, name, created, updated, messages }

/-! ## CRUD -/

private def sessionPath (dir id : String) : String :=
  s!"{dir}/{id}.json"

/-- Auto-name from the first user message (truncated). -/
private def autoNameFromHistory (h : Array ChatMsg) (limit : Nat := 50) : String :=
  match h.find? (·.role == .user) with
  | some m =>
    let t := (m.text.trimAscii.toString).replace "\n" " "
    if t.length ≤ limit then t else (t.take limit).toString ++ "…"
  | none => "(empty)"

/-- Build a fresh empty session ready to be written. -/
def newSession (name : String := "") : IO Session := do
  let id ← freshId
  let now ← now?
  return { id, name, created := now, updated := now, messages := #[] }

/-- Persist a session to disk. If `s.name` is empty we fill it from
    the first user message. Updates `updated` timestamp. -/
def save (dir : String) (s : Session) : IO Session := do
  ensureDir dir
  let name := if s.name.isEmpty then autoNameFromHistory s.messages else s.name
  let now ← now?
  let s' := { s with name, updated := now }
  IO.FS.writeFile (sessionPath dir s.id) (sessionToJson s').pretty
  return s'

/-- Load a session by id. Returns `none` when the file doesn't exist
    or isn't valid JSON we recognise. -/
def load (dir id : String) : IO (Option Session) := do
  let path := sessionPath dir id
  unless ← System.FilePath.pathExists path do return none
  let src ← IO.FS.readFile path
  match Json.parse src with
  | .error _ => return none
  | .ok j    => return sessionFromJson j

/-- Delete a session by id. Returns `true` if a file was removed. -/
def delete (dir id : String) : IO Bool := do
  let path := sessionPath dir id
  if ← System.FilePath.pathExists path then
    IO.FS.removeFile path
    return true
  else
    return false

/-! ## Listing — load just the metadata, not the full history. -/

structure Summary where
  id      : String
  name    : String
  created : Nat
  updated : Nat
  /-- Number of messages in the session, for the listing UI. -/
  count   : Nat
  deriving Inhabited

private def summaryFromJson (j : Json) : Option Summary := do
  let id   ← (j.getObjVal? "id").toOption.bind (·.getStr?.toOption)
  let name := (j.getObjVal? "name").toOption.bind (·.getStr?.toOption) |>.getD ""
  let created := (j.getObjVal? "created").toOption.bind (·.getNat?.toOption) |>.getD 0
  let updated := (j.getObjVal? "updated").toOption.bind (·.getNat?.toOption) |>.getD 0
  let count :=
    match (j.getObjVal? "messages").toOption.bind (·.getArr?.toOption) with
    | some a => a.size
    | none   => 0
  return { id, name, created, updated, count }

/-- Return every session's summary, newest first by `updated`. -/
def list (dir : String) : IO (Array Summary) := do
  ensureDir dir
  let entries ← System.FilePath.readDir dir
  let mut summaries : Array Summary := #[]
  for e in entries do
    if e.fileName.endsWith ".json" then
      let path := e.path.toString
      try
        let src ← IO.FS.readFile path
        match Json.parse src with
        | .error _ => pure ()
        | .ok j    =>
          match summaryFromJson j with
          | some s => summaries := summaries.push s
          | none   => pure ()
      catch _ => pure ()
  /- Sort by `updated` descending. -/
  return summaries.qsort (fun a b => a.updated > b.updated)

/-- Rename a session. Returns the updated session, or `none` if not found. -/
def rename (dir id newName : String) : IO (Option Session) := do
  match ← load dir id with
  | none   => return none
  | some s =>
    let s' ← save dir { s with name := newName }
    return some s'

/-! ## Summary JSON for the Web UI -/

def summaryToJson (s : Summary) : Json :=
  Json.mkObj [
    ("id",      Json.str s.id),
    ("name",    Json.str s.name),
    ("created", Json.num (Int.ofNat s.created)),
    ("updated", Json.num (Int.ofNat s.updated)),
    ("count",   Json.num (Int.ofNat s.count))
  ]

end LeanTea.Llm.ChatStore
