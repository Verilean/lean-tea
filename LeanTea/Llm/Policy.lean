import Lean.Data.Json

/-! # LeanTea.Llm.Policy — interactive allow/deny rules for MCP tool calls

A simple persistent policy engine sitting between the orchestrator
and the MCP layer. Each rule is `{pattern: glob, action: allow|deny}`
matched against the prefixed tool name (`<server>__<tool>`). The
first matching rule wins; if no rule matches the call is `Decision.ask`
and the orchestrator asks the user via a callback.

The user's answer can be one-shot or persistent. When persistent,
we append a new rule and rewrite the policy file so the next call
to the same tool resolves without a prompt.

Glob support is intentionally tiny: `*` matches any run of
characters (no `?`, no character classes, no recursive `**`).
That covers the common cases (`chrome__*`, `*__shell_run`,
`gemini__gemini_list_*`).

Storage: one JSON file (`<storeDir>/policy.json`) shaped as:

```json
{
  "rules": [
    {"pattern": "gemini__gemini_list_*", "action": "allow"},
    {"pattern": "chrome__*",             "action": "allow"},
    {"pattern": "*__shell_*",            "action": "deny"}
  ]
}
``` -/

namespace LeanTea.Llm.Policy

open Lean (Json)

/-! ## Types -/

inductive Action where
  | allow
  | deny
  deriving DecidableEq, Repr, Inhabited

def Action.toString : Action → String
  | .allow => "allow"
  | .deny  => "deny"

instance : ToString Action := ⟨Action.toString⟩

def Action.ofString? : String → Option Action
  | "allow" => some .allow
  | "deny"  => some .deny
  | _       => none

structure Rule where
  pattern : String
  action  : Action
  deriving Repr, Inhabited

/-- Outcome of `check` on a tool name. `ask` means no rule fired —
    the orchestrator should prompt the user. -/
inductive Decision where
  | allow
  | deny
  | ask
  deriving DecidableEq, Repr, Inhabited

/-! ## Glob matching

`*` matches any (possibly empty) run of characters. Implemented
greedy with simple backtracking — fine for the dozens of rules a
human would ever write. -/

private partial def globMatch (pat str : String) : Bool :=
  go pat.toList str.toList
where
  go : List Char → List Char → Bool
    | [],         []        => true
    | [],         _         => false
    | '*' :: ps,  []        => go ps []
    | '*' :: ps,  s@(_::ss) => go ps s || go ('*' :: ps) ss
    | p :: ps,    c :: cs   => p == c && go ps cs
    | _ :: _,     []        => false

/-- Apply the rules in order; first match wins. -/
def check (rules : List Rule) (toolName : String) : Decision :=
  let rec go : List Rule → Decision
    | [] => .ask
    | r :: rest =>
      if globMatch r.pattern toolName then
        match r.action with
        | .allow => .allow
        | .deny  => .deny
      else go rest
  go rules

/-! ## JSON shape -/

def Rule.toJson (r : Rule) : Json :=
  Json.mkObj [
    ("pattern", Json.str r.pattern),
    ("action",  Json.str r.action.toString)
  ]

def Rule.fromJson? (j : Json) : Option Rule := do
  let pat ← (j.getObjVal? "pattern").toOption.bind (·.getStr?.toOption)
  let act ← (j.getObjVal? "action").toOption.bind (·.getStr?.toOption)
  let action ← Action.ofString? act
  return { pattern := pat, action }

def rulesToJson (rs : List Rule) : Json :=
  Json.mkObj [("rules", Json.arr (rs.toArray.map Rule.toJson))]

def rulesFromJson (j : Json) : List Rule :=
  let arr := (j.getObjVal? "rules").toOption.bind (·.getArr?.toOption) |>.getD #[]
  arr.toList.filterMap Rule.fromJson?

/-! ## Disk I/O -/

private def filePath (storeDir : String) : String := s!"{storeDir}/policy.json"

private def ensureDir (dir : String) : IO Unit := do
  unless (← System.FilePath.pathExists dir) do
    IO.FS.createDirAll dir

/-- Load rules from the policy file. Returns `[]` when the file
    doesn't exist (fresh install) or is unreadable. -/
def load (storeDir : String) : IO (List Rule) := do
  let path := filePath storeDir
  unless ← System.FilePath.pathExists path do return []
  try
    let src ← IO.FS.readFile path
    match Json.parse src with
    | .error _ => return []
    | .ok j    => return rulesFromJson j
  catch _ => return []

/-- Persist a rule list. Overwrites the file. -/
def save (storeDir : String) (rules : List Rule) : IO Unit := do
  ensureDir storeDir
  IO.FS.writeFile (filePath storeDir) (rulesToJson rules).pretty

/-- Append a rule, skipping duplicates of `(pattern, action)`.
    Returns the new rule list. -/
def append (storeDir : String) (r : Rule) : IO (List Rule) := do
  let rs ← load storeDir
  let dup := rs.any fun x => x.pattern == r.pattern && x.action == r.action
  let rs' := if dup then rs else rs ++ [r]
  save storeDir rs'
  return rs'

/-- Drop the rule at the given index. No-op when out of range.
    Returns the new rule list. -/
def deleteAt (storeDir : String) (idx : Nat) : IO (List Rule) := do
  let rs ← load storeDir
  let rs' := rs.toArray.zipIdx
              |>.filterMap (fun (r, i) => if i == idx then none else some r)
              |>.toList
  save storeDir rs'
  return rs'

/-! ## Live policy ref

Hot-reloadable in-memory cache for the orchestrator. UIs and the
orchestrator share the same `IO.Ref` so editing rules from the UI
takes effect on the next tool call without a restart. -/

structure LiveRef where
  ref      : IO.Ref (List Rule)
  storeDir : String

def LiveRef.fromDisk (storeDir : String) : IO LiveRef := do
  let rs ← load storeDir
  let ref ← IO.mkRef rs
  return { ref, storeDir }

def LiveRef.get (lr : LiveRef) : IO (List Rule) := lr.ref.get

def LiveRef.check (lr : LiveRef) (toolName : String) : IO Decision := do
  let rs ← lr.ref.get
  return Policy.check rs toolName

/-- Add a rule and persist. Idempotent on duplicates. -/
def LiveRef.append (lr : LiveRef) (r : Rule) : IO Unit := do
  let rs ← Policy.append lr.storeDir r
  lr.ref.set rs

def LiveRef.deleteAt (lr : LiveRef) (idx : Nat) : IO Unit := do
  let rs ← Policy.deleteAt lr.storeDir idx
  lr.ref.set rs

end LeanTea.Llm.Policy
