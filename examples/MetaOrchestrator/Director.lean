import MetaOrchestrator.Llm
import Lean.Data.Json

/-! # examples/MetaOrchestrator/Director.lean — Gemini as the meta-PM

The Director reads the most recent pane snapshot (+ project goal +
short history of past instructions) and returns one of:

  * `Continue`  — don't intervene, give the agent more time
  * `Instruct s` — send the string `s` to the agent as the next prompt
  * `AskUser s` — surface a question to the human operator

The prompt below pins the model into a strict JSON shape. We
post-process to forgive code-fence wrapping and obvious shape drift.

## Why not use the OpenAI chat client + function calling

We could, and on a bigger surface we should. For the first cut a
direct `Gemini.ask` + JSON output keeps the dependency surface flat
and the prompt iteration loop short. Switching to tool-calls is a
follow-up once we have stable decision logs to compare against. -/

namespace MetaOrchestrator.Director

open Lean (Json)
open MetaOrchestrator.Llm (Backend)

/-- One past director decision. Kept short so a long history doesn't
    blow the context window. We only show the *instruction* text and
    a 1-line summary of what changed on the pane after it. The
    `sessionId` lets multiple long-running goals share one log file
    without their memories cross-contaminating. -/
structure Memo where
  sessionId : String   -- stable per goal (auto-derived or --session)
  ts : String          -- ISO8601-ish timestamp
  action : String      -- "continue" | "instruct" | "ask_user"
  text : String        -- what we said (or "" for continue)
  afterSummary : String  -- 1-line note of what happened next, "" if unknown
  deriving Inhabited, Repr

/-- Serialise a memo to one JSON object. Used for the persistent
    memo log (`--memo-log`) so `--resume` can rebuild context. -/
def Memo.toJson (m : Memo) : Json := Json.mkObj [
  ("sessionId", Json.str m.sessionId),
  ("ts", Json.str m.ts),
  ("action", Json.str m.action),
  ("text", Json.str m.text),
  ("afterSummary", Json.str m.afterSummary)
]

private def jstrField' (j : Json) (k : String) : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD ""

/-- Inverse of `Memo.toJson`. Returns `none` on shape mismatch so
    a partially-written log line at crash time doesn't bring resume
    down. -/
def Memo.ofJson? (j : Json) : Option Memo :=
  let action := jstrField' j "action"
  if action.isEmpty then none
  else some {
    sessionId    := jstrField' j "sessionId",
    ts           := jstrField' j "ts",
    action       := action,
    text         := jstrField' j "text",
    afterSummary := jstrField' j "afterSummary"
  }

/-- The Director's verdict. The orchestrator dispatches on this. -/
inductive Decision where
  | continue
  | instruct (text : String)
  | askUser (question : String)
  deriving Inhabited, Repr

/-- Optional reasoning the model attached to the decision. Used in logs
    and shown to the user when escalating. -/
structure Verdict where
  decision : Decision
  reasoning : String
  deriving Inhabited

private def systemPrompt (goal : String) : String :=
"You are the Project Manager for a long-running coding agent (Claude Code) that is working towards this goal:

  GOAL: " ++ goal ++ "

You will be shown:
  1. The agent's most recent terminal pane (visible viewport).
  2. A short history of your past instructions and what changed.

Decide one of three actions:
  * \"continue\"  — the agent is making progress. Don't interrupt.
  * \"instruct\"  — the agent is idle, stuck, drifting, or finished a step. Give a SHORT, SPECIFIC next instruction (1-3 sentences).
  * \"ask_user\"  — you need human input (ambiguous priorities, irreversible action, blocked on credentials).

REPLY WITH STRICT JSON, no code fence, no prose:
  {\"action\":\"continue|instruct|ask_user\",\"reasoning\":\"why (40-120 chars)\",\"text\":\"the instruction or question (empty when continue)\"}

Bias towards `continue` if there's any sign of recent activity (build output, file edits, new lines). Only `instruct` when you see a clear stall (prompt waiting / idle cursor / completed marker) or a wrong direction. `ask_user` should be rare — only when proceeding would be irreversible or off-goal.
"

private def memoLine (m : Memo) : String :=
  let clean := (m.text.replace "\n" " ").trim
  s!"  [{m.ts}] {m.action}: {clean}\n      → {m.afterSummary}"

private def memosToText (memos : List Memo) : String :=
  if memos.isEmpty then "(no prior decisions)"
  else String.intercalate "\n" (memos.map memoLine)

/-- Tail of `s` clipped to roughly the last `n` chars. We slice on
    character count (not bytes) so multibyte CJK doesn't cause a
    mid-codepoint cut. The pane dump can be big; the trailing
    portion is where the useful output lives. -/
private def tailChars (n : Nat) (s : String) : String :=
  let chars := s.toList
  if chars.length ≤ n then s
  else (chars.drop (chars.length - n)).asString

/-- Trim a JSON code fence (```json … ```) if the model added one. -/
private def stripFence (s : String) : String :=
  let t := s.trim
  if t.startsWith "```" then
    let afterOpen := ((t.dropWhile (· != '\n')).drop 1).toString
    let beforeClose :=
      if afterOpen.endsWith "```" then afterOpen.dropRight 3 else afterOpen
    beforeClose.trim
  else t

private def jstrField (j : Json) (k : String) : String :=
  (j.getObjVal? k).toOption.bind (·.getStr?.toOption) |>.getD ""

/-- Ask the configured decide-backend for a verdict on the pane
    snapshot. On any decode trouble we default to `Continue` with
    the raw response in `reasoning` — the safest no-op. -/
def decide (backend : Backend) (goal : String) (memos : List Memo)
    (paneSnapshot : String) : IO Verdict := do
  let tailPane := tailChars 6000 paneSnapshot
  let user :=
    s!"## Recent pane (last ~6 kB)\n\n```\n{tailPane}\n```\n\n## My past decisions\n\n{memosToText memos}\n\n## Now decide"
  let raw' ← backend.ask (systemPrompt goal) user (temperature := 0.4) (maxTokens := 400)
  let raw := stripFence raw'
  match Json.parse raw with
  | .error _ =>
    return { decision := .continue, reasoning := s!"(json parse failed) {raw}" }
  | .ok j =>
    let action := jstrField j "action"
    let reasoning := jstrField j "reasoning"
    let text := jstrField j "text"
    let dec :=
      if action == "instruct" && !text.isEmpty then Decision.instruct text
      else if action == "ask_user" && !text.isEmpty then Decision.askUser text
      else Decision.continue
    return { decision := dec, reasoning }

/-! ## Review pass — heavy-weight audit on demand

Where `decide` is the polling loop's per-stall cheap classifier,
`review` is a slower, thorough audit run when the user explicitly
asks for it (via `/review AGENT_ID`). It reads the whole memo log
for the session, the recent pane, and asks the review backend for
a free-form audit — no JSON verdict, no branching, just prose the
operator can act on. -/

private def reviewSystemPrompt (goal : String) : String :=
"You are a senior engineer reviewing the work of a coding agent that has been running against this goal:

  GOAL: " ++ goal ++ "

You are shown:
  1. The agent's most recent terminal pane (visible viewport).
  2. The full memo log of every past decision the PM (a smaller model) made and how the pane changed after.

Write a REVIEW: what direction the agent is drifting, whether it's actually making progress on the goal, what to redirect it towards. Concise (150-400 words). Cite specific lines / memo entries when calling out concerns. Prioritise the 2-3 most important observations; don't try to cover everything."

def review (backend : Backend) (goal : String) (memos : List Memo)
    (paneSnapshot : String) : IO String := do
  let tailPane := tailChars 10000 paneSnapshot
  let memosBlock := memosToText memos
  let user := s!"## Full memo log\n\n{memosBlock}\n\n## Recent pane (last ~10 kB)\n\n```\n{tailPane}\n```"
  backend.ask (reviewSystemPrompt goal) user (temperature := 0.5) (maxTokens := 1200)

end MetaOrchestrator.Director
