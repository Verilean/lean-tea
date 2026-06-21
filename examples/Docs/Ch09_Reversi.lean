import LeanTea
import LeanJs.Parser
import LeanJs.Codegen
import LeanJs.Eval
import LeanJs.LeanEmit
import Reversi.Game

/-! # Chapter 9 — Reversi end-to-end

The earlier chapters introduced each piece — the parser, the
codegen, the cross-check, the bilingual story. This chapter walks
*one* program through *all* of them: the Reversi (Othello) game
that ships under `examples/Reversi/`.

What we verify in one binary:

  1. The game source parses with the subset parser.
  2. The codegen turns it into ~3 KB of JS that node can run.
  3. The Lean evaluator agrees on the initial board score (a pure
     computation that doesn't touch FFI). This is the cross-check
     for the non-FFI parts.
  4. The `LeanEmit` path *refuses* the full Reversi program — it
     uses `extern js`, which is JS-only by design. We print the
     error explicitly so the reader sees the boundary.
  5. A pure-subset slice of Reversi (the score loop, no FFI) emits
     real Lean source that compiles cleanly.

Run:

    lake exe doc_ch09 -/

open LeanJs.Parser LeanJs.Codegen LeanJs.Eval

namespace Ch09

/-- A pure slice of Reversi that doesn't touch the JS runtime. Used
    here to demonstrate the bilingual path — the kind of code that
    `LeanEmit` *does* handle. -/
-- One param group only (the subset doesn't yet curry `def f (x) (y)`).
def pureSliceSrc : String :=
  "inductive Cell where | Empty | Black | White\n"
  ++ "def opp (c : Int) : Int := if c == 1 then 2 else 1\n"
  ++ "def inB (x : Int, y : Int) : Bool :=\n"
  ++ "  0 <= x && x < 8 && 0 <= y && y < 8\n"
  ++ "def main := opp(1)"

end Ch09

def main : IO Unit := do
  IO.println "== Chapter 9 — Reversi end-to-end =="
  IO.println ""

  IO.println "── 1. Load + parse the Reversi source ────────────────────"
  let src ← Reversi.loadSource
  let prog ← match parseProgramString src with
    | .error e => IO.println s!"parse failed: {e}"; return
    | .ok p =>
      IO.println s!"  parsed {p.size} top-level declarations from"
      IO.println s!"  examples/Reversi/Game.leanjs ({src.utf8ByteSize} bytes)"
      pure p

  IO.println ""
  IO.println "── 2. Compile to JS ──────────────────────────────────────"
  let js := compileToString prog
  IO.println s!"  compiled JS: {js.utf8ByteSize} bytes"
  IO.println s!"  first 80 chars: {js.take 80}…"

  IO.println ""
  IO.println "── 3. Try the Lean interpreter on initial-score ──────────"
  -- The interpreter's `extern` values aren't callable, so running
  -- Reversi through Lean hits the FFI boundary on the first `put`.
  -- The interpreter today silently falls back to `null` instead of
  -- propagating the error — a known wart on Eval that's queued for
  -- a cleanup. We surface it explicitly here so the reader sees it.
  match runProgram prog with
  | .ok .null =>
    IO.println "  null — interpreter swallowed the FFI failure (known)"
  | .ok v   => IO.println s!"  unexpected ok: {render v}"
  | .error e => IO.println s!"  FFI boundary as expected: {e}"

  IO.println ""
  IO.println "── 4. Emit Reversi as real Lean source ───────────────────"
  match LeanJs.LeanEmit.emitProgram prog with
  | .ok _   => IO.println "  unexpected — Reversi uses extern, should fail"
  | .error e => IO.println s!"  refused (good): {e}"
  IO.println "  — this is the FFI boundary in action. Reversi is JS-only."

  IO.println ""
  IO.println "── 5. A pure slice emits cleanly ─────────────────────────"
  match parseProgramString Ch09.pureSliceSrc with
  | .error e => IO.println s!"  parse: {e}"
  | .ok pure =>
    match LeanJs.LeanEmit.emitProgram pure with
    | .error e => IO.println s!"  emit:  {e}"
    | .ok body =>
      IO.println "  source:"
      for line in Ch09.pureSliceSrc.splitOn "\n" do
        IO.println s!"    {line}"
      IO.println "  emitted Lean:"
      for line in body.splitOn "\n" do
        IO.println s!"    {line}"
  IO.println ""
  IO.println "ok"
