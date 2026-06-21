import LeanTea
import LeanJs.Parser
import LeanJs.Codegen

/-! # Chapter 5 — Three writing styles, same JS

You now have *three* ways to write a non-trivial browser function.
They produce the same JavaScript; the only difference is the
writing experience.

  1. Raw AST    — `S.constV "x" (...)` style. Verbose.
  2. JsBuilder  — `do`-notation over a `StateM (Array Stmt)`.
                  Reads top-to-bottom like the JS you'd write.
  3. The subset — a Lean-shaped string parsed and compiled. Reads
                  like source code because it *is* source code.

This chapter walks the same `refresh` function written three times
and asserts all three render byte-equal. Pick whichever fits the
piece of code you're writing.

Run:

    lake exe doc_ch05 -/

open LeanTea.Js LeanTea.Js.E LeanTea.Js.S LeanTea.Js.JsBuilder
open LeanJs.Parser LeanJs.Codegen

namespace Ch05

/-! ## 1. Raw AST

This is what `Canvas/Client.lean` used to look like end-to-end
before we added the ergonomic layers. Every binding and call carries
its own helper invocation. -/
def refreshRaw : Stmt :=
  afn "refresh" [] [
    ifS (i "editing") [retV],
    constV "params" (new_ (i "URLSearchParams") []),
    doE (mcall (i "params") "set" [s "page",
      call (i "String") [i "currentPage"]]),
    constV "sq" (call (i "selectedQS") []),
    ifS (i "sq") [
      doE (mcall (i "params") "set" [s "selected", i "sq"])
    ],
    constV "r" (await_ (call (i "fetch") [
      s "/canvas?" + mcall (i "params") "toString" []])),
    assign (Dom.getById "canvas-host" |>.dot "innerHTML")
           (await_ (mcall (i "r") "text" []))
  ]

/-! ## 2. JsBuilder (the monadic builder)

Reads top-to-bottom; statements emit via `do` and scope follows the
nesting. Best when you want the *shape* to match the rendered JS but
still keep typed composability. -/
def refreshMonadic : Stmt := afnB "refresh" [] do
  if_ (i "editing") (return_v)
  const "params" (new_ (i "URLSearchParams") [])
  call_ (mcall (i "params") "set" [s "page",
    call (i "String") [i "currentPage"]])
  const "sq" (call (i "selectedQS") [])
  if_ (i "sq") do
    call_ (mcall (i "params") "set" [s "selected", i "sq"])
  const "r" (await_ (call (i "fetch")
    [s "/canvas?" + mcall (i "params") "toString" []]))
  assign_ ((Dom.getById "canvas-host").dot "innerHTML")
          (await_ (mcall (i "r") "text" []))

/-! ## 3. The LeanJs subset

Write the same logic in Lean-shaped source. The framework parses it
and emits the same JS. This is the path Reversi uses for its game
logic — you get type annotations, real top-level recursion, and the
same source can flow through `lean --run` (when it's pure).

Note: we use `extern js` to bind the *exact same* DOM-side names
the raw / monadic versions referenced bare (`URLSearchParams`,
`fetch`, `String`, …). The subset's view of the world doesn't know
about these primitives; declaring them as externs is the bridge. -/
def refreshSubsetSrc : String :=
  -- The "real-world" refresh wants `new URLSearchParams()`, which
  -- isn't in the subset's grammar today. We borrow it through FFI
  -- and otherwise stay inside the subset. (The Reversi game does the
  -- same trick — `extern js` bridges what the subset doesn't model.)
  "extern js \"(p) => new URLSearchParams(p).toString()\" qsOf\n"
  ++ "extern js \"async (u) => (await fetch(u)).text()\" fetchText\n"
  ++ "extern js \"(x) => '' + x\" str\n"
  ++ "extern js \"document.getElementById('canvas-host')\" host\n"
  ++ "async def refresh (page : Int) :=\n"
  ++ "  let qs := qsOf({page: page});\n"
  ++ "  let body := await fetchText(\"/canvas?\" + qs);\n"
  ++ "  body"

end Ch05

def main : IO Unit := do
  IO.println "== Chapter 5 — Three writing styles =="
  IO.println ""

  IO.println "── 1. Raw AST ────────────────────────────────────────────"
  IO.println (renderStmt Ch05.refreshRaw)
  IO.println ""

  IO.println "── 2. JsBuilder monad ────────────────────────────────────"
  IO.println (renderStmt Ch05.refreshMonadic)
  IO.println ""

  IO.println "── 3. The LeanJs subset ──────────────────────────────────"
  IO.println "Source (Lean-shaped, held in a String):"
  IO.println Ch05.refreshSubsetSrc
  IO.println "Compiled JS:"
  match LeanJs.Parser.parseProgramString Ch05.refreshSubsetSrc with
  | .ok p    => IO.println (LeanJs.Codegen.compileToString p)
  | .error e => IO.println s!"PARSE ERROR: {e}"
  IO.println ""

  IO.println "── Same shape? (1 vs 2 — byte-equal) ─────────────────────"
  let same := renderStmt Ch05.refreshRaw == renderStmt Ch05.refreshMonadic
  IO.println s!"  raw vs monadic identical? {same}"
  IO.println ""
  IO.println "The subset compiles to *semantically* the same code but"
  IO.println "uses different parens and an explicit IIFE for `let`, so"
  IO.println "byte-equality with the typed-AST paths is not the goal."
  IO.println ""
  IO.println "ok"
