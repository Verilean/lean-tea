import LeanTea

/-! # Chapter 1 — Hello, lean-elm

A Counter in 30 lines. We don't open an HTTP server in this chapter
because the point is to see the *core* — `Model`, `Msg`, `update`,
`view` — in isolation. Running this binary prints the model
transitions a real user would trigger and the corresponding HTML
fragment, so you can see exactly what the framework does on each
step.

Run:

    lake exe doc_ch01

The output proves the snippets in `docs/01-hello.md` execute. -/

open LeanTea

namespace Ch01

/-! ## The Elm-style triple

`Model`, `Msg`, and the trio `init / update / view` form the whole
contract. Everything else — RPC, persistence, HTML — slots into this
shape. We start with the smallest model that's still interesting. -/

abbrev Model := Int

inductive Msg where
  | inc
  | dec
  | reset

/-- `update` is pure. Given the current model and a message, return
    the next model. No `IO`, no side effects — easy to test. -/
def update : Msg → Model → Model
  | .inc,   m => m + 1
  | .dec,   m => m - 1
  | .reset, _ => 0

/-- `view` is also pure. Build an HTML AST node and the framework
    renders it. The `Html` AST is the same one Canvas / English
    use — there is no second view layer. -/
def view (m : Model) : Html :=
  div_ [("class", "counter")] [
    h1 [] [text s!"count: {m}"],
    button_ [("data-msg", "inc")]   [text "+"],
    button_ [("data-msg", "dec")]   [text "-"],
    button_ [("data-msg", "reset")] [text "reset"]
  ]

end Ch01

/-! ## Step the app

Once the triple exists, the runtime is a loop:

    model = init
    forever:
      render(view(model))
      msg   = wait_for_user_input()
      model = update(msg, model)

We don't run the loop here; we drive a fixed script of messages so
the output is reproducible. -/

def driveScript (script : List Ch01.Msg) (start : Ch01.Model)
    : IO Unit := do
  let mut m := start
  IO.println s!"  init: model = {m}"
  IO.println s!"        view  = {(Ch01.view m).render}"
  for msg in script do
    let label := match msg with
      | .inc => "inc" | .dec => "dec" | .reset => "reset"
    m := Ch01.update msg m
    IO.println s!"  msg {label}: model = {m}"
    IO.println s!"               view  = {(Ch01.view m).render}"

def main : IO Unit := do
  IO.println "== Chapter 1 — Hello, lean-elm =="
  IO.println ""
  IO.println "Script: inc inc inc dec reset"
  driveScript [.inc, .inc, .inc, .dec, .reset] 0
  IO.println ""
  IO.println "ok"
