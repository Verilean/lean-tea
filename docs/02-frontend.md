# 2 · Frontend — Elm-style in Lean

The frontend in LeanTEA is **a pure function**:

```lean
Model        → all state shown on the screen
Msg          → every event the user (or the server) can fire
update       → Msg → Model → Model
view         → Model → Html
```

The browser's job is reduced to *"submit a `Msg`, get the new HTML back."*
There is no virtual DOM, no useState, no component lifecycle. There is
a `Model`, an `update`, and a `view`.

## Smallest example — the whole counter in 50 lines

From `examples/CounterWeb/Main.lean` — this *is* the entire app
file. No support modules, no build hooks.

```lean
import LeanTea
open LeanTea

structure Model where count : Int
inductive Msg where | inc | dec | reset

def view (m : Model) : Html :=
  div_ [] [
    h1 [] [text "LeanTea Web Counter"],
    div_ [("class","card")] [
      p [] [text s!"count = {m.count}"],
      div_ [("class","row")] [
        a_ [("class","l primary"),("href","#"),("data-msg","inc")] [text "＋ inc"],
        a_ [("class","l"),("href","#"),("data-msg","dec")] [text "− dec"],
        a_ [("class","l ghost"),("href","#"),("data-msg","reset")] [text "↺ reset"]
      ]
    ]
  ]

def encodeModel (m : Model) : String := toString m.count

def decodeModel (s : String) : Option Model :=
  s.toInt?.map ({ count := · })

def decodeMsg : String → Option Msg
  | "inc"   => some .inc
  | "dec"   => some .dec
  | "reset" => some .reset
  | _       => none

def update : Msg → Model → Model
  | .inc,   m => { m with count := m.count + 1 }
  | .dec,   m => { m with count := m.count - 1 }
  | .reset, _ => { count := 0 }

def app : WebApp Model Msg :=
  { init := { count := 0 }
    title := "LeanTea Counter"
    update, view, encodeModel, decodeModel, decodeMsg }

def main (args : List String) : IO Unit := WebApp.run app args
```

`WebApp.run` wires everything: it serves the initial HTML at `/`,
accepts `Msg` posts at `/msg`, re-renders, and ships the new model
back encoded in the `X-Model` header. The browser-side runtime
(under 1 KB of vanilla JS in `LeanTea/assets/runtime.js`) does the
DOM swap. **No JS framework is loaded.**

### Skip the codec boilerplate — `deriveJson`

The three text codec fields (`encodeModel`, `decodeModel`, `decodeMsg`)
are the busiest part of the example above. If your `Model` and `Msg`
both `deriving ToJson, FromJson`, the framework writes them for you:

```lean
import LeanTea
open LeanTea

structure Model where count : Int  deriving ToJson, FromJson
inductive Msg where | inc | dec | reset  deriving ToJson, FromJson

def app : WebApp Model Msg := WebApp.deriveJson {
  init   := { count := 0 },
  title  := "Counter",
  update := fun
    | .inc,   m => { m with count := m.count + 1 }
    | .dec,   m => { m with count := m.count - 1 }
    | .reset, _ => { count := 0 },
  view   := fun m => …
}
```

That's the same app as above with **three fewer fields** and the
guarantee that the wire format stays in sync when you add a `Model`
field. Lean 4's stdlib derives `ToJson` / `FromJson` for any
structure or inductive whose components also derive them, so this
composes recursively.

### Back / Forward navigation (`viewToUrl` / `urlToMsg`)

`WebApp` has two optional fields for opting into the History API:

```lean
def app : WebApp Model Msg := WebApp.deriveJson {
  init, update, view,
  -- map the current model to the URL the user should see
  viewToUrl := fun m =>
    match m.screen with
    | .home    => some "/"
    | .stats n => some s!"/stats/{n}",
  -- map a URL back to a Msg for popstate (Back button) replays
  urlToMsg := fun u =>
    if u == "/" then some (.goto .home)
    else if u.startsWith "/stats/" then
      (u.drop 7).toNat?.map (fun n => .goto (.stats n))
    else none
}
```

The server sends an `X-Url` header alongside `X-Model`; the runtime
calls `history.pushState`. On Back / Forward, it re-issues the step
with a marker that the server's `urlToMsg` decodes. Default behaviour
(no `viewToUrl`) leaves the URL untouched — apps that don't need
deep-links pay nothing.

## Where Html lives

`LeanTea.Html` is typed HTML — every tag is a Lean constructor.
You compose it like data:

```lean
def vocabCard (word meaning : String) : Html :=
  div_ [("class", "card")] [
    h2 [] [text word],
    p [("class", "muted")] [text meaning]
  ]
```

Renders to:

```html
<div class="card"><h2>word</h2><p class="muted">meaning</p></div>
```

Common helpers: `div_`, `span_`, `h1` … `h6`, `p`, `a_`, `button_`,
`form_`, `input_`. Any tag not pre-defined is `elem "tagName" attrs children`.

There's no JSX, no template strings. **You write Lean.** Type
mismatches surface in the editor; refactoring a `Model` field
updates every `view` automatically.

## Where Css lives

Two paths:

1. **Typed `LeanTea.Css.Sheet`** — write rules in Lean, get one rendered
   stylesheet. Sheet does this (see `examples/Sheet/Serve.lean`,
   `sheetStyles`):
   ```lean
   open LeanTea.Css in
   def sheetStyles : Sheet := [
     rule "body" [("font-family","'Segoe UI',sans-serif"),
                  ("background","#0f172a")],
     rule ".btn" [("background","#0284c7"),("color","#fff")]
   ]
   ```
2. **Plain `.css` files** — for static framing, include them in the
   page shell via `{{#include "site.css"}}` (Chapter 7).

The DSL shines when CSS is data-driven (theme dictionaries, conditional
rules). For static framing, a `.css` file with `{{#include}}` is less
ceremony.

## Where browser JS lives

The frontend's purity ends at the browser boundary. The DOM needs to be
touched on a user click. LeanTEA has two ways to produce that JS:

1. **The `.leanjs` subset** (Chapter 6) — Lean-shaped source files
   compiled to JS at startup. Reversi's whole client logic lives here
   (`examples/Reversi/Game.leanjs`).
2. **The typed `LeanTea.Js` DSL** — JS AST + builder monad inside Lean,
   so you can embed Lean values (`ToJsExpr`) directly. Sheet uses this
   for its RPC client that splices initial state.

Both lower through the same renderer. You can mix them.

## Where state lives

Two layers:

- **In-flight**: the current `Model` rides in the `X-Model` HTTP header
  between server and browser. The browser doesn't *own* state; it
  *forwards* the encoded model on every action. Pure update, stateless
  server, no sessions. (Chapter 3 covers this.)
- **Persisted**: anything that should survive a reload goes to SQLite via
  `Persist` (Chapter 4). The server splices DB-derived fields into the
  Model on hydrate so the browser sees current data even after a
  restart.

## TUI as well

The same `Model / Msg / update / view` triple works in the terminal.
`examples/Counter/Main.lean` is a TUI counter (key presses are the
Msgs); `examples/Quiz/Main.lean` is the same shape over a richer model.

## When to *not* use this

- You're embedding tens of thousands of DOM nodes (data-grid use case):
  the round-trip-per-event cost adds up. Use a SPA framework with a
  virtual DOM.
- You need offline-first behaviour: the round-trip pattern assumes the
  server is reachable. A service-worker layer would have to come first.
- You're embedding into an existing app with its own state model:
  LeanTEA wants the whole page.

For everything else the Elm-style triple holds — and the next chapter
shows the same triple on the *server* side.
