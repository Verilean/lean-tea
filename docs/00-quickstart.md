# 0 · Quickstart — your own LeanTEA app from zero

In ten minutes you'll have a Lean 4 web app serving real HTML, with the
LeanTEA framework as a Lake dependency.

## Prerequisites

- **Lean 4** (the latest stable toolchain — match `lean-toolchain` in
  this repo): `elan toolchain install leanprover/lean4:v4.30.0`
- A C compiler (`cc` / `clang` / `gcc`) for the SQLite + WebSocket FFI
- **(optional)** `node` if you want to use the `leanjs_run` CLI

## Step 1 — create the package

```sh
lake new my_app
cd my_app
```

`lake new` gives you a stock project with `Main.lean`, `lakefile.lean`,
and `lean-toolchain`. Delete `My_app.lean` and the placeholder library
entry — we'll point at our own modules instead.

## Step 2 — depend on LeanTEA

Edit `lakefile.lean`:

```lean
import Lake
open Lake DSL

package my_app

require LeanTea from git
  "https://github.com/<owner>/lean-tea" @ "main"

@[default_target]
lean_exe my_app where
  root := `Main
```

Then `lake update` to fetch the dependency and resolve transitive
toolchains.

If you're hacking on LeanTEA locally, swap `from git "…"` for
`from "/absolute/path/to/lean-tea"` so edits flow through immediately.

## Step 3 — write `Main.lean`

Smallest possible LeanTEA app — a TUI greeter you can extend into a web
app later:

```lean
import LeanTea
open LeanTea

structure Model where greeting : String := "hello"
inductive Msg where | shout | calm

def view (m : Model) : Html :=
  div_ [] [
    h1 [] [text m.greeting],
    button_ [("data-msg","shout")] [text "Shout"],
    button_ [("data-msg","calm")]  [text "Calm"]
  ]

def update : Msg → Model → Model
  | .shout, m => { m with greeting := m.greeting.toUpper ++ "!" }
  | .calm,  m => { m with greeting := m.greeting.toLower }

def encodeModel (m : Model) : String := m.greeting
def decodeModel (s : String) : Option Model := some { greeting := s }
def decodeMsg : String → Option Msg
  | "shout" => some .shout
  | "calm"  => some .calm
  | _       => none

def app : WebApp Model Msg :=
  { init := {}, title := "Greeter", update, view,
    encodeModel, decodeModel, decodeMsg }

def main (args : List String) : IO Unit := WebApp.run app args
```

## Step 4 — build and run

```sh
lake build
./.lake/build/bin/my_app          # serves http://127.0.0.1:8001
```

Open the URL, click the buttons, watch the greeting transform. That's
the whole Elm-style triple wired through a real HTTP server, ~1 KB of
inline JS, no node, no bundler.

## Step 5 — graduate to a real app

From here, the chapters in order:

| Want… | Read |
|---|---|
| Persist clicks across restarts | [Chapter 4 · Persist](04-persist.md) |
| Typed RPC routes Lean ↔ JS | [Chapter 5 · Rpc](05-rpc.md) |
| Static `.html` + `{{var}}` shells | [Chapter 7 · Template](07-template.md) |
| Expose your app as an MCP tool surface | [Chapter 8 · Mcp](08-mcp.md) |
| Lean-shaped browser code (.leanjs) | [Chapter 6 · LeanJs](06-leanjs.md) |
| OAuth / sessions / Passkey | [Chapter 3 · Backend](03-backend.md) |

The `examples/` directory is the ground truth: every line of code
quoted in the book exists there verbatim. When stuck, `grep` it.

## Common pitfalls

- **`lake update` hangs** — clear `~/.elan/toolchains/` partial
  downloads and retry, or pin `lean-toolchain` to a release present
  in your elan cache.
- **`undefined reference to leantea_sqlite_*`** — the C FFI wrapper
  is built only when `LeanTea` is in the dependency graph. Confirm
  your `require` line resolved (check `lake-manifest.json`).
- **Port already bound** — `WebApp.run` defaults to `:8001`; pass
  `--port 8080` to change.
