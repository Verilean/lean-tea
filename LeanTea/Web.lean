import LeanTea.Html
import Lean.Data.Json
import Lean.Data.Json.FromToJson

namespace LeanTea

open Lean (Json ToJson FromJson toJson fromJson?)

/-- An Elm-style web application. Pure: given (model, msg) it returns
    the next model + view. The runtime has two modes:

    * `--gen <dir>` writes the static SPA shell to `<dir>` (one shot).
    * `--loop` enters a stdin/stdout API loop that the HTTP server
      proxies as `GET /api/step`.

    State is held client-side and shipped on every API call via the
    `X-Model` header. That keeps the API stateless (restartable,
    horizontally scalable) and makes the static / API split clean. -/
structure WebApp (Model Msg : Type) where
  init        : Model
  update      : Msg → Model → Model
  view        : Model → Html
  encodeModel : Model → String
  decodeModel : String → Option Model
  decodeMsg   : String → Option Msg
  title       : String := "LeanTea App"
  /-- Optional: map the current `Model` to a browser URL path the user
      should see in the address bar (and that the **Back** button
      should navigate to). When non-`none` the server sets an
      `X-Url` response header and the runtime calls
      `history.pushState`. Pair with `urlToMsg` to make `popstate`
      replay the right action. -/
  viewToUrl   : Model → Option String := fun _ => none
  /-- Optional: derive a `Msg` from a URL path. Fires on `popstate`
      (Back / Forward navigation) and on initial page load when the
      requested URL is not the canonical landing path. -/
  urlToMsg    : String → Option Msg := fun _ => none

namespace WebApp

/-! ## Deriving codecs from `ToJson` / `FromJson`

If your `Model` and `Msg` both `deriving ToJson, FromJson`, you can
build the three text codec fields by reusing the JSON instances:

```lean
structure Model where count : Int  deriving ToJson, FromJson
inductive Msg where | inc | dec    deriving FromJson, ToJson

def app : WebApp Model Msg := WebApp.deriveJson {
  init := { count := 0 },
  title := "Greeter",
  update := …,
  view := …
}
```

Saves the hand-written `encodeModel` / `decodeModel` / `decodeMsg`
match-blocks every Elm-style app starts with. -/

/-- Encode any `ToJson` value as a compact JSON string. -/
def encodeJson [ToJson α] (a : α) : String := (toJson a).compress

/-- Decode a JSON string into a `FromJson` value, dropping the
    error detail. -/
def decodeJson [FromJson α] (s : String) : Option α :=
  match Json.parse s with
  | .ok j =>
    match (fromJson? j : Except String α) with
    | .ok a    => some a
    | .error _ => none
  | .error _ => none

/-- The same shape as `WebApp` but without the three text codec
    fields. Use with `WebApp.deriveJson` to build a `WebApp` from a
    `Model` / `Msg` pair that both derive `ToJson` / `FromJson`. -/
structure JsonApp (Model Msg : Type) where
  init   : Model
  update : Msg → Model → Model
  view   : Model → Html
  title  : String := "LeanTea App"

/-- Build a full `WebApp` from a `JsonApp` plus the `ToJson` /
    `FromJson` instances on `Model` and `Msg`. -/
def deriveJson [ToJson Model] [FromJson Model] [FromJson Msg] [ToJson Msg]
    (a : JsonApp Model Msg) : WebApp Model Msg :=
  { init := a.init
    update := a.update
    view := a.view
    title := a.title
    encodeModel := encodeJson
    decodeModel := decodeJson
    decodeMsg   := fun s =>
      /- Msgs travel as bare tag strings or as JSON objects depending
         on the inductive shape. Try the plain-string path first
         (the historic `data-msg="inc"` shape) then fall back to JSON. -/
      decodeJson s <|> decodeJson s!"\"{s}\"" }

def step (app : WebApp Model Msg) (modelStr? msgStr? : Option String) : String × String :=
  let model0 :=
    match modelStr? with
    | some s => (app.decodeModel s).getD app.init
    | none   => app.init
  let model :=
    match msgStr? with
    | some s =>
      match app.decodeMsg s with
      | some msg => app.update msg model0
      | none     => model0
    | none => model0
  (app.encodeModel model, (app.view model).render)

/-! ## Embedded static assets

`styles` and `runtime` are framework-level — every generated app
uses the same CSS and the same client runtime. -/

def styles    : String := include_str "assets/styles.css"
def runtimeJs : String := include_str "assets/runtime.js"

private def htmlEscape (s : String) : String :=
  s.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;" |>.replace "\"" "&quot;"

def indexHtml (title initialBody initialModel : String) : String :=
  "<!DOCTYPE html>\n" ++
  "<html lang=\"ja\">\n" ++
  "<head>\n" ++
  "<meta charset=\"UTF-8\">\n" ++
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" ++
  s!"<title>{htmlEscape title}</title>\n" ++
  "<link rel=\"stylesheet\" href=\"styles.css\">\n" ++
  "</head>\n" ++
  "<body>\n" ++
  s!"<div id=\"app\" data-model=\"{htmlEscape initialModel}\">{initialBody}</div>\n" ++
  "<script src=\"runtime.js\"></script>\n" ++
  "</body>\n</html>\n"

/-! ## Entry points -/

private def readArg (argv : List String) (flag : String) : Option String :=
  match argv with
  | [] => none
  | [_] => none
  | a :: b :: rest =>
    if a == flag then some b else readArg (b :: rest) flag

private def writeFile (dir : System.FilePath) (name : String) (content : String) : IO Unit := do
  IO.FS.writeFile (dir / name) content

def writeStatic (app : WebApp Model Msg) (outDir : System.FilePath) : IO Unit := do
  IO.FS.createDirAll outDir
  let (encoded, body) := step app none none
  writeFile outDir "index.html" (indexHtml app.title body encoded)
  writeFile outDir "styles.css" styles
  writeFile outDir "runtime.js" runtimeJs
  IO.println s!"wrote index.html, styles.css, runtime.js into {outDir}"

private def emit (encoded html : String) : IO Unit := do
  IO.println s!"MODEL\t{encoded}"
  IO.println "HTML"
  IO.println html
  IO.println "END"
  (← IO.getStdout).flush

partial def runStdinLoop (app : WebApp Model Msg) : IO Unit := do
  let stdin ← IO.getStdin
  let rec go : IO Unit := do
    let raw ← stdin.getLine
    if raw.isEmpty then return ()
    let line := raw.trimAsciiEnd.toString
    let parts := line.splitOn "\t"
    let msg?   := match parts with
                  | [] => none
                  | m :: _ => if m.isEmpty then none else some m
    let model? := match parts with
                  | _ :: mo :: _ => if mo.isEmpty then none else some mo
                  | _ => none
    let (encoded, html) := step app model? msg?
    emit encoded html
    go
  go

/-- Dispatch on `--gen <dir>`, `--loop`, or one-shot `--model` / `--msg`. -/
def run (app : WebApp Model Msg) (argv : List String) : IO Unit := do
  if let some dir := readArg argv "--gen" then
    writeStatic app (System.FilePath.mk dir)
    return
  if argv.contains "--loop" then
    runStdinLoop app
    return
  -- one-shot mode (for testing/debugging)
  let modelStr? := readArg argv "--model"
  let msgStr?   := readArg argv "--msg"
  let (encoded, html) := step app modelStr? msgStr?
  emit encoded html

end WebApp
end LeanTea
