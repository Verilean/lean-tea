/-! # LeanTea.Template — `{{var}}` substitution + `{{#each}}` blocks

The framework's typed `LeanTea.Html` AST is the right tool for HTML
that's *composed* from Lean data — Canvas's tab bar, the Reversi
board view, anything with heavy logic.

This module covers the other end of the spectrum: a `.html` file
shell with `{{key}}` placeholders and `{{#each xs}}…{{/each}}`
blocks for repeated chunks. The page is real HTML the editor
highlights and a designer can hand-edit.

```html
<ul class="toolbar">
  {{#each buttons}}
  <li class="btn {{class}}" data-msg="{{msg}}">{{icon}} {{label}}</li>
  {{/each}}
</ul>
```

```lean
let page ← Template.loadFile "examples/Canvas/page.html"
page.render [
  ("buttons", .list [
    .dict [("class", .str "primary"), ("msg", .str "rect"),
           ("icon", .str "▢"), ("label", .str "Rect")],
    .dict [("class", .str "primary"), ("msg", .str "ellipse"),
           ("icon", .str "○"), ("label", .str "Ellipse")]
  ])
]
```

Why not Pug / Hamlet / JSX literal syntax? See `docs/03-js-dsl.md` —
the framework's typed `LeanTea.Html` AST already covers the
"compose HTML from Lean data with loops/conditionals" use case.
This module covers the *other* use case: a designer-friendly
shell where the typing isn't worth losing native editor support. -/

namespace LeanTea.Template

/-! ## Value tree

`{{key}}` substitutions resolve against a `Value`. Strings are the
common case; lists feed `{{#each}}`; dicts are list items that
expose named fields. -/

mutual
inductive Value where
  | str  (s : String) : Value
  | list (xs : List Value) : Value
  /-- Object with named fields — used as the per-iteration scope
      inside `{{#each}}` when the iterated list contains records. -/
  | dict (fields : List (String × Value)) : Value
  deriving Inhabited
end

abbrev Bindings := List (String × Value)

/-! ## Parsed template -/

/-- A template is a sequence of nodes that produce strings during
    rendering. `each` blocks carry a pre-parsed body so iteration
    is cheap. -/
inductive Node where
  /-- Literal text. -/
  | text  (s : String) : Node
  /-- `{{key}}` — direct substitution. -/
  | var   (key : String) : Node
  /-- `{{#each key}}…{{/each}}`. -/
  | each  (key : String) (body : List Node) : Node
  /-- `{{#if key}}…{{else}}…{{/if}}`. Truthiness rule for the lookup
      at `key`: missing / empty string / empty list / empty dict → false;
      everything else → true. `elseBody` is `[]` when no `{{else}}` arm
      was given. -/
  | ifN   (key : String) (thenBody elseBody : List Node) : Node
  /-- `{{#include "path"}}` — single-tag (no body) load + splice. The
      partial is resolved at render time via `loadFile`; nested
      includes are supported because the included template is parsed
      and rendered with the same routine. -/
  | include (path : String) : Node
  deriving Inhabited

structure Template where
  nodes : List Node
  deriving Inhabited

/-! ## Parsing -/

private structure ParseState where
  bs   : ByteArray
  pos  : Nat
  size : Nat
  deriving Inhabited

private def atDelim (st : ParseState) (a b : Char) : Bool :=
  st.pos + 1 < st.size
    && st.bs[st.pos]! == a.toNat.toUInt8
    && st.bs[st.pos + 1]! == b.toNat.toUInt8

/-- Look for `{{`. -/
private def isOpen (st : ParseState) : Bool := atDelim st '{' '{'

/-- Look for `}}`. -/
private def isClose (st : ParseState) : Bool := atDelim st '}' '}'

/-- Consume the text between `pos` and the next `{{` (or EOF).
    Operates on byte ranges so multi-byte UTF-8 sequences (anything
    above U+007F — em dashes, CJK, emoji) survive intact. -/
private partial def readLiteral (st : ParseState) : ParseState × String := Id.run do
  let start := st.pos
  let mut p := start
  while p < st.size && !(atDelim { st with pos := p } '{' '{') do
    p := p + 1
  let s := String.fromUTF8! (st.bs.extract start p)
  return ({ st with pos := p }, s)

/-- Inside `{{…}}` — consume up to the closing `}}` (not included).
    Returns the raw text between the braces, trimmed of whitespace. -/
private partial def readTag (st : ParseState) : ParseState × String := Id.run do
  -- Skip past `{{`.
  let mut p := st.pos + 2
  let start := p
  while p + 1 < st.size && !(atDelim { st with pos := p } '}' '}') do
    p := p + 1
  let raw := String.fromUTF8! (st.bs.extract start p)
  let trimmed : String := raw.trimAscii.toString
  return ({ st with pos := p + 2 }, trimmed)

/-- Closers accepted by `parseNodes` when recursing. `none` = top
    level (no closer expected). For `{{#if}}` the closer is a *set*
    `/if` ∪ `else` because the same recursion produces the then-body
    and then keeps going for the else-body. The caller distinguishes
    which one fired from the actual tag stashed in `acc`. -/
private inductive Closer
  | none
  | tag (s : String)
  | ifBody  -- accepts both "/if" and "else"
  deriving Inhabited

/-- Result of one parse pass: residual state, parsed nodes, and the
    closing tag that stopped the recursion (empty at top level). -/
private structure ParseResult where
  state    : ParseState
  nodes    : List Node
  stopTag  : String := ""
  deriving Inhabited

/-- Recursive descent. -/
private partial def parseNodes (st : ParseState) (closer : Closer)
    : ParseResult := Id.run do
  let mut s := st
  let mut acc : List Node := []
  while s.pos < s.size do
    if isOpen s then
      let (s', tag) := readTag s
      s := s'
      -- Closer match?
      let stop :=
        match closer with
        | .none      => false
        | .tag t     => tag == t
        | .ifBody    => tag == "/if" || tag == "else"
      if stop then
        return { state := s, nodes := acc.reverse, stopTag := tag }
      -- `#each KEY` opens a block.
      if tag.startsWith "#each " then
        let key : String := (tag.drop 6).trimAscii.toString
        let r := parseNodes s (.tag "/each")
        s := r.state
        acc := .each key r.nodes :: acc
      -- `#if KEY` opens a conditional with optional `{{else}}` arm.
      else if tag.startsWith "#if " then
        let key : String := (tag.drop 4).trimAscii.toString
        let r1 := parseNodes s .ifBody
        s := r1.state
        if r1.stopTag == "else" then
          let r2 := parseNodes s (.tag "/if")
          s := r2.state
          acc := .ifN key r1.nodes r2.nodes :: acc
        else
          acc := .ifN key r1.nodes [] :: acc
      -- `#include "path"` — single-tag splice (no body to recurse on).
      else if tag.startsWith "#include " then
        let rest' := (tag.drop 9).trimAscii.toString
        /- Strip surrounding quotes if present. -/
        let path :=
          if rest'.startsWith "\"" && rest'.endsWith "\"" then
            (rest'.drop 1).dropEnd 1 |>.toString
          else rest'
        acc := .include path :: acc
      else if tag.startsWith "/" then
        -- Stray closer with no matching opener — fall through.
        acc := .text ("{{" ++ tag ++ "}}") :: acc
      else
        acc := .var tag :: acc
    else
      let (s', lit) := readLiteral s
      s := s'
      if lit.isEmpty then break  -- safety
      acc := .text lit :: acc
  return { state := s, nodes := acc.reverse }

/-- Parse a complete template body. -/
def parse (src : String) : Template :=
  let bs := src.toUTF8
  let r := parseNodes { bs, pos := 0, size := bs.size } .none
  { nodes := r.nodes }

/-! ## File loader

Resolves a relative path against several plausible roots so the
binary works from either `lean-elm/` or one level up. Defined ahead
of the renderer so `{{#include}}` can call into it. -/

private def candidates (rel : String) : List String := [
  rel, "../" ++ rel, "../../" ++ rel, "lean-elm/" ++ rel
]

def loadFile (rel : String) : IO Template := do
  for path in candidates rel do
    if ← System.FilePath.pathExists path then
      let src ← IO.FS.readFile path
      return parse src
  throw <| IO.userError <|
    "couldn't locate template " ++ rel ++ " — tried: "
    ++ String.intercalate ", " (candidates rel)

/-! ## Rendering -/

private def Bindings.lookup (b : Bindings) (key : String) : Option Value :=
  b.find? (·.fst == key) |>.map (·.snd)

/-- Render a value as a string. For lists / dicts we fall back to a
    debug-ish form — but anywhere the template uses `{{key}}` the
    target is expected to be a `Value.str`. -/
private def Value.toText : Value → String
  | .str s     => s
  | .list xs   => "[" ++ String.intercalate "," (xs.map Value.toText) ++ "]"
  | .dict _    => "{…}"

/-- Forward-declared loader, set below. Lives in mutual scope so
    `renderNodes` can call into it for `{{#include}}` without
    circular-import gymnastics. -/
private partial def renderNodes (b : Bindings) : List Node → IO String
  | [] => return ""
  | .text s :: rest => do
    let tail ← renderNodes b rest
    return s ++ tail
  | .var key :: rest => do
    let v := (b.lookup key).map Value.toText |>.getD ""
    let tail ← renderNodes b rest
    return v ++ tail
  | .each key body :: rest => do
    let pieces : List String ← (do
      match b.lookup key with
      | some (.list xs) =>
        xs.foldlM (init := []) fun acc item => do
          let inner : Bindings := match item with
            | .dict fs => fs ++ b
            | other    => [("this", other), (".", other)] ++ b
          let s ← renderNodes inner body
          return acc ++ [s]
      | _ => return [])
    let tail ← renderNodes b rest
    return String.join pieces ++ tail
  | .ifN key thenBody elseBody :: rest => do
    let truthy : Bool :=
      match b.lookup key with
      | none           => false
      | some (.str "") => false
      | some (.list []) => false
      | some (.dict []) => false
      | _              => true
    let chosen := if truthy then thenBody else elseBody
    let head ← renderNodes b chosen
    let tail ← renderNodes b rest
    return head ++ tail
  | .include path :: rest => do
    /- Load + parse + render the partial inline. Errors propagate via
       `loadFile`'s throw — we don't silently swallow a missing
       partial because that would mask real problems. -/
    let inc ← loadFile path
    let head ← renderNodes b inc.nodes
    let tail ← renderNodes b rest
    return head ++ tail

def Template.render (t : Template) (bindings : Bindings) : IO String :=
  renderNodes bindings t.nodes

/-! ## Convenience: string-only bindings

Backwards-compatible flat substitution. Useful for the common case
where every binding is a `Value.str`. -/

def Template.renderFlat (t : Template) (bindings : List (String × String))
    : IO String :=
  Template.render t (bindings.map (fun (k, v) => (k, .str v)))

/-! ## Dev-mode hot reload

A `Provider` hides whether the template is cached once at startup
or re-read from disk on every request. Servers in `--dev` mode use
the live variant so edits to `.html` files show up on the next
browser refresh — no Lean rebuild, no restart.

```lean
let pageProvider ← Template.mkProvider "examples/Canvas/page.html" devMode
…
| "/" =>
  let page ← pageProvider   -- one re-read in dev, cache hit in prod
  Response.html 200 (page.render …)
```

In prod (devMode = false), `mkProvider` reads + parses once and
returns a `pure t` action that costs nothing thereafter. In dev,
it returns `loadFile path` directly, so every request takes a
fresh read + parse. -/

abbrev Provider := IO Template

def mkProvider (path : String) (devMode : Bool) : IO Provider := do
  if devMode then
    -- Smoke-test once at startup so a missing/malformed file
    -- still fails loudly, but don't cache the result.
    let _ ← loadFile path
    return loadFile path
  else
    let cached ← loadFile path
    return pure cached

end LeanTea.Template
