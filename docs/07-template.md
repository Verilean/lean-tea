# 7 · Template — `.html` files with the right amount of magic

The typed `LeanTea.Html` DSL (Chapter 2) is the right tool when HTML is
*composed* from Lean data — Sheet's toolbar, dynamic SPA views. For
HTML that's mostly static framing around a `<canvas>` or a `<script>`,
you'd rather edit a real `.html` file in your editor.

`LeanTea.Template` is that path. Four constructs:

| Construct | Behaviour |
|---|---|
| `{{name}}` | direct substitution from a `Bindings` map |
| `{{#each xs}}…{{/each}}` | loop over a list value |
| `{{#if cond}}…{{else}}…{{/if}}` | branch on truthiness |
| `{{#include "path"}}` | splice another file as a partial |

That's the whole grammar. No filters, no helpers, no custom tags. If you
need any of those, drop to the typed `Html` DSL.

## Smallest example

`shell.html`:

```html
<!DOCTYPE html>
<title>{{title}}</title>
<ul>
  {{#each items}}
  <li class="{{class}}">{{label}}</li>
  {{/each}}
</ul>
{{#if note}}<p class="note">{{note}}</p>{{/if}}
```

Lean side:

```lean
import LeanTea

open LeanTea.Template

let page ← loadFile "shell.html"
let body ← page.render [
  ("title", .str "Hi"),
  ("items", .list [
    .dict [("class", .str "x"), ("label", .str "first")],
    .dict [("class", .str "y"), ("label", .str "second")]
  ]),
  ("note", .str "the note")
]
-- `body` is now the rendered HTML.
```

The `Bindings.Value` type is one of `.str`, `.list`, `.dict`. Falsy
for `{{#if}}` purposes: missing key, `.str ""`, `.list []`, `.dict []`.
Everything else is truthy.

## Hot reload

`Template.mkProvider path devMode : IO Provider` returns either a
cached `IO Template` (prod) or a re-read-every-time one (dev). The
servers use it like:

```lean
let pageProv ← Template.mkProvider "examples/Sheet/page.html" a.dev
…
| "/" =>
  let page ← pageProv          -- one cache hit in prod, one re-read in dev
  let body ← page.renderFlat [("gameJs", gameJs)]
  return Response.html 200 body
```

Edit a `.html` in `--dev` mode, refresh the browser, see the change —
no Lean rebuild needed.

## Includes

When several pages share a HUD scaffold, an importmap, or a stylesheet,
rather than duplicate the same N lines across files use `{{#include}}`:

```html
<head>
  <style>
    {{#include "examples/_assets/hud.css"}}
    /* page-specific overrides */
    body { background: #050810; }
  </style>
  {{#include "examples/_assets/importmap.html"}}
</head>
```

Change a colour in `_assets/hud.css` and every including page updates
on the next refresh. Includes are loaded inside the renderer
(`Template.render` is `IO String`), so the resolution chain — current
dir, parent, grandparent, `lean-elm/<path>` — is the same as
`loadFile` itself.

## Templates vs typed Html — when to pick which

| Use the Template engine when… | Use the typed `Html` DSL when… |
|---|---|
| The structure is mostly static framing | The structure is composed from Lean data |
| A designer / non-Lean dev should be able to edit | Refactoring a Lean record should propagate |
| You want hot-reload edits without rebuilding | You want type-checked HTML |
| You need an importmap or 80-line CSS inline | You're building a dozen variant rows |

Sheet mixes both: the toolbar buttons live in the template via
`{{#each toolbarButtons}}`, but every individual `<button>` is built
from a Lean structure list with typed fields. The `Value.dict` shape
carries the per-iteration scope.

## When to *not* use this

- **Conditional that needs more than truthiness** — `{{#if status == 'error'}}`
  isn't supported. Fold the logic into the binding (`.str "true"` or
  `.str ""`) or use the typed DSL.
- **Cross-file template inheritance** — `{{#extends}}` / `{{#block}}`
  isn't here. Use `{{#include}}` for partials; if you need a more
  elaborate layout system, build a shared Lean helper that produces
  the wrapper HTML.

The next chapter walks through the MCP server library.
