import LeanTea.Markdown.Ast
import LeanTea.Markdown.Inline
import LeanTea.Markdown.Parser
import LeanTea.Markdown.Render
import LeanTea.Markdown.Theme

/-! # LeanTea.Markdown — top-level entry points

A focused Markdown subset compiled to typed `LeanTea.Html`. See the
individual sub-modules for the AST, parser, and renderer details.

Quick usage:

```lean
open LeanTea.Markdown

let doc  := Parser.parse "# Hello\n\nWorld."
let html := Render.documentToHtml doc
-- `html.render` produces the HTML string.
```
-/

namespace LeanTea.Markdown

/-- Parse + render in one step. -/
def renderHtml (src : String) : LeanTea.Html :=
  Render.documentToHtml (Parser.parse src)

end LeanTea.Markdown
