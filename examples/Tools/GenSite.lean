import LeanTea
import LeanTea.Site

/-! # gen_site — render docs/*.md to docs-site/*.html

The lean-tea book, built with the generic `LeanTea.Site` generator:
every `.md` under `docs/` (skipping `_drafts/` and other `_` names)
becomes a page in the built-in sidebar layout, plus a typed-CSS
`site.css`. The output `docs-site/` directory is what
`.github/workflows/pages.yml` publishes to GitHub Pages.

```
$ lake exe gen_site                 # writes to docs-site/
$ lake exe gen_site --out _build/site
```

For any other site, use `static_site` (`examples/Tools/StaticSite.lean`). -/

open LeanTea

namespace GenSite

structure Args where
  docsDir : String := "docs"
  outDir  : String := "docs-site"

partial def parseArgs : List String → Args → Args
  | [], a => a
  | "--docs" :: v :: rest, a => parseArgs rest { a with docsDir := v }
  | "--out"  :: v :: rest, a => parseArgs rest { a with outDir := v }
  | _ :: rest, a => parseArgs rest a

def config (a : Args) : Site.Config := {
  srcDir := a.docsDir, outDir := a.outDir,
  siteTitle := "lean-tea", tagline := "the book",
  footer := [text "Built with ", elem "code" [] [text "lake exe gen_site"],
             text " · markdown rendered by ", elem "code" [] [text "LeanTea.Markdown"]] }

end GenSite

def main (args : List String) : IO Unit := do
  let a := GenSite.parseArgs args {}
  let r ← Site.build (GenSite.config a)
  IO.println s!"gen_site: wrote {r.pages.length} pages into {a.outDir}/"
