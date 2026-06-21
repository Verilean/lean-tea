import LeanTea
import LeanTea.Markdown

/-! # gen_site — render docs/*.md to docs-site/*.html

Walks every `.md` file in `docs/` (skipping `_archive/`), parses each
via `LeanTea.Markdown.Parser`, renders the AST to typed `Html`, and
wraps it in a shared layout with a sidebar and a single `site.css`
typed `Sheet`. The output `docs-site/` directory is what gets
published to GitHub Pages by the Actions workflow.

This is the sample CLI:

```
$ lake exe gen_site                 # writes to docs-site/
$ lake exe gen_site --out _build/site
```
-/

open LeanTea

namespace GenSite

/-! ## CLI -/

private structure Args where
  docsDir : String := "docs"
  outDir  : String := "docs-site"

private partial def parseArgs : List String → Args → Args
  | [], a => a
  | "--docs" :: v :: rest, a => parseArgs rest { a with docsDir := v }
  | "--out"  :: v :: rest, a => parseArgs rest { a with outDir := v }
  | _ :: rest, a => parseArgs rest a

/-! ## Sidebar — derived from the chapter list -/

private structure Page where
  slug  : String   -- e.g. "01-overview"
  title : String   -- e.g. "1 · Overview — what `lean-elm` is"
  file  : String   -- "docs/01-overview.md"
  deriving BEq, Inhabited

/-- Pull the first `# Heading` (or fall back to the filename) to use
    as the sidebar label. Strips inline markup that would otherwise
    show up as literal `…` or *…* in the link text. -/
private def titleOf (md : String) (slug : String) : String :=
  let firstLine := (md.splitOn "\n").head!
  let raw :=
    if firstLine.startsWith "# " then
      (firstLine.drop 2).toString.trimAscii.toString
    else slug
  /- Strip backticks, asterisks, and underscores meant as markdown
     markup. They look ugly in a sidebar anchor. -/
  raw.replace "`" "" |>.replace "**" "" |>.replace "*" ""

/-! ## Page chrome (a Lean function that produces full HTML) -/

private def sidebarNav (pages : List Page) (currentSlug : String) : Html :=
  let item (p : Page) : Html :=
    let cls := if p.slug == currentSlug then "current" else ""
    elem "li" [] [
      elem "a" [("href", p.slug ++ ".html"),
                ("class", cls)] [text p.title]
    ]
  elem "nav" [("class", "sidebar")] [
    h2 [] [text "lean-elm"],
    p [("class", "muted"),
       ("style", "color:#94a3b8;font-size:0.78rem;margin-bottom:1.2em")]
      [text "the book"],
    elem "ul" [] (pages.map item)
  ]

/-- Wrap one chapter body in `<html><body><nav><main>` chrome. -/
private def page (pages : List Page) (current : Page) (body : Html) : String :=
  let head :=
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n" ++
    "<meta charset=\"UTF-8\">\n" ++
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n" ++
    s!"<title>{current.title}</title>\n" ++
    "<link rel=\"stylesheet\" href=\"site.css\">\n" ++
    "</head>\n<body>\n"
  let main : Html :=
    elem "main" [("class", "content")] [
      body,
      elem "footer" [] [
        text "Built with ",
        elem "code" [] [text "lake exe gen_site"],
        text " · markdown rendered by ",
        elem "code" [] [text "LeanTea.Markdown"]
      ]
    ]
  let nav := sidebarNav pages current.slug
  head ++ nav.render ++ main.render ++ "\n</body>\n</html>\n"

/-! ## Driver -/

/-- Drop a `.md` filename's extension to get a slug. -/
private def slugOf (filename : String) : String :=
  if filename.endsWith ".md" then (filename.dropEnd 3).toString else filename

private def listMarkdown (dir : String) : IO (List String) := do
  let entries ← System.FilePath.readDir dir
  let names := entries.toList.filterMap fun e =>
    let name := e.fileName
    if name.endsWith ".md" && !name.startsWith "_" then some name
    else none
  /- Sorted alphabetically so the sidebar order is deterministic. -/
  return names.toArray.qsort (· < ·) |>.toList

/-- One-shot generator. Returns the count of pages written. -/
def run (args : Args) : IO Nat := do
  /- Make output dir. -/
  IO.FS.createDirAll args.outDir

  /- Write the typed CSS sheet. -/
  IO.FS.writeFile s!"{args.outDir}/site.css" LeanTea.Markdown.Theme.render
  IO.eprintln s!"wrote {args.outDir}/site.css"

  /- Build the Page index. -/
  let mdNames ← listMarkdown args.docsDir
  let mut pages : List Page := []
  for name in mdNames do
    let file := s!"{args.docsDir}/{name}"
    let src ← IO.FS.readFile file
    let slug := slugOf name
    pages := pages ++ [{ slug, title := titleOf src slug, file }]

  /- Make sure index.md is the entry point if present; otherwise the
     first chapter becomes index. -/
  let indexPage? := pages.find? (·.slug == "index")
  let chapterPages := pages.filter (·.slug != "index")

  let mut count : Nat := 0
  for p in pages do
    let src ← IO.FS.readFile p.file
    let doc := LeanTea.Markdown.Parser.parse src
    let body := LeanTea.Markdown.Render.documentToHtml doc
    let outName :=
      if some p == indexPage? then "index.html" else s!"{p.slug}.html"
    let html := page chapterPages p body
    IO.FS.writeFile s!"{args.outDir}/{outName}" html
    IO.eprintln s!"wrote {args.outDir}/{outName}"
    count := count + 1
  return count

end GenSite

def main (args : List String) : IO Unit := do
  let a := GenSite.parseArgs args {}
  let n ← GenSite.run a
  IO.println s!"gen_site: wrote {n} pages into {a.outDir}/"
