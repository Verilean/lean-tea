import LeanTea.Html
import LeanTea.Markdown.Ast

/-! # LeanTea.Markdown.Render — Block → LeanTea.Html

A straight, no-frills transform. Headings become `<hN>`, paragraphs
become `<p>`, bullets become `<ul><li>…</li></ul>`, code fences become
`<pre><code class="language-…">…</code></pre>`, etc. -/

namespace LeanTea.Markdown.Render

open LeanTea LeanTea.Markdown

/-- Rewrite same-repo `*.md` links to the generated `*.html`. The
    chapter files live flat under `docs-site/`, so cross-references
    of the shape `[…](04-persist.md)` or
    `[…](04-persist.md#section)` need their extension swapped before
    they render into HTML. Absolute URLs, `mailto:`, and pure
    fragments pass through untouched. A leading `docs/` is stripped
    so links of the form `(docs/foo.md)` (common in `README.md`-style
    sources) also resolve once flattened. -/
def rewriteMdLink (url : String) : String :=
  if url.startsWith "http://" || url.startsWith "https://"
     || url.startsWith "mailto:" || url.startsWith "//"
     || url.startsWith "#" then url
  else
    let parts := url.splitOn "#"
    let path  := parts.head!
    let frag  := if parts.length > 1
                 then "#" ++ String.intercalate "#" parts.tail!
                 else ""
    if path.endsWith ".md" then
      let stem := (path.dropEnd 3).toString
      let stem := if stem.startsWith "docs/" then (stem.drop 5).toString else stem
      stem ++ ".html" ++ frag
    else url

mutual

partial def inlineToHtml : Inline → Html
  | .text s     => text s
  | .bold cs    => elem "strong" [] (cs.map inlineToHtml)
  | .italic cs  => elem "em" [] (cs.map inlineToHtml)
  | .code s     => elem "code" [] [text s]
  | .link cs url =>
    elem "a" [("href", rewriteMdLink url)] (cs.map inlineToHtml)
  | .image alt url =>
    elem "img" [("src", url), ("alt", alt)] []
  | .br         => elem "br" [] []

partial def blockToHtml : Block → Html
  | .heading n inlines =>
    let tag := s!"h{n}"
    elem tag [] (inlines.map inlineToHtml)
  | .paragraph inlines =>
    elem "p" [] (inlines.map inlineToHtml)
  | .bullets items =>
    elem "ul" []
      (items.map fun cs => elem "li" [] (cs.map inlineToHtml))
  | .ordered items =>
    elem "ol" []
      (items.map fun cs => elem "li" [] (cs.map inlineToHtml))
  | .code lang body =>
    let codeAttrs :=
      if lang.isEmpty then []
      else [("class", s!"language-{lang}")]
    elem "pre" [] [elem "code" codeAttrs [text body]]
  | .hrule =>
    elem "hr" [] []
  | .blockquote blocks =>
    elem "blockquote" [] (blocks.map blockToHtml)
  | .table header rows =>
    let headerCells := header.map fun cs =>
      elem "th" [] (cs.map inlineToHtml)
    let headerRow := elem "tr" [] headerCells
    let bodyRows := rows.map fun row =>
      elem "tr" [] (row.map fun cs => elem "td" [] (cs.map inlineToHtml))
    elem "table" [] [
      elem "thead" [] [headerRow],
      elem "tbody" [] bodyRows
    ]

end

/-- Render an entire document to a single fragment. The caller wraps
    it in `<main>` / `<article>` / page chrome. -/
def documentToHtml (doc : Document) : Html :=
  elem "div" [("class", "markdown-body")] (doc.map blockToHtml)

end LeanTea.Markdown.Render
