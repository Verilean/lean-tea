/-! # LeanTea.Markdown.Ast

A focused subset of CommonMark, chosen so that every `docs/*.md`
chapter in the book renders cleanly. Two layers:

* `Block` — paragraph-level structure (headings, lists, code fences,
  blockquotes, tables, horizontal rules).
* `Inline` — what lives inside a paragraph (text runs, emphasis,
  links, inline code, line breaks).

Out of scope by design (defer until a real doc needs them): setext
headings, reference-style links, image grids, autolinks, definition
lists, nested lists with arbitrary indentation, HTML pass-through.

The AST is what the parser produces and what the renderer consumes.
Both layers can be replaced independently. -/

namespace LeanTea.Markdown

mutual

/-- Inline elements live inside a paragraph / heading / list-item /
    table-cell. Nesting only goes one level deep — `**bold *italic***`
    works, but we don't try to build arbitrary trees. -/
inductive Inline where
  /-- Plain text run. Newlines are already collapsed by the parser. -/
  | text  (s : String) : Inline
  /-- `**bold**` content. -/
  | bold  (cs : List Inline) : Inline
  /-- `*italic*` content. -/
  | italic (cs : List Inline) : Inline
  /-- ``inline code`` (no further nesting). -/
  | code  (s : String) : Inline
  /-- `[label](url)`. -/
  | link  (label : List Inline) (url : String) : Inline
  /-- `![alt](url)`. -/
  | image (alt : String) (url : String) : Inline
  /-- Hard line break (`\` at end of line, or two trailing spaces). -/
  | br : Inline

/-- Block-level structure. Each constructor maps to one HTML element
    family. The parser emits a flat list of these; the renderer
    folds them back into nested HTML. -/
inductive Block where
  /-- `# H1` through `###### H6`. `level` is 1..6. -/
  | heading    (level : Nat) (inline : List Inline) : Block
  /-- A paragraph — one or more lines joined into a single inline
      list. -/
  | paragraph  (inline : List Inline) : Block
  /-- `- foo` or `* foo` bullet list. Each item is itself an inline
      list (no nested blocks at this level). -/
  | bullets    (items : List (List Inline)) : Block
  /-- `1. foo` numeric list. We don't preserve the actual numbers —
      the renderer uses `<ol>` and the browser numbers. -/
  | ordered    (items : List (List Inline)) : Block
  /-- ` ``` `<lang>` … ` ``` `. `lang` is the optional info string
      (used for syntax-highlighting class). -/
  | code       (lang : String) (body : String) : Block
  /-- `---` horizontal rule. -/
  | hrule      : Block
  /-- `> quoted` — each constituent line stripped of the leading
      `>` and re-parsed as block content. -/
  | blockquote (blocks : List Block) : Block
  /-- A pipe table with a header row, a separator (`|---|---|`), and
      one or more data rows. -/
  | table      (header : List (List Inline))
               (rows   : List (List (List Inline))) : Block

end

instance : Inhabited Inline := ⟨.text ""⟩
instance : Inhabited Block  := ⟨.paragraph []⟩

/-- A complete parsed document is just a list of blocks. -/
abbrev Document := List Block

end LeanTea.Markdown
