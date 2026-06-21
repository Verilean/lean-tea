import LeanTea.Css

/-! # LeanTea.Markdown.Theme — typed CSS sheet for the GitHub Pages
site

A focused, readable, dark-ish theme. Body fills the viewport; a fixed
sidebar carries the chapter list; the main panel is a centred article
column with comfortable reading width. Everything is data, so swapping
the palette is a Lean-side edit. -/

namespace LeanTea.Markdown.Theme

open LeanTea.Css

/-- Colour palette — change these to retheme the whole site. -/
structure Palette where
  bg        : String := "#0b1220"
  surface   : String := "#10182a"
  border    : String := "#1f2a44"
  text      : String := "#e2e8f0"
  textDim   : String := "#94a3b8"
  link      : String := "#7dd3fc"
  linkHover : String := "#bae6fd"
  codeBg    : String := "#0a101e"
  codeText  : String := "#bef264"
  accent    : String := "#fbbf24"
  deriving Inhabited

/-- Build the typed sheet from a palette. -/
def sheet (p : Palette := {}) : Sheet := [
  /- Reset + base -/
  rule "*" [
    ("box-sizing", "border-box"),
    ("margin", "0"), ("padding", "0")
  ],
  rule "html, body" [
    ("background", p.bg),
    ("color", p.text),
    ("font-family", "'Inter','Segoe UI',system-ui,sans-serif"),
    ("line-height", "1.6")
  ],
  rule "body" [
    ("display", "flex"),
    ("min-height", "100vh")
  ],

  /- Sidebar -/
  rule "nav.sidebar" [
    ("flex", "0 0 260px"),
    ("background", p.surface),
    ("border-right", s!"1px solid {p.border}"),
    ("padding", "32px 22px"),
    ("position", "sticky"),
    ("top", "0"),
    ("max-height", "100vh"),
    ("overflow-y", "auto")
  ],
  rule "nav.sidebar h2" [
    ("font-size", "0.85rem"),
    ("text-transform", "uppercase"),
    ("letter-spacing", "1.5px"),
    ("color", p.textDim),
    ("margin-bottom", "12px"),
    ("font-weight", "700")
  ],
  rule "nav.sidebar ul" [
    ("list-style", "none")
  ],
  rule "nav.sidebar li" [
    ("margin", "5px 0")
  ],
  rule "nav.sidebar a" [
    ("color", p.text),
    ("text-decoration", "none"),
    ("font-size", "0.92rem"),
    ("display", "block"),
    ("padding", "5px 8px"),
    ("border-radius", "4px")
  ],
  rule "nav.sidebar a:hover" [
    ("background", p.bg),
    ("color", p.linkHover)
  ],
  rule "nav.sidebar a.current" [
    ("background", p.bg),
    ("color", p.accent),
    ("font-weight", "600")
  ],

  /- Main column -/
  rule "main.content" [
    ("flex", "1"),
    ("max-width", "820px"),
    ("padding", "44px 56px 80px"),
    ("margin", "0 auto")
  ],

  /- Headings -/
  rule "h1, h2, h3, h4, h5, h6" [
    ("color", p.text),
    ("margin-top", "1.6em"),
    ("margin-bottom", "0.5em"),
    ("font-weight", "700"),
    ("line-height", "1.25")
  ],
  rule "h1" [("font-size", "2.0rem"), ("margin-top", "0.4em")],
  rule "h2" [("font-size", "1.45rem"),
              ("border-bottom", s!"1px solid {p.border}"),
              ("padding-bottom", "0.3em")],
  rule "h3" [("font-size", "1.2rem")],
  rule "h4" [("font-size", "1.05rem")],

  /- Paragraphs, links -/
  rule "p" [("margin", "0.6em 0")],
  rule "a" [
    ("color", p.link),
    ("text-decoration", "none")
  ],
  rule "a:hover" [
    ("color", p.linkHover),
    ("text-decoration", "underline")
  ],

  /- Emphasis -/
  rule "strong" [("color", p.text), ("font-weight", "700")],
  rule "em" [("color", p.textDim), ("font-style", "italic")],

  /- Inline code -/
  rule "code" [
    ("font-family", "'JetBrains Mono','SF Mono',Menlo,monospace"),
    ("font-size", "0.9em"),
    ("background", p.codeBg),
    ("color", p.codeText),
    ("padding", "1px 6px"),
    ("border-radius", "3px"),
    ("border", s!"1px solid {p.border}")
  ],

  /- Code blocks -/
  rule "pre" [
    ("background", p.codeBg),
    ("border", s!"1px solid {p.border}"),
    ("border-radius", "6px"),
    ("padding", "14px 16px"),
    ("overflow-x", "auto"),
    ("margin", "1em 0"),
    ("font-size", "0.88rem"),
    ("line-height", "1.5")
  ],
  rule "pre code" [
    ("background", "transparent"),
    ("border", "none"),
    ("padding", "0"),
    ("color", p.codeText)
  ],

  /- Lists -/
  rule "ul, ol" [
    ("margin", "0.6em 0 0.6em 1.4em")
  ],
  rule "li" [
    ("margin", "0.25em 0")
  ],

  /- Blockquote -/
  rule "blockquote" [
    ("border-left", s!"3px solid {p.accent}"),
    ("padding", "0.4em 1em"),
    ("margin", "1em 0"),
    ("color", p.textDim),
    ("background", p.surface),
    ("border-radius", "0 6px 6px 0")
  ],

  /- HR -/
  rule "hr" [
    ("border", "none"),
    ("border-top", s!"1px solid {p.border}"),
    ("margin", "2em 0")
  ],

  /- Tables -/
  rule "table" [
    ("border-collapse", "collapse"),
    ("margin", "1em 0"),
    ("width", "100%"),
    ("font-size", "0.92rem"),
    ("background", p.surface),
    ("border-radius", "6px"),
    ("overflow", "hidden")
  ],
  rule "thead" [
    ("background", p.bg)
  ],
  rule "th, td" [
    ("padding", "8px 12px"),
    ("border-bottom", s!"1px solid {p.border}"),
    ("text-align", "left"),
    ("vertical-align", "top")
  ],
  rule "th" [
    ("color", p.accent),
    ("font-weight", "700")
  ],
  rule "tbody tr:last-child td" [
    ("border-bottom", "none")
  ],

  /- Footer -/
  rule "footer" [
    ("margin-top", "4em"),
    ("padding-top", "2em"),
    ("border-top", s!"1px solid {p.border}"),
    ("color", p.textDim),
    ("font-size", "0.85rem"),
    ("text-align", "center")
  ],

  /- Responsive -/
  media "(max-width: 880px)" [
    rule "body" [("flex-direction", "column")],
    rule "nav.sidebar" [
      ("flex", "0 0 auto"),
      ("position", "static"),
      ("max-height", "none"),
      ("border-right", "none"),
      ("border-bottom", s!"1px solid {p.border}")
    ],
    rule "main.content" [
      ("padding", "28px 22px 60px")
    ]
  ]
]

/-- Render the default theme to a CSS string. -/
def render : String := (sheet {}).render

end LeanTea.Markdown.Theme
