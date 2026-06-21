namespace LeanTea

/-- Minimal HTML AST. `text` content is HTML-escaped on render. -/
inductive Html where
  | text (s : String)
  | elem (tag : String) (attrs : List (String × String)) (children : List Html)
  | raw  (s : String)  -- pre-escaped or trusted block (used sparingly)
  deriving Inhabited

namespace Html

private def escape (s : String) : String :=
  s.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "\"" "&quot;"
   |>.replace "'" "&#39;"

private def renderAttrs (attrs : List (String × String)) : String :=
  attrs.foldl (fun acc (k, v) => acc ++ s!" {k}=\"{escape v}\"") ""

partial def render : Html → String
  | .text s        => escape s
  | .raw  s        => s
  | .elem tag a cs => s!"<{tag}{renderAttrs a}>{cs.foldl (fun acc c => acc ++ render c) ""}</{tag}>"

end Html

/-! ## Convenience builders -/

def text (s : String) : Html := .text s
def raw  (s : String) : Html := .raw s

abbrev Attrs := List (String × String)

def elem (tag : String) (a : Attrs := []) (cs : List Html := []) : Html := .elem tag a cs

def html_   (cs : List Html) : Html := elem "html" [] cs
def head_   (cs : List Html) : Html := elem "head" [] cs
def body_   (a : Attrs := []) (cs : List Html) : Html := elem "body" a cs
def title_  (s : String)      : Html := elem "title" [] [text s]
def meta_   (a : Attrs)       : Html := elem "meta" a []
def style_  (s : String)      : Html := elem "style" [] [raw s]

def h1 (a : Attrs := []) (cs : List Html) : Html := elem "h1" a cs
def h2 (a : Attrs := []) (cs : List Html) : Html := elem "h2" a cs
def p  (a : Attrs := []) (cs : List Html) : Html := elem "p"  a cs
def div_  (a : Attrs := []) (cs : List Html) : Html := elem "div"  a cs
def span_ (a : Attrs := []) (cs : List Html) : Html := elem "span" a cs
def a_    (a : Attrs := []) (cs : List Html) : Html := elem "a"    a cs
def form_ (a : Attrs := []) (cs : List Html) : Html := elem "form" a cs
def button_ (a : Attrs := []) (cs : List Html) : Html := elem "button" a cs
def input_  (a : Attrs := []) : Html := elem "input" a []
def label_  (a : Attrs := []) (cs : List Html) : Html := elem "label" a cs
def br_     : Html := elem "br" [] []
def small_  (a : Attrs := []) (cs : List Html) : Html := elem "small" a cs
def strong_ (a : Attrs := []) (cs : List Html) : Html := elem "strong" a cs

/-! ## SVG builders

The same `Html` AST works for inline SVG since `elem` is tag-agnostic.
These helpers just save the per-call boilerplate. -/

def svg_     (a : Attrs := []) (cs : List Html) : Html := elem "svg" a cs
def g_       (a : Attrs := []) (cs : List Html) : Html := elem "g" a cs
def rectSvg  (a : Attrs := []) : Html := elem "rect" a []
def ellipseSvg (a : Attrs := []) : Html := elem "ellipse" a []
def circleSvg (a : Attrs := []) : Html := elem "circle" a []
def lineSvg  (a : Attrs := []) : Html := elem "line" a []
def pathSvg  (a : Attrs := []) : Html := elem "path" a []
def polylineSvg (a : Attrs := []) : Html := elem "polyline" a []
def textSvg  (a : Attrs := []) (cs : List Html) : Html := elem "text" a cs
def defs_    (a : Attrs := []) (cs : List Html) : Html := elem "defs" a cs
def pattern_ (a : Attrs := []) (cs : List Html) : Html := elem "pattern" a cs
def fObject_ (a : Attrs := []) (cs : List Html) : Html := elem "foreignObject" a cs

/-- Audio playback link that triggers `window.speakLine` on click. -/
def speak (utterance : String) (label : String := "▶") (rate := "0.95") (liaison := true) : Html :=
  let liai := if liaison then "1" else "0"
  a_ [("class","l"), ("href","#"), ("data-tts", utterance),
      ("data-rate", rate), ("data-liaison", liai)] [text label]

/-- Primary action link (rendered as `.btn`). Matches the original
    HTML app's full-width call-to-action style. -/
def btn (label : String) (msg : String) (variant : String := "") : Html :=
  let cls := if variant.isEmpty then "btn" else s!"btn {variant}"
  a_ [("class", cls), ("href","#"), ("data-msg", msg)] [text label]

/-- Mode card on the home screen (icon + title + desc).
    Pass `wide := true` to span both grid columns. -/
def modeCard (icon title desc msg : String) (wide := false) : Html :=
  let attrs : Attrs :=
    [("class","mode-card"), ("href","#"), ("data-msg", msg)] ++
    (if wide then [("style","grid-column:span 2")] else [])
  a_ attrs [
    elem "div" [("class","mode-icon")] [text icon],
    elem "div" [("class","mode-title")] [text title],
    elem "div" [("class","mode-desc")] [text desc]
  ]

/-- A row in the sound-change lesson table (written → spoken + play). -/
def scRow (written _spoken katakana note tts : String) : Html :=
  elem "div" [("class","sc-row")] [
    a_ [("class","sc-play"),("href","#"),("data-tts", tts),
        ("data-rate","0.9"),("data-liaison","0")] [text "▶"],
    elem "div" [] [
      elem "span" [("class","sc-written")] [text written],
      text " → ",
      elem "span" [("class","sc-spoken")] [text katakana]
    ],
    elem "div" [("class","sc-note")] [text note]
  ]

/-- Audio-only play button (no msg dispatch). -/
def play (utterance : String) (label : String := "▶") (rate := "0.95") (liaison := true) : Html :=
  speak utterance (label := label) (rate := rate) (liaison := liaison)

/-- Equalizer-style waveform that the runtime activates while TTS is
    speaking. Bars are CSS-animated; no JS frame loop needed. -/
def waveform : Html :=
  elem "div" [("class","waveform")]
    (List.replicate 24 (elem "div" [("class","wave-bar")] []))

end LeanTea
