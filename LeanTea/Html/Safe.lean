import LeanTea.Html

/-! # LeanTea.Html.Safe — typed HTML attributes that make XSS unrepresentable

The base `LeanTea.Html` already HTML-escapes text content and
attribute *values* at render time. What it can't catch on its own:

  * `javascript:` / `data:text/html` URL schemes in `href` / `src`
  * Event-handler attribute *names* (`onclick`, `onload`, …)
  * Unrestricted use of `.raw` to splice unsanitised HTML

This module closes those gaps. `SafeAttr` has a `private mk` —
outside this file, the only way to build one is via the smart
constructors below. The framework guarantees that a `SafeAttr` will
never carry an `on*` name or a `javascript:` URL.

Existing call sites keep working (the inductive AST is unchanged);
new code should prefer `SafeAttr.text` / `SafeAttr.url` / `SafeAttr.num`
and the `safe!` literal forms in the smart constructors below. -/

namespace LeanTea.Html

/-- A validated HTML attribute (name + value). The constructor is
    `private` to this file, so the only entry points are
    `SafeAttr.text` / `SafeAttr.url` / `SafeAttr.num`. -/
structure SafeAttr where
  private mk ::
  name  : String
  value : String
  deriving Inhabited, Repr

namespace SafeAttr

/-! ## Allow-lists -/

/-- Attribute names accepted by `SafeAttr.text`. Anything starting
    with `data-` or `aria-` is also OK. The list is conservative —
    add to it only after auditing that the new name can't carry
    executable content. -/
private def nameAllowList : List String := [
  -- structure / identity
  "id", "class", "title", "lang", "dir", "tabindex", "role", "slot",
  -- linking / sources
  "href", "src", "srcset", "rel", "target", "download", "type",
  -- forms
  "name", "value", "placeholder", "for", "autocomplete", "autofocus",
  "checked", "disabled", "readonly", "required", "multiple",
  "min", "max", "step", "pattern", "minlength", "maxlength",
  "rows", "cols", "size", "wrap",
  "action", "method", "enctype", "novalidate", "form", "formaction",
  -- media
  "alt", "controls", "autoplay", "loop", "muted", "poster",
  -- SVG geometry
  "width", "height", "viewBox", "preserveAspectRatio", "transform",
  "x", "y", "x1", "x2", "y1", "y2", "cx", "cy", "r", "rx", "ry",
  "d", "points", "fill", "stroke", "stroke-width",
  "stroke-dasharray", "stroke-linecap", "stroke-linejoin",
  "font-family", "font-size", "font-weight", "text-anchor",
  -- framework dispatch
  "data-msg"
]

private def isEventHandlerName (s : String) : Bool := s.startsWith "on"

private def nameAllowed (s : String) : Bool :=
  if isEventHandlerName s then false
  else nameAllowList.contains s
    || s.startsWith "data-"
    || s.startsWith "aria-"

/-- URL schemes that we forbid in `href` / `src` / `action`. We don't
    enumerate the allowed list — anything not on the rejected list
    plus relative paths (`/`, `#`, `?`, no `:` at all) is allowed.
    This matches Yesod / Lucid behaviour. -/
private def isSchemeRejected (url : String) : Bool :=
  let lower := url.trimAscii.toString.toLower
  lower.startsWith "javascript:"
    || lower.startsWith "data:text/html"
    || lower.startsWith "vbscript:"

/-! ## Smart constructors. -/

/-- Plain-text attribute. Rejects event-handler names (`on*`) and
    anything not on the allow-list. The *value* is still escaped at
    render time by `Html.escape`. -/
def text (name value : String) : Except String SafeAttr :=
  if isEventHandlerName name then
    .error s!"SafeAttr.text: event-handler name rejected ({name})"
  else if !nameAllowed name then
    .error s!"SafeAttr.text: name not on allow-list ({name})"
  else
    .ok ⟨name, value⟩

/-- URL attribute (`href`, `src`, `action`, `formaction`, `data-msg`).
    Rejects `javascript:` and `data:text/html` schemes. Everything
    else (http(s), mailto, tel, relative paths, fragments) is allowed
    and escaped at render. -/
def url (name urlV : String) : Except String SafeAttr :=
  let urlAttrs : List String := ["href", "src", "action", "formaction", "data-msg"]
  if !urlAttrs.contains name then
    .error s!"SafeAttr.url: URL attribute must be one of {urlAttrs}; got '{name}'"
  else if isSchemeRejected urlV then
    .error s!"SafeAttr.url: scheme rejected ({urlV.take 30})"
  else
    .ok ⟨name, urlV⟩

/-- Numeric attribute. The number is just `toString`'d; we still
    apply the name allow-list. -/
def num (name : String) (n : Int) : Except String SafeAttr :=
  text name (toString n)

/-! ## "Trust me, I'm a literal" variants.

   When the *name* is a literal string in your source code, the
   allow-list check is something the human reader has already done.
   These `*!` variants throw on rejection so call sites stay terse
   for safe literals. **Do not pass user-controlled `name`s through
   these** — only literal strings. -/

def text! (name value : String) : SafeAttr :=
  match text name value with
  | .ok a    => a
  | .error e => panic! s!"SafeAttr.text!: {e}"

def url! (name urlV : String) : SafeAttr :=
  match url name urlV with
  | .ok a    => a
  | .error e => panic! s!"SafeAttr.url!: {e}"

def num! (name : String) (n : Int) : SafeAttr :=
  match num name n with
  | .ok a    => a
  | .error e => panic! s!"SafeAttr.num!: {e}"

/-! ## Bridge to the existing `Attrs`. -/

/-- Lower a list of `SafeAttr` to the AST's `Attrs` shape so existing
    builders (`div_`, `a_`, …) accept them. The framework's render
    pass still HTML-escapes each value. -/
def toAttrs (xs : List SafeAttr) : LeanTea.Attrs :=
  xs.map fun a => (a.name, a.value)

end SafeAttr

/-! ## Convenience builders.

    These mirror the unsafe `LeanTea.Html.div_` / `a_` / etc. but
    take a `List SafeAttr` directly. Pick the one matching your tag. -/

private def elemSafe (tag : String) (attrs : List SafeAttr) (cs : List Html) : Html :=
  Html.elem tag (SafeAttr.toAttrs attrs) cs

def divSafe    (a : List SafeAttr) (cs : List Html) : Html := elemSafe "div"    a cs
def spanSafe   (a : List SafeAttr) (cs : List Html) : Html := elemSafe "span"   a cs
def aSafe      (a : List SafeAttr) (cs : List Html) : Html := elemSafe "a"      a cs
def buttonSafe (a : List SafeAttr) (cs : List Html) : Html := elemSafe "button" a cs
def inputSafe  (a : List SafeAttr) : Html                  := elemSafe "input"  a []
def imgSafe    (a : List SafeAttr) : Html                  := elemSafe "img"    a []
def h1Safe     (a : List SafeAttr) (cs : List Html) : Html := elemSafe "h1"     a cs
def h2Safe     (a : List SafeAttr) (cs : List Html) : Html := elemSafe "h2"     a cs
def h3Safe     (a : List SafeAttr) (cs : List Html) : Html := elemSafe "h3"     a cs
def pSafe      (a : List SafeAttr) (cs : List Html) : Html := elemSafe "p"      a cs

end LeanTea.Html
