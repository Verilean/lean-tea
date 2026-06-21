import LeanTea
import LeanTea.Html.Safe

/-! # safehtml_smoke — exercise `LeanTea.Html.SafeAttr`

This is `SECURITY.md` §"Primitive 2 · SafeHtml" in code. Demonstrates:

1. `SafeAttr.text` rejects `on*` event-handler names at runtime
2. `SafeAttr.url` rejects `javascript:` / `data:text/html` schemes
3. Allow-listed names (`href`, `class`, `data-*`, `aria-*`, SVG geometry)
   accept happily
4. `SafeAttr.toAttrs` lowers back to the existing `Attrs` shape so
   rendering uses the same HTML-escaping pipeline as the rest of the
   framework
5. Try to fabricate a `SafeAttr` directly from outside the module and
   the compiler refuses — see the bottom of this file. -/

open LeanTea LeanTea.Html

/-! ## Assertion helper — tiny LSpec stand-in. -/

private def expect (label : String) (ok : Bool) : IO Bool := do
  if ok then IO.println s!"  ✓ {label}"
  else IO.println s!"  ✗ {label}"
  return ok

/-! ## Smoke runner. -/

def main : IO Unit := do
  IO.println "── rejected names ─────────────────────────────"
  let mut all := true
  match SafeAttr.text "onclick" "evil()" with
  | .ok  _ => all ← expect "onclick rejected (was accepted!)" false
  | .error _ => all ← expect "onclick rejected" true
  match SafeAttr.text "onerror" "boom()" with
  | .ok _ => all ← expect "onerror rejected (was accepted!)" false
  | .error _ => all ← expect "onerror rejected" true
  match SafeAttr.text "style" "color:red" with
  | .ok _ => all ← expect "style NOT on allow-list — rejected (was accepted!)" false
  | .error _ => all ← expect "style NOT on allow-list — rejected" true

  IO.println "── rejected URL schemes ───────────────────────"
  match SafeAttr.url "href" "javascript:alert(1)" with
  | .ok _ => all ← expect "javascript: rejected (was accepted!)" false
  | .error _ => all ← expect "javascript: rejected" true
  match SafeAttr.url "href" "data:text/html,<script>alert(1)</script>" with
  | .ok _ => all ← expect "data:text/html rejected (was accepted!)" false
  | .error _ => all ← expect "data:text/html rejected" true
  match SafeAttr.url "href" "JaVaScRiPt:alert(1)" with
  | .ok _ => all ← expect "javascript: case-insensitive rejected (was accepted!)" false
  | .error _ => all ← expect "javascript: case-insensitive rejected" true

  IO.println "── accepted (safe) cases ──────────────────────"
  match SafeAttr.text "class" "btn primary" with
  | .ok _ => all ← expect "class accepted" true
  | .error e => all ← expect s!"class WAS rejected: {e}" false
  match SafeAttr.text "data-test" "click-target" with
  | .ok _ => all ← expect "data-* accepted" true
  | .error e => all ← expect s!"data-* WAS rejected: {e}" false
  match SafeAttr.text "aria-label" "Close" with
  | .ok _ => all ← expect "aria-* accepted" true
  | .error e => all ← expect s!"aria-* WAS rejected: {e}" false
  match SafeAttr.url "href" "/login" with
  | .ok _ => all ← expect "relative URL accepted" true
  | .error e => all ← expect s!"relative URL WAS rejected: {e}" false
  match SafeAttr.url "href" "https://example.com/x" with
  | .ok _ => all ← expect "https URL accepted" true
  | .error e => all ← expect s!"https URL WAS rejected: {e}" false
  match SafeAttr.url "href" "mailto:hi@x.com" with
  | .ok _ => all ← expect "mailto URL accepted" true
  | .error e => all ← expect s!"mailto WAS rejected: {e}" false

  IO.println "── rendering still HTML-escapes the value ─────"
  /- The framework's render pass already escapes `&`, `<`, `>`, `"`,
     `'` in attribute values. Make sure a SafeAttr that carries a
     literal `&` shows up escaped. -/
  let aClass := SafeAttr.text! "class" "left & right"
  let aHref  := SafeAttr.url! "href"  "/q?a=1&b=2"
  let link := aSafe [aClass, aHref] [LeanTea.text "go"]
  let rendered := link.render
  let isEscaped := rendered.endsWith "</a>"
    && rendered.startsWith "<a"
    && (rendered.splitOn "&amp;").length == 3
  if isEscaped then
    all ← expect "value is HTML-escaped on render" true
  else
    IO.println s!"  rendered: {rendered}"
    all ← expect "value is HTML-escaped on render" false

  IO.println "── compile-time guarantee (see source comment) ─"
  /-
    Try uncommenting the line below — outside `LeanTea/Html/Safe.lean`
    the `SafeAttr.mk` constructor is `private`, so the compiler rejects
    direct fabrication. The only entry points are `text` / `url` / `num`,
    which run the allow-list checks above.

      example : SafeAttr := SafeAttr.mk "onclick" "evil()"
      -- error: SafeAttr.mk is private to LeanTea.Html.Safe.
  -/
  IO.println "  (see the comment block above; the compile error is the test)"

  if all then
    IO.println "safehtml_smoke: done"
  else
    IO.println "safehtml_smoke: FAILURES"
    IO.Process.exit 1
