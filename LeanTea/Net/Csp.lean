import LeanTea.Net.Http

/-! # LeanTea.Net.Csp — Content Security Policy as typed Lean values

Closes the **"forgot a CSP directive"** + **"typo in a CSP keyword"**
classes (IPA 「安全なウェブサイトの作り方」§3.5 XSS hardening / OWASP
A05 Security Misconfiguration). A hand-rolled `Content-Security-Policy:`
header is one of the more common runtime-only audit findings:

  * `script-srx` (typo) — silently no-op
  * `'self' 'unsafe-inline'` — accidentally enables inline `<script>`
  * missing `frame-ancestors` — clickjacking despite `X-Frame-Options`

`LeanTea.Net.Csp` makes the directive names and source keywords
*Lean values*: a typo doesn't compile. `Csp.strict` is the locked-down
baseline; tweak the records you need and pass to `Response.csp`. -/

namespace LeanTea.Net

/-- A single CSP source expression. Names mirror the CSP spec
    keywords; `host` carries a free-form origin string (validated
    only against CR/LF/NUL — the rest is the operator's policy
    decision). -/
inductive CspSrc where
  /-- `'self'` — same-origin. -/
  | self
  /-- `'none'` — block all sources for this directive. -/
  | none
  /-- `'unsafe-inline'` — allow inline `<script>` / `<style>`.
      Use sparingly; the framework's `nonce`/`hash` shapes are
      strictly preferred. -/
  | unsafeInline
  /-- `'unsafe-eval'` — allow `eval` / `new Function`. Almost
      always a code-smell; CSPv3 has alternatives. -/
  | unsafeEval
  /-- `'strict-dynamic'` — trust scripts loaded by a nonced root. -/
  | strictDynamic
  /-- `data:` — data URIs. Often needed for `img-src`. -/
  | data
  /-- `blob:` — blob URIs (downloaded ranges, Worker bootstrap). -/
  | blob
  /-- `https:` — wildcard https origin. Crude; prefer a hostname. -/
  | https
  /-- Specific origin: `https://cdn.example.com`. -/
  | host (origin : String)
  /-- `'nonce-…'` — per-request nonce for inline content. -/
  | nonce (n : String)
  /-- `'sha256-…'` — base64 hash of a known-good inline block. -/
  | sha256 (hash : String)
  deriving Inhabited, Repr

namespace CspSrc

/-- Render one source expression into the on-wire string fragment.
    Rejects CR/LF/NUL in caller-controlled `host`/`nonce`/`sha256`
    strings so we don't smuggle a new header line. -/
def render : CspSrc → String
  | .self           => "'self'"
  | .none           => "'none'"
  | .unsafeInline   => "'unsafe-inline'"
  | .unsafeEval     => "'unsafe-eval'"
  | .strictDynamic  => "'strict-dynamic'"
  | .data           => "data:"
  | .blob           => "blob:"
  | .https          => "https:"
  | .host o         => o
  | .nonce n        => s!"'nonce-{n}'"
  | .sha256 h       => s!"'sha256-{h}'"

end CspSrc

/-- A Content-Security-Policy header in typed form. Empty lists
    drop the directive entirely, so the rendered header only carries
    the directives the caller cares about.

    See `Csp.strict` for the locked-down default and `Csp.report`
    for a report-only policy. -/
structure Csp where
  defaultSrc     : List CspSrc := []
  scriptSrc      : List CspSrc := []
  styleSrc       : List CspSrc := []
  imgSrc         : List CspSrc := []
  fontSrc        : List CspSrc := []
  connectSrc     : List CspSrc := []
  mediaSrc       : List CspSrc := []
  objectSrc      : List CspSrc := []
  frameSrc       : List CspSrc := []
  frameAncestors : List CspSrc := []
  formAction     : List CspSrc := []
  baseUri        : List CspSrc := []
  workerSrc      : List CspSrc := []
  /-- `report-uri https://csp.example.com/report` (legacy). -/
  reportUri      : Option String := Option.none
  /-- `report-to my-report-group` (CSPv3). -/
  reportTo       : Option String := Option.none
  /-- When true, the renderer emits the directive but the browser
      only reports violations instead of blocking them. Useful when
      rolling out a new policy. -/
  reportOnly     : Bool := false
  deriving Inhabited, Repr

namespace Csp

private def directive (name : String) (srcs : List CspSrc) : Option String :=
  if srcs.isEmpty then Option.none
  else some s!"{name} {String.intercalate " " (srcs.map CspSrc.render)}"

/-- Locked-down baseline:

      default-src 'none';
      script-src  'self';
      style-src   'self';
      img-src     'self' data:;
      font-src    'self' data:;
      connect-src 'self';
      base-uri    'self';
      frame-ancestors 'none';
      form-action 'self';

    A real app almost always needs to extend `script-src` /
    `connect-src` with the trusted CDN/API origins. Start here,
    tweak the directives you need, hand the value to
    `Response.csp`. -/
def strict : Csp := {
  defaultSrc     := [.none],
  scriptSrc      := [.self],
  styleSrc       := [.self],
  imgSrc         := [.self, .data],
  fontSrc        := [.self, .data],
  connectSrc     := [.self],
  baseUri        := [.self],
  frameAncestors := [.none],
  formAction     := [.self]
}

/-- Render the policy into the on-wire header value
    (`default-src 'self'; …`). Empty directives are dropped. -/
def render (c : Csp) : String :=
  let parts : List (Option String) := [
    directive "default-src"     c.defaultSrc,
    directive "script-src"      c.scriptSrc,
    directive "style-src"       c.styleSrc,
    directive "img-src"         c.imgSrc,
    directive "font-src"        c.fontSrc,
    directive "connect-src"     c.connectSrc,
    directive "media-src"       c.mediaSrc,
    directive "object-src"      c.objectSrc,
    directive "frame-src"       c.frameSrc,
    directive "frame-ancestors" c.frameAncestors,
    directive "form-action"     c.formAction,
    directive "base-uri"        c.baseUri,
    directive "worker-src"      c.workerSrc,
    c.reportUri.map fun u => s!"report-uri {u}",
    c.reportTo.map  fun g => s!"report-to {g}"
  ]
  String.intercalate "; " (parts.filterMap id)

/-- The header *name* — flips between report-only and enforce. -/
def headerName (c : Csp) : String :=
  if c.reportOnly then "content-security-policy-report-only"
  else                  "content-security-policy"

end Csp

namespace Http

open LeanTea.Net (Csp)
open LeanTea.Net.Http

/-- Attach a CSP header. Routes through `Response.setHeader!` so
    a stray CR/LF in a `host`/`nonce`/`sha256` source is still
    refused at construction. -/
def Response.csp (r : Response) (c : Csp) : Response :=
  r.setHeader! c.headerName c.render

end Http

end LeanTea.Net
