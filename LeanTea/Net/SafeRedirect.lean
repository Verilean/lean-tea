import LeanTea.Net.Http

/-! # LeanTea.Net.SafeRedirect — open-redirect-proof 302 responses

Closes **Open Redirect** (IPA 「安全なウェブサイトの作り方」§ オープン
リダイレクト, OWASP A01, shadan-kun "Open Redirect").

The classic mistake:

```
let next := req.query.find "next" |>.getD "/"
return Response.redirect next          -- attacker: ?next=https://evil.example
```

`SafeRedirect.to` requires the caller to declare which targets are
acceptable up front: either a list of trusted origins (`https://app.example`),
or "relative paths only". Anything else is rejected at construction.

The constructor is `private mk`, so the only way to obtain a
`SafeRedirect` is via `SafeRedirect.to` (smart) or
`SafeRedirect.toForced` (audit-escape, for places that genuinely
need a non-allow-listed target).

The CRLF guard from `Response.setHeader` still applies at the
`toResponse` step — header injection is checked twice. -/

namespace LeanTea.Net

/-- A redirect target that has passed an allow-list check. Build via
    `SafeRedirect.to`. -/
structure SafeRedirect where
  private mk ::
  /-- The validated `Location:` value. Guaranteed to be (a) a path
      starting with `/` but not `//` or `/\\`, or (b) a URL whose
      origin matches one of the caller-supplied trusted origins. -/
  location : String
  deriving Inhabited, Repr

namespace SafeRedirect

/-- Strip CR/LF/NUL up front; those would smuggle a second header
    line via `Location:`. -/
private def hasCtlInjection (s : String) : Bool :=
  s.contains '\r' || s.contains '\n' || s.contains '\u0000'

/-- A *relative* path: starts with `/`, but neither `//foo`
    (protocol-relative — browser sends to attacker host) nor `/\foo`
    (Windows-flavoured protocol-relative). Also reject `\foo` and
    schemes (anything before the first `/` that contains `:`). -/
private def isSafeRelative (loc : String) : Bool :=
  if loc == "" then false
  else if !loc.startsWith "/" then false
  else if loc.startsWith "//" then false
  else if loc.startsWith "/\\" then false
  /- Reject `/.` or `/..` only at the very root — they look harmless
     but tie up downstream router code; leave to the router. -/
  else true

/-- Normalise an origin into "scheme://host" (drop trailing slash).
    Used to compare a candidate URL's prefix against the allow-list. -/
private def normaliseOrigin (o : String) : String :=
  if o.endsWith "/" then (o.toRawSubstring.dropRight 1).toString else o

/-- Allow a redirect target if:
      * it's a safe relative path (`/dashboard`), OR
      * it begins with an allow-listed origin (`https://app.example`)
        followed by `/` or end-of-string.

    Examples (with `trustedOrigins := ["https://app.example"]`):

      `"/users/me"`                  → ok   (relative)
      `"https://app.example"`        → ok   (exact origin)
      `"https://app.example/x"`      → ok   (origin + path)
      `"//evil.example/x"`           → error (protocol-relative)
      `"https://evil.example/x"`     → error (not on allow-list)
      `"javascript:alert(1)"`        → error (scheme not allowed) -/
def to (trustedOrigins : List String) (loc : String) : Except String SafeRedirect :=
  if hasCtlInjection loc then
    .error "SafeRedirect.to: CR/LF/NUL in location"
  else if isSafeRelative loc then
    .ok ⟨loc⟩
  else
    let lower := loc.toLower
    /- Schemes we always refuse, even if some allow-list entry started
       with the same prefix. Belt-and-braces. -/
    if lower.startsWith "javascript:" || lower.startsWith "data:"
       || lower.startsWith "vbscript:" || lower.startsWith "file:" then
      .error s!"SafeRedirect.to: dangerous scheme in {loc.take 30}"
    else
      let origins := trustedOrigins.map normaliseOrigin
      let matched := origins.any fun o =>
        loc == o || loc.startsWith (o ++ "/")
      if matched then .ok ⟨loc⟩
      else .error s!"SafeRedirect.to: target {loc.take 60} not on allow-list"

/-- Panic variant for literal redirects in trusted code. -/
def to! (trustedOrigins : List String) (loc : String) : SafeRedirect :=
  match to trustedOrigins loc with
  | .ok r    => r
  | .error e => panic! s!"SafeRedirect.to!: {e}"

/-- **Audit escape**: grep for `SafeRedirect.toForced` to find every
    place that intentionally bypasses the allow-list. Use only for
    deliberate cases (e.g. OAuth callback that has already been
    independently verified). -/
def toForced (loc : String) : SafeRedirect := ⟨loc⟩

/-- Lower to the existing HTTP response shape. The framework's
    `setHeader!` will still reject any residual control characters,
    so this is safe to call from a handler. -/
def toResponse (r : SafeRedirect) : LeanTea.Net.Http.Response :=
  LeanTea.Net.Http.Response.redirect r.location

instance : ToString SafeRedirect where toString r := r.location

end SafeRedirect

end LeanTea.Net
