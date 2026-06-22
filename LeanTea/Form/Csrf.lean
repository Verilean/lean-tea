import LeanTea.Auth
import LeanTea.Net.Http
import LeanTea.Html

/-! # LeanTea.Form.Csrf — CSRF middleware via double-submit cookie

Closes **Cross-Site Request Forgery** (IPA 「安全なウェブサイトの作り方」
§3.8 / OWASP A01). The pattern:

  1. On any safe GET that produces a form, mint a random 32-byte
     token, set it as a `Secure`/`HttpOnly`/`SameSite=Strict` cookie,
     **and** echo it as a hidden `<input>` in the form.
  2. On the POST that the form submits to, compare the cookie value
     to the hidden field value with a constant-time string compare.
     Mismatch → 403, request is refused.

This is the "double-submit cookie" CSRF defence. It doesn't need a
server-side token store because the cookie *is* the source of truth;
the attacker's cross-origin POST can't read the victim's cookie
(SameSite blocks it) and can't forge a matching hidden field.

`SameSite=Strict` is what the framework relies on for the bulk of
the guarantee; the hidden field is defence-in-depth for the rare
browser/proxy that strips the cookie attribute. -/

namespace LeanTea.Form.Csrf

open LeanTea.Auth (randomToken)
open LeanTea.Net.Http
open LeanTea.Html

/-! ## Config -/

structure Config where
  /-- Cookie name. Default `csrf`. Change if the host site already
      uses this name for something else. -/
  cookieName : String := "csrf"
  /-- Form field name. Default `_csrf` — matches Django / Express
      so existing tooling works unchanged. -/
  fieldName  : String := "_csrf"
  /-- Set the `Secure` flag on the cookie. Mirror your `COOKIE_SECURE`
      env var. -/
  secure     : Bool := false
  deriving Inhabited, Repr

/-! ## Token minting + cookie roundtrip -/

/-- Pull the token from the request cookie. -/
def Config.tokenInRequest (cfg : Config) (req : Request) : Option String :=
  req.cookie? cfg.cookieName

/-- Get the current token, or mint a new one. Returns the token AND
    the (possibly-updated) response — when the request didn't carry
    the cookie, the returned `Response` carries the freshly minted
    `Set-Cookie:` so the next form render echoes the same value. -/
def Config.ensure (cfg : Config) (req : Request) (r : Response)
    : IO (String × Response) := do
  match cfg.tokenInRequest req with
  | some t => return (t, r)
  | none   =>
    let t ← randomToken
    let cookieVal :=
      let sec := if cfg.secure then "; Secure" else ""
      s!"{cfg.cookieName}={t}; Path=/; HttpOnly; SameSite=Strict{sec}"
    return (t, r.setHeader! "set-cookie" cookieVal)

/-! ## Hidden-input helper

The framework's `Html` already HTML-escapes attribute values, so a
malformed token (which would never happen — `randomToken` is hex)
still couldn't break out of the value attribute. -/

/-- Render the hidden `<input>` to embed in a form. Place it inside
    a `<form method="POST">` immediately after the opening tag. -/
def hiddenInput (cfg : Config) (token : String) : Html :=
  elem "input" [
    ("type",  "hidden"),
    ("name",  cfg.fieldName),
    ("value", token)
  ] []

/-! ## Verification -/

/-- Constant-time byte comparison. Two strings of equal length
    compare as a single timing class; unequal lengths still take
    the same path so an attacker can't measure on the length. -/
private def byteAt (ba : ByteArray) (i : Nat) : UInt8 :=
  if i < ba.size then ba.get! i else 0

private def constantTimeEq (a b : String) : Bool := Id.run do
  let abs := a.toUTF8
  let bbs := b.toUTF8
  let mut diff : UInt8 := if abs.size == bbs.size then 0 else 1
  let n := max abs.size bbs.size
  for i in [:n] do
    let ai := byteAt abs i
    let bi := byteAt bbs i
    diff := diff ||| (ai ^^^ bi)
  return diff == 0

/-- Pull the form-field value from a `application/x-www-form-urlencoded`
    body. Returns `none` if the field is absent or the body isn't
    form-encoded. Multipart bodies (`multipart/form-data`) need the
    caller to do their own form parse first; CSRF still applies
    but extraction lives upstream. -/
private def fieldFromBody (cfg : Config) (req : Request) : Option String :=
  let body := String.fromUTF8? req.body |>.getD ""
  let pref := cfg.fieldName ++ "="
  (body.splitOn "&").findSome? fun pair =>
    if pair.startsWith pref then some (pair.drop pref.length).toString
    else none

/-- Verify the request's CSRF token. **Caller must only call this
    on unsafe methods** (POST / PUT / PATCH / DELETE). Returns
    `.ok ()` on match; `.error msg` on mismatch / missing token. -/
def Config.verify (cfg : Config) (req : Request) : Except String Unit :=
  match cfg.tokenInRequest req, fieldFromBody cfg req with
  | none, _        => .error "csrf: missing cookie"
  | _,    none     => .error s!"csrf: missing form field '{cfg.fieldName}'"
  | some c, some f =>
    if constantTimeEq c f then .ok ()
    else .error "csrf: cookie ≠ field"

/-- Wrap a handler so unsafe-method requests are refused if the
    CSRF token doesn't validate. Safe methods (GET / HEAD / OPTIONS)
    pass through untouched. Use as:

    ```
    let safe := Csrf.gate cfg myHandler
    ```
-/
def gate (cfg : Config) (inner : Handler) : Handler := fun req => do
  let mutating := req.method == "POST" || req.method == "PUT"
               || req.method == "PATCH" || req.method == "DELETE"
  if !mutating then inner req
  else match cfg.verify req with
       | .ok ()   => inner req
       | .error e => return Response.text 403 s!"forbidden: {e}\n"

end LeanTea.Form.Csrf
