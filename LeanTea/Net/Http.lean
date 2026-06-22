import Std.Async.TCP
import Std.Net

/-! # Minimal HTTP/1.1 server using `Std.Internal.Async.TCP`

This is intentionally tiny: enough to serve our static `dist/`
directory and the `/api/step` endpoint that the english app uses.
Not a general-purpose web server.

Limitations:
* connection: close after every response (no keep-alive)
* request body capped at 1 MB
* no chunked encoding, no compression
* request handling is sequential (single-threaded accept loop) — fine
  for a personal app on localhost. -/

namespace LeanTea.Net.Http

open Std.Async Std.Net

/-! ## Types -/

structure Request where
  method  : String
  path    : String        -- URL path, no query
  query   : String        -- raw query string, no `?`
  headers : Array (String × String)   -- header names are lowercased
  body    : ByteArray
  deriving Inhabited

structure Response where
  status  : Nat
  headers : Array (String × String) := #[]
  body    : ByteArray := .empty
  deriving Inhabited

/-! ## Helpers -/

def Response.text (status : Nat) (body : String) : Response := {
  status,
  headers := #[("content-type", "text/plain; charset=utf-8")],
  body := body.toUTF8
}

def Response.html (status : Nat) (body : String) (extra : Array (String × String) := #[])
  : Response := {
  status,
  headers := #[("content-type", "text/html; charset=utf-8")] ++ extra,
  body := body.toUTF8
}

def Response.notFound : Response := .text 404 "not found\n"
def Response.badRequest : Response := .text 400 "bad request\n"
def Response.serverError (msg : String) : Response := .text 500 (msg ++ "\n")

/-! ## Header injection guard

We refuse to put CR (`\r`), LF (`\n`), or NUL (`\0`) into a header
*name* or *value*. Sneaking those characters through is the classic
HTTP-response / mail-header injection trick: a user-controlled
`?next=…` ends up in a `Location:` header containing
`\r\nSet-Cookie: …`, and the browser sees a second header line.

`Response.setHeader` returns `Except String Response` so callers
forward bad input as a 400. `setHeader!` is the panic variant for
literal headers in trusted code. Other helpers in this file
(`redirect`, `withCookie`, `clearCookie`) now route through the
same guard. -/

private def hasHeaderInjection (s : String) : Bool :=
  s.contains '\r' || s.contains '\n' || s.contains '\u0000'

/-- Append a header. Refuses CR / LF / NUL in either the name or
    the value — those characters split the header line and let
    attackers inject extra headers (or an entire body). -/
def Response.setHeader (r : Response) (name value : String)
  : Except String Response :=
  if hasHeaderInjection name then
    .error s!"Response.setHeader: CR/LF/NUL in header name ({repr name})"
  else if hasHeaderInjection value then
    .error s!"Response.setHeader: CR/LF/NUL in header value (name={name})"
  else
    .ok { r with headers := r.headers.push (name, value) }

/-- Panic variant for literal headers in trusted code. -/
def Response.setHeader! (r : Response) (name value : String) : Response :=
  match r.setHeader name value with
  | .ok r    => r
  | .error e => panic! s!"Response.setHeader!: {e}"

/-- Recommended baseline security headers. Add them once to every
    HTML / API response and most clickjacking + MIME-sniff +
    referer-leak issues become structurally impossible:

      * `X-Frame-Options: DENY`           — IPA クリックジャッキング §3.10
      * `X-Content-Type-Options: nosniff` — MIME sniffing
      * `Referrer-Policy: no-referrer`    — referer-leak
      * `Permissions-Policy: ...`         — opt-out of geolocation,
                                            camera, microphone

    Apps that need iframing should pass `frameOptions := none` and
    set a CSP `frame-ancestors` instead. -/
def Response.defaultSecurityHeaders (r : Response)
    (frameOptions : Option String := some "DENY") : Response :=
  let r1 := match frameOptions with
            | some v => r.setHeader! "x-frame-options" v
            | none   => r
  let r2 := r1.setHeader! "x-content-type-options" "nosniff"
  let r3 := r2.setHeader! "referrer-policy" "no-referrer"
  r3.setHeader! "permissions-policy" "geolocation=(), camera=(), microphone=()"

/-- 302 Found redirect. CR/LF in `location` are stripped because they
    would inject extra response headers — see
    `Response.setHeader` doc for the threat model. For an *audited*
    open-redirect guard (allow-list of trusted origins), use
    `LeanTea.Net.SafeRedirect` instead. -/
def Response.redirect (location : String) : Response :=
  let safe := location.replace "\r" "" |>.replace "\n" "" |>.replace "\u0000" ""
  {
    status := 302,
    headers := #[("location", safe), ("content-type", "text/plain; charset=utf-8")],
    body := "redirecting...\n".toUTF8
  }

/-- Attach a Set-Cookie header to a response. Default flags are
    `Path=/; HttpOnly; SameSite=Lax`. Pass `secure := true` for HTTPS
    deployments. CR/LF/NUL in `name` or `value` panic — cookies are
    headers, so a CRLF here is the same injection vector as
    `setHeader`. Validate `value` upstream when it carries user input. -/
def Response.withCookie (r : Response) (name value : String)
    (maxAgeSec : Nat := 86400 * 7) (secure := false) : Response :=
  let secureFlag := if secure then "; Secure" else ""
  let v := s!"{name}={value}; Path=/; HttpOnly; SameSite=Lax; Max-Age={maxAgeSec}{secureFlag}"
  r.setHeader! "set-cookie" v

/-- Set an expired cookie to delete it on the client. -/
def Response.clearCookie (r : Response) (name : String) (secure := false) : Response :=
  let secureFlag := if secure then "; Secure" else ""
  let v := s!"{name}=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0{secureFlag}"
  r.setHeader! "set-cookie" v

private def statusText : Nat → String
  | 200 => "OK"        | 201 => "Created"
  | 204 => "No Content" | 301 => "Moved Permanently"
  | 302 => "Found"     | 304 => "Not Modified"
  | 400 => "Bad Request" | 401 => "Unauthorized"
  | 403 => "Forbidden" | 404 => "Not Found"
  | 405 => "Method Not Allowed"
  | 500 => "Internal Server Error" | 503 => "Service Unavailable"
  | _   => "OK"

def Request.header? (r : Request) (name : String) : Option String :=
  r.headers.findSome? (fun (k, v) => if k == name.toLower then some v else none)

/-- Look up a cookie by name from the `Cookie` request header. -/
def Request.cookie? (r : Request) (name : String) : Option String := do
  let hdr ← r.header? "cookie"
  let pairs := hdr.splitOn ";"
  let pref := name ++ "="
  pairs.findSome? fun p =>
    let trimmed := (p.trimAscii).toString
    if trimmed.startsWith pref then some (trimmed.drop pref.length).toString
    else none

/-! ## Header lookup -/

def Request.contentLength (r : Request) : Nat :=
  (r.header? "content-length").bind (·.toNat?) |>.getD 0

/-! ## ByteArray utilities -/

def baFindSeq (ba : ByteArray) (needle : Array UInt8) (from_ : Nat := 0) : Option Nat := Id.run do
  let n := ba.size
  let m := needle.size
  if m == 0 then return some from_
  let mut i := from_
  while i + m ≤ n do
    let mut ok := true
    for j in [0:m] do
      if ba[(i + j)]! != needle[j]! then ok := false; break
    if ok then return some i
    i := i + 1
  return none

private def baSlice (ba : ByteArray) (start length : Nat) : ByteArray :=
  ba.extract start (start + length)

private def baToString (ba : ByteArray) : String :=
  match String.fromUTF8? ba with
  | some s => s
  | none   => ""

/-! ## Request parsing

We accumulate bytes from the socket until we see "\r\n\r\n" (end of
headers), then read `Content-Length` more bytes for the body. -/

def CRLFCRLF : Array UInt8 := #[13, 10, 13, 10]
def CRLF : Array UInt8 := #[13, 10]

/-- Split request bytes into the header section and the body that
    already arrived past the header terminator. -/
def splitHeaders (raw : ByteArray) : Option (String × ByteArray) := do
  let h ← baFindSeq raw CRLFCRLF
  let headersStr := baToString (baSlice raw 0 h)
  let rest := baSlice raw (h + 4) (raw.size - (h + 4))
  return (headersStr, rest)

private def parseRequestLine (line : String) : Option (String × String × String) := do
  let parts := line.splitOn " "
  match parts with
  | [m, target, _] =>
    -- split path and query
    match target.splitOn "?" with
    | [p]    => some (m, p, "")
    | [p, q] => some (m, p, q)
    | p :: rest => some (m, p, String.intercalate "?" rest)
    | _ => none
  | _ => none

private def parseHeaderLine (line : String) : Option (String × String) :=
  match line.splitOn ":" with
  | [] | [_] => none
  | k :: rest =>
    let v := (String.intercalate ":" rest).trim
    some (k.toLower, v)

/-- Parse the complete request given the raw bytes that include both
    headers and (the entirety of) the body. Returns `none` if the
    request is malformed. -/
def parseRequest (raw : ByteArray) (body : ByteArray) : Option Request := do
  let (headersStr, _earlyBody) ← splitHeaders raw
  let lines := headersStr.splitOn "\r\n"
  match lines with
  | [] => none
  | reqLine :: rest =>
    let (method, path, query) ← parseRequestLine reqLine
    let headers := rest.filterMap parseHeaderLine
    some {
      method := method,
      path := path,
      query := query,
      headers := headers.toArray,
      body := body
    }

/-! ## Response serialization -/

def Response.toBytes (r : Response) : ByteArray :=
  let status := s!"HTTP/1.1 {r.status} {statusText r.status}\r\n"
  let hdrs := r.headers.foldl
    (fun acc (k, v) => acc ++ s!"{k}: {v}\r\n") ""
  let cl := s!"content-length: {r.body.size}\r\n"
  let close := "connection: close\r\n\r\n"
  let head := (status ++ hdrs ++ cl ++ close).toUTF8
  head ++ r.body

/-! ## Handler type -/

abbrev Handler := Request → IO Response

end LeanTea.Net.Http
