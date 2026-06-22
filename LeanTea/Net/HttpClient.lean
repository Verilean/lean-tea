import Std.Async.TCP
import Std.Net

/-! # LeanTea.Net.HttpClient — minimal pure-Lean HTTP/1.1 client

Built on `Std.Internal.Async.TCP` so we don't have to shell out to
`curl` for the framework's own bread-and-butter traffic (LM Studio at
127.0.0.1, the browser-MCP HTTP transport, smoke tests). curl was a
fine prototype dep but isn't guaranteed to exist in every dev /
CI environment, and a 6-line process spawn is heavier than a TCP
socket for a 1-line GET / POST.

Scope on purpose:
* HTTP **only**. No TLS — adding OpenSSL FFI is a separate project.
  Anything HTTPS (Auth, public OpenAI, …) keeps using curl for now.
* HTTP/1.0-style `Connection: close`. Drains the socket to EOF.
  No keep-alive, no chunked decoding, no compression.
* Bodies up to a few MB (the typical vision payload). For multi-GB
  uploads write a streaming variant; for the framework's use this
  is plenty.

If you need HTTPS, the runtime still has `IO.Process.output { cmd :=
"curl", ... }` — wire callers explicitly to one or the other based
on the target URL's scheme. -/

namespace LeanTea.Net.HttpClient

open Std.Async
open Std.Net
open Std.Async.TCP

/-! ## Types -/

structure Response where
  status  : Nat
  /-- Header names are normalised to lowercase. -/
  headers : Array (String × String) := #[]
  body    : ByteArray := .empty
  deriving Inhabited

def Response.bodyText (r : Response) : String :=
  match String.fromUTF8? r.body with
  | some s => s
  | none   => ""

def Response.header? (r : Response) (name : String) : Option String :=
  r.headers.findSome? fun (k, v) =>
    if k == name.toLower then some v else none

/-! ## URL parsing — bare-bones, `http://` only. -/

structure Url where
  host : String
  port : UInt16
  path : String
  deriving Inhabited

/-- Parse an absolute HTTP URL: `http://host[:port]/path?query`.
    Returns `none` for anything else (HTTPS, opaque, etc.) so the
    caller can fall back to a TLS-capable transport. -/
def parseUrl (raw : String) : Option Url := do
  let rest ← if raw.startsWith "http://" then some (raw.drop 7) else none
  let rest := rest.toString
  let (hostPort, path) :=
    match rest.splitOn "/" with
    | h :: rest => (h, "/" ++ String.intercalate "/" rest)
    | []        => (rest, "/")
  let (host, port) :=
    match hostPort.splitOn ":" with
    | [h]    => (h, (80 : UInt16))
    | [h, p] => (h, (p.toNat?.getD 80).toUInt16)
    | _      => (hostPort, (80 : UInt16))
  return { host, port, path }

/-! ## Address — accept literal IPv4 plus the common `localhost`. -/

private def parseIPv4 (s : String) : IPv4Addr :=
  let zero : IPv4Addr := ⟨#v[0, 0, 0, 0]⟩
  match (s.splitOn ".").map (·.toNat?.getD 0) with
  | [a, b, c, d] => ⟨#v[a.toUInt8, b.toUInt8, c.toUInt8, d.toUInt8]⟩
  | _ => zero

private def resolveHost (host : String) : IPv4Addr :=
  if host == "localhost" then parseIPv4 "127.0.0.1" else parseIPv4 host

/-! ## Wire I/O -/

private def connect (url : Url) : IO Socket.Client := do
  let socket ← Socket.Client.mk
  let addr : SocketAddress := .v4 { addr := resolveHost url.host, port := url.port }
  (socket.connect addr).block
  return socket

private def send (s : Socket.Client) (bs : ByteArray) : IO Unit :=
  (s.send bs).block

private partial def drain (s : Socket.Client) (acc : ByteArray := .empty) : IO ByteArray := do
  /- Read until the peer closes — we always send `Connection: close`
     so the server signals end-of-body by hangup. -/
  match (← (s.recv? 65536).block) with
  | none       => return acc
  | some chunk => drain s (acc ++ chunk)

/-- Find `\r\n\r\n` in the buffer; returns position of CRLF start. -/
private partial def findHeaderEnd (raw : ByteArray) (i : Nat) : Option Nat :=
  let needle : ByteArray := "\r\n\r\n".toUTF8
  if i + needle.size > raw.size then none
  else if raw.extract i (i + needle.size) == needle then some i
  else findHeaderEnd raw (i + 1)

/-- Look up `content-length` (case-insensitive) in the head bytes. -/
private def contentLengthOfHead (head : ByteArray) : Option Nat := Id.run do
  let s := match String.fromUTF8? head with | some s => s | none => ""
  let lines := s.splitOn "\r\n"
  for line in lines do
    match line.splitOn ":" with
    | k :: rest =>
      if k.trimAscii.toString.toLower == "content-length" then
        return (String.intercalate ":" rest).trimAscii.toString.toNat?
    | _ => continue
  return none

/-- Read response, honouring `Content-Length` so we don't hang on
    servers that ignore `Connection: close` (Chrome's CDP `/json`). -/
private partial def drainWithLength (s : Socket.Client) (acc : ByteArray) : IO ByteArray := do
  match findHeaderEnd acc 0 with
  | none =>
    match (← (s.recv? 65536).block) with
    | none       => return acc
    | some chunk => drainWithLength s (acc ++ chunk)
  | some hdrEnd =>
    let bodyStart := hdrEnd + 4
    let head := acc.extract 0 hdrEnd
    match contentLengthOfHead head with
    | none =>
      /- No Content-Length: fall back to EOF semantics. -/
      drain s acc
    | some clen =>
      let needed := bodyStart + clen
      let rec readUntil (buf : ByteArray) : IO ByteArray := do
        if buf.size >= needed then return buf
        match (← (s.recv? 65536).block) with
        | none       => return buf
        | some chunk => readUntil (buf ++ chunk)
      readUntil acc

/-! ## Response parsing -/

/-- Split a `\r\n\r\n`-terminated head off the wire-format response. -/
private partial def scanCrlfCrlf (raw : ByteArray) (needle : ByteArray) (i : Nat) : Nat :=
  if i + needle.size > raw.size then raw.size
  else if raw.extract i (i + needle.size) == needle then i
  else scanCrlfCrlf raw needle (i + 1)

private def splitHeadBody (raw : ByteArray) : ByteArray × ByteArray :=
  let needle : ByteArray := "\r\n\r\n".toUTF8
  let cut := scanCrlfCrlf raw needle 0
  if cut == raw.size then (raw, .empty)
  else (raw.extract 0 cut, raw.extract (cut + needle.size) raw.size)

private def parseHead (head : String) : Nat × Array (String × String) :=
  let lines := head.splitOn "\r\n"
  let status :=
    match lines with
    | s :: _ =>
      /- `HTTP/1.1 200 OK` → `200` -/
      match s.splitOn " " with
      | _ :: code :: _ => code.toNat?.getD 0
      | _              => 0
    | []     => 0
  let headers := lines.tail.filterMap fun line =>
    match line.splitOn ":" with
    | k :: rest =>
      let v := String.intercalate ":" rest
      some (k.trimAscii.toString.toLower, v.trimAscii.toString)
    | _         => none
  (status, headers.toArray)

/-! ## High-level request -/

/-- Send one request and return the parsed response. Closes the
    connection. Headers default to `Connection: close` + a
    `Host:` header derived from the URL; callers can add more. -/
def request (method : String) (url : Url)
    (body : ByteArray := .empty)
    (headers : Array (String × String) := #[]) : IO Response := do
  let s ← connect url
  /- Compose request bytes. We pre-set `Host:` and `Connection:` plus
     `Content-Length:` when there's a body; the caller's headers are
     appended afterwards so they can override anything. -/
  let mut head : String :=
    s!"{method} {url.path} HTTP/1.1\r\n" ++
    s!"Host: {url.host}:{url.port}\r\n" ++
    "Connection: close\r\n"
  if body.size > 0 then
    head := head ++ s!"Content-Length: {body.size}\r\n"
  for (k, v) in headers do
    head := head ++ s!"{k}: {v}\r\n"
  head := head ++ "\r\n"
  let reqBytes := head.toUTF8 ++ body
  send s reqBytes
  let raw ← drainWithLength s .empty
  (s.shutdown).block
  let (headBs, bodyBs) := splitHeadBody raw
  let headStr := match String.fromUTF8? headBs with | some s => s | none => ""
  let (status, hdrs) := parseHead headStr
  return { status, headers := hdrs, body := bodyBs }

/-- POST JSON convenience: serialise → request → response. The body
    is sent as `application/json` with `Content-Length` filled in
    automatically. -/
def postJson (url : Url) (body : ByteArray)
    (extraHeaders : Array (String × String) := #[]) : IO Response := do
  let headers := #[("Content-Type", "application/json")] ++ extraHeaders
  request "POST" url body headers

/-- Same but takes the URL as a raw string for the common case. -/
def postJsonUrl (rawUrl : String) (body : ByteArray)
    (extraHeaders : Array (String × String) := #[]) : IO Response := do
  match parseUrl rawUrl with
  | none     => throw <| IO.userError s!"HttpClient: not an http:// URL: {rawUrl}"
  | some url => postJson url body extraHeaders

/-- Drop-in `IO.Process.output { cmd := "curl", … }` replacement for
    JSON-in / JSON-out endpoints. Returns the body as text. -/
def postJsonText (rawUrl : String) (jsonBody : String)
    (extraHeaders : Array (String × String) := #[]) : IO String := do
  let resp ← postJsonUrl rawUrl jsonBody.toUTF8 extraHeaders
  if resp.status >= 400 then
    throw <| IO.userError s!"HttpClient: {rawUrl} returned {resp.status}\n{resp.bodyText}"
  return resp.bodyText

end LeanTea.Net.HttpClient
