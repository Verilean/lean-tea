import LeanTea.Net.HttpClient

/-! # LeanTea.Net.TlsClient — HTTPS/1.1 client over OpenSSL

The plaintext `HttpClient` can't do TLS, so HTTPS traffic (public OpenAI,
Google, GitHub, …) used to shell out to `curl`. This drives the *same*
HTTP/1.1 request build + response parse (shared from `HttpClient`) over an
OpenSSL socket — the thin C transport lives in `c/leantea_tls.c`.

Same semantics as the plaintext client: `Connection: close`, a
Content-Length-aware drain, and the `HttpClient.Response` type.

**Build:** the C shim is opt-in via `LEANTEA_TLS=1` and needs
`-lssl -lcrypto` in the consuming exe's `weakLinkArgs`. Without the flag,
the externs raise an IO error explaining how to enable it. Certificate
verification is on by default; set `LEANTEA_TLS_INSECURE=1` to skip it.

**Timeouts:** `request` gives up after `timeoutMs` (default 10 s): the
connect and every read and write wait at most that long, and the whole
request may not take longer either. The error message ends in
"timed out". Name resolution is not bounded. -/

namespace LeanTea.Net.TlsClient

open LeanTea.Net.HttpClient (Response buildRequest parseResponse drainResponse)

/-- Opaque handle to a live TLS connection. Backed by a `tls_conn*`
    (SSL* + SSL_CTX* + fd) with a finalizer that shuts down and frees it. -/
opaque ConnPointed : NonemptyType
def Conn : Type := ConnPointed.type
instance : Nonempty Conn := ConnPointed.property

/-- TCP connect + TLS handshake. `timeoutMs` bounds the connect and each
    later read / write (0 means the 10 s default). -/
@[extern "leantea_tls_connect"]
opaque connect (host : @& String) (port : UInt16) (timeoutMs : UInt32 := 10000) : IO Conn

@[extern "leantea_tls_send"]
opaque send (c : @& Conn) (bytes : @& ByteArray) : IO Unit

@[extern "leantea_tls_recv"]
opaque recvRaw (c : @& Conn) (max : UInt32) : IO ByteArray

@[extern "leantea_tls_close"]
opaque close (c : @& Conn) : IO Unit

/-- `recv` adapted to the drain contract: an empty read means EOF. Past
    `deadline` (monotonic ms) it fails, so a peer that trickles bytes
    cannot hold the caller longer than the request's timeout. -/
private def recvChunk (c : Conn) (deadline : Nat) : IO (Option ByteArray) := do
  if (← IO.monoMsNow) > deadline then
    throw <| IO.userError "TlsClient: response timed out"
  let bs ← recvRaw c 65536
  return if bs.size == 0 then none else some bs

/-! ## URL — `https://host[:port]/path`. -/

structure Url where
  host : String
  port : UInt16
  path : String
  deriving Inhabited

def parseUrl (raw : String) : Option Url := do
  let rest ← if raw.startsWith "https://" then some (raw.drop 8) else none
  let rest := rest.toString
  let (hostPort, path) :=
    match rest.splitOn "/" with
    | h :: r => (h, "/" ++ String.intercalate "/" r)
    | []     => (rest, "/")
  let (host, port) :=
    match hostPort.splitOn ":" with
    | [h]    => (h, (443 : UInt16))
    | [h, p] => (h, (p.toNat?.getD 443).toUInt16)
    | _      => (hostPort, (443 : UInt16))
  return { host, port, path }

/-! ## Request -/

/-- One HTTPS request → parsed `Response` (handshake, `Connection: close`,
    Content-Length-aware drain, then close), within `timeoutMs`. -/
def request (method : String) (url : Url)
    (body : ByteArray := .empty)
    (headers : Array (String × String) := #[])
    (timeoutMs : Nat := 10000) : IO Response := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  let c ← connect url.host url.port timeoutMs.toUInt32
  let hostHdr := if url.port == 443 then url.host else s!"{url.host}:{url.port}"
  send c (buildRequest method hostHdr url.path body headers)
  let raw ← drainResponse (recvChunk c deadline)
  close c
  return parseResponse raw

def requestUrl (method rawUrl : String)
    (body : ByteArray := .empty)
    (headers : Array (String × String) := #[])
    (timeoutMs : Nat := 10000) : IO Response := do
  match parseUrl rawUrl with
  | none     => throw <| IO.userError s!"TlsClient: not an https:// URL: {rawUrl}"
  | some url => request method url body headers timeoutMs

/-- GET the body as text; throws on status ≥ 400. -/
def getText (rawUrl : String) (headers : Array (String × String) := #[]) : IO String := do
  let r ← requestUrl "GET" rawUrl .empty headers
  if r.status ≥ 400 then throw <| IO.userError s!"TlsClient GET {rawUrl}: HTTP {r.status}"
  return r.bodyText

/-- POST a JSON body as `application/json`; returns the response body text. -/
def postJsonText (rawUrl jsonBody : String)
    (headers : Array (String × String) := #[]) : IO String := do
  let hs := #[("Content-Type", "application/json")] ++ headers
  let r ← requestUrl "POST" rawUrl jsonBody.toUTF8 hs
  if r.status ≥ 400 then throw <| IO.userError s!"TlsClient POST {rawUrl}: HTTP {r.status}\n{r.bodyText}"
  return r.bodyText

end LeanTea.Net.TlsClient

namespace LeanTea.Net

/-- Fetch a URL's body as text, choosing the transport by scheme:
    `https://` → OpenSSL TLS client, `http://` → plaintext client. -/
def fetchText (rawUrl : String) (headers : Array (String × String) := #[]) : IO String := do
  if rawUrl.startsWith "https://" then
    LeanTea.Net.TlsClient.getText rawUrl headers
  else match LeanTea.Net.HttpClient.parseUrl rawUrl with
    | some url =>
      let r ← LeanTea.Net.HttpClient.request "GET" url .empty headers
      if r.status ≥ 400 then throw <| IO.userError s!"fetchText {rawUrl}: HTTP {r.status}"
      return r.bodyText
    | none => throw <| IO.userError s!"fetchText: unsupported URL: {rawUrl}"

/-- POST a JSON body, choosing transport by scheme — the drop-in
    replacement for the framework's curl-based JSON POSTs. -/
def postJsonText (rawUrl jsonBody : String)
    (headers : Array (String × String) := #[]) : IO String := do
  if rawUrl.startsWith "https://" then
    LeanTea.Net.TlsClient.postJsonText rawUrl jsonBody headers
  else
    LeanTea.Net.HttpClient.postJsonText rawUrl jsonBody headers

end LeanTea.Net
