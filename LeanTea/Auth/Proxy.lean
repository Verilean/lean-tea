import LeanTea.Auth.OAuth2
import LeanTea.Net.HttpClient
import LeanTea.Net.Server

/-! # LeanTea.Auth.Proxy — a sign-in proxy in front of a local HTTP service

`OAuth2.gate` in front of a handler that forwards each request to an
upstream over plain HTTP (`LeanTea.Net.HttpClient`) and returns its
status, headers and body. The `auth_proxy` executable
(`examples/AuthProxy/Main.lean`) is this plus argument parsing.

What reaches the upstream:

* the method, path, query and body (up to `maxBody` bytes; larger is
  refused with 413);
* the client's headers, except hop-by-hop ones, `Host` and
  `Content-Length` (both set again), and **every** `X-Forwarded-*`,
  `Forwarded` and `X-Real-Ip` the client sent;
* the client's cookies minus the gate's own (`session`, `session_login`);
* `X-Forwarded-User` (the login), `X-Forwarded-Email`,
  `X-Forwarded-Preferred-Username` (the login) and `X-Forwarded-Host`,
  set by the proxy.

So the upstream may trust `X-Forwarded-User` as long as only the proxy
can reach it (bind it to 127.0.0.1, or put it on a Unix socket only the
proxy and its owner can open).

Two ways to run it:

* `handler` — an ordinary `Handler` with one TCP upstream. The
  upstream's answer is read whole, then sent; no WebSockets.
* `takeover` — a `Server.Takeover` with a routing table (`Config`):
  each signed-in login, and each path prefix, goes to its own upstream,
  TCP or Unix socket. Responses are relayed as they arrive, and a
  WebSocket upgrade becomes a byte tunnel. Logins and paths the table
  does not route get 403. `auth_proxy` runs this.

Serve `takeover` with `Server.serveThreads` or `serveUnix`, not the
libuv servers: on a libuv connection a send to a client that stopped
reading cannot be interrupted, so such a client would hold the relay
and its upstream connection until it reads or goes away. On descriptors
a send left unread for `sendTimeoutMs` fails, and the end of either
direction closes both. -/

namespace LeanTea.Auth.Proxy

open LeanTea.Net.Http
open LeanTea.Auth.OAuth2

structure Upstream where
  host    : String := "127.0.0.1"
  port    : UInt16
  /-- Largest request body passed on. -/
  maxBody : Nat := 1024 * 1024

private def hopByHop : List String :=
  ["connection", "keep-alive", "proxy-authenticate", "proxy-authorization",
   "te", "trailer", "transfer-encoding", "upgrade"]

/-- A client header the proxy does not pass on. Names are compared with
    `_` read as `-`: CGI-style upstreams turn both `X-Forwarded-User`
    and `X-Forwarded_User` into `HTTP_X_FORWARDED_USER`, so the
    underscore spelling would otherwise smuggle an identity through. -/
def dropsRequestHeader (name : String) : Bool :=
  let n := name.toLower.map fun c => if c == '_' then '-' else c
  hopByHop.contains n || n == "host" || n == "content-length" || n == "cookie"
    || n.startsWith "x-forwarded-" || n == "forwarded" || n == "x-real-ip"

/-- The `Cookie` header without the gate's own cookies, if anything is left. -/
def cookiesFor (g : GateConfig) (header : String) : Option String :=
  let own := [g.cookieName, g.cookieName ++ "_login"]
  let kept := (header.splitOn ";").map (·.trimAscii.toString) |>.filter fun c =>
    !c.isEmpty && !own.any fun n => c.startsWith (n ++ "=")
  if kept.isEmpty then none else some (String.intercalate "; " kept)

/-- The headers sent upstream for `req`, made by `user` (`none` on a
    public path without a session). -/
def upstreamHeaders (g : GateConfig) (user : Option User) (req : Request)
    : Array (String × String) :=
  let passed := req.headers.filter fun (k, _) => !dropsRequestHeader k
  let cookie := (req.header? "cookie").bind (cookiesFor g)
  let who := match user with
    | some u => #[("X-Forwarded-User", u.login), ("X-Forwarded-Email", u.email),
                  ("X-Forwarded-Preferred-Username", u.login)]
    | none => #[]
  passed
    ++ (cookie.map (fun c => #[("Cookie", c)])).getD #[]
    ++ who
    ++ ((req.header? "host").map (fun h => #[("X-Forwarded-Host", h)])).getD #[]

/-- Forward one request upstream; 502 when the upstream cannot be reached. -/
def forward (up : Upstream) (g : GateConfig) (user : Option User) (req : Request) : IO Response := do
  if req.body.size > up.maxBody then
    return Response.text 413 s!"request body over {up.maxBody} bytes\n"
  let path := if req.query.isEmpty then req.path else req.path ++ "?" ++ req.query
  try
    let r ← LeanTea.Net.HttpClient.request req.method
      { host := up.host, port := up.port, path } req.body (upstreamHeaders g user req)
    let chunked := r.headers.any fun (k, v) =>
      k.toLower == "transfer-encoding" && v.toLower.trimAscii.toString == "chunked"
    let body := if chunked then LeanTea.Net.HttpClient.dechunk r.body else r.body
    -- `content-length` and the connection state are set again by the server
    let headers := r.headers.filter fun (k, _) =>
      let n := k.toLower
      !hopByHop.contains n && n != "content-length"
    return { status := r.status, headers, body }
  catch e =>
    IO.eprintln s!"auth proxy: upstream {up.host}:{up.port}: {e}"
    return Response.text 502 "upstream unavailable\n"

/-- The proxy: sign-in through `gate`, then `forward`. Public paths are
    forwarded without a user (and without `X-Forwarded-User`). -/
def handler (up : Upstream) (g : GateConfig) (sessions : Sessions) : Handler :=
  OAuth2.gate g sessions (forward up g)

/-! ## Routing table, streamed relay and WebSocket tunnel -/

/-- Where a request goes. -/
inductive Target where
  | tcp (host : String) (port : UInt16)
  | unix (path : String)
  /-- Refuse with 403. -/
  | deny
  deriving Inhabited, Repr, BEq

/-- `unix:/abs/path`, `HOST:PORT` (an IPv4 address or `localhost`; an
    `http://` in front is allowed) or `deny`. -/
def Target.parse? (s : String) : Option Target :=
  if s == "deny" then some .deny
  else if s.startsWith "unix:" then
    let path := (s.drop 5).toString
    if path.startsWith "/" then some (.unix path) else none
  else
    let hp := if s.startsWith "http://" then (s.drop 7).toString else s
    let hp := if hp.endsWith "/" then (hp.dropEnd 1).toString else hp
    match hp.splitOn ":" with
    | [h, p] =>
      match LeanTea.Net.Server.parseIPv4? h, p.toNat? with
      | some _, some n => if 0 < n && n < 65536 then some (.tcp h n.toUInt16) else none
      | _, _ => none
    | _ => none

def Target.describe : Target → String
  | .tcp h p => s!"{h}:{p}"
  | .unix p => s!"unix:{p}"
  | .deny => "deny"

/-- Open a connection to the target (not `deny`), as a descriptor so a
    blocked send can be ended. -/
def Target.connect : Target → IO LeanTea.Net.Transport
  | .tcp h p => LeanTea.Net.Transport.connectTcp (if h == "localhost" then "127.0.0.1" else h) p
  | .unix path => LeanTea.Net.Transport.connectUnix path
  | .deny => throw (IO.userError "deny is not an upstream")

/-- The `Host` sent upstream. -/
def Target.hostHeader : Target → String
  | .tcp h p => s!"{h}:{p}"
  | _ => "localhost"

/-- One line of the table: requests under `pathPrefix` by the logins
    `login` names go to `target`.

    `login` is one login, several separated by commas (`alice,bob`), `*`
    for whoever got past the sign-in (signed in, or on one of the gate's
    public paths), or `-` for everyone, signed in or not: such a route
    never asks to sign in, and still gets `X-Forwarded-User` when there is
    a session. With `strip`, the upstream gets the path without the
    prefix (`/manager/x` → `/x`) and the prefix in `X-Forwarded-Prefix`. -/
structure Route where
  login      : String
  pathPrefix : String
  target     : Target
  strip      : Bool := false
  deriving Inhabited, Repr

/-- Does the route's `login` field let `login` (`none`: no session) in? -/
def Route.admits (r : Route) (login : Option String) : Bool :=
  r.login == "-" || r.login == "*" ||
    login.any fun l => (r.login.splitOn ",").any (·.trimAscii.toString.toLower == l.toLower)

/-- The logins a table names (not `*` or `-`), for the sign-in allow list. -/
def namedLogins (routes : List Route) : List String :=
  (routes.flatMap fun r => (r.login.splitOn ",").map (·.trimAscii.toString))
    |>.filter (fun l => !l.isEmpty && l != "*" && l != "-") |>.eraseDups

/-- Parse a table: one route per line, `LOGIN PATH-PREFIX TARGET [strip]`
    separated by spaces; `#` starts a comment. -/
def parseRoutes (text : String) : Except String (List Route) := do
  let mut out := #[]
  let mut n := 0
  for raw in text.splitOn "\n" do
    n := n + 1
    let line := ((raw.splitOn "#").headD "").trimAscii.toString
    if line.isEmpty then continue
    let ws := (line.split (fun c => c == ' ' || c == '\t')).toList.map (·.toString) |>.filter (!·.isEmpty)
    let (login, pre, tgt, opts) ← match ws with
      | [l, p, t] => pure (l, p, t, [])
      | l :: p :: t :: o => pure (l, p, t, o)
      | _ => throw s!"line {n}: expected LOGIN PATH-PREFIX TARGET [strip]"
    unless pre.startsWith "/" do throw s!"line {n}: the path prefix must start with /: {pre}"
    let some t := Target.parse? tgt
      | throw s!"line {n}: expected unix:/path, HOST:PORT or deny, got {tgt}"
    for o in opts do
      unless o == "strip" do throw s!"line {n}: unknown option {o} (only strip)"
    if (login.splitOn ",").any (fun l => l.trimAscii.isEmpty) then
      throw s!"line {n}: empty login in {login}"
    out := out.push { login, pathPrefix := pre, target := t, strip := opts.contains "strip" }
  return out.toList

/-- A path prefix without its trailing slashes (`/mcp/` is `/mcp`), so
    the same prefix is the same length whichever way it is written. -/
def normPrefix (pre : String) : String := Id.run do
  let mut p := pre
  while p.length > 1 && p.endsWith "/" do
    p := (p.dropEnd 1).toString
  return p

/-- Does `pre` cover `path`, segment-wise (`/stream` covers `/stream` and
    `/stream/x`, not `/streams`)? -/
def prefixCovers (pre path : String) : Bool :=
  let p := normPrefix pre
  p == "/" || path == p || path.startsWith (p ++ "/")

/-- The target for `login` (`none`: not signed in) at the canonical
    `path`: the longest covering prefix among the login's own lines and
    the `*` lines; on a tie, `deny` wins. `none` when nothing covers it. -/
def pickRoute (routes : List Route) (login : Option String) (path : String) : Option Route :=
  let mine := routes.filter fun r => r.admits login && prefixCovers r.pathPrefix path
  -- Compared as normalised, as `prefixCovers` matches them.
  let len (r : Route) := (normPrefix r.pathPrefix).length
  let best := mine.foldl (fun (acc : Option Route) r =>
    match acc with
    | none => some r
    | some a =>
      if len r > len a || (len r == len a && r.target == .deny) then some r else acc) none
  best

/-- `pickRoute`'s target. -/
def pick (routes : List Route) (login : Option String) (path : String) : Option Target :=
  (pickRoute routes login path).map (·.target)

/-- `path` (as received, canonical) without the first segments that make
    up `pre`: `/manager/x` under `/manager` is `/x`, and `/manager` is `/`. -/
def stripPrefix (pre path : String) : String :=
  let n := ((normPrefix pre).splitOn "/").filter (!·.isEmpty) |>.length
  let rest := ((path.splitOn "/").drop (n + 1))
  "/" ++ String.intercalate "/" rest

/-- A `Location` the upstream sent, with the stripped prefix put back
    when it is a path on this host (`/login` → `/manager/login`). -/
def relocate (pre loc : String) : String :=
  if loc.startsWith "/" && !loc.startsWith "//" && !loc.startsWith "/\\" then
    normPrefix pre ++ loc
  else loc

structure Config where
  routes        : List Route
  /-- Largest request body passed on. -/
  maxBody       : Nat := 1024 * 1024
  /-- Origins (`https://host[:port]`) allowed to open WebSockets and to
      send state-changing requests. Empty: the request's own origin
      (`Host`, and `https://` when the gate's cookies are `Secure`). -/
  origins       : List String := []
  /-- How long the upstream may take to send its response head. -/
  headTimeoutMs : Nat := 30000
  /-- How long either side may leave the relay's data unread before the
      relay gives up on it (0: forever). -/
  sendTimeoutMs : Nat := 60000

/-- Is the request's `Origin` (if any) allowed? -/
def originOk (cfg : Config) (g : GateConfig) (req : Request) : Bool :=
  match req.header? "origin" with
  | none => true
  | some o =>
    if !cfg.origins.isEmpty then cfg.origins.contains o
    else
      let auth := LeanTea.Net.WebSocketServer.originAuthority o
      (!g.cookieSecure || o.toLower.startsWith "https://") && !auth.isEmpty &&
        auth == ((req.header? "host").getD "").toLower

private def unsafeMethod (m : String) : Bool :=
  !(m == "GET" || m == "HEAD" || m == "OPTIONS")

/-- The upstream's response head: status line and header lines, and the
    bytes read past it. -/
private structure Head where
  statusLine : String
  status  : Nat
  lines   : List String
  rest    : ByteArray

private def maxHeadBytes : Nat := 64 * 1024

private partial def readHead (up : LeanTea.Net.Transport) (deadline : Nat) (acc : ByteArray)
    : IO (Option Head) := do
  match baFindSeq acc CRLFCRLF with
  | some h =>
    let some text := String.fromUTF8? (acc.extract 0 h) | return none
    match text.splitOn "\r\n" with
    | statusLine :: lines =>
      unless statusLine.startsWith "HTTP/1." do return none
      let some code := ((statusLine.splitOn " ").drop 1).head?.bind (·.toNat?) | return none
      return some { statusLine, status := code, lines, rest := acc.extract (h + 4) acc.size }
    | [] => return none
  | none =>
    if acc.size > maxHeadBytes then return none
    match ← up.recv deadline with
    | some b => readHead up deadline (acc ++ b)
    | none => return none

/-- Pass interim (1xx) responses on to `client` and return the final
    head; `101` is final when the request asked for an upgrade. A 1xx is
    not the answer: the final one follows on the same connection,
    possibly in the bytes already read. -/
private partial def finalHead (up client : LeanTea.Net.Transport) (upgrade : Bool) (deadline : Nat)
    (h : Head) : IO (Option Head) := do
  if h.status < 100 || h.status ≥ 200 || (upgrade && h.status == 101) then return some h
  if h.status == 101 then return none  -- switching protocols that were not asked for
  let name (l : String) := ((l.splitOn ":").headD "").trimAscii.toString.toLower
  let kept := h.lines.filter fun l => !["connection", "keep-alive"].contains (name l)
  client.send (h.statusLine ++ "\r\n" ++ String.join (kept.map (· ++ "\r\n")) ++ "\r\n").toUTF8
  match ← readHead up deadline h.rest with
  | some next => finalHead up client upgrade deadline next
  | none => return none

/-- Copy from `src` to `dst` until `src` ends (or `limit` bytes, when
    given). Stops quietly when `dst` goes away. -/
private partial def pump (src dst : LeanTea.Net.Transport) (limit : Option Nat) : IO Unit := do
  if limit == some 0 then return
  match ← src.recv 0 with
  | none => return
  | some b =>
    let b := match limit with
      | some n => if b.size > n then b.extract 0 n else b
      | none => b
    let ok ← try dst.send b; pure true catch _ => pure false
    if ok then pump src dst (limit.map (· - b.size))

private def answer (client : LeanTea.Net.Transport) (r : Response) : IO Unit :=
  client.send (if r.headers.any (·.1.toLower == "connection") then r
               else { r with headers := r.headers.push ("Connection", "close") }).toBytes

/-- Send `req` to `target` for `user` and relay the answer to `client`:
    a response as it arrives, or, after a `101`, the connection both ways
    until either side closes. -/
def relay (cfg : Config) (g : GateConfig) (user : Option User) (target : Target) (req : Request)
    (client : LeanTea.Net.Transport) (leftover : ByteArray) (strip : Option String := none)
    : IO Unit := do
  if req.body.size > cfg.maxBody then
    return ← answer client (Response.text 413 s!"request body over {cfg.maxBody} bytes\n")
  let upgrade := LeanTea.Net.WebSocketServer.isUpgrade req
  let headers := upstreamHeaders g user req ++
    ((strip.map fun p => #[("X-Forwarded-Prefix", normPrefix p)]).getD #[])
  -- Client headers were parsed line by line, but the identity comes from
  -- the provider: a line break in it would add headers of its choosing.
  if headers.any (fun (k, v) => (k ++ v).any fun c => c == '\r' || c == '\n') then
    return ← answer client (Response.text 500 "header with a line break\n")
  let up ← try target.connect catch e =>
    IO.eprintln s!"auth proxy: upstream {target.describe}: {e}"
    return ← answer client (Response.text 502 "upstream unavailable\n")
  client.setSendTimeout cfg.sendTimeoutMs
  up.setSendTimeout cfg.sendTimeoutMs
  try
    let base := match strip with
      | some p => stripPrefix p req.path
      | none => req.path
    let path := if req.query.isEmpty then base else base ++ "?" ++ req.query
    let mut head := s!"{req.method} {path} HTTP/1.1\r\nHost: {target.hostHeader}\r\n"
    for (k, v) in headers do
      head := head ++ s!"{k}: {v}\r\n"
    head := head ++ (if upgrade then "Upgrade: websocket\r\nConnection: Upgrade\r\n"
                     else "Connection: close\r\n")
    if req.body.size > 0 || unsafeMethod req.method then
      head := head ++ s!"Content-Length: {req.body.size}\r\n"
    up.send ((head ++ "\r\n").toUTF8 ++ req.body)
    let deadline := (← IO.monoMsNow) + cfg.headTimeoutMs
    let some first ← readHead up deadline .empty
      | IO.eprintln s!"auth proxy: upstream {target.describe}: no response head"
        return ← answer client (Response.text 502 "upstream sent no response\n")
    -- After an interim response the client has already seen a status
    -- line, so a broken final one can only end the connection.
    let some h ← finalHead up client upgrade deadline first
      | IO.eprintln s!"auth proxy: upstream {target.describe}: no final response"
        if first.status ≥ 100 && first.status < 200 && !(upgrade && first.status == 101) then return
        return ← answer client (Response.text 502 "upstream sent no response\n")
    let name (l : String) := ((l.splitOn ":").headD "").trimAscii.toString.toLower
    if upgrade && h.status == 101 then
      -- The upgrade answer passes as is; from here on it is a tunnel.
      client.send (("HTTP/1.1 101 Switching Protocols\r\n" ++
        String.join (h.lines.map (· ++ "\r\n")) ++ "\r\n").toUTF8 ++ h.rest)
      if leftover.size > 0 then up.send leftover
      -- Whichever direction ends first (end of stream, an error, or a
      -- send the other side left unread past the timeout) closes both
      -- connections, which wakes the other direction whether it waits
      -- to receive or to send.
      let back ← IO.asTask (prio := .dedicated) do
        pump client up none
        up.close
        client.close
      pump up client none
      up.close
      client.close
      let _ ← IO.wait back
    else
      let kept := h.lines.filter fun l => !["connection", "keep-alive"].contains (name l)
      let kept := match strip with
        | some p => kept.map fun l =>
          if name l == "location" then
            "Location: " ++ relocate p (((l.splitOn ":").drop 1 |> String.intercalate ":").trimAscii.toString)
          else l
        | none => kept
      client.send (h.statusLine ++ "\r\n" ++
        String.join (kept.map (· ++ "\r\n")) ++ "Connection: close\r\n\r\n").toUTF8
      let length := h.lines.findSome? fun l =>
        if name l == "content-length" then ((l.splitOn ":").getD 1 "").trimAscii.toString.toNat?
        else none
      let noBody := req.method == "HEAD" || h.status == 204 || h.status == 304
      let limit := if noBody then some 0 else length
      let first := match limit with
        | some n => h.rest.extract 0 n
        | none => h.rest
      if first.size > 0 then client.send first
      pump up client (limit.map (· - first.size))
  finally
    up.close

/-- The proxy as a `Server.Takeover`: the gate's own routes and refusals,
    then `relay` to the upstream `pick` chooses (403 when it chooses none
    or `deny`; 400 for a path that is not canonical, so `..` or encoded
    slashes cannot reach a route the table did not mean). WebSocket
    upgrades and state-changing requests from another origin get 403. -/
def takeover (cfg : Config) (g : GateConfig) (sessions : Sessions)
    : LeanTea.Net.Server.Takeover := fun req => do
  let respond (r : Response) := some fun (c : LeanTea.Net.Transport) (_ : ByteArray) => answer c r
  let checked ← gateCheck g sessions req
  -- The gate's own routes (sign in, callback, sign out) stay the gate's.
  if req.path == g.loginPath || req.path == g.callbackPath || req.path == g.logoutPath then
    if let .error r := checked then return respond r
  -- `.error` here means no session on a path that needs one; a `-` route
  -- does not, so the gate's answer (a redirect or 401) is kept for later.
  let user := match checked with | .ok u => u | .error _ => none
  let some path := canonicalPath? req.path
    | return respond (Response.text 400 "bad path\n")
  match pickRoute cfg.routes (user.map (·.login)) path, checked with
  | some r, _ =>
    if r.target == .deny then return respond (Response.text 403 "not allowed here\n")
    if r.login != "-" then
      if let .error ans := checked then return respond ans
    if (LeanTea.Net.WebSocketServer.isUpgrade req || unsafeMethod req.method) && !originOk cfg g req then
      return respond (Response.text 403 "cross-origin request refused\n")
    let strip := if r.strip && normPrefix r.pathPrefix != "/" then some r.pathPrefix else none
    return some fun c leftover => relay cfg g user r.target req c leftover strip
  | none, .error ans => return respond ans  -- signing in may open a route
  | none, .ok _ => return respond (Response.text 403 "not allowed here\n")

end LeanTea.Auth.Proxy
