import LeanTea.Auth.OAuth2
import LeanTea.Net.HttpClient

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
can reach it (bind it to 127.0.0.1). WebSockets and streamed responses
are not supported: the upstream's answer is read whole, then sent. -/

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

/-- A client header the proxy does not pass on. -/
def dropsRequestHeader (name : String) : Bool :=
  let n := name.toLower
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

end LeanTea.Auth.Proxy
