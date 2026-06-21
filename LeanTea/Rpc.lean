import LeanTea.Net.Http
import LeanTea.Js

/-! # LeanTea.Rpc — Servant-flavoured typed RPC

One `Endpoint` declaration drives both sides of the wire. -/

namespace LeanTea.Rpc

open LeanTea.Net.Http
open LeanTea.Js (Block Stmt Expr)

/-- Where the parameters of a request live on the wire. -/
inductive Carrier
  | query   -- `?k1=v1&k2=v2`
  | form    -- `application/x-www-form-urlencoded` body
  deriving BEq, Repr, Inhabited

/-- How the response should be shaped on the client. -/
inductive OutputKind
  | text    -- `await r.text()`
  | json    -- `await r.json()`
  | unit    -- ignore the body (returns `null`)
  deriving BEq, Repr, Inhabited

structure Endpoint where
  /-- Used as the JS client function name. -/
  name    : String
  path    : String
  /-- HTTP method, upper-case. -/
  method  : String := "GET"
  /-- Parameter names, in declaration order. -/
  params  : List String := []
  carrier : Carrier := .query
  output  : OutputKind := .text
  deriving Inhabited, Repr

/-- A handler receives positionally-decoded params in declaration
    order. Missing params come through as the empty string so the
    user can default in Lean. Return the response body. -/
abbrev Handler := List String → IO String

structure Route where
  ep      : Endpoint
  handler : Handler

/-! ## Tiny URL parsing -/

private def hexNibble (c : Char) : Nat :=
  if c.isDigit then c.toNat - '0'.toNat
  else if c ≥ 'a' && c ≤ 'f' then c.toNat - 'a'.toNat + 10
  else if c ≥ 'A' && c ≤ 'F' then c.toNat - 'A'.toNat + 10
  else 0

partial def percentDecode (s : String) : String := Id.run do
  let arr := s.toList.toArray
  let mut bytes : ByteArray := .empty
  let mut i := 0
  while i < arr.size do
    let c := arr[i]!
    if c == '+' then
      bytes := bytes.push 32; i := i + 1
    else if c == '%' && i + 2 < arr.size then
      let n := hexNibble arr[i+1]! * 16 + hexNibble arr[i+2]!
      bytes := bytes.push n.toUInt8
      i := i + 3
    else
      for b in c.toString.toUTF8 do bytes := bytes.push b
      i := i + 1
  return String.fromUTF8! bytes

def lookupParam (qs : String) (key : String) : Option String :=
  let pref := key ++ "="
  (qs.splitOn "&").findSome? fun p =>
    if p.startsWith pref then some (percentDecode (p.drop pref.length).toString)
    else none

/-! ## Server dispatch -/

/-- Build an HTTP handler from a list of routes. If no route matches
    the request, `fallback` is invoked. -/
def dispatch (routes : List Route) (fallback : Handler := fun _ => return "") : Net.Http.Handler :=
  fun req => do
    for r in routes do
      if r.ep.path == req.path && r.ep.method == req.method then
        let body := match String.fromUTF8? req.body with
          | some s => s | none => ""
        let values : List String := r.ep.params.map fun p =>
          ((lookupParam req.query p).orElse (fun _ => lookupParam body p)).getD ""
        let out ← r.handler values
        let ctype := match r.ep.output with
          | .json => "application/json"
          | _     => "text/plain; charset=utf-8"
        return {
          status := 200,
          headers := #[("content-type", ctype), ("cache-control", "no-store")],
          body := out.toUTF8
        }
    let _ ← fallback []
    return Response.notFound

/-- Build a handler that tries `routes` first and falls through to
    `next` for unmatched requests. Useful when mixing RPC endpoints
    with static file serving or auth callbacks. -/
def chainWith (routes : List Route) (next : Net.Http.Handler) : Net.Http.Handler :=
  fun req => do
    for r in routes do
      if r.ep.path == req.path && r.ep.method == req.method then
        let body := match String.fromUTF8? req.body with
          | some s => s | none => ""
        let values : List String := r.ep.params.map fun p =>
          ((lookupParam req.query p).orElse (fun _ => lookupParam body p)).getD ""
        let out ← r.handler values
        let ctype := match r.ep.output with
          | .json => "application/json"
          | _     => "text/plain; charset=utf-8"
        return {
          status := 200,
          headers := #[("content-type", ctype), ("cache-control", "no-store")],
          body := out.toUTF8
        }
    next req

/-! ## JS client generation -/

namespace Endpoint

open LeanTea.Js.E LeanTea.Js.S LeanTea.Js.Dom

/-- Build the URL expression for a GET (query-carrier) endpoint.
    Result: ``"/path?k1=" + encodeURIComponent(v1) + "&k2=" + ...``. -/
private def queryUrl (ep : Endpoint) : Expr :=
  match ep.params with
  | [] => s ep.path
  | first :: rest =>
    let head := add (s (ep.path ++ "?" ++ first ++ "="))
                    (encodeURIComponent (i first))
    rest.foldl (fun acc p =>
      add (add acc (s ("&" ++ p ++ "="))) (encodeURIComponent (i p)))
      head

/-- For form-carrier: `new URLSearchParams({k1, k2, …}).toString()`. -/
private def formBody (params : List String) : Expr :=
  let fields : List (String × Expr) := params.map fun p => (p, i p)
  mcall (new_ (i "URLSearchParams") [obj fields]) "toString" []

/-- JS source for this endpoint's client function. The function name
    is `ep.name`. Returns `async function <name>(<params>) { ... }`. -/
def clientFn (ep : Endpoint) : Stmt :=
  let fetchExpr : Expr := match ep.carrier with
    | .query => call (i "fetch") [queryUrl ep]
    | .form =>
      let opts := obj [
        ("method", s ep.method),
        ("body", formBody ep.params),
        ("headers", obj [("content-type", s "application/x-www-form-urlencoded")])
      ]
      call (i "fetch") [s ep.path, opts]
  let body : Block := [
    constV "r" (await_ fetchExpr),
    match ep.output with
    | .text => retE (await_ (mcall (i "r") "text"))
    | .json => retE (await_ (mcall (i "r") "json"))
    | .unit => retV
  ]
  afn ep.name ep.params body

end Endpoint

/-- Render every endpoint's client function into a single JS block. -/
def clientLib (eps : List Endpoint) : LeanTea.Js.Block :=
  eps.map Endpoint.clientFn

end LeanTea.Rpc
