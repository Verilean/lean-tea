import LeanTea.Html
import LeanTea.Net.Http

/-! # LeanTea.Web.Route — Yesod-style typed routes

Each app declares its routes as an inductive type. The compiler
tracks every use of the enum end-to-end:

  * Rendering a link with `Route.link : α → String → Html` accepts
    only a concrete route constructor. Passing a raw String is a
    type error — no ad-hoc `href="/api/step"` typos.
  * Dispatching a `Request` matches on the same inductive. Lean's
    exhaustiveness check tells you what breaks when you add or
    remove a constructor.
  * Renaming a constructor is a single edit; the compiler lists
    every call site that depends on it.

## Shape

```lean
inductive AppRoute where
  | home
  | userProfile (userId : String)
  | apiSetCell
  | staticAsset (path : String)
  deriving BEq, Repr

instance : LeanTea.Web.Route AppRoute where
  toPath
    | .home             => "/"
    | .userProfile uid  => s!"/user/{uid}"
    | .apiSetCell       => "/api/set"
    | .staticAsset p    => s!"/assets/{p}"

-- Usage:
def nav : Html :=
  Route.link .home "Home"
```

## What we do NOT try to do (yet)

- **Bidirectional parsing.** `fromPath : String → Option α` is
  useful for dispatch and is a straight follow-up, but it lives
  on the app's own dispatch function today. Once we settle on the
  right shape (Yesod's `dispatch` derives it; Servant's `Capture`
  weaves it into the type-level route) we'll add a codec.
- **Query-string / body parameters at the type level.** For that
  see the RPC layer (`LeanTea.Rpc`); route parameters here are
  path pieces only.

The overall bet: 90% of the "dead link at deploy time" pain is
solved by just having HTML anchors refuse `String` and demand a
constructor of a known enum. The remaining 10% (typed captures,
inverse parsing) can layer on later without breaking users. -/

namespace LeanTea.Web

/-- Every route enum implements this. `toPath` turns a constructor
    into the URL you'd put in an `href`. -/
class Route (α : Type) where
  toPath : α → String

/-- Render a link to a typed route. Refuses raw String hrefs by
    construction — dead links become compile errors. -/
def Route.link {α : Type} [Route α] (route : α) (label : String)
    (extraAttrs : List (String × String) := []) : LeanTea.Html :=
  let href := Route.toPath route
  let attrs := ("href", href) :: extraAttrs
  LeanTea.a_ attrs [LeanTea.text label]

/-- Emit just the URL. Occasionally handy inside a template that
    the framework didn't build (e.g. a redirect target, or JSON
    payload that carries a route). Same guarantee: only a
    constructor gets past the type checker. -/
def Route.href {α : Type} [Route α] (route : α) : String :=
  Route.toPath route

/-! ## Compile-time dispatch helper

The typical way an app dispatches is to `match req.path` on strings
today. With a route enum, apps can flip the direction — declare a
routing table via a total function on the enum — and Lean's
exhaustiveness check will tell them if they forget a case. -/

/-- A per-route handler: given the request (and any captured route
    params via the enum's constructors), produce a response. This
    is intentionally simpler than the RPC layer's Handler — it's
    the thin ceremony you need for links + traditional web pages. -/
abbrev RouteHandler (α : Type) := α → LeanTea.Net.Http.Request → IO LeanTea.Net.Http.Response

/-- Dispatch a request via a route parser + a total handler. The
    handler must cover every constructor of `α`; missing cases will
    surface at compile time when the caller writes the match. -/
def Route.dispatch {α : Type} [Route α]
    (parseRoute : LeanTea.Net.Http.Request → Option α)
    (handler : RouteHandler α)
    (fallback : LeanTea.Net.Http.Request → IO LeanTea.Net.Http.Response :=
      fun _ => return LeanTea.Net.Http.Response.notFound)
    : LeanTea.Net.Http.Request → IO LeanTea.Net.Http.Response :=
  fun req =>
    match parseRoute req with
    | some r => handler r req
    | none   => fallback req

end LeanTea.Web
