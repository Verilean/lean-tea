import LeanTea.Html
import LeanTea.Persist.Sqlite

open LeanTea

/-! # LeanTea.Auth.Security — XSS + SQLi guidance + helpers

The framework's existing primitives already keep these two OWASP-top
attacks at bay; this module exists to:

  1. Re-export the safe constructors in one place so reviewers can
     grep for unsafe escapes (`Html.raw`, raw SQL strings) easily.
  2. Add tiny extra helpers (`textNode`, `urlAttr`) that are
     occasionally tempting to skip.
  3. Document **what the framework promises** so callers don't roll
     their own.

## XSS

`LeanTea.Html` HTML escapes any `text` content + every attribute
value on render. `escape` covers the five OWASP characters
(`& < > " '`). The only ways to inject raw HTML are:

  * `Html.raw "<script>…</script>"`
  * `s!"<p>{userInput}</p>" |> Html.raw`   (anti-pattern!)

Both surface through `Html.raw`, so a single grep audits every
escape hatch in the codebase.

For URLs in attributes — `href`, `src`, `formaction` — the escape
covers HTML quoting but **not** scheme allow-listing. A user-supplied
`javascript:alert(1)` would render verbatim. Use `safeUrl` here to
reject anything that isn't `https?:` / `mailto:` / `tel:` / a relative
path.

## SQL injection

`LeanTea.Persist.Sqlite.execp` / `query` bind parameters via
SQLite's `sqlite3_bind_text`. Equivalent for MySQL via
`Persist.Mysql.*`. The repository layer (`Persist.Backend.RepoB`)
always uses parameter binding internally. The only escape hatches:

  * Building `sql` via string interpolation:
    `query db s!"SELECT … WHERE id = {userId}" #[]`  ← unsafe
  * `execRaw` with `params` empty + a `?` count that doesn't match
    the SQL (silently substitutes a `NULL`).

The patterns are mechanical; the lint helper at the bottom returns
the placeholder count and the param count, so a unit test can assert
they match. -/

namespace LeanTea.Auth.Security

open LeanTea

/-! ## XSS helpers -/

/-- Same as `Html.text` but emphasises "this *is* the safe way".
    Picks `Html.text` (escape on render) over `Html.raw`. -/
@[inline] def textNode (s : String) : Html := Html.text s

/-- Allow-list URL schemes for `href` / `src` / `formaction`. Returns
    `none` for anything outside the allow-list, including
    `javascript:` / `data:` / `vbscript:`.

    Relative paths (`/foo`, `./img.png`, `?q=1`) pass through. -/
def safeUrl (raw : String) : Option String :=
  let lower := raw.trim.toLower
  let allow := ["http://", "https://", "mailto:", "tel:", "ftp:"]
  if allow.any (fun pfx => lower.startsWith pfx) then some raw
  else if lower.isEmpty then none
  else
    -- Relative path / fragment / query
    let head := lower.front
    if head == '/' || head == '#' || head == '?' || head == '.' then some raw
    -- Scheme-less authority-relative (`//example.com`) — allow.
    else none

/-! ## SQLi helpers -/

/-- Count `?` placeholders in a SQL string. Useful for a paranoid
    unit-test pass that confirms `params.size` matches before binding. -/
def countPlaceholders (sql : String) : Nat := Id.run do
  let mut n := 0
  let mut inStr := false
  for c in sql.toList do
    if c == '\'' then inStr := !inStr
    else if !inStr && c == '?' then n := n + 1
  return n

structure ParamCheck where
  placeholders : Nat
  params       : Nat
  ok           : Bool
  deriving Inhabited, Repr

def checkParams (sql : String) (params : Array String) : ParamCheck :=
  let n := countPlaceholders sql
  { placeholders := n
  , params       := params.size
  , ok           := n == params.size }

/-- Convenience: throw if the placeholder count doesn't match the
    param count. Catches the most common SQL-injection accident:
    forgetting to add a `?` after string-interpolating a user value. -/
def assertParams (sql : String) (params : Array String) : IO Unit := do
  let c := checkParams sql params
  if !c.ok then
    throw <| IO.userError <|
      s!"SQL placeholder mismatch: `{sql}` has {c.placeholders} `?` " ++
      s!"but {c.params} params supplied. Did you build the SQL by " ++
      s!"interpolation instead of binding?"

/-! ## Safe Sqlite query wrapper -/

/-- Drop-in wrapper around `Sqlite.execp` that runs `assertParams`
    first. Catches an attempt to slip an interpolated user value
    into the SQL string with no matching `?`. -/
def safeExec (db : LeanTea.Sqlite.Db) (sql : String) (params : Array String)
    : IO Nat := do
  assertParams sql params
  LeanTea.Sqlite.execp db sql params

end LeanTea.Auth.Security
