/-! # Minimal CSS DSL

Same idea as `LeanTea.Html` and `LeanTea.Js`: a small AST with a
`render` function. Composable across files / functions, themeable
via plain Lean values. Coverage is what's needed by the sample
apps: rules, declarations, media queries, keyframes, raw escape. -/

namespace LeanTea.Css

abbrev Decl := String × String

inductive Rule where
  /-- `selector { prop: value; ... }` -/
  | rule (selector : String) (decls : List Decl)
  /-- `@media (...) { ... }` — nested rules. -/
  | media (query : String) (inner : List Rule)
  /-- `@keyframes name { from { ... } to { ... } }`. Each stop is a
      pair (label, declarations). -/
  | keyframes (name : String) (stops : List (String × List Decl))
  /-- Verbatim escape hatch. -/
  | raw (src : String)
  deriving Inhabited

abbrev Sheet := List Rule

private def renderDecls (ds : List Decl) : String :=
  ds.foldl (fun acc (k, v) => acc ++ k ++ ":" ++ v ++ ";") ""

partial def Rule.render : Rule → String
  | .rule sel ds      => sel ++ "{" ++ renderDecls ds ++ "}"
  | .raw s            => s
  | .media q inner    =>
      "@media " ++ q ++ "{" ++ (inner.foldl (fun a r => a ++ r.render) "") ++ "}"
  | .keyframes n ss   =>
      "@keyframes " ++ n ++ "{" ++
      ss.foldl (fun acc (label, ds) =>
        acc ++ label ++ "{" ++ renderDecls ds ++ "}") "" ++ "}"

def Sheet.render (s : Sheet) : String :=
  s.foldl (fun acc r => acc ++ r.render) ""

/-! ## Convenience builders -/

def rule (sel : String) (decls : List Decl) : Rule := .rule sel decls
def raw (s : String) : Rule := .raw s
def media (query : String) (inner : List Rule) : Rule := .media query inner
def keyframes (name : String) (stops : List (String × List Decl)) : Rule :=
  .keyframes name stops

end LeanTea.Css
