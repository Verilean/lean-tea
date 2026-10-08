import LeanTea.Html
import LeanTea.Template
import LeanTea.Markdown
import LeanTea.Net.Server

/-! # LeanTea.Site — static site generator

Turns a source directory into a static site:

```
site/
  _layouts/default.html   ← LeanTea.Template layouts (optional)
  _includes/footer.html   ← partials for {{#include "_includes/…"}}
  index.html              ← HTML page: front matter + template body
  about.md                ← Markdown page
  posts/hello.md          ← nested pages keep their directory
  img/logo.svg            ← anything else is copied as-is
```

* **Pages** are `.md` files, and `.html` files that start with a
  `---` front-matter block. Other files (including plain `.html`)
  are copied verbatim. Names starting with `_` or `.` are skipped.
* **Front matter** is `key: value` lines between `---` fences. Known
  keys: `title`, `layout` (`none` = no layout), `order`, `date`,
  `description`, `draft: true`, `nav: false`. Every key is also
  available to the layout as `{{key}}`.
* **Titles** come from `title:`, else the first `# Heading`, else the
  file name.
* **Layouts** are `_layouts/<layout>.html` templates (default
  `default`). Bindings: `{{content}}` (raw HTML), `{{title}}`,
  `{{site_title}}`, `{{root}}` (`""` or `"../"`… — prefix it to links
  so the site works from any sub-path or `file://`), `{{url}}`,
  front-matter keys, `{{#each nav}}` (`title`, `url`, `current`),
  `{{#each pages}}`, and one list per top-level directory
  (`{{#each posts}}`, newest `date` first). Values are inserted
  unescaped; `title`/`description` are pre-escaped. Without a layout
  file the built-in sidebar layout (the one the lean-tea book uses)
  is applied.
* **HTML page bodies** are templates too, so an `index.html` can
  list `{{#each posts}}`. Markdown bodies are not (code samples may
  contain `{{`).
* Relative `.md` links are rewritten to `.html`. `sitemap.xml` is
  written when `baseUrl` is set. Lean code can add pages
  (`Page.ofHtml`) — e.g. a TEA view rendered once at build time.

```lean
let r ← LeanTea.Site.build { srcDir := "site", outDir := "_site", siteTitle := "My site" }
```
`static_site build|serve|new` is the CLI (`examples/Tools/StaticSite.lean`). -/

namespace LeanTea.Site

open LeanTea.Template (Value)

structure Config where
  srcDir    : System.FilePath
  outDir    : System.FilePath
  siteTitle : String := "Site"
  tagline   : String := ""
  /-- Absolute site URL (`https://example.com/docs`) — enables `sitemap.xml`. -/
  baseUrl   : String := ""
  /-- Built-in layout's stylesheet: written to `site.css` unless the
      source already provides one. -/
  themeCss  : String := LeanTea.Markdown.Theme.render
  /-- Footer of the built-in layout. -/
  footer    : List Html := [text "Built with ", elem "code" [] [text "LeanTea.Site"]]
  includeDrafts : Bool := false
  deriving Inhabited

structure Page where
  /-- Output path relative to `outDir`, `/`-separated (`posts/hello.html`). -/
  url     : String
  title   : String
  /-- Rendered body HTML (before the layout). -/
  content : String
  params    : List (String × String) := []
  /-- Source path relative to `srcDir` (empty for generated pages). -/
  src     : String := ""
  deriving Inhabited

def Page.get? (p : Page) (k : String) : Option String := (p.params.find? (·.1 == k)).map (·.2)
def Page.order (p : Page) : Int := ((p.get? "order").bind String.toInt?).getD 1000000
def Page.date (p : Page) : String := (p.get? "date").getD ""
/-- In the nav unless `nav: false`; the home page only with `nav: true`. -/
def Page.inNav (p : Page) : Bool :=
  if p.url == "index.html" then p.get? "nav" == some "true" else p.get? "nav" != some "false"

/-- A page built from Lean — e.g. `Page.ofHtml "app.html" "App" (view model)`. -/
def Page.ofHtml (url title : String) (body : Html) (params : List (String × String) := []) : Page :=
  { url, title, content := body.render, params }

/-! ## Small helpers -/

def escape (s : String) : String :=
  s.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;" |>.replace "\"" "&quot;"

/-- `"a/b/c.html"` → `"../../"`. -/
def rootOf (url : String) : String :=
  String.join (List.replicate ((url.splitOn "/").length - 1) "../")

/-- Split `---\nk: v\n---\nbody` into (front matter, body). -/
def frontMatter (src : String) : Option (List (String × String) × String) := do
  let lines := src.splitOn "\n"
  guard (lines.head?.map (·.trimAscii.toString) == some "---")
  let rest := lines.drop 1
  let idx ← rest.findIdx? (·.trimAscii.toString == "---")
  let kvs := (rest.take idx).filterMap fun l =>
    match l.splitOn ":" with
    | k :: vs@(_ :: _) =>
      let v := (":".intercalate vs).trimAscii.toString
      let v := if v.length ≥ 2 && ((v.startsWith "\"" && v.endsWith "\"") || (v.startsWith "'" && v.endsWith "'"))
               then ((v.drop 1).dropEnd 1).toString else v
      let k := k.trimAscii.toString
      if k.isEmpty || k.startsWith "#" then none else some (k, v)
    | _ => none
  return (kvs, "\n".intercalate (rest.drop (idx + 1)))

/-- First `# Heading`, with inline markup stripped. -/
def headingTitle? (md : String) : Option String :=
  (md.splitOn "\n").findSome? fun l =>
    if l.startsWith "# " then
      some ((l.drop 2).toString.trimAscii.toString.replace "`" "" |>.replace "**" "" |>.replace "*" "")
    else none

def stem (name : String) : String :=
  match name.splitOn "." with
  | [n] => n
  | parts => ".".intercalate parts.dropLast

/-- Every file under `dir` (relative, `/`-separated), skipping `_`/`.` names. -/
partial def walk (dir : System.FilePath) (rel : String := "") : IO (Array String) := do
  let mut out := #[]
  for e in ← dir.readDir do
    if e.fileName.startsWith "_" || e.fileName.startsWith "." then continue
    let r := if rel.isEmpty then e.fileName else s!"{rel}/{e.fileName}"
    if ← e.path.isDir then out := out ++ (← walk e.path r)
    else out := out.push r
  return out.qsort (· < ·)

/-! ## Loading -/

inductive Item where
  | page  (p : Page) (isMarkdown : Bool)
  | asset (rel : String)

def loadItem (cfg : Config) (rel : String) : IO (Option Item) := do
  let path := cfg.srcDir / rel
  let base := (rel.splitOn "/").getLast!
  let dir := (rel.splitOn "/").dropLast
  let outUrl := "/".intercalate (dir ++ [stem base ++ ".html"])
  if rel.endsWith ".md" then
    let src ← IO.FS.readFile path
    let (params, body) := (frontMatter src).getD ([], src)
    let title := (params.find? (·.1 == "title")).map (·.2) |>.orElse (fun _ => headingTitle? body) |>.getD (stem base)
    let html := (LeanTea.Markdown.Render.documentToHtml (LeanTea.Markdown.Parser.parse body)).render
    return some (.page { url := outUrl, title, content := html, params, src := rel } true)
  if rel.endsWith ".html" then
    let src ← IO.FS.readFile path
    if let some (params, body) := frontMatter src then
      let title := (params.find? (·.1 == "title")).map (·.2) |>.getD (stem base)
      return some (.page { url := outUrl, title, content := body, params, src := rel } false)
  return some (.asset rel)

/-! ## Bindings + layouts -/

def pageValue (p : Page) (current : String) : Value :=
  let base : List (String × Value) := [
    ("title", .str (escape p.title)), ("url", .str p.url),
    ("current", .str (if p.url == current then "current" else "")),
    ("date", .str p.date), ("description", .str (escape ((p.get? "description").getD "")))]
  let extra : List (String × Value) :=
    (p.params.filter (fun (k, _) => k != "title" && k != "description")).map (fun (k, v) => (k, .str v))
  .dict (base ++ extra)

def sortPages (ps : List Page) : List Page :=
  ps.mergeSort (fun a b => a.order < b.order || (a.order == b.order && a.url ≤ b.url))

/-- Bindings for one page. Links in `nav`/`pages` stay site-relative;
    templates prefix them with `{{root}}`. -/
def bindings (cfg : Config) (all : List Page) (p : Page) : List (String × Value) :=
  let navPages := sortPages (all.filter (·.inNav))
  let sections := all.filterMap (fun q => match q.url.splitOn "/" with
    | d :: _ :: _ => some d | _ => none) |>.eraseDups
  let sectionLists := sections.map fun d =>
    let ps := all.filter (·.url.startsWith (d ++ "/"))
    let ps := ps.mergeSort (fun a b => a.date > b.date || (a.date == b.date && a.url ≤ b.url))
    (d, Value.list (ps.map (pageValue · p.url)))
  [("title", Value.str (escape p.title)), ("site_title", .str (escape cfg.siteTitle)),
   ("tagline", .str (escape cfg.tagline)), ("root", .str (rootOf p.url)), ("url", .str p.url),
   ("description", .str (escape ((p.get? "description").getD ""))),
   ("nav", .list (navPages.map (pageValue · p.url))),
   ("pages", .list ((sortPages all).map (pageValue · p.url)))] ++
  (p.params.filter (fun (k, _) => k != "title" && k != "description") |>.map (fun (k, v) => (k, Value.str v))) ++
  sectionLists

/-- Make `{{#include "_includes/x"}}` resolve inside the source tree. -/
def resolveIncludes (cfg : Config) (src : String) : String :=
  src.replace "{{#include \"_includes/" s!"\{\{#include \"{cfg.srcDir}/_includes/"

/-- The lean-tea book chrome: sidebar nav + main column. -/
def builtinLayout (cfg : Config) (all : List Page) (p : Page) : String :=
  let root := rootOf p.url
  let navPages := sortPages (all.filter (·.inNav))
  let item (q : Page) : Html :=
    elem "li" [] [elem "a" [("href", root ++ q.url), ("class", if q.url == p.url then "current" else "")] [text q.title]]
  let nav := elem "nav" [("class", "sidebar")] ([
      h2 [] [elem "a" [("href", root ++ "index.html"), ("style", "color:inherit;text-decoration:none")] [text cfg.siteTitle]]] ++
      (if cfg.tagline.isEmpty then [] else
        [elem "p" [("class", "muted"), ("style", "color:#94a3b8;font-size:0.78rem;margin-bottom:1.2em")] [text cfg.tagline]]) ++
      [elem "ul" [] (navPages.map item)])
  let main := elem "main" [("class", "content")] [raw p.content, elem "footer" [] cfg.footer]
  "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n" ++
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n" ++
  s!"<title>{escape p.title}</title>\n<link rel=\"stylesheet\" href=\"{root}site.css\">\n</head>\n<body>\n" ++
  nav.render ++ main.render ++ "\n</body>\n</html>\n"

/-! ## Build -/

structure Result where
  pages  : List Page
  assets : Nat
  deriving Inhabited

def writeOut (cfg : Config) (rel : String) (content : String) : IO Unit := do
  let path := cfg.outDir / rel
  if let some d := path.parent then IO.FS.createDirAll d
  IO.FS.writeFile path content

/-- Build the site. `extra` pages (from Lean) join the source pages
    and get the same layouts, nav and listings. -/
def build (cfg : Config) (extra : List Page := []) : IO Result := do
  unless ← cfg.srcDir.isDir do throw <| IO.userError s!"site: source dir {cfg.srcDir} not found"
  IO.FS.createDirAll cfg.outDir
  let mut pages : Array (Page × Bool) := #[]
  let mut assets : Array String := #[]
  for rel in ← walk cfg.srcDir do
    match ← loadItem cfg rel with
    | some (.page p md) =>
      if p.get? "draft" == some "true" && !cfg.includeDrafts then continue
      pages := pages.push (p, md)
    | some (.asset a) => assets := assets.push a
    | none => pure ()
  let all := pages.toList.map (·.1) ++ extra
  let isMd := fun (p : Page) => (pages.find? (·.1.url == p.url)).map (·.2) |>.getD true
  -- assets (byte-exact)
  for a in assets do
    let dst := cfg.outDir / a
    if let some d := dst.parent then IO.FS.createDirAll d
    IO.FS.writeBinFile dst (← IO.FS.readBinFile (cfg.srcDir / a))
  -- pages
  let mut usedBuiltin := false
  for p in all do
    let b := bindings cfg all p
    let content ← if isMd p then pure p.content
      else (LeanTea.Template.parse (resolveIncludes cfg p.content)).render b
    let p := { p with content }
    let layout := (p.get? "layout").getD "default"
    let html ← if layout == "none" then pure content else do
      let lf := cfg.srcDir / "_layouts" / s!"{layout}.html"
      if ← lf.pathExists then
        (LeanTea.Template.parse (resolveIncludes cfg (← IO.FS.readFile lf))).render
          ((("content", .str content) :: b))
      else if layout == "default" then
        usedBuiltin := true
        pure (builtinLayout cfg all p)
      else throw <| IO.userError s!"site: {p.src}: layout {lf} not found"
    writeOut cfg p.url html
  if usedBuiltin && !assets.contains "site.css" then writeOut cfg "site.css" cfg.themeCss
  -- sitemap
  if !cfg.baseUrl.isEmpty then
    let base := if cfg.baseUrl.endsWith "/" then cfg.baseUrl else cfg.baseUrl ++ "/"
    let urls := (sortPages all).foldl (fun acc p => acc ++ s!"  <url><loc>{escape (base ++ p.url)}</loc></url>\n") ""
    writeOut cfg "sitemap.xml" ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
      "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n" ++ urls ++ "</urlset>\n")
  return { pages := all, assets := assets.size }

/-! ## Dev server -/

def contentType (path : String) : String :=
  let lc := path.toLower
  let table : List (String × String) := [
    (".html", "text/html; charset=utf-8"), (".css", "text/css; charset=utf-8"),
    (".js", "application/javascript; charset=utf-8"), (".mjs", "application/javascript; charset=utf-8"),
    (".json", "application/json; charset=utf-8"), (".xml", "application/xml; charset=utf-8"),
    (".txt", "text/plain; charset=utf-8"), (".md", "text/markdown; charset=utf-8"),
    (".svg", "image/svg+xml"), (".png", "image/png"), (".jpg", "image/jpeg"), (".jpeg", "image/jpeg"),
    (".gif", "image/gif"), (".webp", "image/webp"), (".ico", "image/x-icon"), (".wasm", "application/wasm"),
    (".woff2", "font/woff2"), (".woff", "font/woff"), (".pdf", "application/pdf")]
  (table.find? (fun (ext, _) => lc.endsWith ext)).map (·.2) |>.getD "application/octet-stream"

/-- Newest modification time under `dir` (including `_layouts`). -/
partial def newestMtime (dir : System.FilePath) : IO Int := do
  let mut m : Int := 0
  for e in ← dir.readDir do
    if e.fileName.startsWith "." then continue
    let t ← if ← e.path.isDir then newestMtime e.path else pure (← e.path.metadata).modified.sec
    m := max m t
  return m

/-- Serve `outDir`, rebuilding first whenever a source file changed. -/
def serve (cfg : Config) (port : UInt16 := 8080) (host : String := "127.0.0.1")
    (extra : IO (List Page) := pure []) : IO Unit := do
  let built ← IO.mkRef (0 : Int)
  let handler : LeanTea.Net.Http.Handler := fun req => do
    let m ← newestMtime cfg.srcDir
    if m > (← built.get) then
      try
        let r ← build cfg (← extra)
        built.set m
        IO.eprintln s!"site: rebuilt {r.pages.length} pages"
      catch e => return LeanTea.Net.Http.Response.text 500 s!"build failed: {e}"
    let segs := (req.path.splitOn "/").filter (!·.isEmpty)
    if segs.any (fun s => s == ".." || s.startsWith ".") then
      return LeanTea.Net.Http.Response.text 403 "forbidden"
    let mut path := cfg.outDir / "/".intercalate segs
    if ← path.isDir then path := path / "index.html"
    unless ← path.pathExists do
      let nf := cfg.outDir / "404.html"
      if ← nf.pathExists then
        return { status := 404, headers := #[("content-type", "text/html; charset=utf-8")], body := ← IO.FS.readBinFile nf }
      return LeanTea.Net.Http.Response.text 404 "not found"
    return { status := 200, headers := #[("content-type", contentType path.toString), ("cache-control", "no-cache")],
             body := ← IO.FS.readBinFile path }
  IO.eprintln s!"site: serving {cfg.outDir} at http://{host}:{port}/ (rebuilds on change)"
  LeanTea.Net.Server.serve port host handler

end LeanTea.Site
