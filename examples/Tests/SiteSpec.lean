import LeanTea
import LeanTea.Site

/-! # site_spec — LeanTea.Site against a fixture site

Writes two small source trees to a temp dir, builds them with
`LeanTea.Site.build`, and checks the output: front matter and title
fallbacks, layouts and includes, nav order and `nav: false`, dated
section listings, `.md` link rewriting, `{{root}}` for nested pages,
drafts, verbatim assets, a page added from Lean, the built-in layout
fallback, `sitemap.xml`, and escaping. No network. -/

open LeanTea LeanTea.LSpec

def has (h n : String) : Bool := (h.splitOn n).length > 1

def write (root : System.FilePath) (files : List (String × String)) : IO Unit := do
  for (rel, body) in files do
    let p := root / rel
    if let some d := p.parent then IO.FS.createDirAll d
    IO.FS.writeFile p body

def fixture : List (String × String) := [
  ("_layouts/default.html",
   "<html><head><link href=\"{{root}}s.css\"></head><body data-root=\"{{root}}\">" ++
   "<nav>{{#each nav}}<a class=\"{{current}}\" href=\"{{root}}{{url}}\">{{title}}</a>{{/each}}</nav>" ++
   "<h1 class=\"t\">{{title}}</h1><main>{{content}}</main>{{#include \"_includes/foot.html\"}}</body></html>"),
  ("_layouts/bare.html", "BARE[{{content}}]"),
  ("_includes/foot.html", "<footer>FOOT</footer>"),
  ("index.html", "---\ntitle: Home\nnav: true\norder: 0\n---\n<ul>{{#each posts}}<li>{{date}}|{{title}}|{{url}}</li>{{/each}}</ul><p>{{site_title}}</p>"),
  ("about.md", "---\norder: 1\n---\n# About *us*\n\nSee [post](posts/b.md#top) and [ext](https://x.dev/a.md)."),
  ("zeta.md", "No heading here."),
  ("hidden.md", "---\nnav: false\n---\n# Hidden"),
  ("draft.md", "---\ndraft: true\n---\n# Draft"),
  ("raw.md", "---\nlayout: none\n---\n# Raw"),
  ("bare.md", "---\nlayout: bare\ntitle: Bare <one>\n---\nbody"),
  ("posts/a.md", "---\ntitle: Older & wiser\ndate: 2026-01-01\nnav: false\n---\nA"),
  ("posts/b.md", "---\ntitle: Newer\ndate: 2026-02-01\nnav: false\n---\nBack to [about](../about.md)."),
  ("plain.html", "<p>{{not a template}}</p>"),
  ("s.css", "body{}")]

def run : IO LSpec := do
  let tmp : System.FilePath := s!"/tmp/site-spec-{← IO.rand 0 0xffffff}"
  let src := tmp / "src"
  let out := tmp / "out"
  write src fixture
  IO.FS.createDirAll (src / "img")
  IO.FS.writeBinFile (src / "img" / "dot.bin") (ByteArray.mk #[0, 255, 13, 10, 0])
  let extra := Site.Page.ofHtml "app.html" "From Lean" (elem "div" [("id", "app")] [text "1 < 2"]) [("order", "5")]
  let r ← Site.build { srcDir := src, outDir := out, siteTitle := "Spec & Co", baseUrl := "https://ex.com/s" } [extra]
  let rd (p : String) : IO String := do
    let f := out / p
    if ← f.pathExists then IO.FS.readFile f else pure ""
  let index ← rd "index.html"
  let about ← rd "about.html"
  let zeta ← rd "zeta.html"
  let raw ← rd "raw.html"
  let bare ← rd "bare.html"
  let b ← rd "posts/b.html"
  let app ← rd "app.html"
  let plain ← rd "plain.html"
  let sitemap ← rd "sitemap.xml"
  let dot ← IO.FS.readBinFile (out / "img" / "dot.bin")
  let navOf (h : String) := ((h.splitOn "<nav>").getD 1 "").splitOn "</nav>" |>.head!
  -- second site: no layouts → built-in layout + theme css
  let src2 := tmp / "src2"
  let out2 := tmp / "out2"
  write src2 [("index.md", "# Book"), ("ch1.md", "# One"), ("sub/ch2.md", "# Two")]
  let _ ← Site.build { srcDir := src2, outDir := out2, siteTitle := "Book" }
  let ch2 ← IO.FS.readFile (out2 / "sub" / "ch2.html")
  let css2 ← (out2 / "site.css").pathExists
  let css1 ← (out / "site.css").pathExists
  IO.FS.removeDirAll tmp
  return group "LeanTea.Site" [
    group "pages" [
      it "page count: 8 source (draft excluded) + 1 from Lean" (r.pages.length == 9),
      it "draft not written" (!(has index "Draft")),
      it "title from front matter" (has index "<h1 class=\"t\">Home</h1>"),
      it "title from first heading, markup stripped" (has about "<h1 class=\"t\">About us</h1>"),
      it "title falls back to file name" (has zeta "<h1 class=\"t\">zeta</h1>"),
      it "titles are escaped in templates" (has bare "BARE[" && has (navOf index) "From Lean" && !(has index "Older & wiser")),
      it "layout: none" (raw.startsWith "<div class=\"markdown-body\"><h1>Raw</h1>"),
      it "named layout" (bare == "BARE[<div class=\"markdown-body\"><p>body</p></div>]")
    ],
    group "layout + nav" [
      it "include resolved from _includes" (has index "<footer>FOOT</footer>"),
      it "nav order: order, then url" (navOf index == "<a class=\"current\" href=\"index.html\">Home</a><a class=\"\" href=\"about.html\">About us</a><a class=\"\" href=\"app.html\">From Lean</a><a class=\"\" href=\"bare.html\">Bare &lt;one&gt;</a><a class=\"\" href=\"raw.html\">Raw</a><a class=\"\" href=\"zeta.html\">zeta</a>"),
      it "nav: false hides page" (!(has (navOf index) "Hidden")),
      it "nested page root + links" (has b "data-root=\"../\"" && has b "href=\"../s.css\"" && has b "href=\"../about.html\""),
      it "site_title escaped" (has index "<p>Spec &amp; Co</p>")
    ],
    group "content" [
      it "section listing newest first, escaped" (has index "<li>2026-02-01|Newer|posts/b.html</li><li>2026-01-01|Older &amp; wiser|posts/a.html</li>"),
      it ".md links → .html, fragments kept, external untouched" (has about "href=\"posts/b.html#top\"" && has about "href=\"https://x.dev/a.md\""),
      it "page from Lean Html (escaped text)" (has app "<div id=\"app\">1 &lt; 2</div>"),
      it "plain .html copied verbatim" (plain == "<p>{{not a template}}</p>"),
      it "binary asset byte-exact" (dot == ByteArray.mk #[0, 255, 13, 10, 0]),
      it "custom layout → no theme css" (!css1),
      it "sitemap lists pages under baseUrl" (has sitemap "<loc>https://ex.com/s/posts/b.html</loc>" && !(has sitemap "draft"))
    ],
    group "built-in layout" [
      it "sidebar + theme css" (css2 && has ch2 "<nav class=\"sidebar\">"),
      it "nested page links css via root" (has ch2 "href=\"../site.css\"" && has ch2 "href=\"../ch1.html\"")
    ]
  ]

def main : IO UInt32 := do
  let code ← lspecIO (← run)
  return if code == 0 then 0 else 1
