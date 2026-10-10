# 13 · Static sites — `LeanTea.Site`

`LeanTea.Site` turns a directory of Markdown and HTML into a static
site. It is the engine behind this book (`gen_site`) and is exposed
as a CLI for any other site:

```sh
lake build static_site
./.lake/build/bin/static_site new mysite        # starter: layout, include, pages, posts
./.lake/build/bin/static_site serve mysite      # http://127.0.0.1:8080/, rebuilds on change
./.lake/build/bin/static_site build mysite --out _site --title "My site" --base-url https://example.com/
```

## Source layout

| path | becomes |
|---|---|
| `about.md` | `about.html`. Title comes from `title:`, else the first `# Heading`, else the file name. |
| `index.html` with `---` front matter | a page whose body is a `LeanTea.Template` |
| `posts/hello.md` | `posts/hello.html`, listed in `{{#each posts}}` (newest `date` first) |
| `_layouts/<name>.html` | layout templates (`layout:` key, default `default`) |
| `_includes/x.html` | partials, `{{#include "_includes/x.html"}}` |
| anything else | copied byte-for-byte (`.html` without front matter too) |
| `_*`, `.*` | ignored |

Front matter is `key: value` lines between `---` fences:

- Known keys: `title`, `layout` (`none` = no layout), `order`, `date`, `description`, `draft: true`, `nav: false`.
- The home page joins the nav only with `nav: true`.
- Every key is also a template binding.

## Layout bindings

| binding | value |
|---|---|
| `{{content}}` | the page's HTML |
| `{{title}}`, `{{description}}`, `{{site_title}}` | pre-escaped |
| `{{root}}` | `""` / `"../"` … — prefix links with it, so the site works under any sub-path or `file://` |
| `{{url}}` | the page's own URL |
| `{{#each nav}}` | `title`, `url`, `current` |
| `{{#each pages}}` | every page |
| `{{#each <dir>}}` | one list per top-level directory |

Without `_layouts/default.html` the built-in sidebar layout (this
book's) is used, with a typed-CSS `site.css`.

Relative `.md` links are rewritten to `.html`. `sitemap.xml` is written
when `baseUrl` is set.

## From Lean

```lean
import LeanTea.Site
open LeanTea

def main : IO Unit := do
  let app := Site.Page.ofHtml "counter.html" "Counter" (Counter.view Counter.init)
  let r ← Site.build { srcDir := "site", outDir := "_site", siteTitle := "Demo" } [app]
  IO.println s!"{r.pages.length} pages"
```

Pages added from Lean get the same layout, nav and listings as files.
`Site.serve cfg port` is the dev server: it rebuilds when a source file
changes, serves with the right MIME types, and refuses `..` paths.

Not included: incremental builds, pretty URLs (`/about/`), RSS,
syntax highlighting, pagination.
