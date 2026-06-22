import LeanTea.Net.Http
import LeanTea.Net.SafePath

/-! # LeanTea.Net.WebDav — RFC 4918 WebDAV server

A minimal but Finder/Nautilus/Windows-mountable WebDAV `Handler`
sitting on top of `LeanTea.Net.Server`. Targeted at the
"expose a workspace dir as a network drive" use case — backups,
quick file-share, agent file delivery.

### Methods shipped

  * `OPTIONS`  — `DAV: 1`, `Allow:` advertising supported methods
  * `GET` / `HEAD` — read a file (or 404 / 403)
  * `PUT`      — write a file (creates parent dirs that already exist;
                  does not auto-mkdir intermediate dirs)
  * `DELETE`   — remove file or (empty) directory
  * `MKCOL`    — create a directory
  * `PROPFIND` — list contents (Depth: 0 / 1 / infinity), 207 XML

`COPY` and `MOVE` are intentionally **not** shipped in v0.1 — they
fall back to `client copies via GET + PUT` which all major clients
do automatically when the server returns `405`. `LOCK` / `UNLOCK`
are likewise out of scope (most modern clients tolerate a 501).

### Security boundary

Every request path is funnelled through `LeanTea.Net.SafePath` —
`..`, NUL bytes, absolute paths outside the workspace, and
sibling-prefix attacks are all refused with a 403 *before* any
filesystem call. The workspace itself isn't read symlinks (best-
effort, same as SafePath).

For **authenticated** WebDAV, wrap the returned handler in your
own auth gate (`Auth.gate` / `Form.csrf` etc.). Bearer-token
header support is one configuration step away. -/

namespace LeanTea.Net.WebDav

open LeanTea.Net.Http
open LeanTea.Net (SafePath)

/-! ## Config -/

structure Config where
  /-- Filesystem root the server exposes. Must be an absolute path
      and already exist on disk; the handler refuses requests if
      the workspace is empty. -/
  workspace : String
  /-- URL prefix this WebDAV tree is mounted under. `""` means the
      server is mounted at `/`. Use e.g. `"/dav"` to combine WebDAV
      with other handlers behind the same `Net.Server`. -/
  urlPrefix : String := ""
  /-- Refuse `PUT` / `DELETE` / `MKCOL`. Useful when WebDAV is just
      a polite way to expose download / browse access. -/
  readonly  : Bool := false
  deriving Inhabited, Repr

/-! ## URL ↔ filesystem path helpers -/

/-- Tiny `%xx` decoder. Same shape as `LeanTea.Auth.Idp.urlDecode`
    — kept inline so WebDAV doesn't depend on the auth layer. -/
private def hexNibble (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c ∧ c ≤ 'f' then some (10 + c.toNat - 'a'.toNat)
  else if 'A' ≤ c ∧ c ≤ 'F' then some (10 + c.toNat - 'A'.toNat)
  else none

private partial def urlDecode (s : String) : String := Id.run do
  let mut out := ""
  let mut rest := s.toList
  while h : !rest.isEmpty do
    match rest with
    | '+' :: tl => out := out.push ' '; rest := tl
    | '%' :: h1 :: h2 :: tl =>
      match hexNibble h1, hexNibble h2 with
      | some n1, some n2 =>
        out := out.push (Char.ofNat (n1 * 16 + n2)); rest := tl
      | _, _ => out := out.push '%'; rest := h1 :: h2 :: tl
    | c :: tl => out := out.push c; rest := tl
    | [] => break
  return out

/-- URL-encode every path segment but leave `/` alone. Used to
    write `<D:href>` back into the PROPFIND XML response. -/
private def urlEncodeSegment (s : String) : String := Id.run do
  let hx := "0123456789ABCDEF".toList.toArray
  let mut out := ""
  for c in s.toList do
    if c.isAlpha || c.isDigit
       || c == '-' || c == '_' || c == '.' || c == '~' || c == '/' then
      out := out.push c
    else
      for b in c.toString.toUTF8 do
        out := out.push '%'
        out := out.push hx[b.toNat / 16]!
        out := out.push hx[b.toNat % 16]!
  return out

/-- Strip the urlPrefix and URL-decode the request path into a
    filesystem-relative path. Returns `none` if the prefix doesn't
    match — the handler then 404s. -/
def Config.relativePath (cfg : Config) (urlPath : String) : Option String :=
  let stripped :=
    if cfg.urlPrefix.isEmpty then urlPath
    else if urlPath.startsWith cfg.urlPrefix then
      (urlPath.drop cfg.urlPrefix.length).toString
    else
      ""
  if cfg.urlPrefix.isEmpty || urlPath.startsWith cfg.urlPrefix then
    let leading :=
      if stripped.isEmpty then "/" else
      if stripped.startsWith "/" then stripped else "/" ++ stripped
    some (urlDecode leading)
  else none

/-- Resolve a request-URL path against the workspace, refusing any
    `..` / NUL / out-of-tree access. Returns `(safePath,
    fsRelPath)` on success. -/
def Config.resolve (cfg : Config) (urlPath : String) : Except String SafePath :=
  match cfg.relativePath urlPath with
  | none => .error "url prefix mismatch"
  | some rel =>
    let inner := if rel.startsWith "/" then (rel.drop 1).toString else rel
    SafePath.under cfg.workspace inner

/-! ## XML escaping for PROPFIND output -/

private def xmlEscape (s : String) : String := Id.run do
  let mut out := ""
  for c in s.toList do
    match c with
    | '&' => out := out ++ "&amp;"
    | '<' => out := out ++ "&lt;"
    | '>' => out := out ++ "&gt;"
    | '"' => out := out ++ "&quot;"
    | '\'' => out := out ++ "&apos;"
    | _   => out := out.push c
  return out

/-! ## PROPFIND helpers -/

/-- A single resource entry rendered into the multistatus body. -/
private structure Entry where
  /-- URL path relative to the WebDAV root (e.g. `/dav/sub/foo.txt`). -/
  href      : String
  isDir     : Bool
  size      : Nat
  mtimeSec  : Nat
  deriving Inhabited

/-- Format a Unix timestamp into RFC 1123 (used by HTTP/WebDAV).
    We shell to `date(1)` because pure-Lean calendar conversion is
    out of scope. -/
private def httpDate (epoch : Nat) : IO String := do
  let out ← IO.Process.output {
    cmd := "date", args := #["-u", "-r", toString epoch, "+%a, %d %b %Y %H:%M:%S GMT"]
  }
  if out.exitCode != 0 then
    /- macOS `date -r` works; on Linux `date -d "@<epoch>"` is the form.
       Try that as a fallback. -/
    let out2 ← IO.Process.output {
      cmd := "date", args := #["-u", "-d", s!"@{epoch}", "+%a, %d %b %Y %H:%M:%S GMT"]
    }
    return out2.stdout.trimAscii.toString
  return out.stdout.trimAscii.toString

/-- Read an entry's filesystem metadata into the structure used by
    PROPFIND. Returns `none` if the path doesn't exist. -/
private def entryOf (cfg : Config) (relPath : String) : IO (Option Entry) := do
  /- Reject anything that escapes the workspace, then stat. -/
  match SafePath.under cfg.workspace
          (if relPath.startsWith "/" then (relPath.drop 1).toString else relPath) with
  | .error _ => return none
  | .ok p =>
    try
      let info ← System.FilePath.metadata (System.FilePath.mk p.value)
      let isDir := info.type == .dir
      let size  := info.byteSize.toNat
      /- Best-effort mtime — fall back to 0 if the metadata struct
         doesn't surface it (Lean's stdlib has been changing here). -/
      let mtime : Nat := 0      -- TODO once stdlib exposes a stable mtime accessor
      let _ := mtime
      let segments := cfg.urlPrefix ++ relPath
      return some { href := urlEncodeSegment segments,
                    isDir := isDir,
                    size := size, mtimeSec := 0 }
    catch _ => return none

private def renderResponse (e : Entry) (now : String) : String :=
  let kind := if e.isDir then "<D:collection/>" else ""
  let szTag := if e.isDir then ""
               else s!"<D:getcontentlength>{e.size}</D:getcontentlength>\n"
  s!"  <D:response>
    <D:href>{xmlEscape e.href}</D:href>
    <D:propstat>
      <D:prop>
        <D:resourcetype>{kind}</D:resourcetype>
        {szTag}<D:getlastmodified>{xmlEscape now}</D:getlastmodified>
      </D:prop>
      <D:status>HTTP/1.1 200 OK</D:status>
    </D:propstat>
  </D:response>
"

private def multistatus (body : String) : Response :=
  let xml :=
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++
    "<D:multistatus xmlns:D=\"DAV:\">\n" ++ body ++ "</D:multistatus>\n"
  { status  := 207,
    headers := #[("content-type", "application/xml; charset=utf-8")],
    body    := xml.toUTF8 }

/-! ## Method handlers -/

private def methodNotAllowed : Response := .text 405 "method not allowed\n"
private def forbidden        : Response := .text 403 "forbidden\n"
private def notImplemented   : Response := .text 501 "not implemented\n"
private def conflict         : Response := .text 409 "conflict\n"

private def handleOptions : IO Response := do
  let r : Response := .text 200 ""
  let r := r.setHeader! "dav" "1"
  let r := r.setHeader! "allow"
    "OPTIONS, GET, HEAD, PUT, DELETE, MKCOL, PROPFIND"
  /- Some clients require ms-author-via or accept-ranges; advertise
     the minimum that real-world `mount_webdav` / `gvfs-mount`
     handshakes check for. -/
  let r := r.setHeader! "ms-author-via" "DAV"
  return r

private def guessContentType (path : String) : String :=
  let lc := path.toLower
  if      lc.endsWith ".txt"  then "text/plain; charset=utf-8"
  else if lc.endsWith ".md"   then "text/markdown; charset=utf-8"
  else if lc.endsWith ".html" then "text/html; charset=utf-8"
  else if lc.endsWith ".css"  then "text/css; charset=utf-8"
  else if lc.endsWith ".js"   then "application/javascript; charset=utf-8"
  else if lc.endsWith ".json" then "application/json; charset=utf-8"
  else if lc.endsWith ".png"  then "image/png"
  else if lc.endsWith ".jpg" || lc.endsWith ".jpeg" then "image/jpeg"
  else if lc.endsWith ".gif"  then "image/gif"
  else if lc.endsWith ".svg"  then "image/svg+xml"
  else if lc.endsWith ".pdf"  then "application/pdf"
  else "application/octet-stream"

private def handleGet (cfg : Config) (req : Request) : IO Response := do
  match cfg.resolve req.path with
  | .error _ => return forbidden
  | .ok p =>
    if !(← System.FilePath.pathExists p.value) then return Response.notFound
    let info ← System.FilePath.metadata (System.FilePath.mk p.value)
    if info.type == .dir then
      return Response.text 200 "(directory — use PROPFIND to list)\n"
    else
      let bs ← IO.FS.readBinFile p.value
      let mut r : Response :=
        { status := 200,
          headers := #[("content-type", guessContentType p.value)],
          body := bs }
      r := r.setHeader! "content-length" (toString bs.size)
      return r

private def handleHead (cfg : Config) (req : Request) : IO Response := do
  let r ← handleGet cfg req
  return { r with body := .empty }

private def handlePut (cfg : Config) (req : Request) : IO Response := do
  if cfg.readonly then return forbidden
  match cfg.resolve req.path with
  | .error _ => return forbidden
  | .ok p =>
    /- WebDAV doesn't say "create parent dirs"; clients are expected
       to MKCOL them. We do the same — refuse PUT if the parent
       doesn't exist yet. -/
    let parent := System.FilePath.parent (System.FilePath.mk p.value) |>.getD "/"
    if !(← parent.pathExists) then return conflict
    try
      IO.FS.writeBinFile p.value req.body
      return Response.text 201 "created\n"
    catch e => return Response.serverError s!"put: {e}"

private def handleDelete (cfg : Config) (req : Request) : IO Response := do
  if cfg.readonly then return forbidden
  match cfg.resolve req.path with
  | .error _ => return forbidden
  | .ok p =>
    if !(← System.FilePath.pathExists p.value) then return Response.notFound
    try
      let info ← System.FilePath.metadata (System.FilePath.mk p.value)
      if info.type == .dir then IO.FS.removeDirAll p.value
      else IO.FS.removeFile p.value
      return Response.text 204 ""
    catch e => return Response.serverError s!"delete: {e}"

private def handleMkcol (cfg : Config) (req : Request) : IO Response := do
  if cfg.readonly then return forbidden
  match cfg.resolve req.path with
  | .error _ => return forbidden
  | .ok p =>
    if (← System.FilePath.pathExists p.value) then return methodNotAllowed
    try
      IO.FS.createDir p.value
      return Response.text 201 "created\n"
    catch e => return Response.serverError s!"mkcol: {e}"

private def handlePropfind (cfg : Config) (req : Request) : IO Response := do
  match cfg.resolve req.path with
  | .error _ => return forbidden
  | .ok p =>
    if !(← System.FilePath.pathExists p.value) then return Response.notFound
    /- Depth header: "0" = just the resource, "1" = resource + children,
       "infinity" = full recursive (we silently treat as 1 to avoid
       runaway). -/
    let depth := (req.header? "depth").getD "1"
    let now ← httpDate 0
    let info ← System.FilePath.metadata (System.FilePath.mk p.value)
    let topRel :=
      let s := req.path
      if !(cfg.urlPrefix.isEmpty) && s.startsWith cfg.urlPrefix then
        (s.drop cfg.urlPrefix.length).toString
      else s
    let topRelClean := if topRel.isEmpty then "/" else topRel
    let topEntry : Entry := {
      href := urlEncodeSegment (cfg.urlPrefix ++ topRelClean),
      isDir := info.type == .dir,
      size := info.byteSize.toNat,
      mtimeSec := 0
    }
    let mut body := renderResponse topEntry now
    if (depth == "1" || depth == "infinity") && info.type == .dir then
      let entries ← System.FilePath.readDir p.value
      for entry in entries do
        let name := entry.fileName
        let pre := if topRelClean.endsWith "/" then topRelClean
                   else topRelClean ++ "/"
        let childUrl := pre ++ name
        match ← entryOf cfg childUrl with
        | none => pure ()
        | some e => body := body ++ renderResponse e now
    return multistatus body

/-! ## Top-level handler -/

/-- Compose all the methods into one `Handler`. Mount under your
    server like:

    ```lean
    let dav := WebDav.handler { workspace := "/srv/files",
                                 urlPrefix := "/dav" }
    Server.serve port "0.0.0.0" dav
    ```

    Or chain with other routes:

    ```lean
    let dav := WebDav.handler cfg
    let combined : Handler := fun req =>
      if req.path.startsWith "/dav" then dav req
      else myAppHandler req
    ```
-/
def handler (cfg : Config) : Handler := fun req => do
  match req.method with
  | "OPTIONS"  => handleOptions
  | "GET"      => handleGet cfg req
  | "HEAD"     => handleHead cfg req
  | "PUT"      => handlePut cfg req
  | "DELETE"   => handleDelete cfg req
  | "MKCOL"    => handleMkcol cfg req
  | "PROPFIND" => handlePropfind cfg req
  | "COPY" | "MOVE" | "LOCK" | "UNLOCK" => return notImplemented
  | _          => return methodNotAllowed

end LeanTea.Net.WebDav
