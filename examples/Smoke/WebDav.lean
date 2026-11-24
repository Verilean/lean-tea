import LeanTea
import LeanTea.Net.HttpClient
import LeanTea.LSpec

/-! # webdav_smoke — round-trip the WebDAV server via HttpClient

Spawns `./.lake/build/bin/webdav_serve` as a subprocess pointed at
a fresh tempdir, then drives MKCOL / PUT / GET / PROPFIND / DELETE
through `LeanTea.Net.HttpClient`. No external services — same
"two-process serve-and-test" pattern as `auth_spec`. -/

open LeanTea LeanTea.LSpec
open LeanTea.Net.HttpClient

private def port : UInt16 := 18021

private def baseUrl : String := s!"http://127.0.0.1:{port}"

private def parsedUrl (path : String) : IO Url := do
  match parseUrl (baseUrl ++ path) with
  | some u => return u
  | none   => throw <| IO.userError s!"bad URL: {baseUrl ++ path}"

private def headerVal (r : Response) (name : String) : Option String :=
  r.headers.findSome? fun (k, v) => if k.toLower == name.toLower then some v else none

/-- Best-effort: returns true once the IdP answers OPTIONS /dav/. -/
private def probeReady : IO Bool := do
  try
    let u ← parsedUrl "/dav/"
    let r ← request "OPTIONS" u
    return r.status != 0
  catch _ => return false

private def spawnServer (workspace : String) :
    IO (IO.Process.Child { stdin := .null, stdout := .null, stderr := .null }) := do
  let env : Array (String × Option String) := #[
    ("WEBDAV_PORT", some (toString port)),
    ("WEBDAV_HOST", some "127.0.0.1"),
    ("WEBDAV_WORKSPACE", some workspace),
    ("WEBDAV_PREFIX", some "/dav")
  ]
  let child ← IO.Process.spawn {
    cmd := "./.lake/build/bin/webdav_serve",
    args := #[],
    env, stdin := .null, stdout := .null, stderr := .null
  }
  /- Probe until /dav answers OPTIONS, up to ~2 s. -/
  for _ in [:20] do
    if ← probeReady then break
    IO.sleep 100
  return child

private def hasSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Symlinks and the workspace root: nothing outside the workspace is
    read, written or deleted through a link, the root cannot be deleted,
    and files are served as inert data. -/
def boundarySpec (workspace outside : String) : IO LSpec := do
  let ln := fun (target link : String) => do
    let _ ← IO.Process.output { cmd := "ln", args := #["-s", target, link] }
  IO.FS.writeFile s!"{outside}/secret.txt" "outside secret"
  IO.FS.createDirAll s!"{outside}/outdir"
  IO.FS.writeFile s!"{outside}/outdir/keep.txt" "keep"
  IO.FS.createDirAll s!"{workspace}/inner-dir"
  IO.FS.writeFile s!"{workspace}/inner-dir/keep.txt" "keep"
  ln s!"{outside}/secret.txt" s!"{workspace}/link-file"
  ln s!"{outside}/outdir" s!"{workspace}/link-dir"
  ln s!"{outside}/new.txt" s!"{workspace}/dangling"
  ln s!"{workspace}/inner-dir" s!"{workspace}/inner-link"
  let req := fun (m path body : String) => do
    request m (← parsedUrl path) (body := body.toUTF8)
  let getLink ← req "GET" "/dav/link-file" ""
  let putDangling ← req "PUT" "/dav/dangling" "planted"
  let putThroughDir ← req "PUT" "/dav/link-dir/x.txt" "planted"
  let delLinkDir ← req "DELETE" "/dav/link-dir" ""
  let delRoot ← req "DELETE" "/dav/" ""
  let delInnerLink ← req "DELETE" "/dav/inner-link" ""
  let _ ← req "PUT" "/dav/page.html" "<script>alert(1)</script>"
  let page ← req "GET" "/dav/page.html" ""
  return group "workspace boundary" [
    it "GET through a symlink to a file outside is 403" (getLink.status == 403),
    it "PUT to a dangling symlink pointing outside is 403 and creates nothing"
      (putDangling.status == 403 && !(← System.FilePath.pathExists s!"{outside}/new.txt")),
    it "PUT through a symlinked directory outside is 403"
      (putThroughDir.status == 403 && !(← System.FilePath.pathExists s!"{outside}/outdir/x.txt")),
    it "DELETE of a symlink to a directory outside is 403 and leaves it intact"
      (delLinkDir.status == 403 && (← System.FilePath.pathExists s!"{outside}/outdir/keep.txt")),
    it "DELETE of the workspace root is 403" (delRoot.status == 403 && (← System.FilePath.pathExists workspace)),
    it "DELETE of a symlink inside removes the link, not its target"
      (delInnerLink.status == 204 && (← System.FilePath.pathExists s!"{workspace}/inner-dir/keep.txt")),
    it "files are served with nosniff and a sandbox CSP"
      (page.status == 200 && headerVal page "x-content-type-options" == some "nosniff" &&
        headerVal page "content-security-policy" == some "sandbox")
  ]

def spec : IO LSpec := do
  /- 1. OPTIONS → 200 with DAV header. -/
  let u ← parsedUrl "/dav/"
  let opts ← request "OPTIONS" u
  let davHdr := (headerVal opts "dav").getD ""
  /- 2. MKCOL /dav/sub. -/
  let mkU ← parsedUrl "/dav/sub"
  let mk ← request "MKCOL" mkU
  /- 3. PUT a file. -/
  let putU ← parsedUrl "/dav/sub/file.txt"
  let put ← request "PUT" putU (body := "hello-from-webdav-smoke".toUTF8)
  /- 4. GET it back. -/
  let getU ← parsedUrl "/dav/sub/file.txt"
  let get ← request "GET" getU
  let getBody := match String.fromUTF8? get.body with | some s => s | none => ""
  /- 5. PROPFIND /dav/sub with Depth: 1 → multistatus mentioning file.txt. -/
  let pfU ← parsedUrl "/dav/sub"
  let pf ← request "PROPFIND" pfU (headers := #[("Depth", "1")])
  let pfBody := match String.fromUTF8? pf.body with | some s => s | none => ""
  /- 6. DELETE the file. -/
  let del ← request "DELETE" getU
  /- 7. GET after delete → 404. -/
  let getAfter ← request "GET" getU
  /- 8. Path-traversal block → 403. -/
  let badU ← parsedUrl "/dav/../etc/passwd"
  let bad ← request "GET" badU
  return group "WebDAV round-trip" [
    it "OPTIONS returns 200" (opts.status == 200),
    it "DAV header advertises level 1" (davHdr == "1"),
    it "MKCOL /sub returns 201" (mk.status == 201),
    it "PUT /sub/file.txt returns 201" (put.status == 201),
    it "GET /sub/file.txt returns 200" (get.status == 200),
    it "GET body round-trips"
      (getBody == "hello-from-webdav-smoke"),
    it "PROPFIND returns 207" (pf.status == 207),
    it "PROPFIND XML mentions file.txt"
      (hasSubstr pfBody "file.txt"),
    it "DELETE returns 204" (del.status == 204),
    it "GET after delete returns 404" (getAfter.status == 404),
    it "path-traversal blocked with 403" (bad.status == 403)
  ]

def main : IO Unit := do
  /- Fresh tempdir per run. -/
  let ts ← IO.monoMsNow
  let workspace := s!"/tmp/leantea-webdav-smoke-{ts}"
  let outside := s!"/tmp/leantea-webdav-outside-{ts}"
  IO.FS.createDirAll workspace
  IO.FS.createDirAll outside
  IO.println s!"webdav_smoke — workspace={workspace}"
  let child ← spawnServer workspace
  try
    let tree ← spec
    let boundary ← boundarySpec workspace outside
    let code ← lspecIO (group "LeanTEA WebDAV" [tree, boundary])
    let _ ← child.kill
    try IO.FS.removeDirAll workspace catch _ => pure ()
    try IO.FS.removeDirAll outside catch _ => pure ()
    if code != 0 then IO.Process.exit code.toUInt8
  catch e =>
    let _ ← child.kill
    try IO.FS.removeDirAll workspace catch _ => pure ()
    IO.println s!"webdav_smoke: crashed → {e}"
    IO.Process.exit 1
