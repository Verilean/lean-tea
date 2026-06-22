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
  IO.FS.createDirAll workspace
  IO.println s!"webdav_smoke — workspace={workspace}"
  let child ← spawnServer workspace
  try
    let tree ← spec
    let code ← lspecIO (group "LeanTEA WebDAV" [tree])
    let _ ← child.kill
    try IO.FS.removeDirAll workspace catch _ => pure ()
    if code != 0 then IO.Process.exit code.toUInt8
  catch e =>
    let _ ← child.kill
    try IO.FS.removeDirAll workspace catch _ => pure ()
    IO.println s!"webdav_smoke: crashed → {e}"
    IO.Process.exit 1
