import LeanTea
import LeanTea.Net.WebDav

/-! # webdav_serve — expose a directory over WebDAV

A thin binary wrapping `LeanTea.Net.WebDav.handler`. The result is
mountable by:

  * **macOS Finder**: Cmd-K → `http://127.0.0.1:8021/dav`
  * **GNOME Files**: Other Locations → `dav://127.0.0.1:8021/dav`
  * **Windows Explorer**: Map network drive → same URL (needs
                          basic-auth + the Windows registry tweak)
  * **`mount.davfs`**: `mount.davfs http://127.0.0.1:8021/dav /mnt/dav`

```sh
./.lake/build/bin/webdav_serve --port 8021 --workspace /srv/files
```

Args:
  --port      <PORT>        HTTP port (default 8021)
  --host      <HOST>        listen address (default 0.0.0.0)
  --workspace <DIR>         filesystem root to expose (default = cwd)
  --prefix    <PREFIX>      URL prefix the WebDAV tree is mounted
                            under (default `/dav`)
  --readonly                refuse PUT/DELETE/MKCOL

Env overrides:
  WEBDAV_PORT, WEBDAV_HOST, WEBDAV_WORKSPACE, WEBDAV_PREFIX,
  WEBDAV_READONLY=1.

Security boundary: every path is funnelled through
`LeanTea.Net.SafePath`; `..` / NUL / out-of-tree access is refused
at construction time. No authentication is wired in by default —
add `Auth.gate` / your own bearer-token check before opening the
port to anything but localhost. -/

open LeanTea LeanTea.Net

private structure Args where
  port      : UInt16 := 8021
  host      : String := "0.0.0.0"
  workspace : String := ""
  pathPrefix : String := "/dav"
  readonly  : Bool   := false

private partial def parseArgs (xs : List String) (a : Args) : Args :=
  match xs with
  | "--port"      :: v :: rest =>
    parseArgs rest { a with port := (v.toNat?.getD 8021).toUInt16 }
  | "--host"      :: v :: rest => parseArgs rest { a with host := v }
  | "--workspace" :: v :: rest => parseArgs rest { a with workspace := v }
  | "--prefix"    :: v :: rest => parseArgs rest { a with pathPrefix := v }
  | "--readonly"  :: rest      => parseArgs rest { a with readonly := true }
  | _ :: rest                  => parseArgs rest a
  | []                         => a

def main (args : List String) : IO Unit := do
  let mut a := parseArgs args {}
  if a.workspace.isEmpty then a := { a with workspace := (← IO.currentDir).toString }
  if let some p ← IO.getEnv "WEBDAV_PORT" then
    if let some n := p.toNat? then a := { a with port := n.toUInt16 }
  if let some h ← IO.getEnv "WEBDAV_HOST" then a := { a with host := h }
  if let some w ← IO.getEnv "WEBDAV_WORKSPACE" then a := { a with workspace := w }
  if let some p ← IO.getEnv "WEBDAV_PREFIX" then a := { a with pathPrefix := p }
  if let some r ← IO.getEnv "WEBDAV_READONLY" then
    if r.toLower == "1" || r.toLower == "true" then a := { a with readonly := true }
  IO.eprintln s!"webdav-serve: http://{a.host}:{a.port}{a.pathPrefix} → {a.workspace}{if a.readonly then " (read-only)" else ""}"
  let cfg : WebDav.Config := {
    workspace := a.workspace,
    urlPrefix := a.pathPrefix,
    readonly  := a.readonly
  }
  LeanTea.Net.Server.serve a.port a.host (WebDav.handler cfg)
