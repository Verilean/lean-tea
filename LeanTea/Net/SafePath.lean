/-! # LeanTea.Net.SafePath — paths that can't escape their workspace

Closes **Path Traversal** (IPA 「安全なウェブサイトの作り方」§3.4,
OWASP A01 broken access control, shadan-kun "Directory Traversal").

A `SafePath` is the absolute, normalised result of resolving a
caller-supplied filename against a workspace root. Anything that
would resolve outside the workspace — `../etc/passwd`,
`/etc/passwd`, `./../../boot` — is rejected at construction.
The constructor is `private mk`, so the only entry points are
`SafePath.under` (smart) and `SafePath.under!` (panic variant for
literal paths in trusted code).

The check is best-effort against symlinks: if the workspace itself
contains a symlink that points outside, that's on the operator. We
don't `realpath`/`readlink` because the framework targets pure-Lean
deployments without libc.

### Use it like

```
match SafePath.under "/srv/uploads" rawFromUser with
| .ok p    => IO.FS.readFile p.value
| .error e => Response.badRequest e
```

The existing `validatePath` inside `ChromeCdpMcp.Serve` is the
prototype this was lifted from; that call site will switch over
in a follow-up. -/

namespace LeanTea.Net

/-- A path that has been resolved + normalised against a workspace
    root. The `private mk` keeps any `SafePath` construction
    funnelled through `SafePath.under`. -/
structure SafePath where
  private mk ::
  /-- Absolute, normalised path. Guaranteed to live under the
      workspace root that produced it. -/
  value : String
  deriving Inhabited, Repr

namespace SafePath

/-- Normalise an absolute path: drop empty / `.` segments, pop on
    `..`. Idempotent: `normalise (normalise s) = normalise s`. -/
def normalise (raw : String) : String :=
  let parts := raw.splitOn "/"
  let walk (acc : List String) (seg : String) : List String :=
    if seg == "" || seg == "." then acc
    else if seg == ".." then
      match acc with
      | _ :: rest => rest
      | []        => []
    else seg :: acc
  let stack := parts.foldl walk []
  "/" ++ String.intercalate "/" stack.reverse

/-- Resolve `path` against `workspace`. Relative paths are joined to
    the workspace; absolute paths must already live under it. Rejects
    NUL bytes (`\u0000`) — they truncate paths in libc. -/
def under (workspace path : String) : Except String SafePath :=
  if path.contains '\u0000' then
    .error "SafePath.under: NUL byte in path"
  else
    let abs := if path.startsWith "/" then path else workspace ++ "/" ++ path
    let norm := normalise abs
    let wsNorm := normalise workspace
    if norm == wsNorm || norm.startsWith (wsNorm ++ "/") then
      .ok ⟨norm⟩
    else
      .error s!"SafePath.under: {path} escapes workspace {wsNorm}"

/-- Panic variant for literal paths in trusted code (config files,
    tests). Do **not** pass user-controlled paths here — that
    defeats the type. -/
def under! (workspace path : String) : SafePath :=
  match under workspace path with
  | .ok p    => p
  | .error e => panic! s!"SafePath.under!: {e}"

instance : ToString SafePath where
  toString p := p.value

end SafePath

end LeanTea.Net
