import LeanTea
import Breezehollow.Game

/-! # breezehollow — an anime-style open-world demo on three.js

```
$ lake exe breezehollow            # http://localhost:8010/
$ lake exe breezehollow --dev      # recompile World.leanjs on every request
```

The game client is `World.leanjs`: Lean-shaped source that this
server compiles to an ES module at startup (`Breezehollow.Game`) and
serves as `/world.js`. `page.html` is the HUD plus an import map that
pulls three.js from a CDN; the module imports `three` and the bloom
post-processing addons by bare name. -/

open LeanTea LeanTea.Net.Http LeanTea.Net.Server

namespace BreezehollowServe

def jsResponse (js : String) : Response := {
  status := 200,
  headers := #[("content-type", "application/javascript; charset=utf-8"), ("cache-control", "no-store")],
  body := js.toUTF8 }

def handler (pageProv : Template.Provider) (world : IO (String × Bool)) : Handler := fun req => do
  match req.path with
  | "/" =>
    let (_, isError) ← world
    let banner :=
      if isError then "<pre style=\"position:fixed;top:0;left:0;right:0;z-index:9;margin:0;padding:8px;background:#b42318;color:#fff\">LeanJs compile error — see /world.js</pre>"
      else ""
    return Response.html 200 (← (← pageProv).renderFlat [("errorBanner", banner)])
  | "/world.js" => return jsResponse (← world).1
  | "/favicon.ico" => return { status := 204 }
  | _ => return Response.notFound

private structure Args where
  port : UInt16 := 8010
  host : String := "127.0.0.1"
  dev  : Bool := false

private partial def parseArgs : List String → Args → Args
  | "--port" :: v :: rest, a => parseArgs rest { a with port := (v.toNat?.getD 8010).toUInt16 }
  | "--host" :: v :: rest, a => parseArgs rest { a with host := v }
  | "--dev" :: rest, a       => parseArgs rest { a with dev := true }
  | _ :: rest, a             => parseArgs rest a
  | [], a                    => a

def serveMain (args : List String) : IO Unit := do
  let a := parseArgs args {}
  let pageProv ← Template.mkProvider "examples/Breezehollow/page.html" a.dev
  let first ← Breezehollow.compileWorld
  if first.2 then IO.eprintln s!"breezehollow: {first.1}"
  let world : IO (String × Bool) := if a.dev then Breezehollow.compileWorld else pure first
  IO.println s!"breezehollow: http://{a.host}:{a.port}/{if a.dev then "  [dev: recompiles World.leanjs per request]" else ""}"
  serve a.port a.host (handler pageProv world)

end BreezehollowServe

def main (args : List String) : IO Unit := BreezehollowServe.serveMain args
