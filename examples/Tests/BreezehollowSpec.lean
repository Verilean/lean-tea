import LeanTea
import Breezehollow.Game

/-! # breezehollow_spec — offline checks for the Breezehollow demo

No browser and no network: the game needs a GPU and three.js from a
CDN, so this only guards the build side.

  * `World.leanjs` parses and passes the LeanJs checks (arity of every
    extern call, record fields)
  * the emitted module imports three.js and the bloom addons by the
    bare names `page.html`'s import map provides
  * `node --check` accepts the module as valid JavaScript
  * every element id the client looks up exists in `page.html`, and
    the page loads `/world.js` as a module -/

open LeanTea LeanTea.LSpec

def has (h n : String) : Bool := (h.splitOn n).length > 1

/-- Ids passed to `byId("…")` in the LeanJs source. -/
def usedIds (src : String) : List String :=
  ((src.splitOn "byId(\"").drop 1).filterMap fun chunk =>
    (chunk.splitOn "\"").head?

def main : IO UInt32 := do
  let src ← Breezehollow.loadSource
  let page ← IO.FS.readFile "examples/Breezehollow/page.html"
  let compiled := Breezehollow.compileSource src
  let js := match compiled with | .ok js => js | .error _ => ""
  let tmp : System.FilePath := s!"/tmp/breezehollow-spec-{← IO.rand 0 0xffffff}.mjs"
  IO.FS.writeFile tmp js
  let check ← IO.Process.output { cmd := "node", args := #["--check", tmp.toString] }
  IO.FS.removeFile tmp
  let ids := (usedIds src).eraseDups
  let missing := ids.filter (fun id => !has page s!"id=\"{id}\"")
  if !missing.isEmpty then IO.eprintln s!"ids missing from page.html: {missing}"
  if check.exitCode != 0 then IO.eprintln check.stderr
  let tree := group "Breezehollow" [
    group "LeanJs build" [
      it "World.leanjs parses and passes the checks"
        (match compiled with | .ok _ => true | .error _ => false),
      it "imports three.js by bare name" (has js "import * as THREE from 'three';"),
      it "imports the bloom post-processing addons"
        (has js "from 'three/addons/postprocessing/UnrealBloomPass.js'" &&
         has js "from 'three/addons/postprocessing/EffectComposer.js'"),
      it "boots itself (no console.log(main) tail)" (js.endsWith "boot();" && !has js "console.log(main"),
      it "node --check accepts the module" (check.exitCode == 0)
    ],
    group "page.html" [
      it "import map provides three and three/addons/"
        (has page "type=\"importmap\"" && has page "\"three\":" && has page "\"three/addons/\":"),
      it "loads /world.js as an ES module" (has page "<script type=\"module\" src=\"/world.js\">"),
      it s!"every id the client uses exists ({ids.length} ids)" (ids.length > 5 && missing.isEmpty)
    ]
  ]
  let code ← lspecIO tree
  return if code == 0 then 0 else 1
