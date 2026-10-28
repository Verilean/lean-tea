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
    the page loads `/world.js` as a module
  * `heroine.glb` is a glTF binary with every clip the game plays and
    every spring-chain and collider bone `World.leanjs` names
  * the movement rules (`Physics.leanjs`, a pure `step`) behave:
    `PhysicsTest.leanjs` runs them under node on toy worlds and each
    `PASS` / `FAIL` line becomes a test here -/

open LeanTea LeanTea.LSpec

def has (h n : String) : Bool := (h.splitOn n).length > 1

/-- Ids passed to `byId("…")` in the LeanJs source. -/
def usedIds (src : String) : List String :=
  ((src.splitOn "byId(\"").drop 1).filterMap fun chunk =>
    (chunk.splitOn "\"").head?

/-- The JSON chunk of a `.glb` (header: magic, version, length; then
    the first chunk: length, type "JSON", data). -/
def glbJson (b : ByteArray) : Option String := do
  let u32 (i : Nat) : Nat :=
    b[i]!.toNat ||| (b[i+1]!.toNat <<< 8) ||| (b[i+2]!.toNat <<< 16) ||| (b[i+3]!.toNat <<< 24)
  guard (b.size > 20 && u32 0 == 0x46546C67 && u32 4 == 2 && u32 16 == 0x4E4F534A)
  let n := u32 12
  guard (b.size ≥ 20 + n)
  String.fromUTF8? (b.extract 20 (20 + n))

/-- `key: "value"` occurrences in the LeanJs source, e.g. the chain names. -/
def quotedAfter (src key : String) : List String :=
  ((src.splitOn s!"{key}: \"").drop 1).filterMap fun chunk => (chunk.splitOn "\"").head?

/-- Bones each spring chain needs: `name: "hair_C", n: 4` → hair_C_0 … hair_C_4. -/
def chainBones (src : String) : List String :=
  ((src.splitOn "{name: \"").drop 1).flatMap fun chunk =>
    match chunk.splitOn "\", n: " with
    | name :: rest :: _ =>
      let n := (rest.takeWhile Char.isDigit).toString.toNat?.getD 0
      (List.range (n + 1)).map (s!"{name}_{·}")
    | _ => []

/-- Compile `PhysicsTest.leanjs`, run it with node, one test per line. -/
def physicsTests : IO (List LSpec) := do
  match ← Breezehollow.compileFile (← Breezehollow.findFile "PhysicsTest.leanjs") with
  | .error e => return [it "PhysicsTest.leanjs compiles" false e]
  | .ok js =>
    let tmp : System.FilePath := s!"/tmp/breezehollow-physics-{← IO.rand 0 0xffffff}.js"
    IO.FS.writeFile tmp js
    let out ← IO.Process.output { cmd := "node", args := #[tmp.toString] }
    IO.FS.removeFile tmp
    if out.exitCode != 0 then return [it "PhysicsTest.leanjs runs" false out.stderr]
    let lines := (out.stdout.splitOn "\n").filter (!·.isEmpty)
    return lines.map fun l =>
      if l.startsWith "PASS " then it (l.drop 5).toString true
      else it ((l.drop 5).toString.takeWhile (· != '{')).toString false l

def main : IO UInt32 := do
  let src ← IO.FS.readFile (← Breezehollow.findFile "World.leanjs")
  let page ← IO.FS.readFile "examples/Breezehollow/page.html"
  let compiled ← Breezehollow.compileFile (← Breezehollow.findFile "World.leanjs")
  let js := match compiled with | .ok js => js | .error _ => ""
  let tmp : System.FilePath := s!"/tmp/breezehollow-spec-{← IO.rand 0 0xffffff}.mjs"
  IO.FS.writeFile tmp js
  let check ← IO.Process.output { cmd := "node", args := #["--check", tmp.toString] }
  IO.FS.removeFile tmp
  let ids := (usedIds src).eraseDups
  let missing := ids.filter (fun id => !has page s!"id=\"{id}\"")
  if !missing.isEmpty then IO.eprintln s!"ids missing from page.html: {missing}"
  if check.exitCode != 0 then IO.eprintln check.stderr
  let glb := glbJson (← IO.FS.readBinFile (← Breezehollow.findFile "heroine.glb"))
  let gj := glb.getD ""
  let clips := ["idle", "run", "glide", "hover", "fly", "fall"]
  let bones := (chainBones src ++ quotedAfter src "bone").eraseDups
  let missingBones := bones.filter (fun b => !has gj s!"\"name\":\"{b}\"")
  if !missingBones.isEmpty then IO.eprintln s!"bones missing from heroine.glb: {missingBones}"
  let tree := group "Breezehollow" [
    group "LeanJs build" [
      it "World.leanjs (+ Physics.leanjs) parses and passes the checks"
        (match compiled with | .ok _ => true | .error _ => false)
        (match compiled with | .ok _ => "" | .error e => e),
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
    ],
    group "heroine.glb" [
      it "is a glTF 2.0 binary" glb.isSome,
      it "has every clip the game plays" (clips.all fun c => has gj s!"\"name\":\"{c}\""),
      it s!"has every spring-chain and collider bone World.leanjs names ({bones.length})"
        (bones.length > 30 && missingBones.isEmpty),
      it "carries its texture as an embedded PNG"
        (has gj "\"baseColorTexture\"" && has gj "\"mimeType\":\"image/png\""),
      it "is loaded with GLTFLoader from /heroine.glb"
        (has js "from 'three/addons/loaders/GLTFLoader.js'" && has js "/heroine.glb")
    ],
    group "Physics.step (node, toy worlds)" (← physicsTests)
  ]
  let code ← lspecIO tree
  return if code == 0 then 0 else 1
