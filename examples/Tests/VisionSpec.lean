import LeanTea

/-! # examples/Tests/VisionSpec.lean — offline checks for LeanTea.Vision

Everything a vision test relies on that does *not* need a live model:
coordinate parsing and frame conversion, zoom-window maths, yes/no
parsing, the stb-backed image ops, ANSI→HTML for the tmux driver, the
new Script actions' JSON round-trip, and answer-cache replay (proved
by pointing the client at a dead port). -/

open LeanTea LeanTea.LSpec LeanTea.Vision
open Lean (Json)

def approx (a b : Float) (eps : Float := 0.01) : Bool := (a - b).abs < eps

def isBox : Option RawCoords → Float → Float → Float → Float → Bool
  | some (.box a b c d), a', b', c', d' => a == a' && b == b' && c == c' && d == d'
  | _, _, _, _, _ => false

def isPoint : Option RawCoords → Float → Float → Bool
  | some (.point a b), a', b' => a == a' && b == b'
  | _, _, _ => false

def rectIs (r : Rect) (x y w h : Float) : Bool :=
  approx r.x x && approx r.y y && approx r.w w && approx r.h h

/-- 4×2 test image: left half red, right half blue. -/
def testImage : Image := Id.run do
  let mut px := ByteArray.empty
  for _ in [0:2] do
    for i in [0:4] do
      px := if i < 2 then px.push 255 |>.push 0 |>.push 0 |>.push 255
            else px.push 0 |>.push 0 |>.push 255 |>.push 255
  return { width := 4, height := 2, rgba := px }

def parseGroup : LSpec :=
  let qwen3 := parseCoords "{\"bbox_2d\": [430, 337, 470, 374]}"
  let fenced := parseCoords "```json\n[{\"bbox_2d\": [10, 20, 30, 40], \"label\": \"x\"}]\n```"
  let holo := parseCoords "{\"x\": 512, \"y\": 300}"
  let think := parseCoords "<think>maybe 1 2 3 4</think>{\"point_2d\": [500, 250]}"
  let junk := parseCoords "I cannot find it."
  let box1000 := (qwen3.getD (.point 0 0)).toRect .norm1000 1280 800
  let boxAbs := (RawCoords.box 100 50 140 70).toRect .absolute 1280 800
  let boxYx := (RawCoords.box 250 500 500 750).toRect .norm1000yx 1000 1000
  let pt := (holo.getD (.box 0 0 0 0)).toRect .norm1000 1000 1000
  group "Ground parsing" [
    it "qwen3 bbox_2d parses as box" (isBox qwen3 430 337 470 374),
    it "fenced JSON with label parses" (isBox fenced 10 20 30 40),
    it "holo {x,y} parses as point" (isPoint holo 512 300),
    it "think block is ignored" (isPoint think 500 250),
    it "no numbers → none" junk.isNone,
    it "norm1000 scales to image px" (rectIs box1000 550.4 269.6 51.2 29.6),
    it "absolute passes through" (rectIs boxAbs 100 50 40 20),
    it "norm1000yx swaps axes" (rectIs boxYx 500 250 250 250),
    it "point → zero-size rect at point" (rectIs pt 512 300 0 0),
    it "decimals and negatives" (scanNumbers "a -1.5 b 2.25".toList == [-1.5, 2.25]),
    it "frame names round-trip" (CoordFrame.all.all (fun f => CoordFrame.ofName? f.name == some f)),
    it "qwen2.5 guessed absolute" (CoordFrame.guess "qwen/qwen2.5-vl-7b" == .absolute),
    it "qwen3 guessed norm1000" (CoordFrame.guess "qwen/qwen3-vl-4b" == .norm1000)
  ]

def geometryGroup : LSpec :=
  let r : Rect := { x := 10, y := 10, w := 20, h := 10 }
  let img : Image := { width := 1280, height := 800, rgba := .empty }
  let (x0, y0, cw, ch) := zoomWindow img 640 400 3
  let (ex, ey, _, _) := zoomWindow img 1275 5 3
  group "Geometry" [
    it "rect contains centre" (r.contains r.cx r.cy),
    it "rect excludes outside" (!r.contains 5 5),
    it "iou self = 1" (approx (r.iou r) 1),
    it "iou disjoint = 0" (approx (r.iou { x := 100, y := 100, w := 5, h := 5 }) 0),
    it "zoom window is 1/3 and centred" (cw == 426 && ch == 266 && x0 == 427 && y0 == 267),
    it "zoom window clamps at edges" (ex == 1280 - 426 && ey == 0)
  ]

def yesNoGroup : LSpec :=
  group "Yes/no parsing" [
    it "json yes" (parseYesNo "{\"answer\": \"yes\", \"reason\": \"no doubt\"}" == some true),
    it "json no" (parseYesNo "{\"answer\": \"no\", \"reason\": \"yes it is absent\"}" == some false),
    it "bare Yes." (parseYesNo "Yes." == some true),
    it "neither" (parseYesNo "maybe" == none)
  ]

def scriptGroup : LSpec :=
  let src := "{\"name\":\"t\",\"steps\":[" ++
    "{\"act\":\"click_described\",\"target\":\"the OK button\",\"key\":\"app.ok\"}," ++
    "{\"act\":\"assert_visual\",\"question\":\"Is it red?\",\"want\":false}," ++
    "{\"act\":\"assert_text\",\"target\":\"the title\",\"equals\":\"Hello\"}," ++
    "{\"act\":\"assert_pixel\",\"x\":3,\"y\":4,\"rgb\":\"#ff0000\",\"tol\":5}," ++
    "{\"act\":\"key\",\"key\":\"Enter\"},{\"act\":\"type\",\"text\":\"abc\"}]}"
  let parsed := (Json.parse src).toOption.bind (fun j => (Agent.Script.Script.fromJson j).toOption)
  let again := parsed.bind (fun s => (Agent.Script.Script.fromJson s.toJson).toOption)
  group "Script vision actions" [
    it "parses all six" ((parsed.map (·.steps.length)) == some 6),
    it "JSON round-trip is stable" ((again.map (·.toJson.compress)) == (parsed.map (·.toJson.compress)))
  ]

def ansiGroup : LSpec :=
  let html := ansiToHtml "a\x1b[1;31mB\x1b[0m<c\x1b[48;5;21m \x1b[m"
  group "tmux ANSI → HTML" [
    it "bold red span" ((html.splitOn "<span style=\"color:#cd0000;font-weight:bold;\">B</span>").length == 2),
    it "html escaped" ((html.splitOn "&lt;c").length == 2),
    it "256-colour background" ((html.splitOn "background:rgb(0,0,255)").length == 2),
    it "xterm cube corner" (xterm256 231 == "rgb(255,255,255)")
  ]

def run : IO LSpec := do
  -- Image ops (C / stb)
  let png ← testImage.encodePng
  let back ← Image.decode png
  let crop := back.crop 1 0 2 2
  let big := testImage.resize 8 4
  let clamped := testImage.crop 3 1 10 10
  let imgGroup := group "Image (stb FFI)" [
    it "PNG round-trip keeps size" (back.width == 4 && back.height == 2),
    it "PNG round-trip keeps pixels" (back.rgba == testImage.rgba),
    it "pixel read" (back.pixel 0 0 == (255, 0, 0, 255) && back.pixel 3 1 == (0, 0, 255, 255)),
    it "out of range pixel is transparent" (back.pixel 9 9 == (0, 0, 0, 0)),
    it "crop straddles the colour edge" (crop.pixel 0 0 == (255, 0, 0, 255) && crop.pixel 1 0 == (0, 0, 255, 255)),
    it "crop clamps to bounds" (clamped.width == 1 && clamped.height == 1),
    it "resize ×2 keeps corners" (big.width == 8 && big.pixel 0 0 == (255, 0, 0, 255) && big.pixel 7 3 == (0, 0, 255, 255)),
    it "IHDR size probe" (pngSize png == (4, 2))
  ]
  -- Cache replay: record an answer, then read it back with no server.
  let dir : System.FilePath := ".lake/vision-spec-cache"
  if ← dir.pathExists then IO.FS.removeDirAll dir
  let v : Vlm := { cfg := { baseUrl := "http://127.0.0.1:1/v1", timeoutSec := some 2 },
                   model := "fixture-model", cacheDir? := some dir }
  let url ← testImage.dataUrl
  let key := hash (v.model ++ "\x00" ++ locatePrompt "the red half" v.model ++ "\x00" ++ url)
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / s!"{key}.json") "{\"text\":\"{\\\"bbox_2d\\\": [0, 0, 500, 1000]}\",\"ms\":7}"
  let r ← locate v testImage "the red half"
  let replay := group "Answer cache replay (no server)" [
    it "replayed box" (r.rect?.map (fun b => rectIs b 0 0 2 2) == some true),
    it "replayed latency" (r.ms == 7)
  ]
  IO.FS.removeDirAll dir
  return group "LeanTea.Vision" [parseGroup, geometryGroup, yesNoGroup, scriptGroup, ansiGroup, imgGroup, replay]

def main : IO Unit := do
  let code ← lspecIO (← run)
  if code != 0 then IO.Process.exit code.toUInt8
