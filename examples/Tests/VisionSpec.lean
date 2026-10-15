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
    it "qwen3 guessed norm1000" (CoordFrame.guess "qwen/qwen3-vl-4b" == .norm1000),
    it "every box of a two-match answer"
      (match parseBoxes "```json\n[{\"bbox_2d\": [932, 28, 984, 70], \"label\": \"Skip\"},\n {\"bbox_2d\": [584, 552, 627, 598], \"label\": \"Skip\"}]\n```" with
       | [a, b] => isBox (some a) 932 28 984 70 && isBox (some b) 584 552 627 598
       | _ => false),
    it "brightness tells the lit box from the dimmed one"
      -- testImage: left half red (luma 76), right half blue (luma 29)
      (meanLuma testImage { x := 0, y := 0, w := 2, h := 2 } > meanLuma testImage { x := 2, y := 0, w := 2, h := 2 })
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

/-! ## Explore (qa_studio's trial-and-error agent): proposals and change detection -/

open LeanTea.Agent.Script in
def proposalIs (reply : String) (f : Explore.Proposal → Bool) : Bool :=
  match Explore.parseProposal reply with
  | .ok p => f p
  | .error _ => false

def proposalErr (reply : String) : Bool :=
  match Explore.parseProposal reply with
  | .ok _ => false
  | .error _ => true

open LeanTea.Agent.Script in
def exploreGroup : LSpec :=
  group "Explore proposals" [
    it "JSON object pulled out of prose / fences"
      (Explore.extractObject "Sure!\n```json\n{\"action\": \"key\", \"key\": \"Enter\"}\n```" ==
        some "{\"action\": \"key\", \"key\": \"Enter\"}"),
    it "braces inside strings don't end the object"
      (Explore.extractObject "{\"action\":\"type\",\"text\":\"a}b\"} tail" == some "{\"action\":\"type\",\"text\":\"a}b\"}"),
    it "click → click_described"
      (proposalIs "{\"action\":\"click\",\"target\":\"the Add button\",\"why\":\"submit\"}"
        fun | .step (.clickDescribed "the Add button" none) "submit" => true | _ => false),
    it "fill → target + text"
      (proposalIs "{\"action\":\"fill\",\"target\":\"the Username field\",\"text\":\"alice\"}"
        fun | .fill "the Username field" "alice" _ => true | _ => false),
    it "assert expect no → want false"
      (proposalIs "{\"action\":\"assert\",\"question\":\"Is there an error?\",\"expect\":\"no\"}"
        fun | .step (.assertVisual "Is there an error?" false) _ => true | _ => false),
    it "done / fail" (proposalIs "{\"action\":\"done\",\"why\":\"ok\"}" (fun | .done "ok" => true | _ => false) &&
      proposalIs "{\"action\":\"fail\",\"reason\":\"no form\"}" (fun | .fail "no form" => true | _ => false)),
    it "empty type text, missing target, unknown action are rejected"
      (proposalErr "{\"action\":\"type\",\"text\":\"\"}" && proposalErr "{\"action\":\"click\"}" &&
       proposalErr "{\"action\":\"scroll\"}" && proposalErr "no json here"),
    it "wait → seconds, clamped to 1…10"
      (proposalIs "{\"action\":\"wait\",\"seconds\":3}" (fun | .step (.wait 3000) _ => true | _ => false) &&
       proposalIs "{\"action\":\"wait\",\"seconds\":60}" (fun | .step (.wait 10000) _ => true | _ => false)),
    it "wait_until proposal, seconds clamped to 5..600"
      (proposalIs "{\"action\":\"wait_until\",\"question\":\"Is the result shown?\",\"seconds\":180}"
         (fun | .step (.waitUntil "Is the result shown?" 180000) _ => true | _ => false) &&
       proposalIs "{\"action\":\"wait_until\",\"question\":\"q\",\"seconds\":9999}"
         (fun | .step (.waitUntil "q" 600000) _ => true | _ => false) &&
       proposalErr "{\"action\":\"wait_until\"}"),
    it "wait_until survives the script JSON round trip"
      (match Script.fromJson ({ name := "t", steps := [{ act := .waitUntil "Is TAP TO START visible?" 45000 }] } : Script).toJson with
       | .ok sc => match sc.steps with
         | [{ act := .waitUntil "Is TAP TO START visible?" 45000, .. }] => true
         | _ => false
       | .error _ => false),
    it "a pointed click is gated on what is there"
      (match Explore.pointedSteps (some "the Quest button") 250 295 with
       | [{ act := .waitUntil "Is the Quest button visible on the screen?" _, .. }, { act := .clickXy 250 295, .. }] => true
       | _ => false),
    it "a pointed click with no name is just the click"
      (match Explore.pointedSteps none 1 2 with
       | [{ act := .clickXy 1 2, .. }] => true
       | _ => false),
    it "prompt carries goal, notes, history, the last error and the page outline"
      (let p := Explore.promptFor "sign in" ["be quick"] [.typeText "alice"] (some "typing changed nothing") 5 "button \"Sign in\"";
       [ "Goal: sign in", "- be quick", "1. type \"alice\" — ok", "FAILED: typing changed nothing",
         "button \"Sign in\"", "5 steps left" ].all (fun n => (p.splitOn n).length > 1)),
    it "earlier goals' steps are counted, not listed"
      (let p := Explore.promptFor "skip" [] [] none 5 "" 7;
       (p.splitOn "7 steps for earlier goals").length > 1 && (p.splitOn "(none yet for this goal)").length > 1)
  ]

/-! ## Plans and loops -/

open LeanTea.Agent.Script in
def planGroup : LSpec :=
  let text := "Open the chapter map\n\nrepeat until \"Are all stages cleared?\" max 4:\n  Sortie and win\n  # a comment\n  Click Next\nend\nCheck the reward"
  let parsed := Plan.parse text
  let expected : List Plan.Item :=
    [.goal "Open the chapter map", .loop "Are all stages cleared?" 4 [.goal "Sortie and win", .goal "Click Next"],
     .goal "Check the reward"]
  let fromModel := Plan.fromJson ((Json.parse
    "[\"Open Drafts\", \"Repeat until done\", {\"repeat_until\": \"Is the list empty?\", \"max\": 20, \"steps\": [\"Delete the first\"]}]").toOption.getD .null)
  let loopScript : Script := { name := "t", steps := [
    { act := .repeatStart "Is it done?" 3, goal := some "loop" },
    { act := .repeatStart "Inner?" 2 }, { act := .key "Enter" }, { act := .repeatEnd },
    { act := .repeatEnd }] }
  group "Plans and loops" [
    it "plan text parses, with a loop and a comment" (parsed.toOption == some expected),
    it "render then parse is the same plan" ((Plan.parse (Plan.render expected)).toOption == some expected),
    it "repeat without a max defaults to 10"
      (Plan.parseRepeat "repeat until Is the list empty?:" == some ("Is the list empty?", 10)),
    it "an unclosed repeat is an error" (match Plan.parse "repeat until \"x\" max 2:\n  a" with | .error _ => true | _ => false),
    it "a stray end is an error" (match Plan.parse "a\nend" with | .error _ => true | _ => false),
    it "the model's JSON plan, minus its prose repeat line"
      (fromModel == [.goal "Open Drafts", .loop "Is the list empty?" 20 [.goal "Delete the first"]]),
    it "repeat / end_repeat and goal survive the script JSON round trip"
      (match Script.fromJson loopScript.toJson with
       | .ok sc => match sc.steps with
         | [{ act := .repeatStart "Is it done?" 3, goal := some "loop", .. }, { act := .repeatStart "Inner?" 2, .. },
            { act := .key "Enter", .. }, { act := .repeatEnd, .. }, { act := .repeatEnd, .. }] => true
         | _ => false
       | .error _ => false),
    it "matchingEnd skips a nested loop"
      (matchingEnd loopScript.steps.toArray 0 == some 4 && matchingEnd loopScript.steps.toArray 1 == some 3)
  ]

def elementsGroup : LSpec :=
  -- a cut-off answer that repeats its last line, as Qwen-VL does
  let answer := "[{\"label\": \"Start\", \"bbox_2d\": [100, 100, 200, 200]},\n" ++
    "{\"label\": \"Menu\", \"bbox_2d\": [500, 0, 600, 100]},\n" ++
    "{\"label\": \"Menu\", \"bbox_2d\": [500, 0, 600, 100]},\n{\"label\": \"Me"
  let es := Elements.parse answer .norm1000 1000 500
  let t := Elements.tiles 1000 500
  let a : Elements.Element := { label := "OK", rect := { x := 10, y := 10, w := 40, h := 20 } }
  let lines := Ocr.parse "[{\"text\":\"Start\",\"conf\":1,\"x\":110,\"y\":60,\"w\":40,\"h\":20},{\"text\":\"News\",\"conf\":0.5,\"x\":10,\"y\":300,\"w\":60,\"h\":20},{\"text\":\" \",\"conf\":1,\"x\":0,\"y\":0,\"w\":1,\"h\":1}]"
  let merged := Elements.merge es lines
  group "Screen elements" [
    it "OCR output: blank lines dropped" (lines.map (·.text) == ["Start", "News"]),
    it "OCR text inside a button is the button; other text is listed after the buttons"
      (merged.map (fun e => (e.label, e.kind)) == [("Menu", "button"), ("Start", "button"), ("News", "text")]),
    it "parses labels and boxes" (es.map (·.label) == ["Start", "Menu"]),
    it "boxes in screen pixels" (es.head?.map (fun e => rectIs e.rect 100 50 100 50) == some true),
    it "four overlapping quarters" (t == [(0, 0, 560, 310), (440, 0, 560, 310), (0, 190, 560, 310), (440, 190, 560, 310)]),
    it "a 3×3 grid covers the screen" (let g := Elements.tiles 900 300 3 30
      g.length == 9 && g.head? == some (0, 0, 330, 130) && g.getLast? == some (570, 170, 330, 130)),
    it "same label at the same place is one element"
      (a.same { a with rect := { a.rect with x := 14 } } && !a.same { a with label := "Cancel" }
        && !a.same { a with rect := { a.rect with x := 300 } })
  ]

open LeanTea.Agent.Script in
def scriptTextGroup : LSpec :=
  let steps : List Step := [
    { act := .waitUntil "Is \"Quest\" visible?" 30000, note := "wait for it", goal := some "Open quests" },
    { act := .clickXy 176 711, note := "the user pointed here", goal := some "Open quests" },
    { act := .repeatStart "All cleared?" 5, goal := some "Clear all" },
    { act := .clickDescribed "the next stage # 2", goal := some "Clear all" },
    { act := .assertPixel 1 2 "#ff00aa" 8, goal := some "Clear all" },
    { act := .repeatEnd, goal := some "Clear all" },
    { act := .assertVisual "Error shown?" false },
    { act := .wait 500, expect := some "home" }]
  let text := toText steps
  let back := parseText text
  let same := fun (a b : List Step) => (Script.toJson { name := "x", steps := a }).compress == (Script.toJson { name := "x", steps := b }).compress
  group "Script as text" [
    it "round trip keeps every step, note and goal" (match back with | .ok b => same steps b | .error _ => false),
    it "goals become headings, loops are indented"
      ((text.splitOn "\n").take 4 == ["## Open quests", "wait_until \"Is \\\"Quest\\\" visible?\" 30s  # wait for it",
        "click 176 711  # the user pointed here", ""]
       && (text.splitOn "\n").contains "  click \"the next stage # 2\""),
    it "hand-written lines" (match parseText "click \"Start\"  # go\nwait 2s\ncheck not \"Error?\"\nkey Enter\n" with
      | .ok [a, b, c, d] => a.note == "go" && describe' a.act == "click \"Start\"" && describe' b.act == "wait 2000 ms"
          && describe' c.act == "assert \"Error?\" is no" && describe' d.act == "press Enter"
      | _ => false),
    it "errors name the line" (match parseText "click 1 2\nclik 3 4" with | .error e => e.startsWith "line 2" | .ok _ => false),
    it "an unclosed loop is an error" (match parseText "repeat until \"x\" max 2\nwait 1s" with | .error _ => true | .ok _ => false)
  ]
where describe' := Explore.describe

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
  -- change detection: one differing pixel on the sampling grid counts
  let same := Qa.changedPixels testImage testImage
  let poked : Image := ← Image.decode (← (testImage.crop 0 0 4 2).encodePng)
  let changeGroup := group "Change detection" [
    it "identical screenshots: 0 changed pixels" (same == 0),
    it "different sizes count as changed" (Qa.changedPixels testImage (testImage.crop 0 0 2 2) > 0),
    it "a re-encoded copy is still identical" (Qa.changedPixels testImage poked == 0),
    it "window diff only looks inside the window"
      (Qa.changedPixelsIn testImage testImage 0 0 4 2 == 0 && Qa.changedPixelsIn testImage testImage 3 1 50 50 == 0)
  ]
  return group "LeanTea.Vision" [parseGroup, geometryGroup, yesNoGroup, scriptGroup, ansiGroup, imgGroup, replay,
                                 exploreGroup, changeGroup, planGroup, elementsGroup, scriptTextGroup]

def main : IO Unit := do
  let code ← lspecIO (← run)
  if code != 0 then IO.Process.exit code.toUInt8
