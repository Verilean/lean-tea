import LeanTea.Llm.Openai
import LeanTea.Vision.Image

/-! # LeanTea.Vision.Ground — ask a local VLM *where* and *whether*

The primitives a pixel-precise GUI test needs from a vision model:

* `locate`   — "where is the Save button?" → a `Rect` in screenshot pixels
* `locateZoom` — coarse `locate`, then crop around the guess, upscale,
                 and ask again. The second pass is what gets small (12–24 px)
                 targets right with a 2–4B model.
* `ask`      — yes/no visual assertion with a short reason
* `readText` — transcribe the text inside a region

Models disagree on how they emit coordinates, so `CoordFrame` makes it
explicit: Qwen3-VL answers in 0–1000 normalised units, Qwen2.5-VL in
absolute pixels of the image it saw, others vary. `vision_bench`
measures which frame each model actually uses.

Talks to any OpenAI-compatible server (LM Studio by default) through
`LeanTea.Llm.Openai`. -/

namespace LeanTea.Vision

open LeanTea.Llm.Openai

/-- How a model's raw numbers map onto the image. -/
inductive CoordFrame where
  /-- `[x1, y1, x2, y2]` in 0–1000 of the image width/height (Qwen3-VL). -/
  | norm1000
  /-- `[y1, x1, y2, x2]` in 0–1000 (PaliGemma / Gemma convention). -/
  | norm1000yx
  /-- `[x1, y1, x2, y2]` in 0–1. -/
  | norm1
  /-- `[x1, y1, x2, y2]` in pixels of the image as sent (Qwen2.5-VL). -/
  | absolute
  deriving Inhabited, Repr, BEq

def CoordFrame.all : List CoordFrame := [.norm1000, .norm1000yx, .norm1, .absolute]

def CoordFrame.name : CoordFrame → String
  | .norm1000 => "norm1000" | .norm1000yx => "norm1000yx"
  | .norm1 => "norm1" | .absolute => "absolute"

def CoordFrame.ofName? : String → Option CoordFrame
  | "norm1000" => some .norm1000 | "norm1000yx" => some .norm1000yx
  | "norm1" => some .norm1 | "absolute" => some .absolute
  | _ => none

/-- A VLM endpoint plus its coordinate convention. -/
structure Vlm where
  cfg   : Config := { timeoutSec := some 300 }
  model : String
  frame : CoordFrame := .norm1000
  maxTokens : Nat := 256
  /-- When set, answers are memoised on disk keyed by
      `(model, prompt, image)` — re-runs of a bench or a recorded test
      replay without touching the server. -/
  cacheDir? : Option System.FilePath := none
  deriving Inhabited

/-- Best-known frame for a model id; `vision_bench` is the authority. -/
def CoordFrame.guess (model : String) : CoordFrame :=
  let m := model.toLower
  if (m.splitOn "qwen2.5-vl").length > 1 || (m.splitOn "ui-tars").length > 1
     || (m.splitOn "holo1").length > 1 then .absolute
  else if (m.splitOn "gemma").length > 1 then .norm1000yx
  else .norm1000

/-! ## Raw answer parsing -/

/-- What the model said, before applying a `CoordFrame`. -/
inductive RawCoords where
  | box   (a b c d : Float)
  | point (a b : Float)
  deriving Inhabited, Repr

private def isDigit (c : Char) : Bool := c.isDigit

/-- Every number in `cs`, in order (`-`, decimals allowed; no exponents). -/
partial def scanNumbers (cs : List Char) : List Float :=
  let rec digits (cs : List Char) (acc : Nat) (n : Nat) : Nat × Nat × List Char :=
    match cs with
    | c :: rest => if isDigit c then digits rest (acc * 10 + (c.toNat - '0'.toNat)) (n + 1)
                   else (acc, n, cs)
    | [] => (acc, n, [])
  let rec go (cs : List Char) (out : Array Float) : Array Float :=
    match cs with
    | [] => out
    | c :: rest =>
      let (neg, body) := if c == '-' then (true, rest) else (false, c :: rest)
      match body with
      | d :: _ =>
        if isDigit d then
          let (ip, _, after) := digits body 0 0
          let (frac, after') := match after with
            | '.' :: more =>
              let (fp, fn, after2) := digits more 0 0
              (fp.toFloat / (10.0 ^ fn.toFloat), after2)
            | _ => (0.0, after)
          let v := ip.toFloat + frac
          go after' (out.push (if neg then -v else v))
        else go rest out
      | [] => out
  (go cs #[]).toList

private def findSub (hay : List Char) (needle : List Char) : Option (List Char) :=
  match hay with
  | [] => none
  | _ :: rest => if needle.isPrefixOf hay then some (hay.drop needle.length) else findSub rest needle

/-- Remove `<think>…</think>` so reasoning-model chatter can't leak
    numbers into the parse. -/
def stripThink (s : String) : String :=
  match s.splitOn "</think>" with
  | [_, after] => after
  | _ => s

/-- Pull coordinates out of a free-form answer. Prefers a keyed value
    (`bbox_2d`, `box_2d`, `bbox`, `point_2d`, `point`), else the first
    4 (box) or 2 (point) numbers anywhere. -/
def parseCoords (answer : String) : Option RawCoords :=
  let cs := (stripThink answer).toList
  let fromNums (ns : List Float) (preferPoint : Bool) : Option RawCoords :=
    match ns with
    | a :: b :: c :: d :: _ => if preferPoint then some (.point a b) else some (.box a b c d)
    | [a, b] => some (.point a b)
    | _ => none
  let keyed : List (String × Bool) :=
    [("bbox_2d", false), ("box_2d", false), ("\"bbox\"", false),
     ("point_2d", true), ("\"point\"", true), ("<box>", false), ("<point>", true)]
  let rec tryKeys : List (String × Bool) → Option RawCoords
    | [] => fromNums (scanNumbers cs) false
    | (k, pt) :: rest =>
      match findSub cs k.toList with
      | some after =>
        match fromNums ((scanNumbers after).take (if pt then 2 else 4)) pt with
        | some r => some r
        | none => tryKeys rest
      | none => tryKeys rest
  tryKeys keyed

/-- Apply a frame. `w`/`h` are the dimensions of the image the model saw. -/
def RawCoords.toRect (r : RawCoords) (frame : CoordFrame) (w h : Nat) : Rect :=
  let W := w.toFloat
  let H := h.toFloat
  let conv (x1 y1 x2 y2 : Float) : Rect :=
    let (x1, y1, x2, y2) := match frame with
      | .norm1000   => (x1 * W / 1000, y1 * H / 1000, x2 * W / 1000, y2 * H / 1000)
      | .norm1000yx => (y1 * W / 1000, x1 * H / 1000, y2 * W / 1000, x2 * H / 1000)
      | .norm1      => (x1 * W, y1 * H, x2 * W, y2 * H)
      | .absolute   => (x1, y1, x2, y2)
    { x := min x1 x2, y := min y1 y2, w := (x2 - x1).abs, h := (y2 - y1).abs }
  match r with
  | .box a b c d => conv a b c d
  | .point a b   => conv a b a b

/-! ## Calls -/

/-- One VLM round-trip result, with timing for the bench. -/
structure Answer where
  text : String
  ms   : Nat
  deriving Inhabited

private def Vlm.callUncached (v : Vlm) (prompt : String) (imgUrl : String) : IO Answer := do
  let t0 ← IO.monoMsNow
  let res ← chat v.cfg {
    model := v.model,
    messages := [userTextAndImage prompt imgUrl],
    temperature := some 0.0,
    maxTokens := some v.maxTokens
  }
  let t1 ← IO.monoMsNow
  return { text := stripThink res.content, ms := t1 - t0 }

def Vlm.call (v : Vlm) (prompt : String) (imgUrl : String) : IO Answer := do
  match v.cacheDir? with
  | none => v.callUncached prompt imgUrl
  | some dir =>
    -- `maxTokens` joins the key only when non-default, so caches
    -- recorded at the default stay valid.
    let budget := if v.maxTokens == 256 then "" else s!"@{v.maxTokens}"
    let key := hash (v.model ++ budget ++ "\x00" ++ prompt ++ "\x00" ++ imgUrl)
    let path := dir / s!"{key}.json"
    if ← path.pathExists then
      if let .ok j := Lean.Json.parse (← IO.FS.readFile path) then
        let text := (j.getObjValD "text").getStr?.toOption.getD ""
        let ms := (j.getObjValD "ms").getNat?.toOption.getD 0
        return { text, ms }
    let a ← v.callUncached prompt imgUrl
    IO.FS.createDirAll dir
    IO.FS.writeFile path (Lean.Json.mkObj [
      ("model", .str v.model), ("text", .str a.text), ("ms", .num (Int.ofNat a.ms))]).compress
    return a

/-- Generic grounding prompt (Qwen-VL style bbox). Holo models were
    trained on a click-point prompt instead, so they get their own. -/
def locatePrompt (target : String) (model : String := "") : String :=
  if (model.toLower.splitOn "holo").length > 1 then
    "Localize an element on the GUI image according to the provided target and output a click position.\n" ++
    " * You must output a valid JSON following the format: {\"x\": <int 0-1000>, \"y\": <int 0-1000>}\n" ++
    s!" Your target is:\n{target}"
  else
    s!"Locate {target} in this screenshot. Output its bounding box in JSON format: " ++
    "{\"bbox_2d\": [x1, y1, x2, y2]}. Output only the JSON."

/-- Result of a locate: the rect (if parseable) plus raw evidence. -/
structure Located where
  rect?  : Option Rect
  raw?   : Option RawCoords
  answer : String
  ms     : Nat
  deriving Inhabited

def locateUrl (v : Vlm) (imgUrl : String) (w h : Nat) (target : String) : IO Located := do
  let a ← v.call (locatePrompt target v.model) imgUrl
  let raw? := parseCoords a.text
  return { rect? := raw?.map (·.toRect v.frame w h), raw?, answer := a.text, ms := a.ms }

def locate (v : Vlm) (img : Image) (target : String) : IO Located := do
  locateUrl v (← img.dataUrl) img.width img.height target

/-- Window for the second pass: `1/zoom` of each dimension (at least
    `minSide` px), centred on `(cx, cy)`, clamped inside the image. -/
def zoomWindow (img : Image) (cx cy : Float) (zoom : Nat) (minSide : Nat := 160)
    : Nat × Nat × Nat × Nat :=
  let cw := min img.width (max minSide (img.width / zoom))
  let ch := min img.height (max minSide (img.height / zoom))
  let clampStart (c : Float) (size full : Nat) : Nat :=
    let s := c - size.toFloat / 2
    let s := if s < 0 then 0 else s.toUInt64.toNat
    min s (full - size)
  (clampStart cx cw img.width, clampStart cy ch img.height, cw, ch)

/-- Two-pass locate. The crop is upscaled by `zoom` so the model sees
    the neighbourhood at roughly full-screenshot resolution again.
    Falls back to the coarse answer if the fine pass doesn't parse. -/
def locateZoom (v : Vlm) (img : Image) (target : String) (zoom : Nat := 3) : IO Located := do
  let coarse ← locate v img target
  match coarse.rect? with
  | none => return coarse
  | some r =>
    let (x0, y0, cw, ch) := zoomWindow img r.cx r.cy zoom
    let crop := (img.crop x0 y0 cw ch).resize (cw * zoom) (ch * zoom)
    let fine ← locate v crop target
    match fine.rect? with
    | none => return { coarse with ms := coarse.ms + fine.ms }
    | some fr =>
      let z := zoom.toFloat
      let mapped : Rect := { x := x0.toFloat + fr.x / z, y := y0.toFloat + fr.y / z,
                             w := fr.w / z, h := fr.h / z }
      return { rect? := some mapped, raw? := fine.raw?,
               answer := coarse.answer ++ "\n--zoom--\n" ++ fine.answer,
               ms := coarse.ms + fine.ms }

/-- Yes/no visual assertion. `none` when the answer is neither. -/
structure Verdict where
  yes?   : Option Bool
  answer : String
  ms     : Nat
  deriving Inhabited

def parseYesNo (s : String) : Option Bool :=
  let t := s.trimAscii.toString.toLower
  let t := match t.splitOn "\"answer\"" with
    | [_, after] => after
    | _ => t
  let yi := (t.splitOn "yes").head!.length
  let ni := (t.splitOn "no").head!.length
  let hasY := (t.splitOn "yes").length > 1
  let hasN := (t.splitOn "no").length > 1
  if hasY && (!hasN || yi < ni) then some true
  else if hasN then some false
  else none

def ask (v : Vlm) (img : Image) (question : String) : IO Verdict := do
  let prompt := s!"Look at this screenshot and answer the question with yes or no.\n" ++
    s!"Question: {question}\n" ++
    "Reply in JSON: {\"answer\": \"yes\" or \"no\", \"reason\": \"<short reason>\"}"
  let a ← v.call prompt (← img.dataUrl)
  return { yes? := parseYesNo a.text, answer := a.text, ms := a.ms }

/-- Pick which of `candidates` the screen shows, or `"unknown"`.
    Case-insensitive prefix match — small models add prose after the label. -/
def classify (v : Vlm) (img : Image) (candidates : List String) : IO String := do
  let listed := String.intercalate ", " candidates
  let prompt := s!"You are a screen classifier. The screenshot shows ONE of these screens: {listed}. " ++
    "Reply with EXACTLY one of those names, nothing else. If the screen doesn't clearly match any candidate, reply exactly: unknown"
  let a ← v.call prompt (← img.dataUrl)
  let raw := a.text.trimAscii.toString.toLower
  return (candidates.find? (fun c => raw.startsWith c.toLower)).getD "unknown"

/-- Transcribe the text inside `r` (slightly padded). The crop is
    upscaled until its short side is at least `minSide` px — tiny
    crops (a 20 px spreadsheet cell) otherwise come back garbled. -/
def readText (v : Vlm) (img : Image) (r : Rect) (pad : Nat := 2) (minSide : Nat := 96) : IO Answer := do
  let x := r.x.toUInt64.toNat - min pad r.x.toUInt64.toNat
  let y := r.y.toUInt64.toNat - min pad r.y.toUInt64.toNat
  let crop := img.crop x y (r.w.ceil.toUInt64.toNat + 2 * pad) (r.h.ceil.toUInt64.toNat + 2 * pad)
  let short := max 1 (min crop.width crop.height)
  let zoom := max 2 ((minSide + short - 1) / short)
  let crop := crop.resize (crop.width * zoom) (crop.height * zoom)
  let a ← v.call "Transcribe the text in this image exactly. Output only the text, nothing else."
    (← crop.dataUrl)
  return { a with text := a.text.trimAscii.toString }

end LeanTea.Vision
