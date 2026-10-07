import LeanTea

/-! # vision_qa_reversi — a vision QA test written in plain Lean

The same checks as `examples/VisionQa/scripts/reversi.json`, but as a
Lean program: `LeanTea.Vision.Driver` for the screen, `Ground` for the
VLM, `LSpec` for the verdicts. Use this shape when a test needs
control flow (loops, branching on what the screen shows) that a flat
JSON script can't express.

```
$ ./.lake/build/bin/reversi_serve --port 8005 &
$ ./.lake/build/bin/vision_qa_reversi [--url http://127.0.0.1:8005/] [--model qwen/qwen3-vl-4b]
``` -/

open LeanTea LeanTea.LSpec LeanTea.Vision

namespace VisionQaReversi

partial def argOr (key dflt : String) : List String → String
  | k :: v :: rest => if k == key then v else argOr key dflt (v :: rest)
  | _ => dflt

def main (argv : List String) : IO UInt32 := do
  let url := argOr "--url" "http://127.0.0.1:8005/" argv
  let vlm : Vlm := { model := argOr "--model" "qwen/qwen3-vl-4b" argv }
  let dir : System.FilePath := "/tmp/vision-qa-reversi"
  IO.FS.createDirAll dir
  let s ← Browser.Session.spawn
  try
    let _ ← s.open 1280 800 (headless := some true)
    let _ ← s.navigate url
    let drv := Driver.browser s
    let n ← IO.mkRef (0 : Nat)
    let shot : IO Image := do
      n.modify (· + 1)
      let p := dir / s!"shot{← n.get}.png"
      drv.screenshot p
      Image.load p
    let status (img : Image) : IO String := do
      let some r := (← locateZoom vlm img "the status bar text directly below the game board").rect?
        | return "<status bar not found>"
      return (← readText vlm img r).text

    -- Opening position
    let img0 ← shot
    let st0 ← status img0
    let felt := img0.pixel 440 150

    -- x plays the cell above the centre-left o; it flips that o
    drv.click 613 270
    IO.sleep 300
    let img1 ← shot
    let st1 ← status img1
    let moreO ← ask vlm img1 "Are there two or more o pieces on the board?"

    -- Reset, located by description
    let resetPt ← match (← locateZoom vlm img1 "the Reset button").rect? with
      | some r => do
        drv.click r.cx.round.toUInt64.toNat r.cy.round.toUInt64.toNat
        IO.sleep 300
        pure true
      | none => pure false
    let st2 ← status (← shot)

    let tree := group "Reversi via a local VLM" [
      group "opening" [
        it s!"status reads \"x 2 - o 2 - next x\" (got \"{st0}\")" (st0 == "x 2 - o 2 - next x"),
        it "board felt is #065f46 (pixel, no model)" (felt == (6, 95, 70, 255))
      ],
      group "after x's first move" [
        it s!"status reads \"x 4 - o 1 - next o\" (got \"{st1}\")" (st1 == "x 4 - o 1 - next o"),
        it "last move highlighted #047857 (pixel)" (img1.pixel 596 250 == (4, 120, 87, 255)),
        it s!"VLM: fewer than two o pieces ({(moreO.answer.replace "\n" " ").take 60}…)" (moreO.yes? == some false)
      ],
      group "reset" [
        it "Reset button located" resetPt,
        it s!"status back to start (got \"{st2}\")" (st2 == "x 2 - o 2 - next x")
      ]
    ]
    s.close
    let code ← lspecIO tree
    return (if code == 0 then (0 : UInt32) else 3)
  catch e =>
    s.close
    throw e

end VisionQaReversi

def main (argv : List String) : IO UInt32 := VisionQaReversi.main argv
