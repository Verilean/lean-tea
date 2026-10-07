import LeanTea.Browser
import LeanTea.Net.Desktop

/-! # LeanTea.Vision.Driver — one surface for browser, desktop and TUI

A vision test only needs four things from the thing under test:
capture a PNG, click a screenshot pixel, press a key, type text.
`Driver` packages exactly that, so the same script runs against

* `browser` — headless or headed Chromium via `LeanTea.Browser`
* `desktop` — the real macOS display via `LeanTea.Net.Desktop`
               (Retina: screenshots are in pixels, clicks in points —
               the driver rescales so callers always speak pixels)
* `tmux`    — a terminal pane: `capture-pane -e` → HTML → Chromium
               screenshot, so the VLM sees colours and highlights
               exactly like a human would.

Coordinates passed to `click` are always pixels of the most recent
`screenshot`. -/

namespace LeanTea.Vision

structure Driver where
  name       : String
  screenshot : System.FilePath → IO Unit
  click      : Nat → Nat → IO Unit
  key        : String → IO Unit
  typeText   : String → IO Unit
  close      : IO Unit := pure ()

/-! ## Browser -/

def Driver.browser (s : LeanTea.Browser.Session) : Driver where
  name := "browser"
  screenshot p := do let _ ← s.screenshot (outputPath := some p.toString)
  click x y := s.clickXy x y
  key k := s.press k
  typeText t := for c in t.toList do s.press (if c == ' ' then "Space" else c.toString)

/-! ## macOS desktop -/

private def macKeycode : String → Option UInt32
  | "Enter" | "Return" => some 36 | "Tab" => some 48 | "Space" => some 49
  | "Backspace" => some 51 | "Escape" => some 53 | "Delete" => some 117
  | "ArrowLeft" => some 123 | "ArrowRight" => some 124
  | "ArrowDown" => some 125 | "ArrowUp" => some 126
  | _ => none

private def appleScriptQuote (s : String) : String :=
  "\"" ++ (s.replace "\\" "\\\\" |>.replace "\"" "\\\"") ++ "\""

/-- PNG width/height straight from the IHDR chunk (bytes 16–23). -/
def pngSize (bytes : ByteArray) : Nat × Nat :=
  let be (i : Nat) := (bytes[i]!.toNat <<< 24) ||| (bytes[i+1]!.toNat <<< 16) |||
                      (bytes[i+2]!.toNat <<< 8) ||| bytes[i+3]!.toNat
  if bytes.size < 24 then (0, 0) else (be 16, be 20)

/-- Needs a `LEANTEA_DESKTOP=1` build plus Screen Recording and
    Accessibility permission for the terminal running the test. -/
def Driver.desktop : IO Driver := do
  unless ← LeanTea.Net.Desktop.isAvailable do
    throw <| IO.userError "desktop driver: rebuild with LEANTEA_DESKTOP=1 (macOS)"
  -- pixels-per-point, learned from the last screenshot
  let scale ← IO.mkRef (1.0 : Float)
  return {
    name := "desktop"
    screenshot := fun p => do
      LeanTea.Net.Desktop.screenshot p.toString
      let (pw, _) := pngSize (← IO.FS.readBinFile p)
      let (sw, _) ← LeanTea.Net.Desktop.screenSize
      if sw > 0 && pw > 0 then scale.set (pw.toFloat / sw.toFloat)
    click := fun x y => do
      let k ← scale.get
      LeanTea.Net.Desktop.clickXy (x.toFloat / k).round.toUInt32 (y.toFloat / k).round.toUInt32
    key := fun k => do
      let some code := macKeycode k
        | throw <| IO.userError s!"desktop driver: unknown key {k}"
      LeanTea.Net.Desktop.keyPress code
    typeText := fun t => do
      let script := "tell application \"System Events\" to keystroke " ++ appleScriptQuote t
      let r ← IO.Process.output { cmd := "osascript", args := #["-e", script] }
      if r.exitCode != 0 then throw <| IO.userError s!"osascript: {r.stderr}"
  }

/-! ## tmux pane -/

/-- xterm 256-colour palette entry as CSS. -/
def xterm256 (n : Nat) : String :=
  let base := #["#000000", "#cd0000", "#00cd00", "#cdcd00", "#0000ee", "#cd00cd", "#00cdcd", "#e5e5e5",
                "#7f7f7f", "#ff0000", "#00ff00", "#ffff00", "#5c5cff", "#ff00ff", "#00ffff", "#ffffff"]
  if n < 16 then base[n]!
  else if n < 232 then
    let i := n - 16
    let lv (v : Nat) := if v == 0 then 0 else 55 + v * 40
    s!"rgb({lv (i / 36)},{lv ((i / 6) % 6)},{lv (i % 6)})"
  else
    let g := 8 + (n - 232) * 10
    s!"rgb({g},{g},{g})"

private structure Sgr where
  fg : Option String := none
  bg : Option String := none
  bold : Bool := false
  rev : Bool := false
  deriving BEq

private def applySgr (st : Sgr) (ps : List Nat) : Sgr :=
  match ps with
  | [] => st
  | 0 :: r => applySgr {} r
  | 1 :: r => applySgr { st with bold := true } r
  | 22 :: r => applySgr { st with bold := false } r
  | 7 :: r => applySgr { st with rev := true } r
  | 27 :: r => applySgr { st with rev := false } r
  | 39 :: r => applySgr { st with fg := none } r
  | 49 :: r => applySgr { st with bg := none } r
  | 38 :: 5 :: n :: r => applySgr { st with fg := some (xterm256 n) } r
  | 48 :: 5 :: n :: r => applySgr { st with bg := some (xterm256 n) } r
  | 38 :: 2 :: R :: G :: B :: r => applySgr { st with fg := some s!"rgb({R},{G},{B})" } r
  | 48 :: 2 :: R :: G :: B :: r => applySgr { st with bg := some s!"rgb({R},{G},{B})" } r
  | n :: r =>
    if 30 ≤ n && n ≤ 37 then applySgr { st with fg := some (xterm256 (n - 30)) } r
    else if 40 ≤ n && n ≤ 47 then applySgr { st with bg := some (xterm256 (n - 40)) } r
    else if 90 ≤ n && n ≤ 97 then applySgr { st with fg := some (xterm256 (n - 90 + 8)) } r
    else if 100 ≤ n && n ≤ 107 then applySgr { st with bg := some (xterm256 (n - 100 + 8)) } r
    else applySgr st r

private def sgrStyle (st : Sgr) : String :=
  let (fg, bg) := if st.rev then (st.bg.getD "#0c0c0c", st.fg.getD "#c8c8c8") else (st.fg.getD "", st.bg.getD "")
  (if fg.isEmpty then "" else s!"color:{fg};") ++ (if bg.isEmpty then "" else s!"background:{bg};") ++
  (if st.bold then "font-weight:bold;" else "")

private def escHtml (c : Char) : String :=
  match c with | '<' => "&lt;" | '>' => "&gt;" | '&' => "&amp;" | c => c.toString

/-- Render `capture-pane -e` output (SGR escapes) as a standalone HTML page. -/
partial def ansiToHtml (ansi : String) : String := Id.run do
  let mut out := ""
  let mut st : Sgr := {}
  let mut open_ := false
  let mut cs := ansi.toList
  while !cs.isEmpty do
    match cs with
    | '\x1b' :: '[' :: rest =>
      let params := rest.takeWhile (fun c => c.isDigit || c == ';')
      let rest' := rest.drop params.length
      match rest' with
      | 'm' :: more =>
        let ps := (String.ofList params).splitOn ";" |>.map (fun p => if p.isEmpty then 0 else p.toNat!)
        let st' := applySgr st ps
        if st' != st then
          if open_ then out := out ++ "</span>"
          let style := sgrStyle st'
          open_ := !style.isEmpty
          if open_ then out := out ++ s!"<span style=\"{style}\">"
          st := st'
        cs := more
      | _ :: more => cs := more
      | [] => cs := []
    | c :: rest =>
      out := out ++ escHtml c
      cs := rest
    | [] => pure ()
  if open_ then out := out ++ "</span>"
  return "<!doctype html><meta charset=utf-8><style>html,body{margin:0;background:#0c0c0c}" ++
    "pre{display:inline-block;margin:0;padding:8px;color:#c8c8c8;background:#0c0c0c;" ++
    "font:14px/18px Menlo,'Courier New',monospace}</style><pre id=t>" ++ out ++ "</pre>"

private def tmuxKey : String → String
  | "Enter" | "Return" => "Enter" | "Escape" => "Escape" | "Tab" => "Tab"
  | "Space" => "Space" | "Backspace" => "BSpace"
  | "ArrowUp" => "Up" | "ArrowDown" => "Down" | "ArrowLeft" => "Left" | "ArrowRight" => "Right"
  | k => k

private def runTmux (args : Array String) : IO String := do
  let r ← IO.Process.output { cmd := "tmux", args }
  if r.exitCode != 0 then throw <| IO.userError s!"tmux {args}: {r.stderr}"
  return r.stdout

/-- Drive the tmux pane `target`; `renderer` is a browser session used
    only to rasterise the pane. Clicks aren't supported — terminal
    UIs are keyboard-driven, use `key` / `typeText`. -/
def Driver.tmux (target : String) (renderer : LeanTea.Browser.Session)
    (workDir : System.FilePath := "/tmp") : Driver where
  name := "tmux"
  screenshot p := do
    let ansi ← runTmux #["capture-pane", "-e", "-p", "-t", target]
    let html := workDir / "leantea-tmux-pane.html"
    IO.FS.writeFile html (ansiToHtml ansi)
    let _ ← renderer.navigate s!"file://{html}"
    let _ ← renderer.screenshot (selector := some "#t") (outputPath := some p.toString)
  click _ _ := throw <| IO.userError "tmux driver: clicks unsupported — use key/type"
  key k := do let _ ← runTmux #["send-keys", "-t", target, tmuxKey k]
  typeText t := do let _ ← runTmux #["send-keys", "-t", target, "-l", t]

end LeanTea.Vision
