import LeanTea.Vision.Image

/-! # LeanTea.Vision.Ocr — text on a screenshot, from the OS

macOS's Vision framework reads screen text (Japanese included) in a
second or two, with boxes to the pixel — far better recall than asking
a 4B VLM to list what it sees. `tools/ocr/ocr.swift` is compiled with
`swiftc` on first use into `.lake/build/bin/leantea-ocr`. Elsewhere
(no `swiftc`) `read` returns nothing and callers fall back to the VLM. -/

namespace LeanTea.Vision.Ocr

open Lean (Json)

structure Line where
  text : String
  rect : Rect
  /-- Vision's confidence, 0–1 (0.3 / 0.5 / 1 in practice). -/
  conf : Float := 1
  deriving Inhabited, Repr

def source : System.FilePath := "tools/ocr/ocr.swift"
def binary : System.FilePath := ".lake/build/bin/leantea-ocr"

private def mtime (p : System.FilePath) : IO (Option IO.FS.SystemTime) := do
  try return some (← p.metadata).modified catch _ => return none

/-- The OCR binary, compiled if missing or older than its source; none
    when it can't be built (not macOS, no source next to the cwd). -/
def ensure : IO (Option System.FilePath) := do
  let some src ← mtime source | return (if ← binary.pathExists then some binary else none)
  if let some bin ← mtime binary then
    if bin.sec ≥ src.sec then return some binary
  if let some dir := binary.parent then IO.FS.createDirAll dir
  let out ← try IO.Process.output { cmd := "swiftc", args := #["-O", source.toString, "-o", binary.toString] }
    catch _ => return none
  return if out.exitCode == 0 then some binary else none

/-- Parse the tool's JSON output. -/
def parse (out : String) : List Line :=
  match Json.parse out with
  | .ok (.arr xs) => xs.toList.filterMap fun j => do
    let text ← (j.getObjValD "text").getStr?.toOption
    let num := fun k => match j.getObjValD k with
      | .num n => n.toFloat
      | _ => 0
    if text.trimAscii.isEmpty then none
    else some { text := text.trimAscii.toString, conf := num "conf",
                rect := { x := num "x", y := num "y", w := num "w", h := num "h" } }
  | _ => []

/-- The text lines on the PNG at `png`; `[]` when OCR is unavailable. -/
def read (png : System.FilePath) : IO (List Line) := do
  let some bin ← ensure | return []
  let out ← IO.Process.output { cmd := bin.toString, args := #[png.toString] }
  if out.exitCode != 0 then return []
  return parse out.stdout

end LeanTea.Vision.Ocr
