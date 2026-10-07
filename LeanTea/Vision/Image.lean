import LeanTea.Llm.Openai

/-! # LeanTea.Vision.Image — RGBA rasters for vision QA

Just enough image handling to feed a VLM at pixel precision: decode a
screenshot, crop a region, upscale it (zoom-refine), read a pixel,
re-encode as PNG / `data:` URL. Decode / encode / crop / resize are
C (`c/leantea_image.c`, stb-backed); everything else is plain Lean.

Pixels are 8-bit RGBA, row-major, `width * height * 4` bytes. -/

namespace LeanTea.Vision

structure Image where
  width  : Nat
  height : Nat
  rgba   : ByteArray

instance : Inhabited Image := ⟨{ width := 0, height := 0, rgba := .empty }⟩

/-- Axis-aligned rectangle in pixel space (`x`, `y` = top-left). -/
structure Rect where
  x : Float
  y : Float
  w : Float
  h : Float
  deriving Inhabited, Repr, BEq

def Rect.cx (r : Rect) : Float := r.x + r.w / 2
def Rect.cy (r : Rect) : Float := r.y + r.h / 2
def Rect.contains (r : Rect) (px py : Float) : Bool :=
  px >= r.x && px <= r.x + r.w && py >= r.y && py <= r.y + r.h
def Rect.area (r : Rect) : Float := r.w * r.h
def Rect.iou (a b : Rect) : Float :=
  let ix := max 0 (min (a.x + a.w) (b.x + b.w) - max a.x b.x)
  let iy := max 0 (min (a.y + a.h) (b.y + b.h) - max a.y b.y)
  let inter := ix * iy
  let uni := a.area + b.area - inter
  if uni <= 0 then 0 else inter / uni

namespace Image

@[extern "leantea_image_decode"]
private opaque decodeRaw (bytes : @& ByteArray) : IO ByteArray

@[extern "leantea_image_encode_png"]
private opaque encodePngRaw (w h : UInt32) (rgba : @& ByteArray) : IO ByteArray

@[extern "leantea_image_crop"]
private opaque cropRaw (w h : UInt32) (rgba : @& ByteArray) (x y cw ch : UInt32) : ByteArray

@[extern "leantea_image_resize"]
private opaque resizeRaw (w h : UInt32) (rgba : @& ByteArray) (nw nh : UInt32) : ByteArray

private def u32le (b : ByteArray) (i : Nat) : Nat :=
  b[i]!.toNat ||| (b[i+1]!.toNat <<< 8) ||| (b[i+2]!.toNat <<< 16) ||| (b[i+3]!.toNat <<< 24)

/-- Decode PNG or JPEG bytes. -/
def decode (bytes : ByteArray) : IO Image := do
  let raw ← decodeRaw bytes
  let w := u32le raw 0
  let h := u32le raw 4
  return { width := w, height := h, rgba := raw.extract 8 raw.size }

def load (path : System.FilePath) : IO Image := do
  decode (← IO.FS.readBinFile path)

def encodePng (img : Image) : IO ByteArray :=
  encodePngRaw img.width.toUInt32 img.height.toUInt32 img.rgba

def save (img : Image) (path : System.FilePath) : IO Unit := do
  IO.FS.writeBinFile path (← img.encodePng)

def dataUrl (img : Image) : IO String := do
  return "data:image/png;base64," ++ LeanTea.Llm.Openai.base64Encode (← img.encodePng)

/-- Crop to `(x, y, w, h)`, clamped to the image bounds. -/
def crop (img : Image) (x y w h : Nat) : Image :=
  let x := min x img.width
  let y := min y img.height
  let w := min w (img.width - x)
  let h := min h (img.height - y)
  { width := w, height := h,
    rgba := cropRaw img.width.toUInt32 img.height.toUInt32 img.rgba
              x.toUInt32 y.toUInt32 w.toUInt32 h.toUInt32 }

/-- Bilinear resize to exactly `nw × nh`. -/
def resize (img : Image) (nw nh : Nat) : Image :=
  { width := nw, height := nh,
    rgba := resizeRaw img.width.toUInt32 img.height.toUInt32 img.rgba
              nw.toUInt32 nh.toUInt32 }

/-- `(r, g, b, a)` at `(x, y)`; transparent black when out of range. -/
def pixel (img : Image) (x y : Nat) : UInt8 × UInt8 × UInt8 × UInt8 :=
  if x < img.width && y < img.height then
    let i := (y * img.width + x) * 4
    (img.rgba[i]!, img.rgba[i+1]!, img.rgba[i+2]!, img.rgba[i+3]!)
  else (0, 0, 0, 0)

end Image

end LeanTea.Vision
