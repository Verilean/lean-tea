import LeanTea.Tui.Core

/-! # LeanTea.Tui.Input — bytes from the terminal → `Event`s

A pure decoder, so every escape sequence the app understands is unit
tested without a tty. `App.runWith` feeds it whatever `read` returned
and keeps the undecoded tail for the next read.

Understood:

  * UTF-8 text (multi-byte characters arrive as one `Key.char`)
  * C0 controls: Enter, Tab, Backspace, `Ctrl+<letter>`
  * CSI / SS3 keys: arrows, Home/End, PageUp/PageDown, Delete,
    Shift-Tab — with or without modifier parameters (`ESC[1;5A`)
  * SGR mouse reports (`ESC[<b;x;yM` / `…m`, enabled by
    `?1000h ?1002h ?1006h`): press, release, drag, motion, wheel,
    with Shift / Alt / Ctrl
  * a lone `ESC` — but only when `final` says no more bytes are
    coming, since it is also the start of every sequence above

Unknown sequences are consumed and dropped, never leaked as text. -/

namespace LeanTea.Tui

inductive Event where
  | key   (k : Key)
  | mouse (m : Mouse)
  deriving Inhabited, BEq, Repr

namespace Input

private def isFinal (b : UInt8) : Bool := 0x40 ≤ b && b ≤ 0x7E

/-- `"1;5"` → `[1, 5]`; empty fields read as 0. -/
private def params (bs : List UInt8) : List Nat :=
  (String.ofList (bs.map (fun b => Char.ofNat b.toNat))).splitOn ";" |>.map (·.toNat?.getD 0)

/-- Decode an SGR mouse report body (`b;x;y`) with its final byte. -/
private def sgrMouse (body : List UInt8) (fin : UInt8) : Option Event :=
  match params body with
  | [b, x, y] =>
    let btn := match b % 4 with
      | 0 => MouseButton.left | 1 => .middle | 2 => .right | _ => .none
    let kind :=
      if b &&& 64 != 0 then (if b % 2 == 0 then MouseKind.scrollUp else .scrollDown)
      else if b &&& 32 != 0 then (if btn == .none then .move else .drag)
      else if fin == 0x6D /- m -/ then .release
      else .press
    some <| .mouse {
      kind, button := (if kind == .scrollUp || kind == .scrollDown then .none else btn),
      row := (y : Int) - 1, col := (x : Int) - 1,
      shift := b &&& 4 != 0, alt := b &&& 8 != 0, ctrl := b &&& 16 != 0 }
  | _ => none

/-- Map a complete CSI sequence (parameter bytes + final byte). -/
private def csi (body : List UInt8) (fin : UInt8) : Option Event :=
  if body.head? == some 0x3C /- < -/ && (fin == 0x4D || fin == 0x6D) then
    sgrMouse body.tail fin
  else
    let k : Option Key := match fin with
      | 0x41 => some .up | 0x42 => some .down | 0x43 => some .right | 0x44 => some .left
      | 0x48 => some .home | 0x46 => some .end_ | 0x5A => some .shiftTab
      | 0x7E =>
        match (params body).head? with
        | some 1 | some 7 => some .home
        | some 4 | some 8 => some .end_
        | some 3 => some .delete
        | some 5 => some .pageUp
        | some 6 => some .pageDown
        | _ => none
      | _ => none
    k.map .key

/-- Bytes in a UTF-8 sequence, from its lead byte (0 = invalid lead). -/
private def utf8Len (b : UInt8) : Nat :=
  if b < 0x80 then 1 else if b < 0xC2 then 0
  else if b < 0xE0 then 2 else if b < 0xF0 then 3 else if b < 0xF5 then 4 else 0

private def utf8Decode (bs : List UInt8) : Option Char :=
  match String.fromUTF8? (ByteArray.mk bs.toArray) with
  | some s => s.toList.head?
  | none => none

private def control (b : UInt8) : Key :=
  if b == 0x09 then .tab
  else if b == 0x0D || b == 0x0A then .enter
  else if b == 0x7F || b == 0x08 then .backspace
  else .ctrl (Char.ofNat (b.toNat + 0x60))

/-- Decode as many events as `bytes` holds. Returns the events and
    the undecoded tail (an incomplete sequence still waiting for
    bytes). With `final := true` nothing is held back: a dangling
    `ESC` becomes `Key.esc` and broken input is dropped. -/
partial def decode (bytes : List UInt8) (final : Bool := false) : Array Event × List UInt8 :=
  go bytes #[]
where
  go : List UInt8 → Array Event → Array Event × List UInt8
    | [], acc => (acc, [])
    | all@(b :: rest), acc =>
      if b == 0x1B then
        match rest with
        | [] => if final then (acc.push (.key .esc), []) else (acc, all)
        | 0x5B :: tl =>                                    -- CSI: ESC [ params final
          let body := tl.takeWhile (!isFinal ·)
          match tl.drop body.length with
          | fin :: more => go more (match csi body fin with | some e => acc.push e | none => acc)
          | [] => if final then go tl (acc.push (.key .esc)) else (acc, all)
        | 0x4F :: tl =>                                    -- SS3: ESC O final
          match tl with
          | fin :: more => go more (match csi [] fin with | some e => acc.push e | none => acc)
          | [] => if final then go tl (acc.push (.key .esc)) else (acc, all)
        | _ => go rest (acc.push (.key .esc))              -- Alt+key: ESC, then the key
      else if b < 0x20 || b == 0x7F then go rest (acc.push (.key (control b)))
      else if b < 0x80 then go rest (acc.push (.key (.char (Char.ofNat b.toNat))))
      else
        let n := utf8Len b
        if n == 0 then go rest acc
        else if all.length < n then (if final then (acc, []) else (acc, all))
        else match utf8Decode (all.take n) with
          | some c => go (all.drop n) (acc.push (.key (.char c)))
          | none   => go rest acc

/-- Convenience for tests: decode a string's UTF-8 bytes completely. -/
def decodeString (s : String) : Array Event :=
  (decode s.toUTF8.toList true).1

end Input

end LeanTea.Tui
