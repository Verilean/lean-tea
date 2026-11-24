import LeanTea
import Std.Async.Timer

/-! # http_framing_spec

Request framing and request deadlines on every server backend:
`Server.serve` (sequential), `Server.serveConcurrent`, `FastServer` and
`ReactorServer`. Each runs as an `http_framing_serve` subprocess with a
1 s request timeout and is driven with raw bytes over TCP.

The smuggling cases put a whole second request (`GET /injected`) where a
server that frames the first request wrongly would read it as the next
request. A correct server answers once and never sees `/injected`.
Needs `http_framing_serve` built first. -/

open LeanTea.LSpec
open Std.Async
open Std.Async.TCP
open Std.Net

namespace HttpFramingSpec

def timeoutMs : Nat := 1000

def connectTo (port : Nat) : IO Socket.Client := do
  let s ← Socket.Client.mk
  (s.connect (.v4 { addr := LeanTea.Net.Server.parseIPv4 "127.0.0.1", port := port.toUInt16 })).block
  pure s

/-- Read until the peer closes (`true`) or nothing arrives for `idleMs`
    (`false`). A reset counts as closed. -/
partial def readAll (s : Socket.Client) (idleMs : Nat) (acc : ByteArray := .empty)
    : IO (ByteArray × Bool) := do
  let wait : Async (Option (Option ByteArray)) := do
    let timer ← Selector.sleep (Std.Time.Millisecond.Offset.ofNat idleMs)
    Selectable.one #[.case (s.recvSelector 65536) (fun r => pure (some r)),
                     .case timer (fun _ => pure none)]
  match ← (try wait.block catch _ => pure (some none)) with
  | some (some b) => readAll s idleMs (acc ++ b)
  | some none => pure (acc, true)
  | none => pure (acc, false)

structure Reply where
  text   : String
  closed : Bool
  ms     : Nat

/-- Send `raw` on a new connection, then read as `readAll` does. -/
def exchange (port : Nat) (raw : String) (idleMs : Nat := 400) : IO Reply := do
  let s ← connectTo port
  let t0 ← IO.monoMsNow
  (s.send raw.toUTF8).block
  let (bytes, closed) ← readAll s idleMs
  let t1 ← IO.monoMsNow
  pure { text := String.fromUTF8? bytes |>.getD "", closed, ms := t1 - t0 }

def has (s sub : String) : Bool := (s.splitOn sub).length > 1
def responses (r : Reply) : Nat := (r.text.splitOn "HTTP/1.1 ").length - 1
def status (r : Reply) : String :=
  match r.text.splitOn "HTTP/1.1 " with
  | _ :: rest :: _ => (rest.take 3).toString
  | _ => ""

def injected : String := "GET /injected HTTP/1.1\r\nHost: t\r\n\r\n"
def injLen : Nat := injected.utf8ByteSize

/-- Refused with `code`, once, and the connection closed. -/
def refused (r : Reply) (code : String) : Bool :=
  status r == code && responses r == 1 && !has r.text "/injected" && r.closed

def specs (name : String) (port : Nat) : IO LSpec := do
  let twoLengths ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nContent-Length: 0\r\nContent-Length: {injLen}\r\n\r\n{injected}"
  let inValue ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nX-Note: content-length: 0\r\nContent-Length: {injLen}\r\n\r\n{injected}"
  let teAndCl ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nTransfer-Encoding: chunked\r\nContent-Length: {injLen}\r\n\r\n{injected}"
  let chunked ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n{injected}"
  let signed ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nContent-Length: +{injLen}\r\n\r\n{injected}"
  let huge ← exchange port s!"GET /h HTTP/1.1\r\nHost: t\r\nX-Big: {String.ofList (List.replicate 70000 'a')}\r\n\r\n"
  let spaced ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nContent-Length : {injLen}\r\n\r\n{injected}"
  let folded ← exchange port
    s!"POST /a HTTP/1.1\r\nHost: t\r\nX-A: 1\r\n Content-Length: {injLen}\r\n\r\n{injected}"
  let post ← exchange port "POST /p HTTP/1.1\r\nHost: t\r\nContent-Length: 5\r\n\r\nhello"
  let pipelined ← exchange port
    (String.join ((List.range 20).map fun i => s!"GET /{i} HTTP/1.1\r\nHost: t\r\n\r\n"))
  let close ← exchange port "GET /c HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n" (idleMs := 3000)
  let http10 ← exchange port "GET /h HTTP/1.0\r\n\r\n" (idleMs := 3000)
  -- A client that never finishes its headers is dropped at the deadline,
  -- and (on the sequential server too) does not hold back other clients.
  let slow ← connectTo port
  let t0 ← IO.monoMsNow
  (slow.send "GET /slow HTTP/1.1\r\nHost: t\r\n".toUTF8).block
  let other ← exchange port "GET /other HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n" (idleMs := 5000)
  let (_, slowClosed) ← readAll slow 5000
  let slowMs := (← IO.monoMsNow) - t0
  -- Trickling bytes does not extend the deadline.
  let trickle ← connectTo port
  let t1 ← IO.monoMsNow
  let mut trickleOpen := true
  for c in "GET /t HTTP/1.1\r\nX-Slow: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".toList do
    if !trickleOpen then break
    try (trickle.send (String.singleton c).toUTF8).block catch _ => trickleOpen := false
    IO.sleep 100
  let (_, trickleClosed) ← readAll trickle 5000
  let trickleMs := (← IO.monoMsNow) - t1
  pure <| group name [
    group "smuggling attempts are refused before the body is read" [
      it "two different Content-Length → 400" (refused twoLengths "400") twoLengths.text,
      it "Transfer-Encoding with Content-Length → 501" (refused teAndCl "501") teAndCl.text,
      it "Transfer-Encoding: chunked → 501" (refused chunked "501") chunked.text,
      it "Content-Length with a sign → 400" (refused signed "400") signed.text,
      it "space before the colon → 400" (refused spaced "400") spaced.text,
      it "folded header line → 400" (refused folded "400") folded.text,
      it "oversized headers → 431 with its reason phrase"
        (status huge == "431" && has huge.text "431 Request Header Fields Too Large") (huge.text.take 80).toString,
      it "Transfer-Encoding is answered 501 Not Implemented" (has chunked.text "501 Not Implemented") (chunked.text.take 80).toString,
      it "`content-length:` inside another header's value frames nothing"
        (responses inValue == 1 && has inValue.text s!"path=/a body={injLen}" &&
          !has inValue.text "/injected") inValue.text],
    group "ordinary requests still work" [
      it "POST body is read by Content-Length" (has post.text "path=/p body=5") post.text,
      it "20 pipelined GETs get 20 answers, in order"
        (responses pipelined == 20 && has pipelined.text "path=/0 " && has pipelined.text "path=/19 " &&
          (pipelined.text.splitOn "path=/19 ").length == 2) s!"{responses pipelined} responses",
      it "Connection: close closes after the answer"
        (responses close == 1 && status close == "200" && close.closed) close.text,
      it "HTTP/1.0 without keep-alive closes after the answer"
        (responses http10 == 1 && status http10 == "200" && http10.closed) http10.text],
    group "request deadline" [
      it "another client is answered while one stalls in its headers"
        (status other == "200" && other.ms < timeoutMs + 1500) s!"{other.ms} ms",
      it "the stalled client is closed at the deadline"
        (slowClosed && slowMs ≥ timeoutMs - 100 && slowMs < timeoutMs + 1500) s!"{slowMs} ms",
      it "a client trickling header bytes is closed at the deadline too"
        (trickleClosed && trickleMs < timeoutMs + 1500) s!"{trickleMs} ms"]]

end HttpFramingSpec

def backends : List (String × Nat) :=
  [("server", 18770), ("concurrent", 18771), ("fast", 18772), ("reactor", 18773)]

def waitUp (port : Nat) : IO Bool := do
  for _ in [:50] do
    let ok ← try
        let r ← HttpFramingSpec.exchange port "GET /up HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n" 2000
        pure (HttpFramingSpec.status r == "200")
      catch _ => pure false
    if ok then return true
    IO.sleep 100
  return false

def main : IO UInt32 := do
  let children : List (IO.Process.Child ⟨.null, .null, .null⟩) ← backends.mapM fun (b, port) =>
    IO.Process.spawn {
    cmd := "./.lake/build/bin/http_framing_serve",
    args := #["--backend", b, "--port", toString port, "--timeout", toString HttpFramingSpec.timeoutMs],
    stdin := .null, stdout := .null, stderr := .null }
  let code ← try
      let mut groups := []
      for (b, port) in backends do
        if !(← waitUp port) then
          IO.eprintln s!"http_framing_serve --backend {b} did not start (build it: lake build http_framing_serve)"
          return (1 : UInt32)
        groups := groups ++ [← HttpFramingSpec.specs b port]
      lspecIO (group "HTTP framing and deadlines" groups)
    finally
      for c in children do c.kill
  -- Returning from `main` would wait for the client tasks; exit outright.
  IO.Process.exit code.toUInt8
