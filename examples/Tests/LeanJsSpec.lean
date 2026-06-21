import LeanTea
import LeanJs.Ast
import LeanJs.Parser
import LeanJs.Codegen
import LeanJs.Eval
import LeanJs.JsParser
import LeanJs.LeanEmit
import LeanJs.Includes

/-! # examples/Tests/LeanJsSpec.lean — LSpec tests for the Fay pipeline

Two layers:

  1. **Pure** tests — parse a source string, compile, and check
     the *rendered JavaScript string*. Fast, no shell-out.
  2. **End-to-end** tests — pipe the compiled JS through `node`
     and check the stdout against an expected value.

The LSpec-shaped runner lives in `LeanTea.LSpec`; spec tree values
combine via `group "label" [...]`. `lspecIO` walks the tree, prints
results, and exits non-zero on any failure so this binary doubles
as a CI gate. -/

open LeanTea.LSpec LeanJs.Parser LeanJs.Codegen LeanJs.Eval

/-! ## 1. Pure parse + compile tests -/

private def compiledMatches (src : String) (expected : String) : LSpec :=
  match parseExpressionString src with
  | .error e => it s!"{src} → parse error" false e
  | .ok e =>
    let stmt := LeanTea.Js.S.doE (compileExpr e)
    let js := LeanTea.Js.renderStmt stmt
    -- We tolerate the trailing semicolon `S.doE` adds.
    let rendered :=
      if js.endsWith ";" then js.dropRight 1 else js
    if rendered == expected then
      it s!"{src} → {expected}" true
    else
      it s!"{src} → {expected}" false s!"got `{rendered}`"

def pureSpecs : LSpec := group "parse + compile" [
  compiledMatches "1 + 2 * 3"            "(1+(2*3))",
  compiledMatches "(1 + 2) * 3"          "((1+2)*3)",
  compiledMatches "foo(bar, 7)"          "foo(bar,7)",
  compiledMatches "let n := 21; n * 2"
                  "((n)=>{return (n*2);})(21)",
  compiledMatches "if c then 10 else 20" "(c?10:20)",
  compiledMatches "(fun (x) => x + 1)(2)"
                  "((x)=>{return (x+1);})(2)"
]

/-! ## 2. End-to-end: compile, run via node, check stdout

We build a tiny program with a `main`, ask the codegen to wrap a
`console.log(main())`, write to a temp file, and invoke `node`. -/

structure NodeCase where
  label    : String
  source   : String
  expected : String
  /-- `true` when the source uses `extern js …` or otherwise relies
      on the JS runtime in a way the Lean evaluator can't simulate.
      Those cases run *only* the node path; the cross-check skips
      them with an explicit ✓ that says so. -/
  jsOnly   : Bool := false

/-- Run one node case: parse, compile, exec via `node`, then —
    unless flagged `jsOnly` — independently evaluate the same AST
    in Lean and assert all three outputs agree.

    Cross-check shape:

      source string  ──parse──►  AST  ──┬── compileToString → js → node → stdout
                                        └── runProgram (Lean eval) → render
      assert: stdout == render == expected -/
def runNodeCase (c : NodeCase) : IO LSpec := do
  match parseProgramString c.source with
  | .error e => return it c.label false s!"parse failed: {e}"
  | .ok prog =>
    let js := compileToString prog
    let path := s!"/tmp/leanjs_test_{c.label}.js".replace " " "_"
    IO.FS.writeFile path js
    let out ← IO.Process.output { cmd := "node", args := #[path] }
    if out.exitCode != 0 then
      return it c.label false s!"node exit {out.exitCode}: {out.stderr}"
    let nodeOut := out.stdout.trimRight
    if nodeOut != c.expected then
      return it c.label false s!"node: expected `{c.expected}`, got `{nodeOut}`"
    if c.jsOnly then
      return it s!"{c.label} (js only)" true
    -- Cross-check: Lean evaluator must agree.
    match runProgram prog with
    | .error e =>
      return it c.label false s!"lean eval failed: {e}"
    | .ok v =>
      let leanOut := render v
      if leanOut == c.expected then
        return it s!"{c.label} ✓ js == lean" true
      else
        return it c.label false
          s!"node = `{nodeOut}` but lean = `{leanOut}` (expected `{c.expected}`)"

def nodeCases : List NodeCase := [
  { label := "constant",  source := "def main := 42",                expected := "42" },
  { label := "arith",     source := "def main := 1 + 2 * 3",         expected := "7" },
  { label := "let",       source := "def main := let n := 21; n + n", expected := "42" },
  { label := "if",        source := "def main := if 1 then 10 else 20",
                          expected := "10" },
  { label := "fn",        source := "def double(x) := x * 2\ndef main := double(21)",
                          expected := "42" },
  /- sum types via inductive + match -/
  { label := "sumNone",   source :=
      "inductive Option where | None | Some(x)\n"
   ++ "def main := match None with | None => 0 | Some(x) => x",
    expected := "0" },
  { label := "sumSome",   source :=
      "inductive Option where | None | Some(x)\n"
   ++ "def main := match Some(7) with | None => 0 | Some(x) => x + 10",
    expected := "17" },
  { label := "shape",     source :=
      "inductive Shape where | Square(s) | Rect(w, h)\n"
   ++ "def area(sh) := match sh with | Square(s) => s * s | Rect(w, h) => w * h\n"
   ++ "def main := area(Square(4)) + area(Rect(3, 5))",
    expected := "31" },
  /- string match + wildcard fall-through (new in this round) -/
  { label := "strMatch",  source :=
      "def pick(s) := match s with | \"a\" => 1 | \"b\" => 2 | _ => 0\n"
   ++ "def main := pick(\"a\") + pick(\"b\") + pick(\"z\")",
    expected := "3" },
  /- record + named-args construction. Round-trips an object, then
     picks one field to print so `node` prints a number. -/
  { label := "record",    source :=
      "record Pt where x : Int, y : Int\n"
   ++ "def main := Pt { x: 7, y: 35 }.y",
    expected := "35", jsOnly := true },
  /- FFI: pull in JS's Math.sqrt and use it. Cross-check skips —
     Lean can't run a foreign JS expression. -/
  { label := "ffiMath",    source :=
      "extern js \"Math.sqrt\" sqrt\n"
   ++ "def main := sqrt(81)",
    expected := "9", jsOnly := true },
  /- Dot-access for FFI: stringify a number via `.toString()` -/
  { label := "dotAccess",  source :=
      "extern js \"(n) => n\" id\n"
   ++ "def main := id(7).toString()",
    expected := "7", jsOnly := true },
  /- async / await — run a promise that resolves to 42 -/
  { label := "asyncAwait", source :=
      "extern js \"() => Promise.resolve(42)\" makePromise\n"
   ++ "async def main := await makePromise()",
    expected := "42", jsOnly := true },
  /- async + let-chain + await + branch. Regression test for the
     `let`-IIFE / await interaction — the codegen flattens `let` to
     `const` inside async bodies so `await` survives. -/
  { label := "asyncLetAwait", source :=
      "extern js \"(n) => Promise.resolve(n * 2)\" twiceP\n"
   ++ "extern js \"(a, b) => a + b\" add\n"
   ++ "async def f(x) :=\n"
   ++ "  let bumped := add(x, 1);\n"
   ++ "  let doubled := await twiceP(bumped);\n"
   ++ "  if doubled == 0 then 99 else add(doubled, 100)\n"
   ++ "async def main := await f(20)",
    expected := "142", jsOnly := true },
  /- Minimal typeclasses — explicit instance lookup (no inference) -/
  { label := "classBasic", source :=
      "class Show where show\n"
   ++ "extern js \"(x) => String(x)\" jsStr\n"
   ++ "instance Show Int  where show := fun (x) => jsStr(x)\n"
   ++ "instance Show Bool where show := fun (x) => if x then \"true\" else \"false\"\n"
   ++ "def main := Show.Int.show(42)",
    expected := "42", jsOnly := true },
  { label := "classBool", source :=
      "class Show where show\n"
   ++ "instance Show Bool where show := fun (x) => if x then \"yes\" else \"no\"\n"
   ++ "def main := Show.Bool.show(0)",
    expected := "no" },
  { label := "classMulti", source :=
      "class Pair where mk fst\n"
   ++ "instance Pair Int where mk := fun (a, b) => a * 100 + b , fst := fun (p) => p / 100\n"
   ++ "def main := Pair.Int.fst(Pair.Int.mk(7, 0))",
    expected := "7" },
  /- Arrays + indexing -/
  { label := "arrIdx", source :=
      "def main := [10, 20, 30, 40][2]",
    expected := "30" },
  /- Array length via `.length` -/
  { label := "arrLen", source :=
      "def main := [1, 2, 3, 4, 5].length",
    expected := "5" },
  /- Objects + key access -/
  { label := "objField", source :=
      "def player := {name: \"alice\", score: 42}\n"
   ++ "def main := player.score",
    expected := "42" },
  /- Object indexed by computed string key -/
  { label := "objIndex", source :=
      "def cfg := {host: \"localhost\", port: 11211}\n"
   ++ "def main := cfg[\"port\"]",
    expected := "11211" },
  /- Top-level recursion via name reference inside the body. The
     Lean evaluator now ties the knot (`apply` threads `globals`,
     `.var` lookup falls through), so this cross-checks both sides. -/
  { label := "recur", source :=
      "def fact(n) := if n then n * fact(n - 1) else 1\n"
   ++ "def main := fact(5)",
    expected := "120" },
  /- Accumulator pattern via recursion — what `let mut`/`set` would
     give you in an imperative language. Stays expression-shaped and
     cross-emits to Lean. -/
  { label := "accum", source :=
      "def sumLoop(i, acc) := if i == 0 then acc else sumLoop(i - 1, acc + i)\n"
   ++ "def main := sumLoop(10, 0)",
    expected := "55" },
  /- Mutual recursion across top-level defs — exercises the
     globals-fallback lookup, since `isEven` references `isOdd`
     declared after it. -/
  { label := "mutualRec", source :=
      "def isEven(n) := if n == 0 then 1 else isOdd(n - 1)\n"
   ++ "def isOdd(n)  := if n == 0 then 0 else isEven(n - 1)\n"
   ++ "def main := isEven(8) * 10 + isEven(7)",
    expected := "10" },
  /- ── visual-novel-style chat helpers ─────────────────────────────────
     These mirror the equivalent `.leanjs` app so a refactor of
     `buildHistLine` / `findOutfit` / `pickFrame` that breaks the
     real client will fail the suite here first. -/
  { label := "chatHistLinePlain", source :=
      "extern js \"(a, b) => '' + a + b\" sconcat\n"
   ++ "def buildHistLine(mood, reply, inner) :=\n"
   ++ "  let head := sconcat(sconcat(\"[mood:\", mood), \"] \");\n"
   ++ "  if inner == \"\" then sconcat(head, reply)\n"
   ++ "  else sconcat(sconcat(sconcat(head, reply), \"|\"), inner)\n"
   ++ "def main := buildHistLine(\"smile\", \"hi\", \"\")",
    expected := "[mood:smile] hi", jsOnly := true },
  { label := "chatHistLineInner", source :=
      "extern js \"(a, b) => '' + a + b\" sconcat\n"
   ++ "def buildHistLine(mood, reply, inner) :=\n"
   ++ "  let head := sconcat(sconcat(\"[mood:\", mood), \"] \");\n"
   ++ "  if inner == \"\" then sconcat(head, reply)\n"
   ++ "  else sconcat(sconcat(sconcat(head, reply), \"|\"), inner)\n"
   ++ "def main := buildHistLine(\"smile\", \"hi\", \"sleepy\")",
    expected := "[mood:smile] hi|sleepy", jsOnly := true },
  { label := "chatFindOutfit", source :=
      "extern js \"(arr, i) => arr[i]\" arrAt\n"
   ++ "extern js \"(arr) => arr.length\" arrLen\n"
   ++ "def findOutfitLoop(outfits, target, i, fallback) :=\n"
   ++ "  if i >= arrLen(outfits) then fallback\n"
   ++ "  else\n"
   ++ "    let o := arrAt(outfits, i);\n"
   ++ "    if o.id == target then o\n"
   ++ "    else findOutfitLoop(outfits, target, i + 1, fallback)\n"
   ++ "def findOutfit(profile, outfitId) :=\n"
   ++ "  let outfits := profile.outfits;\n"
   ++ "  if arrLen(outfits) == 0 then null\n"
   ++ "  else findOutfitLoop(outfits, outfitId, 0, arrAt(outfits, 0))\n"
   ++ "def profile := {outfits: [{id: \"swim\", v: 10}, {id: \"maid\", v: 20}]}\n"
   ++ "def main := findOutfit(profile, \"maid\").v",
    expected := "20", jsOnly := true },
  { label := "chatFindOutfitFallback", source :=
      "extern js \"(arr, i) => arr[i]\" arrAt\n"
   ++ "extern js \"(arr) => arr.length\" arrLen\n"
   ++ "def findOutfitLoop(outfits, target, i, fallback) :=\n"
   ++ "  if i >= arrLen(outfits) then fallback\n"
   ++ "  else\n"
   ++ "    let o := arrAt(outfits, i);\n"
   ++ "    if o.id == target then o\n"
   ++ "    else findOutfitLoop(outfits, target, i + 1, fallback)\n"
   ++ "def findOutfit(profile, outfitId) :=\n"
   ++ "  let outfits := profile.outfits;\n"
   ++ "  if arrLen(outfits) == 0 then null\n"
   ++ "  else findOutfitLoop(outfits, outfitId, 0, arrAt(outfits, 0))\n"
   ++ "def profile := {outfits: [{id: \"swim\", v: 10}, {id: \"maid\", v: 20}]}\n"
   ++ "def main := findOutfit(profile, \"unknown\").v",
    expected := "10", jsOnly := true },
  { label := "vnPickFrameDirect", source :=
      "extern js \"(o, k) => o[k]\" objGet\n"
   ++ "extern js \"(o) => Object.keys(o)\" objKeys\n"
   ++ "extern js \"(arr, i) => arr[i]\" arrAt\n"
   ++ "def pickFrame(outfit, mood, mouthOpen) :=\n"
   ++ "  let moods := outfit.moods;\n"
   ++ "  let direct := objGet(moods, mood);\n"
   ++ "  let viaDefault :=\n"
   ++ "    if direct == null then objGet(moods, \"default\") else direct;\n"
   ++ "  let safe :=\n"
   ++ "    if viaDefault == null\n"
   ++ "    then objGet(moods, arrAt(objKeys(moods), 0))\n"
   ++ "    else viaDefault;\n"
   ++ "  if mouthOpen == true then\n"
   ++ "    if safe.open == null then safe.closed else safe.open\n"
   ++ "  else safe.closed\n"
   ++ "def outfit := {moods: {default: {open: \"O.png\", closed: \"C.png\"},"
   ++ " smile: {open: \"SO.png\", closed: \"SC.png\"}}}\n"
   ++ "def main := pickFrame(outfit, \"smile\", true)",
    expected := "SO.png", jsOnly := true },
  { label := "vnPickFrameFallback", source :=
      "extern js \"(o, k) => o[k]\" objGet\n"
   ++ "extern js \"(o) => Object.keys(o)\" objKeys\n"
   ++ "extern js \"(arr, i) => arr[i]\" arrAt\n"
   ++ "def pickFrame(outfit, mood, mouthOpen) :=\n"
   ++ "  let moods := outfit.moods;\n"
   ++ "  let direct := objGet(moods, mood);\n"
   ++ "  let viaDefault :=\n"
   ++ "    if direct == null then objGet(moods, \"default\") else direct;\n"
   ++ "  let safe :=\n"
   ++ "    if viaDefault == null\n"
   ++ "    then objGet(moods, arrAt(objKeys(moods), 0))\n"
   ++ "    else viaDefault;\n"
   ++ "  if mouthOpen == true then\n"
   ++ "    if safe.open == null then safe.closed else safe.open\n"
   ++ "  else safe.closed\n"
   ++ "def outfit := {moods: {default: {open: \"O.png\", closed: \"C.png\"},"
   ++ " smile: {open: \"SO.png\", closed: \"SC.png\"}}}\n"
   ++ "def main := pickFrame(outfit, \"angry\", false)",
    expected := "C.png", jsOnly := true }
]

/-! ## 3. Dual-flavour parity

Each row gives a Lean-shaped source and a JS-shaped source that
*should* mean the same thing. We parse both, evaluate both with the
same Lean interpreter, and assert the rendered values agree. -/

structure DualCase where
  label : String
  lean  : String
  js    : String

def dualCases : List DualCase := [
  { label := "addition", lean := "1 + 2 * 3",         js := "1 + 2 * 3" },
  { label := "ternary",  lean := "if 1 then 10 else 20",
                         js   := "1 ? 10 : 20" },
  { label := "arrow",    lean := "(fun (a) => a + 1)(41)",
                         js   := "((a) => a + 1)(41)" },
  { label := "call",     lean := "(fun (g, x) => g(x))(fun (n) => n * 2, 21)",
                         js   := "((g, x) => g(x))((n) => n * 2, 21)" },
  { label := "array",    lean := "[10, 20, 30][1]",   js := "[10, 20, 30][1]" },
  { label := "object",   lean := "{a: 1, b: 42}.b",   js := "{a: 1, b: 42}.b" },
  { label := "dot",      lean := "{x: {y: {z: 99}}}.x.y.z",
                         js   := "{x: {y: {z: 99}}}.x.y.z" }
]

def runDualCase (c : DualCase) : LSpec :=
  match parseExpressionString c.lean,
        LeanJs.JsParser.parseExpressionString c.js with
  | .error e, _ => it c.label false s!"lean parse: {e}"
  | _, .error e => it c.label false s!"js   parse: {e}"
  | .ok eLean, .ok eJs =>
    match eval [] [] eLean, eval [] [] eJs with
    | .error l, _ => it c.label false s!"lean eval: {l}"
    | _, .error r => it c.label false s!"js   eval: {r}"
    | .ok vLean, .ok vJs =>
      let rL := render vLean
      let rJ := render vJs
      if rL == rJ then it s!"{c.label} ✓ {rL}" true
      else it c.label false s!"lean=`{rL}` js=`{rJ}`"

def dualSpecs : LSpec := group "lean source ⇔ js source ⇒ same value"
  (dualCases.map runDualCase)

/-! ## 4. Lean-source emission — `lean --run` on the same program

For each non-FFI case we already verified via node, we *also*
emit Lean source via `LeanJs.LeanEmit`, write it to a temp file,
run it through `lean --run`, and assert it prints the same value
as the node pipeline. This makes the "same source, both sides"
claim concrete on the *real* Lean compiler. -/

structure LeanRunCase where
  label    : String
  source   : String
  expected : String

/-- Pure-subset cases — no extern/class/instance/objects. These can
    flow to Lean. The Reversi-style programs that use FFI live in the
    `nodeCases` suite and stay JS-only by design. -/
def leanRunCases : List LeanRunCase := [
  { label := "constant", source := "def main := 42",                expected := "42" },
  { label := "arith",    source := "def main := 1 + 2 * 3",         expected := "7" },
  { label := "let",      source := "def main := let n := 21; n + n", expected := "42" },
  { label := "if",       source := "def main := if 1 == 1 then 10 else 20",
                         expected := "10" },
  { label := "fn",       source := "def double (x : Int) : Int := x * 2\ndef main := double(21)",
                         expected := "42" },
  { label := "recur",    source :=
      "def fact (n : Int) : Int := if n == 0 then 1 else n * fact(n - 1)\n"
   ++ "def main := fact(5)",
    expected := "120" },
  /- Accumulator via recursion — the answer to "do we need `set`?". -/
  { label := "accum",    source :=
      "def sumLoop (i : Int, acc : Int) : Int :=\n"
   ++ "  if i == 0 then acc else sumLoop(i - 1, acc + i)\n"
   ++ "def main := sumLoop(10, 0)",
    expected := "55" }
]

def runLeanCase (c : LeanRunCase) : IO LSpec := do
  match parseProgramString c.source with
  | .error e => return it c.label false s!"parse: {e}"
  | .ok prog =>
    match LeanJs.LeanEmit.emitProgram prog with
    | .error e => return it c.label false s!"emit:  {e}"
    | .ok body =>
      let leanSrc := LeanJs.LeanEmit.wrapForLeanRun body
      let path := s!"/tmp/leanjs_lean_{c.label}.lean".replace " " "_"
      IO.FS.writeFile path leanSrc
      let out ← IO.Process.output {
        cmd := "lean", args := #["--run", path] }
      if out.exitCode != 0 then
        return it c.label false s!"lean --run exit {out.exitCode}: {out.stderr}"
      let leanOut := out.stdout.trimRight
      if leanOut == c.expected then
        return it s!"{c.label} ✓ lean --run = {leanOut}" true
      else
        return it c.label false
          s!"expected `{c.expected}`, got `{leanOut}`"

/-! ## 5. `include` resolution

Round-trips a two-file program through `LeanJs.Includes.resolve` and
verifies the included def is callable from the main file. -/

def runIncludeSpec : IO LSpec := do
  let dir := "/tmp/leanjs_inc_spec"
  IO.FS.createDirAll dir
  let utilPath := s!"{dir}/util.leanjs"
  let mainPath := s!"{dir}/main.leanjs"
  IO.FS.writeFile utilPath "def square(n) := n * n\ndef cube(n) := n * square(n)\n"
  IO.FS.writeFile mainPath "include \"util.leanjs\"\ndef main := square(4) + cube(3)\n"
  let src ← IO.FS.readFile mainPath
  match parseProgramString src with
  | .error e => return it "include" false s!"parse: {e}"
  | .ok prog0 =>
    let prog ← LeanJs.Includes.resolve mainPath prog0
    match runProgram prog with
    | .error e => return it "include" false s!"eval: {e}"
    | .ok v    =>
      let r := render v
      if r == "43" then return it s!"include ✓ {r}" true
      else return it "include" false s!"expected 43, got {r}"

def main : IO UInt32 := do
  IO.println "── LeanJs pure tests ─────────────────────────────────────"
  let pureCode ← lspecIO pureSpecs

  IO.println ""
  IO.println "── LeanJs end-to-end (node) ──────────────────────────────"
  let mut leaves : List LSpec := []
  for c in nodeCases do
    leaves := leaves ++ [(← runNodeCase c)]
  let nodeCode ← lspecIO (group "node exec" leaves)

  IO.println ""
  IO.println "── LeanJs dual-flavour parity ────────────────────────────"
  let dualCode ← lspecIO dualSpecs

  IO.println ""
  IO.println "── LeanJs `lean --run` ───────────────────────────────────"
  let mut leanLeaves : List LSpec := []
  for c in leanRunCases do
    leanLeaves := leanLeaves ++ [(← runLeanCase c)]
  let leanCode ← lspecIO (group "lean --run" leanLeaves)

  IO.println ""
  IO.println "── LeanJs `include` resolution ───────────────────────────"
  let incCode ← lspecIO (group "include" [← runIncludeSpec])

  return pureCode + nodeCode + dualCode + leanCode + incCode
