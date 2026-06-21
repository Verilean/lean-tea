import LeanTea
import LeanJs.Parser
import LeanJs.Codegen

/-! # Chapter 3 — Writing browser code

The framework gives you *two* places to author client-side code, at
two levels of abstraction:

  1. **The LeanJs subset** — write a small Lean-shaped source as a
     string; the framework parses it, type-checks the pure parts,
     and compiles it to JS. This is the high-level path.
  2. **The typed `LeanTea.Js` AST** — explicit `E.*` / `S.*`
     constructors and the `JsBuilder` monad. This is the level the
     subset itself targets; you reach for it when you need to
     embed Lean values (via `ToJsExpr`) or compose with the rest of
     the framework's typed bits (Endpoint codegen, etc.).

This chapter walks both, in that order, so you can pick whichever
fits the piece of code you're writing.

Run:

    lake exe doc_ch03

The binary takes one source through the subset pipeline, prints the
JS it emits, then renders the same shape using the raw AST so you
can see they line up. -/

open LeanTea LeanJs.Parser LeanJs.Codegen

namespace Ch03

/-! ## 1. The subset path — write the code as text

Most of the time, you want this. The source reads like normal Lean
(parse-only, no elaboration), and the compiler turns it into JS
with all the parenthesisation right. -/

def doubleSrc : String :=
  "def double (x : Int) : Int := x * 2\n"
  ++ "def greet (n : Int) : String := \"hello x\" + n\n"
  ++ "def main := greet(double(21))"

/-- Compile-only helper. Returns the rendered JS or a parse error. -/
def compileOr (src : String) : String :=
  match parseProgramString src with
  | .error e => s!"// PARSE ERROR: {e}"
  | .ok p    => compileToString p

end Ch03

/-! ## 2. The AST path — for embedding Lean values

When initial state or a generated function name has to flow from a
Lean record into the rendered JS, the subset is the wrong tool —
strings are opaque. The `LeanTea.Js` AST is the right tool because
`ToJsExpr` plants Lean values straight into the output.

The classic example is server-rendered initial state. Here we build
the same `console.log("hello x42")` shape but with `42` coming from
a Lean value via `ToJsExpr`. -/

open LeanTea.Js LeanTea.Js.E LeanTea.Js.S

structure HelloModel where
  count : Nat

instance : ToJsExpr HelloModel where
  toJsExpr m := E.obj [("count", toJsExpr m.count)]

namespace Ch03

def modelInit : HelloModel := { count := 42 }

/-- Same logic as `doubleSrc`, but built explicitly from the AST so
    we can plant a Lean record literal into it. -/
def modelEmbedBlock : Block := [
  constV "MODEL" (toJsExpr modelInit),
  doE (mcall (i "console") "log" [
    tmpl ["hello x", ""] [(i "MODEL").dot "count"]
  ])
]

end Ch03

def main : IO Unit := do
  IO.println "== Chapter 3 — Writing browser code =="
  IO.println ""

  IO.println "── Path 1: the LeanJs subset ────────────────────────────"
  IO.println "Source (Lean-shaped, held in a String):"
  IO.println Ch03.doubleSrc
  IO.println ""
  IO.println "Compiled JS:"
  IO.println (Ch03.compileOr Ch03.doubleSrc)
  IO.println ""

  IO.println "── Path 2: the typed AST, when you need ToJsExpr ────────"
  IO.println "Embedding the Lean record { count := 42 } into the rendered JS:"
  IO.println Ch03.modelEmbedBlock.render
  IO.println ""

  IO.println "ok"
