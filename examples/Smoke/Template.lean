import LeanTea

/-! Smoke test for the Template module.

  * Flat substitution
  * `{{#each}}` over a list of strings (renders `this`)
  * `{{#each}}` over a list of dicts (renders named fields)
  * `{{#if}}` with truthy / falsy / `{{else}}` branches -/

open LeanTea.Template

def flatSrc : String := "Hello, {{name}}! You have {{count}} new messages."
def listSrc : String := "<ul>{{#each fruits}}<li>{{this}}</li>{{/each}}</ul>"
def dictSrc : String :=
  "<ul>{{#each users}}<li>{{name}} ({{age}})</li>{{/each}}</ul>"
def ifSrc : String :=
  "{{#if banner}}<b>{{banner}}</b>{{else}}<em>no banner</em>{{/if}}"
def includeSrc : String :=
  "[main start] {{#include \"examples/Smoke/fixtures/partial.html\"}} [main end]"

def main : IO Unit := do
  IO.println "── flat ──────────────────────────────────────────"
  IO.println (← (parse flatSrc).renderFlat
    [("name", "Junji"), ("count", "3")])
  IO.println ""

  IO.println "── each (string list) ────────────────────────────"
  IO.println (← (parse listSrc).render
    [("fruits", .list [.str "apple", .str "banana", .str "cherry"])])
  IO.println ""

  IO.println "── each (dict list) ──────────────────────────────"
  IO.println (← (parse dictSrc).render
    [("users", .list [
      .dict [("name", .str "Alice"), ("age", .str "30")],
      .dict [("name", .str "Bob"),   ("age", .str "25")]
    ])])
  IO.println ""

  IO.println "── if (truthy) ───────────────────────────────────"
  IO.println (← (parse ifSrc).renderFlat [("banner", "compile error")])
  IO.println ""

  IO.println "── if (falsy via empty string) ────────────────────"
  IO.println (← (parse ifSrc).renderFlat [("banner", "")])
  IO.println ""

  IO.println "── if (falsy via absent key) ──────────────────────"
  IO.println (← (parse ifSrc).renderFlat [])
  IO.println ""

  IO.println "── include (partial) ────────────────────────────"
  IO.println (← (parse includeSrc).renderFlat [("name", "Partial-san")])
  IO.println ""

  IO.println "ok"
