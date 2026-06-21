import Lean.Data.Json

/-! # LeanTea.Json.Helpers — terse defaults-with-fallback accessors

`Lean.Data.Json` ships a value-or-error API (`getStr?`, `getObjVal?`)
that's correct but noisy when you just want "give me this string, or
empty if anything goes wrong". Three nested wrappers per access:

```
(j.getObjVal? "k").toOption.bind (·.getStr?.toOption) |>.getD ""
```

These helpers collapse that to `j.getStrD "k"`. -/

namespace Lean.Json

/-- Look up `key` and try to read it as a string. Returns `default`
    (empty string by default) if the key is missing, the value isn't a
    string, or anything else goes wrong. -/
def getStrD (j : Json) (key : String) (default := "") : String :=
  (j.getObjVal? key).toOption.bind (·.getStr?.toOption) |>.getD default

/-- Same for naturals. -/
def getNatD (j : Json) (key : String) (default : Nat := 0) : Nat :=
  (j.getObjVal? key).toOption.bind (·.getNat?.toOption) |>.getD default

/-- Same for booleans. -/
def getBoolD (j : Json) (key : String) (default : Bool := false) : Bool :=
  (j.getObjVal? key).toOption.bind (·.getBool?.toOption) |>.getD default

/-- Look up `key` as a string, returning `none` on any failure.
    For when the absence of a value is meaningful. -/
def getStrOpt (j : Json) (key : String) : Option String :=
  (j.getObjVal? key).toOption.bind (·.getStr?.toOption)

/-- Look up `key` as an array, returning `#[]` if it's missing or
    isn't an array. -/
def getArrD (j : Json) (key : String) : Array Json :=
  (j.getObjVal? key).toOption.bind (·.getArr?.toOption) |>.getD #[]

/-- Look up `key` as a raw `Json` value, returning `default` (a `null`
    by default) if absent. Useful for nested objects (`params`, `result`).
    Pass `Json.mkObj []` as default to get an empty-object fallback. -/
def getJsonD (j : Json) (key : String) (default : Json := Json.null) : Json :=
  (j.getObjVal? key).toOption.getD default

end Lean.Json
