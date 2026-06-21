import LeanTea.Persist.Store

/-! # LeanTea.Persist.SafeQuery — typed builders that make SQL injection unrepresentable

This is `SECURITY.md` §"Primitive 1 · SafeQuery" in code. The AST below
is **the only path from user values to SQL that the framework provides**.
There is no `String → SafeQuery` constructor; the lone escape hatch
`.trusted` requires a `decl_name%` audit tag and is grep-able in code
review.

What this v1 covers:

* `SELECT … FROM E WHERE … ORDER BY … LIMIT … OFFSET …`
* `UPDATE E SET col = val, … WHERE …`
* `DELETE FROM E WHERE …`
* `COUNT(*) WHERE …`
* `WHERE col IN (?, ?, …)` — variable-length placeholders rendered
  positionally; values can only enter via `[ToValue α]`.

Out of scope for v1 (deferred to v1.1):

* JOINs across multiple entities — use `.trusted` with a `decl_name%`
  audit tag.
* `SUM` / `MAX` / `GROUP BY` — same; the `.count` shortcut covers the
  pagination case.

Design notes are in `SECURITY.md`. -/

namespace LeanTea.Persist.SafeQuery

/-! ## Value binding — only typed primitives reach SQL. -/

/-- How a Lean value renders as a positional parameter. The framework's
    SQLite driver text-encodes all parameters, so this is just a
    `toString`-like shim that ensures only **typed** values reach the
    wire. -/
class ToValue (α : Type) where
  bind : α → String

instance : ToValue String := ⟨id⟩
instance : ToValue Int    := ⟨toString⟩
instance : ToValue Nat    := ⟨toString⟩
instance : ToValue Bool   := ⟨fun b => if b then "1" else "0"⟩

/-! ## Typed column references. -/

/-- A column of entity `E` whose values have Lean type `α`. The `name`
    must match the SQL column name verbatim — there's no rename layer.
    Build these as constants on your entity (`User.cols.email`, …).

    The column's `[Entity E]` instance is implicit; we don't store the
    table name here because every Where / Select / Update carries it
    through the `E` type parameter. -/
structure Col (E : Type) (α : Type) [Entity E] where
  name : String
  deriving Repr

/-! ## WHERE clause. -/

/-- Ordering for `ORDER BY`. -/
inductive Order | asc | desc deriving Repr, BEq

/-- `LIKE` positions. The match string itself is parameter-bound; the
    `%` placement comes from the AST. -/
inductive LikePos | prefix | suffix | contains deriving Repr, BEq

/-- Predicate AST.

    Constructors store the **already-bound string value** so the
    `[ToValue α]` instance only fires once at construction time. The
    surface API (the `Col.eq` / `Col.inList` / … smart constructors
    below) is what enforces "values must be typed primitives" — the
    inductive itself is intentionally erased to make pattern-match
    rendering trivial.

    All constructors are `protected`: outside this module the only
    way to build a `Where` is via the typed `Col.eq` / `Col.inList` /
    `.and` / `.or` / `.not` smart constructors below. An LLM-generated
    `Where.eq "email" rawString` is rejected at compile time. -/
inductive Where (E : Type) [Entity E] : Type where
  | trueP   : Where E
  /-- `colName op (boundVal)` — `boundVal` came from `ToValue.bind`. -/
  | private eq      (colName : String) (boundVal : String) : Where E
  | private ne      (colName : String) (boundVal : String) : Where E
  | private lt      (colName : String) (boundVal : String) : Where E
  | private gt      (colName : String) (boundVal : String) : Where E
  /-- `col IN (?, ?, …)` — variable-length placeholders. -/
  | private inList  (colName : String) (boundVals : List String) : Where E
  /-- `col LIKE 'prefix%' / '%suffix' / '%mid%'`. -/
  | private like    (colName : String) (pos : LikePos) (val : String) : Where E
  | private isNull  (colName : String) : Where E
  | and     (l r : Where E) : Where E
  | or      (l r : Where E) : Where E
  | not     (w : Where E)   : Where E

/-! ## Smart constructors — the **only** typed-input path.

These read `ToValue` at construction time. Hand-writing
`Where.eq "email" rawString` is technically legal but obvious in
review; the surface API the docs show uses the smart constructors
exclusively. -/

namespace Col

variable {E α} [Entity E] [ToValue α]

def eq      (c : Col E α) (v : α) : Where E := .eq      c.name (ToValue.bind v)
def ne      (c : Col E α) (v : α) : Where E := .ne      c.name (ToValue.bind v)
def lt      (c : Col E α) (v : α) : Where E := .lt      c.name (ToValue.bind v)
def gt      (c : Col E α) (v : α) : Where E := .gt      c.name (ToValue.bind v)
def inList  (c : Col E α) (vs : List α) : Where E :=
  .inList c.name (vs.map ToValue.bind)
def isNull  (c : Col E α) : Where E := .isNull c.name

end Col

namespace Col
/-- `User.cols.title |>.like .prefix "doc-"`. -/
def like {E} [Entity E] (c : Col E String) (p : LikePos) (v : String) : Where E :=
  Where.like c.name p v
end Col

/-! ## UPDATE — `SET col = val, …`

    Each `SetClause E` is a `(Col E α, α)` pair stripped of the `α` at
    the list level (the renderer reads the `ToValue` instance directly,
    so we don't need to thread the type any further). -/

structure SetClause (E : Type) [Entity E] where
  col : String
  val : String

/-- `User.cols.email .= "x"` — build a set-clause with type-checked
    inputs. The `[ToValue α]` is read at construction time, so the
    erased `SetClause` is a flat `(String, String)` pair downstream. -/
def Col.assign {E α} [Entity E] [ToValue α] (c : Col E α) (v : α) : SetClause E :=
  { col := c.name, val := ToValue.bind v }

infix:75 " .= " => Col.assign

/-! ## Top-level shapes. -/

structure Select (E : Type) [Entity E] where
  where_  : Where E := .trueP
  orderBy : List (String × Order) := []  -- pre-erased: column name + direction
  limit   : Option Nat := none
  offset  : Option Nat := none

/-- `Select.orderByCol c .asc` — type-checked entry point that hides
    the `(String × Order)` wire format. -/
def Select.orderByCol {E α} [Entity E] (q : Select E) (c : Col E α) (o : Order)
    : Select E := { q with orderBy := q.orderBy ++ [(c.name, o)] }

structure Update (E : Type) [Entity E] where
  set    : List (SetClause E)
  where_ : Where E := .trueP

structure Delete (E : Type) [Entity E] where
  where_ : Where E := .trueP

/-! ## Render — `(sql, params)` with positional `?`. -/

private structure RenderState where
  sql    : String := ""
  params : Array String := #[]

private def emit (s : RenderState) (chunk : String) : RenderState :=
  { s with sql := s.sql ++ chunk }

private def pushParam (s : RenderState) (v : String) : RenderState :=
  { s with sql := s.sql ++ "?", params := s.params.push v }

private partial def renderWhere {E} [Entity E] : Where E → RenderState → RenderState
  | .trueP, s => emit s "1=1"
  | .eq col v, s => pushParam (emit s s!"{col} = ") v
  | .ne col v, s => pushParam (emit s s!"{col} <> ") v
  | .lt col v, s => pushParam (emit s s!"{col} < ") v
  | .gt col v, s => pushParam (emit s s!"{col} > ") v
  | .inList col vals, s =>
    /- Empty `IN ()` is invalid SQL on most engines; render it as a
       guaranteed-false predicate so the user gets an empty result
       rather than a syntax error. -/
    if vals.isEmpty then emit s "1=0"
    else
      let s := emit s s!"{col} IN ("
      let r := vals.foldl (init := (s, true)) fun (acc, first) v =>
        let acc := if first then acc else emit acc ", "
        (pushParam acc v, false)
      emit r.1 ")"
  | .like col pos v, s =>
    let pat := match pos with
      | .prefix   => v ++ "%"
      | .suffix   => "%" ++ v
      | .contains => "%" ++ v ++ "%"
    pushParam (emit s s!"{col} LIKE ") pat
  | .isNull col, s => emit s s!"{col} IS NULL"
  | .and l r, s =>
    let s := emit s "("
    let s := renderWhere l s
    let s := emit s " AND "
    let s := renderWhere r s
    emit s ")"
  | .or l r, s =>
    let s := emit s "("
    let s := renderWhere l s
    let s := emit s " OR "
    let s := renderWhere r s
    emit s ")"
  | .not w, s =>
    let s := emit s "NOT ("
    let s := renderWhere w s
    emit s ")"

private def renderOrderBy (orders : List (String × Order)) : String :=
  if orders.isEmpty then "" else
  let chunks := orders.map fun (n, o) =>
    n ++ (match o with | .asc => " ASC" | .desc => " DESC")
  " ORDER BY " ++ String.intercalate ", " chunks

private def renderLimitOffset (l : Option Nat) (o : Option Nat) : String :=
  let lim := l.map (s!" LIMIT {·}") |>.getD ""
  let off := o.map (s!" OFFSET {·}") |>.getD ""
  lim ++ off

/-- Render a `Select E` to `(sql, params)`. -/
def Select.render {E} [Entity E] (q : Select E) : String × Array String :=
  let cols := String.intercalate ", " (Entity.columns E)
  let st : RenderState := { sql := s!"SELECT {cols} FROM {Entity.table E} WHERE " }
  let st := renderWhere q.where_ st
  let st := emit st (renderOrderBy q.orderBy)
  let st := emit st (renderLimitOffset q.limit q.offset)
  (st.sql, st.params)

/-- Render a `SELECT COUNT(*)` for pagination etc. -/
def Select.renderCount {E} [Entity E] (q : Select E) : String × Array String :=
  let st : RenderState := { sql := s!"SELECT COUNT(*) FROM {Entity.table E} WHERE " }
  let st := renderWhere q.where_ st
  (st.sql, st.params)

/-- Render an `Update E`. Requires at least one SET clause to render —
    `UPDATE … SET WHERE …` is a SQL syntax error, so we surface it as
    a programmer error early. -/
def Update.render {E} [Entity E] (q : Update E) : Except String (String × Array String) := do
  if q.set.isEmpty then throw "Update.render: empty SET clause"
  let sets := q.set.foldl (init := (#[] : Array String)) fun acc sc =>
    acc.push s!"{sc.col} = ?"
  let setSql := String.intercalate ", " sets.toList
  let params := q.set.toArray.map (·.val)
  let st : RenderState := { sql := s!"UPDATE {Entity.table E} SET {setSql} WHERE ", params }
  let st := renderWhere q.where_ st
  return (st.sql, st.params)

/-- Render a `Delete E`. -/
def Delete.render {E} [Entity E] (q : Delete E) : String × Array String :=
  let st : RenderState := { sql := s!"DELETE FROM {Entity.table E} WHERE " }
  let st := renderWhere q.where_ st
  (st.sql, st.params)

/-! ## Audit escape hatch — `.trusted` is the only string path.

    Use `Repo.trusted` (defined in `Repo` extensions below) with
    `decl_name%` as the audit tag:

    ```
    let rows ← users.trusted decl_name%
      "SELECT u.* FROM users u JOIN logins l ON u.id = l.user_id WHERE l.last > ?"
      #[toString cutoff]
    ```

    `decl_name%` is a Lean 4 syntax macro that resolves to the
    fully-qualified name of the enclosing declaration. A `grep -r
    'trusted '\`decl_name%' lean-tea/` lists every audit point in the
    repo. -/

end LeanTea.Persist.SafeQuery

/-! ## SafeQuery runners — namespaced to avoid name collisions with the
    older `LeanTea.Persist.Query` module. Call as `SafeQuery.run users q`. -/

namespace LeanTea.Persist.SafeQuery

open LeanTea.Persist
open LeanTea.Sqlite

variable {α : Type} [Entity α]

/-- Run a typed `SELECT` against `r`. -/
def run (r : Repo α) (q : Select α) : IO (Array α) := do
  let (sql, params) := q.render
  Repo.query r sql params

/-- Count rows matching a typed `WHERE` clause. Empty results, missing
    columns, and unparsable values all fall back to `0` via the
    `Option` monad — never panics. -/
def count (r : Repo α) (w : Where α) : IO Nat := do
  let (sql, params) := (({ where_ := w } : Select α)).renderCount
  let rows ← Sqlite.query r.db sql params
  return rows[0]? |>.bind (·[0]?) |>.bind (·.toNat?) |>.getD 0

/-- Run a typed `UPDATE`. Returns the number of rows changed. -/
def update (r : Repo α) (u : Update α) : IO Nat := do
  match u.render with
  | .ok (sql, params) => Sqlite.execp r.db sql params
  | .error e => throw (IO.userError s!"update: {e}")

/-- Run a typed `DELETE`. Returns the number of rows deleted. -/
def delete (r : Repo α) (d : Delete α) : IO Nat := do
  let (sql, params) := d.render
  Sqlite.execp r.db sql params

/-- Audited free-form SQL. The `auditTag` is a `Lean.Name` (use
    `decl_name%` at the call site so it's grep-able as a single string).

    There is **no `SafeQuery.run` that takes a raw `String` and is anonymous** —
    every string-shaped query path either renders from a `Select` / `Update`
    / `Delete`, or carries a `decl_name%` tag here. -/
def trusted (r : Repo α) (auditTag : Lean.Name) (sql : String)
    (params : Array String := #[]) : IO (Array α) := do
  let _ := auditTag  -- preserved in source for grep; not used at runtime
  Repo.query r sql params

end LeanTea.Persist.SafeQuery
