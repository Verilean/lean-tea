import LeanTea.Persist.Store

/-! # Typed query DSL for Repo

A small Persistent-inspired query builder. The aim is to replace
the `s!"SELECT … WHERE x = ?"` strings sprinkled through the example
apps with composable, typed expressions:

```lean
namespace Shape
def kindC   : Col Shape String := col "kind"   id
def pageIdC : Col Shape Nat    := col "page_id" toString
end Shape

-- Find every rect on page 1:
shapes.select <| Q.where_ (Shape.kindC ==. "rect" &&. Shape.pageIdC ==. 1)

-- Move shape 5 to (100,100):
let _ ← shapes.updateWhere (Shape.idC ==. 5) [Shape.xC =. 100, Shape.yC =. 100]
```

Implementation choices:

* Columns are values, not types. Each `Col E α` knows its SQL name
  plus an `encode : α → String` so we never serialise via `Repr`.
* Filters and updates carry their values inline; `compile` walks them
  to produce a `(sql, params)` pair, which is what the underlying
  `Sqlite.execp` / `Sqlite.query` already want.
* The existing `Repo.execRaw` / `Repo.query` keep working — the DSL
  is purely additive. -/

namespace LeanTea.Persist

/-! ## Column reference -/

structure Col (E : Type) (α : Type) where
  name   : String
  encode : α → String

/-- `col name encode` — the boilerplate-free constructor. Most apps
    will end up with `def myCol : Col MyE Int := col "my_col" toString`. -/
def col {E α} (name : String) (encode : α → String) : Col E α :=
  { name, encode }

/-! ## Filter expressions

`Filter E` describes a `WHERE` clause on entity `E`. Values are kept
inline as `String` (already-encoded) so the type can be free of `α`
parameters and stay easy to compose with `andList` / `orList`. -/

inductive Filter (E : Type) where
  | true_
  | false_
  | cmp (op : String) (column : String) (value : String)
  | isNull (column : String)
  | notNull (column : String)
  | inList (column : String) (values : List String)
  | and_ : Filter E → Filter E → Filter E
  | or_  : Filter E → Filter E → Filter E
  | not_ : Filter E → Filter E
  deriving Inhabited

namespace Filter

/-! ### Smart constructors that lift typed values via `Col.encode`. -/

def eq {E α} (c : Col E α) (v : α) : Filter E := .cmp "=" c.name (c.encode v)
def ne {E α} (c : Col E α) (v : α) : Filter E := .cmp "<>" c.name (c.encode v)
def lt {E α} (c : Col E α) (v : α) : Filter E := .cmp "<" c.name (c.encode v)
def gt {E α} (c : Col E α) (v : α) : Filter E := .cmp ">" c.name (c.encode v)
def le {E α} (c : Col E α) (v : α) : Filter E := .cmp "<=" c.name (c.encode v)
def ge {E α} (c : Col E α) (v : α) : Filter E := .cmp ">=" c.name (c.encode v)
def like (c : Col E String) (pat : String) : Filter E := .cmp "LIKE" c.name pat

/-- Conjoin a list, returning `true_` for the empty list. -/
def andList : List (Filter E) → Filter E
  | []      => .true_
  | [f]     => f
  | f :: fs => .and_ f (andList fs)

def orList : List (Filter E) → Filter E
  | []      => .false_
  | [f]     => f
  | f :: fs => .or_ f (orList fs)

/-- Compile to `(sql, params)`. Parameters are appended in left-to-
    right order so positional `?` placeholders bind correctly. -/
partial def compile : Filter E → (String × Array String)
  | .true_              => ("1", #[])
  | .false_             => ("0", #[])
  | .cmp op col v       => (s!"{col} {op} ?", #[v])
  | .isNull col         => (s!"{col} IS NULL", #[])
  | .notNull col        => (s!"{col} IS NOT NULL", #[])
  | .inList col []      => ("0", #[])  -- IN () is invalid; never matches
  | .inList col vs      =>
    let qs := String.intercalate "," (vs.map (fun _ => "?"))
    (s!"{col} IN ({qs})", vs.toArray)
  | .and_ a b           =>
    let (sa, pa) := compile a
    let (sb, pb) := compile b
    (s!"({sa}) AND ({sb})", pa ++ pb)
  | .or_  a b           =>
    let (sa, pa) := compile a
    let (sb, pb) := compile b
    (s!"({sa}) OR ({sb})", pa ++ pb)
  | .not_ a             =>
    let (sa, pa) := compile a
    (s!"NOT ({sa})", pa)

end Filter

/-! ## Operator notation

Mirrors Persistent's `==.` / `&&.` etc. Operators sit between `==`
(`50`) and `&&` (`35`) priorities so chaining feels natural. -/

infix:50 " ==. " => Filter.eq
infix:50 " !=. " => Filter.ne
infix:50 " <. "  => Filter.lt
infix:50 " >. "  => Filter.gt
infix:50 " <=. " => Filter.le
infix:50 " >=. " => Filter.ge
infix:50 " ~. "  => Filter.like
infixr:35 " &&. " => Filter.and_
infixr:30 " ||. " => Filter.or_

/-! ## Updates

`Update E` is a list of `column ← value` assignments. Same encoding
strategy as `Filter`: smart constructors thread the typed `encode`. -/

structure UpdateSet (E : Type) where
  column : String
  value  : String
  deriving Inhabited

def Col.assign {E α} (c : Col E α) (v : α) : UpdateSet E :=
  { column := c.name, value := c.encode v }

infix:30 " =. " => Col.assign

/-! ## Ordering & query body -/

inductive Order (E : Type) where
  | asc  (column : String)
  | desc (column : String)
  deriving Inhabited

def Col.asc  {E α} (c : Col E α) : Order E := .asc  c.name
def Col.desc {E α} (c : Col E α) : Order E := .desc c.name

/-- A `Select` describes one read-only query against an entity.
    Default-initialised values mean "SELECT * with no filter". -/
structure Select (E : Type) where
  filter  : Filter E := .true_
  orders  : List (Order E) := []
  limit?  : Option Nat := none
  offset? : Option Nat := none
  deriving Inhabited

namespace Select

/-- Builder helpers so a chain like `Q.from |>.where_ f |>.orderBy o`
    reads top-to-bottom. They mutate by returning a fresh record. -/
def empty : Select E := {}
def where_ (q : Select E) (f : Filter E) : Select E :=
  { q with filter := q.filter.and_ f }
def orderBy (q : Select E) (o : Order E) : Select E :=
  { q with orders := q.orders ++ [o] }
def limit (q : Select E) (n : Nat) : Select E :=
  { q with limit? := some n }
def offset (q : Select E) (n : Nat) : Select E :=
  { q with offset? := some n }

private def renderOrders : List (Order E) → String
  | []      => ""
  | orders  =>
    let parts := orders.map fun
      | .asc  c => s!"{c} ASC"
      | .desc c => s!"{c} DESC"
    " ORDER BY " ++ String.intercalate ", " parts

/-- Produce `(sql, params)` for a full `SELECT * FROM …` query. -/
def compile (q : Select E) (table : String) : String × Array String :=
  let (whereSql, params) := q.filter.compile
  let head := s!"SELECT * FROM {table} WHERE {whereSql}"
  let withOrd := head ++ renderOrders q.orders
  let withLim := match q.limit? with
    | some n => withOrd ++ s!" LIMIT {n}"
    | none   => withOrd
  let withOff := match q.offset? with
    | some n => withLim ++ s!" OFFSET {n}"
    | none   => withLim
  (withOff, params)

end Select

/-! ## Repo extensions

Plug the DSL into the existing `Repo` so call sites switch from raw
SQL to typed queries without changing the surrounding code. -/

namespace Repo
variable {E : Type} [Entity E]

def select (r : Repo E) (q : Select E) : IO (Array E) := do
  let (sql, ps) := q.compile (Entity.table E)
  Repo.query r sql ps

def selectFirst (r : Repo E) (q : Select E) : IO (Option E) := do
  let rows ← r.select (q.limit 1)
  return rows[0]?

/-- Returns the row count satisfying `f`. Implemented as a separate
    SQL call so the entity decoder doesn't need to know about
    `COUNT(*)`. -/
def count (r : Repo E) (f : Filter E := .true_) : IO Nat := do
  let (whereSql, ps) := f.compile
  let rows ← Sqlite.query r.db
    s!"SELECT COUNT(*) FROM {Entity.table E} WHERE {whereSql}" ps
  return (rows[0]?.bind (·[0]?)).bind (·.toNat?) |>.getD 0

/-- Update rows matching `f`. Returns the affected row count. -/
def updateWhere (r : Repo E) (f : Filter E) (sets : List (UpdateSet E)) : IO Nat := do
  if sets.isEmpty then return 0
  let assigns := sets.map fun u => s!"{u.column} = ?"
  let setSql := String.intercalate ", " assigns
  let (whereSql, ps) := f.compile
  let sql :=
    s!"UPDATE {Entity.table E} SET {setSql} WHERE {whereSql}"
  let params := sets.toArray.map (·.value) ++ ps
  Sqlite.execp r.db sql params

def deleteWhere (r : Repo E) (f : Filter E) : IO Nat := do
  let (whereSql, ps) := f.compile
  Sqlite.execp r.db
    s!"DELETE FROM {Entity.table E} WHERE {whereSql}" ps

end Repo

end LeanTea.Persist
