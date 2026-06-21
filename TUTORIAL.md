# lean-elm Tutorial & Feature Audit

This document does three things:

1. **Inventory** every primitive lean-elm ships today
2. **Assesses sufficiency** — for each piece, is it production-ready,
   MVP-only, or missing important parts?
3. **Walks through real workflows** to measure turn-around time (TAT)
   — how fast can a developer go from "I want to add X" to a working
   `lake exe` binary?

The audit is honest: insufficient pieces are flagged. Treat the
"gaps" section as a roadmap, not a complaint.

---

## 1. Feature inventory

### 1.1 Runtime primitives

| Module | What it does | Sufficiency |
|---|---|---|
| `LeanTea.Cmd` | Side-effect command type wrapping `IO Msg` | ✅ enough for TUI / Web step apps |
| `LeanTea.Sub` | Subscription type — timer + stdin readline | ⚠ MVP; no signals, no websockets |
| `LeanTea.Runtime` | TUI ANSI loop driving `App = {init, update, view, subs}` | ✅ Counter / Quiz examples ship |
| `LeanTea.Web` | Pure `step : Model? → Msg? → (Model, Html)` for web step apps (`english`) | ✅ used in production by the English app |

### 1.2 View layer

| Module | What it does | Sufficiency |
|---|---|---|
| `LeanTea.Html` | Minimal Html AST + escape-aware `render`; SVG tags share the same node type | ✅ enough; works as both HTML and inline SVG |
| `LeanTea.Css` | `Sheet := List Rule` with `rule / media / keyframes / raw`; rendered as a string | ✅ enough for app-scale stylesheets |
| `LeanTea.Js` | Typed JS AST with helper namespaces `E.*` (expr) and `S.*` (stmt); used to codegen client glue | ⚠ enough for the RPC client codegen; the surface area is small (no try/catch, no classes) |

### 1.3 Typed HTTP / RPC

| Module | What it does | Sufficiency |
|---|---|---|
| `LeanTea.Net.Http` | `Request` / `Response` records + helpers (`text`, `html`, `notFound`) | ✅ |
| `LeanTea.Net.Server` | HTTP/1.1 server on `Std.Internal.Async.TCP` (libuv) | ✅ for app traffic; no h2/h3, no graceful shutdown |
| `LeanTea.Rpc` | Servant-style typed endpoints. One `Endpoint` declaration drives both server dispatch *and* client JS codegen | ✅ used by every server we ship |
| `LeanTea.JsonRpc` | JSON-RPC 2.0 server + curl-backed client + a small `Schema` for param validation | ✅ MVP; lacks SSE streaming and websocket transport |
| `LeanTea.Auth` | Google OAuth 2.0 via curl(1); cookie session via SQLite | ⚠ Google-only; no multi-provider, no refresh-token flow |
| `LeanTea.WebGpu` | Emits HTML + module JS that boots a WebGPU canvas given one WGSL fragment shader. Includes a stock fullscreen-triangle vertex shader and a `(resolution, time)` uniform block | ✅ for shadertoy-style apps; needs more for compute pipelines / multi-pass |

### 1.4 Persistence

| Module | What it does | Sufficiency |
|---|---|---|
| `LeanTea.Persist.Sqlite` | FFI to a vendored sqlite3 amalgamation (`c/sqlite3.c`) — `open` / `exec` / `execp` / `query` | ✅ zero extern deps; ships as a fat static lib |
| `LeanTea.Persist.Store` | Persistent-style `Entity` typeclass + `Repo α` for typed CRUD | ✅ |
| `LeanTea.Persist.Query` | Persistent-style typed query DSL — `Col`, `Filter`, `Update`, `Select` with `==.` / `&&.` / `=.` / `Repo.select` / `updateWhere` / `deleteWhere` | ✅ for vanilla CRUD; missing joins, aggregates beyond COUNT, expression-level updates (`x += dx`) |
| `LeanTea.Persist.Backend` | Composable backend layer — `Backend := { exec, query, close }`, `Sqlite.Db.toBackend`, `Backend.shard`, `Backend.shardByParam`, `Cache` interface + LRU, `Backend.cached` decorator, `RepoB` for Backend-backed repos | ✅ for stacking shards / caches; routing assumes "user_id is params[0]" convention |
| `LeanTea.Persist.Mysql` | libmysqlclient FFI — `Conn.toBackend` slots MySQL into the Backend stack. Conditional build: `LEANTEA_MYSQL=1 lake build` enables; without the flag the wrapper is stub-mode | ✅ when enabled; stub returns a clear "rebuild with" error otherwise |
| `LeanTea.Persist.Migrate` | Versioned migration runner against a `Backend`. `Migration { version, description, up, down? }`, `run` / `rollback` / `status`. Bookkeeping in `schema_migrations` table | ✅ for additive changes; no auto-generated schema diff yet |
| `LeanTea.Net.Memcached` | TCP text-protocol client (`get` / `set` / `delete` / `flush_all`) + `Client.asCache` so memcached drops straight into `Backend.cached`. Keys hashed via FNV-1a 64 so the 250-byte / no-whitespace constraint holds for any input | ✅ for cache-aside; no SASL auth, no binary protocol, no multi-key get |

### 1.5 Example apps (live in `examples/`)

| App | Status | What it demonstrates |
|---|---|---|
| `counter`, `counter_web` | ✅ | The Runtime primitives end-to-end |
| `quiz` | ✅ | Static data, multi-screen TUI |
| `canvas_serve` | ✅ | Full web app: SQLite-backed model, OAuth gate, X-Model header, static-asset auto-resolve |
| `canvas_serve` | ✅ | Figma-lite editor with: pages, groups, undo/redo, image / web-embed / VRM shapes, MCP server (20 tools), live sync across tabs |
| `mech_serve` | ✅ | Hosts the mech 2D game + splices a VRM character overlay that runs / jumps in sync with the player state |
| `gpu_serve` | ✅ | Minimal WebGPU demo. Stock animated-color shader on a fullscreen canvas — swap the WGSL string to ship a new shader sketch in one rebuild |

---

## 2. Sufficiency assessment

### What's enough today

A team could ship the following without writing new framework code:

- A typed JSON over HTTP API (server + JS client) — `Rpc`
- A JSON-RPC service callable from MCP clients or other lean-elm
  apps — `JsonRpc`
- A SQLite-backed persistent model with typed CRUD — `Persist.Query`
- A static-rendered web app with hand-authored client JS — `Html` +
  `Css` + `Js` DSLs
- A signed-in user surface gated behind Google OAuth — `Auth`
- An interactive SVG editor (Canvas as the proof point)

### Known gaps (in rough priority order)

| Gap | Workaround today | Risk |
|---|---|---|
| ~~Composable Backend — sharding / cache / MySQL decorators~~ | Shipped via `LeanTea.Persist.Backend` + `Memcached` + `Mysql` | n/a |
| **WebSocket transport** | Polling via `/api/version` (Canvas does this) | Wastes CPU on idle pages; can't push >1Hz updates cleanly |
| **WebGPU compute / multi-pass** | `LeanTea.WebGpu` ships single-fragment render only | Sufficient for shader sketches; compute pipelines need a richer wrapper |
| **Native async HTTP client** | Shell out to curl(1) for outbound calls | Per-call process fork; no connection pooling; no streaming |
| ~~Migration tooling~~ | Shipped via `LeanTea.Persist.Migrate` (versioned, up/down) | n/a |
| **Hot reload of Lean code** | `tools/dev.py` watches files and restarts the binary | ~5s rebuild cycle; could be tighter |
| **Test framework** | Smoke-test executables (`*_smoke`) under `examples/Smoke/` | Works, but no fixtures / assertions / parallelism |
| **Tracing / metrics** | Plain `IO.println` | Hard to observe in prod |
| **MCP transports beyond POST** | None (we serve JSON-RPC over POST only) | Streaming tools (long-running) can't report progress |

The **composable backend** and **WebGPU** items are the two pieces a
casual user is most likely to ask about; both are scoped but
unstarted.

---

## 3. TAT walk-throughs

For each, the question is: *starting from a fresh checkout, how
many minutes from "I want to add X" to a working build?*

### 3.1 Add a new typed HTTP endpoint to Canvas (≈ 3 min)

```lean
-- examples/Canvas/Api.lean
def listGroups : Endpoint := {
  name := "apiListGroups", path := "/api/groups", method := "GET",
  params := [], output := .json
}

def handleListGroups (st : Store) : Handler := fun _ => do
  -- compose query from Persist.Query DSL
  let _ := st  -- ...
  return "[]"

-- add to `all` and `routes`
def all : List Endpoint := [ ... listGroups ]
def routes (st : Store) : List Route := [ ..., {ep := listGroups, handler := handleListGroups st} ]
```

The browser-side `apiListGroups` JS client function is auto-generated
by `Rpc.clientLib all` — no manual fetch wrapper needed.

### 3.2 Add an MCP tool (≈ 5 min)

Two edits in `examples/Canvas/Serve.lean`:

1. Add a `toolDef "name" "desc" #[argSchema …] #[…]` entry to the
   `toolsList` array.
2. Add a `| "name" => …` arm to `callTool`'s `match`.

The schema declared via `argSchema` doubles as both the discovery
descriptor and the validator (because we walk it before dispatching).

### 3.3 Add a JSON-RPC method callable from outside (≈ 4 min)

```lean
open LeanTea.JsonRpc

def myMethod : Method := {
  name := "echo",
  params := .object [("msg", .string_)] ["msg"],
  result := .string_
}

def server : Server := { routes := [
  { method := myMethod,
    handler := fun args => do
      let msg := (args.getObjVal? "msg").toOption.bind (·.getStr?.toOption) |>.getD ""
      return Json.str msg }
]}

-- Mount under `/rpc` (or wherever):
def main := Net.Server.serve 8080 "0.0.0.0" (Server.toHandler server "/rpc")
```

Then `JsonRpc.call "http://…/rpc" "echo" (Json.mkObj [("msg", Json.str "hi")])`
from any other lean-elm process. Schema validation, error envelopes,
and notifications all handled for you.

### 3.4 Add a new database entity (≈ 10 min)

```lean
-- 1. Struct + Entity instance (codec)
structure Task where
  id    : Nat
  title : String
  done  : Bool
  deriving Inhabited, Repr

instance : Entity Task where
  table := "tasks"
  ddl := "CREATE TABLE IF NOT EXISTS tasks(id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, done INTEGER NOT NULL DEFAULT 0)"
  columns := ["title", "done"]
  toRow t := #[t.title, if t.done then "1" else "0"]
  fromRow row := ... -- match on row, parse, build Task

-- 2. Column refs for the DSL
namespace Task
def idC    : Col Task Nat    := col "id"    toString
def titleC : Col Task String := col "title" (·)
def doneC  : Col Task Bool   := col "done"  (fun b => if b then "1" else "0")
end Task

-- 3. Use it
let tasks : Repo Task := Repo.new db
tasks.migrate
let _ ← tasks.insert { id := 0, title := "buy milk", done := false }
let open' ← tasks.select <| Select.empty.where_ (Task.doneC ==. false)
```

The codec is the bulk of the time; the query DSL itself adds almost
nothing.

### 3.5 Stand up a whole new app (≈ 30 min)

Pattern (see Canvas / English / Mech for working examples):

1. `examples/MyApp/App.lean` — model + entities + `Store` record + DDL
2. `examples/MyApp/Api.lean` — `Endpoint` list + handlers
3. `examples/MyApp/Serve.lean` — HTML shell, JS client, MCP tools,
   `Rpc.chainWith` to bolt the API onto the server
4. `lakefile.lean` — add `lean_exe my_app_serve` + register module
   under `Examples` srcDir
5. `lake build my_app_serve && ./.lake/build/bin/my_app_serve`

The four apps we ship sit at 200–1100 lines of Lean each. Most of
that is feature surface, not framework boilerplate.

---

## 4. TAT verdict

The framework optimises for *one-file-edit* changes: adding an
endpoint, a tool, a column, or a CSS rule is a single contiguous
diff. The chains that get long are:

- **First build of a new exe** ≈ 30s on a warm cache, 2–3 min cold
  (sqlite amalgamation re-link)
- **Live reload** — current `tools/dev.py` runs at 2.5 Hz file-mtime
  poll + restart on change; rebuild dominates so user-visible
  latency is the Lean build, not the watcher
- **JS-only changes** — even though `Js.lean` is a typed DSL, the
  client JS is concatenated string-style in `Serve.lean`, so any
  edit requires a Lean rebuild. The biggest TAT improvement available
  right now is splitting client JS into a real `.js` file the server
  reads at request time (we already do that for English's
  `runtime.js`).

Concrete proposal to shrink TAT further:

| Idea | Expected impact |
|---|---|
| Move per-app client JS into `examples/MyApp/static/runtime.js` so editing it doesn't trigger a Lean rebuild | -25s on the typical iteration |
| Pre-build the SQLite amalgamation as a separate static lib outside Lake | -1 min on cold builds |
| Add an `--watch` mode to each `*_serve` that re-reads static assets without restarting | makes asset edits feel instant |

---

## 5. Where this leaves us

- For **server-side apps + a typed RPC contract**: the stack is
  ready. Build away.
- For **interactive single-page UIs**: usable, but JS still lives
  as a string concatenation. Splitting into `.js` files is the next
  productivity win.
- For **real-time / collaborative**: blocked on WebSockets.
- For **GPU shaders**: ready via `LeanTea.WebGpu` (render-only).
  Compute pipelines and multi-pass setups would need a richer
  wrapper.
- For **scale beyond one SQLite**: ready via `Persist.Backend` +
  `Persist.Mysql` + `Net.Memcached`. Stack example:

  ```lean
  let shardA := dbA.toBackend
  let shardB := mysqlConn.toBackend
  let cache  := mc.asCache
  let backend :=
    (Backend.shardByParamNat #[shardA, shardB] 0).cached cache
  Migrate.run backend [m1, m2, m3]
  ```

The remaining open items (WebSocket transport, hot reload, test
framework) are pure ergonomics — they shorten TAT but the framework
already covers the deploy targets we have apps for.
