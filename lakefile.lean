import Lake
open Lake DSL System

package «lean-tea» where
  -- Pre-compile module C output so `lean_exe` targets can link the
  -- SQLite FFI without a separate `lean -c` pass at link time.
  precompileModules := false

lean_lib LeanTea where
  roots := #[`LeanTea]

/-- A small Fay-style Lean-subset → JavaScript compiler. Used by the
    LSpec tests under `examples/Tests/` that exec the emitted JS via
    `node`. Lives in its own library so apps that don't need it don't
    pay the build cost. -/
lean_lib LeanJs where
  roots := #[`LeanJs.Ast, `LeanJs.Parser, `LeanJs.Codegen,
             `LeanJs.Eval, `LeanJs.JsParser, `LeanJs.LeanEmit,
             `LeanJs.Check, `LeanJs.Includes]

lean_lib Examples where
  srcDir := "examples"
  roots := #[
    `Sheet.App,
    `Reversi.Game,
    `Tools.GenSite,
    `Tools.LeanJsCompile, `Tools.LeanJsInterp, `Tools.LeanJsRun,
    `Smoke.Crypto
  ]
  /- Private examples (English Learning + game shells under
     `examples/_private/`) ship third-party content (arXiv quotes,
     various brand-licensed derivatives, NSFW
     character art). They're `.gitignore`d. To build them locally,
     append their roots here. -/

/-! ## SQLite FFI

`leantea_sqlite.c` is built as a single .o then bundled into a static
library that Lake links into every executable. We rely on the host
having `libsqlite3` available (added to `shell.nix` via `sqlite.dev`). -/

/-- SQLite amalgamation `c/sqlite3.c` is vendored in this repo so the
    Lean binary embeds sqlite directly and has no external -lsqlite3
    dependency. The amalgamation is ~9 MB but builds cleanly with cc. -/

target sqlite3_o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "sqlite3.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "sqlite3.c"
  let weakArgs := #["-I", (pkg.dir / "c").toString]
  let traceArgs := #["-fPIC", "-O1",
    "-DSQLITE_OMIT_LOAD_EXTENSION=1",
    "-DSQLITE_THREADSAFE=1",
    "-DSQLITE_DEFAULT_MEMSTATUS=0",
    -- On Linux, glibc's LFS macros transparently rename `fcntl` to
    -- `fcntl64` (etc.) in the emitted .o, but Lean's bundled libc
    -- doesn't ship those symbols. Disable LFS so glibc emits plain
    -- 32-bit-offset variants. Limits sqlite to 2GB DBs, which is
    -- fine for our app.
    "-D_FILE_OFFSET_BITS=32",
    "-U_LARGEFILE_SOURCE",
    "-U_LARGEFILE64_SOURCE",
    "-Wno-everything"]
  buildO oFile srcJob weakArgs traceArgs "cc"

target leantea_sqlite_o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "leantea_sqlite.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "leantea_sqlite.c"
  let weakArgs := #[
    "-I", (← getLeanIncludeDir).toString,
    "-I", (pkg.dir / "c").toString
  ]
  buildO oFile srcJob weakArgs #["-fPIC", "-O2"] "cc"

extern_lib libleantea_sqlite pkg := do
  let name := nameToStaticLib "leantea_sqlite"
  let wrapperO ← leantea_sqlite_o.fetch
  let sqliteO  ← sqlite3_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[wrapperO, sqliteO]

/-! ## MySQL FFI — conditional on `LEANTEA_MYSQL=1`

The wrapper always compiles so links don't fail; `-DLEANTEA_HAVE_MYSQL`
toggles between the real driver and stub-mode (every call returns
"not built in"). When enabled we shell out to `mysql_config` for the
include path + linker flags. -/

target leantea_mysql_o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "leantea_mysql.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "leantea_mysql.c"
  let enabled := (← IO.getEnv "LEANTEA_MYSQL").map (·.toLower == "1")
                  |>.getD false
  let mut weakArgs : Array String := #[
    "-I", (← getLeanIncludeDir).toString
  ]
  let mut traceArgs : Array String := #["-fPIC", "-O2"]
  if enabled then
    let cf ← IO.Process.output {
      cmd := "mysql_config", args := #["--cflags"] }
    if cf.exitCode == 0 then
      for tok in cf.stdout.trim.splitOn " " do
        if !tok.isEmpty then weakArgs := weakArgs.push tok
    traceArgs := traceArgs.push "-DLEANTEA_HAVE_MYSQL"
  buildO oFile srcJob weakArgs traceArgs "cc"

extern_lib libleantea_mysql pkg := do
  let name := nameToStaticLib "leantea_mysql"
  let wrapperO ← leantea_mysql_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[wrapperO]

/-! ## Crypto FFI — conditional on `LEANTEA_CRYPTO=1`

OpenSSL libcrypto wrapper for SHA-256 / HMAC / PBKDF2 / RSA / ECDSA
verification. Same opt-in shape as MySQL: when enabled we look up
OpenSSL via `pkg-config --cflags openssl`; when disabled the wrapper
compiles to a stub that throws on first use, and the pure-Lean
fallbacks in `LeanTea.Crypto` keep the build green.

To use the real backend at link time, add `-lcrypto` (and `-lssl` if
needed) via `NIX_LDFLAGS` or the exe's `weakLinkArgs`. The C side
also references PEM parsing so a full libcrypto is required, not a
trimmed-down crypto-only subset. -/

target leantea_crypto_o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "leantea_crypto.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "leantea_crypto.c"
  let enabled := (← IO.getEnv "LEANTEA_CRYPTO").map (·.toLower == "1")
                  |>.getD false
  let mut weakArgs : Array String := #[
    "-I", (← getLeanIncludeDir).toString
  ]
  let mut traceArgs : Array String := #["-fPIC", "-O2"]
  if enabled then
    /- Find OpenSSL headers. Try pkg-config first (Linux distros +
       most Nix configs), then known Homebrew paths on macOS. -/
    let mut foundHeader := false
    let cf ← (IO.Process.output {
      cmd := "pkg-config", args := #["--cflags", "openssl"] }
        ).catchExceptions (fun _ => pure { exitCode := 1, stdout := "", stderr := "" })
    if cf.exitCode == 0 && !cf.stdout.trim.isEmpty then
      for tok in cf.stdout.trim.splitOn " " do
        if !tok.isEmpty then weakArgs := weakArgs.push tok
      foundHeader := true
    if !foundHeader then
      for guess in [
        "/opt/homebrew/opt/openssl@3/include",
        "/opt/homebrew/include",
        "/usr/local/opt/openssl@3/include",
        "/usr/local/include"] do
        if ← System.FilePath.pathExists (guess ++ "/openssl/evp.h") then
          weakArgs := weakArgs ++ #["-I", guess]
          foundHeader := true
          break
    traceArgs := traceArgs.push "-DLEANTEA_HAVE_CRYPTO"
  buildO oFile srcJob weakArgs traceArgs "cc"

extern_lib libleantea_crypto pkg := do
  let name := nameToStaticLib "leantea_crypto"
  let wrapperO ← leantea_crypto_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[wrapperO]

/-! ## Desktop (OS-level mouse + screenshot) FFI — `LEANTEA_DESKTOP=1`

Same opt-in shape as MySQL. macOS Quartz only today; flip the env
var, accept the macOS Accessibility / Screen-Recording permission
prompt the first time, and the `desktop_*` MCP tools start moving
the real mouse. Without the flag the wrapper compiles to a stub
that returns an IO error on first use, so the rest of the project
keeps building unchanged. -/

target leantea_desktop_o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "leantea_desktop.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "leantea_desktop.c"
  let enabled := (← IO.getEnv "LEANTEA_DESKTOP").map (·.toLower == "1")
                  |>.getD false
  let weakArgs : Array String := #[
    "-I", (← getLeanIncludeDir).toString
  ]
  let mut traceArgs : Array String := #["-fPIC", "-O2"]
  if enabled then
    traceArgs := traceArgs.push "-DLEANTEA_HAVE_DESKTOP"
  buildO oFile srcJob weakArgs traceArgs "cc"

extern_lib libleantea_desktop pkg := do
  let name := nameToStaticLib "leantea_desktop"
  let wrapperO ← leantea_desktop_o.fetch
  let staticLib ← buildStaticLib (pkg.staticLibDir / name) #[wrapperO]
  /- On macOS with the real backend we need to pull in the system
     frameworks at link time. The `weakArgs` list above only affects
     the object compile; framework flags belong on the executable's
     link line. Lake exposes those via `lean_exe.weakLinkArgs`, so
     each `desktop_*` exe that depends on this lib must add the
     frameworks. -/
  return staticLib

@[default_target]
lean_exe counter where
  srcDir := "examples"
  root := `Counter.Main

lean_exe quiz where
  srcDir := "examples"
  root := `Quiz.Main

lean_exe counter_web where
  srcDir := "examples"
  root := `CounterWeb.Main

lean_exe sqlite_smoke where
  srcDir := "examples"
  root := `Smoke.Sqlite

lean_exe http_smoke where
  srcDir := "examples"
  root := `Smoke.Http

lean_exe http_client_smoke where
  srcDir := "examples"
  root := `Smoke.HttpClient

lean_exe template_smoke where
  srcDir := "examples"
  root := `Smoke.Template

lean_exe query_smoke where
  srcDir := "examples"
  root := `Smoke.Query

lean_exe backend_smoke where
  srcDir := "examples"
  root := `Smoke.Backend

lean_exe migrate_smoke where
  srcDir := "examples"
  root := `Smoke.Migrate

lean_exe memcached_smoke where
  srcDir := "examples"
  root := `Smoke.Memcached

lean_exe mysql_smoke where
  srcDir := "examples"
  root := `Smoke.Mysql

/-! ## Executable documentation

Each chapter is a runnable binary so the prose in `docs/` can't
drift from working code. Adding a new chapter: drop a `Chxx_*.lean`
under `examples/Docs/`, add a `lean_exe doc_chxx` entry here, and
mirror the doc in `docs/xx-*.md`. -/

lean_exe doc_ch01 where
  srcDir := "examples"
  root := `Docs.Ch01_Hello

lean_exe doc_ch02 where
  srcDir := "examples"
  root := `Docs.Ch02_Persistence

lean_exe doc_ch03 where
  srcDir := "examples"
  root := `Docs.Ch03_JsDsl

lean_exe doc_ch04 where
  srcDir := "examples"
  root := `Docs.Ch04_TypedRpc

lean_exe doc_ch05 where
  srcDir := "examples"
  root := `Docs.Ch05_MonadicJs

lean_exe doc_ch09 where
  srcDir := "examples"
  root := `Docs.Ch09_Reversi

lean_exe leanjs_spec where
  srcDir := "examples"
  root := `Tests.LeanJsSpec

/-- Crypto smoke test (SHA-256 / HMAC / PBKDF2 / Base64 / Password)
    against RFC test vectors. With `LEANTEA_CRYPTO=1` the smoke also
    exercises the native libcrypto path.

    `weakLinkArgs` below hard-codes the Homebrew openssl@3 location
    so this works out of the box on macOS dev machines. Linux: swap
    in `-L/usr/lib -lcrypto` or set `NIX_LDFLAGS` if your build is
    Nix-managed. Without `LEANTEA_CRYPTO=1` the C wrapper compiles
    to stubs that don't reference libcrypto symbols, so the flags
    are harmless. -/
lean_exe crypto_smoke where
  srcDir := "examples"
  root := `Smoke.Crypto
  weakLinkArgs := #[
    "-L/opt/homebrew/opt/openssl@3/lib",
    "-L/usr/local/opt/openssl@3/lib",
    "-lcrypto"]

/-- LeanJs CLI trio:
    * `leanjs_compile file.leanjs [-o out.js]` — pure compiler
      (parse + check + codegen). No execution.
    * `leanjs_interp  file.leanjs` — pure-Lean evaluator via
      `LeanJs.Eval`. No node, no FFI.
    * `leanjs_run     file.leanjs` — compile + execute via `node`.
      Full feature set including externs / async / await. -/
lean_exe leanjs_compile where
  srcDir := "examples"
  root := `Tools.LeanJsCompile

lean_exe leanjs_interp where
  srcDir := "examples"
  root := `Tools.LeanJsInterp

lean_exe leanjs_run where
  srcDir := "examples"
  root := `Tools.LeanJsRun

lean_exe jsonrpc_smoke where
  srcDir := "examples"
  root := `Smoke.JsonRpc

lean_exe jsonrpc_server_smoke where
  srcDir := "examples"
  root := `Smoke.JsonRpcServer

lean_exe jsonrpc_client_smoke where
  srcDir := "examples"
  root := `Smoke.JsonRpcClient

/-- Smoke test for the OpenAI-compatible streaming client. Expects a
    local LM Studio at http://127.0.0.1:11211/v1. Lists models, runs
    one non-streaming chat, then a streaming chat, printing each token
    as it arrives. Skip in CI — this requires a live model server. -/
lean_exe openai_smoke where
  srcDir := "examples"
  root := `Smoke.Openai

/-- End-to-end demo: drive Chromium via Playwright, screenshot a page,
    feed it to a vision model. Requires the Node bridge under
    `tools/browser-bridge/` (`npm install` + `npx playwright install
    chromium` to set up), plus a live LM Studio with a vision model. -/
lean_exe browser_vision_smoke where
  srcDir := "examples"
  root := `Smoke.BrowserVision

/-- MCP server that exposes the Playwright-backed `LeanTea.Browser`
    tools at `POST /mcp`. Hook this into Claude / Cursor / any MCP
    client to give the model browser-driving abilities — `navigate`,
    `click`, `fill`, `screenshot`, `evaluate`, etc. -/
lean_exe browser_mcp_serve where
  srcDir := "examples"
  root := `BrowserMcp.Serve

/-- MCP server for HTML/CSS-based image composition. Speech bubbles,
    visual-novel caption bars, anything you can lay out in CSS becomes
    a one-call PNG. Reuses the Playwright bridge for rendering so
    fonts (incl. Google Fonts), kerning, and shadow effects match
    the in-game UI styling. -/
lean_exe image_mcp_serve where
  srcDir := "examples"
  root := `ImageMcp.Serve

/-- MCP server that drives a local ComfyUI install via its HTTP API.
    Tools: status, models, txt2img (one-shot), submit_workflow (raw
    graph), wait. The same `ui_*` shared map is mounted so prompts /
    seeds for verified-good game assets persist between runs. -/
lean_exe comfyui_mcp_serve where
  srcDir := "examples"
  root := `ComfyuiMcp.Serve

/-- MCP server controlling a real Chrome instance via the Chrome
    DevTools Protocol. Chrome must be launched with
    `--remote-debugging-port=9222 --user-data-dir=…` (a separate
    profile is required on modern Chrome). Tools: chrome_targets,
    chrome_navigate, chrome_evaluate, chrome_screenshot,
    chrome_click, chrome_fill. -/
lean_exe chrome_cdp_mcp_serve where
  srcDir := "examples"
  root := `ChromeCdpMcp.Serve

/-- MCP server backed by OS-level mouse / screenshot (Quartz on
    macOS today). Same JSON-RPC shape as `browser_mcp_serve` but
    the tools move the real mouse and capture the whole display —
    works for canvas games, native apps, anything visible.

    ## Build modes

    **Stub** (default — what the standard `lake build` produces):
    compiles and runs, every `desktop_*` tool returns
    `"desktop support not compiled in"`. Useful for verifying the
    catalogue / wiring without touching system frameworks.

    **Real** (macOS Quartz): two steps.
    1. Tell the C wrapper to include the implementation:
       `LEANTEA_DESKTOP=1 lake build desktop_mcp_serve`
    2. Provide the linker with the macOS frameworks. On a stock
       `xcrun`-managed Mac that's just `-framework
       ApplicationServices -framework CoreGraphics -framework ImageIO
       -framework CoreServices`; on Nix you also need `-F
       $(xcrun --show-sdk-path)/System/Library/Frameworks`. Either
       hand-patch `weakLinkArgs` below or use a wrapper build script
       that sets `NIX_LDFLAGS` before `lake build`.

    The two-step dance is intentional — wiring framework paths into
    the lakefile would make `lake build` brittle on other platforms
    / shells. Keeping the real path opt-in keeps the rest of the
    project portable. -/
lean_exe desktop_mcp_serve where
  srcDir := "examples"
  root := `DesktopMcp.Serve

/-- Agent loop: user → LM Studio (Gemma 4) → MCP → browser. Spawns
    `browser_mcp_serve` as a child, exposes its tool catalogue to the
    LLM via OpenAI function-calling, dispatches tool_calls, loops.
    Use this to offload UI-testing tasks to a local model. -/
lean_exe browser_agent where
  srcDir := "examples"
  root := `BrowserAgent.Run

/-- Deterministic UI script runner: replays a JSON script of clicks +
    waits + asserts against `browser_mcp_serve`. Optional `--classify`
    sends each step's screenshot to a local LLM for screen-name
    verification. Pair with `browser_agent` (LLM-discovered → record
    once, replay many) for cheap regression tests. -/
lean_exe ui_script where
  srcDir := "examples"
  root := `UiScript.Run

/-- HTML report generator: turns a ui_script manifest JSON into a
    portable single-file HTML page with the step tree, embedded
    evidence screenshots, and per-step verdict / timing. With no args
    it picks the newest manifest under `~/.cache/leantea-agent/runs/`. -/
lean_exe ui_report where
  srcDir := "examples"
  root := `UiReport.Run

/-! ## Sheet — functional spreadsheet (LeanJs formulas, TEA shell).

    Replaces the older Canvas example as the framework's worked
    full-stack app. Cell formulas are real `LeanJs` expressions
    evaluated by `LeanJs.Eval`. -/
lean_exe sheet_serve where
  srcDir := "examples"
  root := `Sheet.Serve

lean_exe gpu_serve where
  srcDir := "examples"
  root := `Gpu.Serve

lean_exe reversi_serve where
  srcDir := "examples"
  root := `Reversi.Serve

/-- Smoke / PoC for `LeanTea.Auth.Proof`. Mints sessions, runs the
    Sigma-wrapped dispatcher across multiple capabilities, exercises
    the dependent-type `Proof (.owner id)` form. See `SECURITY.md`
    §"Primitive 3" for the design rationale. -/
lean_exe auth_proof_smoke where
  srcDir := "examples"
  root := `Smoke.AuthProof

/-- Smoke / PoC for `LeanTea.Persist.SafeQuery`. Exercises typed SELECT
    (with eq / IN / LIKE / AND / NOT), UPDATE, DELETE, COUNT, and the
    `.trusted decl_name% "…"` audit escape hatch. See `SECURITY.md`
    §"Primitive 1" for the design rationale. -/
lean_exe safequery_smoke where
  srcDir := "examples"
  root := `Smoke.SafeQuery

/-- Smoke / PoC for `LeanTea.Html.SafeAttr`. Demonstrates that
    `javascript:` URL schemes, `on*` event-handler attribute names,
    and unsafe `style`-style attribute names are rejected at
    construction time. See `SECURITY.md` §"Primitive 2" for the
    design rationale. -/
lean_exe safehtml_smoke where
  srcDir := "examples"
  root := `Smoke.SafeHtml

/-! ## Private game targets

The following targets are intentionally absent from the public repo
because their content is third-party (various brand-licensed /
Armored Core derivatives, NSFW art) or carries quoted material from
arXiv that we keep out of the public mirror:

* `saber_serve`, `fight2d_serve`, `fight3d_serve`, `mech_serve`,
  `vn_chat_serve` — game shells under `examples/_private/`
* `english`, `english_serve` — English-learning trainer with arXiv
  quotes under `examples/_private/English/`

To enable locally, restore the dirs and append the matching `lean_exe`
entries to a local checkout — do **not** commit:

```
lean_exe english       where srcDir := "examples"; root := `English.Cli
lean_exe english_serve where srcDir := "examples"; root := `English.Serve
lean_exe saber_serve   where srcDir := "examples"; root := `Saber.Serve
lean_exe fight2d_serve where srcDir := "examples"; root := `Fight2d.Serve
lean_exe fight3d_serve where srcDir := "examples"; root := `Fight3d.Serve
lean_exe mech_serve    where srcDir := "examples"; root := `Mech.Serve
lean_exe vn_chat_serve where srcDir := "examples"; root := `VnChat.Serve
``` -/

/-- Site generator: walks docs/*.md, renders each to docs-site/*.html
    plus a single typed-CSS site.css. Used by `.github/workflows/pages.yml`
    to build GitHub Pages on every push to main. -/
lean_exe gen_site where
  srcDir := "examples"
  root := `Tools.GenSite
