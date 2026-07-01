import Lake
open Lake DSL System

/-- Per-platform libpq link argument for `postgres_smoke`. Listed
    as an exact `.so` / `.dylib` path so we don't have to add a
    broad `-L<dir>` (which on Ubuntu 24+ would shadow Lean's
    bundled `libc` with system glibc 2.39, breaking resolution of
    `__libc_csu_init` / `_fini` against Lean's `Scrt1.o`).

    The path is selected at lakefile-load time via
    `System.Platform.isOSX`. On a platform whose canonical libpq
    location isn't installed the postgres_smoke link fails clearly
    at link time (`PQstatus undefined`) — and only fails for
    callers that explicitly `lake build postgres_smoke`, because
    it isn't a default target. -/
def libpqLinkArgs : Array String :=
  if System.Platform.isOSX then
    /- Homebrew arm64 is the default on modern Macs; Intel
       Homebrew lives at /usr/local. Both ship as a keg-only
       formula (libpq.dylib). -/
    #["/opt/homebrew/opt/libpq/lib/libpq.dylib"]
  else
    /- Ubuntu / Debian. After `apt install libpq-dev` the SONAME
       link is at the canonical Debian multiarch path. -/
    #["/usr/lib/x86_64-linux-gnu/libpq.so.5"]

package «lean-tea» where
  -- Pre-compile module C output so `lean_exe` targets can link the
  -- SQLite FFI without a separate `lean -c` pass at link time.
  precompileModules := false

lean_lib LeanTea where
  roots := #[`LeanTea, `LeanTea.Tui]

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
    `Tests.PureSpec,
    `Tests.AuthSpec,
    `Tests.TuiSpec,
    `AuthIdp.Serve,
    `StateMachine.Order,
    `ChuHan.Game,
    `MetaOrchestrator.Zellij,
    `MetaOrchestrator.Director,
    `MetaOrchestrator.Config,
    `MetaOrchestrator.Runtime,
    `MetaOrchestrator.Tui,
    `MetaOrchestrator.Main
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

/-! ## PostgreSQL FFI — conditional on `LEANTEA_POSTGRES=1`

Same opt-in shape as MySQL. The wrapper always compiles so links
don't fail; `-DLEANTEA_HAVE_POSTGRES` toggles between the real
driver and stub-mode. When enabled we shell out to `pg_config` for
include + link flags. -/

target leantea_postgres_o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "leantea_postgres.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "leantea_postgres.c"
  let enabled := (← IO.getEnv "LEANTEA_POSTGRES").map (·.toLower == "1")
                  |>.getD false
  let mut weakArgs : Array String := #[
    "-I", (← getLeanIncludeDir).toString
  ]
  let mut traceArgs : Array String := #["-fPIC", "-O2"]
  if enabled then
    let cf ← (IO.Process.output {
      cmd := "pg_config", args := #["--includedir"] }
        ).catchExceptions (fun _ => pure { exitCode := 1, stdout := "", stderr := "" })
    if cf.exitCode == 0 then
      let inc : String := cf.stdout.trimAscii.toString
      if !inc.isEmpty then
        weakArgs := weakArgs ++ #["-I", inc]
    traceArgs := traceArgs.push "-DLEANTEA_HAVE_POSTGRES"
  buildO oFile srcJob weakArgs traceArgs "cc"

extern_lib libleantea_postgres pkg := do
  let name := nameToStaticLib "leantea_postgres"
  let wrapperO ← leantea_postgres_o.fetch
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

/-- Tetris — TUI demo with raw-mode stdin + concurrent tick
    gravity. ~400 LOC, all in `examples/Tetris/Main.lean`.
    Standard 10×20 board, 7 tetrominoes, line-clear scoring,
    level-based gravity speed.

    ```
    lake build tetris
    ./.lake/build/bin/tetris
    ```

    Unix only — uses `stty(1)` for the raw-mode handshake. If you
    Ctrl-C out and the terminal stays raw, `reset` restores it. -/
lean_exe tetris where
  srcDir := "examples"
  root := `Tetris.Main

lean_exe counter_web where
  srcDir := "examples"
  root := `CounterWeb.Main

/-! ## Subsystem smokes

The Persist-related smokes (sqlite / query / migrate / auth_proof /
safequery) are consolidated into `persist_spec` — see further below.
The construction-time security smokes (safehtml / safepath / safecmd /
safeheader / saferedirect) are consolidated into `security_spec`. -/

/-- WebSpec end-to-end smoke against `counter_web`. Skipped in CI
    because it needs a live Chrome (`--remote-debugging-port=9222`)
    and a live LeanTEA app. See `examples/Smoke/WebSpec.lean` for
    the bootstrap. -/
lean_exe webspec_smoke where
  srcDir := "examples"
  root := `Smoke.WebSpec

/-- WebDAV server exposing a directory mountable by Finder /
    GNOME Files / Windows Explorer. Uses `LeanTea.Net.WebDav.handler`. -/
lean_exe webdav_serve where
  srcDir := "examples"
  root := `WebDav.Serve

/-- WebDAV round-trip smoke. Spawns webdav_serve as a subprocess
    against a tempdir, runs the 11-assertion LSpec via HttpClient,
    tears down. No external services — pure-Lean network test. -/
lean_exe webdav_smoke where
  srcDir := "examples"
  root := `Smoke.WebDav

lean_exe http_smoke where
  srcDir := "examples"
  root := `Smoke.Http

/-- S3 / object-storage round-trip. Opt-in: needs an S3-compatible
    endpoint at `S3_ENDPOINT` (or `http://127.0.0.1:9000` by default
    — MinIO). The CI workflow spins up MinIO as a service and runs
    this binary. -/
lean_exe s3_smoke where
  srcDir := "examples"
  root := `Smoke.S3

lean_exe http_client_smoke where
  srcDir := "examples"
  root := `Smoke.HttpClient

lean_exe backend_smoke where
  srcDir := "examples"
  root := `Smoke.Backend

lean_exe memcached_smoke where
  srcDir := "examples"
  root := `Smoke.Memcached

/-- Valkey / Redis round-trip. CI services block spins up
    `valkey/valkey:latest` at `127.0.0.1:6379` and runs this. -/
lean_exe valkey_smoke where
  srcDir := "examples"
  root := `Smoke.Valkey

lean_exe mysql_smoke where
  srcDir := "examples"
  root := `Smoke.Mysql

/-- PostgreSQL round-trip smoke. `weakLinkArgs` always tries to
    link `-lpq` with a few well-known search paths (Ubuntu libpq-dev,
    Homebrew libpq, Nix). On machines without libpq the **link**
    fails, but only when this exe is explicitly requested (it's not
    a default target — bare `lake build` doesn't build it).

    Stub mode: drop `LEANTEA_POSTGRES=1` from the env. The C wrapper
    compiles to a stub that returns `"PostgreSQL support not
    compiled in"` from every call. -/
lean_exe postgres_smoke where
  srcDir := "examples"
  root := `Smoke.Postgres
  weakLinkArgs := libpqLinkArgs

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

/-- Pure unit tests for the `LeanTea.Tui` widget kit — layout
    primitives, combinators, elements, and the `Session` test
    harness that mirrors `App.run` without a real TTY. -/
lean_exe tui_spec where
  srcDir := "examples"
  root := `Tests.TuiSpec

/-- Aggregated LSpec runner for the construction-time security
    primitives (SafeHtml + SafePath + SafeCmd + SafeHeader +
    SafeRedirect). One binary, one CI step, ~60 LSpec assertions. -/
lean_exe security_spec where
  srcDir := "examples"
  root := `Tests.SecuritySpec

/-- Aggregated LSpec runner for the SQLite-backed integration tests
    (Store roundtrips + Persist.Query DSL + Migration runner +
    Auth.Proof dispatch + SafeQuery typed SQL). One binary, one CI
    step, ~32 LSpec assertions. Each group uses its own temp DB. -/
lean_exe persist_spec where
  srcDir := "examples"
  root := `Tests.PersistSpec

/-- Standalone OAuth 2.0 IdP server used by AuthSpec as a subprocess.
    Same-process `IO.asTask` deadlocks because the IdP and the SP
    share one libuv loop; running the IdP in its own process gets
    them their own schedulers. -/
lean_exe auth_idp_serve where
  srcDir := "examples"
  root := `AuthIdp.Serve

/-- Auth integration tests. Spawns `auth_idp_serve` on
    `AUTH_TEST_PORT` (default 18765), polls until /authorize answers,
    runs the full SP-side round-trip (beginAuth → /authorize →
    exchangeCode → fetchUserInfo), plus SAML fixture parses, and
    teardown-kills the IdP child on exit. Needs `curl(1)` for the
    SP-side OAuth2 token exchange. -/
lean_exe auth_spec where
  srcDir := "examples"
  root := `Tests.AuthSpec

/-- Aggregated LSpec runner for the pure-Lean subsystems
    (Template engine + Crypto known-answer + JWT + SAML + native
    libcrypto FFI parity + Auth.Security helpers). One binary, one
    CI step, ~30 LSpec assertions.

    No `weakLinkArgs := ["-lcrypto"]` here — the default build runs
    the C wrapper in stub mode (no `LEANTEA_HAVE_CRYPTO`), so the
    binary doesn't reference any libcrypto symbols. Locally, build
    with `LEANTEA_CRYPTO=1 NIX_LDFLAGS="… -lcrypto"` to exercise
    the FFI parity assertions. -/
lean_exe pure_spec where
  srcDir := "examples"
  root := `Tests.PureSpec

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

/-- Gemini API wire-up smoke. Skips quietly when `GEMINI_API_KEY` is
    unset (CI default). Otherwise runs one `ask` against
    `gemini-2.5-flash-lite` (cheapest) and one `reviewMany` over two
    repo files. Override the model via `GEMINI_SMOKE_MODEL`. -/
lean_exe gemini_smoke where
  srcDir := "examples"
  root := `Smoke.Gemini

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

/-- MCP server driving `tmux(1)` for AI-orchestrated terminal multiplexing.
    Tools: list/new/kill sessions+windows+panes, send-keys, capture-pane,
    plus a `tmux_run` convenience for one-shot commands. Same stdio +
    HTTP shape as `desktop_mcp_serve`. Optional `--workspace DIR` /
    `TMUX_MCP_WORKSPACE` for diagnostic cwd hints. -/
lean_exe tmux_mcp_serve where
  srcDir := "examples"
  root := `TmuxMcp.Serve

/-- MCP server giving code-editing agents the standard file-system +
    shell toolkit, every operation workspace-bound via
    `LeanTea.Net.SafePath`. Seven tools: `coder_read_file`,
    `coder_list_dir`, `coder_glob`, `coder_grep` (read-only — safe
    to allow globally) and `coder_write_file`, `coder_edit_file`,
    `coder_run` (mutating — best left to the policy `ask` gate).
    Pair with `llm_chat_web` + `LeanTea.Llm.Policy` for a
    Claude-Code-style approve-before-write flow. -/
lean_exe coder_mcp_serve where
  srcDir := "examples"
  root := `CoderMcp.Serve

/-- Visual control / telemetry for a `LeanTea.Agent.Conductor`. Boots
    the MCP orchestrator + a conductor loop in `IO.asTask`, exposes
    `live` / `playbooks` / `rewards` tabs with bandit stats + pause /
    resume / abort controls. Pair with `browser_mcp_serve` + a tiny
    JSON-based playbook collection to play a browser game. -/
lean_exe agent_dashboard_serve where
  srcDir := "examples"
  root := `AgentDashboard.Serve

/-- MCP server fronting the Google Gemini API. Five tools:
    `gemini_ask`, `gemini_chat`, `gemini_review_files` (long-context
    multi-file holistic review — exploits Pro's 2M-token window),
    `gemini_review_diff` (git-diff focused review), `gemini_list_models`.
    Default model `gemini-2.5-pro`, override per-call. The API key
    is read from `GEMINI_API_KEY` (see ai.google.dev for issuance).
    `--workspace DIR` scopes the file-reading tools through
    `LeanTea.Net.SafePath` so a buggy client can't read outside it. -/
lean_exe gemini_mcp_serve where
  srcDir := "examples"
  root := `GeminiMcp.Serve

/-! ## LLM chat demos — three UI shells over `LeanTea.Llm.McpOrchestrator`

All three share the same `--config FILE.json` shape and the same
orchestrator core. Use whichever fits your context:

  * `llm_chat_cli` — stdin/stdout REPL, ANSI colours, scripts cleanly.
  * `llm_chat_tui` — full-screen styled chat with ANSI repaint.
  * `llm_chat_web` — single-page browser UI; talk from any device. -/

lean_exe llm_chat_cli where
  srcDir := "examples"
  root := `LlmChatCli.Main

lean_exe llm_chat_tui where
  srcDir := "examples"
  root := `LlmChatTui.Main

lean_exe llm_chat_web where
  srcDir := "examples"
  root := `LlmChatWeb.Serve

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

/-- 楚漢恋歌 (Chu-Han Love Song) — 2D action / RPG / strategy VN set in
    BCE 209-195 China. Six playable protagonists (Liu Bang, Xiang Yu,
    Han Xin, Zhang Liang, Xiao He, Fan Zeng), each with their wife
    arc + historical fate. Two-layer dialogue (outer speech + inner
    monologue) drives the "charmer / egoist" double face of Liu Bang
    and the corresponding interior voices of the others. LeanJs +
    DOM + Canvas 2D. -/
lean_exe chuhan_serve where
  srcDir := "examples"
  root := `ChuHan.Serve

/-- Gemini-driven PM agent that watches a Claude-Code zellij pane,
    decides when the agent has stalled, and either issues the next
    instruction or escalates to the user. Goal text + log path + poll
    interval are CLI args. See `examples/MetaOrchestrator/Main.lean`
    for the loop, and `Zellij.lean` / `Director.lean` for the two
    layers it stands on. -/
lean_exe meta_orchestrator where
  srcDir := "examples"
  root := `MetaOrchestrator.Main

/-! ## Construction-time security primitives

The five primitives below ship a single aggregated LSpec runner
(`security_spec` defined above) instead of one binary each:

  * SafeHtml     — `javascript:` / `on*` attribute reject
  * SafePath     — workspace-relative + `..` / NUL / sibling-prefix
  * SafeCmd      — `args : List String` + shell-name reject
  * SafeHeader   — Response.setHeader CRLF + defaultSecurityHeaders
  * SafeRedirect — origin allow-list + scheme reject

Source: `examples/Tests/SecuritySpec.lean`. The heavier integration
smokes (`auth_proof_smoke`, `safequery_smoke`) still have their own
binaries because they need a real SQLite. -/

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
