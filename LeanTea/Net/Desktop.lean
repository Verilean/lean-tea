/-! # LeanTea.Net.Desktop — OS-level mouse + screenshot

Thin FFI shim over `c/leantea_desktop.c`. Targets the same job as
`LeanTea.Browser` but skips the browser-bridge layer: clicks become
real OS mouse events, screenshots capture the actual display.

Use this when:
* The thing under test is rendered on a canvas (Pixi, WebGL, native
  game UI) so DOM selectors are useless anyway.
* You want one tool that works for browser tabs AND for native apps
  in the same script.
* You don't need headless mode.

Build with `LEANTEA_DESKTOP=1` to link the real implementation
(macOS Quartz today); without it the calls return an IO error
explaining how to enable it. -/

namespace LeanTea.Net.Desktop

/-- Identifier of the compiled-in backend — `"macos-quartz"` when
    real support is linked, `"stub"` otherwise. Useful as a probe
    before running a script: bail out early if a CI machine has the
    stub. -/
@[extern "leantea_desktop_backend_name"]
opaque backendName : IO String

/-- Synthesize a left-click at the given desktop (not viewport)
    pixel. `x` and `y` are absolute screen coordinates. -/
@[extern "leantea_desktop_click_xy"]
opaque clickXy (x y : UInt32) : IO Unit

/-- Save a PNG of the main display to `path`. Captures everything
    on screen — windowed chrome, multiple apps, etc. — so it's a
    superset of what a browser-tab screenshot shows. -/
@[extern "leantea_desktop_screenshot"]
opaque screenshot (path : @& String) : IO Unit

/-- Press and release a single key by its platform-specific virtual
    keycode. On macOS the codes live in `<HIToolbox/Events.h>`
    (e.g. `kVK_Return = 36`, `kVK_Escape = 53`). -/
@[extern "leantea_desktop_key_press"]
opaque keyPress (keycode : UInt32) : IO Unit

/-- Main display dimensions in physical pixels. Useful as a sanity
    check for click coordinates. -/
@[extern "leantea_desktop_screen_size"]
opaque screenSize : IO (Nat × Nat)

/-- True when the FFI actually has a working implementation. -/
def isAvailable : IO Bool := do
  let b ← backendName
  return b != "stub"

end LeanTea.Net.Desktop
