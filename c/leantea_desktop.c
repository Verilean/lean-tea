/* leantea_desktop.c — OS-level mouse + screenshot wrapper for LeanTea.

   When linked without `-DLEANTEA_HAVE_DESKTOP` every call returns an
   IO error ("desktop support not compiled in") so the rest of the
   build keeps working — same pattern as `leantea_mysql.c`.

   The real implementation uses platform-native APIs only, no
   cppautogui submodule required for the basic flow (click +
   screenshot + key-press):

   * macOS: Quartz (CoreGraphics) — CGEventPost for mouse + key
     events, CGDisplayCreateImage + ImageIO for PNG screenshots.

   * Linux X11: TODO — needs XTestFakeButtonEvent and XGetImage
     wrappers. For now the stub trips on this platform until the
     LEANTEA_DESKTOP_BACKEND env / build flag picks an X11 path.

   The wrapper is intentionally tiny: click(x,y), screenshot(path),
   keyPress(keycode). Anything fancier (drag, scroll, type-string)
   composes from these primitives in the Lean layer. */

#include <lean/lean.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#if defined(LEANTEA_HAVE_DESKTOP) && defined(__APPLE__)
#include <ApplicationServices/ApplicationServices.h>
#include <ImageIO/ImageIO.h>
#include <CoreServices/CoreServices.h>
#define LEANTEA_DESKTOP_BACKEND "macos-quartz"
#define LEANTEA_DESKTOP_BUILT 1
#endif

/* ---------- error helpers ---------- */

static lean_object *err_str(const char *msg) {
  return lean_mk_io_user_error(lean_mk_string(msg));
}

#ifndef LEANTEA_DESKTOP_BUILT
static lean_obj_res not_built(void) {
  return lean_io_result_mk_error(err_str(
    "desktop support not compiled in — rebuild with "
    "LEANTEA_DESKTOP=1 (macOS only for now)"));
}
#endif

/* ---------- backend name ---------- */

lean_obj_res leantea_desktop_backend_name(lean_obj_arg io) {
  (void)io;
#ifdef LEANTEA_DESKTOP_BACKEND
  return lean_io_result_mk_ok(lean_mk_string(LEANTEA_DESKTOP_BACKEND));
#else
  return lean_io_result_mk_ok(lean_mk_string("stub"));
#endif
}

/* ---------- click ---------- */

lean_obj_res leantea_desktop_click_xy(uint32_t x, uint32_t y, lean_obj_arg io) {
  (void)io;
#ifdef LEANTEA_DESKTOP_BUILT
#ifdef __APPLE__
  CGPoint p = CGPointMake((double)x, (double)y);
  /* A complete left-click is mousedown + mouseup at the same point.
     CGEventPost with kCGHIDEventTap puts the event at the HID layer,
     which is what most apps listen to (Pixi/canvas included). */
  CGEventRef down = CGEventCreateMouseEvent(
    NULL, kCGEventLeftMouseDown, p, kCGMouseButtonLeft);
  CGEventRef up = CGEventCreateMouseEvent(
    NULL, kCGEventLeftMouseUp, p, kCGMouseButtonLeft);
  if (!down || !up) {
    if (down) CFRelease(down);
    if (up) CFRelease(up);
    return lean_io_result_mk_error(err_str("CGEventCreateMouseEvent failed"));
  }
  CGEventPost(kCGHIDEventTap, down);
  CGEventPost(kCGHIDEventTap, up);
  CFRelease(down);
  CFRelease(up);
  return lean_io_result_mk_ok(lean_box(0));
#endif
#else
  (void)x; (void)y;
  return not_built();
#endif
}

/* ---------- screenshot ---------- */

lean_obj_res leantea_desktop_screenshot(lean_obj_arg path_obj, lean_obj_arg io) {
  (void)io;
#ifdef LEANTEA_DESKTOP_BUILT
#ifdef __APPLE__
  const char *path = lean_string_cstr(path_obj);
  CGImageRef img = CGDisplayCreateImage(CGMainDisplayID());
  if (!img) {
    return lean_io_result_mk_error(err_str("CGDisplayCreateImage failed"));
  }
  CFURLRef url = CFURLCreateFromFileSystemRepresentation(
    NULL, (const UInt8 *)path, (CFIndex)strlen(path), false);
  if (!url) {
    CGImageRelease(img);
    return lean_io_result_mk_error(err_str("CFURL create failed"));
  }
  /* kUTTypePNG is in CoreServices; the linker pulls it in via
     `-framework CoreServices`. */
  CGImageDestinationRef dst = CGImageDestinationCreateWithURL(
    url, kUTTypePNG, 1, NULL);
  if (!dst) {
    CFRelease(url);
    CGImageRelease(img);
    return lean_io_result_mk_error(err_str(
      "CGImageDestinationCreateWithURL failed"));
  }
  CGImageDestinationAddImage(dst, img, NULL);
  bool ok = CGImageDestinationFinalize(dst);
  CFRelease(dst);
  CFRelease(url);
  CGImageRelease(img);
  if (!ok) {
    return lean_io_result_mk_error(err_str("PNG write failed"));
  }
  return lean_io_result_mk_ok(lean_box(0));
#endif
#else
  (void)path_obj;
  return not_built();
#endif
}

/* ---------- key press (virtual keycode, e.g. macOS keyCodes) ---------- */

lean_obj_res leantea_desktop_key_press(uint32_t keycode, lean_obj_arg io) {
  (void)io;
#ifdef LEANTEA_DESKTOP_BUILT
#ifdef __APPLE__
  CGEventRef down = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)keycode, true);
  CGEventRef up   = CGEventCreateKeyboardEvent(NULL, (CGKeyCode)keycode, false);
  if (!down || !up) {
    if (down) CFRelease(down);
    if (up) CFRelease(up);
    return lean_io_result_mk_error(err_str("CGEventCreateKeyboardEvent failed"));
  }
  CGEventPost(kCGHIDEventTap, down);
  CGEventPost(kCGHIDEventTap, up);
  CFRelease(down);
  CFRelease(up);
  return lean_io_result_mk_ok(lean_box(0));
#endif
#else
  (void)keycode;
  return not_built();
#endif
}

/* ---------- screen size ---------- */

lean_obj_res leantea_desktop_screen_size(lean_obj_arg io) {
  (void)io;
#ifdef LEANTEA_DESKTOP_BUILT
#ifdef __APPLE__
  CGDirectDisplayID d = CGMainDisplayID();
  size_t w = CGDisplayPixelsWide(d);
  size_t h = CGDisplayPixelsHigh(d);
  /* Return as a (Nat × Nat) pair. -/ */
  lean_object *pair = lean_alloc_ctor(0, 2, 0);
  lean_ctor_set(pair, 0, lean_usize_to_nat(w));
  lean_ctor_set(pair, 1, lean_usize_to_nat(h));
  return lean_io_result_mk_ok(pair);
#endif
#else
  return not_built();
#endif
}
