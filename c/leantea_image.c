/* leantea_image.c — minimal raster image ops for LeanTea.Vision.

   PNG/JPEG decode and PNG encode via the vendored public-domain
   stb_image / stb_image_write headers; crop and bilinear resize are
   hand-rolled here. Pixels are always 8-bit RGBA, row-major, no
   padding, so a W×H image is exactly W*H*4 bytes.

   Decode returns a ByteArray with an 8-byte header (width, height as
   little-endian u32) followed by the pixels — it saves building a
   Lean structure from C; the Lean side splits it. */

#include <lean/lean.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define STB_IMAGE_IMPLEMENTATION
#define STBI_ONLY_PNG
#define STBI_ONLY_JPEG
#define STBI_NO_STDIO
#include "stb_image.h"

#define STB_IMAGE_WRITE_IMPLEMENTATION
#define STBI_WRITE_NO_STDIO
#include "stb_image_write.h"

static lean_obj_res io_err(const char *msg) {
  return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

static lean_object *mk_bytes(size_t n) {
  lean_object *arr = lean_alloc_sarray(1, n, n);
  return arr;
}

static void put_u32(uint8_t *p, uint32_t v) {
  p[0] = v & 0xff; p[1] = (v >> 8) & 0xff;
  p[2] = (v >> 16) & 0xff; p[3] = (v >> 24) & 0xff;
}

/* decode : @& ByteArray → IO ByteArray   (header ++ rgba) */
lean_obj_res leantea_image_decode(b_lean_obj_arg bytes, lean_obj_arg io) {
  (void)io;
  int w = 0, h = 0, n = 0;
  stbi_uc *px = stbi_load_from_memory(
      lean_sarray_cptr(bytes), (int)lean_sarray_size(bytes), &w, &h, &n, 4);
  if (!px) return io_err(stbi_failure_reason());
  size_t sz = (size_t)w * (size_t)h * 4;
  lean_object *out = mk_bytes(8 + sz);
  uint8_t *dst = lean_sarray_cptr(out);
  put_u32(dst, (uint32_t)w);
  put_u32(dst + 4, (uint32_t)h);
  memcpy(dst + 8, px, sz);
  stbi_image_free(px);
  return lean_io_result_mk_ok(out);
}

typedef struct { uint8_t *buf; size_t len, cap; } membuf;

static void membuf_write(void *ctx, void *data, int size) {
  membuf *m = (membuf *)ctx;
  if (m->len + (size_t)size > m->cap) {
    size_t nc = m->cap ? m->cap * 2 : 65536;
    while (nc < m->len + (size_t)size) nc *= 2;
    uint8_t *nb = realloc(m->buf, nc);
    if (!nb) return;
    m->buf = nb; m->cap = nc;
  }
  memcpy(m->buf + m->len, data, (size_t)size);
  m->len += (size_t)size;
}

/* encodePng : UInt32 → UInt32 → @& ByteArray → IO ByteArray */
lean_obj_res leantea_image_encode_png(uint32_t w, uint32_t h,
                                      b_lean_obj_arg rgba, lean_obj_arg io) {
  (void)io;
  if (lean_sarray_size(rgba) < (size_t)w * h * 4)
    return io_err("encodePng: pixel buffer smaller than w*h*4");
  membuf m = {0};
  int ok = stbi_write_png_to_func(membuf_write, &m, (int)w, (int)h, 4,
                                  lean_sarray_cptr(rgba), (int)w * 4);
  if (!ok || !m.buf) { free(m.buf); return io_err("encodePng failed"); }
  lean_object *out = mk_bytes(m.len);
  memcpy(lean_sarray_cptr(out), m.buf, m.len);
  free(m.buf);
  return lean_io_result_mk_ok(out);
}

/* crop : UInt32 → UInt32 → @& ByteArray → UInt32 → UInt32 → UInt32 → UInt32 → ByteArray
   The caller clamps the rectangle; out-of-range pixels read as
   transparent black to stay memory-safe regardless. */
lean_obj_res leantea_image_crop(uint32_t w, uint32_t h, b_lean_obj_arg rgba,
                                uint32_t x, uint32_t y, uint32_t cw, uint32_t ch) {
  lean_object *out = mk_bytes((size_t)cw * ch * 4);
  uint8_t *dst = lean_sarray_cptr(out);
  const uint8_t *src = lean_sarray_cptr(rgba);
  for (uint32_t j = 0; j < ch; j++) {
    for (uint32_t i = 0; i < cw; i++) {
      uint32_t sx = x + i, sy = y + j;
      uint8_t *d = dst + ((size_t)j * cw + i) * 4;
      if (sx < w && sy < h) memcpy(d, src + ((size_t)sy * w + sx) * 4, 4);
      else memset(d, 0, 4);
    }
  }
  return out;
}

/* resize : UInt32 → UInt32 → @& ByteArray → UInt32 → UInt32 → ByteArray
   Bilinear, pixel-center aligned. Good enough for zoom-in crops. */
lean_obj_res leantea_image_resize(uint32_t w, uint32_t h, b_lean_obj_arg rgba,
                                  uint32_t nw, uint32_t nh) {
  lean_object *out = mk_bytes((size_t)nw * nh * 4);
  uint8_t *dst = lean_sarray_cptr(out);
  const uint8_t *src = lean_sarray_cptr(rgba);
  if (w == 0 || h == 0) { memset(dst, 0, (size_t)nw * nh * 4); return out; }
  for (uint32_t j = 0; j < nh; j++) {
    float fy = ((float)j + 0.5f) * (float)h / (float)nh - 0.5f;
    if (fy < 0) fy = 0;
    uint32_t y0 = (uint32_t)fy, y1 = y0 + 1 < h ? y0 + 1 : y0;
    float ty = fy - (float)y0;
    for (uint32_t i = 0; i < nw; i++) {
      float fx = ((float)i + 0.5f) * (float)w / (float)nw - 0.5f;
      if (fx < 0) fx = 0;
      uint32_t x0 = (uint32_t)fx, x1 = x0 + 1 < w ? x0 + 1 : x0;
      float tx = fx - (float)x0;
      const uint8_t *a = src + ((size_t)y0 * w + x0) * 4;
      const uint8_t *b = src + ((size_t)y0 * w + x1) * 4;
      const uint8_t *c = src + ((size_t)y1 * w + x0) * 4;
      const uint8_t *d = src + ((size_t)y1 * w + x1) * 4;
      uint8_t *o = dst + ((size_t)j * nw + i) * 4;
      for (int k = 0; k < 4; k++) {
        float top = a[k] + (b[k] - a[k]) * tx;
        float bot = c[k] + (d[k] - c[k]) * tx;
        o[k] = (uint8_t)(top + (bot - top) * ty + 0.5f);
      }
    }
  }
  return out;
}
