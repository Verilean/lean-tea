/* leantea_crypto.c — OpenSSL libcrypto wrapper for LeanTea.

   Same shape as leantea_mysql.c: always compiles, switches between a
   real OpenSSL backend and a stub via `-DLEANTEA_HAVE_CRYPTO`. The
   Lean side calls these via `@[extern "leantea_crypto_*"]`.

   Build modes
   -----------
     Stub  (default)         every call returns an IO error.
     Real  LEANTEA_CRYPTO=1  Lake adds `-DLEANTEA_HAVE_CRYPTO` to the
                             object compile; supply `-lcrypto` to the
                             exe link line via NIX_LDFLAGS or by
                             editing lakefile.lean's exe `weakLinkArgs`.

   API
   ---
     leantea_crypto_sha256(data : ByteArray) : IO ByteArray  -- 32B
     leantea_crypto_hmac_sha256(key, msg)   : IO ByteArray   -- 32B
     leantea_crypto_pbkdf2_sha256(pw, salt, iterations, keyLen)
                                            : IO ByteArray
     leantea_crypto_rsa_verify_sha256(pem, data, sig) : IO UInt8 (1=ok)
     leantea_crypto_ecdsa_p256_verify_sha256(pem, data, sig)
                                            : IO UInt8 (1=ok)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lean/lean.h>

#ifdef LEANTEA_HAVE_CRYPTO
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#endif

/* ---------- error helpers ---------- */

static lean_object *err_str(const char *msg) {
  return lean_mk_io_user_error(lean_mk_string(msg));
}

#ifndef LEANTEA_HAVE_CRYPTO
static lean_obj_res not_built(void) {
  return lean_io_result_mk_error(err_str(
    "Crypto FFI not compiled in — rebuild with LEANTEA_CRYPTO=1 "
    "and OpenSSL (libcrypto) installed"));
}
#endif

/* ---------- ByteArray construction ---------- */

static lean_obj_res mk_bytes(const unsigned char *src, size_t n) {
  lean_obj_res arr = lean_alloc_sarray(1, n, n);
  unsigned char *dst = lean_sarray_cptr(arr);
  if (n) memcpy(dst, src, n);
  return arr;
}

/* ---------- SHA-256 ---------- */

LEAN_EXPORT lean_obj_res
leantea_crypto_sha256(b_lean_obj_arg data, lean_object *io) {
  (void)io;
#ifdef LEANTEA_HAVE_CRYPTO
  size_t in_len = lean_sarray_size(data);
  const unsigned char *in_ptr = lean_sarray_cptr(data);
  unsigned char digest[32];
  unsigned int dlen = 0;
  EVP_MD_CTX *ctx = EVP_MD_CTX_new();
  if (!ctx) return lean_io_result_mk_error(err_str("EVP_MD_CTX_new failed"));
  int ok =
       EVP_DigestInit_ex(ctx, EVP_sha256(), NULL)
    && EVP_DigestUpdate(ctx, in_ptr, in_len)
    && EVP_DigestFinal_ex(ctx, digest, &dlen);
  EVP_MD_CTX_free(ctx);
  if (!ok) return lean_io_result_mk_error(err_str("EVP_DigestFinal_ex failed"));
  return lean_io_result_mk_ok(mk_bytes(digest, dlen));
#else
  (void)data;
  return not_built();
#endif
}

/* ---------- HMAC-SHA256 ---------- */

LEAN_EXPORT lean_obj_res
leantea_crypto_hmac_sha256(b_lean_obj_arg key, b_lean_obj_arg msg,
                           lean_object *io) {
  (void)io;
#ifdef LEANTEA_HAVE_CRYPTO
  size_t klen = lean_sarray_size(key);
  size_t mlen = lean_sarray_size(msg);
  const unsigned char *k = lean_sarray_cptr(key);
  const unsigned char *m = lean_sarray_cptr(msg);
  unsigned char out[32];
  unsigned int olen = 0;
  unsigned char *r = HMAC(EVP_sha256(), k, (int)klen, m, mlen, out, &olen);
  if (!r) return lean_io_result_mk_error(err_str("HMAC failed"));
  return lean_io_result_mk_ok(mk_bytes(out, olen));
#else
  (void)key; (void)msg;
  return not_built();
#endif
}

/* ---------- PBKDF2-HMAC-SHA256 ---------- */

LEAN_EXPORT lean_obj_res
leantea_crypto_pbkdf2_sha256(b_lean_obj_arg pw, b_lean_obj_arg salt,
                             uint32_t iterations, size_t key_len,
                             lean_object *io) {
  (void)io;
#ifdef LEANTEA_HAVE_CRYPTO
  size_t plen = lean_sarray_size(pw);
  size_t slen = lean_sarray_size(salt);
  const unsigned char *p = lean_sarray_cptr(pw);
  const unsigned char *s = lean_sarray_cptr(salt);
  unsigned char *buf = (unsigned char *)malloc(key_len ? key_len : 1);
  if (!buf) return lean_io_result_mk_error(err_str("OOM in PBKDF2"));
  int ok = PKCS5_PBKDF2_HMAC((const char *)p, (int)plen, s, (int)slen,
                             (int)iterations, EVP_sha256(),
                             (int)key_len, buf);
  if (!ok) {
    free(buf);
    return lean_io_result_mk_error(err_str("PKCS5_PBKDF2_HMAC failed"));
  }
  lean_obj_res arr = mk_bytes(buf, key_len);
  free(buf);
  return lean_io_result_mk_ok(arr);
#else
  (void)pw; (void)salt; (void)iterations; (void)key_len;
  return not_built();
#endif
}

/* ---------- RSA / ECDSA signature verification (PEM public key) ---------- */

#ifdef LEANTEA_HAVE_CRYPTO
static int verify_with_pem(const char *pem, const unsigned char *data,
                           size_t data_len, const unsigned char *sig,
                           size_t sig_len, int want_kind /* EVP_PKEY_RSA / EC */) {
  BIO *bio = BIO_new_mem_buf(pem, -1);
  if (!bio) return -1;
  EVP_PKEY *pkey = PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL);
  BIO_free(bio);
  if (!pkey) return -1;
  if (want_kind && EVP_PKEY_base_id(pkey) != want_kind) {
    EVP_PKEY_free(pkey);
    return -2;
  }
  EVP_MD_CTX *ctx = EVP_MD_CTX_new();
  if (!ctx) { EVP_PKEY_free(pkey); return -1; }
  int rc = 0;
  if (EVP_DigestVerifyInit(ctx, NULL, EVP_sha256(), NULL, pkey) == 1
      && EVP_DigestVerifyUpdate(ctx, data, data_len) == 1) {
    rc = EVP_DigestVerifyFinal(ctx, sig, sig_len);
  }
  EVP_MD_CTX_free(ctx);
  EVP_PKEY_free(pkey);
  return rc;        /* 1 = verified, 0 = mismatch, <0 = error */
}
#endif

LEAN_EXPORT lean_obj_res
leantea_crypto_rsa_verify_sha256(b_lean_obj_arg pem,
                                 b_lean_obj_arg data,
                                 b_lean_obj_arg sig,
                                 lean_object *io) {
  (void)io;
#ifdef LEANTEA_HAVE_CRYPTO
  const char *pem_str = lean_string_cstr(pem);
  const unsigned char *d = lean_sarray_cptr(data);
  size_t dlen = lean_sarray_size(data);
  const unsigned char *s = lean_sarray_cptr(sig);
  size_t slen = lean_sarray_size(sig);
  int rc = verify_with_pem(pem_str, d, dlen, s, slen, EVP_PKEY_RSA);
  if (rc < 0)
    return lean_io_result_mk_error(err_str("RSA verify: bad key or library error"));
  return lean_io_result_mk_ok(lean_box((unsigned)(rc == 1 ? 1 : 0)));
#else
  (void)pem; (void)data; (void)sig;
  return not_built();
#endif
}

LEAN_EXPORT lean_obj_res
leantea_crypto_ecdsa_p256_verify_sha256(b_lean_obj_arg pem,
                                        b_lean_obj_arg data,
                                        b_lean_obj_arg sig,
                                        lean_object *io) {
  (void)io;
#ifdef LEANTEA_HAVE_CRYPTO
  const char *pem_str = lean_string_cstr(pem);
  const unsigned char *d = lean_sarray_cptr(data);
  size_t dlen = lean_sarray_size(data);
  const unsigned char *s = lean_sarray_cptr(sig);
  size_t slen = lean_sarray_size(sig);
  int rc = verify_with_pem(pem_str, d, dlen, s, slen, EVP_PKEY_EC);
  if (rc < 0)
    return lean_io_result_mk_error(err_str("ECDSA verify: bad key or library error"));
  return lean_io_result_mk_ok(lean_box((unsigned)(rc == 1 ? 1 : 0)));
#else
  (void)pem; (void)data; (void)sig;
  return not_built();
#endif
}

/* ---------- Build-flag indicator ----------
   Lean callers can `leantea_crypto_available()` to fall back to pure
   Lean / shell-out when the FFI isn't compiled in. */
LEAN_EXPORT uint8_t leantea_crypto_available(void) {
#ifdef LEANTEA_HAVE_CRYPTO
  return 1;
#else
  return 0;
#endif
}
