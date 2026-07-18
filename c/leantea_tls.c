/* leantea_tls.c — HTTPS/TLS client transport via OpenSSL (libssl).
 *
 * The pure-Lean LeanTea.Net.HttpClient handles plaintext HTTP over
 * Std.Async.TCP; this shim adds the TLS transport so LeanTea.Net.TlsClient
 * can speak HTTPS without shelling out to curl. It's deliberately a thin
 * transport (connect / send / recv / close) — the HTTP/1.1 request build
 * and response parse stay in Lean and are shared with the plaintext client.
 *
 * API exposed to Lean (LeanTea.Net.TlsClient):
 *   leantea_tls_connect(host : String, port : UInt16) : IO Conn
 *   leantea_tls_send(conn, ByteArray)                 : IO Unit    -- SSL_write all
 *   leantea_tls_recv(conn, max : UInt32)              : IO ByteArray -- one SSL_read; empty = EOF
 *   leantea_tls_close(conn)                           : IO Unit
 * Conn is an opaque `lean_external_class` (a tls_conn* with a finalizer),
 * exactly like the sqlite/postgres handles.
 *
 * Opt-in via LEANTEA_TLS=1 in the lakefile (compiles the real backend with
 * -DLEANTEA_HAVE_TLS and needs -lssl -lcrypto at link time). Without the
 * flag the four entry points compile to stubs that raise an IO error on
 * first use, so the default portable build stays green.
 *
 * Certificate verification is ON by default (peer chain via OpenSSL's
 * default trust store + hostname check via SSL_set1_host). Set the env var
 * LEANTEA_TLS_INSECURE=1 to skip it (debugging self-signed endpoints).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <lean/lean.h>

#ifdef LEANTEA_HAVE_TLS

#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netdb.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>

typedef struct { SSL_CTX *ctx; SSL *ssl; int fd; } tls_conn;

/* ---------- external class for tls_conn* ---------- */

static void tls_finalize(void *p) {
  tls_conn *c = (tls_conn *)p;
  if (!c) return;
  if (c->ssl) { SSL_shutdown(c->ssl); SSL_free(c->ssl); }
  if (c->fd >= 0) close(c->fd);
  if (c->ctx) SSL_CTX_free(c->ctx);
  free(c);
}
static void tls_foreach(void *p, b_lean_obj_arg f) { (void)p; (void)f; }

static lean_external_class *g_tls_class = NULL;
static lean_external_class *get_tls_class(void) {
  if (!g_tls_class)
    g_tls_class = lean_register_external_class(tls_finalize, tls_foreach);
  return g_tls_class;
}

/* ---------- error helpers ---------- */

static lean_object *tls_err(const char *msg) {
  return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}
static lean_object *tls_err2(const char *prefix, const char *m) {
  char buf[512];
  snprintf(buf, sizeof buf, "%s: %s", prefix, m ? m : "(unknown)");
  return tls_err(buf);
}
static lean_object *ssl_errstr(const char *prefix) {
  unsigned long e = ERR_get_error();
  char eb[256];
  if (e) ERR_error_string_n(e, eb, sizeof eb);
  else   snprintf(eb, sizeof eb, "(no OpenSSL error queued)");
  return tls_err2(prefix, eb);
}

/* ---------- connect (TCP + TLS handshake) ---------- */

LEAN_EXPORT lean_obj_res leantea_tls_connect(
    b_lean_obj_arg host_obj, uint16_t port, lean_obj_arg w) {
  (void)w;
  const char *host = lean_string_cstr(host_obj);
  char portstr[8];
  snprintf(portstr, sizeof portstr, "%u", (unsigned)port);

  /* DNS + TCP connect (getaddrinfo so real public hosts resolve). */
  struct addrinfo hints, *res = NULL, *rp;
  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  int gai = getaddrinfo(host, portstr, &hints, &res);
  if (gai != 0) return tls_err2("tls getaddrinfo", gai_strerror(gai));
  int fd = -1;
  for (rp = res; rp; rp = rp->ai_next) {
    fd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (fd < 0) continue;
    if (connect(fd, rp->ai_addr, rp->ai_addrlen) == 0) break;
    close(fd); fd = -1;
  }
  freeaddrinfo(res);
  if (fd < 0) return tls_err2("tls connect", strerror(errno));

  SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
  if (!ctx) { close(fd); return ssl_errstr("SSL_CTX_new"); }
  SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);

  const char *insecure = getenv("LEANTEA_TLS_INSECURE");
  int verify = !(insecure && insecure[0] == '1');
  if (verify) {
    SSL_CTX_set_default_verify_paths(ctx);
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  }

  SSL *ssl = SSL_new(ctx);
  if (!ssl) { SSL_CTX_free(ctx); close(fd); return ssl_errstr("SSL_new"); }
  SSL_set_fd(ssl, fd);
  SSL_set_tlsext_host_name(ssl, host);          /* SNI */
  if (verify) {
    SSL_set_hostflags(ssl, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    if (!SSL_set1_host(ssl, host)) {
      lean_object *e = ssl_errstr("SSL_set1_host");
      SSL_free(ssl); SSL_CTX_free(ctx); close(fd);
      return e;
    }
  }

  if (SSL_connect(ssl) != 1) {
    lean_object *e = ssl_errstr("SSL_connect (TLS handshake)");
    SSL_free(ssl); SSL_CTX_free(ctx); close(fd);
    return e;
  }

  tls_conn *c = (tls_conn *)malloc(sizeof *c);
  if (!c) { SSL_free(ssl); SSL_CTX_free(ctx); close(fd); return tls_err("tls: OOM"); }
  c->ctx = ctx; c->ssl = ssl; c->fd = fd;
  return lean_io_result_mk_ok(lean_alloc_external(get_tls_class(), c));
}

/* ---------- send (write-all) ---------- */

LEAN_EXPORT lean_obj_res leantea_tls_send(
    b_lean_obj_arg conn_obj, b_lean_obj_arg ba, lean_obj_arg w) {
  (void)w;
  tls_conn *c = (tls_conn *)lean_get_external_data(conn_obj);
  if (!c || !c->ssl) return tls_err("tls_send: connection closed");
  size_t total = lean_sarray_size(ba);
  const uint8_t *data = lean_sarray_cptr(ba);
  size_t sent = 0;
  while (sent < total) {
    int n = SSL_write(c->ssl, data + sent, (int)(total - sent));
    if (n <= 0) return ssl_errstr("SSL_write");
    sent += (size_t)n;
  }
  return lean_io_result_mk_ok(lean_box(0));
}

/* ---------- recv (one read; empty ByteArray = EOF/clean close) ---------- */

LEAN_EXPORT lean_obj_res leantea_tls_recv(
    b_lean_obj_arg conn_obj, uint32_t max, lean_obj_arg w) {
  (void)w;
  tls_conn *c = (tls_conn *)lean_get_external_data(conn_obj);
  if (!c || !c->ssl) return tls_err("tls_recv: connection closed");
  if (max == 0) max = 65536;
  lean_object *ba = lean_alloc_sarray(1, 0, (size_t)max);
  uint8_t *buf = lean_sarray_cptr(ba);
  int n = SSL_read(c->ssl, buf, (int)max);
  if (n <= 0) {
    int se = SSL_get_error(c->ssl, n);
    /* Peer closed the TLS session (or the TCP conn) => end of body. */
    if (se == SSL_ERROR_ZERO_RETURN || se == SSL_ERROR_SYSCALL) {
      lean_sarray_set_size(ba, 0);
      return lean_io_result_mk_ok(ba);
    }
    lean_dec(ba);
    return ssl_errstr("SSL_read");
  }
  lean_sarray_set_size(ba, (size_t)n);
  return lean_io_result_mk_ok(ba);
}

/* ---------- close (half-shut write side; finalizer frees on GC) ---------- */

LEAN_EXPORT lean_obj_res leantea_tls_close(
    b_lean_obj_arg conn_obj, lean_obj_arg w) {
  (void)w;
  tls_conn *c = (tls_conn *)lean_get_external_data(conn_obj);
  if (c && c->ssl) SSL_shutdown(c->ssl);
  return lean_io_result_mk_ok(lean_box(0));
}

#else  /* !LEANTEA_HAVE_TLS — stubs keep the default build green */

static lean_object *tls_disabled(void) {
  return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(
    "TLS not enabled: rebuild lean-tea with LEANTEA_TLS=1 "
    "(and link -lssl -lcrypto). HTTPS via TlsClient requires OpenSSL.")));
}

LEAN_EXPORT lean_obj_res leantea_tls_connect(b_lean_obj_arg h, uint16_t p, lean_obj_arg w) {
  (void)h; (void)p; (void)w; return tls_disabled();
}
LEAN_EXPORT lean_obj_res leantea_tls_send(b_lean_obj_arg c, b_lean_obj_arg b, lean_obj_arg w) {
  (void)c; (void)b; (void)w; return tls_disabled();
}
LEAN_EXPORT lean_obj_res leantea_tls_recv(b_lean_obj_arg c, uint32_t m, lean_obj_arg w) {
  (void)c; (void)m; (void)w; return tls_disabled();
}
LEAN_EXPORT lean_obj_res leantea_tls_close(b_lean_obj_arg c, lean_obj_arg w) {
  (void)c; (void)w; return tls_disabled();
}

#endif
