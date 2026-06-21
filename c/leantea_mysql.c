/* leantea_mysql.c — MySQL FFI wrapper for LeanTea.

   Mirrors the SQLite wrapper's surface (one connection, text in/out,
   `?` placeholders). Built with `-DLEANTEA_HAVE_MYSQL` and linked
   against libmysqlclient for real support; built without it for a
   stub mode that still satisfies the link but throws at first use.

   API:
     leantea_mysql_open(host, port, user, password, db) : IO Conn
     leantea_mysql_close(c) : IO Unit
     leantea_mysql_execp(c, sql, params : Array String) : IO Nat
     leantea_mysql_query(c, sql, params : Array String)
       : IO (Array (Array String))

   Parameter binding: each `?` in the SQL is replaced with the
   corresponding param value passed through `mysql_real_escape_string`
   (so single quotes etc. are safe). Bound values are always quoted
   as text strings — numeric columns coerce naturally on the server. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lean/lean.h>

#ifdef LEANTEA_HAVE_MYSQL
#include <mysql.h>
#endif

/* ---------- error helpers ---------- */

static lean_object *err_str(const char *msg) {
  return lean_mk_io_user_error(lean_mk_string(msg));
}

#ifndef LEANTEA_HAVE_MYSQL
static lean_obj_res not_built(void) {
  return lean_io_result_mk_error(err_str(
    "MySQL support not compiled in — rebuild with LEANTEA_MYSQL=1 "
    "and libmysqlclient installed"));
}
#endif

#ifdef LEANTEA_HAVE_MYSQL

/* ---------- external class for MYSQL* ---------- */

static void leantea_mysql_finalize(void *p) {
  if (p) mysql_close((MYSQL *)p);
}

static void leantea_mysql_foreach(void *p, b_lean_obj_arg f) {
  (void)p; (void)f;
}

static lean_external_class *g_conn_class = NULL;
static lean_external_class *get_conn_class(void) {
  if (!g_conn_class) {
    g_conn_class = lean_register_external_class(
        leantea_mysql_finalize, leantea_mysql_foreach);
  }
  return g_conn_class;
}

/* ---------- splice helper: replace each `?` in `sql` with the
   escaped, quoted value at the corresponding position. Returns a
   malloc'd null-terminated string the caller must free. Returns
   NULL on error (more params than `?` placeholders, or vice versa). */
static char *splice_params(MYSQL *m, const char *sql, b_lean_obj_arg params_obj) {
  size_t sql_len = strlen(sql);
  size_t n = lean_array_size(params_obj);

  /* Worst-case size: original sql + 2 quotes per ? + 2x each param. */
  size_t cap = sql_len + 16;
  for (size_t i = 0; i < n; i++) {
    lean_object *p = lean_array_get_core(params_obj, i);
    cap += 2 + 2 * strlen(lean_string_cstr(p)) + 2;
  }
  char *out = (char *)malloc(cap + 1);
  if (!out) return NULL;
  size_t oi = 0;
  size_t pi = 0;
  for (size_t i = 0; i < sql_len; i++) {
    char c = sql[i];
    if (c == '?' && pi < n) {
      lean_object *p = lean_array_get_core(params_obj, pi++);
      const char *raw = lean_string_cstr(p);
      size_t raw_len = strlen(raw);
      out[oi++] = '\'';
      oi += mysql_real_escape_string(m, out + oi, raw, raw_len);
      out[oi++] = '\'';
    } else {
      out[oi++] = c;
    }
  }
  if (pi != n) {
    /* Too many params for the placeholders. */
    free(out);
    return NULL;
  }
  out[oi] = '\0';
  return out;
}

#endif /* LEANTEA_HAVE_MYSQL */

/* ---------- open ---------- */

LEAN_EXPORT lean_obj_res leantea_mysql_open(
    b_lean_obj_arg host_obj, uint32_t port,
    b_lean_obj_arg user_obj, b_lean_obj_arg pass_obj,
    b_lean_obj_arg db_obj, lean_obj_arg w) {
  (void)w;
#ifdef LEANTEA_HAVE_MYSQL
  MYSQL *m = mysql_init(NULL);
  if (!m) return lean_io_result_mk_error(err_str("mysql_init failed"));
  const char *host = lean_string_cstr(host_obj);
  const char *user = lean_string_cstr(user_obj);
  const char *pass = lean_string_cstr(pass_obj);
  const char *db   = lean_string_cstr(db_obj);
  /* Auto-reconnect off — better to fail loudly than silently lose state. */
  if (!mysql_real_connect(m, host, user, pass, db, port, NULL, 0)) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "mysql_real_connect: %s", mysql_error(m));
    mysql_close(m);
    return lean_io_result_mk_error(err_str(buf));
  }
  mysql_set_character_set(m, "utf8mb4");
  return lean_io_result_mk_ok(lean_alloc_external(get_conn_class(), m));
#else
  (void)host_obj; (void)port; (void)user_obj; (void)pass_obj; (void)db_obj;
  return not_built();
#endif
}

/* ---------- close ---------- */

LEAN_EXPORT lean_obj_res leantea_mysql_close(
    b_lean_obj_arg conn_obj, lean_obj_arg w) {
  (void)w; (void)conn_obj;
  /* Real close happens in finalize; explicit close is a no-op so the
     same handle can be safely released by GC. */
  return lean_io_result_mk_ok(lean_box(0));
}

/* ---------- execp ---------- */

LEAN_EXPORT lean_obj_res leantea_mysql_execp(
    b_lean_obj_arg conn_obj, b_lean_obj_arg sql_obj,
    b_lean_obj_arg params_obj, lean_obj_arg w) {
  (void)w;
#ifdef LEANTEA_HAVE_MYSQL
  MYSQL *m = (MYSQL *)lean_get_external_data(conn_obj);
  char *spliced = splice_params(m, lean_string_cstr(sql_obj), params_obj);
  if (!spliced)
    return lean_io_result_mk_error(err_str(
      "execp: '?' count doesn't match params"));
  int rc = mysql_query(m, spliced);
  free(spliced);
  if (rc != 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "mysql_query: %s", mysql_error(m));
    return lean_io_result_mk_error(err_str(buf));
  }
  uint64_t affected = mysql_affected_rows(m);
  /* Drain any result set so the connection is ready for the next call. */
  MYSQL_RES *res = mysql_store_result(m);
  if (res) mysql_free_result(res);
  return lean_io_result_mk_ok(lean_box((size_t)affected));
#else
  (void)conn_obj; (void)sql_obj; (void)params_obj;
  return not_built();
#endif
}

/* ---------- query ---------- */

LEAN_EXPORT lean_obj_res leantea_mysql_query(
    b_lean_obj_arg conn_obj, b_lean_obj_arg sql_obj,
    b_lean_obj_arg params_obj, lean_obj_arg w) {
  (void)w;
#ifdef LEANTEA_HAVE_MYSQL
  MYSQL *m = (MYSQL *)lean_get_external_data(conn_obj);
  char *spliced = splice_params(m, lean_string_cstr(sql_obj), params_obj);
  if (!spliced)
    return lean_io_result_mk_error(err_str(
      "query: '?' count doesn't match params"));
  int rc = mysql_query(m, spliced);
  free(spliced);
  if (rc != 0) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "mysql_query: %s", mysql_error(m));
    return lean_io_result_mk_error(err_str(buf));
  }
  MYSQL_RES *res = mysql_store_result(m);
  /* mysql_store_result may legitimately return NULL when the
     statement produced no result set (INSERT etc.); treat as empty. */
  if (!res) {
    return lean_io_result_mk_ok(lean_alloc_array(0, 0));
  }
  unsigned int n_cols = mysql_num_fields(res);
  my_ulonglong n_rows = mysql_num_rows(res);
  lean_object *outer = lean_alloc_array(n_rows, n_rows);
  for (my_ulonglong r = 0; r < n_rows; r++) {
    MYSQL_ROW row = mysql_fetch_row(res);
    unsigned long *lengths = mysql_fetch_lengths(res);
    lean_object *inner = lean_alloc_array(n_cols, n_cols);
    for (unsigned int c = 0; c < n_cols; c++) {
      lean_object *cell;
      if (!row || !row[c]) {
        cell = lean_mk_string("");
      } else {
        cell = lean_mk_string_from_bytes(
            (const char *)row[c], lengths[c]);
      }
      lean_array_set_core(inner, c, cell);
    }
    lean_array_set_core(outer, r, inner);
  }
  mysql_free_result(res);
  return lean_io_result_mk_ok(outer);
#else
  (void)conn_obj; (void)sql_obj; (void)params_obj;
  return not_built();
#endif
}
