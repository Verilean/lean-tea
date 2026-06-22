/* leantea_postgres.c — PostgreSQL FFI wrapper for LeanTea.

   Mirrors the MySQL wrapper's surface (one connection, text in/out,
   `?` placeholders translated to `$1`, `$2`, …). Built with
   `-DLEANTEA_HAVE_POSTGRES` and linked against libpq for real
   support; built without it for a stub mode that still links but
   throws at first use.

   API:
     leantea_pg_open(connStr) : IO Conn
     leantea_pg_close(c) : IO Unit
     leantea_pg_execp(c, sql, params : Array String) : IO Nat
     leantea_pg_query(c, sql, params : Array String)
       : IO (Array (Array String))

   Parameter binding: each `?` in the SQL is rewritten to `$1`,
   `$2`, … and the values are passed to `PQexecParams` as text
   parameters (format=0). libpq handles the escaping/quoting. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lean/lean.h>

#ifdef LEANTEA_HAVE_POSTGRES
#include <libpq-fe.h>
#endif

/* ---------- error helpers ---------- */

static lean_object *err_str(const char *msg) {
  return lean_mk_io_user_error(lean_mk_string(msg));
}

#ifndef LEANTEA_HAVE_POSTGRES
static lean_obj_res not_built(void) {
  return lean_io_result_mk_error(err_str(
    "PostgreSQL support not compiled in — rebuild with "
    "LEANTEA_POSTGRES=1 and libpq-dev installed"));
}
#endif

#ifdef LEANTEA_HAVE_POSTGRES

/* ---------- external class for PGconn* ---------- */

static void leantea_pg_finalize(void *p) {
  if (p) PQfinish((PGconn *)p);
}

static void leantea_pg_foreach(void *p, b_lean_obj_arg f) {
  (void)p; (void)f;
}

static lean_external_class *g_conn_class = NULL;
static lean_external_class *get_conn_class(void) {
  if (!g_conn_class) {
    g_conn_class = lean_register_external_class(
        leantea_pg_finalize, leantea_pg_foreach);
  }
  return g_conn_class;
}

/* ---------- ? → $N translator ----------

   Rewrites the SQL so each `?` becomes `$1`, `$2`, … in order.
   Counts the placeholders along the way for sanity-checking against
   the params array. Returns a malloc'd null-terminated string the
   caller must free; sets `*out_count` to the number of placeholders
   seen. Returns NULL on OOM. */
static char *rewrite_placeholders(const char *sql, size_t *out_count) {
  size_t sql_len = strlen(sql);
  /* Worst case: every char is `?` → 4 bytes each ($10000). Reserve
     a generous expansion factor. */
  size_t cap = sql_len * 5 + 16;
  char *out = (char *)malloc(cap);
  if (!out) return NULL;
  size_t oi = 0;
  size_t count = 0;
  int in_squote = 0;          /* inside '...' literal */
  int in_dquote = 0;          /* inside "..." identifier */
  for (size_t i = 0; i < sql_len; i++) {
    char c = sql[i];
    /* Toggle string-literal state — Postgres allows `''` to escape
       a single quote inside a literal. Same for double-quoted
       identifiers. */
    if (!in_dquote && c == '\'') {
      if (in_squote && i + 1 < sql_len && sql[i + 1] == '\'') {
        out[oi++] = c;
        out[oi++] = sql[++i];
        continue;
      }
      in_squote = !in_squote;
      out[oi++] = c;
      continue;
    }
    if (!in_squote && c == '"') {
      in_dquote = !in_dquote;
      out[oi++] = c;
      continue;
    }
    if (c == '?' && !in_squote && !in_dquote) {
      count++;
      int n = snprintf(out + oi, cap - oi, "$%zu", count);
      if (n < 0 || (size_t)n >= cap - oi) {
        free(out);
        return NULL;
      }
      oi += (size_t)n;
      continue;
    }
    out[oi++] = c;
  }
  out[oi] = '\0';
  *out_count = count;
  return out;
}

#endif /* LEANTEA_HAVE_POSTGRES */

/* ---------- open ---------- */

LEAN_EXPORT lean_obj_res leantea_pg_open(
    b_lean_obj_arg conn_str_obj, lean_obj_arg w) {
  (void)w;
#ifdef LEANTEA_HAVE_POSTGRES
  const char *conn_str = lean_string_cstr(conn_str_obj);
  PGconn *c = PQconnectdb(conn_str);
  if (PQstatus(c) != CONNECTION_OK) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "PQconnectdb: %s", PQerrorMessage(c));
    PQfinish(c);
    return lean_io_result_mk_error(err_str(buf));
  }
  return lean_io_result_mk_ok(lean_alloc_external(get_conn_class(), c));
#else
  (void)conn_str_obj;
  return not_built();
#endif
}

/* ---------- close ---------- */

LEAN_EXPORT lean_obj_res leantea_pg_close(
    b_lean_obj_arg conn_obj, lean_obj_arg w) {
  (void)w; (void)conn_obj;
  /* Real close happens in finalize; explicit close is a no-op so
     the same handle can be safely released by GC. */
  return lean_io_result_mk_ok(lean_box(0));
}

/* ---------- shared exec path ----------

   Drives PQexecParams with text-format params. Returns the result
   handle; caller decides how to interpret tuples. NULL on error,
   in which case `*err_out` holds a malloc'd message the caller
   must free. */
#ifdef LEANTEA_HAVE_POSTGRES
static PGresult *exec_with_params(
    PGconn *c, const char *sql_in, b_lean_obj_arg params_obj,
    char **err_out) {
  *err_out = NULL;
  size_t expected = 0;
  char *sql = rewrite_placeholders(sql_in, &expected);
  if (!sql) {
    *err_out = strdup("execp: OOM rewriting placeholders");
    return NULL;
  }
  size_t n = lean_array_size(params_obj);
  if (n != expected) {
    char buf[256];
    snprintf(buf, sizeof(buf),
      "execp: `?` count (%zu) doesn't match params (%zu)", expected, n);
    free(sql);
    *err_out = strdup(buf);
    return NULL;
  }
  const char **values = NULL;
  if (n > 0) {
    values = (const char **)malloc(sizeof(char *) * n);
    if (!values) {
      free(sql);
      *err_out = strdup("execp: OOM allocating params array");
      return NULL;
    }
    for (size_t i = 0; i < n; i++) {
      lean_object *p = lean_array_get_core(params_obj, i);
      values[i] = lean_string_cstr(p);
    }
  }
  /* nParamTypes=0 → libpq infers; resultFormat=0 → text. */
  PGresult *r = PQexecParams(c, sql, (int)n, NULL, values, NULL, NULL, 0);
  free(sql);
  free(values);
  ExecStatusType s = PQresultStatus(r);
  if (s != PGRES_COMMAND_OK && s != PGRES_TUPLES_OK) {
    char buf[1024];
    snprintf(buf, sizeof(buf), "PQexecParams: %s", PQresultErrorMessage(r));
    *err_out = strdup(buf);
    PQclear(r);
    return NULL;
  }
  return r;
}
#endif

/* ---------- execp ---------- */

LEAN_EXPORT lean_obj_res leantea_pg_execp(
    b_lean_obj_arg conn_obj, b_lean_obj_arg sql_obj,
    b_lean_obj_arg params_obj, lean_obj_arg w) {
  (void)w;
#ifdef LEANTEA_HAVE_POSTGRES
  PGconn *c = (PGconn *)lean_get_external_data(conn_obj);
  char *err = NULL;
  PGresult *r = exec_with_params(c, lean_string_cstr(sql_obj), params_obj, &err);
  if (!r) {
    lean_object *e = err_str(err ? err : "PQexecParams: unknown error");
    free(err);
    return lean_io_result_mk_error(e);
  }
  const char *aff_s = PQcmdTuples(r);
  size_t affected = 0;
  if (aff_s && *aff_s) {
    affected = (size_t)strtoull(aff_s, NULL, 10);
  }
  PQclear(r);
  return lean_io_result_mk_ok(lean_box(affected));
#else
  (void)conn_obj; (void)sql_obj; (void)params_obj;
  return not_built();
#endif
}

/* ---------- query ---------- */

LEAN_EXPORT lean_obj_res leantea_pg_query(
    b_lean_obj_arg conn_obj, b_lean_obj_arg sql_obj,
    b_lean_obj_arg params_obj, lean_obj_arg w) {
  (void)w;
#ifdef LEANTEA_HAVE_POSTGRES
  PGconn *c = (PGconn *)lean_get_external_data(conn_obj);
  char *err = NULL;
  PGresult *r = exec_with_params(c, lean_string_cstr(sql_obj), params_obj, &err);
  if (!r) {
    lean_object *e = err_str(err ? err : "PQexecParams: unknown error");
    free(err);
    return lean_io_result_mk_error(e);
  }
  int n_rows = PQntuples(r);
  int n_cols = PQnfields(r);
  lean_object *outer = lean_alloc_array(n_rows, n_rows);
  for (int row = 0; row < n_rows; row++) {
    lean_object *inner = lean_alloc_array(n_cols, n_cols);
    for (int col = 0; col < n_cols; col++) {
      lean_object *cell;
      if (PQgetisnull(r, row, col)) {
        cell = lean_mk_string("");
      } else {
        const char *raw = PQgetvalue(r, row, col);
        int len = PQgetlength(r, row, col);
        cell = lean_mk_string_from_bytes(raw, (size_t)len);
      }
      lean_array_set_core(inner, col, cell);
    }
    lean_array_set_core(outer, row, inner);
  }
  PQclear(r);
  return lean_io_result_mk_ok(outer);
#else
  (void)conn_obj; (void)sql_obj; (void)params_obj;
  return not_built();
#endif
}
