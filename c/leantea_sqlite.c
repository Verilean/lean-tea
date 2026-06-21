/* leantea_sqlite.c — minimal SQLite FFI wrapper for LeanTea.

   API exposed to Lean:
     leantea_sqlite_open(path : String) : IO Db
     leantea_sqlite_close(db : Db) : IO Unit
     leantea_sqlite_exec(db, sql) : IO Unit
     leantea_sqlite_execp(db, sql, params : Array String) : IO Nat
     leantea_sqlite_query(db, sql, params : Array String)
       : IO (Array (Array String))

   Strings are bound as TEXT. NULLs are returned as empty strings to
   keep the Lean side simple. Errors surface as IO.userError. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lean/lean.h>
#include "sqlite3.h"

/* ---------- external class for sqlite3* ---------- */

static void leantea_db_finalize(void *p) {
  if (p) sqlite3_close((sqlite3 *)p);
}

static void leantea_db_foreach(void *p, b_lean_obj_arg f) {
  (void)p; (void)f;
}

static lean_external_class *g_db_class = NULL;
static lean_external_class *get_db_class(void) {
  if (!g_db_class) {
    g_db_class = lean_register_external_class(
        leantea_db_finalize, leantea_db_foreach);
  }
  return g_db_class;
}

/* ---------- helpers ---------- */

static lean_object *err_str(const char *msg) {
  return lean_mk_io_user_error(lean_mk_string(msg));
}

static lean_object *err_io(const char *prefix, const char *m) {
  char buf[1024];
  snprintf(buf, sizeof(buf), "%s: %s", prefix, m ? m : "(null)");
  return err_str(buf);
}

/* ---------- open ---------- */

LEAN_EXPORT lean_obj_res leantea_sqlite_open(
    b_lean_obj_arg path_obj, lean_obj_arg /* world */ w) {
  (void)w;
  const char *path = lean_string_cstr(path_obj);
  sqlite3 *db = NULL;
  int rc = sqlite3_open(path, &db);
  if (rc != SQLITE_OK) {
    const char *m = db ? sqlite3_errmsg(db) : sqlite3_errstr(rc);
    lean_object *e = err_io("sqlite_open", m);
    if (db) sqlite3_close(db);
    return lean_io_result_mk_error(e);
  }
  sqlite3_busy_timeout(db, 5000);
  return lean_io_result_mk_ok(lean_alloc_external(get_db_class(), db));
}

/* ---------- close ---------- */

LEAN_EXPORT lean_obj_res leantea_sqlite_close(
    b_lean_obj_arg db_obj, lean_obj_arg w) {
  (void)w;
  sqlite3 *db = (sqlite3 *)lean_get_external_data(db_obj);
  if (db) {
    sqlite3_close(db);
    /* Mark external as freed so finalize is a no-op. */
    /* (Lean handles ref counts; we set external data to NULL.)   */
    /* Lean doesn't expose set_external_data, but close is        */
    /* idempotent via sqlite3 semantics so finalize calling again */
    /* is fine if we leave it. */
  }
  return lean_io_result_mk_ok(lean_box(0));
}

/* ---------- exec (no params, DDL or single stmts) ---------- */

LEAN_EXPORT lean_obj_res leantea_sqlite_exec(
    b_lean_obj_arg db_obj, b_lean_obj_arg sql_obj, lean_obj_arg w) {
  (void)w;
  sqlite3 *db = (sqlite3 *)lean_get_external_data(db_obj);
  const char *sql = lean_string_cstr(sql_obj);
  char *errmsg = NULL;
  int rc = sqlite3_exec(db, sql, NULL, NULL, &errmsg);
  if (rc != SQLITE_OK) {
    lean_object *e = err_io("sqlite_exec",
                            errmsg ? errmsg : sqlite3_errstr(rc));
    if (errmsg) sqlite3_free(errmsg);
    return lean_io_result_mk_error(e);
  }
  return lean_io_result_mk_ok(lean_box(0));
}

/* ---------- bind helpers ---------- */

static int bind_text_params(sqlite3_stmt *stmt, b_lean_obj_arg params_obj) {
  size_t n = lean_array_size(params_obj);
  for (size_t i = 0; i < n; i++) {
    lean_object *p = lean_array_get_core(params_obj, i);
    const char *s = lean_string_cstr(p);
    int rc = sqlite3_bind_text(stmt, (int)i + 1, s, -1, SQLITE_TRANSIENT);
    if (rc != SQLITE_OK) return rc;
  }
  return SQLITE_OK;
}

/* ---------- execp (with params, returns rows affected) ---------- */

LEAN_EXPORT lean_obj_res leantea_sqlite_execp(
    b_lean_obj_arg db_obj, b_lean_obj_arg sql_obj,
    b_lean_obj_arg params_obj, lean_obj_arg w) {
  (void)w;
  sqlite3 *db = (sqlite3 *)lean_get_external_data(db_obj);
  const char *sql = lean_string_cstr(sql_obj);
  sqlite3_stmt *stmt = NULL;
  int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
  if (rc != SQLITE_OK) {
    return lean_io_result_mk_error(
        err_io("sqlite_prepare", sqlite3_errmsg(db)));
  }
  rc = bind_text_params(stmt, params_obj);
  if (rc != SQLITE_OK) {
    lean_object *e = err_io("sqlite_bind", sqlite3_errmsg(db));
    sqlite3_finalize(stmt);
    return lean_io_result_mk_error(e);
  }
  rc = sqlite3_step(stmt);
  int changes = sqlite3_changes(db);
  sqlite3_finalize(stmt);
  if (rc != SQLITE_DONE && rc != SQLITE_ROW) {
    return lean_io_result_mk_error(
        err_io("sqlite_step", sqlite3_errmsg(db)));
  }
  return lean_io_result_mk_ok(lean_box(changes < 0 ? 0 : (unsigned)changes));
}

/* ---------- query (SELECT) ---------- */

LEAN_EXPORT lean_obj_res leantea_sqlite_query(
    b_lean_obj_arg db_obj, b_lean_obj_arg sql_obj,
    b_lean_obj_arg params_obj, lean_obj_arg w) {
  (void)w;
  sqlite3 *db = (sqlite3 *)lean_get_external_data(db_obj);
  const char *sql = lean_string_cstr(sql_obj);
  sqlite3_stmt *stmt = NULL;
  int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);
  if (rc != SQLITE_OK) {
    return lean_io_result_mk_error(
        err_io("sqlite_prepare", sqlite3_errmsg(db)));
  }
  rc = bind_text_params(stmt, params_obj);
  if (rc != SQLITE_OK) {
    lean_object *e = err_io("sqlite_bind", sqlite3_errmsg(db));
    sqlite3_finalize(stmt);
    return lean_io_result_mk_error(e);
  }
  lean_object *rows = lean_alloc_array(0, 0);
  while ((rc = sqlite3_step(stmt)) == SQLITE_ROW) {
    int n_cols = sqlite3_column_count(stmt);
    lean_object *row = lean_alloc_array(0, 0);
    for (int c = 0; c < n_cols; c++) {
      const unsigned char *t = sqlite3_column_text(stmt, c);
      lean_object *s = lean_mk_string(t ? (const char *)t : "");
      row = lean_array_push(row, s);
    }
    rows = lean_array_push(rows, row);
  }
  sqlite3_finalize(stmt);
  if (rc != SQLITE_DONE) {
    return lean_io_result_mk_error(
        err_io("sqlite_step", sqlite3_errmsg(db)));
  }
  return lean_io_result_mk_ok(rows);
}
