/*************************************************************************/
/*                                                                       */
/* SPDX-License-Identifier: MIT                                          */
/* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>          */
/*                                                                       */
/*************************************************************************/

#ifndef SQLITE3_OCAML_STUBS_H
#define SQLITE3_OCAML_STUBS_H

#include <sqlite3.h>
#include <caml/mlvalues.h>

/* Abstract OCaml SQLite3 db handle.
 *
 * This minimal declaration mirrors the upstream ocaml-sqlite3
 * implementation, without requiring the full original header.
 *
 * ABI assumption: [db_wrap] (the C struct that backs an OCaml
 * [Sqlite3.db]) has [sqlite3 *db] as its first field, and an OCaml
 * [Sqlite3.db] is a custom block whose payload is a single [void *]
 * pointing at that struct. Verified against sqlite3-ocaml 5.3.1; if
 * either of those two facts changes upstream, [ocaml_sqlite3_db]
 * silently dereferences a wrongly-typed pointer.
 *
 * The dependency is exact-pinned at "sqlite3 = 5.3.1" in
 * [manifest/externals.ml]; any bump must re-verify this layout. */

/* Prefix view of db_wrap: MUST match the first field */
typedef struct {
  sqlite3 *db;
} db_wrap_prefix;

/* Extract sqlite3* from an OCaml Sqlite3.db */
static inline sqlite3 *ocaml_sqlite3_db(value v)
{
    /* db_wrap is stored as a pointer in the custom block */
    void *p = *((void **)Data_custom_val(v));
    if (p == NULL)
        return NULL;
    return ((db_wrap_prefix *)p)->db;
}

#endif /* SQLITE3_OCAML_STUBS_H */
