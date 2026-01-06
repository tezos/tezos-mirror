/*************************************************************************/
/*                                                                       */
/* SPDX-License-Identifier: MIT                                          */
/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>          */
/* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com> */
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
 */

/*
 * NOTE:
 * Verified against sqlite3-ocaml 5.3.1.
 * struct db_wrap has sqlite3* as its first field.
 */

/* Prefix view of db_wrap: MUST match the first field */
typedef struct {
  sqlite3 *db;
} db_wrap_prefix;

/* Extract sqlite3* from an OCaml Sqlite3.db */
static inline sqlite3 *ocaml_sqlite3_db(value v)
{
    /* db_wrap is stored as a pointer in the custom block */
    void *p = *((void **)Data_custom_val(v));
    return ((db_wrap_prefix *)p)->db;
}

#endif /* SQLITE3_OCAML_STUBS_H */
