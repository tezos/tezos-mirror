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

typedef struct user_function {
  value v_fun;
  struct user_function *next;
} user_function;

typedef struct user_collation {
  value v_fun;
  struct user_collation *next;
} user_collation;

typedef struct db_wrap {
  sqlite3 *db;
  int rc;
  _Atomic(int) ref_count;
  user_function *user_functions;
  user_collation *user_collations;
} db_wrap;

#define Sqlite3_val(x) (*((db_wrap **)Data_custom_val(x)))

#endif /* SQLITE3_OCAML_STUBS_H */
