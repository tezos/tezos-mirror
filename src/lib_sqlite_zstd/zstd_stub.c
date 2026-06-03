/*************************************************************************/
/*                                                                       */
/* SPDX-License-Identifier: MIT                                          */
/* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>          */
/*                                                                       */
/*************************************************************************/

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <sqlite3.h>
#include "sqlite3_ocaml_stubs.h"

/* Provided by zstd.c */
int sqlite3_zstd_init(sqlite3 *db, int level);

CAMLprim value caml_sqlite3_register_zstd(value v_db, value v_level)
{
    CAMLparam2(v_db, v_level);

    sqlite3 *db = ocaml_sqlite3_db(v_db);

    if (db == NULL)
        caml_failwith("sqlite_zstd: invalid sqlite3 handle");

    int level = Int_val(v_level);

    if (sqlite3_zstd_init(db, level) != SQLITE_OK)
        caml_failwith("sqlite_zstd: init failed");

    CAMLreturn(Val_unit);
}
