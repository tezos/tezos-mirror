/*************************************************************************/
/*                                                                       */
/* SPDX-License-Identifier: MIT                                          */
/* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>          */
/* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com> */
/*                                                                       */
/*************************************************************************/

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <sqlite3.h>
#include "sqlite3_ocaml_stubs.h"

/* Provided by receipt_bloom.c */
int sqlite3_receipt_bloom_init(sqlite3 *db);

CAMLprim value caml_sqlite3_register_receipt_bloom(value v_db)
{
    sqlite3 *db = ocaml_sqlite3_db(v_db);

    if (sqlite3_receipt_bloom_init(db) != SQLITE_OK)
        caml_failwith("receipt_bloom: init failed");

    return Val_unit;
}
