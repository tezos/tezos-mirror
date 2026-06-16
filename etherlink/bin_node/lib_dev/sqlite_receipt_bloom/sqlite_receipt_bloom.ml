(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Register user defined SQLite functions:
    - [receipt_contains_bloom_filter(receipt_blob, bloom256_blob)]
*)
external register : Sqlite3.db -> unit = "caml_sqlite3_register_receipt_bloom"
