(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** The RPCs directory of the protocol part of DAL nodes. *)
val directory :
  [< `KVS of Kvs_skip_list_cells_store.t | `SQLite3 of Dal_store_sqlite3.t] ->
  unit Environment.RPC_directory.t
