(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [migrate_skip_list_store kvs_store sql_store] populates the
    [sql_store] by iterating over each attested level found in the
    [kvs_store] and inserting the corresponding payload in the
    [sql_store]. *)
val migrate_skip_list_store :
  Kvs_skip_list_cells_store.t ->
  Dal_store_sqlite3.Skip_list_cells.t ->
  unit tzresult Lwt.t
