(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let version = Store_version.V5_sqlite

include Sql_store

let init mode ~data_dir =
  let open Lwt_result_syntax in
  let* store_version = Store_version.read_version_file ~dir:data_dir in
  (match store_version with
  | None ->
      (* Don't write the store version in a file anymore. *)
      ()
  | Some V5_sqlite -> ()) ;
  init mode ~data_dir
