(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Dal node storage
   Invocation:   dune exec src/lib_dal_node/test/main.exe \
                  -- --file test_storage.ml
   Subject:      Testsuite related with the DAL node storage backend
*)

(* [number_of_slots] indicates the number of available slots per
   block.
   TODO: Update this value to match the current protocol constant when
   needed. *)
let number_of_slots = 32

(* As found in bin_dal_node/store.ml *)
let init_skip_list_cells_store node_store_dir =
  let padded_encoded_cell_size = 64 * (32 + 1) in
  let encoded_hash_size = 32 + 4 in
  Kvs_skip_list_cells_store.init
    ~node_store_dir
    ~skip_list_store_dir:"skip_list_store"
    ~padded_encoded_cell_size
    ~encoded_hash_size

(* Required by the KVS store. *)
let () = Value_size_hooks.set_number_of_slots number_of_slots

(* This test checks that the sqlite and the kvs backends exhibit the same behaviours.
   To do that, it generates lists of actions to be performed. After performing the
   list of actions, the test runs a handshake to verify that the stores are equivalent
   w.r.t to the cell hashes generated during the run. *)
let consistency_test =
  let open Lwt_result_syntax in
  let property _ =
    let run =
      (* Initialize the KVS and the SQLite stores. *)
      let data_dir = Tezt.Temp.dir "data-dir" in
      let* kvs_store = init_skip_list_cells_store data_dir in
      let* _sql_store = Dal_store_sqlite3.init ~data_dir ~perm:`Read_write () in
      (* We are done with this run. *)
      let* () = Kvs_skip_list_cells_store.close kvs_store in
      return_unit
    in
    match Lwt_main.run run with
    | Ok () -> Ok ()
    | Error err ->
        Error (`Fail (Format.asprintf "%a" Error_monad.pp_print_trace err))
  in
  Tezt_bam.Pbt.register
    ~pp:Format.pp_print_bool
    ~hash:Ppx_hash_lib.Std.Hash.Builtin.hash_bool
    ~minimum_number_of_samples:10
    ~title:"KVS and SQLite3 stores consistency"
    ~stop_after:(`Timeout 30.)
    ~__FILE__
    ~tags:["kvs"; "sqlite"]
    ~gen:(Bam.Std.bool ())
    ~property
    ()
