(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_riscv_pvm

let test_simple () =
  let open Lwt_syntax in
  Lwt_utils_unix.with_tempdir "repo_dir" (fun dir_name ->
      let empty = Storage.empty () in
      let* repo = Storage.load ~cache_size:0 ~readonly:false dir_name in
      let* id = Storage.commit repo empty in
      let* checked_out_empty = Storage.checkout repo id in
      assert (
        Option.equal Storage.Mutable_state.equal checked_out_empty (Some empty)) ;
      let* id2 = Storage.commit repo empty in
      assert (Storage.Id.equal id id2) ;
      let* () = Storage.close repo in
      return_unit)

let test_state_simple () =
  let open Lwt_syntax in
  let empty = Storage.empty () in
  let* found_empty = Storage.find empty Storage.pvm_state_key in
  assert (Option.equal Storage.Mutable_state.equal found_empty (Some empty)) ;
  let empty2 = Storage.empty () in
  let* () = Storage.set empty Storage.pvm_state_key empty in
  assert (Storage.Mutable_state.equal empty empty2) ;
  return_unit

let test_export_snapshot () =
  let open Lwt_result_syntax in
  Lwt_utils_unix.with_tempdir "base_dir" (fun dir_name ->
      let repo_dir = dir_name ^ "/repo" in
      let snapshot_dir = dir_name ^ "/snapshot" in
      let empty = Storage.empty () in
      let*! repo = Storage.load ~cache_size:0 ~readonly:false repo_dir in
      let*! id = Storage.commit repo empty in
      let* () = Storage.export_snapshot repo id snapshot_dir in
      let*! snapshot = Storage.load ~cache_size:0 ~readonly:true snapshot_dir in
      let*! exported_empty = Storage.checkout snapshot id in
      assert (
        Option.equal Storage.Mutable_state.equal exported_empty (Some empty)) ;
      let*! () = Storage.close repo in
      let*! () = Storage.close snapshot in
      return_unit)
