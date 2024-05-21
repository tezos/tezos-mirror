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
      assert (Option.equal Storage.State.equal checked_out_empty (Some empty)) ;
      let* id2 = Storage.commit repo empty in
      assert (Storage.Id.equal id id2) ;
      let* () = Storage.close repo in
      return_unit)
