(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let set_all_bakers_attest_first_level ctxt =
  let open Lwt_result_syntax in
  let* launch_level_opt = Storage.All_bakers_attest_activation.find ctxt in
  match launch_level_opt with
  | None -> return ctxt
  | Some level ->
      return @@ Raw_context.set_all_bakers_attest_first_level ctxt level
