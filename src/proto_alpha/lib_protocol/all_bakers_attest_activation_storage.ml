(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let may_update_all_bakers_attest_first_level ctxt cycle ~total_number_bakers
    ~tz4_number_bakers =
  let open Lwt_syntax in
  let* b = Storage.All_bakers_attest_activation.mem ctxt in
  if b then return ctxt
  else
    let Ratio_repr.{numerator; denominator} =
      Constants_storage.all_bakers_attest_activation_threshold ctxt
    in
    (* Activate iff tz4_bakers >= total_bakers * numerator / denominator
                  <=> tz4_bakers * denominator >= total_bakers * numerator *)
    let total = total_number_bakers * numerator in
    let tz4_weight = tz4_number_bakers * denominator in
    if Compare.Int.(tz4_weight < total) then return ctxt
    else
      let cycle_eras = Raw_context.cycle_eras ctxt in
      let level = Level_repr.first_level_in_cycle_from_eras ~cycle_eras cycle in
      Storage.All_bakers_attest_activation.add ctxt level

let set_all_bakers_attest_first_level ctxt =
  let open Lwt_result_syntax in
  let* launch_level_opt = Storage.All_bakers_attest_activation.find ctxt in
  match launch_level_opt with
  | None -> return ctxt
  | Some level ->
      return @@ Raw_context.set_all_bakers_attest_first_level ctxt level
