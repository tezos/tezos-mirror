(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let check_all_bakers_attest_at_level ctxt ~attested_level =
  match Constants_storage.all_bakers_attest_first_level ctxt with
  | None -> false
  | Some act_level -> Level_repr.(attested_level >= act_level)

let consensus_committee ctxt ~attested_level =
  let open Lwt_result_syntax in
  if check_all_bakers_attest_at_level ctxt ~attested_level then
    let* ctxt, total_staking_weight, _ =
      Delegate_sampler.stake_info ctxt attested_level
    in
    return (ctxt, total_staking_weight)
  else
    return
      (ctxt, Int64.of_int @@ Constants_storage.consensus_committee_size ctxt)

let consensus_threshold ctxt ~attested_level =
  let open Lwt_result_syntax in
  if check_all_bakers_attest_at_level ctxt ~attested_level then
    let* ctxt, committee = consensus_committee ctxt ~attested_level in
    let const_committee =
      Int64.of_int @@ Constants_storage.consensus_committee_size ctxt
    in
    let const_threshold =
      Int64.of_int @@ Constants_storage.consensus_threshold_size ctxt
    in
    let threshold =
      Int64.(mul const_threshold (div committee const_committee))
    in
    return (ctxt, threshold)
  else
    return
      (ctxt, Int64.of_int @@ Constants_storage.consensus_threshold_size ctxt)

let is_all_bakers_attest_enabled_for_cycle ctxt cycle =
  let cycle_eras = Raw_context.cycle_eras ctxt in
  let first_level_of_cycle =
    Level_repr.first_level_in_cycle_from_eras ~cycle_eras cycle
  in
  check_all_bakers_attest_at_level ctxt ~attested_level:first_level_of_cycle
