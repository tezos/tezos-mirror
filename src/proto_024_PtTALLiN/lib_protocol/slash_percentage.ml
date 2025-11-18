(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let for_double_baking ctxt =
  Constants_storage.percentage_of_frozen_deposits_slashed_per_double_baking ctxt

let for_double_attestation ctxt ~committee_size rights denounced =
  let total_rights_denounced =
    List.fold_left
      (fun total delegate ->
        Option.value
          (Signature.Public_key_hash.Map.find delegate rights)
          ~default:0L
        |> Int64.add total)
      0L
      denounced
  in
  let Ratio_repr.{numerator; denominator} =
    Constants_storage.max_slashing_threshold ctxt
  in
  (* Temporary Z cast to avoid overflow *)
  let committee_size = Z.of_int64 committee_size in
  let numerator, denominator = Z.(of_int numerator, of_int denominator) in
  (* Set the threshold to ⌈committee_size * threshold_ratio⌉
               = 1 + (((committee_size * numerator) - 1) / denominator) *)
  let threshold_max =
    Z.(
      succ (div (sub (mul committee_size numerator) one) denominator)
      |> to_int64)
  in
  let max_slashing = Constants_storage.max_slashing_per_block ctxt in
  if Compare.Int64.(total_rights_denounced >= threshold_max) then max_slashing
  else
    let num_z = Z.(pow (of_int64 total_rights_denounced) 2) in
    let den_z = Z.(pow (of_int64 threshold_max) 2) in
    Percentage.mul_q_bounded ~round:`Up max_slashing Q.(num_z /// den_z)

let get ctxt misbehaviour (denounced : Signature.public_key_hash list) =
  let open Lwt_result_syntax in
  match misbehaviour.Misbehaviour_repr.kind with
  | Double_baking -> return (ctxt, for_double_baking ctxt)
  | Double_attesting | Double_preattesting ->
      let misbehaviour_level = Level_storage.from_raw ctxt misbehaviour.level in
      let all_bakers_attest_enabled =
        Consensus_parameters_storage.check_all_bakers_attest_at_level
          ctxt
          ~attested_level:misbehaviour_level
      in
      let* ctxt, rights =
        Delegate_sampler.attesting_power
          ~all_bakers_attest_enabled
          ctxt
          misbehaviour_level
      in
      let* ctxt, committee_size =
        Consensus_parameters_storage.consensus_committee
          ctxt
          ~attested_level:misbehaviour_level
      in
      return (ctxt, for_double_attestation ctxt ~committee_size rights denounced)

module Internal_for_tests = struct
  let for_double_attestation = for_double_attestation
end
