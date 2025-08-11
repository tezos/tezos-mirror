(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let for_double_baking ctxt =
  Constants_storage.percentage_of_frozen_deposits_slashed_per_double_baking ctxt

let for_double_attestation ctxt committee_size rights denounced =
  let total_rights_denounced =
    List.fold_left
      (fun total delegate ->
        Option.value
          (Signature.Public_key_hash.Map.find delegate rights)
          ~default:0
        |> Int64.of_int |> Int64.add total)
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

let get ctxt ~(kind : Misbehaviour_repr.kind) ~(level : Level_repr.t)
    (denounced : Signature.public_key_hash list) =
  let open Lwt_result_syntax in
  match kind with
  | Double_baking -> return (ctxt, for_double_baking ctxt)
  | Double_attesting | Double_preattesting ->
      (* TODO ABAAB. Note that the [rights] amount should also depend on the flag status. *)
      let* ctxt, rights = Delegate_sampler.attesting_rights_count ctxt level in
      let* ctxt, committee_size =
        Consensus_parameters_storage.consensus_committee ctxt level
      in
      return (ctxt, for_double_attestation ctxt committee_size rights denounced)

module Internal_for_tests = struct
  let for_double_attestation = for_double_attestation
end
