(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let for_double_baking ctxt =
  Constants_storage.percentage_of_frozen_deposits_slashed_per_double_baking ctxt

let for_double_attestation ctxt rights denounced =
  if
    not
      (Constants_storage.adaptive_issuance_ns_enable ctxt
      && Constants_storage.adaptive_issuance_enable ctxt)
  then
    Constants_storage
    .percentage_of_frozen_deposits_slashed_per_double_attestation
      ctxt
  else
    let total_rights_denounced =
      List.fold_left
        (fun total delegate ->
          Option.value
            (Signature.Public_key_hash.Map.find delegate rights)
            ~default:0
          |> ( + ) total)
        0
        denounced
    in
    let threshold_max = (Raw_context.constants ctxt).max_slashing_threshold in
    let max_slashing = (Raw_context.constants ctxt).max_slashing_per_block in
    if Compare.Int.(total_rights_denounced >= threshold_max) then max_slashing
    else
      let num_z = Z.(pow (of_int total_rights_denounced) 2) in
      let den_z = Z.(pow (of_int threshold_max) 2) in
      Percentage.mul_q_bounded ~round:`Up max_slashing Q.(num_z /// den_z)

let get ctxt ~(kind : Misbehaviour_repr.kind) ~(level : Level_repr.t)
    (denounced : Signature.public_key_hash list) =
  let open Lwt_result_syntax in
  match kind with
  | Double_baking -> return (ctxt, for_double_baking ctxt)
  | Double_attesting | Double_preattesting ->
      let* ctxt, rights = Delegate_sampler.attesting_rights_count ctxt level in
      return (ctxt, for_double_attestation ctxt rights denounced)

module Internal_for_tests = struct
  let for_double_attestation = for_double_attestation
end
