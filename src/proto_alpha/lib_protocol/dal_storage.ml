(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Store = Storage.Dal

let get_past_parameters ctxt =
  let open Lwt_result_syntax in
  let+ cell = Store.Past_parameters.find ctxt in
  Option.value ~default:[] cell

let save_parameters ctxt
    (previous_parameters : Constants_parametric_previous_repr.dal)
    ~next_protocol_activation =
  let open Lwt_result_syntax in
  let* past_parameters = get_past_parameters ctxt in
  let previous_parameters : Constants_parametric_repr.dal =
    let Constants_parametric_previous_repr.
          {
            feature_enable;
            incentives_enable;
            number_of_slots;
            attestation_lag;
            attestation_threshold;
            cryptobox_parameters;
            minimal_participation_ratio;
            rewards_ratio;
            traps_fraction;
          } =
      previous_parameters
    in
    {
      feature_enable;
      incentives_enable;
      number_of_slots;
      attestation_lag;
      attestation_threshold;
      cryptobox_parameters;
      minimal_participation_ratio;
      rewards_ratio;
      traps_fraction;
    }
  in
  let*! ctxt =
    Store.Past_parameters.add
      ctxt
      (Constants_parametric_repr.
         {dal_parameters = previous_parameters; next_protocol_activation}
      :: past_parameters)
  in
  return ctxt

let parameters ctxt level =
  let open Lwt_result_syntax in
  let* past_parameters = get_past_parameters ctxt in
  let parameters =
    List.find_map
      (fun Constants_parametric_repr.{dal_parameters; next_protocol_activation}
         ->
        if Raw_level_repr.(level <= next_protocol_activation) then
          Some dal_parameters
        else None)
      (List.rev past_parameters)
  in
  match parameters with
  | Some parameters -> return parameters
  | None ->
      let constants = Constants_storage.parametric ctxt in
      return constants.dal
