(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

let get_monitored_slot_indices node_ctxt =
  let profile =
    Profile_manager.get_profiles (Node_context.get_profile_ctxt node_ctxt)
  in
  match profile with
  | Bootstrap | Random_observer -> []
  | Operator operator_profile ->
      Operator_profile.get_all_slot_indexes operator_profile

let inject_entrapment_evidences (type b)
    (module Plugin : Dal_plugin.T with type block_info = b) node_ctxt rpc_ctxt
    (block : b) =
  let open Lwt_result_syntax in
  let attested_level = (Plugin.block_shell_header block).level in
  let proto_parameters = Node_context.get_proto_parameters node_ctxt in
  when_ proto_parameters.incentives_enable (fun () ->
      let monitored_slot_indices = get_monitored_slot_indices node_ctxt in
      if List.is_empty monitored_slot_indices then return_unit
      else
        let published_level =
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4612
             Correctly compute [published_level] in case of protocol changes, in
             particular a change of the value of [attestation_lag]. *)
          Int32.(sub attested_level (of_int proto_parameters.attestation_lag))
        in
        let store = Node_context.get_store node_ctxt in
        let traps_store = Store.traps store in
        let attestations = Plugin.get_attestation_operations block in
        let attestation_map =
          List.fold_left
            (fun map (delegate_opt, operation, dal_attestation) ->
              match delegate_opt with
              | None -> map
              | Some delegate ->
                  let new_map =
                    Signature.Public_key_hash.Map.add
                      delegate
                      (operation, dal_attestation)
                      map
                  in
                  new_map)
            Signature.Public_key_hash.Map.empty
            attestations
        in
        List.iter_es
          (fun slot_index ->
            let slot_id =
              Types.Slot_id.{slot_index; slot_level = published_level}
            in
            let traps = Store.Traps.find traps_store ~slot_id in
            List.iter_es
              (fun (shard_index, (delegate, share, proof)) ->
                let attestation_opt =
                  Signature.Public_key_hash.Map.find delegate attestation_map
                in
                match attestation_opt with
                | None ->
                    (* It could happen if the delegate didn't attest at all. *)
                    return_unit
                | Some (_, None) ->
                    (* No dal attestation found. *)
                    return_unit
                | Some (attestation, Some dal_attestation) ->
                    if Plugin.is_attested dal_attestation slot_index then
                      let shard = Cryptobox.{index = shard_index; share} in
                      Plugin.inject_entrapment_evidence
                        rpc_ctxt
                        ~attested_level
                        attestation
                        ~slot_index
                        ~shard
                        ~proof
                    else return_unit)
              traps)
          monitored_slot_indices)
