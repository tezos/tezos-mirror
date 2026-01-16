(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(* [get_attestation_map] transforms DAL attestation operations from a block into
   a map where delegates are mapped to their corresponding attestation operation
   and DAL attestations. *)
let get_attestation_map attestations slot_to_committee_pkh tb_slot_to_int =
  List.fold_left
    (fun map (tb_slot, operation, dal_attestations) ->
      match
        List.find
          (fun (v, _) -> v = tb_slot_to_int tb_slot)
          slot_to_committee_pkh
      with
      | None -> map
      | Some (_, (delegate, _)) ->
          Signature.Public_key_hash.Map.add
            delegate
            (operation, dal_attestations, tb_slot)
            map)
    Signature.Public_key_hash.Map.empty
    attestations

(* [filter_injectable_traps] filters a list of traps to identify which
   ones are injectable by checking if each trap's delegate has both an
   attestation and DAL attestation in [attestation_map]. *)
let filter_injectable_traps attestations_map traps =
  List.filter_map
    (fun trap ->
      let Types.{delegate; slot_index; shard; shard_proof} = trap in
      let attestations_opt =
        Signature.Public_key_hash.Map.find delegate attestations_map
      in
      match attestations_opt with
      | None ->
          (* The delegate did not TB attest or we have not found the delegate in
             the attestation operation's receipt. *)
          None
      | Some (_attestation, None, _tb_slot) ->
          (* The delegate did not DAL attest. *)
          None
      | Some (attestation, Some dal_attestations, tb_slot) ->
          Some
            ( delegate,
              slot_index,
              attestation,
              dal_attestations,
              shard,
              shard_proof,
              tb_slot ))
    traps

(* [inject_entrapment_evidences] processes and injects trap evidence
   retrieving traps from a specific published level, filtering them to
   identify injectable ones, and then injecting entrapment evidence
   for each injectable trap that the delegate actually attested.

   Guarded by [proto_parameters.incentives_enable].
*)
let inject_entrapment_evidences
    (type attestation_operation dal_attestations tb_slot)
    (module Plugin : Dal_plugin.T
      with type attestation_operation = attestation_operation
       and type dal_attestations = dal_attestations
       and type tb_slot = tb_slot) attestations slot_to_committee_pkh node_ctxt
    rpc_ctxt ~attested_level tb_slot_to_int =
  let open Lwt_result_syntax in
  let*? proto_parameters =
    Node_context.get_proto_parameters node_ctxt ~level:(`Level attested_level)
  in
  when_ proto_parameters.incentives_enable (fun () ->
      let published_level =
        (* In case a protocol changes the value of [attestation_lag], the
           computed [published_level] is wrong just after the
           migration. However, since no slots are considered protocol-attested
           in this period, no entrapment would be injected ([Plugin.is_attested]
           returns [false]). *)
        Int32.(sub attested_level (of_int proto_parameters.attestation_lag))
      in
      let store = Node_context.get_store node_ctxt in
      let traps_store = Store.traps store in
      let traps = Store.Traps.find traps_store ~level:published_level in
      match traps with
      | [] -> return_unit
      | traps ->
          let attestation_map =
            get_attestation_map
              attestations
              slot_to_committee_pkh
              tb_slot_to_int
          in
          let traps_to_inject =
            filter_injectable_traps attestation_map traps
            |>
            (* We do not emit two denunciations for the same level, delegate and
               slot index, even if 2 shards assigned to this delegate were
               traps. *)
            List.sort_uniq
              (fun
                (delegate1, slot_index1, _, _, _, _, _)
                (delegate2, slot_index2, _, _, _, _, _)
              ->
                let deleg_comp =
                  Signature.Public_key_hash.compare delegate1 delegate2
                in
                if deleg_comp = 0 then compare slot_index1 slot_index2
                else deleg_comp)
          in
          let*? () = Node_context.assert_single_lag node_ctxt in
          List.iter_es
            (fun ( delegate,
                   slot_index,
                   attestation,
                   dal_attestation,
                   shard,
                   shard_proof,
                   tb_slot )
               ->
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/8218
                 Checks for traps for each lag *)
              let number_of_lags =
                List.length proto_parameters.attestation_lags
              in
              if
                Plugin.is_baker_attested
                  dal_attestation
                  ~number_of_slots:proto_parameters.number_of_slots
                  ~number_of_lags
                  ~lag_index:(number_of_lags - 1)
                  slot_index
              then
                let*! () =
                  Event.emit_trap_injection
                    ~delegate
                    ~published_level
                    ~attested_level
                    ~shard_index:shard.Cryptobox.index
                      (* TODO: https://gitlab.com/tezos/tezos/-/issues/8218
                         Add [~lag_index] *)
                    ~slot_index
                in
                let*! res =
                  Plugin.inject_entrapment_evidence
                    rpc_ctxt
                    ~attested_level
                    attestation
                    ~slot_index
                    ~lag_index:(number_of_lags - 1)
                    ~shard
                    ~proof:shard_proof
                    ~tb_slot
                in
                match res with
                | Ok () -> return_unit
                | Error error ->
                    let*! () =
                      Event.emit_trap_injection_failure
                        ~delegate
                        ~published_level
                        ~attested_level
                        ~slot_index
                          (* TODO: https://gitlab.com/tezos/tezos/-/issues/8218
                             Add [~lag_index] *)
                        ~shard_index:shard.Cryptobox.index
                        ~error
                    in
                    return_unit
              else return_unit)
            traps_to_inject)
