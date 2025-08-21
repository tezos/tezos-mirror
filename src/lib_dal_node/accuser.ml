(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(* [get_attestation_map] retrieves DAL attestation operations from a
   block and transforms them into a map where delegates are mapped to
   their corresponding attestation operation and DAL attestation. *)
let get_attestation_map attestations =
  List.fold_left
    (fun map (tb_slot, delegate_opt, operation, dal_attestation) ->
      match delegate_opt with
      | None -> map
      | Some delegate ->
          Signature.Public_key_hash.Map.add
            delegate
            (operation, dal_attestation, tb_slot)
            map)
    Signature.Public_key_hash.Map.empty
    attestations

(* [filter_injectable_traps] filters a list of traps to identify which
   ones are injectable by checking if each trap's delegate has both an
   attestation and DAL attestation in [attestation_map]. *)
let filter_injectable_traps attestation_map traps =
  List.filter_map
    (fun trap ->
      let Types.{delegate; slot_index; shard; shard_proof} = trap in
      let attestation_opt =
        Signature.Public_key_hash.Map.find delegate attestation_map
      in
      match attestation_opt with
      | None ->
          (* The delegate did not TB attest or we have not found the delegate in
             the attestation operation's receipt. *)
          None
      | Some (_attestation, None, _tb_slot) ->
          (* The delegate did not DAL attest. *)
          None
      | Some (attestation, Some dal_attestation, tb_slot) ->
          Some
            ( delegate,
              slot_index,
              attestation,
              dal_attestation,
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
    (type attestation_operation dal_attestation tb_slot)
    (module Plugin : Dal_plugin.T
      with type attestation_operation = attestation_operation
       and type dal_attestation = dal_attestation
       and type tb_slot = tb_slot) attestations node_ctxt rpc_ctxt
    ~attested_level =
  let open Lwt_result_syntax in
  let*? proto_parameters =
    Node_context.get_proto_parameters node_ctxt ~level:(`Level attested_level)
  in
  when_ proto_parameters.incentives_enable (fun () ->
      let published_level =
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4612
           Correctly compute [published_level] in case of protocol changes, in
           particular a change of the value of [attestation_lag]. *)
        Int32.(sub attested_level (of_int proto_parameters.attestation_lag))
      in
      let store = Node_context.get_store node_ctxt in
      let traps_store = Store.traps store in
      let traps = Store.Traps.find traps_store ~level:published_level in
      match traps with
      | [] -> return_unit
      | traps ->
          let attestation_map = get_attestation_map attestations in
          let traps_to_inject =
            filter_injectable_traps attestation_map traps
            |> (* We do not emit two denunciations for the same level, delegate
                  and slot index, even if 2 shards assigned to this delegate
                  were traps *)
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
          List.iter_es
            (fun ( delegate,
                   slot_index,
                   attestation,
                   dal_attestation,
                   shard,
                   shard_proof,
                   tb_slot ) ->
              if Plugin.is_attested dal_attestation slot_index then
                let*! () =
                  Event.emit_trap_injection
                    ~delegate
                    ~published_level
                    ~attested_level
                    ~shard_index:shard.Cryptobox.index
                    ~slot_index
                in
                let*! res =
                  Plugin.inject_entrapment_evidence
                    rpc_ctxt
                    ~attested_level
                    attestation
                    ~slot_index
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
                        ~shard_index:shard.Cryptobox.index
                        ~error
                    in
                    return_unit
              else return_unit)
            traps_to_inject)
