(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(* [get_attestation_map] retrieves DAL attestation operations from a
   block and transforms them into a map where delegates are mapped to
   their corresponding attestation operation and DAL attestation. *)
let get_attestation_map (type block_info attestation_operation dal_attestation)
    (module Plugin : Dal_plugin.T
      with type block_info = block_info
       and type attestation_operation = attestation_operation
       and type dal_attestation = dal_attestation) block =
  let attestations = Plugin.get_attestations block in
  List.fold_left
    (fun map (_tb_slot, delegate_opt, operation, dal_attestation) ->
      match delegate_opt with
      | None -> map
      | Some delegate ->
          Signature.Public_key_hash.Map.add
            delegate
            (operation, dal_attestation)
            map)
    Signature.Public_key_hash.Map.empty
    attestations

(* [filter_injectable_traps] filters a list of traps to identify which
   ones are injectable by checking if each trap's delegate has both an
   attestation and DAL attestation in [attestation_map]. *)
let filter_injectable_traps ~attested_level ~published_level attestation_map
    traps =
  let open Lwt_result_syntax in
  List.filter_map_s
    (fun trap ->
      let Store.Traps.{delegate; slot_index; shard; shard_proof} = trap in
      let attestation_opt =
        Signature.Public_key_hash.Map.find delegate attestation_map
      in
      match attestation_opt with
      | None ->
          let*! () =
            Event.emit_trap_delegate_attestation_not_found
              ~delegate
              ~slot_index
              ~shard_index:shard.Cryptobox.index
              ~published_level
              ~attested_level
          in
          Lwt.return_none
      | Some (_attestation, None) ->
          (* The delegate did not DAL attest at all. *)
          Lwt.return_none
      | Some (attestation, Some dal_attestation) ->
          Lwt.return_some
            ( delegate,
              slot_index,
              attestation,
              dal_attestation,
              shard,
              shard_proof ))
    traps

(* [inject_entrapment_evidences] processes and injects trap evidence
   retrieving traps from a specific published level, filtering them to
   identify injectable ones, and then injecting entrapment evidence
   for each injectable trap that the delegate actually attested.

   Guarded by [proto_parameters.incentives_enable].
*)
let inject_entrapment_evidences (type block_info)
    (module Plugin : Dal_plugin.T with type block_info = block_info) node_ctxt
    rpc_ctxt block =
  let open Lwt_result_syntax in
  let attested_level = (Plugin.block_shell_header block).level in
  let*? proto_parameters =
    Node_context.get_proto_parameters node_ctxt ~level:attested_level
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
          let attestation_map = get_attestation_map (module Plugin) block in
          let*! traps_to_inject =
            filter_injectable_traps
              ~attested_level
              ~published_level
              attestation_map
              traps
          in
          List.iter_es
            (fun ( delegate,
                   slot_index,
                   attestation,
                   dal_attestation,
                   shard,
                   shard_proof ) ->
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
