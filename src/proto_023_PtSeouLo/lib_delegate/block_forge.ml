(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type unsigned_block = {
  unsigned_block_header : Block_header.t;
  operations : Tezos_base.Operation.t list list;
  manager_operations_infos : Baking_state.manager_operations_infos option;
}

type simulation_kind =
  | Filter of Operation_pool.Prioritized.t
  | Apply of {
      ordered_pool : Operation_pool.ordered_pool;
      payload_hash : Block_payload_hash.t;
    }

(* [forge_faked_protocol_data ?payload_hash ~payload_round ~seed_nonce_hash
   ~liquidity_baking_toggle_vote] forges a fake [block_header_data] with
   [payload_hash] ([zero] by default), [payload_round], [seed_nonce_hash],
   [liquidity_baking_toggle_vote] and with an empty [proof_of_work_nonce] and a
   dummy [signature]. *)
let forge_faked_protocol_data ?(payload_hash = Block_payload_hash.zero)
    ~payload_round ~seed_nonce_hash ~liquidity_baking_toggle_vote
    ~adaptive_issuance_vote () =
  Block_header.
    {
      contents =
        {
          payload_hash;
          payload_round;
          seed_nonce_hash;
          proof_of_work_nonce = Baking_pow.empty_proof_of_work_nonce;
          per_block_votes =
            {
              liquidity_baking_vote = liquidity_baking_toggle_vote;
              adaptive_issuance_vote;
            };
        };
      signature = Signature.zero;
    }

let convert_operation (op : packed_operation) : Tezos_base.Operation.t =
  {
    shell = op.shell;
    proto =
      Data_encoding.Binary.to_bytes_exn
        Alpha_context.Operation.protocol_data_encoding
        op.protocol_data;
  }

(* [finalize_block_header] updates the [shell_header] that was created
   with dummy fields at the beginning of the block construction. It
   increments the [level] and sets the actual [operations_hash],
   [fitness], [validation_passes], and [context] (the predecessor
   resulting context hash).

   When the operations from the block have been applied, the [fitness]
   is simply retrieved from the [validation_result]. Otherwise, the
   [fitness] is computed from the [round] and [locked_round]
   arguments. *)
let finalize_block_header ~shell_header ~validation_result ~operations_hash
    ~(pred_info : Baking_state_types.block_info) ~pred_resulting_context_hash
    ~round ~locked_round =
  let open Lwt_result_syntax in
  let* fitness =
    match validation_result with
    | Some {Tezos_protocol_environment.fitness; _} -> return fitness
    | None ->
        let*? level =
          Environment.wrap_tzresult @@ Raw_level.of_int32
          @@ Int32.succ shell_header.Tezos_base.Block_header.level
        in
        let*? fitness =
          Environment.wrap_tzresult
          @@ Fitness.create
               ~level
               ~round
               ~predecessor_round:pred_info.round
               ~locked_round
        in
        return (Fitness.to_raw fitness)
  in
  let validation_passes = List.length Main.validation_passes in
  let header =
    Tezos_base.Block_header.
      {
        shell_header with
        level = Int32.succ shell_header.level;
        validation_passes;
        operations_hash;
        fitness;
        context = pred_resulting_context_hash;
      }
  in
  return header

let retain_live_operations_only ~live_blocks operation_pool =
  Operation_pool.Prioritized.filter
    (fun ({shell; _} : packed_operation) ->
      Block_hash.Set.mem shell.branch live_blocks)
    operation_pool

(* [check_protocol_changed] checks whether the protocol will change with the current
   block. This function returns true if the block is the last of an [adoption]
   period. It can also return true if an [user_activated_upgrades] is given. *)
let check_protocol_changed ~user_activated_upgrades ~level
    ~(validation_result : Tezos_protocol_environment.validation_result option)
    ~(incremental : Baking_simulator.incremental) =
  let open Lwt_result_syntax in
  match
    Tezos_base.Block_header.get_forced_protocol_upgrade
      ~user_activated_upgrades
      ~level
  with
  | None -> (
      match validation_result with
      | None -> (
          let context = Validate.get_initial_ctxt (fst incremental.state) in
          let* voting_period =
            Lwt.map
              Environment.wrap_tzresult
              (Voting_period.get_current context)
          in
          match voting_period.kind with
          | Voting_period.Proposal | Exploration | Cooldown | Promotion ->
              return_false
          | Adoption ->
              Lwt.map
                Environment.wrap_tzresult
                (Voting_period.is_last_block context))
      | Some validation_result ->
          let*! next_protocol =
            Context_ops.get_protocol validation_result.context
          in
          return Protocol_hash.(Protocol.hash <> next_protocol))
  | Some next_protocol -> return Protocol_hash.(Protocol.hash <> next_protocol)

(* [filter_via_node] filters operations using
   {!Operation_selection.filter_operations_without_simulation} and then applies
   them in a block via {!Node_rpc.preapply_block}. [filter_via_node] returns a
   [shell_header], the list of operations that have been applied in the block
   and the [payload_hash] corresponding to these operations. *)
let filter_via_node ~chain_id ~fees_config ~hard_gas_limit_per_block
    ~faked_protocol_data ~timestamp ~(pred_info : Baking_state_types.block_info)
    ~payload_round ~operation_pool cctxt =
  let open Lwt_result_syntax in
  let chain = `Hash chain_id in
  let filtered_operations =
    Operation_selection.filter_operations_without_simulation
      fees_config
      ~hard_gas_limit_per_block
      operation_pool
  in
  let* shell_header, preapply_result =
    Node_rpc.preapply_block
      cctxt
      ~chain
      ~head:pred_info.hash
      ~timestamp
      ~protocol_data:faked_protocol_data
      filtered_operations
  in
  (* only retain valid operations *)
  let operations =
    List.map (fun l -> List.map snd l.Preapply_result.applied) preapply_result
  in
  let payload_hash =
    let operation_hashes =
      Stdlib.List.tl operations |> List.flatten
      |> List.map Tezos_base.Operation.hash
    in
    Block_payload.hash
      ~predecessor_hash:shell_header.predecessor
      ~payload_round
      operation_hashes
  in
  let manager_operations_infos =
    None
    (* We do not compute operations infos from node results to avoid potential
       costly computation *)
  in
  return (shell_header, operations, manager_operations_infos, payload_hash)

(* [filter_with_context] filters operations using a local context via
   {!Operation_selection.filter_operations_with_simulation} and a fresh state
   from {!Baking_simulator.begin_construction}. [finalize_block_header] is then
   called and a [shell_header], the list of operations and the corresponding
   [payload_hash] are returned. If the block is a transition block,
   [filter_via_node] is called to return these values. *)
let filter_with_context ~chain_id ~fees_config ~hard_gas_limit_per_block
    ~faked_protocol_data ~user_activated_upgrades ~timestamp
    ~(pred_info : Baking_state_types.block_info) ~pred_resulting_context_hash
    ~force_apply ~round ~context_index ~payload_round ~operation_pool cctxt =
  let open Lwt_result_syntax in
  let* incremental =
    Baking_simulator.begin_construction
      ~timestamp
      ~protocol_data:faked_protocol_data
      ~force_apply
      ~pred_resulting_context_hash
      context_index
      pred_info
      chain_id
  in
  let* {
         Operation_selection.operations;
         validation_result;
         operations_hash;
         manager_operations_infos;
         _;
       } =
    Operation_selection.filter_operations_with_simulation
      incremental
      fees_config
      ~hard_gas_limit_per_block
      operation_pool
  in
  let* changed =
    check_protocol_changed
      ~level:(Int32.succ pred_info.shell.level)
      ~user_activated_upgrades
      ~validation_result
      ~incremental
  in
  if changed then
    (* Fallback to processing via node, which knows both old and new protocol. *)
    filter_via_node
      ~chain_id
      ~fees_config
      ~hard_gas_limit_per_block
      ~faked_protocol_data
      ~timestamp
      ~pred_info
      ~payload_round
      ~operation_pool
      cctxt
  else
    let* shell_header =
      finalize_block_header
        ~shell_header:incremental.header
        ~validation_result
        ~operations_hash
        ~pred_info
        ~pred_resulting_context_hash
        ~round
        ~locked_round:None
    in
    let operations = List.map (List.map convert_operation) operations in
    let payload_hash =
      let operation_hashes =
        Stdlib.List.tl operations |> List.flatten
        |> List.map Tezos_base.Operation.hash
      in
      Block_payload.hash
        ~predecessor_hash:shell_header.predecessor
        ~payload_round
        operation_hashes
    in
    return (shell_header, operations, manager_operations_infos, payload_hash)

(* [apply_via_node] applies already filtered and validated operations in a block
   via {!Node_rpc.preapply_block}. A [shell_header] is recovered from this call
   and returned alongside of the list of operations and the payload_hash. *)
let apply_via_node ~chain_id ~faked_protocol_data ~timestamp
    ~(pred_info : Baking_state_types.block_info) ~ordered_pool ~payload_hash
    cctxt =
  let open Lwt_result_syntax in
  let chain = `Hash chain_id in
  let operations = Operation_pool.ordered_to_list_list ordered_pool in
  let* shell_header, _preapply_result =
    Node_rpc.preapply_block
      cctxt
      ~chain
      ~head:pred_info.hash
      ~timestamp
      ~protocol_data:faked_protocol_data
      operations
  in
  let operations = List.map (List.map convert_operation) operations in
  let manager_operations_infos =
    None
    (* We do not compute operations infos from node results to avoid potential
       costly computation *)
  in
  return (shell_header, operations, manager_operations_infos, payload_hash)

(* [apply_with_context] is similar to [filter_with_context] but filters
   consensus operations only from an [ordered_pool] via
   {!Operation_selection.filter_consensus_operations_only}. *)
let apply_with_context ~chain_id ~faked_protocol_data ~user_activated_upgrades
    ~timestamp ~(pred_info : Baking_state_types.block_info)
    ~pred_resulting_context_hash ~force_apply ~round ~ordered_pool
    ~context_index ~payload_hash cctxt =
  let open Lwt_result_syntax in
  let* incremental =
    Baking_simulator.begin_construction
      ~timestamp
      ~protocol_data:faked_protocol_data
      ~force_apply
      ~pred_resulting_context_hash
      context_index
      pred_info
      chain_id
  in
  (* We still need to filter attestations. Two attestations could be
     referring to the same slot. *)
  let* incremental, ordered_pool =
    Operation_selection.filter_consensus_operations_only
      incremental
      ordered_pool
  in
  let operations = Operation_pool.ordered_to_list_list ordered_pool in
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map
         (fun sl ->
           Operation_list_hash.compute (List.map Operation.hash_packed sl))
         operations)
  in
  (* We need to compute the final [operations_hash] before
     finalizing the block because it will be used in the cache's nonce. *)
  let incremental =
    {incremental with header = {incremental.header with operations_hash}}
  in
  let* validation_result = Baking_simulator.finalize_construction incremental in
  let validation_result = Option.map fst validation_result in
  let* changed =
    check_protocol_changed
      ~level:(Int32.succ pred_info.shell.level)
      ~user_activated_upgrades
      ~validation_result
      ~incremental
  in
  if changed then
    (* Fallback to processing via node, which knows both old and new protocol. *)
    apply_via_node
      ~chain_id
      ~faked_protocol_data
      ~timestamp
      ~pred_info
      ~ordered_pool
      ~payload_hash
      cctxt
  else
    let locked_round_when_no_validation_result =
      (* [locked_round] will not be used in [finalize_block_header] if there is
         a [validation_result] *)
      if Option.is_some validation_result then None
      else
        List.find_map
          (fun {protocol_data = Operation_data protocol_data; _} ->
            match protocol_data.contents with
            | Single (Preattestation {round; _}) -> Some round
            | _ -> None)
          (Option.value (List.hd operations) ~default:[])
    in
    let* shell_header =
      finalize_block_header
        ~shell_header:incremental.header
        ~validation_result
        ~operations_hash
        ~pred_info
        ~pred_resulting_context_hash
        ~round
        ~locked_round:locked_round_when_no_validation_result
    in
    let operations = List.map (List.map convert_operation) operations in
    let manager_operations_infos =
      None
      (* We do not compute operations infos from node results to avoid potential
         costly computation *)
    in
    return (shell_header, operations, manager_operations_infos, payload_hash)

(* [aggregate_preattestations preattestations] constructs a single
   Preattestations_aggregate operation from the given list of [preattestations].

   Each preattestation is assumed to carry a valid BLS signature. All
   preattestations are assumed to target the same branch, level, round, and
   block payload hash. The list is assumed to be conflict-free: i.e., each
   delegate contributes at most one preattestation. *)
let aggregate_preattestations eligible_preattestations =
  let open Result_syntax in
  let aggregate =
    List.fold_left
      (fun acc ({shell; protocol_data} : Kind.preattestation Operation.t) ->
        match (protocol_data.contents, protocol_data.signature) with
        | Single (Preattestation consensus_content), Some (Bls signature) -> (
            let {slot; level; round; block_payload_hash} = consensus_content in
            match acc with
            | None ->
                let consensus_content = {level; round; block_payload_hash} in
                Some (shell, consensus_content, [slot], [signature])
            | Some (shell, consensus_content, committee, signatures) ->
                let committee = slot :: committee in
                let signatures = signature :: signatures in
                Some (shell, consensus_content, committee, signatures))
        | _, _ -> assert false)
      None
      eligible_preattestations
  in
  match aggregate with
  | None -> return_none
  | Some (shell, consensus_content, committee, signatures) -> (
      (* We disable the subgroup check for better performance, as operations
         come from the mempool where it has already been checked. *)
      match
        Signature.Bls.aggregate_signature_opt ~subgroup_check:false signatures
      with
      | Some signature ->
          let contents =
            Single (Preattestations_aggregate {consensus_content; committee})
          in
          let protocol_data = {contents; signature = Some (Bls signature)} in
          return (Some {shell; protocol_data = Operation_data protocol_data})
      | None -> tzfail Baking_errors.Signature_aggregation_failure)

(* [aggregate_attestations attestations] constructs a single
   Attestations_aggregate operation from the given list of [attestations].

   Each attestation is assumed to carry a valid BLS signature. All attestations
   are assumed to target the same branch, level, round, and block payload hash.
   The list is assumed to be conflict-free: i.e., each delegate contributes at
   most one attestation. *)
let aggregate_attestations eligible_attestations =
  let open Result_syntax in
  let aggregate =
    List.fold_left
      (fun acc ({shell; protocol_data} : Kind.attestation Operation.t) ->
        let (Single (Attestation {consensus_content; dal_content})) =
          protocol_data.contents
        in
        let signature =
          match protocol_data.signature with
          | Some (Bls signature) -> signature
          | _ -> assert false
          (* Should not happen because the precondition stipulates
             that eligible attestations only have BLS signatures. *)
        in
        let {slot; level; round; block_payload_hash} = consensus_content in
        match acc with
        | None ->
            let consensus_content = {level; round; block_payload_hash} in
            Some (shell, consensus_content, [(slot, dal_content)], [signature])
        | Some (shell, consensus_content, committee, signatures) ->
            let committee = (slot, dal_content) :: committee in
            let signatures = signature :: signatures in
            Some (shell, consensus_content, committee, signatures))
      None
      eligible_attestations
  in
  match aggregate with
  | None -> return_none
  | Some (shell, consensus_content, committee, signatures) -> (
      (* We disable the subgroup check for better performance, as operations
         come from the mempool where it has already been checked. *)
      match
        Signature.Bls.aggregate_signature_opt ~subgroup_check:false signatures
      with
      | Some signature ->
          let contents =
            Single (Attestations_aggregate {consensus_content; committee})
          in
          let protocol_data = {contents; signature = Some (Bls signature)} in
          return (Some {shell; protocol_data = Operation_data protocol_data})
      | None -> tzfail Baking_errors.Signature_aggregation_failure)

(* [partition_consensus_operations_on_proposal operations] partitions consensus
   operations as follows :
   - a list of tz4 attestations eligible for aggregation
   - a Prioritized_operation_set of remaining operations *)
let partition_consensus_operations_on_proposal consensus_operations =
  let open Operation_pool in
  Prioritized_operation_set.fold
    (fun operation
         ( attestations_aggregate_opt,
           eligible_attestations,
           remaining_operations )
       ->
      let ({shell; protocol_data = Operation_data protocol_data; _} as
           packed_operation) =
        Prioritized_operation.packed operation
      in
      match (protocol_data.contents, protocol_data.signature) with
      | Single (Attestation _), Some (Bls _) ->
          let attestation : Kind.attestation Operation.t =
            {shell; protocol_data}
          in
          let remaining_operations =
            Prioritized_operation_set.remove operation remaining_operations
          in
          ( attestations_aggregate_opt,
            attestation :: eligible_attestations,
            remaining_operations )
      | ( Single (Attestations_aggregate {consensus_content; committee}),
          Some (Bls signature) ) ->
          let attestations_aggregate_opt =
            Some
              (shell, consensus_content, committee, signature, packed_operation)
          in
          let remaining_operations =
            Prioritized_operation_set.remove operation remaining_operations
          in
          ( attestations_aggregate_opt,
            eligible_attestations,
            remaining_operations )
      | _, _ ->
          ( attestations_aggregate_opt,
            eligible_attestations,
            remaining_operations ))
    consensus_operations
    (None, [], consensus_operations)

(* [partition_consensus_operations_on_reproposal consensus_operations] partitions
   [consensus_operations] as follows :
   - an optional Attestations_aggregate
   - an optional Preattestations_aggregate
   - a list of attestations eligible for aggregation
   - a list of preattestations eligible for aggregation
   - a list containing all other remaining consensus operations *)
let partition_consensus_operations_on_reproposal consensus_operations =
  List.fold_left
    (fun ( attestations_aggregate_opt,
           preattestations_aggregate_opt,
           eligible_attestations,
           eligible_preattestations,
           other_operations )
         operation
       ->
      let {shell; protocol_data = Operation_data protocol_data} = operation in
      let {contents; signature} = protocol_data in
      match (contents, signature) with
      | Single (Preattestation _), Some (Bls _) ->
          let op : Kind.preattestation Operation.t = {shell; protocol_data} in
          let eligible_preattestations = op :: eligible_preattestations in
          ( attestations_aggregate_opt,
            preattestations_aggregate_opt,
            eligible_attestations,
            eligible_preattestations,
            other_operations )
      | Single (Attestation _), Some (Bls _) ->
          let op : Kind.attestation Operation.t = {shell; protocol_data} in
          let eligible_attestations = op :: eligible_attestations in
          ( attestations_aggregate_opt,
            preattestations_aggregate_opt,
            eligible_attestations,
            eligible_preattestations,
            other_operations )
      | ( Single (Preattestations_aggregate {consensus_content; committee}),
          Some (Bls signature) ) ->
          let preattestations_aggregate_opt =
            Some (shell, consensus_content, committee, signature, operation)
          in
          ( attestations_aggregate_opt,
            preattestations_aggregate_opt,
            eligible_attestations,
            eligible_preattestations,
            other_operations )
      | ( Single (Attestations_aggregate {consensus_content; committee}),
          Some (Bls signature) ) ->
          let attestations_aggregate_opt =
            Some (shell, consensus_content, committee, signature, operation)
          in
          ( attestations_aggregate_opt,
            preattestations_aggregate_opt,
            eligible_attestations,
            eligible_preattestations,
            other_operations )
      | _ ->
          let other_operations = operation :: other_operations in
          ( attestations_aggregate_opt,
            preattestations_aggregate_opt,
            eligible_attestations,
            eligible_preattestations,
            other_operations ))
    (None, None, [], [], [])
    consensus_operations

module SlotMap : Map.S with type key = Slot.t = Map.Make (Slot)

(* [filter_best_attestations_per_slot] deduplicates a list of attestation
   operations by selecting, for each slot, the attestation with the highest
   number of DAL-attested slots. *)
let filter_best_attestations_per_slot attestations =
  let dal_attested_slot_count = function
    | None -> 0
    | Some dal_content ->
        Dal.Attestation.number_of_attested_slots dal_content.attestation
  in
  let slot_map =
    List.fold_left
      (fun slot_map
           (({protocol_data; _} : Kind.attestation operation) as attestation)
         ->
        let slot, dal_count =
          match protocol_data.contents with
          | Single (Attestation {consensus_content; dal_content}) ->
              (consensus_content.slot, dal_attested_slot_count dal_content)
        in
        SlotMap.update
          slot
          (function
            | None -> Some attestation
            | Some ({protocol_data; _} : Kind.attestation operation) as
              current_value -> (
                match protocol_data.contents with
                | Single (Attestation {dal_content; _}) ->
                    let current_dal_count =
                      dal_attested_slot_count dal_content
                    in
                    if dal_count > current_dal_count then Some attestation
                    else current_value))
          slot_map)
      SlotMap.empty
      attestations
  in
  SlotMap.fold (fun _slot attestation acc -> attestation :: acc) slot_map []

module SlotSet : Set.S with type elt = Slot.t = Set.Make (Slot)

let aggregate_attestations aggregate_opt eligible_attestations =
  let open Result_syntax in
  let eligible_attestations =
    filter_best_attestations_per_slot eligible_attestations
  in
  match (aggregate_opt, eligible_attestations) with
  | None, [] -> return_none
  | None, _ :: _ ->
      (* The proposal did not contain an aggregate. Since additional eligible
         attestations are available, we must aggregate them and include the
         result in the reproposal. *)
      aggregate_attestations eligible_attestations
  | Some (_, _, _, _, operation), [] -> return_some operation
  | Some (shell, consensus_content, committee, signature, _), _ :: _ -> (
      (* The proposal already contains an aggregate.
         We must incorporate additional attestations *)
      let aggregated_slots =
        (* Build the set of aggregated slots for a logarithmic presence lookup *)
        SlotSet.of_list (Operation.committee_slots committee)
      in
      (* Gather slots and signatures incorporating fresh attestations. *)
      let committee, signatures =
        List.fold_left
          (fun ((slots, signatures) as acc)
               ({protocol_data; _} : Kind.attestation operation)
             ->
            match (protocol_data.contents, protocol_data.signature) with
            | ( Single (Attestation {consensus_content; dal_content}),
                Some (Bls signature) )
              when not (SlotSet.mem consensus_content.slot aggregated_slots) ->
                ( (consensus_content.slot, dal_content) :: slots,
                  signature :: signatures )
            | _ -> acc)
          (committee, [signature])
          eligible_attestations
      in
      (* We disable the subgroup check for better performance, as operations
         come from the mempool where it has already been checked. *)
      match
        Signature.Bls.aggregate_signature_opt ~subgroup_check:false signatures
      with
      | Some signature ->
          let contents =
            Single (Attestations_aggregate {consensus_content; committee})
          in
          let protocol_data = {contents; signature = Some (Bls signature)} in
          let attestations_aggregate =
            {shell; protocol_data = Operation_data protocol_data}
          in
          return_some attestations_aggregate
      | None -> tzfail Baking_errors.Signature_aggregation_failure)

(* [aggregate_attestations_on_proposal attestations] replaces all eligible
   attestations from [attestations] by a single Attestations_aggregate.

   All attestations are assumed to target the same branch, level, round and
   block_payload_hash. *)
let aggregate_attestations_on_proposal attestations =
  let open Result_syntax in
  let attestations_aggregate_opt, eligible_attestations, remaining_attestations
      =
    partition_consensus_operations_on_proposal attestations
  in
  let* aggregate_opt =
    aggregate_attestations attestations_aggregate_opt eligible_attestations
  in
  match aggregate_opt with
  | Some aggregate ->
      let open Operation_pool in
      return
      @@ Prioritized_operation_set.add
           (Prioritized_operation.extern ~priority:1 aggregate)
           remaining_attestations
  | None -> return remaining_attestations

let aggregate_preattestations_on_reproposal aggregate_opt
    eligible_preattestations =
  let open Result_syntax in
  match (aggregate_opt, eligible_preattestations) with
  | None, [] -> return_none
  | None, _ :: _ ->
      (* The proposal did not contain an aggregate. Since additional eligible
         preattestations are available, we must aggregate them and include the
         result in the reproposal. *)
      aggregate_preattestations eligible_preattestations
  | Some (_, _, _, _, operation), [] -> return_some operation
  | Some (shell, consensus_content, committee, signature, _), _ :: _ -> (
      (* The proposal already contains an aggregate.
         We must incorporate additional attestations *)
      let aggregated_slots =
        (* Build the set of aggregated slots for a logarithmic presence lookup *)
        SlotSet.of_list committee
      in
      (* Gather slots and signatures incorporating fresh attestations. *)
      let committee, signatures =
        List.fold_left
          (fun ((slots, signatures) as acc)
               ({protocol_data; _} : Kind.preattestation operation)
             ->
            match (protocol_data.contents, protocol_data.signature) with
            | Single (Preattestation consensus_content), Some (Bls signature)
              when not (SlotSet.mem consensus_content.slot aggregated_slots) ->
                (consensus_content.slot :: slots, signature :: signatures)
            | _ -> acc)
          (committee, [signature])
          eligible_preattestations
      in
      (* We disable the subgroup check for better performance, as operations
         come from the mempool where it has already been checked. *)
      match
        Signature.Bls.aggregate_signature_opt ~subgroup_check:false signatures
      with
      | Some signature ->
          let contents =
            Single (Preattestations_aggregate {consensus_content; committee})
          in
          let protocol_data = {contents; signature = Some (Bls signature)} in
          let preattestations_aggregate =
            {shell; protocol_data = Operation_data protocol_data}
          in
          return_some preattestations_aggregate
      | None -> tzfail Baking_errors.Signature_aggregation_failure)

(* [aggregate_consensus_operations_on_reproposal consensus_operations] replaces
   all eligible attestations in [consensus_operations] with a single
   Attestations_aggregate, and all eligible preattestation by a single
   Preattestations_aggregate.

   All operations are assumed to target the same branch, level, round and
   block_payload_hash. *)
let aggregate_consensus_operations_on_reproposal consensus_operations =
  let open Result_syntax in
  let ( attestations_aggregate_opt,
        preattestations_aggregate_opt,
        eligible_attestations,
        eligible_preattestations,
        other_operations ) =
    partition_consensus_operations_on_reproposal consensus_operations
  in
  let* attestations_aggregate_opt =
    aggregate_attestations attestations_aggregate_opt eligible_attestations
  in
  let* preattestations_aggregate_opt =
    aggregate_preattestations_on_reproposal
      preattestations_aggregate_opt
      eligible_preattestations
  in
  match (attestations_aggregate_opt, preattestations_aggregate_opt) with
  | Some attestations_aggregate, Some preattestations_aggregate ->
      return
        (attestations_aggregate :: preattestations_aggregate :: other_operations)
  | Some attestations_aggregate, None ->
      return (attestations_aggregate :: other_operations)
  | None, Some preattestations_aggregate ->
      return (preattestations_aggregate :: other_operations)
  | None, None -> return other_operations

(* [forge] a new [unsigned_block] in accordance with [simulation_kind] and
   [simulation_mode] *)
let forge (cctxt : #Protocol_client_context.full) ~chain_id
    ~(pred_info : Baking_state_types.block_info) ~pred_resulting_context_hash
    ~pred_live_blocks ~timestamp ~round ~liquidity_baking_toggle_vote
    ~adaptive_issuance_vote ~user_activated_upgrades fees_config ~force_apply
    ~seed_nonce_hash ~payload_round simulation_mode simulation_kind constants =
  let open Lwt_result_syntax in
  let hard_gas_limit_per_block =
    constants.Constants.Parametric.hard_gas_limit_per_block
  in
  let* simulation_kind =
    match simulation_kind with
    | Filter operation_pool ->
        (* We cannot include operations that are not live with respect
           to our predecessor otherwise the node would reject the block. *)
        let filtered_pool =
          retain_live_operations_only
            ~live_blocks:pred_live_blocks
            operation_pool
        in
        if constants.aggregate_attestation then
          let*? consensus =
            aggregate_attestations_on_proposal filtered_pool.consensus
          in
          return (Filter {filtered_pool with consensus})
        else return (Filter filtered_pool)
    | Apply {ordered_pool; payload_hash} ->
        if constants.aggregate_attestation then
          let*? consensus =
            aggregate_consensus_operations_on_reproposal ordered_pool.consensus
          in
          let ordered_pool = {ordered_pool with consensus} in
          return (Apply {ordered_pool; payload_hash})
        else return simulation_kind
  in
  let* shell_header, operations, manager_operations_infos, payload_hash =
    match (simulation_mode, simulation_kind) with
    | Baking_state.Node, Filter operation_pool ->
        let faked_protocol_data =
          forge_faked_protocol_data
            ~payload_round
            ~seed_nonce_hash
            ~liquidity_baking_toggle_vote
            ~adaptive_issuance_vote
            ()
        in
        filter_via_node
          ~chain_id
          ~faked_protocol_data
          ~fees_config
          ~hard_gas_limit_per_block
          ~timestamp
          ~pred_info
          ~payload_round
          ~operation_pool
          cctxt
    | Node, Apply {ordered_pool; payload_hash} ->
        let faked_protocol_data =
          forge_faked_protocol_data
            ~payload_hash
            ~payload_round
            ~seed_nonce_hash
            ~liquidity_baking_toggle_vote
            ~adaptive_issuance_vote
            ()
        in
        apply_via_node
          ~chain_id
          ~faked_protocol_data
          ~timestamp
          ~pred_info
          ~ordered_pool
          ~payload_hash
          cctxt
    | Local context_index, Filter operation_pool ->
        let faked_protocol_data =
          forge_faked_protocol_data
            ~payload_round
            ~seed_nonce_hash
            ~liquidity_baking_toggle_vote
            ~adaptive_issuance_vote
            ()
        in
        filter_with_context
          ~chain_id
          ~faked_protocol_data
          ~fees_config
          ~hard_gas_limit_per_block
          ~user_activated_upgrades
          ~timestamp
          ~pred_info
          ~pred_resulting_context_hash
          ~force_apply
          ~round
          ~context_index
          ~payload_round
          ~operation_pool
          cctxt
    | Local context_index, Apply {ordered_pool; payload_hash} ->
        let faked_protocol_data =
          forge_faked_protocol_data
            ~payload_hash
            ~payload_round
            ~seed_nonce_hash
            ~liquidity_baking_toggle_vote
            ~adaptive_issuance_vote
            ()
        in
        apply_with_context
          ~chain_id
          ~faked_protocol_data
          ~user_activated_upgrades
          ~timestamp
          ~pred_info
          ~pred_resulting_context_hash
          ~force_apply
          ~round
          ~ordered_pool
          ~context_index
          ~payload_hash
          cctxt
  in
  let* contents =
    Baking_pow.mine
      ~proof_of_work_threshold:constants.proof_of_work_threshold
      shell_header
      (fun proof_of_work_nonce ->
        {
          Block_header.payload_hash;
          payload_round;
          seed_nonce_hash;
          proof_of_work_nonce;
          per_block_votes =
            {
              liquidity_baking_vote = liquidity_baking_toggle_vote;
              adaptive_issuance_vote;
            };
        })
  in
  let unsigned_block_header =
    {
      Block_header.shell = shell_header;
      protocol_data = {contents; signature = Signature.zero};
    }
  in
  return {unsigned_block_header; operations; manager_operations_infos}
