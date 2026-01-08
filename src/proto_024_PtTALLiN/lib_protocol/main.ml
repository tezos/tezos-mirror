(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* Tezos Protocol Implementation - Protocol Signature Instance *)

type block_header_data = Alpha_context.Block_header.protocol_data

type block_header = Alpha_context.Block_header.t = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

let block_header_data_encoding =
  Alpha_context.Block_header.protocol_data_encoding

type block_header_metadata = Apply_results.block_metadata

let block_header_metadata_encoding = Apply_results.block_metadata_encoding

type operation_data = Alpha_context.packed_protocol_data =
  | Operation_data :
      'kind Alpha_context.Operation.protocol_data
      -> operation_data

let operation_data_encoding = Alpha_context.Operation.protocol_data_encoding

type operation_receipt = Apply_results.packed_operation_metadata =
  | Operation_metadata :
      'kind Apply_results.operation_metadata
      -> operation_receipt
  | No_operation_metadata : operation_receipt

let operation_receipt_encoding = Apply_results.operation_metadata_encoding

let operation_data_and_receipt_encoding =
  Apply_results.operation_data_and_metadata_encoding

type operation = Alpha_context.packed_operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

let acceptable_pass = Alpha_context.Operation.acceptable_pass

let max_block_length = Alpha_context.Block_header.max_header_length

let max_operation_data_length =
  Alpha_context.Constants.max_operation_data_length

let validation_passes =
  let open Alpha_context.Constants in
  Updater.
    [
      (* 2048 attestations *)
      {max_size = 2048 * 2048; max_op = Some 2048};
      (* 32k of voting operations *)
      {max_size = 32 * 1024; max_op = None};
      (* revelations, wallet activations and denunciations *)
      {
        max_size = max_anon_ops_per_block * 1024;
        max_op = Some max_anon_ops_per_block;
      };
      (* 512kB *)
      {max_size = 512 * 1024; max_op = None};
    ]

let rpc_services = RPC_directory.empty

type validation_state = Validate.validation_state

type application_state = Apply.application_state

(** Circumstances and relevant information for [begin_validation] and
    [begin_application] below. *)
type mode =
  | Application of block_header
  | Partial_validation of block_header
  | Construction of {
      predecessor_hash : Block_hash.t;
      timestamp : Time.t;
      block_header_data : block_header_data;
    }
  | Partial_construction of {
      predecessor_hash : Block_hash.t;
      timestamp : Time.t;
    }

let can_contain_preattestations mode =
  let open Result_syntax in
  match mode with
  | Construction _ | Partial_construction _ -> return_true
  | Application block_header | Partial_validation block_header ->
      (* A preexisting block, which has a complete and correct block
         header, can only contain preattestations when the locked
         round in the fitness has an actual value. *)
      let* locked_round =
        Alpha_context.Fitness.locked_round_from_raw block_header.shell.fitness
      in
      return (Option.is_some locked_round)

(** Initialize the consensus rights by first slot for modes that are
    about the validation/application of a block: application, partial
    validation, and full construction.

    In these modes, attestations must point to the predecessor's level
    and preattestations, if any, to the block's level. *)
let init_consensus_rights_for_block ctxt mode ~predecessor_level =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ctxt, attestations_map =
    Baking.attesting_rights_by_first_slot ctxt ~attested_level:predecessor_level
  in
  let*? can_contain_preattestations = can_contain_preattestations mode in
  let* ctxt, allowed_preattestations =
    if can_contain_preattestations then
      let* ctxt, preattestations_map =
        Baking.attesting_rights_by_first_slot
          ctxt
          ~attested_level:(Level.current ctxt)
      in
      return (ctxt, Some preattestations_map)
    else return (ctxt, None)
  in
  let ctxt =
    Consensus.initialize_consensus_operation
      ctxt
      ~allowed_attestations:(Some attestations_map)
      ~allowed_preattestations
      ~allowed_consensus:None
  in
  return ctxt

(** Initialize the consensus rights for a mempool (partial
    construction mode).

    In the mempool, there are three allowed levels for both
    attestations and preattestations: [predecessor_level - 1] (aka the
    grandparent's level), [predecessor_level] (that is, the level of
    the mempool's head), and [predecessor_level + 1] (aka the current
    level in ctxt). *)
let init_consensus_rights_for_mempool ctxt ~predecessor_level =
  let open Lwt_result_syntax in
  let open Alpha_context in
  (* For each allowed level, compute a map associating slots to their owners.
     Only the lowest slot assigned to each delegate is included. *)
  let allowed_levels =
    let levels = [predecessor_level; Level.(succ ctxt predecessor_level)] in
    match Level.pred ctxt predecessor_level with
    | Some grandparent_level -> grandparent_level :: levels
    | None -> levels
  in
  let* ctxt, minimal_slots =
    List.fold_left_es
      (fun (ctxt, minimal_slots) level ->
        let* ctxt, level_minimal_slot =
          Baking.attesting_rights_by_first_slot ctxt ~attested_level:level
        in
        return (ctxt, Level.Map.add level level_minimal_slot minimal_slots))
      (ctxt, Level.Map.empty)
      allowed_levels
  in
  let ctxt =
    (* Store the resulting map in the context as [allowed_consensus]. *)
    Consensus.initialize_consensus_operation
      ctxt
      ~allowed_attestations:None
      ~allowed_preattestations:None
      ~allowed_consensus:(Some minimal_slots)
  in
  return ctxt

let prepare_ctxt ctxt mode ~(predecessor : Block_header.shell_header) =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let level, timestamp =
    match mode with
    | Application block_header | Partial_validation block_header ->
        (block_header.shell.level, block_header.shell.timestamp)
    | Construction {timestamp; _} | Partial_construction {timestamp; _} ->
        (Int32.succ predecessor.level, timestamp)
  in
  let* ctxt, migration_balance_updates, migration_operation_results =
    prepare ctxt ~level ~predecessor_timestamp:predecessor.timestamp ~timestamp
  in
  let*? predecessor_raw_level = Raw_level.of_int32 predecessor.level in
  let predecessor_level = Level.from_raw ctxt predecessor_raw_level in
  let* ctxt = Delegate.prepare_stake_distribution ctxt in
  let* ctxt =
    match mode with
    | Application _ | Partial_validation _ | Construction _ ->
        init_consensus_rights_for_block ctxt mode ~predecessor_level
    | Partial_construction _ ->
        init_consensus_rights_for_mempool ctxt ~predecessor_level
  in
  return
    ( ctxt,
      migration_balance_updates,
      migration_operation_results,
      predecessor_level )

let begin_validation ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ( ctxt,
         _migration_balance_updates,
         _migration_operation_results,
         predecessor_level ) =
    prepare_ctxt ctxt ~predecessor mode
  in
  let predecessor_timestamp = predecessor.timestamp in
  let predecessor_fitness = predecessor.fitness in
  match mode with
  | Application block_header ->
      let*? fitness = Fitness.from_raw block_header.shell.fitness in
      Validate.begin_application
        ctxt
        chain_id
        ~predecessor_level
        ~predecessor_timestamp
        block_header
        fitness
  | Partial_validation block_header ->
      let*? fitness = Fitness.from_raw block_header.shell.fitness in
      Validate.begin_partial_validation
        ctxt
        chain_id
        ~predecessor_level
        ~predecessor_timestamp
        block_header
        fitness
  | Construction {predecessor_hash; timestamp; block_header_data} ->
      let*? predecessor_round = Fitness.round_from_raw predecessor_fitness in
      let*? round =
        Round.round_of_timestamp
          (Constants.round_durations ctxt)
          ~predecessor_timestamp
          ~predecessor_round
          ~timestamp
      in
      Validate.begin_full_construction
        ctxt
        chain_id
        ~predecessor_level
        ~predecessor_round
        ~predecessor_timestamp
        ~predecessor_hash
        round
        block_header_data.contents
  | Partial_construction _ ->
      let*? predecessor_round = Fitness.round_from_raw predecessor_fitness in
      return
        (Validate.begin_partial_construction
           ctxt
           chain_id
           ~predecessor_level
           ~predecessor_round)

let validate_operation = Validate.validate_operation

let finalize_validation = Validate.finalize_block

type error += Cannot_apply_in_partial_validation

let () =
  register_error_kind
    `Permanent
    ~id:"main.begin_application.cannot_apply_in_partial_validation"
    ~title:"cannot_apply_in_partial_validation"
    ~description:
      "Cannot instantiate an application state using the 'Partial_validation' \
       mode."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot instantiate an application state using the \
         'Partial_validation' mode.")
    Data_encoding.(empty)
    (function Cannot_apply_in_partial_validation -> Some () | _ -> None)
    (fun () -> Cannot_apply_in_partial_validation)

let begin_application ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ( ctxt,
         migration_balance_updates,
         migration_operation_results,
         predecessor_level ) =
    prepare_ctxt ctxt ~predecessor mode
  in
  let predecessor_timestamp = predecessor.timestamp in
  let predecessor_fitness = predecessor.fitness in
  match mode with
  | Application block_header ->
      Apply.begin_application
        ctxt
        chain_id
        ~migration_balance_updates
        ~migration_operation_results
        ~predecessor_fitness
        block_header
  | Partial_validation _ -> tzfail Cannot_apply_in_partial_validation
  | Construction {predecessor_hash; timestamp; block_header_data; _} ->
      let*? predecessor_round = Fitness.round_from_raw predecessor_fitness in
      Apply.begin_full_construction
        ctxt
        chain_id
        ~migration_balance_updates
        ~migration_operation_results
        ~predecessor_timestamp
        ~predecessor_level
        ~predecessor_round
        ~predecessor_hash
        ~timestamp
        block_header_data.contents
  | Partial_construction {predecessor_hash; _} ->
      Apply.begin_partial_construction
        ctxt
        chain_id
        ~migration_balance_updates
        ~migration_operation_results
        ~predecessor_hash
        ~predecessor_fitness

let apply_operation = Apply.apply_operation

let finalize_application = Apply.finalize_block

let compare_operations (oph1, op1) (oph2, op2) =
  Alpha_context.Operation.compare (oph1, op1) (oph2, op2)

let init chain_id ctxt block_header =
  let open Lwt_result_syntax in
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in
  let predecessor = block_header.predecessor in
  let typecheck_smart_contract (ctxt : Alpha_context.context)
      (script : Alpha_context.Script.t) =
    let allow_forged_tickets_in_storage, allow_forged_lazy_storage_id_in_storage
        =
      (false, false)
      (* There should be no forged value in bootstrap contracts. *)
    in
    let* Ex_script (Script parsed_script), ctxt =
      Script_ir_translator.parse_script
        ctxt
        ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
        ~allow_forged_tickets_in_storage
        ~allow_forged_lazy_storage_id_in_storage
        script
    in
    let* storage, lazy_storage_diff, ctxt =
      Script_ir_translator.extract_lazy_storage_diff
        ctxt
        Optimized
        parsed_script.storage_type
        parsed_script.storage
        ~to_duplicate:Script_ir_translator.no_lazy_storage_id
        ~to_update:Script_ir_translator.no_lazy_storage_id
        ~temporary:false
    in
    let+ storage, ctxt =
      Script_ir_translator.unparse_data
        ctxt
        Optimized
        parsed_script.storage_type
        storage
    in
    let storage = Alpha_context.Script.lazy_expr storage in
    (({script with storage}, lazy_storage_diff), ctxt)
  in
  (* The cache must be synced at the end of block validation, so we do
     so here for the first block in a protocol where `finalize_block`
     is not called. *)
  let*? raw_level = Alpha_context.Raw_level.of_int32 level in
  let init_fitness =
    Alpha_context.Fitness.create_without_locked_round
      ~level:raw_level
      ~round:Alpha_context.Round.zero
      ~predecessor_round:Alpha_context.Round.zero
  in
  let* ctxt =
    Alpha_context.prepare_first_block
      chain_id
      ~typecheck_smart_contract
      ~typecheck_smart_rollup:
        Sc_rollup_operations.validate_untyped_parameters_ty
      ~level
      ~timestamp
      ~predecessor
      ctxt
  in
  let cache_nonce =
    Alpha_context.Cache.cache_nonce_from_block_header
      block_header
      ({
         payload_hash = Block_payload_hash.zero;
         payload_round = Alpha_context.Round.zero;
         per_block_votes =
           {
             liquidity_baking_vote =
               Alpha_context.Per_block_votes.Per_block_vote_pass;
           };
         seed_nonce_hash = None;
         proof_of_work_nonce =
           Bytes.make Constants_repr.proof_of_work_nonce_size '0';
       }
        : Alpha_context.Block_header.contents)
  in
  let*! ctxt = Alpha_context.Cache.Admin.sync ctxt cache_nonce in
  return
    (Alpha_context.finalize ctxt (Alpha_context.Fitness.to_raw init_fitness))

let value_of_key ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp
    ~predecessor_level:pred_level ~predecessor_fitness:_ ~predecessor:_
    ~timestamp =
  let open Lwt_result_syntax in
  let level = Int32.succ pred_level in
  let* ctxt, _, _ =
    Alpha_context.prepare ctxt ~level ~predecessor_timestamp ~timestamp
  in
  return (Apply.value_of_key ctxt)

module Mempool = struct
  include Mempool_validation

  let init ctxt chain_id ~head_hash ~(head : Block_header.shell_header) =
    let open Lwt_result_syntax in
    let open Alpha_context in
    let* ( ctxt,
           _migration_balance_updates,
           _migration_operation_results,
           head_level ) =
      (* We use Partial_construction to factorize the [prepare_ctxt]. *)
      prepare_ctxt
        ctxt
        (Partial_construction
           {predecessor_hash = head_hash; timestamp = head.timestamp})
        ~predecessor:head
    in
    let*? predecessor_round = Fitness.round_from_raw head.fitness in
    return
      (init
         ctxt
         chain_id
         ~predecessor_level:head_level
         ~predecessor_round
         ~predecessor_hash:head_hash)
end

(* Vanity nonce: 3018338012077776 *)
