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
      (* 2048 endorsements *)
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

let rpc_services =
  Alpha_services.register () ;
  Services_registration.get_rpc_services ()

type validation_state = {
  validity_state : Validate.validation_state;
  application_state : Apply.application_state;
}

let prepare_context ctxt ~level ~predecessor_timestamp ~timestamp =
  Alpha_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt

let init_allowed_consensus_operations ctxt ~endorsement_level
    ~preendorsement_level =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ctxt = Delegate.prepare_stake_distribution ctxt in
  let* ctxt, allowed_endorsements, allowed_preendorsements =
    if Level.(endorsement_level = preendorsement_level) then
      let* ctxt, slots =
        Baking.endorsing_rights_by_first_slot ctxt endorsement_level
      in
      let consensus_operations = slots in
      return (ctxt, consensus_operations, consensus_operations)
    else
      let* ctxt, endorsements =
        Baking.endorsing_rights_by_first_slot ctxt endorsement_level
      in
      let* ctxt, preendorsements =
        Baking.endorsing_rights_by_first_slot ctxt preendorsement_level
      in
      return (ctxt, endorsements, preendorsements)
  in
  let ctxt =
    Consensus.initialize_consensus_operation
      ctxt
      ~allowed_endorsements
      ~allowed_preendorsements
  in
  return ctxt

let begin_application ~chain_id ~predecessor_context ~predecessor_timestamp
    ~(predecessor_fitness : Fitness.t)
    (block_header : Alpha_context.Block_header.t) =
  let open Lwt_tzresult_syntax in
  let open Alpha_context in
  let* ctxt, migration_balance_updates, migration_operation_results =
    prepare_context
      predecessor_context
      ~level:block_header.shell.level
      ~predecessor_timestamp
      ~timestamp:block_header.shell.timestamp
  in
  let*? predecessor_level =
    Alpha_context.Raw_level.of_int32 (Int32.pred block_header.shell.level)
  in
  let predecessor_level = Alpha_context.Level.from_raw ctxt predecessor_level in
  let current_level = Level.current ctxt in
  let* ctxt =
    init_allowed_consensus_operations
      ctxt
      ~endorsement_level:predecessor_level
      ~preendorsement_level:current_level
  in
  let*? fitness = Alpha_context.Fitness.from_raw block_header.shell.fitness in
  let* validity_state =
    Validate.begin_application
      ctxt
      chain_id
      ~predecessor_level
      ~predecessor_timestamp
      block_header
      fitness
  in
  let* application_state =
    Apply.begin_application
      ctxt
      chain_id
      ~migration_balance_updates
      ~migration_operation_results
      ~predecessor_fitness
      (block_header : Alpha_context.Block_header.t)
  in
  return {validity_state; application_state}

let begin_partial_application ~chain_id ~ancestor_context ~predecessor_timestamp
    ~(predecessor_fitness : Fitness.t)
    (block_header : Alpha_context.Block_header.t) =
  let open Lwt_tzresult_syntax in
  let open Alpha_context in
  let* ancestor_context, migration_balance_updates, migration_operation_results
      =
    prepare_context
      ancestor_context
      ~level:block_header.shell.level
      ~predecessor_timestamp
      ~timestamp:block_header.shell.timestamp
  in
  let*? predecessor_level =
    Raw_level.of_int32 (Int32.pred block_header.shell.level)
  in
  let predecessor_level = Level.from_raw ancestor_context predecessor_level in
  let current_level = Level.current ancestor_context in
  let* ancestor_context =
    init_allowed_consensus_operations
      ancestor_context
      ~endorsement_level:predecessor_level
      ~preendorsement_level:current_level
  in
  let*? fitness = Fitness.from_raw block_header.shell.fitness in
  let* validity_state =
    Validate.begin_partial_application
      ~ancestor_context
      chain_id
      ~predecessor_level
      ~predecessor_timestamp
      block_header
      fitness
  in
  let* application_state =
    Apply.begin_partial_application
      chain_id
      ~ancestor_context
      ~migration_balance_updates
      ~migration_operation_results
      ~predecessor_fitness
      block_header
  in
  return {validity_state; application_state}

let begin_full_construction ~chain_id ~predecessor_context
    ~predecessor_timestamp ~predecessor_level ~(predecessor_fitness : Fitness.t)
    ~predecessor ~timestamp
    (block_header_contents : Alpha_context.Block_header.contents) =
  let open Lwt_tzresult_syntax in
  let open Alpha_context in
  let level = Int32.succ predecessor_level in
  let* ctxt, migration_balance_updates, migration_operation_results =
    prepare_context ~level ~predecessor_timestamp ~timestamp predecessor_context
  in
  let*? predecessor_level = Raw_level.of_int32 predecessor_level in
  let predecessor_level = Level.from_raw ctxt predecessor_level in
  let current_level = Level.current ctxt in
  let* ctxt =
    init_allowed_consensus_operations
      ctxt
      ~endorsement_level:predecessor_level
      ~preendorsement_level:current_level
  in
  let round_durations = Constants.round_durations ctxt in
  let*? predecessor_round = Fitness.round_from_raw predecessor_fitness in
  let*? round =
    Round.round_of_timestamp
      round_durations
      ~predecessor_timestamp
      ~predecessor_round
      ~timestamp
  in
  let* validity_state =
    Validate.begin_full_construction
      ctxt
      chain_id
      ~predecessor_level
      ~predecessor_round
      ~predecessor_timestamp
      ~predecessor_hash:predecessor
      round
      block_header_contents
  in
  let* application_state =
    Apply.begin_full_construction
      ctxt
      chain_id
      ~migration_balance_updates
      ~migration_operation_results
      ~predecessor_timestamp
      ~predecessor_level
      ~predecessor_round
      ~predecessor
      ~timestamp
      block_header_contents
  in
  return {validity_state; application_state}

let begin_partial_construction ~chain_id ~predecessor_context
    ~predecessor_timestamp ~predecessor_level ~predecessor_fitness ~predecessor
    ~timestamp =
  let open Lwt_tzresult_syntax in
  let open Alpha_context in
  let level = Int32.succ predecessor_level in
  let* ctxt, migration_balance_updates, migration_operation_results =
    prepare ~level ~predecessor_timestamp ~timestamp predecessor_context
  in
  let*? predecessor_raw_level = Raw_level.of_int32 predecessor_level in
  let predecessor_level = Level.from_raw ctxt predecessor_raw_level in
  (* In the mempool, only consensus operations for [predecessor_level]
     (that is, head's level) are allowed, contrary to block validation
     where endorsements are for the previous level and
     preendorsements, if any, for the block's level. *)
  let* ctxt =
    init_allowed_consensus_operations
      ctxt
      ~endorsement_level:predecessor_level
      ~preendorsement_level:predecessor_level
  in
  let*? predecessor_round = Fitness.round_from_raw predecessor_fitness in
  let*? grandparent_round =
    Alpha_context.Fitness.predecessor_round_from_raw predecessor_fitness
  in
  let validity_state =
    Validate.begin_partial_construction
      ctxt
      chain_id
      ~predecessor_level
      ~predecessor_round
      ~predecessor_hash:predecessor
      ~grandparent_round
  in
  let* application_state =
    Apply.begin_partial_construction
      ctxt
      chain_id
      ~migration_balance_updates
      ~migration_operation_results
      ~predecessor_level:predecessor_raw_level
      ~predecessor_fitness
  in
  return {validity_state; application_state}

(* Updater's signature compliant function *)
let begin_construction ~chain_id ~predecessor_context ~predecessor_timestamp
    ~predecessor_level ~(predecessor_fitness : Fitness.t) ~predecessor
    ~timestamp ?(protocol_data : block_header_data option) () =
  match protocol_data with
  | None ->
      begin_partial_construction
        ~chain_id
        ~predecessor_context
        ~predecessor_timestamp
        ~predecessor_level
        ~predecessor_fitness
        ~predecessor
        ~timestamp
  | Some protocol_data ->
      begin_full_construction
        ~chain_id
        ~predecessor_context
        ~predecessor_timestamp
        ~predecessor_level
        ~predecessor_fitness
        ~predecessor
        ~timestamp
        protocol_data.contents

let validate_operation validity_state
    (packed_operation : Alpha_context.packed_operation) =
  let {shell; protocol_data = Operation_data protocol_data} =
    packed_operation
  in
  let operation : _ Alpha_context.operation = {shell; protocol_data} in
  let oph = Alpha_context.Operation.hash operation in
  Validate.validate_operation validity_state oph packed_operation

let apply_operation (state : validation_state)
    (packed_operation : Alpha_context.packed_operation) =
  let open Lwt_result_syntax in
  let* validation_state =
    validate_operation state.validity_state packed_operation
  in
  let operation_hash = Alpha_context.Operation.hash_packed packed_operation in
  let* application_state, operation_receipt =
    Apply.apply_operation
      state.application_state
      operation_hash
      packed_operation
  in
  return
    ({validity_state = validation_state; application_state}, operation_receipt)

let finalize_block state shell_header =
  let open Lwt_result_syntax in
  let* () = Validate.finalize_block state.validity_state in
  Apply.finalize_block state.application_state shell_header

let compare_operations (oph1, op1) (oph2, op2) =
  Alpha_context.Operation.compare (oph1, op1) (oph2, op2)

let init chain_id ctxt block_header =
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in
  let typecheck (ctxt : Alpha_context.context) (script : Alpha_context.Script.t)
      =
    let allow_forged_in_storage =
      false
      (* There should be no forged value in bootstrap contracts. *)
    in
    Script_ir_translator.parse_script
      ctxt
      ~elab_conf:Script_ir_translator_config.(make ~legacy:true ())
      ~allow_forged_in_storage
      script
    >>=? fun (Ex_script (Script parsed_script), ctxt) ->
    Script_ir_translator.extract_lazy_storage_diff
      ctxt
      Optimized
      parsed_script.storage_type
      parsed_script.storage
      ~to_duplicate:Script_ir_translator.no_lazy_storage_id
      ~to_update:Script_ir_translator.no_lazy_storage_id
      ~temporary:false
    >>=? fun (storage, lazy_storage_diff, ctxt) ->
    Script_ir_translator.unparse_data
      ctxt
      Optimized
      parsed_script.storage_type
      storage
    >|=? fun (storage, ctxt) ->
    let storage =
      Alpha_context.Script.lazy_expr (Micheline.strip_locations storage)
    in
    (({script with storage}, lazy_storage_diff), ctxt)
  in
  (* The cache must be synced at the end of block validation, so we do
     so here for the first block in a protocol where `finalize_block`
     is not called. *)
  Alpha_context.Raw_level.of_int32 level >>?= fun raw_level ->
  let init_fitness =
    Alpha_context.Fitness.create_without_locked_round
      ~level:raw_level
      ~round:Alpha_context.Round.zero
      ~predecessor_round:Alpha_context.Round.zero
  in
  Alpha_context.prepare_first_block chain_id ~typecheck ~level ~timestamp ctxt
  >>=? fun ctxt ->
  let cache_nonce =
    Alpha_context.Cache.cache_nonce_from_block_header
      block_header
      ({
         payload_hash = Block_payload_hash.zero;
         payload_round = Alpha_context.Round.zero;
         liquidity_baking_toggle_vote = Alpha_context.Liquidity_baking.LB_pass;
         seed_nonce_hash = None;
         proof_of_work_nonce =
           Bytes.make Constants_repr.proof_of_work_nonce_size '0';
       }
        : Alpha_context.Block_header.contents)
  in
  Alpha_context.Cache.Admin.sync ctxt cache_nonce >>= fun ctxt ->
  return
    (Alpha_context.finalize ctxt (Alpha_context.Fitness.to_raw init_fitness))

let value_of_key ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp
    ~predecessor_level:pred_level ~predecessor_fitness:_ ~predecessor:_
    ~timestamp =
  let level = Int32.succ pred_level in
  Alpha_context.prepare ctxt ~level ~predecessor_timestamp ~timestamp
  >>=? fun (ctxt, _, _) -> return (Apply.value_of_key ctxt)

(* Vanity nonce: TBD *)
