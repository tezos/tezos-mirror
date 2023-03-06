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

(** Initialize the consensus rights by first slot for modes that are
    about the validation/application of a block: application, partial
    validation, and full construction.

    In these modes, endorsements must point to the predecessor's level
    and preendorsements, if any, to the block's level. *)
let init_consensus_rights_for_block ctxt ~predecessor_level =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let* ctxt, endorsements_map =
    Baking.endorsing_rights_by_first_slot ctxt predecessor_level
  in
  let* ctxt, preendorsements_map =
    Baking.endorsing_rights_by_first_slot ctxt (Level.current ctxt)
  in
  let ctxt =
    Consensus.initialize_consensus_operation
      ctxt
      ~allowed_endorsements:(Some endorsements_map)
      ~allowed_preendorsements:(Some preendorsements_map)
  in
  return ctxt

(** Initialize the consensus rights for a mempool (partial
    construction mode).

    In the mempool, there are three allowed levels for both
    endorsements and preendorsements: [predecessor_level - 1] (aka the
    grandparent's level), [predecessor_level] (that is, the level of
    the mempool's head), and [predecessor_level + 1] (aka the current
    level in ctxt). We don't want to compute the tables by first slot
    for all three possible levels because it is
    time-consuming. However, we want to ensure that the cycle rights
    are loaded in the context, so that {!Stake_distribution.slot_owner}
    doesn't have to initialize them each time it is called (we do this
    now because the context is discarded at the end of the validation
    of each operation, so we can't rely on the caching done by
    [slot_owner] itself). *)
let init_consensus_rights_for_mempool ctxt =
  (* Cycle rights loading is coming in the next commit. *)
  let ctxt =
    Alpha_context.Consensus.initialize_consensus_operation
      ctxt
      ~allowed_endorsements:None
      ~allowed_preendorsements:None
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
        init_consensus_rights_for_block ctxt ~predecessor_level
    | Partial_construction _ -> init_consensus_rights_for_mempool ctxt
  in
  Dal_apply.initialisation ~level:predecessor_level ctxt >>=? fun ctxt ->
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
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in
  let predecessor = block_header.predecessor in
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
    let storage = Alpha_context.Script.lazy_expr storage in
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
  Alpha_context.prepare_first_block
    chain_id
    ~typecheck
    ~level
    ~timestamp
    ~predecessor
    ctxt
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

(* Vanity nonce: TBD *)
