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

let acceptable_passes = Alpha_context.Operation.acceptable_passes

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

type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t;
      fitness : Alpha_context.Fitness.t;
      payload_producer : Alpha_context.public_key_hash;
      block_producer : Alpha_context.public_key_hash;
      predecessor_round : Alpha_context.Round.t;
      predecessor_level : Alpha_context.Level.t;
    }
  | Partial_application of {
      block_header : Alpha_context.Block_header.t;
      fitness : Alpha_context.Fitness.t;
      payload_producer : Alpha_context.public_key_hash;
      block_producer : Alpha_context.public_key_hash;
      predecessor_level : Alpha_context.Level.t;
      predecessor_round : Alpha_context.Round.t;
    }
  (* Mempool only *)
  | Partial_construction of {
      predecessor : Block_hash.t;
      predecessor_fitness : Fitness.t;
      predecessor_level : Alpha_context.Level.t;
      predecessor_round : Alpha_context.Round.t;
    }
  (* Baker only *)
  | Full_construction of {
      predecessor : Block_hash.t;
      payload_producer : Alpha_context.public_key_hash;
      block_producer : Alpha_context.public_key_hash;
      protocol_data_contents : Alpha_context.Block_header.contents;
      level : Int32.t;
      round : Alpha_context.Round.t;
      predecessor_level : Alpha_context.Level.t;
      predecessor_round : Alpha_context.Round.t;
    }

type validation_state = {
  mode : validation_mode;
  chain_id : Chain_id.t;
  ctxt : Alpha_context.t;
  op_count : int;
  migration_balance_updates : Alpha_context.Receipt.balance_updates;
  liquidity_baking_escape_ema : Int32.t;
  implicit_operations_results :
    Apply_results.packed_successful_manager_operation_result list;
}

let cache_layout = Apply.cache_layout

let begin_partial_application ~chain_id ~ancestor_context:ctxt
    ~predecessor_timestamp ~(predecessor_fitness : Fitness.t)
    (block_header : Alpha_context.Block_header.t) =
  (* Note: we don't have access to the predecessor context. *)
  let level = block_header.shell.level in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.Fitness.from_raw block_header.shell.fitness >>?= fun fitness ->
  Alpha_context.Fitness.round_from_raw predecessor_fitness
  >>?= fun predecessor_round ->
  Alpha_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  >>=? fun (ctxt, migration_balance_updates, migration_operation_results) ->
  Alpha_context.Raw_level.of_int32 (Int32.pred level)
  >>?= fun predecessor_level ->
  let predecessor_level =
    Alpha_context.Level.(from_raw ctxt predecessor_level)
  in
  Apply.begin_application
    ctxt
    chain_id
    block_header
    fitness
    ~predecessor_timestamp
    ~predecessor_level
    ~predecessor_round
  >>=? fun ( ctxt,
             payload_producer_pk,
             block_producer,
             liquidity_baking_operations_results,
             liquidity_baking_escape_ema ) ->
  let mode =
    Partial_application
      {
        block_header;
        fitness;
        predecessor_level;
        predecessor_round;
        payload_producer = Signature.Public_key.hash payload_producer_pk;
        block_producer;
      }
  in
  return
    {
      mode;
      chain_id;
      ctxt;
      op_count = 0;
      migration_balance_updates;
      liquidity_baking_escape_ema;
      implicit_operations_results =
        Apply_results.pack_migration_operation_results
          migration_operation_results
        @ liquidity_baking_operations_results;
    }

(* During applications the valid consensus operations are:
 * Endorsements on previous block with the right round, level, payload_hash (of the predecessor block)
 * Preendorsements on current level, previous round, and the payload_hash of the current block
   Those endorsements justify that the previous block was finalized.
   Those preendorsements justify the locked_round part of the fitness of the current block
 *)
let begin_application ~chain_id ~predecessor_context:ctxt ~predecessor_timestamp
    ~predecessor_fitness (block_header : Alpha_context.Block_header.t) =
  let level = block_header.shell.level in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.Fitness.from_raw block_header.shell.fitness >>?= fun fitness ->
  Alpha_context.Fitness.round_from_raw predecessor_fitness
  >>?= fun predecessor_round ->
  Alpha_context.Raw_level.of_int32 (Int32.pred level)
  >>?= fun predecessor_level ->
  Alpha_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  >>=? fun (ctxt, migration_balance_updates, migration_operation_results) ->
  let predecessor_level = Alpha_context.Level.from_raw ctxt predecessor_level in
  Apply.begin_application
    ctxt
    chain_id
    block_header
    fitness
    ~predecessor_timestamp
    ~predecessor_level
    ~predecessor_round
  >>=? fun ( ctxt,
             payload_producer,
             block_producer,
             liquidity_baking_operations_results,
             liquidity_baking_escape_ema ) ->
  let mode =
    Application
      {
        block_header;
        fitness;
        predecessor_round;
        predecessor_level;
        payload_producer = Signature.Public_key.hash payload_producer;
        block_producer;
      }
  in
  return
    {
      mode;
      chain_id;
      ctxt;
      op_count = 0;
      migration_balance_updates;
      liquidity_baking_escape_ema;
      implicit_operations_results =
        Apply_results.pack_migration_operation_results
          migration_operation_results
        @ liquidity_baking_operations_results;
    }

let begin_construction ~chain_id ~predecessor_context:ctxt
    ~predecessor_timestamp ~predecessor_level ~predecessor_fitness ~predecessor
    ~timestamp ?(protocol_data : block_header_data option) () =
  let level = Int32.succ predecessor_level in
  Alpha_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  >>=? fun (ctxt, migration_balance_updates, migration_operation_results) ->
  Alpha_context.Raw_level.of_int32 predecessor_level
  >>?= fun predecessor_level ->
  let predecessor_level =
    Alpha_context.Level.(from_raw ctxt predecessor_level)
  in
  (match protocol_data with
  | None ->
      Alpha_context.Fitness.round_from_raw predecessor_fitness
      >>?= fun predecessor_round ->
      let escape_vote = false in
      Apply.begin_partial_construction ctxt ~predecessor_level ~escape_vote
      >>=? fun ( ctxt,
                 liquidity_baking_operations_results,
                 liquidity_baking_escape_ema ) ->
      let mode =
        Partial_construction
          {
            predecessor;
            predecessor_fitness;
            predecessor_level;
            predecessor_round;
          }
      in
      return
        ( mode,
          ctxt,
          liquidity_baking_operations_results,
          liquidity_baking_escape_ema )
  | Some proto_header ->
      Alpha_context.Fitness.round_from_raw predecessor_fitness
      >>?= fun predecessor_round ->
      let round_durations = Alpha_context.Constants.round_durations ctxt in
      Alpha_context.Round.round_of_timestamp
        round_durations
        ~predecessor_timestamp
        ~predecessor_round
        ~timestamp
      >>?= fun round ->
      (* The endorsement/preendorsement validation rules for construction are the
         same as for application. *)
      Apply.begin_full_construction
        ctxt
        ~predecessor_timestamp
        ~predecessor_round
        ~predecessor_level
        ~round
        proto_header.contents
      >>=? fun {
                 ctxt;
                 protocol_data = protocol_data_contents;
                 payload_producer;
                 block_producer;
                 round;
                 liquidity_baking_escape_ema;
                 implicit_operations_results =
                   liquidity_baking_operations_results;
               } ->
      let mode =
        Full_construction
          {
            predecessor;
            payload_producer;
            block_producer;
            level;
            round;
            protocol_data_contents;
            predecessor_round;
            predecessor_level;
          }
      in
      return
        ( mode,
          ctxt,
          liquidity_baking_operations_results,
          liquidity_baking_escape_ema ))
  >|=? fun ( mode,
             ctxt,
             liquidity_baking_operations_results,
             liquidity_baking_escape_ema ) ->
  {
    mode;
    chain_id;
    ctxt;
    op_count = 0;
    migration_balance_updates;
    liquidity_baking_escape_ema;
    implicit_operations_results =
      Apply_results.pack_migration_operation_results migration_operation_results
      @ liquidity_baking_operations_results;
  }

let apply_operation_with_mode mode ctxt chain_id data op_count operation
    ~payload_producer =
  let {shell; protocol_data = Operation_data protocol_data} = operation in
  let operation : _ Alpha_context.operation = {shell; protocol_data} in
  Apply.apply_operation
    ctxt
    chain_id
    mode
    Optimized
    ~payload_producer
    (Alpha_context.Operation.hash operation)
    operation
  >|=? fun (ctxt, result) ->
  let op_count = op_count + 1 in
  ({data with ctxt; op_count}, Operation_metadata result)

let apply_operation ({mode; chain_id; ctxt; op_count; _} as data)
    (operation : Alpha_context.packed_operation) =
  match mode with
  | Partial_application _
    when not
           (List.exists
              (Compare.Int.equal 0)
              (Alpha_context.Operation.acceptable_passes operation)) ->
      (* Multipass validation only considers operations in pass 0. *)
      let op_count = op_count + 1 in
      return ({data with ctxt; op_count}, No_operation_metadata)
  | Partial_application
      {
        block_header =
          {
            shell = {predecessor; _};
            protocol_data = {contents = {payload_hash; _}; _};
          };
        fitness;
        payload_producer;
        predecessor_round;
        predecessor_level;
        _;
      } ->
      let locked_round = Alpha_context.Fitness.locked_round fitness in
      apply_operation_with_mode
        (Apply.Application
           {
             payload_hash;
             predecessor_block = predecessor;
             predecessor_round;
             predecessor_level;
             locked_round;
             round = Alpha_context.Fitness.round fitness;
           })
        ctxt
        chain_id
        data
        op_count
        operation
        ~payload_producer
  | Application
      {
        block_header =
          {
            shell = {predecessor; _};
            protocol_data = {contents = {payload_hash; _}; _};
          };
        fitness;
        payload_producer;
        predecessor_round;
        predecessor_level;
        _;
      } ->
      let locked_round = Alpha_context.Fitness.locked_round fitness in
      apply_operation_with_mode
        (Apply.Application
           {
             payload_hash;
             predecessor_block = predecessor;
             predecessor_round;
             predecessor_level;
             locked_round;
             round = Alpha_context.Fitness.round fitness;
           })
        ctxt
        chain_id
        data
        op_count
        operation
        ~payload_producer
  | Partial_construction
      {predecessor_level; predecessor_round; predecessor_fitness; _} ->
      Alpha_context.Fitness.predecessor_round_from_raw predecessor_fitness
      >>?= fun grand_parent_round ->
      apply_operation_with_mode
        (Apply.Partial_construction
           {predecessor_round; predecessor_level; grand_parent_round})
        ctxt
        chain_id
        data
        op_count
        operation
        ~payload_producer:Signature.Public_key_hash.zero
  | Full_construction
      {
        payload_producer;
        predecessor;
        predecessor_round;
        predecessor_level;
        protocol_data_contents = {payload_hash; _};
        round;
        _;
      } ->
      apply_operation_with_mode
        (Apply.Full_construction
           {
             payload_hash;
             predecessor_block = predecessor;
             predecessor_level;
             predecessor_round;
             round;
           })
        ctxt
        chain_id
        data
        op_count
        operation
        ~payload_producer

let cache_nonce_from_block_header shell contents =
  let open Alpha_context.Block_header in
  let shell =
    Block_header.
      {
        level = 0l;
        proto_level = 0;
        predecessor = shell.predecessor;
        timestamp = Time.of_seconds 0L;
        validation_passes = 0;
        operations_hash = shell.operations_hash;
        fitness = [];
        context = Context_hash.zero;
      }
  in
  let contents =
    {
      contents with
      payload_hash = Block_payload_hash.zero;
      proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '0';
    }
  in
  let protocol_data = {signature = Signature.zero; contents} in
  let x = {shell; protocol_data} in
  Block_hash.to_bytes (hash x)

let finalize_block_application ctxt round ~cache_nonce finalize_application_mode
    protocol_data payload_producer block_producer liquidity_baking_escape_ema
    implicit_operations_results predecessor migration_balance_updates op_count =
  Apply.finalize_application
    ctxt
    finalize_application_mode
    protocol_data
    ~payload_producer
    ~block_producer
    liquidity_baking_escape_ema
    implicit_operations_results
    ~round
    ~predecessor
    ~migration_balance_updates
  >>=? fun (ctxt, fitness, receipt) ->
  Alpha_context.Cache.Admin.sync ctxt ~cache_nonce >>= fun ctxt ->
  let level = Alpha_context.Level.current ctxt in
  let raw_level = Alpha_context.Raw_level.to_int32 level.level in
  let commit_message =
    Format.asprintf
      "lvl %ld, fit:%a, round %a, %d ops"
      raw_level
      Alpha_context.Fitness.pp
      fitness
      Alpha_context.Round.pp
      round
      op_count
  in
  let validation_result =
    Alpha_context.finalize
      ~commit_message
      ctxt
      (Alpha_context.Fitness.to_raw fitness)
  in
  return (validation_result, receipt)

type error += Missing_shell_header

let () =
  register_error_kind
    `Permanent
    ~id:"main.missing_shell_header"
    ~title:"Missing shell_header during finalisation of a block"
    ~description:
      "During finalisation of a block header in Application mode or Full \
       construction mode, a shell header should be provided so that a cache \
       nonce can be computed."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "No shell header provided during the finalisation of a block.")
    Data_encoding.unit
    (function Missing_shell_header -> Some () | _ -> None)
    (fun () -> Missing_shell_header)

let finalize_block
    {
      mode;
      ctxt;
      op_count;
      migration_balance_updates;
      liquidity_baking_escape_ema;
      implicit_operations_results;
      _;
    } shell_header =
  match mode with
  | Partial_construction {predecessor_fitness; _} ->
      Alpha_context.Voting_period.get_rpc_current_info ctxt
      >>=? fun voting_period_info ->
      let level_info = Alpha_context.Level.current ctxt in
      let fitness = predecessor_fitness in
      let ctxt = Alpha_context.finalize ctxt fitness in
      return
        ( ctxt,
          Apply_results.
            {
              proposer = Signature.Public_key_hash.zero;
              baker = Signature.Public_key_hash.zero;
              level_info;
              voting_period_info;
              nonce_hash = None;
              consumed_gas = Alpha_context.Gas.Arith.zero;
              deactivated = [];
              balance_updates = migration_balance_updates;
              liquidity_baking_escape_ema;
              implicit_operations_results;
            } )
  | Partial_application {fitness; block_producer; _} ->
      (* For partial application we do not completely check the block validity.
         Validating the endorsements is sufficient for a good precheck *)
      let level = Alpha_context.Level.current ctxt in
      let included_endorsements =
        Alpha_context.Consensus.current_endorsement_power ctxt
      in
      let minimum = Alpha_context.Constants.consensus_threshold ctxt in
      Apply.are_endorsements_required ctxt ~level:level.level
      >>=? fun endorsements_required ->
      (if endorsements_required then
       Apply.check_minimum_endorsements
         ~endorsing_power:included_endorsements
         ~minimum
      else return_unit)
      >>=? fun () ->
      Alpha_context.Voting_period.get_rpc_current_info ctxt
      >|=? fun voting_period_info ->
      let level_info = Alpha_context.Level.current ctxt in
      let ctxt =
        Alpha_context.finalize ctxt (Alpha_context.Fitness.to_raw fitness)
      in
      ( ctxt,
        Apply_results.
          {
            proposer = Signature.Public_key_hash.zero;
            (* We cannot retrieve the proposer as it requires the
               frozen deposit that might not be available depending on
               the context given to the partial application. *)
            baker = block_producer;
            level_info;
            voting_period_info;
            nonce_hash = None;
            consumed_gas = Alpha_context.Gas.Arith.zero;
            deactivated = [];
            balance_updates = migration_balance_updates;
            liquidity_baking_escape_ema;
            implicit_operations_results;
          } )
  | Application
      {
        payload_producer;
        fitness;
        block_producer;
        block_header = {protocol_data = {contents = protocol_data; _}; shell};
        _;
      } ->
      let round = Alpha_context.Fitness.round fitness in
      let cache_nonce = cache_nonce_from_block_header shell protocol_data in
      finalize_block_application
        ctxt
        ~cache_nonce
        round
        (Finalize_application fitness)
        protocol_data
        payload_producer
        block_producer
        liquidity_baking_escape_ema
        implicit_operations_results
        shell.predecessor
        migration_balance_updates
        op_count
  | Full_construction
      {
        predecessor;
        predecessor_round;
        protocol_data_contents;
        round;
        level;
        payload_producer;
        block_producer;
        _;
      } ->
      Option.value_e
        shell_header
        ~error:(Error_monad.trace_of_error Missing_shell_header)
      >>?= fun shell_header ->
      let cache_nonce =
        cache_nonce_from_block_header shell_header protocol_data_contents
      in
      Alpha_context.Raw_level.of_int32 level >>?= fun level ->
      finalize_block_application
        ctxt
        round
        ~cache_nonce
        (Finalize_full_construction {level; predecessor_round})
        protocol_data_contents
        payload_producer
        block_producer
        liquidity_baking_escape_ema
        implicit_operations_results
        predecessor
        migration_balance_updates
        op_count

let relative_position_within_block op1 op2 =
  let open Alpha_context in
  let (Operation_data op1) = op1.protocol_data in
  let (Operation_data op2) = op2.protocol_data in
  match[@coq_match_with_default] (op1.contents, op2.contents) with
  | (Single (Preendorsement _), Single (Preendorsement _)) -> 0
  | (Single (Preendorsement _), _) -> -1
  | (_, Single (Preendorsement _)) -> 1
  | (Single (Endorsement _), Single (Endorsement _)) -> 0
  | (Single (Endorsement _), _) -> -1
  | (_, Single (Endorsement _)) -> 1
  | (Single (Seed_nonce_revelation _), Single (Seed_nonce_revelation _)) -> 0
  | (_, Single (Seed_nonce_revelation _)) -> 1
  | (Single (Seed_nonce_revelation _), _) -> -1
  | ( Single (Double_preendorsement_evidence _),
      Single (Double_preendorsement_evidence _) ) ->
      0
  | (_, Single (Double_preendorsement_evidence _)) -> 1
  | (Single (Double_preendorsement_evidence _), _) -> -1
  | ( Single (Double_endorsement_evidence _),
      Single (Double_endorsement_evidence _) ) ->
      0
  | (_, Single (Double_endorsement_evidence _)) -> 1
  | (Single (Double_endorsement_evidence _), _) -> -1
  | (Single (Double_baking_evidence _), Single (Double_baking_evidence _)) -> 0
  | (_, Single (Double_baking_evidence _)) -> 1
  | (Single (Double_baking_evidence _), _) -> -1
  | (Single (Activate_account _), Single (Activate_account _)) -> 0
  | (_, Single (Activate_account _)) -> 1
  | (Single (Activate_account _), _) -> -1
  | (Single (Proposals _), Single (Proposals _)) -> 0
  | (_, Single (Proposals _)) -> 1
  | (Single (Proposals _), _) -> -1
  | (Single (Ballot _), Single (Ballot _)) -> 0
  | (_, Single (Ballot _)) -> 1
  | (Single (Ballot _), _) -> -1
  | (Single (Failing_noop _), Single (Failing_noop _)) -> 0
  | (_, Single (Failing_noop _)) -> 1
  | (Single (Failing_noop _), _) -> -1
  (* Manager operations with smaller counter are pre-validated first. *)
  | (Single (Manager_operation op1), Single (Manager_operation op2)) ->
      Z.compare op1.counter op2.counter
  | (Cons (Manager_operation op1, _), Single (Manager_operation op2)) ->
      Z.compare op1.counter op2.counter
  | (Single (Manager_operation op1), Cons (Manager_operation op2, _)) ->
      Z.compare op1.counter op2.counter
  | (Cons (Manager_operation op1, _), Cons (Manager_operation op2, _)) ->
      Z.compare op1.counter op2.counter

let init_cache ctxt =
  Context.Cache.set_cache_layout ctxt cache_layout >>= fun ctxt ->
  Lwt.return (Context.Cache.clear ctxt)

let init ctxt block_header =
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
      ~legacy:true
      ~allow_forged_in_storage
      script
    >>=? fun (Ex_script parsed_script, ctxt) ->
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
  init_cache ctxt >>= fun ctxt ->
  Alpha_context.prepare_first_block ~typecheck ~level ~timestamp ctxt
  >>=? fun ctxt ->
  let cache_nonce =
    cache_nonce_from_block_header
      block_header
      {
        payload_hash = Block_payload_hash.zero;
        payload_round = Alpha_context.Round.zero;
        liquidity_baking_escape_vote = false;
        seed_nonce_hash = None;
        proof_of_work_nonce =
          Bytes.make Constants_repr.proof_of_work_nonce_size '0';
      }
  in
  Alpha_context.Cache.Admin.sync ctxt ~cache_nonce >>= fun ctxt ->
  return
    (Alpha_context.finalize ctxt (Alpha_context.Fitness.to_raw init_fitness))

let value_of_key ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp
    ~predecessor_level:pred_level ~predecessor_fitness:_ ~predecessor:_
    ~timestamp =
  let level = Int32.succ pred_level in
  Alpha_context.prepare ctxt ~level ~predecessor_timestamp ~timestamp
  >>=? fun (ctxt, _, _) -> return (Apply.value_of_key ctxt)

let check_manager_signature {chain_id; ctxt; _} op raw_op =
  Apply.check_manager_signature ctxt chain_id op raw_op

let precheck_manager {ctxt; _} op =
  (* We do not account for the gas limit of the batch in the block
     since this function does not return a context, but we check that
     this limit is within bounds (and fail otherwise with a
     permanenent error). *)
  Apply.precheck_manager_contents_list ctxt op ~mempool_mode:true
  >|=? fun (_ :
             Alpha_context.t
             * 'kind Alpha_context.Kind.manager
               Apply_results.prechecked_contents_list) -> ()

(* Vanity nonce: 1031119008347755 *)
