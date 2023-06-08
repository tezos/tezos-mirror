(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
module Proto_Nonce = Nonce (* Renamed otherwise is masked by Alpha_context *)
open Alpha_context

(* This type collects a block and the context that results from its application *)
type t = {
  hash : Block_hash.t;
  header : Block_header.t;
  operations : Operation.packed list;
  context : Tezos_protocol_environment.Context.t;
}

type block = t

let rpc_context block =
  {
    Environment.Updater.block_hash = block.hash;
    block_header = block.header.shell;
    context = block.context;
  }

let rpc_ctxt =
  new Environment.proto_rpc_context_of_directory
    rpc_context
    Plugin.RPC.rpc_services

(******** Policies ***********)

(* Policies are functions that take a block and return a tuple
   [(account, level, timestamp)] for the [forge_header] function. *)

(* This type is used only to provide a simpler interface to the exterior. *)
type baker_policy =
  | By_round of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

type baking_mode = Application | Baking

let get_next_baker_by_round round block =
  Plugin.RPC.Baking_rights.get rpc_ctxt ~all:true ~max_round:(round + 1) block
  >|=? fun bakers ->
  let {Plugin.RPC.Baking_rights.delegate = pkh; consensus_key; timestamp; _} =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun {Plugin.RPC.Baking_rights.round = r; _} ->
           Round.to_int32 r = Int32.of_int round)
         bakers
  in
  ( pkh,
    consensus_key,
    round,
    WithExceptions.Option.to_exn ~none:(Failure "") timestamp )

let get_next_baker_by_account pkh block =
  Plugin.RPC.Baking_rights.get rpc_ctxt ~delegates:[pkh] block
  >>=? fun bakers ->
  (match List.hd bakers with
  | Some b -> return b
  | None -> failwith "No slots found for %a" Signature.Public_key_hash.pp pkh)
  >>=? fun {
             Plugin.RPC.Baking_rights.delegate = pkh;
             consensus_key;
             timestamp;
             round;
             _;
           } ->
  Environment.wrap_tzresult (Round.to_int round) >>?= fun round ->
  return
    ( pkh,
      consensus_key,
      round,
      WithExceptions.Option.to_exn ~none:(Failure __LOC__) timestamp )

let get_next_baker_excluding excludes block =
  Plugin.RPC.Baking_rights.get rpc_ctxt block >>=? fun bakers ->
  let {
    Plugin.RPC.Baking_rights.delegate = pkh;
    consensus_key;
    timestamp;
    round;
    _;
  } =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun {Plugin.RPC.Baking_rights.consensus_key; _} ->
           not
             (List.mem
                ~equal:Signature.Public_key_hash.equal
                consensus_key
                excludes))
         bakers
  in
  Environment.wrap_tzresult (Round.to_int round) >>?= fun round ->
  return
    ( pkh,
      consensus_key,
      round,
      WithExceptions.Option.to_exn ~none:(Failure "") timestamp )

let dispatch_policy = function
  | By_round r -> get_next_baker_by_round r
  | By_account a -> get_next_baker_by_account a
  | Excluding al -> get_next_baker_excluding al

let get_next_baker ?(policy = By_round 0) = dispatch_policy policy

let get_round (b : t) =
  let fitness = b.header.shell.fitness in
  Fitness.(from_raw fitness >|? round) |> Environment.wrap_tzresult

module Forge = struct
  type header = {
    baker : public_key_hash;
    consensus_key : public_key_hash;
    (* the signer of the block *)
    shell : Block_header.shell_header;
    contents : Block_header.contents;
  }

  let default_proof_of_work_nonce =
    Bytes.create Constants.proof_of_work_nonce_size

  let rec naive_pow_miner ~proof_of_work_threshold shell header =
    match
      Hacl_star.Hacl.RandomBuffer.randombytes
        ~size:Constants.proof_of_work_nonce_size
    with
    | Some proof_of_work_nonce ->
        let cand = Block_header.{header with proof_of_work_nonce} in
        if
          Block_header.Proof_of_work.check_header_proof_of_work_stamp
            shell
            cand
            proof_of_work_threshold
        then return cand
        else naive_pow_miner ~proof_of_work_threshold shell header
    | None -> failwith "Impossible to gather randomness"

  let make_contents
      ?(proof_of_work_threshold =
        Tezos_protocol_alpha_parameters.Default_parameters.constants_test
          .proof_of_work_threshold) ~payload_hash ~payload_round
      ?(liquidity_baking_toggle_vote = Toggle_votes.Toggle_vote_pass)
      ?(adaptive_inflation_vote = Toggle_votes.Toggle_vote_pass)
      ~seed_nonce_hash shell =
    naive_pow_miner
      ~proof_of_work_threshold
      shell
      Block_header.
        {
          payload_hash;
          payload_round;
          proof_of_work_nonce = default_proof_of_work_nonce;
          seed_nonce_hash;
          toggle_votes =
            {
              liquidity_baking_vote = liquidity_baking_toggle_vote;
              adaptive_inflation_vote;
            };
        }

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        (* We don't care of the following values, only the shell validates them. *)
        proto_level = 0;
        validation_passes = 0;
        context = Context_hash.zero;
      }

  let set_seed_nonce_hash
      ?(proof_of_work_threshold =
        Tezos_protocol_alpha_parameters.Default_parameters.constants_test
          .proof_of_work_threshold) seed_nonce_hash
      {baker; consensus_key; shell; contents} =
    naive_pow_miner
      ~proof_of_work_threshold
      shell
      {contents with seed_nonce_hash}
    >|=? fun contents -> {baker; consensus_key; shell; contents}

  let set_baker baker ?(consensus_key = baker) header =
    {header with baker; consensus_key}

  let sign_header {consensus_key; shell; contents; _} =
    Account.find consensus_key >|=? fun signer_account ->
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    let signature =
      Signature.sign
        ~watermark:Block_header.(to_watermark (Block_header Chain_id.zero))
        signer_account.sk
        unsigned_bytes
    in
    Block_header.{shell; protocol_data = {contents; signature}}

  let classify_operations operations =
    let validation_passes_len = List.length Main.validation_passes in
    let t = Array.make validation_passes_len [] in
    List.iter
      (fun (op : packed_operation) ->
        match Main.acceptable_pass op with
        | None -> ()
        | Some pass -> t.(pass) <- op :: t.(pass))
      operations ;
    let t = Array.map List.rev t in
    Array.to_list t

  let forge_header ?(locked_round = None) ?(payload_round = None)
      ?(policy = By_round 0) ?timestamp ?(operations = [])
      ?liquidity_baking_toggle_vote ?adaptive_inflation_vote pred =
    let pred_fitness =
      match Fitness.from_raw pred.header.shell.fitness with
      | Ok pred_fitness -> pred_fitness
      | _ -> assert false
    in
    let predecessor_round = Fitness.round pred_fitness in
    dispatch_policy policy pred
    >>=? fun (delegate, consensus_key, round, expected_timestamp) ->
    let timestamp = Option.value ~default:expected_timestamp timestamp in
    let level = Int32.succ pred.header.shell.level in
    Raw_level.of_int32 level |> Environment.wrap_tzresult >>?= fun raw_level ->
    Round.of_int round |> Environment.wrap_tzresult >>?= fun round ->
    Fitness.create ~level:raw_level ~predecessor_round ~round ~locked_round
    >|? Fitness.to_raw |> Environment.wrap_tzresult
    >>?= fun fitness ->
    (Plugin.RPC.current_level ~offset:1l rpc_ctxt pred >|=? function
     | {expected_commitment = true; _} -> Some (fst (Proto_Nonce.generate ()))
     | {expected_commitment = false; _} -> None)
    >>=? fun seed_nonce_hash ->
    let hashes = List.map Operation.hash_packed operations in
    let operations_hash =
      Operation_list_list_hash.compute [Operation_list_hash.compute hashes]
    in
    let shell =
      make_shell
        ~level
        ~predecessor:pred.hash
        ~timestamp
        ~fitness
        ~operations_hash
    in
    let operations = classify_operations operations in
    let non_consensus_operations =
      List.concat (match List.tl operations with None -> [] | Some l -> l)
    in
    let hashes = List.map Operation.hash_packed non_consensus_operations in
    let payload_round =
      match payload_round with None -> round | Some r -> r
    in
    let payload_hash =
      Block_payload.hash
        ~predecessor_hash:shell.predecessor
        ~payload_round
        hashes
    in
    make_contents
      ~seed_nonce_hash
      ?liquidity_baking_toggle_vote
      ?adaptive_inflation_vote
      ~payload_hash
      ~payload_round
      shell
    >|=? fun contents -> {baker = delegate; consensus_key; shell; contents}

  (* compatibility only, needed by incremental *)
  let contents
      ?(proof_of_work_threshold =
        Tezos_protocol_alpha_parameters.Default_parameters.constants_test
          .proof_of_work_threshold) ?seed_nonce_hash
      ?(liquidity_baking_toggle_vote = Toggle_votes.Toggle_vote_pass)
      ?(adaptive_inflation_vote = Toggle_votes.Toggle_vote_pass) ~payload_hash
      ~payload_round shell_header =
    naive_pow_miner
      ~proof_of_work_threshold
      shell_header
      {
        Block_header.proof_of_work_nonce = default_proof_of_work_nonce;
        seed_nonce_hash;
        toggle_votes =
          {
            liquidity_baking_vote = liquidity_baking_toggle_vote;
            adaptive_inflation_vote;
          };
        payload_hash;
        payload_round;
      }
end

(********* Genesis creation *************)

(* Hard-coded context key *)
let protocol_param_key = ["protocol_parameters"]

let check_constants_consistency constants =
  let open Constants.Parametric in
  let {
    blocks_per_cycle;
    blocks_per_commitment;
    nonce_revelation_threshold;
    blocks_per_stake_snapshot;
    _;
  } =
    constants
  in
  Error_monad.unless (blocks_per_commitment <= blocks_per_cycle) (fun () ->
      failwith
        "Inconsistent constants : blocks_per_commitment must be less than \
         blocks_per_cycle")
  >>=? fun () ->
  Error_monad.unless (nonce_revelation_threshold <= blocks_per_cycle) (fun () ->
      failwith
        "Inconsistent constants : nonce_revelation_threshold must be less than \
         blocks_per_cycle")
  >>=? fun () ->
  Error_monad.unless (blocks_per_cycle >= blocks_per_stake_snapshot) (fun () ->
      failwith
        "Inconsistent constants : blocks_per_cycle must be superior than \
         blocks_per_stake_snapshot")

let prepare_main_init_params ?bootstrap_contracts commitments constants
    bootstrap_accounts =
  let open Tezos_protocol_alpha_parameters in
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ?bootstrap_contracts
      ~commitments
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Tezos_protocol_environment.Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt protocol_param_key proto_params)

let initial_context ?(commitments = []) ?bootstrap_contracts chain_id constants
    header bootstrap_accounts =
  prepare_main_init_params
    ?bootstrap_contracts
    commitments
    constants
    bootstrap_accounts
  >>= fun ctxt ->
  Main.init chain_id ctxt header >|= Environment.wrap_tzresult
  >|=? fun {context; _} -> context

let initial_alpha_context ?(commitments = []) constants
    (block_header : Block_header.shell_header) bootstrap_accounts =
  prepare_main_init_params commitments constants bootstrap_accounts
  >>= fun ctxt ->
  let level = block_header.level in
  let timestamp = block_header.timestamp in
  let predecessor = block_header.predecessor in
  let typecheck_smart_contract (ctxt : Alpha_context.context)
      (script : Alpha_context.Script.t) =
    let allow_forged_in_storage =
      false
      (* There should be no forged value in bootstrap contracts. *)
    in
    Script_ir_translator.parse_script
      ctxt
      ~elab_conf:(Script_ir_translator_config.make ~legacy:true ())
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
  Alpha_context.prepare_first_block
    ~typecheck_smart_contract
    ~typecheck_smart_rollup:Sc_rollup_operations.validate_untyped_parameters_ty
    ~level
    ~timestamp
    ~predecessor
    Chain_id.zero
    ctxt
  >|= Environment.wrap_tzresult

let genesis_with_parameters parameters =
  let hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  let fitness =
    Fitness_repr.create_without_locked_round
      ~level:(Protocol.Raw_level_repr.of_int32_exn 0l)
      ~predecessor_round:Round_repr.zero
      ~round:Round_repr.zero
    |> Fitness_repr.to_raw
  in
  let shell =
    Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp:Time.Protocol.epoch
      ~fitness
      ~operations_hash:Operation_list_list_hash.zero
  in
  Forge.make_contents
    ~payload_hash:Block_payload_hash.zero
    ~payload_round:Round.zero
    ~seed_nonce_hash:None
    shell
  >>=? fun contents ->
  let open Tezos_protocol_alpha_parameters in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Tezos_protocol_environment.Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt protocol_param_key proto_params)
  >>= fun ctxt ->
  let chain_id = Chain_id.of_block_hash hash in
  Main.init chain_id ctxt shell >|= Environment.wrap_tzresult
  >|=? fun {context; _} ->
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
  }

let validate_bootstrap_accounts
    (bootstrap_accounts : Parameters.bootstrap_account list) minimal_stake =
  if bootstrap_accounts = [] then
    Stdlib.failwith "Must have one account with minimal_stake to bake" ;
  (* Check there are at least minimal_stake tokens *)
  Lwt.catch
    (fun () ->
      List.fold_left_es
        (fun acc (Parameters.{amount; _} : Parameters.bootstrap_account) ->
          Environment.wrap_tzresult @@ Tez.( +? ) acc amount >>?= fun acc ->
          if acc >= minimal_stake then raise Exit else return acc)
        Tez.zero
        bootstrap_accounts
      >>=? fun (_ : Tez.t) ->
      failwith
        "Insufficient tokens in initial accounts: the amount should be at \
         least minimal_stake")
    (function Exit -> return_unit | exc -> raise exc)

let prepare_initial_context_params ?consensus_threshold ?min_proposal_quorum
    ?level ?cost_per_byte ?reward_weights ?origination_size ?blocks_per_cycle
    ?cycles_per_voting_period ?sc_rollup_enable ?sc_rollup_arith_pvm_enable
    ?dal_enable ?zk_rollup_enable ?adaptive_inflation_enable
    ?hard_gas_limit_per_block ?nonce_revelation_threshold () =
  let open Tezos_protocol_alpha_parameters in
  let constants = Default_parameters.constants_test in
  let min_proposal_quorum =
    Option.value ~default:constants.min_proposal_quorum min_proposal_quorum
  in
  let cost_per_byte =
    Option.value ~default:constants.cost_per_byte cost_per_byte
  in
  let reward_weights =
    Option.value ~default:constants.reward_weights reward_weights
  in
  let origination_size =
    Option.value ~default:constants.origination_size origination_size
  in
  let blocks_per_cycle =
    Option.value ~default:constants.blocks_per_cycle blocks_per_cycle
  in
  let cycles_per_voting_period =
    Option.value
      ~default:constants.cycles_per_voting_period
      cycles_per_voting_period
  in
  let consensus_threshold =
    Option.value ~default:constants.consensus_threshold consensus_threshold
  in
  let sc_rollup_enable =
    Option.value ~default:constants.sc_rollup.enable sc_rollup_enable
  in
  let sc_rollup_arith_pvm_enable =
    Option.value ~default:constants.sc_rollup.enable sc_rollup_arith_pvm_enable
  in
  let dal_enable =
    Option.value ~default:constants.dal.feature_enable dal_enable
  in
  let zk_rollup_enable =
    Option.value ~default:constants.zk_rollup.enable zk_rollup_enable
  in
  let adaptive_inflation_enable =
    Option.value
      ~default:constants.adaptive_inflation.enable
      adaptive_inflation_enable
  in
  let hard_gas_limit_per_block =
    Option.value
      ~default:constants.hard_gas_limit_per_block
      hard_gas_limit_per_block
  in
  let nonce_revelation_threshold =
    Option.value
      ~default:constants.nonce_revelation_threshold
      nonce_revelation_threshold
  in
  let constants =
    {
      constants with
      reward_weights;
      origination_size;
      blocks_per_cycle;
      cycles_per_voting_period;
      min_proposal_quorum;
      cost_per_byte;
      consensus_threshold;
      tx_rollup = constants.tx_rollup;
      sc_rollup =
        {
          constants.sc_rollup with
          enable = sc_rollup_enable;
          arith_pvm_enable = sc_rollup_arith_pvm_enable;
        };
      dal = {constants.dal with feature_enable = dal_enable};
      zk_rollup = {constants.zk_rollup with enable = zk_rollup_enable};
      adaptive_inflation =
        {constants.adaptive_inflation with enable = adaptive_inflation_enable};
      hard_gas_limit_per_block;
      nonce_revelation_threshold;
    }
  in
  check_constants_consistency constants >>=? fun () ->
  let hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  let level = Option.value ~default:0l level in
  let fitness =
    Fitness_repr.create_without_locked_round
      ~level:(Protocol.Raw_level_repr.of_int32_exn level)
      ~predecessor_round:Round_repr.zero
      ~round:Round_repr.zero
    |> Fitness_repr.to_raw
  in
  let shell =
    Forge.make_shell
      ~level
      ~predecessor:hash
      ~timestamp:Time.Protocol.epoch
      ~fitness
      ~operations_hash:Operation_list_list_hash.zero
  in
  return (constants, shell, hash)

(* if no parameter file is passed we check in the current directory
   where the test is run *)
let genesis ?commitments ?consensus_threshold ?min_proposal_quorum
    ?bootstrap_contracts ?level ?cost_per_byte ?reward_weights ?origination_size
    ?blocks_per_cycle ?cycles_per_voting_period ?sc_rollup_enable
    ?sc_rollup_arith_pvm_enable ?dal_enable ?zk_rollup_enable
    ?hard_gas_limit_per_block ?adaptive_inflation_enable
    ?nonce_revelation_threshold
    (bootstrap_accounts : Parameters.bootstrap_account list) =
  prepare_initial_context_params
    ?consensus_threshold
    ?min_proposal_quorum
    ?level
    ?cost_per_byte
    ?reward_weights
    ?origination_size
    ?blocks_per_cycle
    ?cycles_per_voting_period
    ?sc_rollup_enable
    ?sc_rollup_arith_pvm_enable
    ?dal_enable
    ?zk_rollup_enable
    ?hard_gas_limit_per_block
    ?adaptive_inflation_enable
    ?nonce_revelation_threshold
    ()
  >>=? fun (constants, shell, hash) ->
  validate_bootstrap_accounts bootstrap_accounts constants.minimal_stake
  >>=? fun () ->
  initial_context
    ?commitments
    ?bootstrap_contracts
    (Chain_id.of_block_hash hash)
    constants
    shell
    bootstrap_accounts
  >>=? fun context ->
  Forge.make_contents
    ~payload_hash:Block_payload_hash.zero
    ~payload_round:Round.zero
    ~seed_nonce_hash:None
    shell
  >|=? fun contents ->
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
  }

let alpha_context ?commitments ?min_proposal_quorum
    (bootstrap_accounts : Parameters.bootstrap_account list) =
  prepare_initial_context_params ?min_proposal_quorum ()
  >>=? fun (constants, shell, _hash) ->
  validate_bootstrap_accounts bootstrap_accounts constants.minimal_stake
  >>=? fun () ->
  initial_alpha_context ?commitments constants shell bootstrap_accounts

(********* Baking *************)

let begin_validation_and_application ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let* validation_state = begin_validation ctxt chain_id mode ~predecessor in
  let* application_state = begin_application ctxt chain_id mode ~predecessor in
  return (validation_state, application_state)

let get_application_vstate (pred : t) (operations : Protocol.operation trace) =
  Forge.forge_header pred ~operations >>=? fun header ->
  Forge.sign_header header >>=? fun header ->
  let open Environment.Error_monad in
  begin_validation_and_application
    pred.context
    Chain_id.zero
    (Application header)
    ~predecessor:pred.header.shell
  >|= Environment.wrap_tzresult

(* Note that by calling this function without [protocol_data], we
   force the mode to be partial construction. *)
let get_construction_vstate ?(policy = By_round 0) ?timestamp
    ?(protocol_data = None) (pred : t) =
  let open Protocol in
  dispatch_policy policy pred
  >>=? fun (_pkh, _ck, _round, expected_timestamp) ->
  let timestamp = Option.value ~default:expected_timestamp timestamp in
  let mode =
    match protocol_data with
    | None -> Partial_construction {predecessor_hash = pred.hash; timestamp}
    | Some block_header_data ->
        Construction
          {predecessor_hash = pred.hash; timestamp; block_header_data}
  in
  begin_validation_and_application
    pred.context
    Chain_id.zero
    mode
    ~predecessor:pred.header.shell
  >|= Environment.wrap_tzresult

let validate_and_apply_operation (validation_state, application_state) op =
  let open Lwt_result_syntax in
  let oph = Operation.hash_packed op in
  let* validation_state = validate_operation validation_state oph op in
  let* application_state, receipt = apply_operation application_state oph op in
  return ((validation_state, application_state), receipt)

let finalize_validation_and_application (validation_state, application_state)
    shell_header =
  let open Lwt_result_syntax in
  let* () = finalize_validation validation_state in
  finalize_application application_state shell_header

let detect_manager_failure :
    type kind. kind Apply_results.operation_metadata -> _ =
  let rec detect_manager_failure :
      type kind. kind Apply_results.contents_result_list -> _ =
    let open Apply_results in
    let open Apply_operation_result in
    let open Apply_internal_results in
    let detect_manager_failure_single (type kind)
        (Manager_operation_result
           {operation_result; internal_operation_results; _} :
          kind Kind.manager Apply_results.contents_result) =
      let detect_manager_failure (type kind)
          (result : (kind, _, _) operation_result) =
        match result with
        | Applied _ -> Ok ()
        | Skipped _ -> assert false
        | Backtracked (_, None) ->
            (* there must be another error for this to happen *)
            Ok ()
        | Backtracked (_, Some errs) -> Error errs
        | Failed (_, errs) -> Error errs
      in
      detect_manager_failure operation_result >>? fun () ->
      List.iter_e
        (fun (Internal_operation_result (_, r)) -> detect_manager_failure r)
        internal_operation_results
    in
    function
    | Single_result (Manager_operation_result _ as res) ->
        detect_manager_failure_single res
    | Single_result _ -> Ok ()
    | Cons_result (res, rest) ->
        detect_manager_failure_single res >>? fun () ->
        detect_manager_failure rest
  in
  fun {contents} -> detect_manager_failure contents

let apply_with_metadata ?(policy = By_round 0) ?(check_size = true) ~baking_mode
    ~allow_manager_failures header ?(operations = []) pred =
  let open Environment.Error_monad in
  ( (match baking_mode with
    | Application ->
        begin_validation_and_application
          pred.context
          Chain_id.zero
          (Application header)
          ~predecessor:pred.header.shell
        >|= Environment.wrap_tzresult
    | Baking ->
        get_construction_vstate
          ~policy
          ~protocol_data:(Some header.protocol_data)
          (pred : t))
  >>=? fun vstate ->
    List.fold_left_es
      (fun vstate op ->
        (if check_size then
         let operation_size =
           Data_encoding.Binary.length
             Operation.encoding_with_legacy_attestation_name
             op
         in
         if operation_size > Constants_repr.max_operation_data_length then
           raise
             (invalid_arg
                (Format.sprintf
                   "The operation size is %d, it exceeds the constant maximum \
                    size %d"
                   operation_size
                   Constants_repr.max_operation_data_length))) ;
        validate_and_apply_operation vstate op >>=? fun (state, result) ->
        if allow_manager_failures then return state
        else
          match result with
          | No_operation_metadata -> return state
          | Operation_metadata metadata ->
              detect_manager_failure metadata >>?= fun () -> return state)
      vstate
      operations
    >|= Environment.wrap_tzresult
    >>=? fun vstate ->
    finalize_validation_and_application vstate (Some header.shell)
    >|= Environment.wrap_tzresult
    >|=? fun (validation, result) -> (validation.context, result) )
  >|=? fun (context, result) ->
  let hash = Block_header.hash header in
  ({hash; header; operations; context}, result)

let apply header ?(operations = []) ?(allow_manager_failures = false) pred =
  apply_with_metadata
    header
    ~operations
    pred
    ~baking_mode:Application
    ~allow_manager_failures
  >>=? fun (t, _metadata) -> return t

let bake_with_metadata ?locked_round ?policy ?timestamp ?operation ?operations
    ?payload_round ?check_size ~baking_mode ?(allow_manager_failures = false)
    ?liquidity_baking_toggle_vote ?adaptive_inflation_vote pred =
  let operations =
    match (operation, operations) with
    | Some op, Some ops -> Some (op :: ops)
    | Some op, None -> Some [op]
    | None, Some ops -> Some ops
    | None, None -> None
  in
  Forge.forge_header
    ?payload_round
    ?locked_round
    ?timestamp
    ?policy
    ?operations
    ?liquidity_baking_toggle_vote
    ?adaptive_inflation_vote
    pred
  >>=? fun header ->
  Forge.sign_header header >>=? fun header ->
  apply_with_metadata
    ?policy
    ?check_size
    ~baking_mode
    ~allow_manager_failures
    header
    ?operations
    pred

let bake ?(baking_mode = Application) ?(allow_manager_failures = false)
    ?payload_round ?locked_round ?policy ?timestamp ?operation ?operations
    ?liquidity_baking_toggle_vote ?adaptive_inflation_vote ?check_size pred =
  bake_with_metadata
    ?payload_round
    ~baking_mode
    ~allow_manager_failures
    ?locked_round
    ?policy
    ?timestamp
    ?operation
    ?operations
    ?liquidity_baking_toggle_vote
    ?adaptive_inflation_vote
    ?check_size
    pred
  >>=? fun (t, (_metadata : block_header_metadata)) -> return t

(********** Cycles ****************)

(* This function is duplicated from Context to avoid a cyclic dependency *)
let get_constants b = Alpha_services.Constants.all rpc_ctxt b

let bake_n ?(baking_mode = Application) ?policy ?liquidity_baking_toggle_vote
    ?adaptive_inflation_vote n b =
  List.fold_left_es
    (fun b _ ->
      bake
        ~baking_mode
        ?policy
        ?liquidity_baking_toggle_vote
        ?adaptive_inflation_vote
        b)
    b
    (1 -- n)

let rec bake_while ?(baking_mode = Application) ?policy
    ?liquidity_baking_toggle_vote ?adaptive_inflation_vote predicate b =
  let open Lwt_result_syntax in
  let* new_block =
    bake
      ~baking_mode
      ?policy
      ?liquidity_baking_toggle_vote
      ?adaptive_inflation_vote
      b
  in
  if predicate new_block then
    (bake_while [@ocaml.tailcall])
      ~baking_mode
      ?policy
      ?liquidity_baking_toggle_vote
      ?adaptive_inflation_vote
      predicate
      new_block
  else return b

let bake_until_level ?(baking_mode = Application) ?policy
    ?liquidity_baking_toggle_vote ?adaptive_inflation_vote level b =
  bake_while
    ~baking_mode
    ?policy
    ?liquidity_baking_toggle_vote
    ?adaptive_inflation_vote
    (fun b -> b.header.shell.level <= Raw_level.to_int32 level)
    b

let bake_n_with_all_balance_updates ?(baking_mode = Application) ?policy
    ?liquidity_baking_toggle_vote ?adaptive_inflation_vote n b =
  List.fold_left_es
    (fun (b, balance_updates_rev) _ ->
      bake_with_metadata
        ~baking_mode
        ?policy
        ?liquidity_baking_toggle_vote
        ?adaptive_inflation_vote
        b
      >>=? fun (b, metadata) ->
      let balance_updates_rev =
        List.rev_append metadata.balance_updates balance_updates_rev
      in
      let balance_updates_rev =
        List.fold_left
          (fun balance_updates_rev ->
            let open Apply_results in
            fun (Successful_manager_result r) ->
              match r with
              | Transaction_result (Transaction_to_sc_rollup_result _)
              | Reveal_result _ | Delegation_result _
              | Update_consensus_key_result _ | Set_deposits_limit_result _
              | Transfer_ticket_result _ | Dal_publish_slot_header_result _
              | Sc_rollup_originate_result _ | Sc_rollup_add_messages_result _
              | Sc_rollup_cement_result _ | Sc_rollup_publish_result _
              | Sc_rollup_refute_result _ | Sc_rollup_timeout_result _
              | Sc_rollup_execute_outbox_message_result _
              | Sc_rollup_recover_bond_result _ | Zk_rollup_origination_result _
              | Zk_rollup_publish_result _ | Zk_rollup_update_result _ ->
                  balance_updates_rev
              | Transaction_result
                  ( Transaction_to_contract_result {balance_updates; _}
                  | Transaction_to_zk_rollup_result {balance_updates; _} )
              | Origination_result {balance_updates; _}
              | Register_global_constant_result {balance_updates; _}
              | Increase_paid_storage_result {balance_updates; _} ->
                  List.rev_append balance_updates balance_updates_rev)
          balance_updates_rev
          metadata.implicit_operations_results
      in
      return (b, balance_updates_rev))
    (b, [])
    (1 -- n)
  >|=? fun (b, balance_updates_rev) -> (b, List.rev balance_updates_rev)

let bake_n_with_origination_results ?(baking_mode = Application) ?policy n b =
  List.fold_left_es
    (fun (b, origination_results_rev) _ ->
      bake_with_metadata ~baking_mode ?policy b >>=? fun (b, metadata) ->
      let origination_results_rev =
        List.fold_left
          (fun origination_results_rev ->
            let open Apply_results in
            function
            | Successful_manager_result (Reveal_result _)
            | Successful_manager_result (Delegation_result _)
            | Successful_manager_result (Update_consensus_key_result _)
            | Successful_manager_result (Transaction_result _)
            | Successful_manager_result (Register_global_constant_result _)
            | Successful_manager_result (Set_deposits_limit_result _)
            | Successful_manager_result (Increase_paid_storage_result _)
            | Successful_manager_result (Transfer_ticket_result _)
            | Successful_manager_result (Dal_publish_slot_header_result _)
            | Successful_manager_result (Sc_rollup_originate_result _)
            | Successful_manager_result (Sc_rollup_add_messages_result _)
            | Successful_manager_result (Sc_rollup_cement_result _)
            | Successful_manager_result (Sc_rollup_publish_result _)
            | Successful_manager_result (Sc_rollup_refute_result _)
            | Successful_manager_result (Sc_rollup_timeout_result _)
            | Successful_manager_result
                (Sc_rollup_execute_outbox_message_result _)
            | Successful_manager_result (Sc_rollup_recover_bond_result _)
            | Successful_manager_result (Zk_rollup_origination_result _)
            | Successful_manager_result (Zk_rollup_publish_result _)
            | Successful_manager_result (Zk_rollup_update_result _) ->
                origination_results_rev
            | Successful_manager_result (Origination_result x) ->
                Origination_result x :: origination_results_rev)
          origination_results_rev
          metadata.implicit_operations_results
      in
      return (b, origination_results_rev))
    (b, [])
    (1 -- n)
  >|=? fun (b, origination_results_rev) -> (b, List.rev origination_results_rev)

let bake_n_with_liquidity_baking_toggle_ema ?(baking_mode = Application) ?policy
    ?liquidity_baking_toggle_vote ?adaptive_inflation_vote n b =
  let initial_ema = Toggle_votes.Liquidity_baking_toggle_EMA.zero in
  List.fold_left_es
    (fun (b, _toggle_ema) _ ->
      bake_with_metadata
        ~baking_mode
        ?policy
        ?liquidity_baking_toggle_vote
        ?adaptive_inflation_vote
        b
      >|=? fun (b, metadata) -> (b, metadata.liquidity_baking_toggle_ema))
    (b, initial_ema)
    (1 -- n)

let bake_until_cycle_end ?policy b =
  get_constants b >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let current_level = b.header.shell.level in
  let current_level = Int32.rem current_level blocks_per_cycle in
  let delta = Int32.sub blocks_per_cycle current_level in
  bake_n ?policy (Int32.to_int delta) b

let bake_until_n_cycle_end ?policy n b =
  List.fold_left_es (fun b _ -> bake_until_cycle_end ?policy b) b (1 -- n)

let current_cycle b =
  get_constants b >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let current_level = b.header.shell.level in
  let current_cycle = Int32.div current_level blocks_per_cycle in
  let current_cycle = Cycle.add Cycle.root (Int32.to_int current_cycle) in
  return current_cycle

let bake_until_cycle ?policy cycle (b : t) =
  let rec loop (b : t) =
    current_cycle b >>=? fun current_cycle ->
    if Cycle.equal cycle current_cycle then return b
    else bake_until_cycle_end ?policy b >>=? fun b -> loop b
  in
  loop b
