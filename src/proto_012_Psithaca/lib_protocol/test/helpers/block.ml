(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; _} =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun {Plugin.RPC.Baking_rights.round = r; _} -> r = round)
         bakers
  in
  (pkh, round, WithExceptions.Option.to_exn ~none:(Failure "") timestamp)

let get_next_baker_by_account pkh block =
  Plugin.RPC.Baking_rights.get rpc_ctxt ~delegates:[pkh] block
  >>=? fun bakers ->
  (match List.hd bakers with
  | Some b -> return b
  | None -> failwith "No slots found for %a" Signature.Public_key_hash.pp pkh)
  >>=? fun {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; round; _} ->
  return
    (pkh, round, WithExceptions.Option.to_exn ~none:(Failure __LOC__) timestamp)

let get_next_baker_excluding excludes block =
  Plugin.RPC.Baking_rights.get rpc_ctxt block >|=? fun bakers ->
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; round; _} =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun {Plugin.RPC.Baking_rights.delegate; _} ->
           not
             (List.mem ~equal:Signature.Public_key_hash.equal delegate excludes))
         bakers
  in
  (pkh, round, WithExceptions.Option.to_exn ~none:(Failure "") timestamp)

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
    (* the signer of the block *)
    shell : Block_header.shell_header;
    contents : Block_header.contents;
  }

  let default_proof_of_work_nonce =
    Bytes.create Constants.proof_of_work_nonce_size

  let make_contents ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ~payload_hash ~payload_round ?(liquidity_baking_escape_vote = false)
      ~seed_nonce_hash () =
    Block_header.
      {
        payload_hash;
        payload_round;
        proof_of_work_nonce;
        seed_nonce_hash;
        liquidity_baking_escape_vote;
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

  let set_seed_nonce_hash seed_nonce_hash {baker; shell; contents} =
    {baker; shell; contents = {contents with seed_nonce_hash}}

  let set_baker baker header = {header with baker}

  let sign_header {baker; shell; contents} =
    Account.find baker >|=? fun delegate ->
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    let signature =
      Signature.sign
        ~watermark:Block_header.(to_watermark (Block_header Chain_id.zero))
        delegate.sk
        unsigned_bytes
    in
    Block_header.{shell; protocol_data = {contents; signature}}

  let classify_operations operations =
    let validation_passes_len = List.length Main.validation_passes in
    let t = Array.make validation_passes_len [] in
    List.iter
      (fun (op : packed_operation) ->
        List.iter
          (fun pass -> t.(pass) <- op :: t.(pass))
          (Main.acceptable_passes op))
      operations ;
    let t = Array.map List.rev t in
    Array.to_list t

  let forge_header ?(locked_round = None) ?(payload_round = None)
      ?(policy = By_round 0) ?timestamp ?(operations = [])
      ?liquidity_baking_escape_vote pred =
    let pred_fitness =
      match Fitness.from_raw pred.header.shell.fitness with
      | Ok pred_fitness -> pred_fitness
      | _ -> assert false
    in
    let predecessor_round = Fitness.round pred_fitness in
    dispatch_policy policy pred >>=? fun (pkh, round, expected_timestamp) ->
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
    >|=? fun seed_nonce_hash ->
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
    let non_consensus_operations_hash = Operation_list_hash.compute hashes in
    let payload_round =
      match payload_round with None -> round | Some r -> r
    in
    let payload_hash =
      Block_payload.hash
        ~predecessor:shell.predecessor
        payload_round
        non_consensus_operations_hash
    in
    let contents =
      make_contents
        ~seed_nonce_hash
        ?liquidity_baking_escape_vote
        ~payload_hash
        ~payload_round
        ()
    in
    {baker = pkh; shell; contents}

  (* compatibility only, needed by incremental *)
  let contents ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ?seed_nonce_hash ?(liquidity_baking_escape_vote = false) ~payload_hash
      ~payload_round () =
    {
      Block_header.proof_of_work_nonce;
      seed_nonce_hash;
      liquidity_baking_escape_vote;
      payload_hash;
      payload_round;
    }
end

(********* Genesis creation *************)

(* Hard-coded context key *)
let protocol_param_key = ["protocol_parameters"]

let check_constants_consistency constants =
  let open Constants in
  let {blocks_per_cycle; blocks_per_commitment; blocks_per_stake_snapshot; _} =
    constants
  in
  Error_monad.unless (blocks_per_commitment <= blocks_per_cycle) (fun () ->
      failwith
        "Inconsistent constants : blocks per commitment must be less than \
         blocks per cycle")
  >>=? fun () ->
  Error_monad.unless (blocks_per_cycle >= blocks_per_stake_snapshot) (fun () ->
      failwith
        "Inconsistent constants : blocks per cycle must be superior than \
         blocks per roll snapshot")

let prepare_main_init_params ?bootstrap_contracts commitments constants
    initial_accounts =
  let open Tezos_protocol_012_Psithaca_parameters in
  let bootstrap_accounts =
    List.map
      (fun (Account.{pk; pkh; _}, amount) ->
        Default_parameters.make_bootstrap_account (pkh, pk, amount))
      initial_accounts
  in
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
    let empty = Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt protocol_param_key proto_params)

let initial_context ?(commitments = []) ?bootstrap_contracts constants header
    initial_accounts =
  prepare_main_init_params
    ?bootstrap_contracts
    commitments
    constants
    initial_accounts
  >>= fun ctxt ->
  Main.init ctxt header >|= Environment.wrap_tzresult >|=? fun {context; _} ->
  context

let initial_alpha_context ?(commitments = []) constants
    (block_header : Block_header.shell_header) initial_accounts =
  prepare_main_init_params commitments constants initial_accounts
  >>= fun ctxt ->
  let level = block_header.level in
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
  Main.init_cache ctxt >>= fun ctxt ->
  Alpha_context.prepare_first_block ~typecheck ~level ~timestamp ctxt
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
  let contents =
    Forge.make_contents
      ~payload_hash:Block_payload_hash.zero
      ~payload_round:Round.zero
      ~seed_nonce_hash:None
      ()
  in
  let open Tezos_protocol_012_Psithaca_parameters in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt protocol_param_key proto_params)
  >>= fun ctxt ->
  Main.init ctxt shell >|= Environment.wrap_tzresult >|=? fun {context; _} ->
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
  }

let validate_initial_accounts (initial_accounts : (Account.t * Tez.t) list)
    tokens_per_roll =
  if initial_accounts = [] then
    Stdlib.failwith "Must have one account with a roll to bake" ;
  (* Check there is at least one roll *)
  Lwt.catch
    (fun () ->
      List.fold_left_es
        (fun acc (_, amount) ->
          Environment.wrap_tzresult @@ Tez.( +? ) acc amount >>?= fun acc ->
          if acc >= tokens_per_roll then raise Exit else return acc)
        Tez.zero
        initial_accounts
      >>=? fun _ ->
      failwith "Insufficient tokens in initial accounts to create one roll")
    (function Exit -> return_unit | exc -> raise exc)

let prepare_initial_context_params ?consensus_threshold ?min_proposal_quorum
    ?level ?cost_per_byte ?liquidity_baking_subsidy ?endorsing_reward_per_slot
    ?baking_reward_bonus_per_slot ?baking_reward_fixed_portion ?origination_size
    ?blocks_per_cycle initial_accounts =
  let open Tezos_protocol_012_Psithaca_parameters in
  let constants = Default_parameters.constants_test in
  let min_proposal_quorum =
    Option.value ~default:constants.min_proposal_quorum min_proposal_quorum
  in
  let cost_per_byte =
    Option.value ~default:constants.cost_per_byte cost_per_byte
  in
  let liquidity_baking_subsidy =
    Option.value
      ~default:constants.liquidity_baking_subsidy
      liquidity_baking_subsidy
  in
  let endorsing_reward_per_slot =
    Option.value
      ~default:constants.endorsing_reward_per_slot
      endorsing_reward_per_slot
  in
  let baking_reward_bonus_per_slot =
    Option.value
      ~default:constants.baking_reward_bonus_per_slot
      baking_reward_bonus_per_slot
  in
  let baking_reward_fixed_portion =
    Option.value
      ~default:constants.baking_reward_fixed_portion
      baking_reward_fixed_portion
  in
  let origination_size =
    Option.value ~default:constants.origination_size origination_size
  in
  let blocks_per_cycle =
    Option.value ~default:constants.blocks_per_cycle blocks_per_cycle
  in
  (* ?origination_size *)
  let consensus_threshold =
    Option.value ~default:constants.consensus_threshold consensus_threshold
  in
  let constants =
    {
      constants with
      endorsing_reward_per_slot;
      baking_reward_bonus_per_slot;
      baking_reward_fixed_portion;
      origination_size;
      blocks_per_cycle;
      min_proposal_quorum;
      cost_per_byte;
      liquidity_baking_subsidy;
      consensus_threshold;
    }
  in
  (* Check there is at least one roll *)
  Lwt.catch
    (fun () ->
      List.fold_left_es
        (fun acc (_, amount) ->
          Environment.wrap_tzresult @@ Tez.( +? ) acc amount >>?= fun acc ->
          if acc >= constants.tokens_per_roll then raise Exit else return acc)
        Tez.zero
        initial_accounts
      >>=? fun _ ->
      failwith "Insufficient tokens in initial accounts to create one roll")
    (function Exit -> return_unit | exc -> raise exc)
  >>=? fun () ->
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
  validate_initial_accounts initial_accounts constants.tokens_per_roll
  (* Perhaps this could return a new type  signifying its name *)
  >|=? fun _initial_accounts -> (constants, shell, hash)

(* if no parameter file is passed we check in the current directory
   where the test is run *)
let genesis ?commitments ?consensus_threshold ?min_proposal_quorum
    ?bootstrap_contracts ?level ?cost_per_byte ?liquidity_baking_subsidy
    ?endorsing_reward_per_slot ?baking_reward_bonus_per_slot
    ?baking_reward_fixed_portion ?origination_size ?blocks_per_cycle
    (initial_accounts : (Account.t * Tez.t) list) =
  prepare_initial_context_params
    ?consensus_threshold
    ?min_proposal_quorum
    ?level
    ?cost_per_byte
    ?liquidity_baking_subsidy
    ?endorsing_reward_per_slot
    ?baking_reward_bonus_per_slot
    ?baking_reward_fixed_portion
    ?origination_size
    ?blocks_per_cycle
    initial_accounts
  >>=? fun (constants, shell, hash) ->
  initial_context
    ?commitments
    ?bootstrap_contracts
    constants
    shell
    initial_accounts
  >|=? fun context ->
  let contents =
    Forge.make_contents
      ~payload_hash:Block_payload_hash.zero
      ~payload_round:Round.zero
      ~seed_nonce_hash:None
      ()
  in
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
  }

let alpha_context ?commitments ?min_proposal_quorum
    (initial_accounts : (Account.t * Tez.t) list) =
  prepare_initial_context_params ?min_proposal_quorum initial_accounts
  >>=? fun (constants, shell, _hash) ->
  initial_alpha_context ?commitments constants shell initial_accounts

(********* Baking *************)

(* Note that by calling this function without [protocol_data], we force the mode
   to be partial construction (by correspondingly calling [begin_construction]
   without [protocol_data]). *)
let get_application_vstate (pred : t) (operations : Protocol.operation trace) =
  Forge.forge_header pred ~operations >>=? fun header ->
  Forge.sign_header header >>=? fun header ->
  let open Environment.Error_monad in
  Main.begin_application
    ~chain_id:Chain_id.zero
    ~predecessor_context:pred.context
    ~predecessor_fitness:pred.header.shell.fitness
    ~predecessor_timestamp:pred.header.shell.timestamp
    header
  >|= Environment.wrap_tzresult

let get_construction_vstate ?(policy = By_round 0) ?timestamp
    ?(protocol_data = None) (pred : t) =
  let open Protocol in
  dispatch_policy policy pred >>=? fun (_pkh, _round, expected_timestamp) ->
  let timestamp = Option.value ~default:expected_timestamp timestamp in
  Main.begin_construction
    ~chain_id:Chain_id.zero
    ~predecessor_context:pred.context
    ~predecessor_timestamp:pred.header.shell.timestamp
    ~predecessor_level:pred.header.shell.level
    ~predecessor_fitness:pred.header.shell.fitness
    ~predecessor:pred.hash
    ?protocol_data
    ~timestamp
    ()
  >|= Environment.wrap_tzresult

let apply_with_metadata ?(policy = By_round 0) ~baking_mode header
    ?(operations = []) pred =
  let open Environment.Error_monad in
  ( (match baking_mode with
    | Application ->
        Main.begin_application
          ~chain_id:Chain_id.zero
          ~predecessor_context:pred.context
          ~predecessor_fitness:pred.header.shell.fitness
          ~predecessor_timestamp:pred.header.shell.timestamp
          header
        >|= Environment.wrap_tzresult
    | Baking ->
        get_construction_vstate
          ~policy
          ~protocol_data:(Some header.protocol_data)
          (pred : t))
  >>=? fun vstate ->
    List.fold_left_es
      (fun vstate op ->
        apply_operation vstate op >|= Environment.wrap_tzresult
        >|=? fun (state, _result) -> state)
      vstate
      operations
    >>=? fun vstate ->
    Main.finalize_block vstate (Some header.shell) >|= Environment.wrap_tzresult
    >|=? fun (validation, result) -> (validation.context, result) )
  >|=? fun (context, result) ->
  let hash = Block_header.hash header in
  ({hash; header; operations; context}, result)

let apply header ?(operations = []) pred =
  apply_with_metadata header ~operations pred ~baking_mode:Application
  >>=? fun (t, _metadata) -> return t

let bake_with_metadata ?locked_round ?policy ?timestamp ?operation ?operations
    ?payload_round ~baking_mode ?liquidity_baking_escape_vote pred =
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
    ?liquidity_baking_escape_vote
    pred
  >>=? fun header ->
  Forge.sign_header header >>=? fun header ->
  apply_with_metadata ?policy ~baking_mode header ?operations pred

let bake ?(baking_mode = Application) ?payload_round ?locked_round ?policy
    ?timestamp ?operation ?operations ?liquidity_baking_escape_vote pred =
  bake_with_metadata
    ?payload_round
    ~baking_mode
    ?locked_round
    ?policy
    ?timestamp
    ?operation
    ?operations
    ?liquidity_baking_escape_vote
    pred
  >>=? fun (t, (_metadata : block_header_metadata)) -> return t

(********** Cycles ****************)

(* This function is duplicated from Context to avoid a cyclic dependency *)
let get_constants b = Alpha_services.Constants.all rpc_ctxt b

let bake_n ?(baking_mode = Application) ?policy ?liquidity_baking_escape_vote n
    b =
  List.fold_left_es
    (fun b _ -> bake ~baking_mode ?policy ?liquidity_baking_escape_vote b)
    b
    (1 -- n)

let bake_n_with_all_balance_updates ?(baking_mode = Application) ?policy
    ?liquidity_baking_escape_vote n b =
  List.fold_left_es
    (fun (b, balance_updates_rev) _ ->
      bake_with_metadata ~baking_mode ?policy ?liquidity_baking_escape_vote b
      >>=? fun (b, metadata) ->
      let balance_updates_rev =
        List.rev_append metadata.balance_updates balance_updates_rev
      in
      let balance_updates_rev =
        List.fold_left
          (fun balance_updates_rev ->
            let open Apply_results in
            function
            | Successful_manager_result (Reveal_result _)
            | Successful_manager_result (Delegation_result _) ->
                balance_updates_rev
            | Successful_manager_result (Set_deposits_limit_result _) ->
                balance_updates_rev
            | Successful_manager_result
                (Transaction_result {balance_updates; _})
            | Successful_manager_result
                (Origination_result {balance_updates; _})
            | Successful_manager_result
                (Register_global_constant_result {balance_updates; _}) ->
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
            | Successful_manager_result (Transaction_result _)
            | Successful_manager_result (Register_global_constant_result _)
            | Successful_manager_result (Set_deposits_limit_result _) ->
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

let bake_n_with_liquidity_baking_escape_ema ?(baking_mode = Application) ?policy
    ?liquidity_baking_escape_vote n b =
  List.fold_left_es
    (fun (b, _escape_ema) _ ->
      bake_with_metadata ~baking_mode ?policy ?liquidity_baking_escape_vote b
      >|=? fun (b, metadata) -> (b, metadata.liquidity_baking_escape_ema))
    (b, 0l)
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
