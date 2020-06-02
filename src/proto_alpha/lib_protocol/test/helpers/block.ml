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
  origination_nonce : Contract.origination_nonce;
}

type block = t

let rpc_context block =
  {
    Environment.Updater.block_hash = block.hash;
    block_header = block.header.shell;
    context = block.context;
  }

let rpc_ctxt =
  new Environment.proto_rpc_context_of_directory rpc_context rpc_services

(******** Policies ***********)

(* Policies are functions that take a block and return a tuple
   [(account, level, timestamp)] for the [forge_header] function. *)

(* This type is used only to provide a simpler interface to the exterior. *)
type baker_policy =
  | By_priority of int
  | By_account of baker_hash
  | Excluding of baker_hash list

let get_next_baker_by_priority priority block =
  Alpha_services.Baker.Baking_rights.get
    rpc_ctxt
    ~all:true
    ~max_priority:(priority + 1)
    block
  >|=? fun bakers ->
  let {Alpha_services.Baker.Baking_rights.baker; timestamp; _} =
    List.find
      (fun {Alpha_services.Baker.Baking_rights.priority = p; _} ->
        p = priority)
      bakers
  in
  (baker, priority, Option.unopt_exn (Failure "") timestamp)

let get_next_baker_by_account baker block =
  Alpha_services.Baker.Baking_rights.get
    rpc_ctxt
    ~bakers:[baker]
    ~max_priority:256
    block
  >|=? fun bakers ->
  let {Alpha_services.Baker.Baking_rights.baker; timestamp; priority; _} =
    List.hd bakers
  in
  (baker, priority, Option.unopt_exn (Failure "") timestamp)

let get_next_baker_excluding excludes block =
  Alpha_services.Baker.Baking_rights.get rpc_ctxt ~max_priority:256 block
  >|=? fun bakers ->
  let {Alpha_services.Baker.Baking_rights.baker; timestamp; priority; _} =
    List.find
      (fun {Alpha_services.Baker.Baking_rights.baker; _} ->
        not (List.mem baker excludes))
      bakers
  in
  (baker, priority, Option.unopt_exn (Failure "") timestamp)

let dispatch_policy = function
  | By_priority p ->
      get_next_baker_by_priority p
  | By_account a ->
      get_next_baker_by_account a
  | Excluding al ->
      get_next_baker_excluding al

let get_next_baker ?(policy = By_priority 0) = dispatch_policy policy

let get_endorsing_power b =
  fold_left_s
    (fun acc (op : Operation.packed) ->
      let (Operation_data data) = op.protocol_data in
      match data.contents with
      | Single (Endorsement _) ->
          Alpha_services.Baker.Endorsing_power.get rpc_ctxt b op Chain_id.zero
          >|=? fun endorsement_power -> acc + endorsement_power
      | _ ->
          return acc)
    0
    b.operations

module Forge = struct
  type header = {
    baker : baker_hash;
    (* the signer of the block *)
    shell : Block_header.shell_header;
    contents : Block_header.contents;
  }

  let default_proof_of_work_nonce =
    Bytes.create Constants.proof_of_work_nonce_size

  let make_contents ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ~priority ~seed_nonce_hash () =
    Block_header.{priority; proof_of_work_nonce; seed_nonce_hash}

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
    Account.find_baker baker
    >|=? fun account ->
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    let signature =
      Signature.sign
        ~watermark:Signature.(Block_header Chain_id.zero)
        account.key.sk
        unsigned_bytes
    in
    Block_header.{shell; protocol_data = {contents; signature}}

  let forge_header ?(policy = By_priority 0) ?timestamp ?(operations = []) pred
      =
    dispatch_policy policy pred
    >>=? fun (baker, priority, _timestamp) ->
    Alpha_services.Baker.Minimal_valid_time.get rpc_ctxt pred priority 0
    >>=? fun expected_timestamp ->
    let timestamp = Option.value ~default:expected_timestamp timestamp in
    let level = Int32.succ pred.header.shell.level in
    ( match Fitness_repr.to_int64 pred.header.shell.fitness with
    | Ok old_fitness ->
        Fitness_repr.from_int64 (Int64.add (Int64.of_int 1) old_fitness)
    | Error _ ->
        assert false )
    |> fun fitness ->
    Alpha_services.Helpers.current_level ~offset:1l rpc_ctxt pred
    >|=? (function
           | {expected_commitment = true; _} ->
               Some (fst (Proto_Nonce.generate ()))
           | {expected_commitment = false; _} ->
               None)
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
    let contents = make_contents ~priority ~seed_nonce_hash () in
    {baker; shell; contents}

  (* compatibility only, needed by incremental *)
  let contents ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ?(priority = 0) ?seed_nonce_hash () =
    {Block_header.priority; proof_of_work_nonce; seed_nonce_hash}
end

(********* Genesis creation *************)

(* Hard-coded context key *)
let protocol_param_key = ["protocol_parameters"]

let check_constants_consistency constants =
  let open Constants_repr in
  let {blocks_per_cycle; blocks_per_commitment; blocks_per_roll_snapshot; _} =
    constants
  in
  Error_monad.unless (blocks_per_commitment <= blocks_per_cycle) (fun () ->
      failwith
        "Inconsistent constants : blocks per commitment must be less than \
         blocks per cycle")
  >>=? fun () ->
  Error_monad.unless (blocks_per_cycle >= blocks_per_roll_snapshot) (fun () ->
      failwith
        "Inconsistent constants : blocks per cycle must be superior than \
         blocks per roll snapshot")

let initial_context ?(with_commitments = false) constants header
    initial_accounts initial_bakers =
  let open Tezos_protocol_alpha_parameters in
  let bootstrap_accounts =
    List.map
      (fun (Account.{pk; pkh; _}, amount) ->
        Default_parameters.make_bootstrap_account (pkh, pk, amount))
      initial_accounts
  in
  let bootstrap_bakers =
    List.map
      (fun (Account.{key = {pk; _}; baker}, amount) ->
        Default_parameters.make_bootstrap_baker (baker, amount, pk))
      initial_bakers
  in
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~bootstrap_bakers
      ~with_commitments
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    set empty ["version"] (Bytes.of_string "genesis")
    >>= fun ctxt -> set ctxt protocol_param_key proto_params)
  >>= fun ctxt ->
  Main.init ctxt header >|= Environment.wrap_error
  >|=? fun {context; _} -> context

let genesis_with_parameters parameters =
  let hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  let shell =
    Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp:Time.Protocol.epoch
      ~fitness:(Fitness_repr.from_int64 0L)
      ~operations_hash:Operation_list_list_hash.zero
  in
  let contents = Forge.make_contents ~priority:0 ~seed_nonce_hash:None () in
  let open Tezos_protocol_alpha_parameters in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    set empty ["version"] (Bytes.of_string "genesis")
    >>= fun ctxt -> set ctxt protocol_param_key proto_params)
  >>= fun ctxt ->
  Main.init ctxt shell >|= Environment.wrap_error
  >|=? fun {context; _} ->
  let origination_nonce =
    Contract.initial_origination_nonce Operation_hash.zero
  in
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
    origination_nonce;
  }

(* if no parameter file is passed we check in the current directory
   where the test is run *)
let genesis ?with_commitments ?endorsers_per_block ?initial_endorsers
    ?min_proposal_quorum (initial_accounts : (Account.t * Tez_repr.t) list)
    (initial_bakers : (Account.baker * Tez_repr.t) list)
    (origination_nonce : Contract.origination_nonce) =
  if initial_bakers = [] then
    Stdlib.failwith "Must have one account with a roll to bake" ;
  let open Tezos_protocol_alpha_parameters in
  let constants = Default_parameters.constants_test in
  let endorsers_per_block =
    Option.value ~default:constants.endorsers_per_block endorsers_per_block
  in
  let initial_endorsers =
    Option.value ~default:constants.initial_endorsers initial_endorsers
  in
  let min_proposal_quorum =
    Option.value ~default:constants.min_proposal_quorum min_proposal_quorum
  in
  let constants =
    {
      constants with
      endorsers_per_block;
      initial_endorsers;
      min_proposal_quorum;
    }
  in
  (* Check there is at least one roll *)
  ( try
      fold_left_s
        (fun acc (_baker, baker_amount) ->
          Environment.wrap_error @@ Tez_repr.( +? ) acc baker_amount
          >>?= fun acc ->
          if acc >= constants.tokens_per_roll then raise Exit else return acc)
        Tez_repr.zero
        initial_bakers
      >>=? fun _ ->
      failwith "Insufficient tokens in initial accounts to create one roll"
    with Exit -> return_unit )
  >>=? fun () ->
  check_constants_consistency constants
  >>=? fun () ->
  let hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  let shell =
    Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp:Time.Protocol.epoch
      ~fitness:(Fitness_repr.from_int64 0L)
      ~operations_hash:Operation_list_list_hash.zero
  in
  let contents = Forge.make_contents ~priority:0 ~seed_nonce_hash:None () in
  initial_context
    ?with_commitments
    constants
    shell
    initial_accounts
    initial_bakers
  >|=? fun context ->
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
    origination_nonce;
  }

(********* Baking *************)

(* collect operation receipt errors, if any *)
let get_operation_receipt_errors :
    operation_receipt -> Environment.Error_monad.error list =
 fun receipt ->
  let open Apply_results in
  let get_operation_result_errors = function
    | Failed (_, errors) ->
        errors
    | Backtracked (_, errors) ->
        Option.value ~default:[] errors
    | _ ->
        []
  in
  let get_internal_operation_result_errors :
      packed_internal_operation_result -> Environment.Error_monad.error list =
    function
    | Internal_manager_operation_result (_op, mop) ->
        get_operation_result_errors mop
    | Internal_baker_operation_result (_op, mop) ->
        get_operation_result_errors mop
  in
  let get_contents_result_errors :
      type kind. kind contents_result -> Environment.Error_monad.error list =
    function
    | Manager_operation_result
        {operation_result; internal_operation_results = rs; _} ->
        get_operation_result_errors operation_result
        :: List.map get_internal_operation_result_errors rs
        |> List.flatten
    | _ ->
        []
  in
  let rec get_contents_result_list_errors :
      type kind.
      kind contents_result_list -> Environment.Error_monad.error list =
    function
    | Single_result result ->
        get_contents_result_errors result
    | Cons_result (r, rs) ->
        let errors = get_contents_result_errors r in
        get_contents_result_list_errors rs |> List.append errors
  in
  match receipt with
  | No_operation_metadata ->
      []
  | Operation_metadata {contents; _} ->
      get_contents_result_list_errors contents

let apply header ?(operations = []) pred =
  (let open Environment.Error_monad in
  Main.begin_application
    ~chain_id:Chain_id.zero
    ~predecessor_context:pred.context
    ~predecessor_fitness:pred.header.shell.fitness
    ~predecessor_timestamp:pred.header.shell.timestamp
    header
  >>=? fun vstate ->
  fold_left_s
    (fun (vstate, ops_errors) op ->
      apply_operation vstate op
      >|=? fun (state, result) ->
      let op_errors = get_operation_receipt_errors result in
      (state, List.append ops_errors op_errors))
    (vstate, [])
    operations
  >>=? fun (vstate, ops_errors) ->
  (* if any operations resulted in errors, return them *)
  ( if List.length ops_errors = 0 then return_unit
  else Lwt.return @@ Error ops_errors )
  >>=? fun () ->
  Main.finalize_block vstate
  >|=? fun (validation, _result) -> validation.context)
  >|= Environment.wrap_error
  >|=? fun context ->
  let hash = Block_header.hash header in
  {
    hash;
    header;
    operations;
    context;
    origination_nonce = pred.origination_nonce;
  }

let bake ?policy ?timestamp ?operation ?operations pred =
  let operations =
    match (operation, operations) with
    | (Some op, Some ops) ->
        Some (op :: ops)
    | (Some op, None) ->
        Some [op]
    | (None, Some ops) ->
        Some ops
    | (None, None) ->
        None
  in
  Forge.forge_header ?timestamp ?policy ?operations pred
  >>=? fun header ->
  Forge.sign_header header >>=? fun header -> apply header ?operations pred

(********** Cycles ****************)

(* This function is duplicated from Context to avoid a cyclic dependency *)
let get_constants b = Alpha_services.Constants.all rpc_ctxt b

let bake_n ?policy n b =
  Error_monad.fold_left_s (fun b _ -> bake ?policy b) b (1 -- n)

let bake_until_cycle_end ?policy b =
  get_constants b
  >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let current_level = b.header.shell.level in
  let current_level = Int32.rem current_level blocks_per_cycle in
  let delta = Int32.sub blocks_per_cycle current_level in
  bake_n ?policy (Int32.to_int delta) b

let bake_until_n_cycle_end ?policy n b =
  Error_monad.fold_left_s (fun b _ -> bake_until_cycle_end ?policy b) b (1 -- n)

let current_cycle b =
  get_constants b
  >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let current_level = b.header.shell.level in
  let current_cycle = Int32.div current_level blocks_per_cycle in
  let current_cycle = Cycle.add Cycle.root (Int32.to_int current_cycle) in
  return current_cycle

let bake_until_cycle ?policy cycle (b : t) =
  let rec loop (b : t) =
    current_cycle b
    >>=? fun current_cycle ->
    if Cycle.equal cycle current_cycle then return b
    else bake_until_cycle_end ?policy b >>=? fun b -> loop b
  in
  loop b
