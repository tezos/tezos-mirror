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
  new Environment.proto_rpc_context_of_directory rpc_context rpc_services

(******** Policies ***********)

(* Policies are functions that take a block and return a tuple
   [(account, level, timestamp)] for the [forge_header] function. *)

(* This type is used only to provide a simpler interface to the exterior. *)
type baker_policy =
  | By_priority of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

let get_next_baker_by_priority priority block =
  Alpha_services.Delegate.Baking_rights.get
    rpc_ctxt
    ~all:true
    ~max_priority:(priority + 1)
    block
  >|=? fun bakers ->
  let {Alpha_services.Delegate.Baking_rights.delegate = pkh; timestamp; _} =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun {Alpha_services.Delegate.Baking_rights.priority = p; _} ->
           p = priority)
         bakers
  in
  (pkh, priority, WithExceptions.Option.to_exn ~none:(Failure "") timestamp)

let get_next_baker_by_account pkh block =
  Alpha_services.Delegate.Baking_rights.get
    rpc_ctxt
    ~delegates:[pkh]
    ~max_priority:256
    block
  >|=? fun bakers ->
  let { Alpha_services.Delegate.Baking_rights.delegate = pkh;
        timestamp;
        priority;
        _ } =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers
  in
  (pkh, priority, WithExceptions.Option.to_exn ~none:(Failure "") timestamp)

let get_next_baker_excluding excludes block =
  Alpha_services.Delegate.Baking_rights.get rpc_ctxt ~max_priority:256 block
  >|=? fun bakers ->
  let { Alpha_services.Delegate.Baking_rights.delegate = pkh;
        timestamp;
        priority;
        _ } =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.find
         (fun {Alpha_services.Delegate.Baking_rights.delegate; _} ->
           not (List.mem delegate excludes))
         bakers
  in
  (pkh, priority, WithExceptions.Option.to_exn ~none:(Failure "") timestamp)

let dispatch_policy = function
  | By_priority p ->
      get_next_baker_by_priority p
  | By_account a ->
      get_next_baker_by_account a
  | Excluding al ->
      get_next_baker_excluding al

let get_next_baker ?(policy = By_priority 0) = dispatch_policy policy

let get_endorsing_power b =
  List.fold_left_es
    (fun acc (op : Operation.packed) ->
      let (Operation_data data) = op.protocol_data in
      match data.contents with
      | Single (Endorsement _) ->
          Alpha_services.Delegate.Endorsing_power.get
            rpc_ctxt
            b
            op
            Chain_id.zero
          >|=? fun endorsement_power -> acc + endorsement_power
      | _ ->
          return acc)
    0
    b.operations

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
    Account.find baker
    >|=? fun delegate ->
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    let signature =
      Signature.sign
        ~watermark:Signature.(Block_header Chain_id.zero)
        delegate.sk
        unsigned_bytes
    in
    Block_header.{shell; protocol_data = {contents; signature}}

  let forge_header ?(policy = By_priority 0) ?timestamp ?(operations = []) pred
      =
    dispatch_policy policy pred
    >>=? fun (pkh, priority, _timestamp) ->
    Alpha_services.Delegate.Minimal_valid_time.get rpc_ctxt pred priority 0
    >>=? fun expected_timestamp ->
    let timestamp = Option.value ~default:expected_timestamp timestamp in
    let level = Int32.succ pred.header.shell.level in
    ( match Fitness.to_int64 pred.header.shell.fitness with
    | Ok old_fitness ->
        Fitness.from_int64 (Int64.add (Int64.of_int 1) old_fitness)
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
    {baker = pkh; shell; contents}

  (* compatibility only, needed by incremental *)
  let contents ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ?(priority = 0) ?seed_nonce_hash () =
    {Block_header.priority; proof_of_work_nonce; seed_nonce_hash}
end

(********* Genesis creation *************)

(* Hard-coded context key *)
let protocol_param_key = ["protocol_parameters"]

let check_constants_consistency constants =
  let open Constants in
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
  >>=? fun () ->
  let min_time_between_blocks =
    match constants.time_between_blocks with
    | first_time_between_blocks :: _ ->
        first_time_between_blocks
    | [] ->
        (* this constant is used in the Baking module *)
        Period.one_minute
  in
  Error_monad.unless
    Compare.Int64.(
      Period.to_seconds min_time_between_blocks
      >= Period.to_seconds constants.minimal_block_delay)
    (fun () ->
      failwith
        "minimal_block_delay value (%Ld) should be smaller than \
         time_between_blocks[0] value (%Ld)"
        (Period.to_seconds constants.minimal_block_delay)
        (Period.to_seconds min_time_between_blocks))
  >>=? fun () ->
  Error_monad.unless
    Compare.Int.(constants.endorsers_per_block >= constants.initial_endorsers)
    (fun () ->
      failwith "initial_endorsers should be smaller than endorsers_per_block")

let initial_context ?(with_commitments = false) constants header
    initial_accounts =
  let open Tezos_protocol_alpha_parameters in
  let bootstrap_accounts =
    List.map
      (fun (Account.{pk; pkh; _}, amount) ->
        Default_parameters.make_bootstrap_account (pkh, pk, amount))
      initial_accounts
  in
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~with_commitments
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis")
    >>= fun ctxt -> add ctxt protocol_param_key proto_params)
  >>= fun ctxt ->
  Main.init ctxt header >|= Environment.wrap_tzresult
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
      ~fitness:(Fitness.from_int64 0L)
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
    add empty ["version"] (Bytes.of_string "genesis")
    >>= fun ctxt -> add ctxt protocol_param_key proto_params)
  >>= fun ctxt ->
  Main.init ctxt shell >|= Environment.wrap_tzresult
  >|=? fun {context; _} ->
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
  }

(* if no parameter file is passed we check in the current directory
   where the test is run *)
let genesis ?with_commitments ?endorsers_per_block ?initial_endorsers
    ?min_proposal_quorum ?time_between_blocks ?minimal_block_delay
    ?delay_per_missing_endorsement
    (initial_accounts : (Account.t * Tez.t) list) =
  if initial_accounts = [] then
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
  let time_between_blocks =
    Option.value ~default:constants.time_between_blocks time_between_blocks
  in
  let minimal_block_delay =
    Option.value ~default:constants.minimal_block_delay minimal_block_delay
  in
  let delay_per_missing_endorsement =
    Option.value
      ~default:constants.delay_per_missing_endorsement
      delay_per_missing_endorsement
  in
  let constants =
    {
      constants with
      endorsers_per_block;
      initial_endorsers;
      min_proposal_quorum;
      time_between_blocks;
      minimal_block_delay;
      delay_per_missing_endorsement;
    }
  in
  (* Check there is at least one roll *)
  Lwt.catch
    (fun () ->
      List.fold_left_es
        (fun acc (_, amount) ->
          Environment.wrap_tzresult @@ Tez.( +? ) acc amount
          >>?= fun acc ->
          if acc >= constants.tokens_per_roll then raise Exit else return acc)
        Tez.zero
        initial_accounts
      >>=? fun _ ->
      failwith "Insufficient tokens in initial accounts to create one roll")
    (function Exit -> return_unit | exc -> raise exc)
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
      ~fitness:(Fitness.from_int64 0L)
      ~operations_hash:Operation_list_list_hash.zero
  in
  let contents = Forge.make_contents ~priority:0 ~seed_nonce_hash:None () in
  initial_context ?with_commitments constants shell initial_accounts
  >|=? fun context ->
  {
    hash;
    header = {shell; protocol_data = {contents; signature = Signature.zero}};
    operations = [];
    context;
  }

(********* Baking *************)

let apply header ?(operations = []) pred =
  (let open Environment.Error_monad in
  Main.begin_application
    ~chain_id:Chain_id.zero
    ~predecessor_context:pred.context
    ~predecessor_fitness:pred.header.shell.fitness
    ~predecessor_timestamp:pred.header.shell.timestamp
    header
  >>=? fun vstate ->
  List.fold_left_es
    (fun vstate op ->
      apply_operation vstate op >|=? fun (state, _result) -> state)
    vstate
    operations
  >>=? fun vstate ->
  Main.finalize_block vstate
  >|=? fun (validation, _result) -> validation.context)
  >|= Environment.wrap_tzresult
  >|=? fun context ->
  let hash = Block_header.hash header in
  {hash; header; operations; context}

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
  List.fold_left_es (fun b _ -> bake ?policy b) b (1 -- n)

let bake_until_cycle_end ?policy b =
  get_constants b
  >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let current_level = b.header.shell.level in
  let current_level = Int32.rem current_level blocks_per_cycle in
  let delta = Int32.sub blocks_per_cycle current_level in
  bake_n ?policy (Int32.to_int delta) b

let bake_until_n_cycle_end ?policy n b =
  List.fold_left_es (fun b _ -> bake_until_cycle_end ?policy b) b (1 -- n)

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
