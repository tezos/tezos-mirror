(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_protocol_alpha.Protocol
open Alpha_context
open Tezos_context
open Tezos_shell_context

module Proto_nonce = struct
  module Table = Hashtbl.Make (struct
    type t = Nonce_hash.t

    let hash h = Int32.to_int (Bytes.get_int32_be (Nonce_hash.to_bytes h) 0)

    let equal = Nonce_hash.equal
  end)

  let known_nonces = Table.create 17

  let generate () =
    match
      Alpha_context.Nonce.of_bytes
      @@ Bytes.init Alpha_context.Constants.nonce_length (fun _ -> '\000')
    with
    | Ok nonce ->
        let hash = Alpha_context.Nonce.hash nonce in
        Table.add known_nonces hash nonce ;
        (hash, nonce)
    | Error _ -> assert false

  let forget_all () = Table.clear known_nonces

  let get hash = Table.find known_nonces hash
end

module Account = struct
  type t = {
    pkh : Signature.Public_key_hash.t;
    pk : Signature.Public_key.t;
    sk : Signature.Secret_key.t;
  }

  let pp fmt {pkh; pk; sk} =
    Format.fprintf
      fmt
      "pkh: %a@ pk: %a@ nsk: %a"
      Signature.Public_key_hash.pp
      pkh
      Signature.Public_key.pp
      pk
      Signature.Secret_key.pp
      sk

  type account = t

  let known_accounts = Signature.Public_key_hash.Table.create 17

  let new_account ?seed () =
    let (pkh, pk, sk) = Signature.generate_key ?seed () in
    let account = {pkh; pk; sk} in
    Signature.Public_key_hash.Table.add known_accounts pkh account ;
    account

  let add_account ({pkh; _} as account) =
    Signature.Public_key_hash.Table.replace known_accounts pkh account

  let activator_account = new_account ()

  let find pkh =
    try
      return
        (Signature.Public_key_hash.Table.find known_accounts pkh
        |> WithExceptions.Option.to_exn ~none:Not_found)
    with Not_found ->
      failwith "Missing account: %a" Signature.Public_key_hash.pp pkh

  let find_alternate pkh =
    let exception Found of t in
    try
      Signature.Public_key_hash.Table.iter
        (fun pkh' account ->
          if not (Signature.Public_key_hash.equal pkh pkh') then
            raise (Found account))
        known_accounts ;
      raise Not_found
    with Found account -> account

  let dummy_account = new_account ()

  let account_to_bootstrap ({pkh; pk; _}, amount) =
    let open Parameters in
    ({public_key_hash = pkh; public_key = Some pk; amount} : bootstrap_account)

  let commitment_secret =
    Blinded_public_key_hash.activation_code_of_hex
      "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"

  let new_commitment ?seed () =
    let (pkh, pk, sk) = Signature.generate_key ?seed ~algo:Ed25519 () in
    let unactivated_account = {pkh; pk; sk} in
    let open Commitment in
    let pkh = match pkh with Ed25519 pkh -> pkh | _ -> assert false in
    let bpkh = Blinded_public_key_hash.of_ed25519_pkh commitment_secret pkh in
    (Lwt.return @@ Environment.wrap_tzresult @@ Tez.(one *? 4_000L))
    >>=? fun amount ->
    return @@ (unactivated_account, {blinded_public_key_hash = bpkh; amount})
end

let rpc_context ctxt block =
  let ctxt = Shell_context.wrap_disk_context ctxt in
  {
    Environment.Updater.block_hash = Store.Block.hash block;
    block_header = Store.Block.shell_header block;
    context = ctxt;
  }

let rpc_ctxt ctxt =
  new Environment.proto_rpc_context_of_directory
    (rpc_context ctxt)
    Plugin.RPC.rpc_services

(******** Policies ***********)

(* Policies are functions that take a block and return a tuple
   [(account, level, timestamp)] for the [forge_header] function. *)

(* This type is used only to provide a simpler interface to the exterior. *)
type baker_policy =
  | By_priority of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

let get_next_baker_by_priority ctxt priority block =
  Plugin.RPC.Baking_rights.get
    (rpc_ctxt ctxt)
    ~all:true
    ~max_priority:(priority + 1)
    block
  >>=? fun bakers ->
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; _} =
    List.find
      (fun {Plugin.RPC.Baking_rights.priority = p; _} -> p = priority)
      bakers
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  return (pkh, priority, WithExceptions.Option.get ~loc:__LOC__ timestamp)

let get_next_baker_by_account ctxt pkh block =
  Plugin.RPC.Baking_rights.get
    (rpc_ctxt ctxt)
    ~delegates:[pkh]
    ~max_priority:256
    block
  >>=? fun bakers ->
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; priority; _} =
    List.hd bakers |> WithExceptions.Option.get ~loc:__LOC__
  in
  return (pkh, priority, WithExceptions.Option.get ~loc:__LOC__ timestamp)

let get_next_baker_excluding ctxt excludes block =
  Plugin.RPC.Baking_rights.get (rpc_ctxt ctxt) ~max_priority:256 block
  >>=? fun bakers ->
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; priority; _} =
    List.find
      (fun {Plugin.RPC.Baking_rights.delegate; _} ->
        not (List.mem ~equal:Signature.Public_key_hash.equal delegate excludes))
      bakers
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  return (pkh, priority, WithExceptions.Option.get ~loc:__LOC__ timestamp)

let dispatch_policy ctxt = function
  | By_priority p -> get_next_baker_by_priority ctxt p
  | By_account a -> get_next_baker_by_account ctxt a
  | Excluding al -> get_next_baker_excluding ctxt al

let get_next_baker chain_store ?(policy = By_priority 0) =
  dispatch_policy chain_store policy

let get_endorsing_power _chain_store _b = 0

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
      ?(liquidity_baking_escape_vote = false) ~priority ~seed_nonce_hash () =
    Block_header.
      {
        priority;
        proof_of_work_nonce;
        seed_nonce_hash;
        liquidity_baking_escape_vote;
      }

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash
      ~proto_level =
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        proto_level;
        validation_passes = List.length Main.validation_passes;
        context = Context_hash.zero (* to update later *);
      }

  let set_seed_nonce_hash seed_nonce_hash {baker; shell; contents} =
    {baker; shell; contents = {contents with seed_nonce_hash}}

  let set_baker baker header = {header with baker}

  let sign_header ~chain_id {baker; shell; contents} =
    Account.find baker >>=? fun delegate ->
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    let signature =
      Signature.sign
        ~watermark:Signature.(Block_header chain_id)
        delegate.sk
        unsigned_bytes
    in
    Block_header.{shell; protocol_data = {contents; signature}} |> return

  let forge_header ctxt ?(policy = By_priority 0) ?timestamp ~operations pred =
    let proto_level = Store.Block.proto_level pred in
    dispatch_policy ctxt policy pred >>=? fun (pkh, priority, _timestamp) ->
    Alpha_services.Delegate.Minimal_valid_time.get
      (rpc_ctxt ctxt)
      pred
      priority
      0
    >>=? fun expected_timestamp ->
    let timestamp = Option.value ~default:expected_timestamp timestamp in
    let level = Int32.succ (Store.Block.level pred) in
    let fitness = Fitness_repr.to_int64 (Store.Block.fitness pred) in
    (match fitness with
    | Ok old_fitness ->
        return
          (Fitness_repr.from_int64 (Int64.add (Int64.of_int 1) old_fitness))
    | Error _ -> assert false)
    >>=? fun fitness ->
    (Plugin.RPC.current_level ~offset:1l (rpc_ctxt ctxt) pred >|=? function
     | {expected_commitment = true; _} -> Some (fst (Proto_nonce.generate ()))
     | {expected_commitment = false; _} -> None)
    >>=? fun seed_nonce_hash ->
    let operations_hash =
      Operation_list_list_hash.compute
        (List.map
           Operation_list_hash.compute
           (List.map (List.map Operation.hash_packed) operations))
    in
    let shell =
      make_shell
        ~level
        ~predecessor:(Store.Block.hash pred)
        ~timestamp
        ~fitness
        ~operations_hash
        ~proto_level
    in
    let contents = make_contents ~priority ~seed_nonce_hash () in
    return {baker = pkh; shell; contents}

  (* compatibility only, needed by incremental *)
  let contents ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ?(liquidity_baking_escape_vote = false) ?(priority = 0) ?seed_nonce_hash
      () =
    Block_header.
      {
        priority;
        proof_of_work_nonce;
        seed_nonce_hash;
        liquidity_baking_escape_vote;
      }
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
  >>=? return

let default_accounts =
  let initial_accounts =
    [
      ( "tz1Wi61aZXxBDTa3brfPfYgMawojnAFTjy8u",
        "edpkvMmiFiAs9Uj9a53dZVPGNJDxMDkAcsEAyVG6dau7GF9vfGWGEY",
        "edsk3UqeiQWXX7NFEY1wUs6J1t2ez5aQ3hEWdqX5Jr5edZiGLW8nZr" );
      ( "tz1Y5JfsJXF4ip1RUQHCgaMbqHAzMDWaiJFf",
        "edpktnweMhc2suERAJVCLQwfbJovHsdMKeHC7GqaGQXhvX7SpDRtTc",
        "edsk4Z5G4QFmVc4iHbpyp35E6272gWhTvDeerpivH78oUX1LVKZTGb" );
      ( "tz1dcv5NSS2Fbs2dW9pRDhi6KJTBAXqiJKBP",
        "edpkv7dXhM2emnJouMb1phgvGW6fMGHjJjmo1ntyjkqGxARbdgk4T6",
        "edsk2hP48izVsHsXtqguwiNt5wq1qXdwLyxFQC8Qc72KuyKS9q88XS" );
      ( "tz1YEjis1GFsL1rKSyLtmSKQypVp1sniosVt",
        "edpkvYDUiKiMnCNSG4riBy2WSLaLEyAo763KhPFXtuBw2PMPzvTY93",
        "edsk3Dn8hFgHKxvjK89tMnU2fCrR6AxSprTM8cR9WaBZcysEa2uird" );
      ( "tz1c7arDAi3tDzAXEmYHprwuNsJiFBQJKtjc",
        "edpku6BBVDhWUBCrcVEYjjAdizR1NQGF24v5bAEL34A71oLr9QqzNo",
        "edsk2q6rzFB35micz8ZauYcUMUFyF9rVPvP3PQXZyuYPSzuEYbSMkG" );
    ]
  in
  let default_amount = Tez.of_mutez_exn 4_000_000_000_000L in
  let open Account in
  let to_account (pkh, pk, sk) =
    {
      pkh = Signature.Public_key_hash.of_b58check_exn pkh;
      pk = Signature.Public_key.of_b58check_exn pk;
      sk = Signature.Secret_key.of_b58check_exn sk;
    }
  in
  let accounts = List.map to_account initial_accounts in
  List.iter Account.add_account accounts ;
  List.map
    (fun acc -> Account.account_to_bootstrap (acc, default_amount))
    accounts

let default_genesis_parameters =
  let open Tezos_protocol_alpha_parameters in
  {
    Default_parameters.(parameters_of_constants constants_sandbox) with
    bootstrap_accounts = default_accounts;
  }

let default_patch_context ctxt =
  let shell =
    {
      Tezos_base.Block_header.level = 0l;
      proto_level = 0;
      predecessor = Test_utils.genesis.block;
      timestamp = Test_utils.genesis.time;
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.empty;
      fitness = [];
      context = Context_hash.zero;
    }
  in
  let open Tezos_protocol_alpha_parameters in
  let json = Default_parameters.json_of_parameters default_genesis_parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Context.add ctxt ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
  Context.add ctxt protocol_param_key proto_params >>= fun ctxt ->
  let ctxt = Shell_context.wrap_disk_context ctxt in
  Main.init ctxt shell >|= Environment.wrap_tzresult >>= function
  | Error _ -> assert false
  | Ok {context; _} -> return (Shell_context.unwrap_disk_context context)

(********* Baking *************)

let nb_validation_passes = List.length Main.validation_passes

let list_init_exn n f =
  List.init ~when_negative_length:(Failure "list init exn") n f |> function
  | Ok x -> x
  | _ -> assert false

let empty_operations = list_init_exn nb_validation_passes (fun _ -> [])

let apply ctxt chain_id ~policy ?(operations = empty_operations) pred =
  Forge.forge_header ctxt ?policy ~operations pred
  >>=? fun {shell; contents; baker} ->
  let protocol_data = {Block_header.contents; signature = Signature.zero} in
  (match Store.Block.block_metadata_hash pred with
  | None -> Lwt.return ctxt
  | Some hash -> Context.add_predecessor_block_metadata_hash ctxt hash)
  >>= fun context ->
  (match Store.Block.all_operations_metadata_hash pred with
  | None -> Lwt.return context
  | Some hash -> Context.add_predecessor_ops_metadata_hash context hash)
  >>= fun ctxt ->
  let element_of_key ~chain_id ~predecessor_context ~predecessor_timestamp
      ~predecessor_level ~predecessor_fitness ~predecessor ~timestamp =
    Main.value_of_key
      ~chain_id
      ~predecessor_context
      ~predecessor_timestamp
      ~predecessor_level
      ~predecessor_fitness
      ~predecessor
      ~timestamp
    >|= Environment.wrap_tzresult
    >>=? fun f -> return (fun x -> f x >|= Environment.wrap_tzresult)
  in
  let predecessor_context = Shell_context.wrap_disk_context ctxt in
  element_of_key
    ~chain_id
    ~predecessor_context
    ~predecessor_timestamp:(Store.Block.timestamp pred)
    ~predecessor_level:(Store.Block.level pred)
    ~predecessor_fitness:(Store.Block.fitness pred)
    ~predecessor:(Store.Block.hash pred)
    ~timestamp:shell.timestamp
  >>=? fun element_of_key ->
  Environment_context.Context.load_cache
    predecessor_context
    `Lazy
    element_of_key
  >>=? fun predecessor_context ->
  (let open Environment.Error_monad in
  Main.begin_construction
    ~chain_id
    ~predecessor_context
    ~predecessor_timestamp:(Store.Block.timestamp pred)
    ~predecessor_level:(Store.Block.level pred)
    ~predecessor_fitness:(Store.Block.fitness pred)
    ~predecessor:(Store.Block.hash pred)
    ~timestamp:shell.timestamp
    ~protocol_data
    ()
  >>=? fun vstate ->
  List.fold_left_es
    (fun vstate op ->
      apply_operation vstate op >>=? fun (state, _result) -> return state)
    vstate
    (List.concat operations)
  >>=? fun vstate -> Main.finalize_block vstate (Some shell))
  >|= Environment.wrap_tzresult
  >>=? fun (validation, block_header_metadata) ->
  let max_operations_ttl =
    max
      0
      (min
         (Int32.to_int (Store.Block.level pred) + 1)
         validation.max_operations_ttl)
  in
  let validation = {validation with max_operations_ttl} in
  let context = Shell_context.unwrap_disk_context validation.context in
  Context.commit ~time:shell.timestamp ?message:validation.message context
  >>= fun context_hash ->
  let block_header_metadata =
    Data_encoding.Binary.to_bytes_exn
      Main.block_header_metadata_encoding
      block_header_metadata
  in
  let shell = {shell with context = context_hash} in
  Forge.sign_header ~chain_id {baker; shell; contents} >>=? fun header ->
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn
      Main.block_header_data_encoding
      header.protocol_data
  in
  let block_hash_metadata =
    Some (Block_metadata_hash.hash_bytes [block_header_metadata])
  in
  let block_header =
    {Tezos_base.Block_header.shell = header.shell; protocol_data}
  in
  let operations_metadata_hashes =
    Some
      (List.map
         (List.map (fun r -> Operation_metadata_hash.hash_bytes [r]))
         (list_init_exn 4 (fun _ -> [])))
  in
  return
    ( block_header,
      block_header_metadata,
      block_hash_metadata,
      operations_metadata_hashes,
      validation )

let apply_and_store chain_store ?(synchronous_merge = true) ?policy
    ?(operations = empty_operations) pred =
  Store.Block.context chain_store pred >>=? fun ctxt ->
  let chain_id = Store.Chain.chain_id chain_store in
  apply ctxt chain_id ~policy ~operations pred
  >>=? fun ( block_header,
             block_header_metadata,
             block_metadata_hash,
             ops_metadata_hashes,
             validation ) ->
  let context_hash = block_header.shell.context in
  let validation_result =
    {
      Tezos_validation.Block_validation.validation_store =
        {
          context_hash;
          message = validation.Environment_context.message;
          max_operations_ttl = validation.max_operations_ttl;
          last_allowed_fork_level = validation.last_allowed_fork_level;
        };
      block_metadata = block_header_metadata;
      ops_metadata = list_init_exn 4 (fun _ -> []);
      block_metadata_hash;
      ops_metadata_hashes;
    }
  in
  let operations =
    List.map
      (List.map (fun op ->
           let op = Data_encoding.Binary.to_bytes_exn Operation.encoding op in
           Data_encoding.Binary.of_bytes_exn Tezos_base.Operation.encoding op))
      operations
  in
  Store.Block.store_block
    chain_store
    ~block_header
    ~operations
    validation_result
  >>=? function
  | Some b ->
      if synchronous_merge then (
        let block_store = Store.Unsafe.get_block_store chain_store in
        Block_store.await_merging block_store >>= fun () ->
        Store.Chain.set_head chain_store b >>=? fun _ ->
        Block_store.await_merging block_store >>= fun () ->
        (match Block_store.get_merge_status block_store with
        | Merge_failed err -> Assert.fail_msg "%a" pp_print_trace err
        | Running | Not_running -> ()) ;
        return b)
      else Store.Chain.set_head chain_store b >>=? fun _ -> return b
  | None ->
      let h = Tezos_base.Block_header.hash block_header in
      Format.eprintf "block %a already stored@." Block_hash.pp h ;
      Store.Block.read_block chain_store h

let bake chain_store ?synchronous_merge ?policy ?operation ?operations pred =
  let operations =
    match (operation, operations) with
    | (Some op, Some ops) -> Some (op :: ops)
    | (Some op, None) -> Some [op]
    | (None, Some ops) -> Some ops
    | (None, None) -> None
  in
  apply_and_store ?synchronous_merge chain_store ?policy ?operations pred

(********** Cycles ****************)

(* This function is duplicated from Context to avoid a cyclic dependency *)
let get_constants chain_store b =
  Store.Block.context chain_store b >>=? fun ctxt ->
  Alpha_services.Constants.all (rpc_ctxt ctxt) b

let bake_n chain_store ?synchronous_merge ?policy n b =
  List.fold_left_es
    (fun (bl, last) _ ->
      bake ?synchronous_merge chain_store ?policy last >>=? fun b ->
      return (b :: bl, b))
    ([], b)
    (1 -- n)
  >>=? fun (bl, last) -> return (List.rev bl, last)

let bake_until_cycle_end chain_store ?synchronous_merge ?policy b =
  get_constants chain_store b
  >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let current_level = Store.Block.level b in
  let current_level = Int32.rem current_level blocks_per_cycle in
  let delta = Int32.sub blocks_per_cycle current_level in
  bake_n ?synchronous_merge chain_store ?policy (Int32.to_int delta) b

let bake_until_n_cycle_end chain_store ?synchronous_merge ?policy n b =
  List.fold_left_es
    (fun (bll, last) _ ->
      bake_until_cycle_end chain_store ?synchronous_merge ?policy last
      >>=? fun (bl, last) -> return (bl :: bll, last))
    ([], b)
    (1 -- n)
  >>=? fun (bll, last) -> return (List.concat (List.rev bll), last)

let bake_until_cycle chain_store ?synchronous_merge ?policy cycle b =
  get_constants chain_store b
  >>=? fun Constants.{parametric = {blocks_per_cycle; _}; _} ->
  let rec loop (bl, b) =
    let current_cycle =
      let current_level = Store.Block.level b in
      let current_cycle = Int32.div current_level blocks_per_cycle in
      current_cycle
    in
    if Int32.equal (Cycle.to_int32 cycle) current_cycle then return (bl, b)
    else
      bake_until_cycle_end chain_store ?synchronous_merge ?policy b
      >>=? fun (bl', b') -> loop (bl @ bl', b')
  in
  loop ([b], b)
