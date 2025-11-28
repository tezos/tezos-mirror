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

module Assert = Assert
open Tezos_protocol_alpha
open Protocol
open Alpha_context

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
    let pkh, pk, sk = Signature.generate_key ?seed () in
    let account = {pkh; pk; sk} in
    Signature.Public_key_hash.Table.add known_accounts pkh account ;
    account

  let add_account ({pkh; _} as account) =
    Signature.Public_key_hash.Table.replace known_accounts pkh account

  let activator_account = new_account ()

  let find pkh =
    let open Lwt_result_syntax in
    match Signature.Public_key_hash.Table.find known_accounts pkh with
    | Some v -> return v
    | None -> failwith "Missing account: %a" Signature.Public_key_hash.pp pkh

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

  let account_to_bootstrap ({pkh; pk; _}, amount, delegate_to) =
    let open Parameters in
    ({
       public_key_hash = pkh;
       public_key = Some pk;
       amount;
       delegate_to;
       consensus_key = None;
     }
      : bootstrap_account)

  let commitment_secret =
    Blinded_public_key_hash.activation_code_of_hex
      "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"
    |> WithExceptions.Option.get ~loc:__LOC__

  let new_commitment ?seed () =
    let open Lwt_result_syntax in
    let pkh, pk, sk = Signature.generate_key ?seed ~algo:Ed25519 () in
    let unactivated_account = {pkh; pk; sk} in
    let open Commitment in
    let pkh = match pkh with Ed25519 pkh -> pkh | _ -> assert false in
    let bpkh = Blinded_public_key_hash.of_ed25519_pkh commitment_secret pkh in
    let*? amount = Environment.wrap_tzresult @@ Tez.(one *? 4_000L) in
    return @@ (unactivated_account, {blinded_public_key_hash = bpkh; amount})
end

let make_rpc_context ~chain_id ctxt block =
  let open Lwt_result_syntax in
  let header = Store.Block.shell_header block in
  let ({
         timestamp = predecessor_timestamp;
         level = predecessor_level;
         fitness = predecessor_fitness;
         _;
       }
        : Block_header.shell_header) =
    header
  in
  let timestamp = Time.System.to_protocol (Tezos_base.Time.System.now ()) in
  (* We need to forge a predecessor hash to pass it to [value_of_key].
     This initial context is used for RPC, hence this piece of
     information is not important and does not have to be meaningful
  *)
  let predecessor =
    Tezos_base.Block_header.hash
      {shell = header; protocol_data = Store.Block.protocol_data block}
  in
  let*! value_of_key =
    Main.value_of_key
      ~chain_id
      ~predecessor_context:ctxt
      ~predecessor_timestamp
      ~predecessor_level
      ~predecessor_fitness
      ~predecessor
      ~timestamp
  in
  let*? value_of_key =
    Tezos_protocol_alpha.Environment.wrap_tzresult value_of_key
  in
  let* ctxt =
    Tezos_protocol_environment.Context.load_cache
      (Store.Block.hash block)
      ctxt
      `Lazy
      (fun key ->
        let*! value = value_of_key key in
        let*? value = Tezos_protocol_alpha.Environment.wrap_tzresult value in
        return value)
  in
  return
  @@ new Environment.proto_rpc_context_of_directory
       (fun block ->
         {
           Environment.Updater.block_hash = Store.Block.hash block;
           block_header = Store.Block.shell_header block;
           context = ctxt;
         })
       Plugin.RPC.rpc_services

(******** Policies ***********)

(* Policies are functions that take a block and return a tuple
   [(account, level, timestamp)] for the [forge_header] function. *)

(* This type is used only to provide a simpler interface to the exterior. *)
type baker_policy =
  | By_round of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

let get_next_baker_by_round rpc_ctxt round block =
  let open Lwt_result_syntax in
  let* bakers =
    Plugin.RPC.Baking_rights.get rpc_ctxt ~all:true ~max_round:(round + 1) block
  in
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; _} =
    List.find
      (fun {Plugin.RPC.Baking_rights.round = p; _} ->
        Round.to_int32 p = Int32.of_int round)
      bakers
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  return (pkh, round, WithExceptions.Option.get ~loc:__LOC__ timestamp)

let get_next_baker_by_account rpc_ctxt pkh block =
  let open Lwt_result_syntax in
  let* bakers =
    Plugin.RPC.Baking_rights.get rpc_ctxt ~delegates:[pkh] ~max_round:256 block
  in
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; round; _} =
    List.hd bakers |> WithExceptions.Option.get ~loc:__LOC__
  in
  let*? round = Round.to_int round |> Environment.wrap_tzresult in
  return (pkh, round, WithExceptions.Option.get ~loc:__LOC__ timestamp)

let get_next_baker_excluding rpc_ctxt excludes block =
  let open Lwt_result_syntax in
  let* bakers = Plugin.RPC.Baking_rights.get rpc_ctxt ~max_round:256 block in
  let {Plugin.RPC.Baking_rights.delegate = pkh; timestamp; round; _} =
    List.find
      (fun {Plugin.RPC.Baking_rights.delegate; _} ->
        not (List.mem ~equal:Signature.Public_key_hash.equal delegate excludes))
      bakers
    |> WithExceptions.Option.get ~loc:__LOC__
  in
  let*? round = Round.to_int round |> Environment.wrap_tzresult in
  return (pkh, round, WithExceptions.Option.get ~loc:__LOC__ timestamp)

let dispatch_policy rpc_ctxt = function
  | By_round p -> get_next_baker_by_round rpc_ctxt p
  | By_account a -> get_next_baker_by_account rpc_ctxt a
  | Excluding al -> get_next_baker_excluding rpc_ctxt al

let get_next_baker chain_store ?(policy = By_round 0) =
  dispatch_policy chain_store policy

module Forge = struct
  type header = {
    baker : public_key_hash;
    (* the signer of the block *)
    shell : Block_header.shell_header;
    contents : Block_header.contents;
  }

  let default_proof_of_work_nonce =
    Bytes.create Constants.proof_of_work_nonce_size

  let make_contents ~payload_hash ~payload_round
      ?(proof_of_work_nonce = default_proof_of_work_nonce)
      ?(liquidity_baking_toggle_vote = Per_block_votes.Per_block_vote_pass)
      ~seed_nonce_hash () =
    Block_header.
      {
        payload_hash;
        payload_round;
        proof_of_work_nonce;
        seed_nonce_hash;
        per_block_votes = {liquidity_baking_vote = liquidity_baking_toggle_vote};
      }

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash
      ~proto_level ~pred_resulting_context_hash =
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        proto_level;
        validation_passes = List.length Main.validation_passes;
        context = pred_resulting_context_hash;
      }

  let set_seed_nonce_hash seed_nonce_hash {baker; shell; contents} =
    {baker; shell; contents = {contents with seed_nonce_hash}}

  let set_baker baker header = {header with baker}

  let sign_header ~chain_id {baker; shell; contents} =
    let open Lwt_result_syntax in
    let* delegate = Account.find baker in
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell, contents)
    in
    let signature =
      Signature.sign
        ~watermark:Block_header.(to_watermark (Block_header chain_id))
        delegate.sk
        unsigned_bytes
    in
    Block_header.{shell; protocol_data = {contents; signature}} |> return

  let forge_header rpc_ctxt ?(policy = By_round 0) ?timestamp ~operations pred
      pred_resulting_context_hash =
    let open Lwt_result_syntax in
    let predecessor_round =
      match Fitness.from_raw (Store.Block.fitness pred) with
      | Ok pred_fitness -> Fitness.round pred_fitness
      | _ -> Round.zero
    in
    let proto_level = Store.Block.proto_level pred in
    let* pkh, round, expected_timestamp =
      dispatch_policy rpc_ctxt policy pred
    in
    let timestamp = Option.value ~default:expected_timestamp timestamp in
    let level = Int32.succ (Store.Block.level pred) in
    let*? raw_level = Raw_level.of_int32 level |> Environment.wrap_tzresult in
    let locked_round = None in
    let*? round = Environment.wrap_tzresult (Round.of_int round) in
    let*? fitness =
      Fitness.create ~level:raw_level ~locked_round ~predecessor_round ~round
      |> Environment.wrap_tzresult
    in
    let fitness = Fitness.to_raw fitness in
    let* seed_nonce_hash =
      let+ l = Plugin.RPC.current_level ~offset:1l rpc_ctxt pred in
      match l with
      | {expected_commitment = true; _} -> Some (fst (Proto_nonce.generate ()))
      | {expected_commitment = false; _} -> None
    in
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
        ~pred_resulting_context_hash
    in
    let contents =
      make_contents
        ~payload_hash:Block_payload_hash.zero
        ~payload_round:round
        ~seed_nonce_hash
        ()
    in
    return {baker = pkh; shell; contents}
end

(********* Genesis creation *************)

(* Hard-coded context key *)
let protocol_param_key = ["protocol_parameters"]

let check_constants_consistency constants =
  let open Lwt_result_syntax in
  let open Constants_parametric_repr in
  let {blocks_per_cycle; blocks_per_commitment; _} = constants in
  let* () =
    Error_monad.unless (blocks_per_commitment <= blocks_per_cycle) (fun () ->
        failwith
          "Inconsistent constants : blocks per commitment must be less than \
           blocks per cycle")
  in
  return_unit

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
    (fun acc -> Account.account_to_bootstrap (acc, default_amount, None))
    accounts

let default_genesis_parameters =
  let open Tezos_protocol_alpha_parameters in
  {
    Default_parameters.(
      parameters_of_constants
        {
          constants_sandbox with
          consensus_threshold_size = 0;
          proof_of_work_threshold = -1L;
        })
    with
    bootstrap_accounts = default_accounts;
  }

let patch_context ctxt ~json =
  let open Lwt_syntax in
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
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  let* ctxt = Context_ops.add ctxt ["version"] (Bytes.of_string "genesis") in
  let* ctxt = Context_ops.add ctxt protocol_param_key proto_params in
  let* r = Main.init Chain_id.zero ctxt shell in
  match r with
  | Error e -> failwith "%a" Environment.Error_monad.pp_trace e
  | Ok {context; _} -> return_ok context

let default_patch_context ctxt =
  patch_context
    ctxt
    ~json:(Default_parameters.json_of_parameters default_genesis_parameters)

(********* Baking *************)

let nb_validation_passes = List.length Main.validation_passes

let empty_operations =
  WithExceptions.List.init ~loc:__LOC__ nb_validation_passes (fun _ -> [])

let begin_validation_and_application ctxt chain_id mode ~predecessor =
  let open Lwt_result_syntax in
  let* validation_state =
    Main.begin_validation ctxt chain_id mode ~predecessor
  in
  let* application_state =
    Main.begin_application ctxt chain_id mode ~predecessor
  in
  return (validation_state, application_state)

let validate_and_apply_operation (validation_state, application_state) op =
  let open Lwt_result_syntax in
  let oph = Operation.hash_packed op in
  let* validation_state = Main.validate_operation validation_state oph op in
  let* application_state, receipt =
    Main.apply_operation application_state oph op
  in
  return ((validation_state, application_state), receipt)

let finalize_validation_and_application (validation_state, application_state)
    shell_header =
  let open Lwt_result_syntax in
  let* () = Main.finalize_validation validation_state in
  Main.finalize_application application_state shell_header

let apply pred_resulting_ctxt chain_id ~policy ?(operations = empty_operations)
    pred pred_resulting_context_hash =
  let open Lwt_result_syntax in
  let* rpc_ctxt = make_rpc_context ~chain_id pred_resulting_ctxt pred in
  let element_of_key ~chain_id ~predecessor_context ~predecessor_timestamp
      ~predecessor_level ~predecessor_fitness ~predecessor ~timestamp =
    let*! f =
      Main.value_of_key
        ~chain_id
        ~predecessor_context
        ~predecessor_timestamp
        ~predecessor_level
        ~predecessor_fitness
        ~predecessor
        ~timestamp
    in
    let*? f = Environment.wrap_tzresult f in
    return (fun x ->
        let*! r = f x in
        let*? r = Environment.wrap_tzresult r in
        return r)
  in
  let* {shell; contents; baker} =
    Forge.forge_header
      rpc_ctxt
      ?policy
      ~operations
      pred
      pred_resulting_context_hash
  in
  let protocol_data = {Block_header.contents; signature = Signature.zero} in
  let*! ctxt =
    match Store.Block.block_metadata_hash pred with
    | None -> Lwt.return pred_resulting_ctxt
    | Some hash ->
        Context_ops.add_predecessor_block_metadata_hash pred_resulting_ctxt hash
  in
  let*! ctxt =
    match Store.Block.all_operations_metadata_hash pred with
    | None -> Lwt.return ctxt
    | Some hash -> Context_ops.add_predecessor_ops_metadata_hash ctxt hash
  in
  let* element_of_key =
    element_of_key
      ~chain_id
      ~predecessor_context:ctxt
      ~predecessor_timestamp:(Store.Block.timestamp pred)
      ~predecessor_level:(Store.Block.level pred)
      ~predecessor_fitness:(Store.Block.fitness pred)
      ~predecessor:(Store.Block.hash pred)
      ~timestamp:shell.timestamp
  in
  let* pred_ctxt =
    Tezos_protocol_environment.Context.load_cache
      (Store.Block.hash pred)
      ctxt
      `Lazy
      element_of_key
  in
  let* (validation, block_header_metadata), operations_results =
    let*! r =
      let* vstate =
        begin_validation_and_application
          pred_ctxt
          chain_id
          (Construction
             {
               predecessor_hash = Store.Block.hash pred;
               timestamp = shell.timestamp;
               block_header_data = protocol_data;
             })
          ~predecessor:(Store.Block.shell_header pred)
      in
      let* vstate, resultll =
        List.fold_left_es
          (fun (vstate, resultll) l ->
            let* vstate, resultl =
              List.fold_left_es
                (fun (vstate, resultl) op ->
                  let* state, result = validate_and_apply_operation vstate op in
                  return (state, result :: resultl))
                (vstate, [])
                l
            in
            return (vstate, List.rev resultl :: resultll))
          (vstate, [])
          operations
      in
      let* x = finalize_validation_and_application vstate (Some shell) in
      return (x, List.rev resultll)
    in
    let*? r = Environment.wrap_tzresult r in
    return r
  in
  let max_operations_ttl =
    max
      0
      (min
         (Int32.to_int (Store.Block.level pred) + 1)
         validation.max_operations_ttl)
  in
  let validation = {validation with max_operations_ttl} in
  let*! resulting_context_hash =
    Context_ops.commit
      ~time:shell.timestamp
      ?message:validation.message
      validation.context
  in
  let block_header_metadata =
    Data_encoding.Binary.to_bytes_exn
      Main.block_header_metadata_encoding
      block_header_metadata
  in
  let payload_hash =
    let non_consensus_operations = Stdlib.List.tl operations |> List.concat in
    let hashes = List.map Operation.hash_packed non_consensus_operations in
    Block_payload.hash
      ~predecessor_hash:shell.predecessor
      ~payload_round:contents.payload_round
      hashes
  in
  let contents = {contents with payload_hash} in
  let* header = Forge.sign_header ~chain_id {baker; shell; contents} in
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
  let operations_results =
    List.map
      (List.map
         (Data_encoding.Binary.to_bytes_exn
            Protocol.Main.operation_receipt_encoding))
      operations_results
  in
  let operations_metadata_hashes =
    Some
      (List.map
         (List.map (fun r -> Operation_metadata_hash.hash_bytes [r]))
         operations_results)
  in
  return
    ( block_header,
      block_header_metadata,
      block_hash_metadata,
      operations_results,
      operations_metadata_hashes,
      resulting_context_hash,
      validation )

let apply_and_store chain_store ?(synchronous_merge = true) ?policy
    ?(operations = empty_operations) pred =
  let open Lwt_result_syntax in
  let* pred_resulting_ctxt = Store.Block.context chain_store pred in
  let* pred_resulting_context_hash =
    Store.Block.resulting_context_hash chain_store pred
  in
  let chain_id = Store.Chain.chain_id chain_store in
  let* ( block_header,
         block_header_metadata,
         block_metadata_hash,
         ops_metadata,
         ops_metadata_hashes,
         resulting_context_hash,
         validation ) =
    apply
      pred_resulting_ctxt
      chain_id
      ~policy
      ~operations
      pred
      pred_resulting_context_hash
  in
  let ops_metadata =
    match ops_metadata_hashes with
    | Some metadata_hashes ->
        let res =
          WithExceptions.List.map2
            ~loc:__LOC__
            (WithExceptions.List.map2 ~loc:__LOC__ (fun x y ->
                 (Block_validation.Metadata x, y)))
            ops_metadata
            metadata_hashes
        in
        Block_validation.Metadata_hash res
    | None ->
        let operations_metadata =
          WithExceptions.List.init ~loc:__LOC__ 4 (fun _ -> [])
        in
        Block_validation.(No_metadata_hash operations_metadata)
  in
  let validation_result =
    {
      Tezos_validation.Block_validation.validation_store =
        {
          resulting_context_hash;
          timestamp = block_header.shell.timestamp;
          message = validation.Tezos_protocol_environment.message;
          max_operations_ttl = validation.max_operations_ttl;
          last_finalized_block_level = validation.last_finalized_block_level;
          last_preserved_block_level = validation.last_preserved_block_level;
        };
      block_metadata = (block_header_metadata, block_metadata_hash);
      ops_metadata;
      shell_header_hash = Block_validation.Shell_header_hash.zero;
    }
  in
  let operations =
    List.map
      (List.map (fun op ->
           let op = Data_encoding.Binary.to_bytes_exn Operation.encoding op in
           Data_encoding.Binary.of_bytes_exn Tezos_base.Operation.encoding op))
      operations
  in
  let* o =
    Store.Block.store_block
      chain_store
      ~block_header
      ~operations
      validation_result
  in
  match o with
  | Some b ->
      if synchronous_merge then (
        let block_store = Store.Unsafe.get_block_store chain_store in
        let*! () = Block_store.await_merging block_store in
        let* _ = Store.Chain.set_head chain_store b in
        let*! () = Block_store.await_merging block_store in
        let context_index =
          Store.context_index (Store.Chain.global_store chain_store)
        in
        let*! () = Context_ops.wait_gc_completion context_index in
        (match Block_store.get_merge_status block_store with
        | Merge_failed err -> Assert.fail_msg "%a" pp_print_trace err
        | Running | Not_running -> ()) ;
        return b)
      else
        let* _ = Store.Chain.set_head chain_store b in
        return b
  | None ->
      let h = Tezos_base.Block_header.hash block_header in
      Format.eprintf "block %a already stored@." Block_hash.pp h ;
      Store.Block.read_block chain_store h

let bake chain_store ?synchronous_merge ?policy ?operation ?operations pred =
  let operations =
    match (operation, operations) with
    | Some op, Some ops -> Some (op :: ops)
    | Some op, None -> Some [op]
    | None, Some ops -> Some ops
    | None, None -> None
  in
  apply_and_store ?synchronous_merge chain_store ?policy ?operations pred

(********** Cycles ****************)

(* This function is duplicated from Context to avoid a cyclic dependency *)
let get_constants rpc_ctxt b = Alpha_services.Constants.all rpc_ctxt b

let bake_n chain_store ?synchronous_merge ?policy n b =
  let open Lwt_result_syntax in
  let* bl, last =
    List.fold_left_es
      (fun (bl, last) _ ->
        let* b = bake ?synchronous_merge chain_store ?policy last in
        return (b :: bl, b))
      ([], b)
      (1 -- n)
  in
  return (List.rev bl, last)

let bake_until_cycle_end chain_store ?synchronous_merge ?policy b =
  let open Lwt_result_syntax in
  let* ctxt = Store.Block.context chain_store b in
  let* rpc_ctxt =
    make_rpc_context ~chain_id:(Store.Chain.chain_id chain_store) ctxt b
  in
  let* Constants.{parametric = {blocks_per_cycle; _}; _} =
    get_constants rpc_ctxt b
  in
  let current_level = Store.Block.level b in
  let current_level = Int32.rem current_level blocks_per_cycle in
  let delta = Int32.sub blocks_per_cycle current_level in
  bake_n ?synchronous_merge chain_store ?policy (Int32.to_int delta) b

let bake_until_n_cycle_end chain_store ?synchronous_merge ?policy n b =
  let open Lwt_result_syntax in
  let* bll, last =
    List.fold_left_es
      (fun (bll, last) _ ->
        let* bl, last =
          bake_until_cycle_end chain_store ?synchronous_merge ?policy last
        in
        return (bl :: bll, last))
      ([], b)
      (1 -- n)
  in
  return (List.concat (List.rev bll), last)

let bake_until_cycle chain_store ?synchronous_merge ?policy cycle b =
  let open Lwt_result_syntax in
  let* ctxt = Store.Block.context chain_store b in
  let* rpc_ctxt =
    make_rpc_context ~chain_id:(Store.Chain.chain_id chain_store) ctxt b
  in
  let* constants = get_constants rpc_ctxt b in
  let Constants.{parametric = {blocks_per_cycle; _}; _} = constants in
  let rec loop (bl, b) =
    let current_cycle =
      let current_level = Store.Block.level b in
      let current_cycle = Int32.div current_level blocks_per_cycle in
      current_cycle
    in
    if Int32.equal (Cycle.to_int32 cycle) current_cycle then return (bl, b)
    else
      let* bl', b' =
        bake_until_cycle_end chain_store ?synchronous_merge ?policy b
      in
      loop (bl @ bl', b')
  in
  loop ([b], b)

let get_constants chain_store b =
  let open Lwt_result_syntax in
  let* ctxt = Store.Block.context chain_store b in
  let* rpc_ctxt =
    make_rpc_context ~chain_id:(Store.Chain.chain_id chain_store) ctxt b
  in
  get_constants rpc_ctxt b
