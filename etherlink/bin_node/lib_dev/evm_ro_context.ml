(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  preimages : string;
  preimages_endpoint : Uri.t option;
  native_execution_policy : Configuration.native_execution_policy;
  data_dir : string;
  store : Evm_store.t;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  index : Pvm.Context.ro_index;
  finalized_view : bool;
  execution_pool : Lwt_domain.pool;
  trace_host_funs : bool;
}

let get_evm_state ctxt hash =
  let open Lwt_result_syntax in
  Pvm.Context.reload ctxt.index ;
  let*! context = Pvm.Context.checkout_exn ctxt.index hash in
  let*! res = Pvm.State.get context in
  return res

let read state path = Durable_storage.read_opt (Raw_path path) state

let with_latest_state ctxt f =
  let open Lwt_result_syntax in
  let* _, hash = Evm_store.(use ctxt.store Context_hashes.get_latest) in
  let* evm_state = get_evm_state ctxt hash in
  f evm_state

let read_chain_family ctxt chain_id =
  with_latest_state ctxt (fun state ->
      Durable_storage.read (Chain_config_family chain_id) state)

let read_enable_multichain_flag ctxt =
  with_latest_state ctxt (Durable_storage.exists Multichain_flag)

let chain_id ctxt = with_latest_state ctxt (Durable_storage.read Chain_id)

let michelson_runtime_chain_id ctxt =
  with_latest_state ctxt (Durable_storage.read Michelson_runtime_chain_id)

let michelson_activation_level ctxt =
  let open Lwt_result_syntax in
  let* qty_opt =
    with_latest_state
      ctxt
      (Durable_storage.read_opt Michelson_runtime_sunrise_level)
  in
  return (Option.map (fun (Ethereum_types.Qty z) -> Z.to_int64 z) qty_opt)

let current_block_number_durable ctxt ~chain_family =
  with_latest_state
    ctxt
    (Durable_storage.read (Current_block_number chain_family))

let storage_version ctxt =
  with_latest_state ctxt Durable_storage.storage_version

let list_runtimes ctxt = with_latest_state ctxt Durable_storage.list_runtimes

let list_l1_l2_levels ctxt ~from_l1_level =
  let open Lwt_result_syntax in
  Evm_store.use ctxt.store @@ fun conn ->
  let* last = Evm_store.L1_l2_finalized_levels.last conn in
  match last with
  | None -> return_nil
  | Some (end_l1_level, _) ->
      Evm_store.L1_l2_finalized_levels.list_by_l1_levels
        conn
        ~start_l1_level:from_l1_level
        ~end_l1_level

let l2_levels_of_l1_level ctxt l1_level =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.L1_l2_finalized_levels.find conn ~l1_level

(* [chain_family] is currently ignored because the store uses a single
   block numbering scheme shared across chain families. The parameter is
   kept for forward compatibility. *)
let block_param_to_block_number ctxt ~chain_family:_ ?hash_column
    (block_param : Ethereum_types.Block_parameter.extended) =
  let open Lwt_result_syntax in
  match block_param with
  | Block_hash {hash; _} -> (
      Evm_store.use ctxt.store @@ fun conn ->
      let* res =
        match hash_column with
        | Some `Michelson -> Evm_store.Blocks.find_number_of_tez_hash conn hash
        | None | Some `Evm -> Evm_store.Blocks.find_number_of_hash conn hash
      in
      match res with
      | Some number -> return number
      | None -> failwith "Missing block %a" Ethereum_types.pp_block_hash hash)
  | Block_parameter (Number n) -> return n
  | Block_parameter (Latest | Pending) when ctxt.finalized_view -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_finalized) in
      match res with
      | Some (latest, _) -> return latest
      | None -> failwith "The EVM node does not have any state available")
  | Block_parameter (Latest | Pending) -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_latest) in
      match res with
      | Some (latest, _) -> return latest
      | None -> failwith "The EVM node does not have any state available")
  | Block_parameter Earliest -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_earliest) in
      match res with
      | Some (earliest, _) -> return earliest
      | None -> failwith "The EVM node does not have any state available")
  | Block_parameter Finalized -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_finalized) in
      match res with
      | Some (finalized, _) -> return finalized
      | None -> failwith "The EVM node is not aware of any finalized block")

let single_chain_id_and_family ctxt ~(config : Configuration.t)
    ~enable_multichain =
  let open Lwt_result_syntax in
  match (config.experimental_features.l2_chains, enable_multichain) with
  | None, false -> return (None, L2_types.Ex_chain_family EVM)
  | None, true -> tzfail Node_error.Singlechain_node_multichain_kernel
  | Some [l2_chain], false ->
      let*! () = Events.multichain_node_singlechain_kernel () in
      return (Some l2_chain.chain_id, L2_types.Ex_chain_family EVM)
  | Some [l2_chain], true ->
      let chain_id = l2_chain.chain_id in
      let* chain_family = read_chain_family ctxt chain_id in
      if l2_chain.chain_family = chain_family then
        return (Some chain_id, chain_family)
      else
        tzfail
          (Node_error.Mismatched_chain_family
             {
               chain_id;
               node_family = l2_chain.chain_family;
               kernel_family = chain_family;
             })
  | _ -> tzfail Node_error.Unexpected_multichain

(* Block storage operations (store-backed) *)

let current_block_number ctxt =
  block_param_to_block_number
    ctxt
    ~chain_family:(L2_types.Ex_chain_family EVM)
    (Block_parameter Latest)

let nth_block ctxt ~full_transaction_object level =
  let open Lwt_result_syntax in
  Evm_store.use ctxt.store @@ fun conn ->
  let* block_opt =
    Evm_store.Blocks.find_with_level ~full_transaction_object conn (Qty level)
  in
  match block_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block -> return block

(* Return [Some block] if a block with [hash] is known to the store,
   [None] if the store was reachable but has no such block, and propagate
   any genuine store/DB error unchanged. Callers that want a "not found"
   to surface as an error should use {!block_by_hash}; callers that want
   to distinguish "unknown" from "DB error" should use this variant. *)
let block_by_hash_opt ctxt ~full_transaction_object hash =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Blocks.find_with_hash ~full_transaction_object conn hash

let block_by_hash ctxt ~full_transaction_object hash =
  let open Lwt_result_syntax in
  let* block_opt = block_by_hash_opt ctxt ~full_transaction_object hash in
  match block_opt with
  | None -> failwith "Block %a not found" Ethereum_types.pp_block_hash hash
  | Some block -> return block

let block_receipts ctxt level =
  let open Lwt_result_syntax in
  Evm_store.use ctxt.store @@ fun conn ->
  let* found = Evm_store.Blocks.find_hash_of_number conn (Qty level) in
  match found with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some _hash ->
      Evm_store.Transactions.receipts_of_block_number conn (Qty level)

let block_range_receipts ctxt ?mask level len =
  let open Lwt_result_syntax in
  Evm_store.use ctxt.store @@ fun conn ->
  let start = Ethereum_types.Qty level in
  let finish = Ethereum_types.Qty Z.(pred (level + of_int len)) in
  let* found1 = Evm_store.Blocks.find_hash_of_number conn start in
  let* found2 = Evm_store.Blocks.find_hash_of_number conn finish in
  match (found1, found2) with
  | None, _ | _, None ->
      failwith
        "Block range [%a, %a] unavailable"
        Ethereum_types.pp_quantity
        start
        Ethereum_types.pp_quantity
        finish
  | _ -> Evm_store.Transactions.receipts_of_block_range ?mask conn start len

let transaction_receipt ctxt hash =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Transactions.find_receipt conn hash

let transaction_object ctxt hash =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Transactions.find_object conn hash

let network_sanity_check ~network ctxt =
  let open Lwt_result_syntax in
  let expected_smart_rollup_address = Constants.rollup_address network in
  let (Chain_id expected_chain_id) = Configuration.chain_id network in

  let* _, hash = Evm_store.(use ctxt.store Context_hashes.get_latest) in
  let* evm_state = get_evm_state ctxt hash in
  let*! chain_id = Durable_storage.read Chain_id evm_state in

  let* () =
    match chain_id with
    | Ok (Chain_id chain_id) ->
        unless Compare.Z.(chain_id = expected_chain_id) @@ fun () ->
        failwith
          "Local state is inconsistent with selected network %a: incorrect \
           chain id (%a instead of %a)"
          Configuration.pp_supported_network
          network
          Z.pp_print
          chain_id
          Z.pp_print
          expected_chain_id
    | Error _ ->
        (* The chain id was not already set, which necessarily means we are
           bootstrapping a chain from scratch. The smart rollup address check
           will be enough. *)
        let*! () = Events.missing_chain_id () in
        return_unit
  in

  let* () =
    unless Address.(ctxt.smart_rollup_address = expected_smart_rollup_address)
    @@ fun () ->
    failwith
      "Smart rollup address is inconsistent with selected network %a: %a \
       instead of %a"
      Configuration.pp_supported_network
      network
      Address.pp
      ctxt.smart_rollup_address
      Address.pp
      expected_smart_rollup_address
  in

  return_unit

let load ~pool ?network ?smart_rollup_address (configuration : Configuration.t)
    =
  let open Lwt_result_syntax in
  let* store =
    Evm_store.init
      ~chain_family:L2_types.EVM
      ~data_dir:configuration.data_dir
      ~perm:(Read_only {pool_size = configuration.db.pool_size})
      ?max_conn_reuse_count:configuration.db.max_conn_reuse_count
      ()
  in
  let* index =
    Pvm.Context.(
      load
        (module Pvm.Irmin_context)
        ~cache_size:100_000
        Read_only
        (Evm_state.irmin_store_path ~data_dir:configuration.data_dir))
  in
  let* smart_rollup_address =
    match smart_rollup_address with
    | None ->
        let* metadata = Evm_store.(use store Metadata.get) in
        return metadata.smart_rollup_address
    | Some smart_rollup_address -> return smart_rollup_address
  in
  let ctxt =
    {
      store;
      index;
      data_dir = configuration.data_dir;
      preimages = Configuration.preimages_path configuration;
      preimages_endpoint = configuration.kernel_execution.preimages_endpoint;
      native_execution_policy =
        configuration.kernel_execution.native_execution_policy;
      smart_rollup_address;
      finalized_view = configuration.finalized_view;
      execution_pool = pool;
      trace_host_funs = configuration.opentelemetry.trace_host_functions;
    }
  in

  let+ () =
    match network with
    | Some network -> network_sanity_check ~network ctxt
    | None -> return_unit
  in

  ctxt

let find_latest_hash ctxt =
  let open Lwt_result_syntax in
  let* res = Evm_store.(use ctxt.store Context_hashes.find_latest) in
  match res with
  | Some (_, hash) -> return hash
  | None -> failwith "No state available"

let find_finalized_hash ctxt =
  let open Lwt_result_syntax in
  let* res = Evm_store.(use ctxt.store Context_hashes.find_finalized) in
  match res with
  | Some (_, hash) -> return hash
  | None -> failwith "No state available"

let get_irmin_hash_from_number ctxt number =
  let open Lwt_result_syntax in
  let* res =
    Evm_store.(use ctxt.store @@ fun conn -> Context_hashes.find conn number)
  in
  match res with
  | Some hash -> return hash
  | None ->
      failwith
        "No state available for block %a"
        Ethereum_types.pp_quantity
        number

let find_irmin_hash ctxt (block : Ethereum_types.Block_parameter.extended) =
  let open Lwt_result_syntax in
  match block with
  | Block_parameter (Latest | Pending) when ctxt.finalized_view ->
      find_finalized_hash ctxt
  | Block_parameter (Latest | Pending) -> find_latest_hash ctxt
  | Block_parameter Earliest -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_earliest) in
      match res with
      | Some (_, hash) -> return hash
      | None -> failwith "No state available")
  | Block_parameter Finalized -> find_finalized_hash ctxt
  | Block_parameter (Number number) -> (
      let* res =
        Evm_store.(
          use ctxt.store @@ fun conn -> Context_hashes.find conn number)
      in
      match res with
      | Some hash -> return hash
      | None ->
          failwith
            "No state available for block %a"
            Ethereum_types.pp_quantity
            number)
  | Block_hash {hash; require_canonical = _} -> (
      Evm_store.use ctxt.store @@ fun conn ->
      let* context_hash_opt = Evm_store.context_hash_of_block_hash conn hash in
      match context_hash_opt with
      | Some context_hash -> return context_hash
      | None -> failwith "Unknown block %a" Ethereum_types.pp_block_hash hash)

let get_state ctxt
    ?(block = Ethereum_types.Block_parameter.Block_parameter Latest) () =
  let open Lwt_result_syntax in
  let* hash = find_irmin_hash ctxt block in
  get_evm_state ctxt hash

let read_state = read

let subkeys state path = Durable_storage.subkeys path state

let entrypoint_config ctxt =
  Pvm.Kernel.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:false
    ~destination:ctxt.smart_rollup_address
    ~trace_host_funs:ctxt.trace_host_funs
    ()

let execute_entrypoint ctxt state ~input_path ~input ~output_path ~entrypoint =
  let open Lwt_result_syntax in
  let config = entrypoint_config ctxt in
  let* result =
    Evm_state.execute_entrypoint
      ~data_dir:ctxt.data_dir
      ~pool:ctxt.execution_pool
      ~native_execution_policy:Configuration.Always
      ~config
      state
      ~input_path
      ~input
      ~output_path
      ~entrypoint
  in
  return result

let execute_entrypoint_with_insights ctxt state ~input_path ~input
    ~insight_requests ~entrypoint =
  let open Lwt_result_syntax in
  let config = entrypoint_config ctxt in
  let* state = Durable_storage.write (Raw_path input_path) input state in
  let execution_input =
    Simulation.Encodings.
      {
        messages = [];
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = None;
      }
  in
  let* raw_insights =
    Evm_state.execute_and_inspect
      ~pool:ctxt.execution_pool
      ~native_execution_policy:Configuration.Always
      ~config
      ~data_dir:ctxt.data_dir
      ~wasm_entrypoint:entrypoint
      ~input:execution_input
      state
  in
  return raw_insights

let pvm_config ctxt =
  Pvm.Kernel.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ~trace_host_funs:ctxt.trace_host_funs
    ()

(** [promote_native_execution ctxt] promotes [Rpcs_only] to [Always].
    The read-only context is only used to serve RPCs, so [replay] and
    [execute] are only used for serving RPCs and it is safe to enable
    native execution unconditionally.  Without this, the node would
    believe it is executing a block and default to WASM execution. *)
let promote_native_execution ctxt =
  match ctxt.native_execution_policy with
  | Rpcs_only -> {ctxt with native_execution_policy = Configuration.Always}
  | _ -> ctxt

let execution_gas ~base_fee_per_gas ~da_fee_per_byte receipt object_ =
  let authorization_list_len =
    List.length (Transaction_object.authorization_list object_)
  in
  let da_fees =
    Fees.gas_used_for_da_fees
      ~da_fee_per_byte
      ~base_fee_per_gas
      ~authorization_list_len
      Ethereum_types.(Transaction_object.input object_ |> hex_to_real_bytes)
  in
  let (Qty gas_used) = receipt.Transaction_receipt.gasUsed in
  Z.sub gas_used da_fees

let cumulative_execution_gas ~base_fee_per_gas ~da_fee_per_byte ctxt =
  let open Lwt_result_syntax in
  function
  | L2_types.Eth block ->
      let hashes =
        match block.transactions with
        | TxFull _l -> assert false
        | TxHash l -> l
      in
      let+ result =
        List.fold_left_es
          (fun cumulative_execution_gas hash ->
            let* object_ =
              Evm_store.(
                use ctxt.store @@ fun conn -> Transactions.find_object conn hash)
            in
            let object_ = WithExceptions.Option.get ~loc:__LOC__ object_ in
            let+ receipt =
              Evm_store.(
                use ctxt.store @@ fun conn ->
                Transactions.find_receipt conn hash)
            in
            let receipt = WithExceptions.Option.get ~loc:__LOC__ receipt in
            Z.add cumulative_execution_gas
            @@ execution_gas ~base_fee_per_gas ~da_fee_per_byte receipt object_)
          Z.zero
          hashes
      in
      Ethereum_types.Qty result
  | Tez _ -> return (Ethereum_types.Qty Z.zero)

type replay_result =
  | Replay_success of {
      block : Ethereum_types.legacy_transaction_object L2_types.block;
      evm_state : Evm_state.t;
      diverged : bool;
      process_time : Ptime.span;
      execution_gas : Ethereum_types.quantity;
      tezos_block : L2_types.Tezos_block.t option;
    }
  | Replay_failure

type replay_strategy = Blueprint | Assemble

let apply_blueprint ?log_file ?profile ctxt blueprint evm_state =
  let open Lwt_result_syntax in
  let*? chunks =
    Sequencer_blueprint.chunks_of_external_messages
      blueprint.Blueprint_types.blueprint.payload
  in
  (* We are replaying, so we can assume the signatures are correct *)
  let chunks = Sequencer_blueprint.unsafe_drop_signatures chunks in
  Evm_state.apply_unsigned_chunks
    ~pool:ctxt.execution_pool
    ?log_file
    ?profile
    ~data_dir:ctxt.data_dir
    ~chain_family:EVM
    ~config:(pvm_config ctxt)
    ~native_execution_policy:ctxt.native_execution_policy
    evm_state
    chunks

let assemble_blueprint ?log_file ?profile ctxt blueprint evm_state =
  let open Lwt_result_syntax in
  let* storage_version = storage_version ctxt in
  let*? txns =
    Blueprint_decoder.transactions blueprint.Blueprint_types.blueprint.payload
  in
  if txns = [] then apply_blueprint ?log_file ?profile ctxt blueprint evm_state
  else
    let* txns =
      List.map_es
        (function
          | hash, Some txn -> return (hash, Broadcast.Common txn)
          | hash, None -> (
              let* sql_res =
                Evm_store.(
                  use ctxt.store @@ fun conn ->
                  Delayed_transactions.at_hash conn hash)
              in
              match sql_res with
              | Some txn -> return (hash, Broadcast.Delayed txn)
              | None ->
                  failwith
                    "Missing delayed transaction %a"
                    Ethereum_types.pp_hash
                    hash))
        txns
    in
    let* evm_state, _ =
      List.fold_left_es
        (fun (evm_state, idx) (hash, txn) ->
          let* _, evm_state =
            Evm_state.execute_single_transaction
              ~storage_version
              ~data_dir:ctxt.data_dir
              ~pool:ctxt.execution_pool
              ~native_execution:(ctxt.native_execution_policy = Always)
              ~config:(pvm_config ctxt)
              evm_state
              {
                timestamp = blueprint.blueprint.timestamp;
                number = blueprint.blueprint.number;
                transactions_count = idx;
              }
              hash
              txn
          in
          return (evm_state, Int32.succ idx))
        (evm_state, 0l)
        txns
    in

    Evm_state.assemble_block
      ~pool:ctxt.execution_pool
      ~data_dir:ctxt.data_dir
      ~chain_family:EVM
      ~timestamp:blueprint.blueprint.timestamp
      ~number:blueprint.blueprint.number
      ~native_execution:(ctxt.native_execution_policy = Always)
      ~config:(pvm_config ctxt)
      evm_state

let replay ctxt ?log_file ?profile ?evm_state
    ?(alter_evm_state = Lwt_result_syntax.return) strategy
    (Ethereum_types.Qty number) =
  let open Lwt_result_syntax in
  let* evm_state =
    match evm_state with
    | Some evm_state -> return evm_state
    | None ->
        let* hash = get_irmin_hash_from_number ctxt (Qty (Z.pred number)) in
        get_evm_state ctxt hash
  in
  let* evm_state = alter_evm_state evm_state in
  let* blueprint =
    Evm_store.use ctxt.store @@ fun conn ->
    Evm_store.Blueprints.get_with_events conn (Qty number)
  in
  let* expected_block =
    Evm_store.use ctxt.store @@ fun conn ->
    Evm_store.Blocks.get_with_level
      ~full_transaction_object:false
      conn
      (Qty number)
  in
  let log_file =
    Option.map
      (fun name -> Printf.sprintf "%s_%s" name (Z.to_string number))
      log_file
  in
  let*! () = Evm_state.preload_kernel ~pool:ctxt.execution_pool evm_state in
  let process_time = ref (Ptime.Span.of_int_s 0) in
  let* apply_result =
    Misc.with_timing (fun dt ->
        process_time := dt ;
        Lwt.return_unit)
    @@ fun () ->
    match strategy with
    | Blueprint -> apply_blueprint ?log_file ?profile ctxt blueprint evm_state
    | Assemble -> assemble_blueprint ?log_file ?profile ctxt blueprint evm_state
  in

  match apply_result with
  | Apply_success {block; evm_state; tezos_block} ->
      let* (Qty base_fee_per_gas) =
        Etherlink_durable_storage.base_fee_per_gas evm_state
      in
      let* da_fee_per_byte =
        Etherlink_durable_storage.da_fee_per_byte evm_state
      in
      let* execution_gas =
        cumulative_execution_gas ~base_fee_per_gas ~da_fee_per_byte ctxt block
      in
      return
        (Replay_success
           {
             block;
             evm_state;
             diverged = L2_types.block_hash block <> expected_block.hash;
             process_time = !process_time;
             execution_gas;
             tezos_block;
           })
  | Apply_failure -> return Replay_failure

module Etherlink = struct
  let balance ctxt address block_param =
    let open Lwt_result_syntax in
    let* state = get_state ctxt ~block:block_param () in
    Etherlink_durable_storage.balance state address

  let nonce ctxt address block_param =
    let open Lwt_result_syntax in
    let* state = get_state ctxt ~block:block_param () in
    Etherlink_durable_storage.nonce state address

  let code ctxt address block_param =
    let open Lwt_result_syntax in
    let* state = get_state ctxt ~block:block_param () in
    Etherlink_durable_storage.code state address

  let storage_at ctxt address position block_param =
    let open Lwt_result_syntax in
    let* state = get_state ctxt ~block:block_param () in
    Etherlink_durable_storage.storage_at state address position

  let base_fee_per_gas ctxt =
    with_latest_state ctxt Etherlink_durable_storage.base_fee_per_gas

  let backlog ctxt = with_latest_state ctxt Etherlink_durable_storage.backlog

  let minimum_base_fee_per_gas ctxt =
    with_latest_state ctxt Etherlink_durable_storage.minimum_base_fee_per_gas

  let coinbase ctxt = with_latest_state ctxt Etherlink_durable_storage.coinbase

  let replay ctxt number =
    let open Lwt_result_syntax in
    let ctxt = promote_native_execution ctxt in
    let* result = replay ctxt ~log_file:"replay_rpc" Blueprint number in
    match result with
    | Replay_success {block = Eth block; _} -> return block
    | Replay_success {block = Tez _; _} ->
        failwith "Could not replay a tezlink block"
    | Replay_failure -> failwith "Could not replay the block"
end

let make_executor ctxt =
  let ctxt = promote_native_execution ctxt in
  let pvm_config = pvm_config ctxt in
  (module struct
    let replay ?log_file ?profile ?alter_evm_state number =
      let open Lwt_result_syntax in
      let+ result =
        replay ctxt ?log_file ?profile ?alter_evm_state Blueprint number
      in
      match result with
      | Replay_success {block; evm_state; _} ->
          Evm_state.Apply_success {block; evm_state; tezos_block = None}
      | Replay_failure -> Apply_failure

    let execute ?(alter_evm_state = Lwt_result_syntax.return) input block =
      let open Lwt_result_syntax in
      let native_execution =
        match ctxt.native_execution_policy with
        | Always | Rpcs_only -> true
        | Never -> false
      in
      let message = Simulation.Encodings.(input.messages) in
      let* hash = find_irmin_hash ctxt block in
      let* evm_state = get_evm_state ctxt hash in
      let* evm_state = alter_evm_state evm_state in
      Evm_state.execute
        ~pool:ctxt.execution_pool
        ?log_file:input.log_kernel_debug_file
        ~data_dir:ctxt.data_dir
        ~config:pvm_config
        ~native_execution
        evm_state
        (`Inbox message)
  end : Evm_execution.S)

module Http_tracer = struct
  (* Enable per-transaction HTTP trace capture for the upcoming replay by
     writing to the durable storage flag consumed by the kernel. *)
  let set_http_trace_flag state =
    Durable_storage.write
      (Raw_path Durable_storage_path.Http_trace.enabled_flag)
      (Bytes.of_string "\001")
      state

  (* Read the RLP-encoded HTTP traces the kernel wrote for a single
     transaction during a replay triggered by [set_http_trace_flag]. A missing
     key means the transaction performed no cross-runtime HTTP call — return
     the empty list rather than an error. Decoding failures preserve the
     underlying error (instead of collapsing it to a fixed message) so the
     cause surfaces in RPC error responses.

     The kernel keys traces by [hex::encode(tx_hash)] (lowercase, no [0x]
     prefix). [Ethereum_types.Hex] already strips the [0x] prefix, but the
     case of the inner string is whatever the caller passed in through the
     JSON-RPC request. Lowercase here to make the lookup case-insensitive
     and match the kernel-side convention, otherwise an uppercase hash
     provided by the caller would silently return the empty list (no key
     at the mixed-case path). *)
  let read_http_traces_for state transaction_hash =
    let open Lwt_result_syntax in
    let (Ethereum_types.Hash (Hex hash)) = transaction_hash in
    let hash = String.lowercase_ascii hash in
    let path = Durable_storage_path.Http_trace.for_tx ~transaction_hash:hash in
    let* bytes_opt = Durable_storage.read_opt (Raw_path path) state in
    match bytes_opt with
    | None -> return []
    | Some bytes -> (
        match Simulation.decode_http_traces bytes with
        | Ok traces -> return traces
        | Error err ->
            failwith
              "Failed to decode HTTP traces for %a from kernel output: %a"
              Ethereum_types.pp_hash
              transaction_hash
              pp_print_trace
              err)

  (* Extract the transaction hashes of [block] in block order. *)
  let tx_hashes_of_block (block : _ Ethereum_types.block) =
    match block.transactions with
    | TxHash hashes -> hashes
    | TxFull objs -> List.map Transaction_object.hash objs

  (* Replay [block_number] with HTTP trace capture enabled and return the
     resulting EVM state. Centralizes the [make_executor]/[Exe.replay]/
     [Apply_failure → Trace_not_found] boilerplate so all three trace_*
     entry points share the same replay path. *)
  let replay_with_http_trace_flag ctxt block_number =
    let open Lwt_result_syntax in
    let (module Exe) = make_executor ctxt in
    let* apply_result =
      Exe.replay ~alter_evm_state:set_http_trace_flag block_number
    in
    match apply_result with
    | Evm_state.Apply_failure -> tzfail Tracer_types.Trace_not_found
    | Evm_state.Apply_success {evm_state; _} -> return evm_state

  (* Replay [block_number] with HTTP trace capture enabled and return the
     list of [(tx_hash, traces)] entries for [tx_hashes]. Short-circuits
     on empty blocks to avoid a full-block replay that would produce no
     work. *)
  let traces_for_block_with_hashes ctxt block_number tx_hashes =
    let open Lwt_result_syntax in
    match tx_hashes with
    | [] -> return []
    | _ :: _ ->
        let* evm_state = replay_with_http_trace_flag ctxt block_number in
        List.map_es
          (fun hash ->
            let+ traces = read_http_traces_for evm_state hash in
            (hash, traces))
          tx_hashes

  let trace_transaction ctxt transaction_hash =
    let open Lwt_result_syntax in
    let* receipt = transaction_receipt ctxt transaction_hash in
    match receipt with
    | None -> tzfail (Tracer_types.Transaction_not_found transaction_hash)
    | Some Transaction_receipt.{blockNumber; _} ->
        let* evm_state = replay_with_http_trace_flag ctxt blockNumber in
        read_http_traces_for evm_state transaction_hash

  let trace_block ctxt block_number =
    let open Lwt_result_syntax in
    let (Ethereum_types.Qty block_z) = block_number in
    let* block = nth_block ctxt ~full_transaction_object:false block_z in
    traces_for_block_with_hashes ctxt block_number (tx_hashes_of_block block)

  let trace_block_by_hash ctxt block_hash =
    let open Lwt_result_syntax in
    (* Use the [_opt] variant so we can distinguish "hash unknown to the
       store" (the only case we translate to [Block_hash_not_found]) from
       a genuine store/DB error — which should surface with its original
       trace rather than being relabelled as "unknown hash". *)
    let* block_opt =
      block_by_hash_opt ctxt ~full_transaction_object:false block_hash
    in
    match block_opt with
    | None -> tzfail (Tracer_types.Block_hash_not_found block_hash)
    | Some block ->
        traces_for_block_with_hashes
          ctxt
          block.number
          (tx_hashes_of_block block)
end

module Tracer_etherlink = struct
  let trace_transaction ctxt transaction_hash config =
    let open Lwt_result_syntax in
    let* receipt = transaction_receipt ctxt transaction_hash in
    match receipt with
    | None -> tzfail (Tracer_types.Transaction_not_found transaction_hash)
    | Some Transaction_receipt.{blockNumber; _} ->
        Tracer.trace_transaction
          (make_executor ctxt)
          ~block_number:blockNumber
          ~transaction_hash
          ~config

  let trace_call ctxt call block config =
    Tracer.trace_call (make_executor ctxt) ~call ~block ~config

  let trace_block ctxt block_number config =
    let (module Executor) = make_executor ctxt in
    let module Storage = struct
      let current_block_number () = current_block_number ctxt

      let nth_block = nth_block ctxt

      let block_by_hash = block_by_hash ctxt

      let block_receipts = block_receipts ctxt

      let block_range_receipts = block_range_receipts ctxt

      let transaction_receipt = transaction_receipt ctxt

      let transaction_object = transaction_object ctxt
    end in
    Tracer.trace_block (module Executor) (module Storage) ~block_number ~config
end

let tezlink_nth_block ctxt level =
  let open Lwt_result_syntax in
  Evm_store.use ctxt.store @@ fun conn ->
  let* block_opt = Evm_store.Blocks.tez_find_with_level conn (Qty level) in
  match block_opt with
  | None -> failwith "Block %a not found" Z.pp_print level
  | Some block -> return block

let tezlink_nth_block_hash ctxt level =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Blocks.find_hash_of_number conn (Qty level)

let tezosx_nth_block ctxt level =
  let open Lwt_result_syntax in
  Evm_store.use ctxt.store @@ fun conn ->
  let* block_opt =
    Evm_store.Blocks.tezosx_find_tez_block_with_level conn (Qty level)
  in
  match block_opt with
  | None -> failwith "TezosX Tezos block %a not found" Z.pp_print level
  | Some block -> return block

let tezosx_nth_block_hash ctxt level =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Blocks.find_tez_hash_of_number conn (Qty level)

let meta_block_hashes_of_number ctxt level =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Blocks.find_hashes_of_number conn (Qty level)

let meta_block_number_of_hash ctxt hash =
  Evm_store.use ctxt.store @@ fun conn ->
  match (hash : Meta_block.block_hash_identifier) with
  | Evm hash -> Evm_store.Blocks.find_number_of_hash conn hash
  | Michelson hash ->
      let hash =
        Ethereum_types.block_hash_of_bytes (Block_hash.to_bytes hash)
      in
      Evm_store.Blocks.find_number_of_tez_hash conn hash

let next_blueprint_number ctxt =
  let open Lwt_result_syntax in
  let* Qty current_block_number, _ =
    Evm_store.use ctxt.store Evm_store.Context_hashes.get_latest
  in
  return (Ethereum_types.Qty Z.(succ current_block_number))

let preload_kernel_from_level ctxt level =
  let open Lwt_result_syntax in
  let* hash =
    Evm_store.(use ctxt.store @@ fun conn -> Context_hashes.find conn level)
  in
  match hash with
  | Some hash ->
      let* evm_state = get_evm_state ctxt hash in
      let*! () = Evm_state.preload_kernel ~pool:ctxt.execution_pool evm_state in
      return_unit
  | None -> return_unit

let preload_known_kernels ctxt =
  let open Lwt_result_syntax in
  let* activation_levels =
    Evm_store.use ctxt.store Evm_store.Kernel_upgrades.activation_levels
  in
  let* earliest_info =
    Evm_store.use ctxt.store Evm_store.Context_hashes.find_earliest
  in
  let earliest_level =
    Option.fold ~none:[] ~some:(fun (l, _) -> [l]) earliest_info
  in
  List.iter_ep
    (preload_kernel_from_level ctxt)
    (earliest_level @ activation_levels)

type evm_services_methods = {
  next_blueprint_number : unit -> Ethereum_types.quantity Lwt.t;
  find_blueprint :
    Ethereum_types.quantity -> Blueprint_types.with_events option tzresult Lwt.t;
  find_blueprint_legacy :
    Ethereum_types.quantity ->
    Blueprint_types.Legacy.with_events option tzresult Lwt.t;
  smart_rollup_address : Address.t;
  time_between_blocks : Evm_node_config.Configuration.time_between_blocks;
}

let evm_services_methods ctxt time_between_blocks =
  {
    next_blueprint_number =
      (fun () ->
        let open Lwt_syntax in
        let+ res = next_blueprint_number ctxt in
        match res with
        | Ok res -> res
        | Error _ -> Stdlib.failwith "Couldn't fetch next blueprint number");
    find_blueprint_legacy =
      (fun level ->
        Evm_store.use ctxt.store (fun conn ->
            Evm_store.Blueprints.find_with_events_legacy conn level));
    find_blueprint =
      (fun level ->
        Evm_store.use ctxt.store (fun conn ->
            Evm_store.Blueprints.find_with_events conn level));
    smart_rollup_address = ctxt.smart_rollup_address;
    time_between_blocks;
  }

let blueprints_range ctxt ~from ~to_ =
  Evm_store.use ctxt.store @@ fun conn ->
  Evm_store.Blueprints.find_range conn ~from ~to_
