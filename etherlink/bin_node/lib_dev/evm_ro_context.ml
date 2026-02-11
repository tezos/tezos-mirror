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

let read state path =
  let open Lwt_result_syntax in
  let*! res = Evm_state.inspect state path in
  return res

let read_chain_family ctxt chain_id =
  let open Lwt_result_syntax in
  let* _, hash = Evm_store.(use ctxt.store Context_hashes.get_latest) in
  let* evm_state = get_evm_state ctxt hash in
  let* chain_family = Durable_storage.chain_family (read evm_state) chain_id in
  return chain_family

let read_enable_multichain_flag ctxt =
  let open Lwt_result_syntax in
  let* _, hash = Evm_store.(use ctxt.store Context_hashes.get_latest) in
  let* evm_state = get_evm_state ctxt hash in
  Durable_storage.is_multichain_enabled (read evm_state)

let network_sanity_check ~network ctxt =
  let open Lwt_result_syntax in
  let expected_smart_rollup_address = Constants.rollup_address network in
  let (Chain_id expected_chain_id) = Configuration.chain_id network in

  let* _, hash = Evm_store.(use ctxt.store Context_hashes.get_latest) in
  let* evm_state = get_evm_state ctxt hash in
  let*! chain_id = Durable_storage.chain_id (read evm_state) in

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

module MakeBackend (Ctxt : sig
  val ctxt : t

  val evm_node_endpoint : Uri.t option

  val keep_alive : bool

  val timeout : float

  val execution_pool : Lwt_domain.pool
end) =
struct
  module Reader = struct
    type state = Evm_state.t

    let get_state
        ?(block = Ethereum_types.Block_parameter.Block_parameter Latest) () =
      let open Lwt_result_syntax in
      let* hash = find_irmin_hash Ctxt.ctxt block in
      get_evm_state Ctxt.ctxt hash

    let read = read

    let subkeys state path =
      let open Lwt_result_syntax in
      let*! res = Evm_state.subkeys state path in
      return res
  end

  module TxEncoder = struct
    type transactions = (string * Transaction_object.t) list

    type messages = string list

    let encode_transactions ~smart_rollup_address:_ ~transactions =
      let open Result_syntax in
      List.to_seq transactions
      |> Seq.map (fun (raw_tx, (obj : Transaction_object.t)) ->
             (Transaction_object.hash obj, raw_tx))
      |> Seq.split
      |> fun (l, r) -> (List.of_seq l, List.of_seq r) |> return
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let check_response =
      let open Rpc_encodings.JSONRPC in
      let open Lwt_result_syntax in
      function
      | {value = Ok _; _} -> return_unit
      | {value = Error {message; _}; _} ->
          failwith "Send_raw_transaction failed with message \"%s\"" message

    let check_batched_response =
      let open Batch in
      function
      | Batch l -> List.iter_es check_response l
      | Singleton r -> check_response r

    let send_raw_transaction_method txn =
      let open Rpc_encodings in
      let message =
        Hex.of_string txn |> Hex.show |> Ethereum_types.hex_of_string
      in
      JSONRPC.
        {
          method_ = Send_raw_transaction.method_;
          parameters =
            Some
              (Data_encoding.Json.construct
                 Send_raw_transaction.input_encoding
                 message);
          id = Some (random_id ());
        }

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages =
      let open Rollup_services in
      let open Lwt_result_syntax in
      match Ctxt.evm_node_endpoint with
      | Some evm_node_endpoint ->
          let methods = List.map send_raw_transaction_method messages in

          let* response =
            call_service
              ~keep_alive:Ctxt.keep_alive
              ~timeout:Ctxt.timeout
              ~base:evm_node_endpoint
              (Batch.dispatch_batch_service ~path:Resto.Path.root)
              ()
              ()
              (Batch methods)
          in

          let* () = check_batched_response response in

          return_unit
      | None -> assert false
  end

  module SimulatorBackend = struct
    include Reader

    let modify ~key ~value state =
      let open Lwt_result_syntax in
      let*! state = Evm_state.modify ~key ~value state in
      return state

    let simulate_and_read ?state_override simulate_state ~input =
      let open Lwt_result_syntax in
      let config =
        Pvm.Kernel.config
          ~preimage_directory:Ctxt.ctxt.preimages
          ?preimage_endpoint:Ctxt.ctxt.preimages_endpoint
          ~kernel_debug:false
          ~destination:Ctxt.ctxt.smart_rollup_address
          ~trace_host_funs:Ctxt.ctxt.trace_host_funs
          ()
      in
      let* simulate_state =
        State_override.update_accounts state_override simulate_state
      in
      let* raw_insights =
        Evm_state.execute_and_inspect
          ~pool:Ctxt.execution_pool
          ~native_execution_policy:Ctxt.ctxt.native_execution_policy
          ~config
          ~data_dir:Ctxt.ctxt.data_dir
          ~input
          simulate_state
      in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"
  end

  module Tracer = Tracer

  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_string
      Ctxt.ctxt.smart_rollup_address

  let list_l1_l2_levels ~from_l1_level =
    let open Lwt_result_syntax in
    Evm_store.use Ctxt.ctxt.store @@ fun conn ->
    let* last = Evm_store.L1_l2_finalized_levels.last conn in
    match last with
    | None -> return_nil
    | Some (end_l1_level, _) ->
        Evm_store.L1_l2_finalized_levels.list_by_l1_levels
          conn
          ~start_l1_level:from_l1_level
          ~end_l1_level

  let l2_levels_of_l1_level l1_level =
    Evm_store.use Ctxt.ctxt.store @@ fun conn ->
    Evm_store.L1_l2_finalized_levels.find conn ~l1_level

  let block_param_to_block_number ~chain_family
      (block_param : Ethereum_types.Block_parameter.extended) =
    let open Lwt_result_syntax in
    let root = Durable_storage_path.root_of_chain_family chain_family in
    match block_param with
    | Block_parameter (Number n) -> return n
    | Block_parameter (Latest | Pending) when Ctxt.ctxt.finalized_view -> (
        let* res =
          Evm_store.(use Ctxt.ctxt.store Context_hashes.find_finalized)
        in
        match res with
        | Some (latest, _) -> return latest
        | None -> failwith "The EVM node does not have any state available")
    | Block_parameter (Latest | Pending) -> (
        let* res = Evm_store.(use Ctxt.ctxt.store Context_hashes.find_latest) in
        match res with
        | Some (latest, _) -> return latest
        | None -> failwith "The EVM node does not have any state available")
    | Block_parameter Earliest -> (
        let* res =
          Evm_store.(use Ctxt.ctxt.store Context_hashes.find_earliest)
        in
        match res with
        | Some (earliest, _) -> return earliest
        | None -> failwith "The EVM node does not have any state available")
    | Block_parameter Finalized -> (
        let* res =
          Evm_store.(use Ctxt.ctxt.store Context_hashes.find_finalized)
        in
        match res with
        | Some (finalized, _) -> return finalized
        | None -> failwith "The EVM node is not aware of any finalized block")
    | Block_hash {hash; _} -> (
        let* irmin_hash = find_irmin_hash Ctxt.ctxt block_param in
        let* evm_state = get_evm_state Ctxt.ctxt irmin_hash in
        let*! bytes =
          Evm_state.inspect
            evm_state
            Durable_storage_path.(Block.by_hash ~root hash)
        in
        match bytes with
        | Some bytes ->
            let block = L2_types.block_from_bytes ~chain_family bytes in
            return (L2_types.block_number block)
        | None -> failwith "Missing block %a" Ethereum_types.pp_block_hash hash)
end

module Make (Base : sig
  module Executor : Evm_execution.S

  val ctxt : t

  val evm_node_endpoint : Uri.t option

  val keep_alive : bool

  val timeout : float

  val execution_pool : Lwt_domain.pool
end) =
  Services_backend_sig.Make (MakeBackend (Base)) (Base.Executor)

let pvm_config ctxt =
  Pvm.Kernel.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ~trace_host_funs:ctxt.trace_host_funs
    ()

let execution_gas ~base_fee_per_gas ~da_fee_per_byte receipt object_ =
  let da_fees =
    Fees.gas_used_for_da_fees
      ~da_fee_per_byte
      ~base_fee_per_gas
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
  let*? txns =
    Blueprint_decoder.transactions blueprint.Blueprint_types.blueprint.payload
  in
  if txns = [] then apply_blueprint ?log_file ?profile ctxt blueprint evm_state
  else
    let* txns =
      List.map_es
        (function
          | hash, Some txn -> return (hash, Broadcast.Common (Evm txn))
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

let replay ctxt ?log_file ?profile ?(alter_evm_state = Lwt_result_syntax.return)
    strategy (Ethereum_types.Qty number) =
  let open Lwt_result_syntax in
  let* hash = get_irmin_hash_from_number ctxt (Qty (Z.pred number)) in
  let* evm_state = get_evm_state ctxt hash in
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
  | Apply_success {block; evm_state} ->
      let* (Qty base_fee_per_gas) =
        Etherlink_durable_storage.base_fee_per_gas (fun path ->
            let*! result = Evm_state.inspect evm_state path in
            return result)
      in
      let* da_fee_per_byte =
        Etherlink_durable_storage.da_fee_per_byte (fun path ->
            let*! result = Evm_state.inspect evm_state path in
            return result)
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
           })
  | Apply_failure -> return Replay_failure

let ro_backend ?evm_node_endpoint ctxt config : (module Services_backend_sig.S)
    =
  let module Executor = struct
    let ctxt =
      match ctxt.native_execution_policy with
      | Rpcs_only ->
          (* The [ro_backend] is only used to serve RPCs, so [replay]
             and [execute] are only used for serving RPCs so it is
             safe to “promote” the native execution policy to [Always].
             Without this change, the node believes it is executing a
             block and default to WASM execution. *)
          {ctxt with native_execution_policy = Configuration.Always}
      | _ -> ctxt

    let pvm_config = pvm_config ctxt

    let replay ?log_file ?profile ?alter_evm_state number =
      let open Lwt_result_syntax in
      let+ result =
        replay ctxt ?log_file ?profile ?alter_evm_state Blueprint number
      in
      match result with
      | Replay_success {block; evm_state; _} ->
          Evm_state.Apply_success {block; evm_state}
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
  end in
  let module Backend = Make (struct
    module Executor = Executor

    let ctxt = ctxt

    let evm_node_endpoint = evm_node_endpoint

    let keep_alive = config.Configuration.keep_alive

    let timeout = config.rpc_timeout

    let execution_pool = ctxt.execution_pool
  end) in
  (module struct
    include Backend

    (* This function is generic that's why we don't define it in Tezlink block storage
         (even if for now this is the only place where it's used) *)
    let nth_block_hash level =
      Evm_store.use ctxt.store @@ fun conn ->
      Evm_store.Blocks.find_hash_of_number conn (Qty level)

    (* Overwrite Etherlink_block_storage module *)
    module Etherlink_block_storage = struct
      (* Current block number is kept in durable storage. *)
      let current_block_number = Etherlink_block_storage.current_block_number

      let nth_block ~full_transaction_object level =
        let open Lwt_result_syntax in
        Evm_store.use ctxt.store @@ fun conn ->
        let* block_opt =
          Evm_store.Blocks.find_with_level
            ~full_transaction_object
            conn
            (Qty level)
        in
        match block_opt with
        | None -> failwith "Block %a not found" Z.pp_print level
        | Some block -> return block

      let block_by_hash ~full_transaction_object hash =
        let open Lwt_result_syntax in
        Evm_store.use ctxt.store @@ fun conn ->
        let* block_opt =
          Evm_store.Blocks.find_with_hash ~full_transaction_object conn hash
        in
        match block_opt with
        | None ->
            failwith "Block %a not found" Ethereum_types.pp_block_hash hash
        | Some block -> return block

      let block_receipts level =
        let open Lwt_result_syntax in
        Evm_store.use ctxt.store @@ fun conn ->
        let* found = Evm_store.Blocks.find_hash_of_number conn (Qty level) in
        match found with
        | None -> failwith "Block %a not found" Z.pp_print level
        | Some _hash ->
            Evm_store.Transactions.receipts_of_block_number conn (Qty level)

      let block_range_receipts ?mask level len =
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
        | _ ->
            Evm_store.Transactions.receipts_of_block_range ?mask conn start len

      let transaction_receipt hash =
        Evm_store.use ctxt.store @@ fun conn ->
        Evm_store.Transactions.find_receipt conn hash

      let transaction_object hash =
        Evm_store.use ctxt.store @@ fun conn ->
        Evm_store.Transactions.find_object conn hash
    end

    (* Overwrite Etherlink Tracer using the new Etherlink_block_storage *)
    module Tracer_etherlink =
      Tracer_sig.Make (Executor) (Etherlink_block_storage) (Tracer)

    let block_param_to_block_number ~chain_family
        (block_param : Ethereum_types.Block_parameter.extended) =
      let open Lwt_result_syntax in
      match block_param with
      | Block_hash {hash; _} -> (
          Evm_store.use ctxt.store @@ fun conn ->
          let* res = Evm_store.Blocks.find_number_of_hash conn hash in
          match res with
          | Some number -> return number
          | None ->
              failwith "Missing block %a" Ethereum_types.pp_block_hash hash)
      | param -> block_param_to_block_number ~chain_family param

    module Tezlink_block_storage : Tezlink_block_storage_sig.S = struct
      let nth_block level =
        let open Lwt_result_syntax in
        Evm_store.use ctxt.store @@ fun conn ->
        let* block_opt =
          Evm_store.Blocks.tez_find_with_level conn (Qty level)
        in
        match block_opt with
        | None -> failwith "Block %a not found" Z.pp_print level
        | Some block -> return block

      let nth_block_hash = nth_block_hash
    end

    (* Overwrites Tezlink using the store instead of the durable_storage *)
    module Tezlink =
      Tezlink_services_impl.Make
        (struct
          include Backend.SimulatorBackend

          let block_param_to_block_number =
            block_param_to_block_number ~chain_family:L2_types.Michelson
        end)
        (Tezlink_block_storage)

    module Tezosx_block_storage : Tezlink_block_storage_sig.S = struct
      let nth_block level =
        let open Lwt_result_syntax in
        Evm_store.use ctxt.store @@ fun conn ->
        let* block_opt =
          Evm_store.Blocks.tezosx_find_tez_block_with_level conn (Qty level)
        in
        match block_opt with
        | None -> failwith "TezosX Tezos block %a not found" Z.pp_print level
        | Some block -> return block

      let nth_block_hash = nth_block_hash
    end

    (* Overwrites Tezos using the store instead of the durable_storage *)
    module Tezos =
      Tezos_backend.Make
        (struct
          include Backend.SimulatorBackend

          let block_param_to_block_number =
            block_param_to_block_number ~chain_family:L2_types.Michelson
        end)
        (Tezosx_block_storage)
  end)

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

let evm_services_methods ctxt time_between_blocks =
  Rpc_server.
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
