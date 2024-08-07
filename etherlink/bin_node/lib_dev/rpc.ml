(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  preimages : string;
  preimages_endpoint : Uri.t option;
  data_dir : string;
  store : Evm_store.t;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  index : Irmin_context.ro_index;
  mutable current_block_number : Ethereum_types.quantity;
}

let load ~data_dir ~preimages ?preimages_endpoint () =
  let open Lwt_result_syntax in
  let* store = Evm_store.init ~data_dir ~perm:`Read_only () in
  let* index =
    Irmin_context.(
      load ~cache_size:100_000 Read_only (Evm_state.irmin_store_path ~data_dir))
  in
  Evm_store.use store @@ fun conn ->
  let* current_block_number, _ = Evm_store.Context_hashes.get_latest conn in
  let+ smart_rollup_address = Evm_store.Metadata.get conn in
  {
    store;
    index;
    data_dir;
    preimages;
    preimages_endpoint;
    smart_rollup_address;
    current_block_number;
  }

let with_evm_state ctxt hash k =
  let open Lwt_result_syntax in
  Irmin_context.reload ctxt.index ;
  let*! context = Irmin_context.checkout_exn ctxt.index hash in
  let*! res = Irmin_context.PVMState.get context in
  let* res = k res in
  return res

let find_latest_hash ctxt =
  let open Lwt_result_syntax in
  let* res = Evm_store.(use ctxt.store Context_hashes.find_latest) in
  match res with
  | Some (_, hash) -> return hash
  | None -> failwith "No state available"

let find_irmin_hash_from_number ctxt number =
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
  | Block_parameter Latest -> find_latest_hash ctxt
  | Block_parameter Earliest -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_earliest) in
      match res with
      | Some (_, hash) -> return hash
      | None -> failwith "No state available")
  | Block_parameter Finalized -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_finalized) in
      match res with
      | Some (_, hash) -> return hash
      | None -> failwith "No state available")
  | Block_parameter Pending ->
      failwith "Block parameter pending is not supported"
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
      (* we use the latest state to read the contents of the block *)
      let* latest_hash = find_latest_hash ctxt in
      with_evm_state ctxt latest_hash @@ fun evm_tree ->
      let*! res =
        Evm_state.inspect evm_tree Durable_storage_path.Block.(by_hash hash)
      in
      match res with
      | Some block_bytes ->
          let block = Ethereum_types.block_from_rlp block_bytes in
          find_irmin_hash_from_number ctxt block.number
      | None -> failwith "Unknown block %a" Ethereum_types.pp_block_hash hash)

module MakeBackend (Ctxt : sig
  val ctxt : t

  val evm_node_endpoint : Uri.t

  val keep_alive : bool
end) =
struct
  module Reader = struct
    open Ethereum_types.Block_parameter

    let read ?(block = Block_parameter Latest) path =
      let open Lwt_result_syntax in
      let* hash = find_irmin_hash Ctxt.ctxt block in
      with_evm_state Ctxt.ctxt hash @@ fun tree ->
      let*! res = Evm_state.inspect tree path in
      return res

    let subkeys ?(block = Block_parameter Latest) path =
      let open Lwt_result_syntax in
      let* hash = find_irmin_hash Ctxt.ctxt block in
      with_evm_state Ctxt.ctxt hash @@ fun tree ->
      let*! res = Evm_state.subkeys tree path in
      return res
  end

  module TxEncoder = struct
    type transactions = string list

    type messages = string list

    let encode_transactions ~smart_rollup_address:_ ~transactions =
      let open Result_syntax in
      let hashes = List.map Ethereum_types.hash_raw_tx transactions in
      return (hashes, transactions)
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
      let open Services in
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
          id = None;
        }

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages =
      let open Rollup_services in
      let open Lwt_result_syntax in
      let methods = List.map send_raw_transaction_method messages in

      let* response =
        call_service
          ~keep_alive:Ctxt.keep_alive
          ~base:Ctxt.evm_node_endpoint
          (Services.dispatch_service ~path:Resto.Path.root)
          ()
          ()
          (Batch methods)
      in

      let* () = check_batched_response response in

      return_unit
  end

  module SimulatorBackend = struct
    type simulation_state = Evm_state.t

    let simulation_state
        ?(block = Ethereum_types.Block_parameter.(Block_parameter Latest)) () =
      let open Lwt_result_syntax in
      let* hash = find_irmin_hash Ctxt.ctxt block in
      with_evm_state Ctxt.ctxt hash return

    let simulate_and_read simulate_state ~input =
      let open Lwt_result_syntax in
      let config =
        Config.config
          ~preimage_directory:Ctxt.ctxt.preimages
          ?preimage_endpoint:Ctxt.ctxt.preimages_endpoint
          ~kernel_debug:false
          ~destination:Ctxt.ctxt.smart_rollup_address
          ()
      in
      let* raw_insights =
        Evm_state.execute_and_inspect
          ~config
          ~data_dir:Ctxt.ctxt.data_dir
          ~input
          simulate_state
      in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"

    let read simulation_state ~path =
      let open Lwt_result_syntax in
      let*! res = Evm_state.inspect simulation_state path in
      return res
  end

  module Tracer = Tracer

  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_string
      Ctxt.ctxt.smart_rollup_address

  let block_param_to_block_number
      (block_param : Ethereum_types.Block_parameter.extended) =
    let open Lwt_result_syntax in
    match block_param with
    | Block_parameter (Number n) -> return n
    | Block_parameter Latest -> (
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
    | Block_parameter Finalized ->
        failwith "Block parameter finalized is not supported in rpc mode"
    | Block_parameter Pending ->
        failwith "Pending block parameter is not supported"
    | Block_hash {hash; _} -> (
        let* irmin_hash = find_irmin_hash Ctxt.ctxt block_param in
        with_evm_state Ctxt.ctxt irmin_hash @@ fun evm_state ->
        let*! bytes =
          Evm_state.inspect evm_state Durable_storage_path.(Block.by_hash hash)
        in
        match bytes with
        | Some bytes ->
            let block = Ethereum_types.block_from_rlp bytes in
            return block.number
        | None -> failwith "Missing block %a" Ethereum_types.pp_block_hash hash)
end

module Make (Base : sig
  module Executor : Evm_execution.S

  val ctxt : t

  val evm_node_endpoint : Uri.t

  val keep_alive : bool
end) =
struct
  include Services_backend_sig.Make (MakeBackend (Base)) (Base.Executor)

  let current_block_number () = Lwt_result.return Base.ctxt.current_block_number
end

let callback server dir =
  let open Cohttp in
  let open Lwt_syntax in
  let callback_log conn req body =
    let path = Request.uri req |> Uri.path in
    if path = "/metrics" then
      let* response = Metrics.Metrics_server.callback conn req body in
      Lwt.return (`Response response)
    else
      let uri = req |> Request.uri |> Uri.to_string in
      let meth = req |> Request.meth |> Code.string_of_method in
      let* body_str = body |> Cohttp_lwt.Body.to_string in
      let* () = Events.callback_log ~uri ~meth ~body:body_str in
      Tezos_rpc_http_server.RPC_server.resto_callback
        server
        conn
        req
        (Cohttp_lwt.Body.of_string body_str)
  in
  let update_metrics uri meth =
    Prometheus.Summary.(time (labels Metrics.Rpc.metrics [uri; meth]) Sys.time)
  in
  Tezos_rpc_http_server.RPC_middleware.rpc_metrics_transform_callback
    ~update_metrics
    dir
    callback_log

let rpc_start
    ({rpc_addr; rpc_port; cors_origins; cors_headers; max_active_connections; _} :
      Configuration.t) ~directory =
  let open Lwt_result_syntax in
  let open Tezos_rpc_http_server in
  Metrics.Info.init ~mode:"rpc" ;
  let p2p_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string p2p_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let cors =
    Resto_cohttp.Cors.
      {allowed_headers = cors_headers; allowed_origins = cors_origins}
  in
  let server =
    RPC_server.init_server
      ~acl
      ~cors
      ~media_types:Media_type.all_media_types
      directory
  in
  let*! () =
    RPC_server.launch
      ~max_active_connections
      ~host
      ~callback:(callback server directory)
      server
      node
  in
  let*! () = Events.is_ready ~rpc_addr ~rpc_port in
  return server

let install_finalizer_rpc server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = Events.shutdown_rpc_server ~private_:false in
  Misc.unwrap_error_monad @@ fun () -> Tx_pool.shutdown ()

let next_blueprint_number ctxt () =
  let open Lwt_syntax in
  let (Qty current) = ctxt.current_block_number in
  return (Ethereum_types.Qty Z.(succ current))

let preload_kernel_from_level ctxt level =
  let open Lwt_result_syntax in
  let* hash = find_irmin_hash_from_number ctxt level in
  with_evm_state ctxt hash @@ fun evm_state ->
  let*! () = Evm_state.preload_kernel evm_state in
  return_unit

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

let main ~data_dir ~evm_node_endpoint ~(config : Configuration.t) =
  let open Lwt_result_syntax in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let* ctxt =
    load
      ~data_dir
      ~preimages:config.kernel_execution.preimages
      ?preimages_endpoint:config.kernel_execution.preimages_endpoint
      ()
  in

  let* () = preload_known_kernels ctxt in

  let rpc_backend =
    (module Make (struct
      module Executor = struct
        let pvm_config =
          Config.config
            ~preimage_directory:ctxt.preimages
            ?preimage_endpoint:ctxt.preimages_endpoint
            ~kernel_debug:true
            ~destination:ctxt.smart_rollup_address
            ()

        let replay ?(log_file = "replay") ?profile
            ?(alter_evm_state = Lwt_result_syntax.return)
            (Ethereum_types.Qty number) =
          let open Lwt_result_syntax in
          let* hash = find_irmin_hash_from_number ctxt (Qty Z.(pred number)) in
          with_evm_state ctxt hash @@ fun evm_state ->
          let* evm_state = alter_evm_state evm_state in
          let* blueprint =
            Evm_store.use ctxt.store @@ fun conn ->
            Evm_store.Blueprints.get_with_events conn (Qty number)
          in
          Evm_state.apply_blueprint
            ~log_file
            ?profile
            ~data_dir
            ~config:pvm_config
            evm_state
            blueprint.blueprint.payload

        let execute ?(alter_evm_state = Lwt_result_syntax.return) input block =
          let open Lwt_result_syntax in
          let message =
            List.map (fun s -> `Input s) Simulation.Encodings.(input.messages)
          in
          let* hash = find_irmin_hash ctxt block in
          with_evm_state ctxt hash @@ fun evm_state ->
          let* evm_state = alter_evm_state evm_state in
          Evm_state.execute
            ?log_file:input.log_kernel_debug_file
            ~data_dir
            ~config:pvm_config
            evm_state
            message
      end

      let ctxt = ctxt

      let evm_node_endpoint = evm_node_endpoint

      let keep_alive = config.keep_alive
    end) : Services_backend_sig.S)
  in

  let* () =
    Tx_pool.start
      {
        rollup_node = rpc_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            ctxt.smart_rollup_address;
        mode = Relay;
        tx_timeout_limit = config.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int config.tx_pool_tx_per_addr_limit;
        max_number_of_chunks = None;
      }
  in

  let directory =
    Services.directory config (rpc_backend, ctxt.smart_rollup_address)
  in
  let directory =
    directory
    |> Evm_services.register
         (next_blueprint_number ctxt)
         (fun level ->
           Evm_store.use ctxt.store (fun conn ->
               Evm_store.Blueprints.find_with_events conn level))
         ctxt.smart_rollup_address
         time_between_blocks
  in
  let* server = rpc_start config ~directory in

  let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer_rpc server in

  Blueprints_follower.start
    ~time_between_blocks
    ~evm_node_endpoint
    ~get_next_blueprint_number:(next_blueprint_number ctxt)
  @@ fun number blueprint ->
  let* () =
    when_ (Option.is_some blueprint.kernel_upgrade) @@ fun () ->
    preload_kernel_from_level ctxt number
  in
  Blueprints_watcher.notify blueprint ;
  ctxt.current_block_number <- number ;
  return_unit
