(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module MakeBackend (Ctxt : sig
  val evm_node_endpoint : Uri.t

  val keep_alive : bool

  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.Backend = struct
  module Reader = struct
    let read ?block path = Evm_context.inspect ?block path

    let subkeys ?block path = Evm_context.inspect_subkeys ?block path
  end

  module TxEncoder = struct
    type transactions = string list

    type messages = string list

    let encode_transactions ~smart_rollup_address:_ ~transactions =
      let open Result_syntax in
      let hashes = List.map hash_raw_tx transactions in
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
      Evm_context.get_evm_state block

    let simulate_and_read simulation_state ~input =
      let open Lwt_result_syntax in
      let* raw_insights =
        Evm_context.execute_and_inspect simulation_state input
      in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"

    let read simulation_state ~path =
      let open Lwt_result_syntax in
      let*! res = Evm_state.inspect simulation_state path in
      return res
  end

  let block_param_to_block_number = Evm_context.block_param_to_block_number

  module Tracer = Tracer

  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_string Ctxt.smart_rollup_address
end

let on_new_blueprint next_blueprint_number
    ({delayed_transactions; kernel_upgrade; blueprint} :
      Blueprint_types.with_events) =
  let open Lwt_result_syntax in
  let (Qty level) = blueprint.number in
  let (Qty number) = next_blueprint_number in
  if Z.(equal level number) then
    let events =
      List.map
        (fun delayed_transaction ->
          Ethereum_types.Evm_events.New_delayed_transaction delayed_transaction)
        delayed_transactions
      @
      match kernel_upgrade with
      | Some kernel_upgrade ->
          [Ethereum_types.Evm_events.Upgrade_event kernel_upgrade]
      | None -> []
    in
    let* () = Evm_context.apply_evm_events events in
    let delayed_transactions =
      List.map
        (fun Ethereum_types.Delayed_transaction.{hash; _} -> hash)
        delayed_transactions
    in
    Evm_context.apply_blueprint
      blueprint.timestamp
      blueprint.payload
      delayed_transactions
  else failwith "Received a blueprint with an unexpected number."

module Make (Ctxt : sig
  val evm_node_endpoint : Uri.t

  val keep_alive : bool

  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.S =
  Services_backend_sig.Make (MakeBackend (Ctxt))

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

let observer_start
    ({rpc_addr; rpc_port; cors_origins; cors_headers; max_active_connections; _} :
      Configuration.t) ~directory =
  let open Lwt_result_syntax in
  let open Tezos_rpc_http_server in
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
      ~media_types:Supported_media_types.all
      directory
  in
  let*! () =
    RPC_server.launch
      ~max_active_connections
      ~host
      server
      ~callback:(callback server directory)
      node
  in
  let*! () = Events.is_ready ~rpc_addr ~rpc_port in
  return server

let install_finalizer_observer server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = Events.shutdown_rpc_server ~private_:false in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  let* () = Evm_events_follower.shutdown () in
  Evm_context.shutdown ()

type error += Timeout

let timeout_from_tbb = function
  | Some Configuration.Nothing | None ->
      let p, _ = Lwt.task () in
      p
  | Some (Time_between_blocks tbb) ->
      let open Lwt_result_syntax in
      let*! _ = Lwt_unix.sleep (tbb +. 1.) in
      tzfail Timeout

let local_head_too_old ?remote_head ~evm_node_endpoint
    (Qty next_blueprint_number) =
  let open Lwt_result_syntax in
  let open Rpc_encodings in
  let* (Qty remote_head_number) =
    match remote_head with
    | None ->
        (* The observer is designed to be resilent to downtime from its EVM
           node endpoint. It would not make sense to break this logic here, so
           we force [keep_alive] to true. *)
        Services.call
          (module Block_number)
          ~keep_alive:true
          ~evm_node_endpoint
          ()
    | Some remote_head -> return remote_head
  in
  return
    ( Z.Compare.(next_blueprint_number <= remote_head_number),
      Qty remote_head_number )

let[@tailrec] rec main_loop ?remote_head ?time_between_blocks ~first_connection
    ~evm_node_endpoint () =
  let open Lwt_result_syntax in
  let*! head = Evm_context.head_info () in

  let* local_head_too_old, remote_head =
    local_head_too_old
      ?remote_head
      ~evm_node_endpoint
      head.next_blueprint_number
  in

  if local_head_too_old then
    let* blueprint =
      Evm_services.get_blueprint ~evm_node_endpoint head.next_blueprint_number
    in
    let* () = on_new_blueprint head.next_blueprint_number blueprint in
    (main_loop [@tailcall])
      ~remote_head
      ?time_between_blocks
      ~first_connection
      ~evm_node_endpoint
      ()
  else
    let* () =
      when_ (not first_connection) @@ fun () ->
      let delay = Random.float 2. in
      let*! () = Events.retrying_connect ~endpoint:evm_node_endpoint ~delay in
      let*! () = Lwt_unix.sleep delay in
      return_unit
    in

    let*! call_result =
      Evm_services.monitor_blueprints
        ~evm_node_endpoint
        head.next_blueprint_number
    in

    match call_result with
    | Ok blueprints_stream ->
        (stream_loop [@tailcall])
          ~evm_node_endpoint
          head.next_blueprint_number
          blueprints_stream
          ~time_between_blocks
    | Error _ ->
        (main_loop [@tailcall])
          ?time_between_blocks
          ~first_connection:false
          ~evm_node_endpoint
          ()

and[@tailrec] stream_loop ~time_between_blocks ~evm_node_endpoint
    (Qty next_blueprint_number) stream =
  let open Lwt_result_syntax in
  let*! candidate =
    Lwt.pick
      [
        (let*! res = Lwt_stream.get stream in
         return res);
        timeout_from_tbb time_between_blocks;
      ]
  in
  match candidate with
  | Ok (Some blueprint) ->
      let* () = on_new_blueprint (Qty next_blueprint_number) blueprint in
      let* () = Tx_pool.pop_and_inject_transactions () in
      (stream_loop [@tailcall])
        ~evm_node_endpoint
        (Qty (Z.succ next_blueprint_number))
        stream
        ~time_between_blocks
  | Ok None | Error [Timeout] ->
      (main_loop [@tailcall])
        ~first_connection:false
        ~evm_node_endpoint
        ?time_between_blocks
        ()
  | Error err -> fail err

let main ?kernel_path ~data_dir ~(config : Configuration.t) () =
  let open Lwt_result_syntax in
  Metrics.Info.init ~mode:"observer" ;
  let rollup_node_endpoint = config.rollup_node_endpoint in
  let*? {
          evm_node_endpoint;
          threshold_encryption_bundler_endpoint;
          preimages;
          preimages_endpoint;
          time_between_blocks;
        } =
    Configuration.observer_config_exn config
  in
  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address ~evm_node_endpoint
  in
  let* _loaded =
    Evm_context.start
      ~data_dir
      ?kernel_path
      ~preimages
      ~preimages_endpoint
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ~fail_on_missing_blueprint:false
      ~store_perm:`Read_write
      ()
  in

  let observer_backend =
    (module Make (struct
      let smart_rollup_address = smart_rollup_address

      let keep_alive = config.keep_alive

      let evm_node_endpoint =
        match threshold_encryption_bundler_endpoint with
        | Some endpoint -> endpoint
        | None -> evm_node_endpoint
    end) : Services_backend_sig.S)
  in

  let* () =
    Tx_pool.start
      {
        rollup_node = observer_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            smart_rollup_address;
        mode = Observer;
        tx_timeout_limit = config.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int config.tx_pool_tx_per_addr_limit;
        max_number_of_chunks = None;
      }
  in

  let directory =
    Services.directory config (observer_backend, smart_rollup_address)
  in
  let directory = directory |> Evm_services.register smart_rollup_address in

  let* server = observer_start config ~directory in

  let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer_observer server in
  let* () =
    Evm_events_follower.start
      {
        rollup_node_endpoint;
        keep_alive = config.keep_alive;
        filter_event =
          (function
          | New_delayed_transaction _ | Upgrade_event _ -> false | _ -> true);
      }
  in
  let () =
    Rollup_node_follower.start
      ~keep_alive:config.keep_alive
      ~proxy:false
      ~rollup_node_endpoint
      ()
  in

  main_loop ~first_connection:true ~evm_node_endpoint ?time_between_blocks ()
