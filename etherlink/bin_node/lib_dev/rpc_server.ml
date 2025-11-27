(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type finalizer = unit -> unit Lwt.t

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

type block_production = [`Single_node | `Disabled]

module Resto = struct
  let callback ~port server Evm_directory.{dir; extra} =
    let open Cohttp in
    let open Lwt_syntax in
    let callback_log conn req body =
      let path = Request.uri req |> Uri.path in
      let meth = Request.meth req in
      match Evm_directory.EndpointMap.find (meth, path) extra with
      | Some callback -> callback conn req body
      | None ->
          let uri = req |> Request.uri |> Uri.to_string in
          let meth = req |> Request.meth |> Code.string_of_method in
          let* body_str = body |> Cohttp_lwt.Body.to_string in
          let* () = Events.callback_log ~uri ~meth ~body:body_str in
          Octez_telemetry.HTTP_server.resto_callback
            ~port
            server
            conn
            req
            (Cohttp_lwt.Body.of_string body_str)
    in
    Tezos_rpc_http_server.RPC_middleware.rpc_metrics_transform_callback
      ~update_metrics:Metrics.Rpc.update_metrics
      dir
      callback_log

  let conn_closed conn = Evm_websocket.on_conn_closed conn

  let start_server config rpc directory =
    let open Lwt_result_syntax in
    let open Tezos_rpc_http_server in
    let Configuration.
          {port; addr; cors_origins; cors_headers; max_active_connections; _} =
      rpc
    in

    let p2p_addr = P2p_addr.of_string_exn addr in
    let host = Ipaddr.V6.to_string p2p_addr in
    let node = `TCP (`Port port) in
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
        directory.Evm_directory.dir
    in
    let*? () =
      match config.Configuration.websockets with
      | Some
          {rate_limit = Some {max_frames; max_messages; interval; strategy}; _}
        ->
          let interval = Ptime.Span.of_int_s interval in
          let messages_limit =
            Option.map
              (fun max -> {Evm_websocket.max; interval; strategy})
              max_messages
          in
          let frames_limit =
            {Evm_websocket.max = max_frames; interval; strategy = `Close}
          in
          Evm_websocket.setup_rate_limiters ?messages_limit ~frames_limit ()
      | _ -> Ok ()
    in
    let*! () =
      RPC_server.launch
        ~max_active_connections
        ~host
        server
        ~callback:(callback ~port server directory)
        ~conn_closed
        node
    in

    let finalizer () =
      let open Lwt_syntax in
      let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
      return_unit
    in

    return finalizer
end

module Dream = struct
  let start_server rpc routes =
    let open Lwt_result_syntax in
    let Configuration.{port; addr; cors_origins = _; cors_headers = _; _} =
      rpc
    in
    let stop, resolve_stop = Lwt.wait () in
    let shutdown () =
      Lwt.wakeup_later resolve_stop () ;
      Lwt.return_unit
    in
    Lwt.dont_wait
      (fun () ->
        routes |> Dream.router |> Dream.serve ~interface:addr ~port ~stop)
      (function
        | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
            Logs.err (fun m ->
                m "Cannot start RPC server on port %d, already in use." port) ;
            exit 1
        | exn -> Events.rpc_server_error exn) ;
    return shutdown
end

let start_server config rpc = function
  | Evm_directory.Resto dir -> Resto.start_server config rpc dir
  | Evm_directory.Dream routes -> Dream.start_server rpc routes

let monitor_performances ~data_dir =
  let (module Performance) = Lazy.force Metrics.performance_metrics in
  let rec aux () =
    Performance.set_stats ~data_dir ;
    Unix.sleep 10 ;
    aux ()
  in
  let domain = Domain.spawn aux in
  (* Run in background *)
  ignore domain

let start_public_server (type f) ~(mode : f Mode.t)
    ~(rpc_server_family : f Rpc_types.rpc_server_family) ~l2_chain_id ~tick
    ?evm_services (config : Configuration.t) ctxt =
  let open Lwt_result_syntax in
  let can_start_performance_metrics =
    Octez_performance_metrics.Unix.supports_performance_metrics ()
  in
  if can_start_performance_metrics then
    monitor_performances ~data_dir:config.data_dir ;
  let register_evm_services =
    match evm_services with
    | None -> Fun.id
    | Some impl ->
        Evm_services.register
          impl.next_blueprint_number
          impl.find_blueprint_legacy
          impl.find_blueprint
          impl.smart_rollup_address
          impl.time_between_blocks
  in
  let*? () = Rpc_types.check_rpc_server_config rpc_server_family config in
  let* register_tezos_services =
    let (module Backend : Services_backend_sig.S), _ = ctxt in
    match rpc_server_family with
    | Rpc_types.Single_chain_node_rpc_server Michelson ->
        let add_transaction ~next_nonce transaction_object ~raw_op =
          match mode with
          | Observer (Michelson_tx_container (module Tx_container))
          | Proxy (Michelson_tx_container (module Tx_container))
          | Sequencer (Michelson_tx_container (module Tx_container)) ->
              Tx_container.add
                ~next_nonce
                transaction_object
                ~raw_tx:(Ethereum_types.hex_of_bytes raw_op)
          | Rpc {evm_node_private_endpoint; _} ->
              Injector.inject_tezlink_operation
                ~keep_alive:config.keep_alive
                ~timeout:config.rpc_timeout
                ~base:evm_node_private_endpoint
                ~op:transaction_object
                ~raw_op
        in
        let* l2_chain_id =
          match l2_chain_id with
          | Some l2_chain_id -> return l2_chain_id
          | None -> Backend.chain_id ()
        in
        return @@ Evm_directory.init_from_resto_directory
        @@ Tezlink_directory.register_tezlink_services
             ~l2_chain_id
             (module Backend.Tezlink)
             ~add_operation:(fun raw ->
               (* TODO: https://gitlab.com/tezos/tezos/-/issues/8007
                  Validate the operation and use the resulting "next_nonce" *)
               let raw_str = Bytes.to_string raw in
               let raw_hex = Ethereum_types.hex_of_bytes raw in
               let* res =
                 Prevalidator.prevalidate_raw_transaction_tezlink raw_str
               in
               match res with
               | Error err ->
                   let*! () =
                     Tx_pool_events.invalid_transaction ~transaction:raw_hex
                   in
                   failwith
                     "Prevalidation of operation %s failed with error: %s"
                     raw_str
                     err
               | Ok {next_nonce; transaction_object} ->
                   let* hash_res =
                     add_transaction
                       ~next_nonce
                       transaction_object
                       ~raw_op:(Bytes.of_string raw_str)
                   in
                   let* hash =
                     match hash_res with
                     | Ok hash -> return hash
                     | Error s -> failwith "%s" s
                   in
                   return hash)
    | Single_chain_node_rpc_server EVM | Multichain_sequencer_rpc_server -> (
        let*! runtimes = Backend.list_runtimes () in
        match runtimes with
        | Ok [] ->
            return
            @@ Evm_directory.empty config.experimental_features.rpc_server
        | Ok runtimes ->
            let*! _ = List.map_p Tezosx_events.runtime_activated runtimes in
            let* l2_chain_id =
              match l2_chain_id with
              | Some l2_chain_id -> return l2_chain_id
              | None -> Backend.chain_id ()
            in
            let* () =
              (* we use Resto for some runtimes, so we can _only_ use resto. *)
              if config.experimental_features.rpc_server = Dream then
                failwith "Dream RPC server is not supported for TezosX"
              else return_unit
            in
            return @@ Evm_directory.init_from_resto_directory
            @@ List.fold_left
                 (Tezosx_rpc.add_rpc_directory (module Backend) ~l2_chain_id)
                 Tezos_rpc.Directory.empty
                 runtimes
        | Error e ->
            (* FIXME: added during TezosX POC

               This branch is taken by the observer on some tests. It's not
               clear if it's a pb in the test setup or a pb with the observer.
               To be explored when the POC is a bit more advanced. *)
            let*! () = Tezosx_events.list_runtime_failed e in
            return
            @@ Evm_directory.empty config.experimental_features.rpc_server)
  in
  (* If spawn_rpc is defined, use it as intermediate *)
  let rpc =
    match config.experimental_features.spawn_rpc with
    | Some port -> {config.public_rpc with port}
    | _ -> config.public_rpc
  in

  let directory =
    register_tezos_services
    |> Services.directory ~rpc_server_family mode rpc config ctxt ~tick
    |> register_evm_services
    |> Evm_directory.register_metrics "/metrics"
    |> Evm_directory.register_describe
  in
  let* finalizer = start_server config rpc directory in
  let*! () =
    Events.is_ready
      ~rpc_addr:rpc.addr
      ~rpc_port:rpc.port
      ~backend:config.experimental_features.rpc_server
      ~websockets:(Option.is_some config.websockets)
  in
  return finalizer

let start_private_server ~mode
    ~(rpc_server_family : _ Rpc_types.rpc_server_family) ~tick
    ?(block_production = `Disabled) config ctxt =
  let open Lwt_result_syntax in
  match config.Configuration.private_rpc with
  | Some private_rpc ->
      let directory =
        Services.private_directory
          mode
          ~rpc_server_family
          private_rpc
          ~block_production
          config
          ctxt
          ~tick
        |> Evm_directory.register_metrics "/metrics"
        |> Evm_directory.register_describe
      in
      let* finalizer = start_server config private_rpc directory in
      let*! () =
        Events.private_server_is_ready
          ~rpc_addr:private_rpc.addr
          ~rpc_port:private_rpc.port
          ~backend:config.experimental_features.rpc_server
          ~websockets:(Option.is_some config.websockets)
      in
      return finalizer
  | None -> return (fun () -> Lwt_syntax.return_unit)
