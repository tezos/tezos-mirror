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
  smart_rollup_address : Address.t;
  time_between_blocks : Evm_node_config.Configuration.time_between_blocks;
}

type block_production = [`Single_node | `Disabled]

module Resto = struct
  let callback server Evm_directory.{dir; extra} =
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
          Tezos_rpc_http_server.RPC_server.resto_callback
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

  let start_server rpc directory =
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

    let*! () =
      RPC_server.launch
        ~max_active_connections
        ~host
        server
        ~callback:(callback server directory)
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

let start_server rpc = function
  | Evm_directory.Resto dir -> Resto.start_server rpc dir
  | Evm_directory.Dream routes -> Dream.start_server rpc routes

let monitor_performances ~data_dir =
  let (module Performance) = Lazy.force Metrics.performance_metrics in
  let rec aux () =
    let open Lwt_syntax in
    let* () = Performance.set_stats ~data_dir in
    let* () = Lwt_unix.sleep 10.0 in
    aux ()
  in
  Lwt.dont_wait aux (Fun.const ())

let start_public_server ?delegate_health_check_to ?evm_services ?data_dir
    (config : Configuration.t) ctxt =
  let open Lwt_result_syntax in
  let*! can_start_performance_metrics =
    Octez_performance_metrics.supports_performance_metrics ()
  in
  if can_start_performance_metrics && Option.is_some data_dir then
    monitor_performances
      ~data_dir:WithExceptions.Option.(get ~loc:__LOC__ data_dir) ;
  let register_evm_services =
    match evm_services with
    | None -> Fun.id
    | Some impl ->
        Evm_services.register
          impl.next_blueprint_number
          impl.find_blueprint
          impl.smart_rollup_address
          impl.time_between_blocks
  in
  let directory =
    Services.directory ?delegate_health_check_to config.public_rpc config ctxt
    |> register_evm_services
    |> Evm_directory.register_metrics "/metrics"
  in
  let* finalizer = start_server config.public_rpc directory in
  let*! () =
    Events.is_ready
      ~rpc_addr:config.public_rpc.addr
      ~rpc_port:config.public_rpc.port
      ~backend:config.experimental_features.rpc_server
      ~websockets:config.experimental_features.enable_websocket
  in
  return finalizer

let start_private_server ?(block_production = `Disabled) config ctxt =
  let open Lwt_result_syntax in
  match config.Configuration.private_rpc with
  | Some private_rpc ->
      let directory =
        Services.private_directory private_rpc ~block_production config ctxt
        |> Evm_directory.register_metrics "/metrics"
      in
      let* finalizer = start_server private_rpc directory in
      let*! () =
        Events.private_server_is_ready
          ~rpc_addr:private_rpc.addr
          ~rpc_port:private_rpc.port
          ~backend:config.experimental_features.rpc_server
          ~websockets:config.experimental_features.enable_websocket
      in
      return finalizer
  | None -> return (fun () -> Lwt_syntax.return_unit)
