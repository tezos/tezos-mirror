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

type block_production = [`Single_node | `Threshold_encryption | `Disabled]

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

let start_server ~rpc_port directory config =
  let open Lwt_result_syntax in
  let open Tezos_rpc_http_server in
  let Configuration.
        {rpc_addr; cors_origins; cors_headers; max_active_connections; _} =
    config
  in
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

  let finalizer () =
    let open Lwt_syntax in
    let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
    return_unit
  in

  return finalizer

let start_public_server ?evm_services config ctxt =
  let open Lwt_result_syntax in
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
  let directory = Services.directory config ctxt |> register_evm_services in
  let* finalizer = start_server ~rpc_port:config.rpc_port directory config in
  let*! () =
    Events.is_ready ~rpc_addr:config.rpc_addr ~rpc_port:config.rpc_port
  in
  return finalizer

let start_private_server ?(block_production = `Disabled) config ctxt =
  let open Lwt_result_syntax in
  match config.Configuration.private_rpc_port with
  | Some private_rpc_port ->
      let directory =
        Services.private_directory ~block_production config ctxt
      in
      let* finalizer =
        start_server ~rpc_port:private_rpc_port directory config
      in
      let*! () =
        Events.private_server_is_ready
          ~rpc_addr:config.rpc_addr
          ~rpc_port:private_rpc_port
      in
      return finalizer
  | None -> return (fun () -> Lwt_syntax.return_unit)
