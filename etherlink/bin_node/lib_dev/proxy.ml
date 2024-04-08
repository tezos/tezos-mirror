(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

let callback_log server conn req body =
  let open Cohttp in
  let open Lwt_syntax in
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let* body_str = body |> Cohttp_lwt.Body.to_string in
  let* () = Events.callback_log ~uri ~meth ~body:body_str in
  Tezos_rpc_http_server.RPC_server.resto_callback
    server
    conn
    req
    (Cohttp_lwt.Body.of_string body_str)

let start
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
      ~media_types:Media_type.all_media_types
      directory
  in
  Lwt.catch
    (fun () ->
      let*! () =
        RPC_server.launch
          ~max_active_connections
          ~host
          server
          ~callback:(callback_log server)
          node
      in
      let*! () = Events.is_ready ~rpc_addr ~rpc_port in
      return server)
    (fun _ -> return server)

let install_finalizer server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = Events.shutdown_rpc_server ~private_:false in
  Helpers.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  Evm_context.shutdown ()

(** [retry_connection f] retries the connection using [f]. If an error
    happens in [f] and it is a lost connection, the connection is retried  *)
let retry_connection (f : Uri.t -> string tzresult Lwt.t) endpoint :
    string tzresult Lwt.t =
  let open Lwt_result_syntax in
  let rec retry ~delay =
    let*! result = f endpoint in
    match result with
    | Ok smart_rollup_address -> return smart_rollup_address
    | Error err when Rollup_services.is_connection_error err ->
        let*! () = Events.retrying_connect ~endpoint ~delay in
        let*! () = Lwt_unix.sleep delay in
        let next_delay = delay *. 2. in
        let delay = Float.min next_delay 30. in
        retry ~delay
    | res -> Lwt.return res
  in
  retry ~delay:1.

(** [fetch_smart_rollup_address ~keep_alive f] tries to fetch the
    smart rollup address using [f]. If [keep_alive] is true, tries to
    fetch until it works. *)
let fetch_smart_rollup_address ~keep_alive f (endpoint : Uri.t) =
  if keep_alive then retry_connection f endpoint else f endpoint

let main (config : Configuration.t) ~keep_alive ~rollup_node_endpoint =
  let open Lwt_result_syntax in
  let* smart_rollup_address =
    fetch_smart_rollup_address
      ~keep_alive
      Rollup_services.smart_rollup_address
      rollup_node_endpoint
  in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint

    let smart_rollup_address = smart_rollup_address
  end) in
  let* () =
    Tx_pool.start
      {
        rollup_node = (module Rollup_node_rpc);
        smart_rollup_address;
        mode = Proxy {rollup_node_endpoint};
        tx_timeout_limit = config.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int config.tx_pool_tx_per_addr_limit;
      }
  in
  let () = Rollup_node_follower.start ~proxy:true ~rollup_node_endpoint in
  let directory =
    Services.directory config ((module Rollup_node_rpc), smart_rollup_address)
  in
  let* server = start config ~directory in
  let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer server in
  let wait, _resolve = Lwt.wait () in
  let* () = wait in
  return_unit
