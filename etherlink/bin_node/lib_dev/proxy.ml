(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

let install_finalizer server_finalizer =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_finalizer () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  Evm_context.shutdown ()

let send_raw_transaction_method txn =
  let open Rpc_encodings in
  let message = Hex.of_string txn |> Hex.show |> Ethereum_types.hex_of_string in
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

let main
    ({
       keep_alive;
       rollup_node_endpoint;
       experimental_features = {drop_duplicate_on_injection; _};
       _;
     } as config :
      Configuration.t) =
  let open Lwt_result_syntax in
  let* smart_rollup_address =
    Rollup_services.smart_rollup_address
      ~keep_alive:config.keep_alive
      rollup_node_endpoint
  in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint

    let keep_alive = keep_alive

    let drop_duplicate_on_injection = drop_duplicate_on_injection

    let smart_rollup_address = smart_rollup_address

    let finalized = config.proxy.finalized_view

    let ignore_block_param = config.proxy.ignore_block_param
  end) in
  let mode =
    match config.proxy.evm_node_endpoint with
    | None -> Tx_pool.Proxy
    | Some evm_node_endpoint ->
        let injector raw_txn =
          let* response =
            let open Rollup_services in
            call_service
              ~keep_alive
              ~base:evm_node_endpoint
              (Services.dispatch_service ~path:Resto.Path.root)
              ()
              ()
              (Singleton (send_raw_transaction_method raw_txn))
          in

          match response with
          | Singleton {value = Ok json; id = _} ->
              let open Rpc_encodings in
              let hash =
                Data_encoding.Json.destruct
                  Send_raw_transaction.output_encoding
                  json
              in
              return (Ok hash)
          | Singleton {value = Error err; id = _}
          | Batch [{value = Error err; id = _}] ->
              return (Error err.message)
          | Batch _ ->
              (* should not be possible, we consider it failed *)
              return
                (Error
                   "Upstream EVM endpoint returned an inconsistent response \
                    (more than one result)")
        in

        Tx_pool.Forward {injector}
  in
  let* () =
    if not config.experimental_features.enable_send_raw_transaction then
      return_unit
    else
      Tx_pool.start
        {
          rollup_node = (module Rollup_node_rpc);
          smart_rollup_address;
          mode;
          tx_timeout_limit = config.tx_pool_timeout_limit;
          tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit =
            Int64.to_int config.tx_pool_tx_per_addr_limit;
          max_number_of_chunks = None;
        }
  in
  let () =
    Rollup_node_follower.start
      ~keep_alive:config.keep_alive
      ~proxy:true
      ~rollup_node_endpoint
      ()
  in
  let* server_finalizer =
    Rpc_server.start_public_server
      config
      ((module Rollup_node_rpc), smart_rollup_address)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer server_finalizer
  in
  let wait, _resolve = Lwt.wait () in
  let* () = wait in
  return_unit
