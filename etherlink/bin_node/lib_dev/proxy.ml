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
  let* () =
    when_ Option.(is_some config.proxy.finalized_view) @@ fun () ->
    let*! () =
      Events.deprecation_note
        "proxy.finalized_view has been deprecated and will be removed in a \
         future version"
    in
    return_unit
  in
  let* finalized_view =
    match (config.proxy.finalized_view, config.finalized_view) with
    | Some fv, false -> return fv
    | _, fv -> return fv
  in
  let module Rollup_node_rpc = Rollup_node.Make (struct
    let base = rollup_node_endpoint

    let keep_alive = keep_alive

    let drop_duplicate_on_injection = drop_duplicate_on_injection

    let smart_rollup_address = smart_rollup_address

    let finalized = finalized_view

    let ignore_block_param = config.proxy.ignore_block_param
  end) in
  let pool_mode, validation_mode =
    match config.proxy.evm_node_endpoint with
    | None -> (Tx_pool.Proxy, Validate.Full)
    | Some evm_node_endpoint ->
        ( Tx_pool.Forward
            {
              injector =
                (fun _ raw_tx ->
                  Injector.send_raw_transaction
                    ~keep_alive:config.keep_alive
                    ~base:evm_node_endpoint
                    ~raw_tx);
            },
          Validate.Stateless )
  in
  let* () =
    if not config.experimental_features.enable_send_raw_transaction then
      return_unit
    else
      Tx_pool.start
        {
          backend = (module Rollup_node_rpc);
          smart_rollup_address;
          mode = pool_mode;
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
  let* chain_family =
    let open Ethereum_types in
    if finalized_view then
      if
        (* When finalized_view is set, it's too early to request the
           feature flag from the rollup node. *)
        Option.is_some config.experimental_features.l2_chains
      then
        (* The finalized view of the proxy mode and the multichain feature are not compatible. *)
        tzfail (Node_error.Proxy_finalize_with_multichain `Node)
      else return EVM
    else
      let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
      match (config.experimental_features.l2_chains, enable_multichain) with
      | None, false -> return EVM
      | None, true -> tzfail Node_error.Singlechain_node_multichain_kernel
      | Some [_], false ->
          let*! () = Events.multichain_node_singlechain_kernel () in
          return EVM
      | Some [l2_chain], true -> Rollup_node_rpc.chain_family l2_chain.chain_id
      | _ -> tzfail Node_error.Unexpected_multichain
  in
  let* server_finalizer =
    Rpc_server.start_public_server
      ~rpc_server_family:(Rpc_types.Single_chain_node_rpc_server chain_family)
      ?tezlink_services:
        (if chain_family = Michelson then
           Some
             Tezlink_services_impl.(
               michelson_services_methods
                 (module Rollup_node_rpc)
                 Tezlink_constants.mainnet)
         else None)
      validation_mode
      config
      ((module Rollup_node_rpc), smart_rollup_address)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer server_finalizer
  in
  let wait, _resolve = Lwt.wait () in
  let* () = wait in
  if finalized_view then
    let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
    if enable_multichain then
      tzfail (Node_error.Proxy_finalize_with_multichain `Kernel)
    else return_unit
  else return_unit
