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

let container_forward_tx ~evm_node_endpoint ~keep_alive :
    (module Services_backend_sig.Tx_container) =
  (module struct
    let nonce ~next_nonce _address = Lwt_result.return next_nonce

    let add ~next_nonce:_ _tx_object ~raw_tx =
      match evm_node_endpoint with
      | Some evm_node_endpoint ->
          Injector.send_raw_transaction
            ~keep_alive
            ~base:evm_node_endpoint
            ~raw_tx:(Ethereum_types.hex_to_bytes raw_tx)
      | None ->
          Lwt.return_ok
          @@ Error
               "the node is in read-only mode, it doesn't accept transactions"

    let find _hash = Lwt_result.return None

    let content () =
      Lwt_result.return
        Ethereum_types.{pending = AddressMap.empty; queued = AddressMap.empty}
  end)

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
  let validation_mode =
    match config.proxy.evm_node_endpoint with
    | Some _base -> Validate.Stateless
    | None -> Validate.Full
  in
  let* tx_container =
    match
      ( config.experimental_features.enable_send_raw_transaction,
        config.proxy.evm_node_endpoint )
    with
    | true, None ->
        let* () =
          Tx_pool.start
            {
              backend = (module Rollup_node_rpc);
              smart_rollup_address;
              mode = Proxy;
              tx_timeout_limit = config.tx_pool_timeout_limit;
              tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
              tx_pool_tx_per_addr_limit =
                Int64.to_int config.tx_pool_tx_per_addr_limit;
            }
        in
        return (module Tx_pool.Tx_container : Services_backend_sig.Tx_container)
    | enable_send_raw_transaction, evm_node_endpoint ->
        let evm_node_endpoint =
          if enable_send_raw_transaction then evm_node_endpoint else None
        in
        return @@ container_forward_tx ~evm_node_endpoint ~keep_alive
  in

  let () =
    Rollup_node_follower.start
      ~keep_alive:config.keep_alive
      ~proxy:true
      ~rollup_node_endpoint
      ()
  in
  let* chain_family =
    if finalized_view then
      if
        (* When finalized_view is set, it's too early to request the
           feature flag from the rollup node. *)
        Option.is_some config.experimental_features.l2_chains
      then
        (* The finalized view of the proxy mode and the multichain feature are not compatible. *)
        tzfail (Node_error.Proxy_finalize_with_multichain `Node)
      else return L2_types.EVM
    else
      let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
      match (config.experimental_features.l2_chains, enable_multichain) with
      | None, false -> return L2_types.EVM
      | None, true -> tzfail Node_error.Singlechain_node_multichain_kernel
      | Some [_], false ->
          let*! () = Events.multichain_node_singlechain_kernel () in
          return L2_types.EVM
      | Some [l2_chain], true ->
          let* chain_family = Rollup_node_rpc.chain_family l2_chain.chain_id in
          if l2_chain.chain_family = chain_family then return chain_family
          else
            tzfail
              (Node_error.Mismatched_chain_family
                 {
                   chain_id = l2_chain.chain_id;
                   node_family = l2_chain.chain_family;
                   kernel_family = chain_family;
                 })
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
      tx_container
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
