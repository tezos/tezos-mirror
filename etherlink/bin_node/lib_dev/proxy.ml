(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

let install_finalizer server_finalizer
    (module Tx_container : Services_backend_sig.Tx_container) =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_finalizer () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_container.shutdown () in
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

    let shutdown () = Lwt_result_syntax.return_unit

    let clear () = Lwt_result_syntax.return_unit

    let tx_queue_tick ~evm_node_endpoint:_ = Lwt_result_syntax.return_unit

    let tx_queue_beacon ~evm_node_endpoint:_ ~tick_interval:_ =
      Lwt_result_syntax.return_unit

    let lock_transactions () = Lwt_result_syntax.return_unit

    let unlock_transactions () = Lwt_result_syntax.return_unit

    let is_locked () = Lwt_result_syntax.return_false

    let pop_transactions ~maximum_cumulative_size:_ ~validate_tx:_
        ~initial_validation_state:_ =
      Lwt_result_syntax.return_nil

    let confirm_transactions ~clear_pending_queue_after:_ ~confirmed_txs:_ =
      Lwt_result_syntax.return_unit
  end)

let tx_queue_pop_and_inject (module Rollup_node_rpc : Services_backend_sig.S)
    (module Tx_container : Services_backend_sig.Tx_container)
    ~smart_rollup_address =
  let open Lwt_result_syntax in
  let maximum_cumulative_size =
    Sequencer_blueprint.maximum_usable_space_in_blueprint
      Sequencer_blueprint.maximum_chunks_per_l1_level
  in
  let initial_validation_state = 0 in
  let validate_tx current_size raw_tx _tx_object =
    let new_size = current_size + String.length raw_tx in
    if new_size >= maximum_cumulative_size then return `Stop
    else return (`Keep new_size)
  in
  let* popped_txs =
    Tx_container.pop_transactions
      ~maximum_cumulative_size
      ~initial_validation_state
      ~validate_tx
  in
  let*! hashes =
    Rollup_node_rpc.Etherlink.inject_transactions
    (* The timestamp is ignored in observer and proxy mode, it's just for
       compatibility with sequencer mode. *)
      ~timestamp:(Misc.now ())
      ~smart_rollup_address
      ~transactions:popped_txs
  in
  match hashes with
  | Error trace ->
      let*! () = Tx_pool_events.transaction_injection_failed trace in
      return_unit
  | Ok hashes ->
      let* () =
        Tx_container.confirm_transactions
          ~clear_pending_queue_after:true
          ~confirmed_txs:(List.to_seq hashes)
      in
      let*! () =
        List.iter_s
          (fun hash -> Tx_pool_events.transaction_injected ~hash)
          hashes
      in
      return_unit

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
  let* l2_chain_id, chain_family =
    if finalized_view then
      if
        (* When finalized_view is set, it's too early to request the
           feature flag from the rollup node. *)
        Option.is_some config.experimental_features.l2_chains
      then
        (* The finalized view of the proxy mode and the multichain feature are not compatible. *)
        tzfail (Node_error.Proxy_finalize_with_multichain `Node)
      else return (None, L2_types.EVM)
    else
      let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
      Rollup_node_rpc.single_chain_id_and_family ~config ~enable_multichain
  in
  let* on_new_head, tx_container =
    match
      ( config.experimental_features.enable_send_raw_transaction,
        config.proxy.evm_node_endpoint,
        config.experimental_features.enable_tx_queue )
    with
    | true, None, Some tx_queue_config ->
        let* () =
          Tx_queue.start
            ~config:tx_queue_config
            ~keep_alive:config.keep_alive
            ()
        in
        let tx_container =
          (module Tx_queue.Tx_container : Services_backend_sig.Tx_container)
        in
        return
        @@ ( Some
               (fun () ->
                 tx_queue_pop_and_inject
                   (module Rollup_node_rpc)
                   tx_container
                   ~smart_rollup_address),
             tx_container )
    | true, None, None ->
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
              chain_family;
            }
        in
        return
          ( Some Tx_pool.pop_and_inject_transactions_lazy,
            (module Tx_pool.Tx_container : Services_backend_sig.Tx_container) )
    | enable_send_raw_transaction, evm_node_endpoint, _ ->
        let evm_node_endpoint =
          if enable_send_raw_transaction then evm_node_endpoint else None
        in
        return @@ (None, container_forward_tx ~evm_node_endpoint ~keep_alive)
  in
  let () =
    Rollup_node_follower.start
      ~keep_alive:config.keep_alive
      ?on_new_head
      ~rollup_node_endpoint
      ()
  in

  let* server_finalizer =
    Rpc_server.start_public_server
      ~rpc_server_family:(Rpc_types.Single_chain_node_rpc_server chain_family)
      ~l2_chain_id
      validation_mode
      config
      tx_container
      ((module Rollup_node_rpc), smart_rollup_address)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer server_finalizer tx_container
  in
  let wait, _resolve = Lwt.wait () in
  let* () = wait in
  if finalized_view then
    let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
    if enable_multichain then
      tzfail (Node_error.Proxy_finalize_with_multichain `Kernel)
    else return_unit
  else return_unit
