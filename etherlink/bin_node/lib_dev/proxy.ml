(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

let install_finalizer ~(tx_container : _ Services_backend_sig.tx_container)
    server_finalizer =
  let open Lwt_syntax in
  let (module Tx_container) =
    Services_backend_sig.tx_container_module tx_container
  in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_finalizer () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_container.shutdown () in
  Evm_context.shutdown ()

let container_forward_tx (type f) ~(chain_family : f L2_types.chain_family)
    ~evm_node_endpoint ~keep_alive ~timeout :
    f Services_backend_sig.tx_container tzresult =
  let (module Tx_container) =
    (module struct
      type address = Ethereum_types.address

      type transaction_object = Transaction_object.t

      let nonce ~next_nonce _address = Lwt_result.return next_nonce

      let add_pending_callback _ ~callback:_ = Lwt_result_syntax.return_unit

      let add ?callback:_ ~next_nonce:_ _tx_object ~raw_tx =
        match evm_node_endpoint with
        | Some evm_node_endpoint ->
            Injector.send_raw_transaction
              ~keep_alive
              ~timeout
              ~base:evm_node_endpoint
              ~raw_tx:(Ethereum_types.hex_to_bytes raw_tx)
        | None ->
            Lwt.return_ok
            @@ Error
                 "the node is in read-only mode, it doesn't accept transactions"

      let find _hash = Lwt_result.return None

      let content () =
        Lwt_result.return
          Transaction_object.
            {
              pending = Ethereum_types.AddressMap.empty;
              queued = Ethereum_types.AddressMap.empty;
            }

      let shutdown () = Lwt_result_syntax.return_unit

      let clear () = Lwt_result_syntax.return_unit

      let tx_queue_tick ~evm_node_endpoint:_ = Lwt_result_syntax.return_unit

      let tx_queue_beacon ~evm_node_endpoint:_ ~tick_interval:_ =
        Lwt_result_syntax.return_unit

      let pop_transactions ~maximum_cumulative_size:_ ~validate_tx:_
          ~initial_validation_state:_ =
        Lwt_result_syntax.return_nil

      let size_info () =
        Lwt_result.return
          Metrics.Tx_pool.{number_of_addresses = 0; number_of_transactions = 0}

      let confirm_transactions ~clear_pending_queue_after:_ ~confirmed_txs:_ =
        Lwt_result_syntax.return_unit

      let dropped_transaction ~dropped_tx:_ ~reason:_ =
        Lwt_result_syntax.return_unit
    end : Services_backend_sig.Tx_container
      with type address = Ethereum_types.address
       and type transaction_object = Transaction_object.t)
  in
  let open Result_syntax in
  match chain_family with
  | EVM -> return @@ Services_backend_sig.Evm_tx_container (module Tx_container)
  | Michelson ->
      error_with "Proxy.container_forward_tx not implemented for Tezlink"

let tx_queue_pop_and_inject (type f)
    (module Rollup_node_rpc : Services_backend_sig.S)
    ~(tx_container : f Services_backend_sig.tx_container) ~smart_rollup_address
    =
  let open Lwt_result_syntax in
  match tx_container with
  | Evm_tx_container m -> (
      let (module Tx_container) = m in
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
          return_unit)
  | Michelson_tx_container _ -> return_unit

let main
    ({
       keep_alive;
       rollup_node_endpoint;
       rpc_timeout;
       experimental_features = {drop_duplicate_on_injection; _};
       _;
     } as config :
      Configuration.t) =
  let open Lwt_result_syntax in
  let* smart_rollup_address =
    Rollup_services.smart_rollup_address
      ~keep_alive:config.keep_alive
      ~timeout:rpc_timeout
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

    let timeout = rpc_timeout

    let drop_duplicate_on_injection = drop_duplicate_on_injection

    let smart_rollup_address = smart_rollup_address

    let finalized = finalized_view

    let ignore_block_param = config.proxy.ignore_block_param
  end) in
  let validation_mode =
    match config.proxy.evm_node_endpoint with
    | Some _base -> Prevalidator.Minimal
    | None -> Full
  in
  let* l2_chain_id, Ex_chain_family chain_family =
    if finalized_view then
      if
        (* When finalized_view is set, it's too early to request the
           feature flag from the rollup node. *)
        Option.is_some config.experimental_features.l2_chains
      then
        (* The finalized view of the proxy mode and the multichain feature are not compatible. *)
        tzfail (Node_error.Proxy_finalize_with_multichain `Node)
      else return (None, L2_types.Ex_chain_family EVM)
    else
      let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
      Rollup_node_rpc.single_chain_id_and_family ~config ~enable_multichain
  in
  let* on_new_head, tx_container =
    match
      ( config.experimental_features.enable_send_raw_transaction,
        config.proxy.evm_node_endpoint )
    with
    | true, None ->
        let start, tx_container = Tx_queue.tx_container ~chain_family in
        let* () =
          start
            ~config:config.tx_queue
            ~keep_alive:config.keep_alive
            ~timeout:config.rpc_timeout
            ~start_injector_worker:true
            ()
        in
        return
        @@ ( Some
               (fun () ->
                 tx_queue_pop_and_inject
                   (module Rollup_node_rpc)
                   ~tx_container
                   ~smart_rollup_address),
             tx_container )
    | enable_send_raw_transaction, evm_node_endpoint ->
        let evm_node_endpoint =
          if enable_send_raw_transaction then evm_node_endpoint else None
        in
        let*? tx_container =
          container_forward_tx
            ~chain_family
            ~evm_node_endpoint
            ~keep_alive
            ~timeout:config.rpc_timeout
        in
        return (None, tx_container)
  in
  let* () =
    Prevalidator.start ~chain_family validation_mode (module Rollup_node_rpc)
  in
  let () =
    Rollup_node_follower.start
      ~keep_alive:config.keep_alive
      ?on_new_head
      ~rollup_node_endpoint
      ~rollup_node_endpoint_timeout:rpc_timeout
      ()
  in

  let* server_finalizer =
    Rpc_server.start_public_server
      ~mode:(Proxy tx_container)
      ~rpc_server_family:(Rpc_types.Single_chain_node_rpc_server chain_family)
      ~l2_chain_id
      ~tick:(fun () -> Lwt_result_syntax.return_unit)
      config
      ((module Rollup_node_rpc), smart_rollup_address)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer server_finalizer ~tx_container
  in
  let wait, _resolve = Lwt.wait () in
  let* () = wait in
  if finalized_view then
    let* enable_multichain = Rollup_node_rpc.is_multichain_enabled () in
    if enable_multichain then
      tzfail (Node_error.Proxy_finalize_with_multichain `Kernel)
    else return_unit
  else return_unit
