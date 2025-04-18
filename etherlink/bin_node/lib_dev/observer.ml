(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types
open L2_types

let confirm_txs config confirmed_txs =
  let open Lwt_result_syntax in
  if Configuration.is_tx_queue_enabled config then
    Tx_queue.confirm_transactions
      ~clear_pending_queue_after:false
      ~confirmed_txs
  else return_unit

(** [on_new_blueprint evm_node_endpoint next_blueprint_number
    blueprint] applies evm events found in the blueprint, then applies
    the blueprint itself.

    There are 3 possible outcomes for the blueprint's application:
    1. Application succeeds, a block is produced.
    2. Application fails (e.g. invalid signature).
    3. Blueprint is correct, but is on a different branch.

    The case (3.) can happen if the rollup node flushed its delayed inbox
    into a forced blueprint. The sequencer has performed a reorganization and
    starts submitting blocks from the new branch.
*)
let on_new_blueprint config evm_node_endpoint next_blueprint_number
    (({delayed_transactions; blueprint; _} : Blueprint_types.with_events) as
     blueprint_with_events) =
  let open Lwt_result_syntax in
  let (Qty level) = blueprint.number in
  let (Qty number) = next_blueprint_number in
  if Z.(equal level number) then
    let events =
      Blueprint_types.events_of_blueprint_with_events blueprint_with_events
    in
    (* Apply blueprint is allowed to fail. *)
    let*! res =
      Evm_context.apply_blueprint
        ~events
        blueprint.timestamp
        blueprint.payload
        delayed_transactions
    in
    match res with
    | Error (Evm_context.Cannot_apply_blueprint _ :: _) -> (
        (* Apply blueprint failed, it is potentially the sign of a reorg.
           If it's not a reorg, the call to {!potential_observer_reorg} will
           exit as soon as possible anyway.
        *)
        let* reorg =
          Evm_context.potential_observer_reorg
            evm_node_endpoint
            blueprint_with_events
        in
        match reorg with
        | Some level -> return (`Restart_from level)
        | None ->
            (* We could not apply the received blueprint, and could not reorg
               properly. There is something wrong with our state, we should
               crash. *)
            failwith
              "Could not recover from failing to apply latest received \
               blueprint.")
    | Ok tx_hashes ->
        let* () = confirm_txs config tx_hashes in
        return `Continue
    | Error (Node_error.Diverged {must_exit = false; _} :: _) ->
        (* If we have diverged, but should keep the node alive. This happens
           when the node successfully reset its head. We restart the blueprints
           follower to the new expected blueprint number. *)
        let*! head_info = Evm_context.head_info () in
        return (`Restart_from head_info.next_blueprint_number)
    | Error err -> fail err
  else if Z.(lt level number) then
    (* The endpoint's stream has provided a blueprint smaller than
       expected. It could be the sign of a reorganization. *)
    let* reorg =
      Evm_context.potential_observer_reorg
        evm_node_endpoint
        blueprint_with_events
    in
    match reorg with
    | Some level -> return (`Restart_from level)
    | None -> return (`Restart_from next_blueprint_number)
  else
    (* We received a blueprint in the future. Let’s try again. *)
    let*! () =
      Blueprint_events.unexpected_blueprint_from_remote_node
        ~received:blueprint.number
        ~expected:next_blueprint_number
    in
    return (`Restart_from next_blueprint_number)

let install_finalizer_observer ~rollup_node_tracking finalizer_public_server
    finalizer_private_server finalizer_rpc_process =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = finalizer_public_server () in
  let* () = finalizer_private_server () in
  let* () = Option.iter_s (fun f -> f ()) finalizer_rpc_process in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  let* () = Tx_queue.shutdown () in
  let* () = Evm_context.shutdown () in
  when_ rollup_node_tracking @@ fun () -> Evm_events_follower.shutdown ()

let container_forward_tx ~keep_alive ~evm_node_endpoint :
    (module Services_backend_sig.Tx_container) =
  (module struct
    let nonce ~next_nonce _address = Lwt_result.return next_nonce

    let add ~next_nonce:_ _tx_object ~raw_tx =
      Injector.send_raw_transaction
        ~keep_alive
        ~base:evm_node_endpoint
        ~raw_tx:(Ethereum_types.hex_to_bytes raw_tx)

    let find _hash = Lwt_result.return None

    let content () =
      Lwt_result.return {pending = AddressMap.empty; queued = AddressMap.empty}
  end)

let main ?network ?kernel_path ~data_dir ~(config : Configuration.t) ~no_sync
    ~init_from_snapshot () =
  let open Lwt_result_syntax in
  let open Configuration in
  let*? {
          evm_node_endpoint;
          threshold_encryption_bundler_endpoint;
          rollup_node_tracking;
        } =
    Configuration.observer_config_exn config
  in
  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address
      ~keep_alive:config.keep_alive
      ~evm_node_endpoint
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in

  let*? snapshot_url =
    Option.map_e
      (Snapshots.interpolate_snapshot_provider
         ~rollup_address:smart_rollup_address
         ?network
         (Option.value
            ~default:Configuration.default_history_mode
            config.history_mode))
      init_from_snapshot
  in
  let* _loaded =
    Evm_context.start
      ~configuration:config
      ~data_dir
      ?kernel_path
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ~store_perm:`Read_write
      ?snapshot_url
      ()
  in
  let* ro_ctxt =
    Evm_ro_context.load ?network ~smart_rollup_address ~data_dir config
  in

  let evm_node_endpoint =
    match threshold_encryption_bundler_endpoint with
    | Some endpoint -> endpoint
    | None -> evm_node_endpoint
  in
  let observer_backend =
    Evm_ro_context.ro_backend ro_ctxt config ~evm_node_endpoint
  in

  let* tx_container, ping_tx_pool =
    match config.experimental_features.enable_tx_queue with
    | Some tx_queue_config ->
        let* () =
          Tx_queue.start
            ~config:tx_queue_config
            ~keep_alive:config.keep_alive
            ()
        in
        return
          ( (module Tx_queue.Tx_container : Services_backend_sig.Tx_container),
            false )
    | None ->
        if config.finalized_view then
          let tx_container =
            container_forward_tx
              ~keep_alive:config.keep_alive
              ~evm_node_endpoint
          in
          return (tx_container, false)
        else
          let* () =
            Tx_pool.start
              {
                backend = observer_backend;
                smart_rollup_address =
                  Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
                    smart_rollup_address;
                mode = Relay;
                tx_timeout_limit = config.tx_pool_timeout_limit;
                tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
                tx_pool_tx_per_addr_limit =
                  Int64.to_int config.tx_pool_tx_per_addr_limit;
              }
          in
          return
            ( (module Tx_pool.Tx_container : Services_backend_sig.Tx_container),
              true )
  in

  Metrics.init
    ~mode:"observer"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address ;

  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ro_ctxt in

  let* chain_family =
    match (config.experimental_features.l2_chains, enable_multichain) with
    | None, false -> return EVM
    | None, true -> tzfail Node_error.Singlechain_node_multichain_kernel
    | Some [_], false ->
        let*! () = Events.multichain_node_singlechain_kernel () in
        return EVM
    | Some [l2_chain], true ->
        let* chain_family =
          Evm_ro_context.read_chain_family ro_ctxt l2_chain.chain_id
        in
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

  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~evm_services:
        Evm_ro_context.(evm_services_methods ro_ctxt time_between_blocks)
      ~data_dir
      ~rpc_server_family:(Rpc_types.Single_chain_node_rpc_server chain_family)
      Stateless
      config
      tx_container
      (observer_backend, smart_rollup_address)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~rpc_server_family:(Rpc_types.Single_chain_node_rpc_server chain_family)
      config
      tx_container
      (observer_backend, smart_rollup_address)
  in

  let* () =
    if rollup_node_tracking then
      let* () =
        Evm_events_follower.start
          {
            rollup_node_endpoint = config.rollup_node_endpoint;
            keep_alive = config.keep_alive;
            filter_event =
              (function
              | New_delayed_transaction _ | Upgrade_event _
              | Flush_delayed_inbox _ ->
                  false
              | _ -> true);
          }
      in
      let () =
        Rollup_node_follower.start
          ~keep_alive:config.keep_alive
          ~rollup_node_endpoint:config.rollup_node_endpoint
          ()
      in
      return_unit
    else
      let*! () = Rollup_node_follower_events.disabled () in
      return_unit
  in

  let finalizer_rpc_process =
    Option.map
      (fun port ->
        let protected_endpoint =
          Uri.make ~scheme:"http" ~host:config.public_rpc.addr ~port ()
        in
        let private_endpoint =
          Option.map
            (fun {addr; port; _} -> Uri.make ~scheme:"http" ~host:addr ~port ())
            config.private_rpc
        in
        Rpc.spawn_main
          ~exposed_port:config.public_rpc.port
          ~protected_endpoint
          ?private_endpoint
          ~data_dir
          ())
      config.experimental_features.spawn_rpc
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_observer
      ~rollup_node_tracking
      finalizer_public_server
      finalizer_private_server
      finalizer_rpc_process
  in

  let*! next_blueprint_number = Evm_context.next_blueprint_number () in

  if no_sync then
    let task, _resolver = Lwt.task () in
    let*! () = task in
    return_unit
  else
    let* () =
      Blueprints_follower.start
        ~ping_tx_pool
        ~time_between_blocks
        ~evm_node_endpoint
        ~next_blueprint_number
        (on_new_blueprint config evm_node_endpoint)
    and* () =
      Drift_monitor.run ~evm_node_endpoint Evm_context.next_blueprint_number
    and* () =
      if Configuration.is_tx_queue_enabled config then
        Tx_queue.beacon ~evm_node_endpoint ~tick_interval:0.05
      else return_unit
    in
    return_unit
