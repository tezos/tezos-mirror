(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

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
let on_new_blueprint (tx_container : (module Services_backend_sig.Tx_container))
    evm_node_endpoint next_blueprint_number
    (({delayed_transactions; blueprint; _} : Blueprint_types.with_events) as
     blueprint_with_events) =
  let open Lwt_result_syntax in
  let (module Tx_container) = tx_container in
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
    | Ok confirmed_txs ->
        let* () =
          Tx_container.confirm_transactions
            ~clear_pending_queue_after:false
            ~confirmed_txs
        in
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
    finalizer_private_server finalizer_rpc_process
    (module Tx_container : Services_backend_sig.Tx_container) =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = finalizer_public_server () in
  let* () = finalizer_private_server () in
  let* () = Option.iter_s (fun f -> f ()) finalizer_rpc_process in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_container.shutdown () in
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

    let shutdown () = Lwt_result_syntax.return_unit

    let clear () = Lwt_result_syntax.return_unit

    let tx_queue_tick ~evm_node_endpoint:_ = Lwt_result_syntax.return_unit

    let tx_queue_beacon ~evm_node_endpoint:_ ~tick_interval:_ =
      Lwt_result_syntax.return_unit

    let lock_transactions () = Lwt_result_syntax.return_unit

    let unlock_transactions () = Lwt_result_syntax.return_unit

    let is_locked () = Lwt_result_syntax.return_false

    let confirm_transactions ~clear_pending_queue_after:_ ~confirmed_txs:_ =
      Lwt_result_syntax.return_unit

    let pop_transactions ~maximum_cumulative_size:_ ~validate_tx:_
        ~initial_validation_state:_ =
      Lwt_result_syntax.return_nil
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

  let tx_container, ping_tx_pool =
    match config.experimental_features.enable_tx_queue with
    | Some _tx_queue_config ->
        ( (module Tx_queue.Tx_container : Services_backend_sig.Tx_container),
          false )
    | None ->
        if config.finalized_view then
          let tx_container =
            container_forward_tx
              ~keep_alive:config.keep_alive
              ~evm_node_endpoint
          in
          (tx_container, false)
        else
          ( (module Tx_pool.Tx_container : Services_backend_sig.Tx_container),
            true )
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
      ~tx_container
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

  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ro_ctxt in
  let* l2_chain_id, chain_family =
    let (module Backend) = observer_backend in
    Backend.single_chain_id_and_family ~config ~enable_multichain
  in

  let* () =
    match config.experimental_features.enable_tx_queue with
    | Some tx_queue_config ->
        Tx_queue.start ~config:tx_queue_config ~keep_alive:config.keep_alive ()
    | None ->
        if config.finalized_view then return_unit
        else
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
              chain_family;
            }
  in

  Metrics.init
    ~mode:"observer"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address ;

  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~l2_chain_id
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
      tx_container
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
        (on_new_blueprint tx_container evm_node_endpoint)
    and* () =
      Drift_monitor.run ~evm_node_endpoint Evm_context.next_blueprint_number
    and* () =
      let (module Tx_container) = tx_container in
      Tx_container.tx_queue_beacon
        ~evm_node_endpoint:(Rpc evm_node_endpoint)
        ~tick_interval:0.05
    in
    return_unit
