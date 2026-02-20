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
let on_new_blueprint evm_node_endpoint next_blueprint_number
    (({delayed_transactions; blueprint; _} : Blueprint_types.with_events) as
     blueprint_with_events) ~expected_block_hash =
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
        ?expected_block_hash
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
          Tx_queue.confirm_transactions
            ~clear_pending_queue_after:false
            ~confirmed_txs
        in
        let*! head = Evm_context.head_info () in
        let* storage_version = Evm_state.storage_version head.evm_state in
        let sub_block_latency_disabled =
          Storage_version.sub_block_latency_entrypoints_disabled
            ~storage_version
        in
        return
          (`Continue
             Blueprints_follower.
               {sbl_callbacks_activated = not sub_block_latency_disabled})
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

let on_finalized_levels ~rollup_node_tracking ~l1_level ~start_l2_level
    ~end_l2_level =
  let open Lwt_result_syntax in
  if not rollup_node_tracking then
    Evm_context.apply_finalized_levels ~l1_level ~start_l2_level ~end_l2_level
  else return_unit

let on_next_block_info timestamp number =
  let open Lwt_result_syntax in
  Broadcast.notify_next_block_info timestamp number ;
  let*! () = Events.next_block_info timestamp number in
  let* () = Evm_context.next_block_info timestamp number in
  return_unit

let on_inclusion (txn : Broadcast.transaction) hash =
  let open Lwt_result_syntax in
  Broadcast.notify_inclusion txn hash ;
  let*! () = Events.inclusion hash in
  let* opt_receipt = Evm_context.execute_single_transaction txn hash in
  Option.iter
    (fun (r : Transaction_receipt.t) ->
      Broadcast.notify_transaction_result
        {hash = r.transactionHash; result = Ok r})
    opt_receipt ;
  return_unit

let install_finalizer_observer ~rollup_node_tracking finalizer_public_server
    finalizer_private_server finalizer_rpc_process telemetry_cleanup =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = telemetry_cleanup () in
  let* () = Events.shutdown_node ~exit_status in
  let* () = finalizer_public_server () in
  let* () = finalizer_private_server () in
  let* () = Option.iter_s (fun f -> f ()) finalizer_rpc_process in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_queue.shutdown () in
  let* () = Evm_context.shutdown () in
  when_ rollup_node_tracking @@ fun () -> Evm_events_follower.shutdown ()

let main ?network ?kernel_path ~(config : Configuration.t) ~no_sync
    ~init_from_snapshot ~sandbox () =
  let open Lwt_result_syntax in
  let open Configuration in
  let* config =
    if config.experimental_features.preconfirmation_stream_enabled then
      let*! () = Events.forced_native_execution_instant_confirmation () in
      return
        {
          config with
          kernel_execution =
            {config.kernel_execution with native_execution_policy = Always};
        }
    else return config
  in
  let* telemetry_cleanup =
    Octez_telemetry.Opentelemetry_setup.setup
      ~data_dir:config.data_dir
      ~service_namespace:"evm_node"
      ~service_name:"observer"
      ~version:Tezos_version_value.Bin_version.octez_evm_node_version_string
      config.opentelemetry.config
  in
  let*? {evm_node_endpoint; rollup_node_tracking; _} =
    Configuration.observer_config_exn config
  in
  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address
      ~keep_alive:config.keep_alive
      ~timeout:config.rpc_timeout
      evm_node_endpoint
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~timeout:config.rpc_timeout
      ~evm_node_endpoint
      ()
  in

  let*? snapshot_source =
    let open Result_syntax in
    match init_from_snapshot with
    | None -> return_none
    | Some provider ->
        let+ url =
          Snapshots.interpolate_snapshot_provider
            ~rollup_address:smart_rollup_address
            ?network
            (Option.value
               ~default:Configuration.default_history_mode
               config.history_mode)
            provider
        in
        Some (Evm_context.Url_legacy url)
  in

  let (Ex_chain_family chain_family) =
    Configuration.retrieve_chain_family
      ~l2_chains:config.experimental_features.l2_chains
  in

  let* () =
    Tx_queue.start
      ~config:config.tx_queue
      ~keep_alive:config.keep_alive
      ~timeout:config.rpc_timeout
      ~start_injector_worker:true
      ()
  in

  let* _loaded =
    Evm_context.start
      ~configuration:config
      ?kernel_path
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ~store_perm:Read_write
      ?snapshot_source
      ()
  in

  let* () =
    when_ sandbox @@ fun () ->
    let*! {next_blueprint_number; _} = Evm_context.head_info () in
    let* pk =
      Batch.call
        ~timeout:config.rpc_timeout
        ~keep_alive:config.keep_alive
        (module Rpc_encodings.Sequencer)
        ~evm_node_endpoint
        (Block_parameter (Number next_blueprint_number))
    in
    Evm_context.patch_state
      ~key:Durable_storage_path.sequencer_key
      ~value:(Signature.Public_key.to_b58check pk)
      ()
  in

  (* One domain for the Lwt scheduler, one domain for Evm_context, one domain
     for spawning processes, one for the Irmin GC and the rest of the RPCs. *)
  let pool = Lwt_domain.setup_pool (max 1 (Misc.domain_count_cap () - 4)) in
  let* ro_ctxt =
    Evm_ro_context.load ~pool ?network ~smart_rollup_address config
  in
  let* () = Evm_ro_context.preload_known_kernels ro_ctxt in

  let (module Rpc_backend) =
    Evm_ro_context.ro_backend ro_ctxt config ~evm_node_endpoint
  in

  (* Check that the multichain configuration is consistent with the
     kernel config. *)
  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ro_ctxt in
  let* l2_chain_id, _chain_family =
    Rpc_backend.single_chain_id_and_family ~config ~enable_multichain
  in

  Metrics.init
    ~mode:"observer"
    ~tx_pool_size_info:Tx_queue.size_info
    ~smart_rollup_address
    () ;

  let* () =
    Prevalidator.start
      ~max_number_of_chunks:config.sequencer.max_number_of_chunks
      ~chain_family
      Minimal
      (module Rpc_backend)
  in
  let rpc_server_family = Rpc_types.Single_chain_node_rpc_server chain_family in
  let tick () =
    Tx_queue.tx_queue_tick ~evm_node_endpoint:(Rpc evm_node_endpoint)
  in
  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~mode:Observer
      ~l2_chain_id
      ~evm_services:
        Evm_ro_context.(evm_services_methods ro_ctxt time_between_blocks)
      ~rpc_server_family
      ~tick
      config
      ((module Rpc_backend), smart_rollup_address)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~mode:Observer
      ~rpc_server_family
      ~tick
      config
      ((module Rpc_backend), smart_rollup_address)
  in

  let* () =
    if rollup_node_tracking && not sandbox then
      let* () =
        Evm_events_follower.start
          {
            rollup_node_endpoint = config.rollup_node_endpoint;
            keep_alive = config.keep_alive;
            rpc_timeout = config.rpc_timeout;
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
          ~rollup_node_endpoint_timeout:config.rpc_timeout
          ()
      in
      return_unit
    else
      let*! () = Rollup_node_follower_events.disabled () in
      return_unit
  in

  let*! finalizer_rpc_process =
    Option.map_s
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
          ~data_dir:config.data_dir
          ())
      config.experimental_features.spawn_rpc
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_observer
      ~rollup_node_tracking
      finalizer_public_server
      finalizer_private_server
      finalizer_rpc_process
      telemetry_cleanup
  in

  let*! next_blueprint_number = Evm_context.next_blueprint_number () in

  if no_sync then
    let task, _resolver = Lwt.task () in
    let*! () = task in
    return_unit
  else (
    Misc.background_task ~name:"drift_monitor" (fun () ->
        Drift_monitor.run
          ~evm_node_endpoint
          ~timeout:config.rpc_timeout
          ~chain_family
          Evm_context.next_blueprint_number) ;
    Misc.background_task ~name:"tx_queue_beacon" (fun () ->
        Tx_queue.tx_queue_beacon
          ~evm_node_endpoint:(Rpc evm_node_endpoint)
          ~tick_interval:(float_of_int config.tx_queue.max_lifespan_s)) ;
    Blueprints_follower.start
      ~multichain:enable_multichain
      ~time_between_blocks
      ~evm_node_endpoint
      ~rpc_timeout:config.rpc_timeout
      ~next_blueprint_number
      ~instant_confirmations:
        config.experimental_features.preconfirmation_stream_enabled
      ~on_new_blueprint:(on_new_blueprint evm_node_endpoint)
      ~on_finalized_levels:(on_finalized_levels ~rollup_node_tracking)
      ~on_next_block_info
      ~on_inclusion
      ~on_dropped:(fun hash reason ->
        Broadcast.notify_transaction_result {hash; result = Error reason} ;
        let* () = Tx_queue.dropped_transaction ~dropped_tx:hash ~reason in
        return_unit)
      ())
