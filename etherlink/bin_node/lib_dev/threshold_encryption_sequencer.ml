(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let install_finalizer_seq server_public_finalizer server_private_finalizer =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_public_finalizer () in
  let* () = server_private_finalizer () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  let* () = Evm_events_follower.shutdown () in
  let* () = Blueprints_publisher.shutdown () in
  let* () = Signals_publisher.shutdown () in
  return_unit

let main ~data_dir ?(genesis_timestamp = Misc.now ()) ~cctxt
    ~(configuration : Configuration.t) ?kernel () =
  let open Lwt_result_syntax in
  let open Configuration in
  let {rollup_node_endpoint; keep_alive; _} = configuration in
  let* smart_rollup_address =
    Rollup_services.smart_rollup_address
      ~keep_alive:configuration.keep_alive
      rollup_node_endpoint
  in
  let*? (Threshold_encryption_sequencer threshold_encryption_sequencer_config) =
    Configuration.threshold_encryption_sequencer_config_exn configuration
  in
  let* status, _smart_rollup_address =
    Evm_context.start
      ?kernel_path:kernel
      ~data_dir
      ~preimages:configuration.kernel_execution.preimages
      ~preimages_endpoint:configuration.kernel_execution.preimages_endpoint
      ~smart_rollup_address
      ~fail_on_missing_blueprint:true
      ~store_perm:`Read_write
      ~block_storage_sqlite3:
        configuration.experimental_features.block_storage_sqlite3
      ()
  in
  let*! (Qty next_blueprint_number) = Evm_context.next_blueprint_number () in
  let* () =
    Option.iter_es
      (fun _ ->
        Signals_publisher.start
          ~cctxt
          ~smart_rollup_address
          ~sequencer_key:threshold_encryption_sequencer_config.sequencer
          ~rollup_node_endpoint
          ())
      threshold_encryption_sequencer_config.blueprints_publisher_config
        .dal_slots
  in
  let* () =
    Blueprints_publisher.start
      ~rollup_node_endpoint
      ~config:threshold_encryption_sequencer_config.blueprints_publisher_config
      ~latest_level_seen:(Z.pred next_blueprint_number)
      ~keep_alive
      ()
  in
  let* () =
    if status = Created then
      (* Create the first empty block. *)
      let* genesis_chunks =
        Sequencer_blueprint.prepare
          ~cctxt
          ~sequencer_key:threshold_encryption_sequencer_config.sequencer
          ~timestamp:genesis_timestamp
          ~transactions:[]
          ~delayed_transactions:[]
          ~number:Ethereum_types.(Qty Z.zero)
          ~parent_hash:Ethereum_types.genesis_parent_hash
      in
      let genesis_payload =
        Sequencer_blueprint.create_inbox_payload
          ~smart_rollup_address
          ~chunks:genesis_chunks
      in
      let* () =
        Evm_context.apply_blueprint genesis_timestamp genesis_payload []
      in
      Blueprints_publisher.publish
        Z.zero
        (Blueprints_publisher_types.Request.Blueprint
           {chunks = genesis_chunks; inbox_payload = genesis_payload})
    else return_unit
  in

  let smart_rollup_address_typed =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in

  let* ro_ctxt =
    Evm_ro_context.load
      ~smart_rollup_address:smart_rollup_address_typed
      ~data_dir
      ~preimages:configuration.kernel_execution.preimages
      ?preimages_endpoint:configuration.kernel_execution.preimages_endpoint
      ()
  in
  let ro_backend = Evm_ro_context.ro_backend ro_ctxt configuration in

  let* () =
    Tx_pool.start
      {
        rollup_node = ro_backend;
        smart_rollup_address;
        mode = Sequencer;
        tx_timeout_limit = configuration.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int configuration.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int configuration.tx_pool_tx_per_addr_limit;
        max_number_of_chunks =
          Some threshold_encryption_sequencer_config.max_number_of_chunks;
      }
  in
  let* () =
    Threshold_encryption_proposals_handler.start
      {
        sidecar_endpoint = threshold_encryption_sequencer_config.sidecar_endpoint;
        keep_alive = configuration.keep_alive;
        maximum_number_of_chunks =
          threshold_encryption_sequencer_config.max_number_of_chunks;
        time_between_blocks =
          threshold_encryption_sequencer_config.time_between_blocks;
      }
  in
  let* () =
    Threshold_encryption_block_producer.start
      {
        sequencer_key = threshold_encryption_sequencer_config.sequencer;
        smart_rollup_address;
        cctxt;
      }
  in
  let* () =
    Evm_events_follower.start
      {rollup_node_endpoint; keep_alive; filter_event = (fun _ -> true)}
  in
  let () =
    Rollup_node_follower.start ~keep_alive ~proxy:false ~rollup_node_endpoint ()
  in
  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~evm_services:
        Evm_ro_context.(
          evm_services_methods
            ro_ctxt
            threshold_encryption_sequencer_config.time_between_blocks)
      configuration
      (ro_backend, smart_rollup_address_typed)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~block_production:`Threshold_encryption
      configuration
      (ro_backend, smart_rollup_address_typed)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq finalizer_public_server finalizer_private_server
  in
  Threshold_encryption_preblocks_monitor.start
    ~sidecar_endpoint:threshold_encryption_sequencer_config.sidecar_endpoint
    ~time_between_blocks:
      threshold_encryption_sequencer_config.time_between_blocks
