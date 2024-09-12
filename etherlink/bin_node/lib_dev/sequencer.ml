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

let loop_sequencer (sequencer_config : Configuration.sequencer) =
  let open Lwt_result_syntax in
  let time_between_blocks = sequencer_config.time_between_blocks in
  let rec loop last_produced_block =
    match time_between_blocks with
    | Nothing ->
        (* Bind on a never-resolved promise ensures this call never returns,
           meaning no block will ever be produced. *)
        let task, _resolver = Lwt.task () in
        let*! () = task in
        return_unit
    | Time_between_blocks time_between_blocks ->
        let now = Misc.now () in
        (* We force if the last produced block is older than [time_between_blocks]. *)
        let force =
          let diff = Time.Protocol.(diff now last_produced_block) in
          diff >= Int64.of_float time_between_blocks
        in
        let* nb_transactions =
          Block_producer.produce_block ~force ~timestamp:now
        and* () = Lwt.map Result.ok @@ Lwt_unix.sleep 0.5 in
        if nb_transactions > 0 || force then loop now
        else loop last_produced_block
  in
  loop Misc.(now ())

let main ~data_dir ?(genesis_timestamp = Misc.now ()) ~cctxt
    ~(configuration : Configuration.t) ?kernel ?sandbox_key () =
  let open Lwt_result_syntax in
  let open Configuration in
  let {rollup_node_endpoint; keep_alive; _} = configuration in
  let*? sequencer_config = Configuration.sequencer_config_exn configuration in
  let* rollup_node_smart_rollup_address =
    if Option.is_some sandbox_key then return_none
    else
      let* sr1 =
        Rollup_services.smart_rollup_address
          ~keep_alive:configuration.keep_alive
          rollup_node_endpoint
      in
      return_some sr1
  in
  let* status, smart_rollup_address_typed =
    Evm_context.start
      ?kernel_path:kernel
      ~data_dir
      ~preimages:configuration.kernel_execution.preimages
      ~preimages_endpoint:configuration.kernel_execution.preimages_endpoint
      ~fail_on_missing_blueprint:true
      ?smart_rollup_address:rollup_node_smart_rollup_address
      ~store_perm:`Read_write
      ~block_storage_sqlite3:
        configuration.experimental_features.block_storage_sqlite3
      ()
  in
  let smart_rollup_address = Address.to_string smart_rollup_address_typed in
  let* () =
    match sandbox_key with
    | Some (pk, _sk) -> Evm_context.patch_sequencer_key pk
    | None -> return_unit
  in

  let*! head = Evm_context.head_info () in
  let (Qty next_blueprint_number) = head.next_blueprint_number in
  Metrics.set_level ~level:(Z.pred next_blueprint_number) ;
  let* () =
    Option.iter_es
      (fun _ ->
        Signals_publisher.start
          ~cctxt
          ~smart_rollup_address
          ~sequencer_key:sequencer_config.sequencer
          ~rollup_node_endpoint
          ())
      sequencer_config.blueprints_publisher_config.dal_slots
  in
  let* () =
    Blueprints_publisher.start
      ~rollup_node_endpoint
      ~config:sequencer_config.blueprints_publisher_config
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
          ~sequencer_key:sequencer_config.sequencer
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
  let* ro_ctxt =
    Evm_ro_context.load
      ~smart_rollup_address:smart_rollup_address_typed
      ~data_dir
      ~preimages:configuration.kernel_execution.preimages
      ?preimages_endpoint:configuration.kernel_execution.preimages_endpoint
      ()
  in

  let backend = Evm_ro_context.ro_backend ro_ctxt configuration in
  let* () =
    Tx_pool.start
      {
        rollup_node = backend;
        smart_rollup_address;
        mode = Sequencer;
        tx_timeout_limit = configuration.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int configuration.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int configuration.tx_pool_tx_per_addr_limit;
        max_number_of_chunks =
          (match configuration.sequencer with
          | Some {max_number_of_chunks; _} -> Some max_number_of_chunks
          | None -> None);
      }
  in
  Metrics.init ~mode:"sequencer" ~tx_pool_size_info:Tx_pool.size_info ;
  let* () =
    Block_producer.start
      {
        cctxt;
        smart_rollup_address;
        sequencer_key = sequencer_config.sequencer;
        maximum_number_of_chunks = sequencer_config.max_number_of_chunks;
      }
  in
  let* () =
    if Option.is_some sandbox_key then
      let*! () = Events.sandbox_started (Z.pred next_blueprint_number) in
      return_unit
    else
      let* () =
        Evm_events_follower.start
          {rollup_node_endpoint; keep_alive; filter_event = (fun _ -> true)}
      in
      let () =
        Rollup_node_follower.start
          ~keep_alive:configuration.keep_alive
          ~proxy:false
          ~rollup_node_endpoint
          ()
      in
      return_unit
  in
  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~evm_services:
        Evm_ro_context.(
          evm_services_methods ro_ctxt sequencer_config.time_between_blocks)
      configuration
      (backend, smart_rollup_address_typed)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~block_production:`Single_node
      configuration
      (backend, smart_rollup_address_typed)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq finalizer_public_server finalizer_private_server
  in
  let* () = loop_sequencer sequencer_config in
  return_unit
