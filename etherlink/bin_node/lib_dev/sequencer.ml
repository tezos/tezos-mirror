(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type sandbox_config = {
  public_key : Signature.public_key;
  secret_key : Signature.secret_key;
  init_from_snapshot : string option;
  network : Configuration.supported_network option;
  funded_addresses : Ethereum_types.address list;
}

let install_finalizer_seq server_public_finalizer server_private_finalizer
    finalizer_rpc_process =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_public_finalizer () in
  let* () = server_private_finalizer () in
  let* () = Option.iter_s (fun f -> f ()) finalizer_rpc_process in
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
  match time_between_blocks with
  | Nothing ->
      (* Bind on a never-resolved promise ensures this call never returns,
         meaning no block will ever be produced. *)
      let task, _resolver = Lwt.task () in
      let*! () = task in
      return_unit
  | Time_between_blocks time_between_blocks ->
      let rec loop last_produced_block =
        let now = Misc.now () in
        (* We force if the last produced block is older than [time_between_blocks]. *)
        let force =
          let diff = Time.Protocol.(diff now last_produced_block) in
          diff >= Int64.of_float time_between_blocks
        in
        let* has_produced_block =
          Block_producer.produce_block ~force ~timestamp:now
        and* () = Lwt.map Result.ok @@ Lwt_unix.sleep 0.5 in
        match has_produced_block with
        | `Block_produced _nb_transactions -> loop now
        | `No_block -> loop last_produced_block
      in
      loop Misc.(now ())

let main ~data_dir ?(genesis_timestamp = Misc.now ()) ~cctxt
    ~(configuration : Configuration.t) ?kernel ?sandbox_config () =
  let open Lwt_result_syntax in
  let open Configuration in
  let is_sandbox = Option.is_some sandbox_config in
  let {rollup_node_endpoint; keep_alive; _} = configuration in
  let*? sequencer_config = Configuration.sequencer_config_exn configuration in
  let* rollup_node_smart_rollup_address =
    if Option.is_some sandbox_config then return_none
    else
      let* sr1 =
        Rollup_services.smart_rollup_address
          ~keep_alive:configuration.keep_alive
          rollup_node_endpoint
      in
      return_some sr1
  in
  let*? snapshot_url =
    match sandbox_config with
    | Some {init_from_snapshot; network; _} ->
        Option.map_e
          (Snapshots.interpolate_snapshot_provider
             ?rollup_address:
               (Option.map
                  Address.of_b58check_exn
                  rollup_node_smart_rollup_address)
             ?network
             Configuration.(Rolling (gc_param_from_retention_period ~days:1)))
          init_from_snapshot
    | None -> Result.return_none
  in
  let* status, smart_rollup_address_typed =
    Evm_context.start
      ~configuration
      ?kernel_path:kernel
      ~data_dir
      ?smart_rollup_address:rollup_node_smart_rollup_address
      ~store_perm:`Read_write
      ~sequencer_wallet:(sequencer_config.sequencer, cctxt)
      ?snapshot_url
      ()
  in
  let smart_rollup_address_b58 = Address.to_string smart_rollup_address_typed in
  let* () =
    match sandbox_config with
    | Some {public_key = pk; funded_addresses; _} ->
        let* () = Evm_context.patch_sequencer_key pk in
        let new_balance =
          Ethereum_types.quantity_of_z Z.(of_int 10_000 * pow (of_int 10) 18)
        in
        let* () =
          List.iter_es
            (fun address -> Evm_context.provision_balance address new_balance)
            funded_addresses
        in
        let* () =
          Option.iter_es
            (fun kernel ->
              match kernel with
              | Wasm_debugger.On_disk _ when status = Loaded ->
                  (* [kernel] being [On_disk] means it was provided by the
                     user. [status] being [Loaded] means the data-dir was
                     already populated or a snapshot was imported.

                     This is the only case where we patch the kernel. If
                     [kernel] is [In_memory], then it was inferred from
                     [network]. If [status] is [Created], then [kernel] was
                     already used as initial kernel. *)
                  Evm_context.patch_kernel kernel
              | _ -> return_unit)
            kernel
        in
        return_unit
    | None -> return_unit
  in

  let*! head = Evm_context.head_info () in
  let (Qty next_blueprint_number) = head.next_blueprint_number in
  let* () =
    Option.iter_es
      (fun _ ->
        Signals_publisher.start
          ~cctxt
          ~smart_rollup_address:smart_rollup_address_b58
          ~sequencer_key:sequencer_config.sequencer
          ~rollup_node_endpoint
          ())
      sequencer_config.blueprints_publisher_config.dal_slots
  in
  let* ro_ctxt =
    Evm_ro_context.load
      ?network:(Option.bind sandbox_config (fun config -> config.network))
      ~smart_rollup_address:smart_rollup_address_typed
      ~data_dir
      configuration
  in
  let* () =
    when_ (not is_sandbox) @@ fun () ->
    Blueprints_publisher.start
      ~blueprints_range:(Evm_ro_context.blueprints_range ro_ctxt)
      ~rollup_node_endpoint
      ~config:sequencer_config.blueprints_publisher_config
      ~latest_level_seen:(Z.pred next_blueprint_number)
      ~keep_alive
      ~drop_duplicate:
        configuration.experimental_features.drop_duplicate_on_injection
      ~order_enabled:
        configuration.experimental_features.blueprints_publisher_order_enabled
      ~tx_queue_enabled:(Configuration.is_tx_queue_enabled configuration)
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
          ~smart_rollup_address:smart_rollup_address_b58
          ~chunks:genesis_chunks
      in
      let* _tx_hashes =
        Evm_context.apply_blueprint genesis_timestamp genesis_payload []
      in
      Blueprints_publisher.publish
        Z.zero
        (Blueprints_publisher_types.Request.Blueprint
           {chunks = genesis_chunks; inbox_payload = genesis_payload})
    else return_unit
  in

  let backend = Evm_ro_context.ro_backend ro_ctxt configuration in
  let* tx_container =
    match configuration.experimental_features.enable_tx_queue with
    | Some tx_queue_config ->
        let* () =
          Tx_queue.start
            ~config:tx_queue_config
            ~keep_alive:configuration.keep_alive
            ()
        in
        return
          (module Tx_queue.Tx_container : Services_backend_sig.Tx_container)
    | None ->
        let* () =
          Tx_pool.start
            {
              backend;
              smart_rollup_address = smart_rollup_address_b58;
              mode = Sequencer;
              tx_timeout_limit = configuration.tx_pool_timeout_limit;
              tx_pool_addr_limit = Int64.to_int configuration.tx_pool_addr_limit;
              tx_pool_tx_per_addr_limit =
                Int64.to_int configuration.tx_pool_tx_per_addr_limit;
            }
        in
        return (module Tx_pool.Tx_container : Services_backend_sig.Tx_container)
  in
  Metrics.init
    ~mode:"sequencer"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address:smart_rollup_address_typed ;
  let* () =
    Block_producer.start
      {
        cctxt;
        smart_rollup_address = smart_rollup_address_b58;
        sequencer_key = sequencer_config.sequencer;
        maximum_number_of_chunks = sequencer_config.max_number_of_chunks;
        uses_tx_queue = Configuration.is_tx_queue_enabled configuration;
      }
  in
  let* () =
    if is_sandbox then
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
  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ro_ctxt in
  let* () =
    match
      (configuration.experimental_features.l2_chains, enable_multichain)
    with
    | None, false -> return_unit
    | None, true -> tzfail Node_error.Singlechain_node_multichain_kernel
    | Some _, false ->
        let*! () = Events.multichain_node_singlechain_kernel () in
        return_unit
    | Some l2_chains, true ->
        List.iter_es
          (fun l2_chain ->
            let chain_id = l2_chain.chain_id in
            let* chain_family =
              Evm_ro_context.read_chain_family ro_ctxt chain_id
            in
            if chain_family = l2_chain.chain_family then return_unit
            else
              tzfail
                (Node_error.Mismatched_chain_family
                   {
                     chain_id;
                     node_family = l2_chain.chain_family;
                     kernel_family = chain_family;
                   }))
          l2_chains
  in
  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~evm_services:
        Evm_ro_context.(
          evm_services_methods ro_ctxt sequencer_config.time_between_blocks)
      ~data_dir
      ~rpc_server_family:
        (if enable_multichain then Rpc_types.Multichain_sequencer_rpc_server
         else Rpc_types.Single_chain_node_rpc_server EVM)
      (* When the tx_queue is enabled the validation is done in the
         block_producer instead of in the RPC. This allows for a more
         accurate validation as it's delayed up to when the block is
         created. *)
      (if Configuration.is_tx_queue_enabled configuration then Stateless
       else Full)
      configuration
      tx_container
      (backend, smart_rollup_address_typed)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~rpc_server_family:
        (if enable_multichain then Rpc_types.Multichain_sequencer_rpc_server
         else Rpc_types.Single_chain_node_rpc_server EVM)
      ~block_production:`Single_node
      configuration
      tx_container
      (backend, smart_rollup_address_typed)
  in
  let finalizer_rpc_process =
    Option.map
      (fun port ->
        let protected_endpoint =
          Uri.make ~scheme:"http" ~host:configuration.public_rpc.addr ~port ()
        in
        let private_endpoint =
          Option.map
            (fun {addr; port; _} -> Uri.make ~scheme:"http" ~host:addr ~port ())
            configuration.private_rpc
        in
        Rpc.spawn_main
          ~exposed_port:configuration.public_rpc.port
          ~protected_endpoint
          ?private_endpoint
          ~data_dir
          ())
      configuration.experimental_features.spawn_rpc
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq
      finalizer_public_server
      finalizer_private_server
      finalizer_rpc_process
  in
  let* () = loop_sequencer sequencer_config in
  return_unit
