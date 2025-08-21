(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type sandbox_config = {
  init_from_snapshot : string option;
  network : Configuration.supported_network option;
  funded_addresses : Ethereum_types.address list;
  parent_chain : Uri.t option;
  disable_da_fees : bool;
  kernel_verbosity : Events.kernel_log_level option;
  tezlink : int option;
}

let install_finalizer_seq ~(tx_container : _ Services_backend_sig.tx_container)
    server_public_finalizer server_private_finalizer finalizer_rpc_process =
  let open Lwt_syntax in
  let (module Tx_container) =
    Services_backend_sig.tx_container_module tx_container
  in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_public_finalizer () in
  let* () = server_private_finalizer () in
  let* () = Option.iter_s (fun f -> f ()) finalizer_rpc_process in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_container.shutdown () in
  let* () = Evm_events_follower.shutdown () in
  let* () = Blueprints_publisher.shutdown () in
  let* () = Signals_publisher.shutdown () in
  return_unit

let validate_and_add_etherlink_tx backend
    ~(tx_container :
       L2_types.evm_chain_family Services_backend_sig.tx_container) raw_tx =
  let open Lwt_result_syntax in
  let (Evm_tx_container (module Tx_container)) = tx_container in
  let* res = Validate.is_tx_valid backend ~mode:Minimal raw_tx in
  match res with
  | Ok (next_nonce, txn_obj) ->
      let raw_tx = Ethereum_types.hex_of_utf8 raw_tx in
      let* _ = Tx_container.add ~next_nonce txn_obj ~raw_tx in
      return_unit
  | Error reason ->
      let hash = Ethereum_types.hash_raw_tx raw_tx in
      let*! () = Events.replicate_transaction_dropped hash reason in
      return_unit

(* TODO: https://gitlab.com/tezos/tezos/-/issues/8007
   Validate Tezlink operations before adding them to the queue. *)
let validate_and_add_tezlink_operation
    ~(tx_container :
       L2_types.michelson_chain_family Services_backend_sig.tx_container) raw_tx
    =
  let open Lwt_result_syntax in
  let (Michelson_tx_container (module Tx_container)) = tx_container in
  let*? (op : Tezos_types.Operation.t) =
    raw_tx |> Bytes.of_string |> Tezos_types.Operation.decode
  in
  let raw_tx = Ethereum_types.hex_of_utf8 raw_tx in
  let* _ =
    Tx_container.add ~next_nonce:(Ethereum_types.Qty op.counter) op ~raw_tx
  in
  return_unit

let validate_and_add_tx (type f) backend
    ~(tx_container : f Services_backend_sig.tx_container) :
    string -> unit tzresult Lwt.t =
  match tx_container with
  | Evm_tx_container _ as tx_container ->
      validate_and_add_etherlink_tx backend ~tx_container
  | Michelson_tx_container _ as tx_container ->
      validate_and_add_tezlink_operation ~tx_container

let loop_sequencer (type f) multichain backend
    ~(tx_container : f Services_backend_sig.tx_container) ?sandbox_config
    time_between_blocks =
  let open Lwt_result_syntax in
  match sandbox_config with
  | Some {parent_chain = Some evm_node_endpoint; _} ->
      let*! head = Evm_context.head_info () in
      Blueprints_follower.start
        ~multichain
        ~ping_tx_pool:false
        ~time_between_blocks
        ~evm_node_endpoint
        ~next_blueprint_number:head.next_blueprint_number
        ~on_new_blueprint:(fun (Qty number) blueprint ->
          let*! {next_blueprint_number = Qty expected_number; _} =
            Evm_context.head_info ()
          in
          if Compare.Z.(number = expected_number) then
            let events =
              Evm_events.of_parts
                ~delayed_transactions:blueprint.delayed_transactions
                ~kernel_upgrade:blueprint.kernel_upgrade
                ~sequencer_upgrade:blueprint.sequencer_upgrade
            in
            let* () = Evm_context.apply_evm_events events in
            let*? all_txns =
              Blueprint_decoder.transactions blueprint.blueprint.payload
            in
            let txns = List.filter_map snd all_txns in
            let* () =
              List.iter_es (validate_and_add_tx backend ~tx_container) txns
            in
            let* _ =
              Block_producer.produce_block
                ~force:true
                ~timestamp:blueprint.blueprint.timestamp
            in
            return `Continue
          else return (`Restart_from (Ethereum_types.Qty expected_number)))
        ~on_finalized_levels:(fun
            ~l1_level:_ ~start_l2_level:_ ~end_l2_level:_ -> return_unit)
        ()
  | _ -> (
      match time_between_blocks with
      | Configuration.Nothing ->
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
          loop Misc.(now ()))

let activate_tezlink chain_id =
  let open Lwt_result_syntax in
  let* () =
    Evm_context.patch_state
      ~key:"/evm/feature_flags/enable_multichain"
      ~value:""
      ()
  in
  let* () =
    Evm_context.patch_state
      ~key:"/evm/chain_id"
      ~value:
        Ethereum_types.(
          encode_u256_le (Qty Z.(of_int chain_id)) |> String.of_bytes)
      ()
  in
  let* () =
    Evm_context.patch_state
      ~key:(Format.sprintf "/evm/chain_configurations/%d/chain_family" chain_id)
      ~value:"Michelson"
      ()
  in
  return_unit

let main ~data_dir ~cctxt ?signer ?(genesis_timestamp = Misc.now ())
    ~(configuration : Configuration.t) ?kernel ?sandbox_config () =
  let open Lwt_result_syntax in
  let open Configuration in
  let*! () =
    Octez_telemetry.Opentelemetry_setup.setup
      ~data_dir
      ~service_namespace:"evm_node"
      ~service_name:"sequencer"
      configuration.opentelemetry
  in
  let is_sandbox = Option.is_some sandbox_config in
  let {rollup_node_endpoint; keep_alive; _} = configuration in
  let sequencer_config = configuration.sequencer in
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
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7859
     For now we assume that there is a single L2 chain. We should
     iterate when multichain *)
  let (Ex_chain_family chain_family) =
    Configuration.retrieve_chain_family
      ~l2_chains:configuration.experimental_features.l2_chains
  in
  (* The Tx_pool parameters are ignored by the start function when a
     Tx_queue is configured.

     TODO: simplify start_tx_container when removing the Tx_pool. *)
  let*? start_tx_container, tx_container =
    let open Result_syntax in
    match configuration.experimental_features.enable_tx_queue with
    | Some tx_queue_config ->
        let start, tx_container = Tx_queue.tx_container ~chain_family in
        return
          ( (fun ~tx_pool_parameters:_ ->
              start
                ~config:tx_queue_config
                ~keep_alive:configuration.keep_alive
                ()),
            tx_container )
    | None ->
        let* tx_container = Tx_pool.tx_container ~chain_family in
        return (Tx_pool.start, tx_container)
  in
  let (module Tx_container) =
    Services_backend_sig.tx_container_module tx_container
  in

  let* signer =
    match signer with
    | Some signer -> return signer
    | None ->
        let*? key = Configuration.sequencer_key configuration in
        let* signer = Signer.of_sequencer_key configuration cctxt key in
        return signer
  in

  let* status, smart_rollup_address_typed =
    Evm_context.start
      ~configuration
      ?kernel_path:kernel
      ~data_dir
      ?smart_rollup_address:rollup_node_smart_rollup_address
      ~store_perm:Read_write
      ~signer
      ?snapshot_url
      ~tx_container
      ()
  in
  let smart_rollup_address_b58 = Address.to_string smart_rollup_address_typed in
  let* () =
    match sandbox_config with
    | Some {funded_addresses; disable_da_fees; kernel_verbosity; tezlink; _} ->
        let* pk = Signer.public_key signer in
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
        let* () =
          Option.iter_es
            (fun kernel_verbosity ->
              Evm_context.patch_state
                ~key:Durable_storage_path.kernel_verbosity
                ~value:(Events.string_from_kernel_log_level kernel_verbosity)
                ())
            kernel_verbosity
        in
        let* () =
          when_ disable_da_fees @@ fun () ->
          Evm_context.patch_state
            ~key:"/evm/world_state/fees/da_fee_per_byte"
            ~value:
              Ethereum_types.(encode_u256_le (Qty Z.zero) |> String.of_bytes)
            ()
        in
        let* () =
          Option.iter_es (fun chain_id -> activate_tezlink chain_id) tezlink
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
          ~signer
          ~smart_rollup_address:smart_rollup_address_b58
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
      ~tx_container
      ()
  in
  let* () =
    if status = Created then
      (* Create the first empty block. *)
      let* genesis_chunks =
        Sequencer_blueprint.prepare
          ~signer
          ~timestamp:genesis_timestamp
          ~transactions:[]
          ~delayed_transactions:[]
          ~number:Ethereum_types.(Qty Z.zero)
          ~parent_hash:(L2_types.genesis_parent_hash ~chain_family)
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
  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ro_ctxt in
  let* l2_chain_id, _chain_family =
    let (module Backend) = backend in
    Backend.single_chain_id_and_family ~config:configuration ~enable_multichain
  in
  let* () =
    start_tx_container
      ~tx_pool_parameters:
        {
          backend;
          smart_rollup_address = smart_rollup_address_b58;
          mode = Sequencer;
          tx_timeout_limit = configuration.tx_pool_timeout_limit;
          tx_pool_addr_limit = Int64.to_int configuration.tx_pool_addr_limit;
          tx_pool_tx_per_addr_limit =
            Int64.to_int configuration.tx_pool_tx_per_addr_limit;
          chain_family = Ex_chain_family chain_family;
        }
  in
  Metrics.init
    ~mode:"sequencer"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address:smart_rollup_address_typed ;
  let* () =
    Block_producer.start
      {
        signer;
        smart_rollup_address = smart_rollup_address_b58;
        maximum_number_of_chunks = sequencer_config.max_number_of_chunks;
        tx_container = Ex_tx_container tx_container;
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
          ~rollup_node_endpoint
          ()
      in
      return_unit
  in
  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~is_sequencer:true
      ~l2_chain_id
      ~evm_services:
        Evm_ro_context.(
          evm_services_methods ro_ctxt sequencer_config.time_between_blocks)
      ~data_dir
      ~rpc_server_family:
        (if enable_multichain then Rpc_types.Multichain_sequencer_rpc_server
         else Rpc_types.Single_chain_node_rpc_server chain_family)
      (* When the tx_queue is enabled the validation is done in the
         block_producer instead of in the RPC. This allows for a more
         accurate validation as it's delayed up to when the block is
         created. *)
      (if Configuration.is_tx_queue_enabled configuration then Minimal else Full)
      configuration
      tx_container
      (backend, smart_rollup_address_typed)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~rpc_server_family:
        (if enable_multichain then Rpc_types.Multichain_sequencer_rpc_server
         else Rpc_types.Single_chain_node_rpc_server chain_family)
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
      ~tx_container
  in
  let* () =
    loop_sequencer
      enable_multichain
      backend
      ~tx_container
      ?sandbox_config
      sequencer_config.time_between_blocks
  in
  return_unit
