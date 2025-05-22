(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let spawn_main ~exposed_port ~protected_endpoint ?private_endpoint ~data_dir ()
    =
  let p_name = Sys.executable_name in
  let base_cmd =
    [|
      Filename.basename p_name;
      "experimental";
      "run";
      "rpc";
      "--rpc-port";
      string_of_int exposed_port;
      "--evm-node-endpoint";
      Uri.to_string protected_endpoint;
      "--data-dir";
      data_dir;
    |]
  in
  let private_cmd =
    Option.fold
      ~none:[||]
      ~some:(fun uri -> [|"--evm-node-private-endpoint"; Uri.to_string uri|])
      private_endpoint
  in
  let process =
    Lwt_process.open_process_none (p_name, Array.concat [base_cmd; private_cmd])
  in
  let finalizer () = Lwt.return process#terminate in
  finalizer

let install_finalizer_rpc server_public_finalizer
    (module Tx_container : Services_backend_sig.Tx_container) =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_public_finalizer () in
  Misc.unwrap_error_monad @@ fun () -> Tx_container.shutdown ()

let set_metrics_level (ctxt : Evm_ro_context.t) =
  let open Lwt_result_syntax in
  let+ candidate = Evm_store.(use ctxt.store Context_hashes.find_latest) in
  match candidate with
  | Some (Qty latest, _) -> Metrics.set_level ~level:latest
  | None -> ()

let set_metrics_confirmed_levels (ctxt : Evm_ro_context.t) =
  let open Lwt_result_syntax in
  let+ candidate = Evm_store.(use ctxt.store L1_l2_finalized_levels.last) in
  match candidate with
  | Some (l1_level, {end_l2_level = Qty finalized; _}) ->
      Metrics.set_confirmed_level ~level:finalized ;
      Metrics.set_l1_level ~level:l1_level
  | None -> ()

let container_forward_request ~public_endpoint ~private_endpoint ~keep_alive :
    (module Services_backend_sig.Tx_container) =
  (module struct
    let rpc_error =
      Internal_event.Simple.declare_2
        ~section:Events.section
        ~name:"local_node_rpc_failure"
        ~msg:"local node failed answering {rpc} with {message}"
        ~level:Error
        ("rpc", Data_encoding.string)
        ("message", Data_encoding.string)

    let forwarding_transaction =
      Internal_event.Simple.declare_1
        ~section:Events.section
        ~name:"forward_transaction"
        ~msg:"forwarding transaction {tx_hash} to local node"
        ~level:Info
        ~pp1:(fun fmt Ethereum_types.(Hash (Hex h)) ->
          Format.fprintf fmt "%10s" h)
        ("tx_hash", Ethereum_types.hash_encoding)

    let get_or_emit_error ~rpc_name res =
      let open Lwt_result_syntax in
      match res with
      | Ok res -> return_some res
      | Error msg ->
          let*! () = Internal_event.Simple.emit rpc_error (rpc_name, msg) in
          return_none

    let nonce ~next_nonce address =
      let open Lwt_result_syntax in
      let* res =
        Injector.get_transaction_count
          ~keep_alive
          ~base:public_endpoint
          address
          (* The function [nonce] is only ever called when
             requesting the nonce for the pending block. It's
             safe to assume the pending block. *)
          Ethereum_types.Block_parameter.(Block_parameter Pending)
      in
      let* nonce = get_or_emit_error ~rpc_name:"get_transaction_count" res in
      match nonce with
      | Some nonce -> return nonce
      | None ->
          (*we return the known next_nonce instead of failing *)
          return next_nonce

    let add ~next_nonce:_ (tx_object : Ethereum_types.legacy_transaction_object)
        ~raw_tx =
      let open Lwt_syntax in
      let* () =
        Internal_event.Simple.emit forwarding_transaction tx_object.hash
      in
      Injector.inject_transaction
        ~keep_alive
        ~base:private_endpoint
        ~tx_object
        ~raw_tx:(Ethereum_types.hex_to_bytes raw_tx)

    let find hash =
      let open Lwt_result_syntax in
      let* res =
        Injector.get_transaction_by_hash ~keep_alive ~base:public_endpoint hash
      in
      let* tx_object =
        get_or_emit_error ~rpc_name:"get_transaction_by_hash" res
      in
      let tx_object = Option.join tx_object in
      return tx_object

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

    let confirm_transactions ~clear_pending_queue_after:_ ~confirmed_txs:_ =
      Lwt_result_syntax.return_unit

    let pop_transactions ~maximum_cumulative_size:_ ~validate_tx:_
        ~initial_validation_state:_ =
      Lwt_result_syntax.return_nil
  end)

let main ~data_dir ~evm_node_endpoint ?evm_node_private_endpoint
    ~(config : Configuration.t) () =
  let open Lwt_result_syntax in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let* ctxt = Evm_ro_context.load ~data_dir config in
  let* () = Evm_ro_context.preload_known_kernels ctxt in

  let* legacy_block_storage =
    Evm_store.(use ctxt.store Block_storage_mode.legacy)
  in
  if not legacy_block_storage then
    Block_storage_setup.enable ~keep_alive:config.keep_alive ctxt.store ;

  let rpc_backend = Evm_ro_context.ro_backend ctxt config ~evm_node_endpoint in

  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ctxt in
  let* l2_chain_id, chain_family =
    let (module Backend) = rpc_backend in
    Backend.single_chain_id_and_family ~config ~enable_multichain
  in

  let* ping_tx_pool, tx_container =
    match
      (evm_node_private_endpoint, config.experimental_features.enable_tx_queue)
    with
    | Some private_endpoint, _ ->
        let forward_request =
          container_forward_request
            ~keep_alive:config.keep_alive
            ~public_endpoint:evm_node_endpoint
            ~private_endpoint
        in

        return (false, forward_request)
    | None, Some tx_queue_config ->
        let* () =
          Tx_queue.start
            ~config:tx_queue_config
            ~keep_alive:config.keep_alive
            ()
        in
        return
          ( false,
            (module Tx_queue.Tx_container : Services_backend_sig.Tx_container)
          )
    | None, None ->
        let* () =
          Tx_pool.start
            {
              backend = rpc_backend;
              smart_rollup_address =
                Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
                  ctxt.smart_rollup_address;
              mode = Relay;
              tx_timeout_limit = config.tx_pool_timeout_limit;
              tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
              tx_pool_tx_per_addr_limit =
                Int64.to_int config.tx_pool_tx_per_addr_limit;
              chain_family;
            }
        in
        return
          ( true,
            (module Tx_pool.Tx_container : Services_backend_sig.Tx_container) )
  in

  let* () = set_metrics_level ctxt in
  let* () = set_metrics_confirmed_levels ctxt in

  Metrics.init
    ~mode:"rpc"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address:ctxt.smart_rollup_address ;

  (* Never spawn from an RPC node *)
  let rpc_config =
    {
      config with
      experimental_features =
        {config.experimental_features with spawn_rpc = None};
    }
  in

  let* server_public_finalizer =
    Rpc_server.start_public_server
      ~l2_chain_id
      ~delegate_health_check_to:evm_node_endpoint
      ~evm_services:
        Evm_ro_context.(evm_services_methods ctxt time_between_blocks)
      ~data_dir
      ~rpc_server_family:(Rpc_types.Single_chain_node_rpc_server chain_family)
      Stateless
      rpc_config
      tx_container
      (rpc_backend, ctxt.smart_rollup_address)
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_rpc server_public_finalizer tx_container
  in

  let* () =
    when_ (Option.is_some config.experimental_features.spawn_rpc) @@ fun () ->
    Lwt_result.ok (Events.spawn_rpc_is_ready ())
  in

  let* next_blueprint_number = Evm_ro_context.next_blueprint_number ctxt in
  let* () =
    let (module Tx_container) = tx_container in
    Tx_container.tx_queue_beacon
      ~evm_node_endpoint:(Rpc evm_node_endpoint)
      ~tick_interval:0.05
  and* () =
    Blueprints_follower.start
      ~ping_tx_pool
      ~time_between_blocks
      ~evm_node_endpoint
      ~next_blueprint_number
    @@ fun (Qty number) blueprint ->
    let (Qty level) = blueprint.blueprint.number in
    if Z.Compare.(number = level) then (
      let* () =
        when_ (Option.is_some blueprint.kernel_upgrade) @@ fun () ->
        Evm_ro_context.preload_kernel_from_level ctxt (Qty number)
      in
      Broadcast.notify @@ Broadcast.Blueprint blueprint ;
      Metrics.set_level ~level:number ;
      let* () = set_metrics_confirmed_levels ctxt in
      return `Continue)
    else
      let*! () =
        Blueprint_events.unexpected_blueprint_from_remote_node
          ~received:blueprint.blueprint.number
          ~expected:next_blueprint_number
      in
      return (`Restart_from (Ethereum_types.Qty number))
  in
  return_unit
