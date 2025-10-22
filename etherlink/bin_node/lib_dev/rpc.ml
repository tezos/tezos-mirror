(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let spawn_main ~exposed_port ~protected_endpoint ?private_endpoint ~data_dir ()
    =
  let open Lwt_syntax in
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
  let+ process =
    Process_manager.open_process_out
      (p_name, Array.concat [base_cmd; private_cmd])
  in
  let finalizer () = Lwt.return process#terminate in
  finalizer

let install_finalizer_rpc ~(tx_container : _ Services_backend_sig.tx_container)
    server_public_finalizer telemetry_cleanup =
  let open Lwt_syntax in
  let (module Tx_container) =
    Services_backend_sig.tx_container_module tx_container
  in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = telemetry_cleanup () in
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

module Forward_container
    (Tx : Tx_queue_types.L2_transaction)
    (C : sig
      val public_endpoint : Uri.t

      val private_endpoint : Uri.t

      val keep_alive : bool
    end)
    (Injector : sig
      val get_transaction_count :
        keep_alive:bool ->
        base:Uri.t ->
        Tx.address ->
        Ethereum_types.Block_parameter.extended ->
        (Ethereum_types.quantity, string) result tzresult Lwt.t

      val inject_transaction :
        keep_alive:bool ->
        base:Uri.t ->
        tx_object:Tx.t ->
        raw_tx:string ->
        (Ethereum_types.hash, string) result tzresult Lwt.t

      val get_transaction_by_hash :
        keep_alive:bool ->
        base:Uri.t ->
        Ethereum_types.hash ->
        (Tx.t option, string) result tzresult Lwt.t
    end) :
  Services_backend_sig.Tx_container
    with type address = Tx.address
     and type transaction_object = Tx.t = struct
  type address = Tx.address

  type transaction_object = Tx.t

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
        ~keep_alive:C.keep_alive
        ~base:C.public_endpoint
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

  let add ~next_nonce:_ (tx_object : Tx.t) ~raw_tx =
    let open Lwt_syntax in
    let* () =
      Internal_event.Simple.emit
        forwarding_transaction
        (Tx.hash_of_tx_object tx_object)
    in
    Injector.inject_transaction
      ~keep_alive:C.keep_alive
      ~base:C.private_endpoint
      ~tx_object
      ~raw_tx:(Ethereum_types.hex_to_bytes raw_tx)

  let find hash : transaction_object option tzresult Lwt.t =
    let open Lwt_result_syntax in
    let* res =
      Injector.get_transaction_by_hash
        ~keep_alive:C.keep_alive
        ~base:C.public_endpoint
        hash
    in
    let* tx_object =
      get_or_emit_error ~rpc_name:"get_transaction_by_hash" res
    in
    let tx_object = Option.join tx_object in
    return tx_object

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

  let lock_transactions () = Lwt_result_syntax.return_unit

  let unlock_transactions () = Lwt_result_syntax.return_unit

  let is_locked () = Lwt_result_syntax.return_false

  let confirm_transactions ~clear_pending_queue_after:_ ~confirmed_txs:_ =
    Lwt_result_syntax.return_unit

  let size_info () =
    Lwt_result.return
      Metrics.Tx_pool.{number_of_addresses = 0; number_of_transactions = 0}

  let pop_transactions ~maximum_cumulative_size:_ ~validate_tx:_
      ~initial_validation_state:_ =
    Lwt_result_syntax.return_nil
end

let container_forward_request (type f) ~(chain_family : f L2_types.chain_family)
    ~public_endpoint ~private_endpoint ~keep_alive :
    f Services_backend_sig.tx_container =
  match chain_family with
  | EVM ->
      Services_backend_sig.Evm_tx_container
        (module Forward_container
                  (Tx_queue_types.Eth_transaction_object)
                  (struct
                    let public_endpoint = public_endpoint

                    let private_endpoint = private_endpoint

                    let keep_alive = keep_alive
                  end)
                  (Injector))
  | Michelson ->
      Services_backend_sig.Michelson_tx_container
        (module Forward_container
                  (Tx_queue_types.Tezlink_operation)
                  (struct
                    let public_endpoint = public_endpoint

                    let private_endpoint = private_endpoint

                    let keep_alive = keep_alive
                  end)
                  (struct
                    let get_transaction_count ~keep_alive:_ ~base:_ _ _ =
                      failwith
                        "TODO: implement get_transaction_count in the Tezlink \
                         case (using counter RPC)"

                    let inject_transaction ~keep_alive ~base ~tx_object ~raw_tx
                        =
                      Injector.inject_tezlink_operation
                        ~keep_alive
                        ~base
                        ~op:tx_object
                        ~raw_op:(Bytes.of_string raw_tx)

                    let get_transaction_by_hash ~keep_alive:_ ~base:_ _ =
                      failwith
                        "TODO: implement get_transaction_by_hash in the \
                         Tezlink case"
                  end))

let main ~evm_node_endpoint ?evm_node_private_endpoint
    ~(config : Configuration.t) () =
  let open Lwt_result_syntax in
  let* telemetry_cleanup =
    Octez_telemetry.Opentelemetry_setup.setup
      ~data_dir:config.data_dir
      ~service_namespace:"evm_node"
      ~service_name:"rpc"
      ~version:Tezos_version_value.Bin_version.octez_evm_node_version_string
      config.opentelemetry.config
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let pool =
    (* One domain for the scheduler, one domain for spawning processes, one for
       the Irmin GC, and the rest for the RPCs. *)
    Lwt_domain.setup_pool (max 1 (Misc.domain_count_cap () - 3))
  in
  let* ctxt = Evm_ro_context.load ~pool config in
  let* () = Evm_ro_context.preload_known_kernels ctxt in
  Block_storage_setup.enable ~keep_alive:config.keep_alive ctxt.store ;

  let (module Rpc_backend) =
    Evm_ro_context.ro_backend ctxt config ~evm_node_endpoint
  in

  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ctxt in
  let* l2_chain_id, Ex_chain_family chain_family =
    Rpc_backend.single_chain_id_and_family ~config ~enable_multichain
  in

  let* tx_container =
    match evm_node_private_endpoint with
    | Some private_endpoint ->
        return
          (container_forward_request
             ~chain_family
             ~keep_alive:config.keep_alive
             ~public_endpoint:evm_node_endpoint
             ~private_endpoint)
    | None ->
        let start, tx_container = Tx_queue.tx_container ~chain_family in
        let* () =
          start ~config:config.tx_queue ~keep_alive:config.keep_alive ()
        in
        return tx_container
  in

  let* () = set_metrics_level ctxt in
  let* () = set_metrics_confirmed_levels ctxt in

  let (module Tx_container) =
    Services_backend_sig.tx_container_module tx_container
  in

  Metrics.init
    ~mode:"rpc"
    ~tx_pool_size_info:Tx_container.size_info
    ~smart_rollup_address:ctxt.smart_rollup_address ;

  (* Never spawn from an RPC node *)
  let rpc_config =
    {
      config with
      experimental_features =
        {config.experimental_features with spawn_rpc = None};
    }
  in

  let* () = Prevalidator.start ~chain_family Minimal (module Rpc_backend) in
  let rpc_server_family = Rpc_types.Single_chain_node_rpc_server chain_family in
  let* server_public_finalizer =
    Rpc_server.start_public_server
      ~mode:(Rpc {evm_node_endpoint})
      ~l2_chain_id
      ~evm_services:
        Evm_ro_context.(evm_services_methods ctxt time_between_blocks)
      ~rpc_server_family
      rpc_config
      tx_container
      ((module Rpc_backend), ctxt.smart_rollup_address)
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_rpc
      server_public_finalizer
      telemetry_cleanup
      ~tx_container
  in

  let* () =
    when_ (Option.is_some config.experimental_features.spawn_rpc) @@ fun () ->
    Lwt_result.ok (Events.spawn_rpc_is_ready ())
  in

  let* next_blueprint_number = Evm_ro_context.next_blueprint_number ctxt in
  let* () =
    let (module Tx_container) =
      Services_backend_sig.tx_container_module tx_container
    in
    Tx_container.tx_queue_beacon
      ~evm_node_endpoint:(Rpc evm_node_endpoint)
      ~tick_interval:0.05
  and* () =
    Blueprints_follower.start
      ~multichain:enable_multichain
      ~time_between_blocks
      ~evm_node_endpoint
      ~next_blueprint_number
      ~on_new_blueprint:(fun (Qty number) blueprint ->
        let (Qty level) = blueprint.blueprint.number in
        if Z.Compare.(number = level) then (
          let* () =
            when_ (Option.is_some blueprint.kernel_upgrade) @@ fun () ->
            Evm_ro_context.preload_kernel_from_level ctxt (Qty number)
          in
          let* () = Prevalidator.refresh_state () in
          Broadcast.notify_blueprint blueprint ;
          Metrics.set_level ~level:number ;
          let* () = set_metrics_confirmed_levels ctxt in
          return `Continue)
        else
          let*! () =
            Blueprint_events.unexpected_blueprint_from_remote_node
              ~received:blueprint.blueprint.number
              ~expected:next_blueprint_number
          in
          return (`Restart_from (Ethereum_types.Qty number)))
      ~on_finalized_levels:(fun ~l1_level ~start_l2_level ~end_l2_level ->
        Broadcast.notify_finalized_levels
          ~l1_level
          ~start_l2_level
          ~end_l2_level ;
        return_unit)
      ()
  in
  return_unit
