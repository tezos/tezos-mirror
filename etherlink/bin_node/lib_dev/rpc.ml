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

let install_finalizer_rpc server_public_finalizer telemetry_cleanup =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = telemetry_cleanup () in
  let* () = Events.shutdown_node ~exit_status in
  server_public_finalizer ()

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

let main ~evm_node_endpoint ~evm_node_private_endpoint
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
      ~timeout:config.rpc_timeout
      ()
  in
  let pool =
    (* One domain for the scheduler, one domain for spawning processes, one for
       the Irmin GC, and the rest for the RPCs. *)
    Lwt_domain.setup_pool (max 1 (Misc.domain_count_cap () - 3))
  in
  let* ctxt = Evm_ro_context.load ~pool config in
  let* () = Evm_ro_context.preload_known_kernels ctxt in
  Block_storage_setup.enable
    ~keep_alive:config.keep_alive
    ~timeout:config.rpc_timeout
    ctxt.store ;

  let (module Rpc_backend) =
    Evm_ro_context.ro_backend ctxt config ~evm_node_endpoint
  in

  let* enable_multichain = Evm_ro_context.read_enable_multichain_flag ctxt in
  let* l2_chain_id, Ex_chain_family chain_family =
    Rpc_backend.single_chain_id_and_family ~config ~enable_multichain
  in

  let* () = set_metrics_level ctxt in
  let* () = set_metrics_confirmed_levels ctxt in

  Metrics.init ~mode:"rpc" ~smart_rollup_address:ctxt.smart_rollup_address () ;

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
  let ws_client =
    match rpc_config.websockets with
    | None -> None
    | Some ws ->
        let monitoring =
          match ws.monitor_heartbeat with
          | None -> Websocket_client.{ping_interval = 10.; ping_timeout = 10.}
          | Some {ping_interval; ping_timeout} ->
              Websocket_client.{ping_interval; ping_timeout}
        in
        let ws_client =
          Websocket_client.create
            ~monitoring
            ~keep_alive:true
            Media_type.json
            (Uri.with_path
               evm_node_endpoint
               (Uri.path evm_node_endpoint ^ "/ws"))
        in
        Some ws_client
  in
  let* server_public_finalizer =
    Rpc_server.start_public_server
      ~mode:
        (Rpc
           {evm_node_endpoint; evm_node_private_endpoint; websocket = ws_client})
      ~l2_chain_id
      ~evm_services:
        Evm_ro_context.(evm_services_methods ctxt time_between_blocks)
      ~rpc_server_family
      ~tick:(fun () -> Lwt_result_syntax.return_unit)
      rpc_config
      ((module Rpc_backend), ctxt.smart_rollup_address)
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_rpc server_public_finalizer telemetry_cleanup
  in

  let* () =
    when_ (Option.is_some config.experimental_features.spawn_rpc) @@ fun () ->
    Lwt_result.ok (Events.spawn_rpc_is_ready ())
  in

  let* next_blueprint_number = Evm_ro_context.next_blueprint_number ctxt in
  Blueprints_follower.start
    ~multichain:enable_multichain
    ~time_between_blocks
    ~evm_node_endpoint
    ~rpc_timeout:config.rpc_timeout
    ~next_blueprint_number
    ~instant_confirmations:
      config.experimental_features.preconfirmation_stream_enabled
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
        return (`Continue Blueprints_follower.{sbl_callbacks_activated = true}))
      else
        let*! () =
          Blueprint_events.unexpected_blueprint_from_remote_node
            ~received:blueprint.blueprint.number
            ~expected:next_blueprint_number
        in
        return (`Restart_from (Ethereum_types.Qty number)))
    ~on_finalized_levels:(fun ~l1_level ~start_l2_level ~end_l2_level ->
      Broadcast.notify_finalized_levels ~l1_level ~start_l2_level ~end_l2_level ;
      return_unit)
    ~on_next_block_info:(fun timestamp number ->
      Broadcast.notify_next_block_info timestamp number ;
      let*! () = Events.next_block_info timestamp number in
      return_unit)
    ~on_inclusion:(fun tx hash ->
      Broadcast.notify_inclusion tx hash ;
      let*! () = Events.inclusion hash in
      return_unit)
    ~on_dropped:(fun hash reason ->
      Broadcast.notify_dropped ~hash ~reason ;
      return_unit)
    ()
