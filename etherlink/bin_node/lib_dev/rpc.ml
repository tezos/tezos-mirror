(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let install_finalizer_rpc server_public_finalizer =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = server_public_finalizer () in
  Misc.unwrap_error_monad @@ fun () -> Tx_pool.shutdown ()

let main ~data_dir ~evm_node_endpoint ~(config : Configuration.t) =
  let open Lwt_result_syntax in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let* ctxt =
    Evm_ro_context.load
      ~data_dir
      ~preimages:config.kernel_execution.preimages
      ?preimages_endpoint:config.kernel_execution.preimages_endpoint
      ()
  in
  let* () = Evm_ro_context.preload_known_kernels ctxt in
  let rpc_backend = Evm_ro_context.ro_backend ctxt config evm_node_endpoint in

  let* () =
    Tx_pool.start
      {
        rollup_node = rpc_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            ctxt.smart_rollup_address;
        mode = Relay;
        tx_timeout_limit = config.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int config.tx_pool_tx_per_addr_limit;
        max_number_of_chunks = None;
      }
  in
  Metrics.init ~mode:"rpc" ~tx_pool_size_info:Tx_pool.size_info ;

  let* server_public_finalizer =
    Rpc_server.start_public_server
      ~evm_services:
        Evm_ro_context.(evm_services_methods ctxt time_between_blocks)
      config
      (rpc_backend, ctxt.smart_rollup_address)
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_rpc server_public_finalizer
  in

  let* next_blueprint_number = Evm_ro_context.next_blueprint_number ctxt in

  Blueprints_follower.start
    ~time_between_blocks
    ~evm_node_endpoint
    ~next_blueprint_number
  @@ fun number blueprint ->
  let* () =
    when_ (Option.is_some blueprint.kernel_upgrade) @@ fun () ->
    Evm_ro_context.preload_kernel_from_level ctxt number
  in
  Blueprints_watcher.notify blueprint ;
  return_unit
