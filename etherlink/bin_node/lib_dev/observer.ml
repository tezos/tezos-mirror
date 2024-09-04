(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

let on_new_blueprint next_blueprint_number
    ({delayed_transactions; kernel_upgrade; blueprint} :
      Blueprint_types.with_events) =
  let open Lwt_result_syntax in
  let (Qty level) = blueprint.number in
  let (Qty number) = next_blueprint_number in
  if Z.(equal level number) then
    let events =
      List.map
        (fun delayed_transaction ->
          Evm_events.New_delayed_transaction delayed_transaction)
        delayed_transactions
      @
      match kernel_upgrade with
      | Some kernel_upgrade -> [Evm_events.Upgrade_event kernel_upgrade]
      | None -> []
    in
    let* () = Evm_context.apply_evm_events events in
    Evm_context.apply_blueprint
      blueprint.timestamp
      blueprint.payload
      delayed_transactions
  else failwith "Received a blueprint with an unexpected number."

let install_finalizer_observer ~rollup_node_tracking finalizer_public_server
    finalizer_private_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = finalizer_public_server () in
  let* () = finalizer_private_server () in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  let* () = Evm_context.shutdown () in
  when_ rollup_node_tracking @@ fun () -> Evm_events_follower.shutdown ()

let main ?kernel_path ~data_dir ~(config : Configuration.t) ~no_sync () =
  let open Lwt_result_syntax in
  let*? {
          evm_node_endpoint;
          threshold_encryption_bundler_endpoint;
          rollup_node_tracking;
        } =
    Configuration.observer_config_exn config
  in
  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address ~evm_node_endpoint
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let* _loaded =
    Evm_context.start
      ~data_dir
      ?kernel_path
      ~preimages:config.kernel_execution.preimages
      ~preimages_endpoint:config.kernel_execution.preimages_endpoint
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ~fail_on_missing_blueprint:false
      ~store_perm:`Read_write
      ~block_storage_sqlite3:config.experimental_features.block_storage_sqlite3
      ()
  in
  let* ro_ctxt = Evm_ro_context.load ~smart_rollup_address ~data_dir config in

  let evm_node_endpoint =
    match threshold_encryption_bundler_endpoint with
    | Some endpoint -> endpoint
    | None -> evm_node_endpoint
  in
  let observer_backend =
    Evm_ro_context.ro_backend ro_ctxt config ~evm_node_endpoint
  in

  let* () =
    Tx_pool.start
      {
        rollup_node = observer_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            smart_rollup_address;
        mode = Relay;
        tx_timeout_limit = config.tx_pool_timeout_limit;
        tx_pool_addr_limit = Int64.to_int config.tx_pool_addr_limit;
        tx_pool_tx_per_addr_limit =
          Int64.to_int config.tx_pool_tx_per_addr_limit;
        max_number_of_chunks = None;
      }
  in
  Metrics.init
    ~mode:"observer"
    ~tx_pool_size_info:Tx_pool.size_info
    ~smart_rollup_address ;

  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~evm_services:
        Evm_ro_context.(evm_services_methods ro_ctxt time_between_blocks)
      config
      (observer_backend, smart_rollup_address)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      config
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
              | New_delayed_transaction _ | Upgrade_event _ -> false | _ -> true);
          }
      in
      let () =
        Rollup_node_follower.start
          ~keep_alive:config.keep_alive
          ~proxy:false
          ~rollup_node_endpoint:config.rollup_node_endpoint
          ()
      in
      return_unit
    else
      let*! () = Rollup_node_follower_events.disabled () in
      return_unit
  in

  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_observer
      ~rollup_node_tracking
      finalizer_public_server
      finalizer_private_server
  in

  let*! next_blueprint_number = Evm_context.next_blueprint_number () in

  if no_sync then
    let task, _resolver = Lwt.task () in
    let*! () = task in
    return_unit
  else
    Blueprints_follower.start
      ~time_between_blocks
      ~evm_node_endpoint
      ~next_blueprint_number
      on_new_blueprint
