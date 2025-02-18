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
     blueprint_with_events) =
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
        | Some level -> return (`Check_for_reorg level)
        | None -> return `Continue)
    | Ok () | Error (Node_error.Diverged {must_exit = false; _} :: _) ->
        return `Continue
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
    | Some level -> return (`Check_for_reorg level)
    | None -> return `Continue
  else return `Continue

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

type error += Invalid_snapshot_provider of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_invalid_snapshot_provider"
    ~title:"Invalid snapshot provider"
    ~description:"The snapshot provider is invalid."
    ~pp:(fun ppf name ->
      Format.fprintf
        ppf
        "%s is not a valid snapshot provider name. It is likely because you \
         are using an invalid string interpolation variable or have a trailing \
         percent"
        name)
    Data_encoding.(obj1 (req "snapshot_provider" string))
    (function Invalid_snapshot_provider name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_provider name)

let interpolate_snapshot_provider rollup_address network provider =
  let open Result_syntax in
  let percent = ('%', "%") in
  let rollup_address_short =
    ( 'r',
      Tezos_crypto.Hashed.Smart_rollup_address.to_short_b58check rollup_address
    )
  in
  let rollup_address_long =
    ('R', Tezos_crypto.Hashed.Smart_rollup_address.to_b58check rollup_address)
  in
  let network =
    ('n', Format.asprintf "%a" Configuration.pp_supported_network network)
  in
  try
    return
      (Misc.interpolate
         provider
         [percent; rollup_address_short; rollup_address_long; network])
  with _ -> tzfail (Invalid_snapshot_provider provider)

let main ?network ?kernel_path ~data_dir ~(config : Configuration.t) ~no_sync
    ~init_from_snapshot () =
  let open Lwt_result_syntax in
  let*? {
          evm_node_endpoint;
          threshold_encryption_bundler_endpoint;
          rollup_node_tracking;
        } =
    Configuration.observer_config_exn config
  in
  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address
      ~keep_alive:config.keep_alive
      ~evm_node_endpoint
  in
  let* time_between_blocks =
    Evm_services.get_time_between_blocks
      ~fallback:(Time_between_blocks 10.)
      ~evm_node_endpoint
      ()
  in
  let*? snapshot_url =
    match network with
    | None -> Result.return_none
    | Some network ->
        Option.map_e
          (interpolate_snapshot_provider smart_rollup_address network)
          init_from_snapshot
  in
  let* _loaded =
    Evm_context.start
      ~configuration:config
      ~data_dir
      ?kernel_path
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ~store_perm:`Read_write
      ?snapshot_url
      ()
  in
  let* ro_ctxt =
    Evm_ro_context.load ?network ~smart_rollup_address ~data_dir config
  in

  let evm_node_endpoint =
    match threshold_encryption_bundler_endpoint with
    | Some endpoint -> endpoint
    | None -> evm_node_endpoint
  in
  let observer_backend =
    Evm_ro_context.ro_backend ro_ctxt config ~evm_node_endpoint
  in

  let mode =
    if config.finalized_view then
      Tx_pool.Forward
        {
          injector =
            (fun _ raw_tx ->
              Injector.send_raw_transaction
                ~keep_alive:config.keep_alive
                ~base:evm_node_endpoint
                ~raw_tx);
        }
    else Tx_pool.Relay
  in

  let* () =
    Tx_pool.start
      {
        backend = observer_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            smart_rollup_address;
        mode;
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
      ~data_dir
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
              | New_delayed_transaction _ | Upgrade_event _
              | Flush_delayed_inbox _ ->
                  false
              | _ -> true);
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
      (on_new_blueprint evm_node_endpoint)
