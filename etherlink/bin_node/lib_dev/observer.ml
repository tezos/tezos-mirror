(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module MakeBackend (Ctxt : sig
  val evm_node_endpoint : Uri.t

  val keep_alive : bool

  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.Backend = struct
  module Reader = Evm_context_based_reader
  module SimulatorBackend = Evm_context_based_reader

  module TxEncoder = struct
    type transactions = (string * Ethereum_types.transaction_object) list

    type messages = string list

    let encode_transactions ~smart_rollup_address:_ ~transactions =
      let open Result_syntax in
      List.to_seq transactions
      |> Seq.map (fun (raw_tx, (obj : transaction_object)) ->
             (obj.hash, raw_tx))
      |> Seq.split
      |> (fun (l, r) -> (List.of_seq l, List.of_seq r))
      |> return
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let check_response =
      let open Rpc_encodings.JSONRPC in
      let open Lwt_result_syntax in
      function
      | {value = Ok _; _} -> return_unit
      | {value = Error {message; _}; _} ->
          failwith "Send_raw_transaction failed with message \"%s\"" message

    let check_batched_response =
      let open Services in
      function
      | Batch l -> List.iter_es check_response l
      | Singleton r -> check_response r

    let send_raw_transaction_method txn =
      let open Rpc_encodings in
      let message =
        Hex.of_string txn |> Hex.show |> Ethereum_types.hex_of_string
      in
      JSONRPC.
        {
          method_ = Send_raw_transaction.method_;
          parameters =
            Some
              (Data_encoding.Json.construct
                 Send_raw_transaction.input_encoding
                 message);
          id = None;
        }

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages =
      let open Rollup_services in
      let open Lwt_result_syntax in
      let methods = List.map send_raw_transaction_method messages in

      let* response =
        call_service
          ~keep_alive:Ctxt.keep_alive
          ~base:Ctxt.evm_node_endpoint
          (Services.dispatch_service ~path:Resto.Path.root)
          ()
          ()
          (Batch methods)
      in

      let* () = check_batched_response response in

      return_unit
  end

  let block_param_to_block_number = Evm_context.block_param_to_block_number

  module Tracer = Tracer

  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_string Ctxt.smart_rollup_address
end

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
    let delayed_transactions =
      List.map
        (fun Evm_events.Delayed_transaction.{hash; _} -> hash)
        delayed_transactions
    in
    Evm_context.apply_blueprint
      blueprint.timestamp
      blueprint.payload
      delayed_transactions
  else failwith "Received a blueprint with an unexpected number."

module Make (Ctxt : sig
  val evm_node_endpoint : Uri.t

  val keep_alive : bool

  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.S =
  Services_backend_sig.Make (MakeBackend (Ctxt)) (Evm_context)

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

let main ?kernel_path ~data_dir ~(config : Configuration.t) () =
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
      ()
  in

  let observer_backend =
    (module Make (struct
      let smart_rollup_address = smart_rollup_address

      let keep_alive = config.keep_alive

      let evm_node_endpoint =
        match threshold_encryption_bundler_endpoint with
        | Some endpoint -> endpoint
        | None -> evm_node_endpoint
    end) : Services_backend_sig.S)
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
  Metrics.init ~mode:"observer" ~tx_pool_size_info:Tx_pool.size_info ;

  let* finalizer_public_server =
    Rpc_server.start_public_server
      ~evm_services:
        {
          next_blueprint_number = Evm_context.next_blueprint_number;
          find_blueprint = Evm_context.blueprint;
          smart_rollup_address;
          time_between_blocks;
        }
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

  Blueprints_follower.start
    ~time_between_blocks
    ~evm_node_endpoint
    ~get_next_blueprint_number:Evm_context.next_blueprint_number
    on_new_blueprint
