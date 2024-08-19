(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

module MakeBackend (Ctxt : sig
  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.Backend = struct
  module Reader = struct
    let read ?block path = Evm_context.inspect ?block path

    let subkeys ?block path = Evm_context.inspect_subkeys ?block path
  end

  module TxEncoder = struct
    type transactions = (string * Ethereum_types.transaction_object) list

    type messages = transactions

    let encode_transactions ~smart_rollup_address:_ ~transactions:_ =
      assert false
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages:_ =
      assert false
  end

  module Tracer = Tracer

  module SimulatorBackend = struct
    type simulation_state = Evm_state.t

    let simulation_state
        ?(block = Ethereum_types.Block_parameter.(Block_parameter Latest)) () =
      Evm_context.get_evm_state block

    let simulate_and_read simulation_state ~input =
      let open Lwt_result_syntax in
      let* raw_insights =
        Evm_context.execute_and_inspect simulation_state input
      in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"

    let read simulation_state ~path =
      let open Lwt_result_syntax in
      let*! res = Evm_state.inspect simulation_state path in
      return res
  end

  let block_param_to_block_number = Evm_context.block_param_to_block_number

  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_string Ctxt.smart_rollup_address
end

module Make (Ctxt : sig
  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) =
  Services_backend_sig.Make (MakeBackend (Ctxt))

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
          ~max_blueprints_lag:
            sequencer_config.blueprints_publisher_config.max_blueprints_lag
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
      let* genesis =
        Sequencer_blueprint.create
          ~cctxt
          ~sequencer_key:sequencer_config.sequencer
          ~timestamp:genesis_timestamp
          ~smart_rollup_address
          ~transactions:[]
          ~delayed_transactions:[]
          ~number:Ethereum_types.(Qty Z.zero)
          ~parent_hash:Ethereum_types.genesis_parent_hash
      in
      let* () = Evm_context.apply_blueprint genesis_timestamp genesis [] in
      Blueprints_publisher.publish Z.zero genesis
    else return_unit
  in

  let module Rollup_rpc =
    Make
      (struct
        let smart_rollup_address = smart_rollup_address_typed
      end)
      (Evm_context)
  in
  let* () =
    Tx_pool.start
      {
        rollup_node = (module Rollup_rpc);
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
        {
          next_blueprint_number = Evm_context.next_blueprint_number;
          find_blueprint = Evm_context.blueprint;
          smart_rollup_address = smart_rollup_address_typed;
          time_between_blocks = sequencer_config.time_between_blocks;
        }
      configuration
      ((module Rollup_rpc), smart_rollup_address_typed)
  in
  let* finalizer_private_server =
    Rpc_server.start_private_server
      ~block_production:`Single_node
      configuration
      ((module Rollup_rpc), smart_rollup_address_typed)
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq finalizer_public_server finalizer_private_server
  in
  let* () = loop_sequencer sequencer_config in
  return_unit
