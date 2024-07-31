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
    type transactions = string list

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

let install_finalizer_seq server private_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = Events.shutdown_rpc_server ~private_:false in
  let* () =
    Option.iter_s
      (fun private_server ->
        let* () = Tezos_rpc_http_server.RPC_server.shutdown private_server in
        Events.shutdown_rpc_server ~private_:true)
      private_server
  in
  Misc.unwrap_error_monad @@ fun () ->
  let open Lwt_result_syntax in
  let* () = Tx_pool.shutdown () in
  let* () = Evm_events_follower.shutdown () in
  let* () = Blueprints_publisher.shutdown () in
  return_unit

let callback server dir =
  let open Cohttp in
  let open Lwt_syntax in
  let callback_log conn req body =
    let path = Request.uri req |> Uri.path in
    if path = "/metrics" then
      let* response = Metrics.Metrics_server.callback conn req body in
      Lwt.return (`Response response)
    else
      let uri = req |> Request.uri |> Uri.to_string in
      let meth = req |> Request.meth |> Code.string_of_method in
      let* body_str = body |> Cohttp_lwt.Body.to_string in
      let* () = Events.callback_log ~uri ~meth ~body:body_str in
      Tezos_rpc_http_server.RPC_server.resto_callback
        server
        conn
        req
        (Cohttp_lwt.Body.of_string body_str)
  in
  let update_metrics uri meth =
    Prometheus.Summary.(time (labels Metrics.Rpc.metrics [uri; meth]) Sys.time)
  in
  Tezos_rpc_http_server.RPC_middleware.rpc_metrics_transform_callback
    ~update_metrics
    dir
    callback_log

let start_server
    Configuration.
      {
        rpc_addr;
        rpc_port;
        cors_origins;
        cors_headers;
        max_active_connections;
        _;
      } ~directory ~private_info =
  let open Lwt_result_syntax in
  let open Tezos_rpc_http_server in
  let p2p_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string p2p_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let cors =
    Resto_cohttp.Cors.
      {allowed_headers = cors_headers; allowed_origins = cors_origins}
  in
  let server =
    RPC_server.init_server
      ~acl
      ~cors
      ~media_types:Supported_media_types.all
      directory
  in
  let private_info =
    Option.map
      (fun (private_directory, private_rpc_port) ->
        let private_server =
          RPC_server.init_server
            ~acl
            ~cors
            ~media_types:Media_type.all_media_types
            private_directory
        in
        (private_server, private_rpc_port))
      private_info
  in
  let private_server = Option.map fst private_info in
  Lwt.catch
    (fun () ->
      let*! () =
        RPC_server.launch
          ~max_active_connections
          ~host
          server
          ~callback:(callback server directory)
          node
      in
      let*! () =
        Option.iter_s
          (fun (private_server, private_rpc_port) ->
            let host = Ipaddr.V4.(to_string localhost) in
            let*! () =
              RPC_server.launch
                ~max_active_connections
                ~host
                private_server
                (`TCP (`Port private_rpc_port))
            in
            Events.private_server_is_ready
              ~rpc_addr:host
              ~rpc_port:private_rpc_port)
          private_info
      in
      let*! () = Events.is_ready ~rpc_addr ~rpc_port in
      return (server, private_server))
    (fun _ -> return (server, private_server))

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
  Metrics.Info.init ~mode:"sequencer" ;
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
      ~preimages:sequencer_config.preimages
      ~preimages_endpoint:sequencer_config.preimages_endpoint
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

  let module Rollup_rpc = Make (struct
    let smart_rollup_address = smart_rollup_address_typed
  end) in
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
  let directory =
    Services.directory configuration ((module Rollup_rpc), smart_rollup_address)
  in
  let directory =
    directory
    |> Evm_services.register
         smart_rollup_address_typed
         sequencer_config.time_between_blocks
  in
  let private_info =
    Option.map
      (fun private_rpc_port ->
        let private_directory =
          Services.private_directory
            ~threshold_encryption:false
            configuration
            ((module Rollup_rpc), smart_rollup_address)
        in
        (private_directory, private_rpc_port))
      sequencer_config.private_rpc_port
  in
  let* server, private_server =
    start_server configuration ~directory ~private_info
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq server private_server
  in
  let* () = loop_sequencer sequencer_config in
  return_unit
