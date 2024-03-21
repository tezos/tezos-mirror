(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module MakeBackend (Ctxt : sig
  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.Backend = struct
  module Reader = struct
    let read path = Evm_context.inspect path
  end

  module TxEncoder = struct
    type transactions = {
      raw : string list;
      delayed : Ethereum_types.Delayed_transaction.t list;
    }

    type messages = transactions

    let encode_transactions ~smart_rollup_address:_ ~transactions:_ =
      assert false
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages:_ =
      assert false
  end

  module SimulatorBackend = struct
    let simulate_and_read ~input =
      let open Lwt_result_syntax in
      let* raw_insights = Evm_context.execute_and_inspect input in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"
  end

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
  let* () = Tx_pool.shutdown () in
  let* () = Rollup_node_follower.shutdown () in
  let* () = Evm_events_follower.shutdown () in
  let* () = Blueprints_publisher.shutdown () in
  let* () = Delayed_inbox.shutdown () in
  return_unit

let callback_log server conn req body =
  let open Cohttp in
  let open Lwt_syntax in
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let* body_str = body |> Cohttp_lwt.Body.to_string in
  let* () = Events.callback_log ~uri ~meth ~body:body_str in
  Tezos_rpc_http_server.RPC_server.resto_callback
    server
    conn
    req
    (Cohttp_lwt.Body.of_string body_str)

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
      ~media_types:Media_type.all_media_types
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
          ~callback:(callback_log server)
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
                ~callback:(callback_log private_server)
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

let loop_sequencer :
    Configuration.sequencer Configuration.t -> unit tzresult Lwt.t =
 fun config ->
  let open Lwt_result_syntax in
  let time_between_blocks = config.mode.time_between_blocks in
  let rec loop last_produced_block =
    match time_between_blocks with
    | Nothing ->
        (* Bind on a never-resolved promise ensures this call never returns,
           meaning no block will ever be produced. *)
        let task, _resolver = Lwt.task () in
        let*! () = task in
        return_unit
    | Time_between_blocks time_between_blocks ->
        let now = Helpers.now () in
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
  let now = Helpers.now () in
  loop now

let[@tailrec] rec catchup_evm_event ~rollup_node_endpoint =
  let open Lwt_result_syntax in
  let* rollup_node_l1_level =
    Rollup_services.tezos_level rollup_node_endpoint
  in
  let* latest_known_l1_level = Evm_context.last_known_l1_level () in
  match latest_known_l1_level with
  | None ->
      (* sequencer has no value to start from, it must be the initial
         start or we went from prod to dev. *)
      let*! () = Evm_store_events.no_l1_latest_level_to_catch_up () in
      return_unit
  | Some latest_known_l1_level ->
      let finalized_level = Int32.(sub rollup_node_l1_level 2l) in
      if latest_known_l1_level = finalized_level then return_unit
      else if latest_known_l1_level > finalized_level then
        tzfail
          (error_of_fmt
             "Internal error: The sequencer has processed more l1 level than \
              the rollup node, it should be impossible. ")
      else
        let*! () =
          Events.catching_up_evm_event
            ~from:latest_known_l1_level
            ~to_:finalized_level
        in
        catch_evm_event_aux
          ~rollup_node_endpoint
          ~from:latest_known_l1_level
          ~to_:finalized_level

and[@tailrec] catch_evm_event_aux ~rollup_node_endpoint ~from ~to_ =
  let open Lwt_result_syntax in
  if from = to_ then catchup_evm_event ~rollup_node_endpoint
  else if from > to_ then
    tzfail
      (error_of_fmt
         "Internal error: The catchup of evm_event went too far, it should be \
          impossible.")
  else
    let next_l1_level = Int32.succ from in
    let* () = Evm_events_follower.new_rollup_block next_l1_level in
    let* () = Evm_context.new_last_known_l1_level next_l1_level in
    catch_evm_event_aux ~rollup_node_endpoint ~from:next_l1_level ~to_

let main ~data_dir ~rollup_node_endpoint ~max_blueprints_lag
    ~max_blueprints_ahead ~max_blueprints_catchup ~catchup_cooldown
    ?(genesis_timestamp = Helpers.now ()) ~cctxt ~sequencer
    ~(configuration : Configuration.sequencer Configuration.t) ?kernel () =
  let open Lwt_result_syntax in
  let open Configuration in
  let* smart_rollup_address =
    Rollup_services.smart_rollup_address rollup_node_endpoint
  in
  let* status =
    Evm_context.start
      ?kernel_path:kernel
      ~data_dir
      ~preimages:configuration.mode.preimages
      ~preimages_endpoint:configuration.mode.preimages_endpoint
      ~smart_rollup_address
      ()
  in
  let* head = Evm_context.head_info () in
  let (Qty next_blueprint_number) = head.next_blueprint_number in
  let* () =
    Blueprints_publisher.start
      ~rollup_node_endpoint
      ~max_blueprints_lag
      ~max_blueprints_ahead
      ~max_blueprints_catchup
      ~catchup_cooldown
      ~latest_level_seen:(Z.pred next_blueprint_number)
      ()
  in
  let* () =
    if status = Created then
      (* Create the first empty block. *)
      let* genesis =
        Sequencer_blueprint.create
          ~cctxt
          ~sequencer_key:sequencer
          ~timestamp:genesis_timestamp
          ~smart_rollup_address
          ~transactions:[]
          ~delayed_transactions:[]
          ~number:Ethereum_types.(Qty Z.zero)
          ~parent_hash:Ethereum_types.genesis_parent_hash
      in
      let* () =
        Evm_context.apply_sequencer_blueprint genesis_timestamp genesis
      in
      Blueprints_publisher.publish Z.zero genesis.to_publish
    else return_unit
  in

  let smart_rollup_address_typed =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in

  let module Sequencer = Make (struct
    let smart_rollup_address = smart_rollup_address_typed
  end) in
  let* () =
    Tx_pool.start
      {rollup_node = (module Sequencer); smart_rollup_address; mode = Sequencer}
  in
  let* () =
    Block_producer.start
      {cctxt; smart_rollup_address; sequencer_key = sequencer}
  in
  let* () =
    Delayed_inbox.start {rollup_node_endpoint; delayed_inbox_interval = 1}
  in
  let* () = Evm_events_follower.start {rollup_node_endpoint} in
  let* () = catchup_evm_event ~rollup_node_endpoint in
  let* () = Rollup_node_follower.start {rollup_node_endpoint} in
  let directory =
    Services.directory configuration ((module Sequencer), smart_rollup_address)
  in
  let directory =
    directory |> Evm_services.register smart_rollup_address_typed
  in
  let private_info =
    Option.map
      (fun private_rpc_port ->
        let private_directory =
          Services.private_directory
            configuration
            ((module Sequencer), smart_rollup_address)
        in
        (private_directory, private_rpc_port))
      configuration.mode.private_rpc_port
  in
  let* server, private_server =
    start_server configuration ~directory ~private_info
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq server private_server
  in
  let* () = loop_sequencer configuration in
  return_unit
