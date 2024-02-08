(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module MakeBackend (Ctxt : sig
  val ctxt : Evm_context.t

  val secret_key : Signature.secret_key
end) : Services_backend_sig.Backend = struct
  module READER = struct
    let read path =
      let open Lwt_result_syntax in
      let* ctxt = Evm_context.sync Ctxt.ctxt in
      let*! evm_state = Evm_context.evm_state ctxt in
      let*! res = Evm_state.inspect evm_state path in
      return res
  end

  module TxEncoder = struct
    type transactions = {
      raw : string list;
      delayed : Ethereum_types.Delayed_transaction.t list;
    }

    type messages = transactions

    let encode_transactions ~smart_rollup_address:_
        ~(transactions : transactions) =
      let open Result_syntax in
      let delayed_hashes =
        List.map Ethereum_types.Delayed_transaction.hash transactions.delayed
      in
      let hashes =
        List.map
          (fun transaction ->
            let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
            Ethereum_types.(
              Hash Hex.(of_string tx_hash_str |> show |> hex_of_string)))
          transactions.raw
      in
      return (delayed_hashes @ hashes, transactions)
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let publish_messages ~timestamp ~smart_rollup_address ~messages =
      let open Lwt_result_syntax in
      let* ctxt = Evm_context.sync Ctxt.ctxt in
      (* Create the blueprint with the messages. *)
      let blueprint =
        Sequencer_blueprint.create
          ~secret_key:Ctxt.secret_key
          ~timestamp
          ~smart_rollup_address
          ~transactions:messages.TxEncoder.raw
          ~delayed_transactions:messages.TxEncoder.delayed
          ~number:ctxt.next_blueprint_number
      in
      (* Apply the blueprint *)
      let* _ctxt = Evm_context.apply_and_publish_blueprint ctxt blueprint in
      return_unit
  end

  module SimulatorBackend = struct
    let simulate_and_read ~input =
      let open Lwt_result_syntax in
      let* ctxt = Evm_context.sync Ctxt.ctxt in
      let* raw_insights = Evm_context.execute_and_inspect ctxt ~input in
      match Simulation.Encodings.insights_from_list raw_insights with
      | Some i -> return i
      | None -> Error_monad.failwith "Invalid insights format"
  end

  let inject_kernel_upgrade ~payload =
    let open Lwt_result_syntax in
    let* ctxt = Evm_context.sync Ctxt.ctxt in
    let*! evm_state = Evm_context.evm_state ctxt in
    let*! evm_state =
      Evm_state.modify
        ~key:Durable_storage_path.kernel_upgrade
        ~value:payload
        evm_state
    in
    let* _ctxt = Evm_context.commit ctxt evm_state in
    return_unit
end

module Make (Ctxt : sig
  val ctxt : Evm_context.t

  val secret_key : Signature.secret_key
end) =
  Services_backend_sig.Make (MakeBackend (Ctxt))

let install_finalizer_seq server private_server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = Events.shutdown_rpc_server ~private_:false in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown private_server in
  let* () = Events.shutdown_rpc_server ~private_:true in
  let* () = Tx_pool.shutdown () in
  let* () = Tx_pool_events.shutdown () in
  let* () = Blueprints_publisher.shutdown () in
  let* () = Blueprint_events.publisher_shutdown () in
  let* () = Delayed_inbox.shutdown () in
  Delayed_inbox_events.shutdown ()

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
        mode = {private_rpc_port; _};
        max_active_connections;
        _;
      } ~directory ~private_directory =
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
  let private_node = `TCP (`Port private_rpc_port) in
  let private_server =
    RPC_server.init_server
      ~acl
      ~cors
      ~media_types:Media_type.all_media_types
      private_directory
  in
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
        RPC_server.launch
          ~max_active_connections
          ~host:Ipaddr.V4.(to_string localhost)
          private_server
          ~callback:(callback_log private_server)
          private_node
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
    let now = Helpers.now () in
    (* We force if the last produced block is older than [time_between_blocks]. *)
    let force =
      match time_between_blocks with
      | Nothing -> false
      | Time_between_blocks time_between_blocks ->
          let diff = Time.Protocol.(diff now last_produced_block) in
          diff >= Int64.of_float time_between_blocks
    in
    let* nb_transactions = Tx_pool.produce_block ~force ~timestamp:now in
    let*! () = Lwt_unix.sleep 0.5 in
    if nb_transactions > 0 || force then loop now else loop last_produced_block
  in
  let now = Helpers.now () in
  loop now

let main ~data_dir ~rollup_node_endpoint ?genesis_timestamp ~sequencer
    ~(configuration : Configuration.sequencer Configuration.t) ?kernel () =
  let open Lwt_result_syntax in
  let open Configuration in
  let* smart_rollup_address =
    Rollup_node_services.smart_rollup_address rollup_node_endpoint
  in
  let* () = Blueprints_publisher.start {rollup_node_endpoint} in
  let* ctxt =
    Evm_context.init
      ?genesis_timestamp
      ~produce_genesis_with:sequencer
      ~data_dir
      ?kernel_path:kernel
      ~preimages:configuration.mode.preimages
      ~smart_rollup_address
      ()
  in
  let module Sequencer = Make (struct
    let ctxt = ctxt

    let secret_key = sequencer
  end) in
  let* () =
    Tx_pool.start
      {rollup_node = (module Sequencer); smart_rollup_address; mode = Sequencer}
  in
  let* () =
    Delayed_inbox.start {rollup_node_endpoint; delayed_inbox_interval = 1}
  in
  let directory =
    Services.directory configuration ((module Sequencer), smart_rollup_address)
  in
  let directory = directory |> Evm_services.register ctxt in
  let private_directory =
    Services.private_directory
      configuration
      ((module Sequencer), smart_rollup_address)
  in
  let* server, private_server =
    start_server configuration ~directory ~private_directory
  in
  let (_ : Lwt_exit.clean_up_callback_id) =
    install_finalizer_seq server private_server
  in
  let* () = loop_sequencer configuration in
  return_unit
