(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module MakeBackend (Ctxt : sig
  val evm_node_endpoint : Uri.t

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

    type messages = string list

    let encode_transactions ~smart_rollup_address:_
        ~(transactions : transactions) =
      let open Result_syntax in
      let hashes =
        List.map
          (fun transaction ->
            let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
            Ethereum_types.(
              Hash Hex.(of_string tx_hash_str |> show |> hex_of_string)))
          transactions.raw
      in
      return (hashes, transactions.raw)
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
          ~base:Ctxt.evm_node_endpoint
          (Services.dispatch_service ~path:Resto.Path.root)
          ()
          ()
          (Batch methods)
      in

      let* () = check_batched_response response in

      return_unit
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

let on_new_blueprint next_blueprint_number (blueprint : Blueprint_types.t) =
  let (Qty level) = blueprint.number in
  let (Qty number) = next_blueprint_number in
  if Z.(equal level number) then
    Evm_context.apply_blueprint blueprint.timestamp blueprint.payload
  else failwith "Received a blueprint with an unexpected number."

module Make (Ctxt : sig
  val evm_node_endpoint : Uri.t

  val smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t
end) : Services_backend_sig.S =
  Services_backend_sig.Make (MakeBackend (Ctxt))

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

let observer_start
    ({rpc_addr; rpc_port; cors_origins; cors_headers; max_active_connections; _} :
      Configuration.observer Configuration.t) ~directory =
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
  let*! () =
    RPC_server.launch
      ~max_active_connections
      ~host
      server
      ~callback:(callback_log server)
      node
  in
  let*! () = Events.is_ready ~rpc_addr ~rpc_port in
  return server

let install_finalizer_observer server =
  let open Lwt_syntax in
  Lwt_exit.register_clean_up_callback ~loc:__LOC__ @@ fun exit_status ->
  let* () = Events.shutdown_node ~exit_status in
  let* () = Tezos_rpc_http_server.RPC_server.shutdown server in
  let* () = Events.shutdown_rpc_server ~private_:false in
  let* () = Tx_pool.shutdown () in
  let* () = Tx_pool_events.shutdown () in
  Evm_context.shutdown ()

let main_loop ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let rec loop (Qty next_blueprint_number) stream =
    let*! candidate = Lwt_stream.get stream in
    match candidate with
    | Some blueprint ->
        let* () = on_new_blueprint (Qty next_blueprint_number) blueprint in
        let* _ = Tx_pool.pop_and_inject_transactions () in
        loop (Qty (Z.succ next_blueprint_number)) stream
    | None -> return_unit
  in

  let* head = Evm_context.head_info () in

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6876
     Should be resilient to errors from the EVM node endpoint *)
  let*! blueprints_stream =
    Evm_services.monitor_blueprints
      ~evm_node_endpoint
      head.next_blueprint_number
  in

  loop head.next_blueprint_number blueprints_stream

let main ?kernel_path ~evm_node_endpoint ~data_dir
    ~(config : Configuration.observer Configuration.t) () =
  let open Lwt_result_syntax in
  let* smart_rollup_address =
    Evm_services.get_smart_rollup_address ~evm_node_endpoint
  in

  let* _loaded =
    Evm_context.start
      ~data_dir
      ?kernel_path
      ~preimages:config.mode.preimages
      ~preimages_endpoint:config.mode.preimages_endpoint
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ()
  in

  let observer_backend =
    (module Make (struct
      let smart_rollup_address = smart_rollup_address

      let evm_node_endpoint = evm_node_endpoint
    end) : Services_backend_sig.S)
  in

  let* () =
    Tx_pool.start
      {
        rollup_node = observer_backend;
        smart_rollup_address =
          Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
            smart_rollup_address;
        mode = Observer;
      }
  in

  let directory =
    Services.directory config (observer_backend, smart_rollup_address)
  in
  let directory = directory |> Evm_services.register smart_rollup_address in

  let* server = observer_start config ~directory in

  let (_ : Lwt_exit.clean_up_callback_id) = install_finalizer_observer server in
  main_loop ~evm_node_endpoint
