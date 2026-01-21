(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* To avoid shadowing the module with the [Gas_price] module defining the
   [eth_gasPrice] RPC. *)
module G = Gas_price
module Tezosx_mod = Tezosx
open Tezos_rpc
open Rpc_encodings
open Batch

let version_service =
  Service.get_service
    ~description:"version"
    ~query:Query.empty
    ~output:Data_encoding.string
    Path.(root / "version")

let configuration_service =
  Service.get_service
    ~description:"configuration"
    ~query:Query.empty
    ~output:Data_encoding.Json.encoding
    Path.(root / "configuration")

let mode_service =
  Service.get_service
    ~description:"Expose the operating mode of the EVM node"
    ~query:Query.empty
    ~output:Data_encoding.string
    Path.(root / "mode")

type health_check_query = {drift_threshold : Z.t}

let health_check_service =
  Service.get_service
    ~description:"Assess the health of the RPC server"
    ~query:
      Query.(
        query (fun drift_threshold ->
            {drift_threshold = Z.of_int drift_threshold})
        |+ field "drift_threshold" Arg.int 100 (fun {drift_threshold} ->
               Z.to_int drift_threshold)
        |> seal)
    ~output:Data_encoding.empty
    Path.(root / "health_check")

type error += Node_is_bootstrapping | Node_is_lagging | Stalled_database

let () =
  register_error_kind
    `Temporary
    ~id:"node_is_bootstrapping"
    ~title:"Node is bootstrapping"
    ~description:
      "Node is bootstrapping and result from the RPC servers can be outdated"
    Data_encoding.empty
    (function Node_is_bootstrapping -> Some () | _ -> None)
    (fun () -> Node_is_bootstrapping) ;
  register_error_kind
    `Temporary
    ~id:"node_is_lagging"
    ~title:"Node is lagging"
    ~description:
      "Node is lagging behind its upstream EVM node and result from the RPC \
       servers can be outdated"
    Data_encoding.empty
    (function Node_is_lagging -> Some () | _ -> None)
    (fun () -> Node_is_lagging) ;
  register_error_kind
    `Temporary
    ~id:"stalled_database"
    ~title:"Database of the node is not responsive"
    ~description:
      "The database connections used by the node are stalled, the node needs \
       to be restarted."
    Data_encoding.empty
    (function Stalled_database -> Some () | _ -> None)
    (fun () -> Stalled_database)

let client_version =
  Format.sprintf
    "%s/%s-%s/%s/ocamlc.%s"
    "octez-evm-node"
    (Tezos_version.Version.to_string
       Tezos_version_value.Current_git_info.octez_evm_node_version)
    Tezos_version_value.Current_git_info.abbreviated_commit_hash
    Stdlib.Sys.os_type
    Stdlib.Sys.ocaml_version

let configuration_handler config =
  let open Configuration in
  (* Hide some parts of the configuration. *)
  let hidden = "hidden" in
  let kernel_execution =
    Configuration.{config.kernel_execution with preimages = Some hidden}
  in
  let sequencer = {config.sequencer with sequencer = []} in
  let observer =
    Option.map
      (fun (observer : observer) ->
        {observer with evm_node_endpoint = Uri.of_string hidden})
      config.observer
  in
  let proxy : proxy =
    let evm_node_endpoint =
      Option.map (fun _ -> Uri.of_string hidden) config.proxy.evm_node_endpoint
    in
    {config.proxy with evm_node_endpoint}
  in

  let config =
    {
      config with
      data_dir = hidden;
      rollup_node_endpoint = Uri.of_string hidden;
      kernel_execution;
      sequencer;
      proxy;
      observer;
      private_rpc = None;
    }
  in

  Data_encoding.Json.construct
    ~include_default_fields:`Always
    (Configuration.encoding ())
    config

let health_check_handler config mode db_liveness_check query =
  match mode with
  | Mode.Sequencer _ | Observer _ | Proxy _ ->
      let open Lwt_result_syntax in
      let* () = fail_when (Metrics.is_bootstrapping ()) Node_is_bootstrapping
      and* () =
        fail_when
          Z.Compare.(
            Drift_monitor.last_observed_drift () > query.drift_threshold)
          Node_is_lagging
      and* () =
        let* has_timeout =
          Lwt.pick
            [
              (let*! () = Lwt_unix.sleep 2. in
               return true);
              (let* _ = db_liveness_check () in
               return false);
            ]
        in
        fail_when has_timeout Stalled_database
      in
      return_unit
  | Rpc {evm_node_endpoint; _} ->
      Rollup_services.call_service
        ~keep_alive:false
        ~base:evm_node_endpoint
        ~timeout:config.Configuration.rpc_timeout
        ~media_types:[Media_type.json]
        health_check_service
        ()
        query
        ()

let evm_mode_handler config evm_mode =
  let open Lwt_result_syntax in
  match evm_mode with
  | Mode.Sequencer _ -> return "sequencer"
  | Observer _ -> return "observer"
  | Proxy _ -> return "proxy"
  | Rpc {evm_node_endpoint; _} ->
      let+ evm_node_mode =
        Rollup_services.call_service
          ~keep_alive:false
          ~base:evm_node_endpoint
          ~timeout:config.Configuration.rpc_timeout
          ~media_types:[Media_type.json]
          mode_service
          ()
          ()
          ()
      in
      Format.sprintf "rpc (%s)" evm_node_mode

let version dir =
  Evm_directory.register0 dir version_service (fun () () ->
      Lwt.return_ok client_version)

let configuration config dir =
  Evm_directory.register0 dir configuration_service (fun () () ->
      configuration_handler config |> Lwt.return_ok)

let evm_mode config evm_mode dir =
  Evm_directory.register0 dir mode_service (fun () () ->
      evm_mode_handler config evm_mode)

let health_check config mode db_liveness_check dir =
  Evm_directory.register0 dir health_check_service (fun query () ->
      health_check_handler config mode db_liveness_check query)

let get_block_by_number ~full_transaction_object block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty n) =
    Rollup_node_rpc.block_param_to_block_number
      ~chain_family:L2_types.EVM
      (Block_parameter block_param)
  in
  Rollup_node_rpc.Etherlink_block_storage.nth_block ~full_transaction_object n

let get_block_receipts block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty n) =
    Rollup_node_rpc.block_param_to_block_number
      ~chain_family:L2_types.EVM
      (Block_parameter block_param)
  in
  Rollup_node_rpc.Etherlink_block_storage.block_receipts n

let get_transaction_from_index block index
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  match block.Ethereum_types.transactions with
  | TxHash l -> (
      match List.nth_opt l index with
      | None -> return_none
      | Some hash ->
          Rollup_node_rpc.Etherlink_block_storage.transaction_object hash)
  | TxFull l -> return @@ List.nth_opt l index

let block_transaction_count block =
  Ethereum_types.quantity_of_z @@ Z.of_int
  @@
  match block.Ethereum_types.transactions with
  | TxHash l -> List.length l
  | TxFull l -> List.length l

type sub_stream = {
  kind : Ethereum_types.Subscription.kind;
  stream :
    ( Transaction_object.t,
      Transaction_receipt.t )
    Ethereum_types.Subscription.output
    tzresult
    Lwt_stream.t;
  stopper : unit -> bool tzresult Lwt.t;
}

let subscriptions :
    (Ethereum_types.Subscription.id, sub_stream) Stdlib.Hashtbl.t =
  (* 10 seems like a reasonable number since there is only
     four types of subscription, and only `logs` make sense
     to be sent multiple times. *)
  Stdlib.Hashtbl.create 10

let encode_id bytes =
  let id_hex = Hex.of_bytes bytes |> Hex.show in
  let buf = Buffer.create (String.length id_hex) in
  (* Trimming leading zeros. *)
  String.fold_left
    (fun only_zeros -> function
      | '0' when only_zeros -> only_zeros
      | c ->
          Buffer.add_char buf c ;
          false)
    true
    id_hex
  |> ignore ;
  Buffer.contents buf

let make_id ~id = Ethereum_types.Subscription.(Id (Hex id))

(* [generate_id]'s implementation is inspired by geth's one.
   See:
   https://github.com/ethereum/go-ethereum/blob/master/rpc/subscription.go *)
let generate_id () =
  let id = Bytes.make 16 '\000' in
  Bytes.iteri (fun i _ -> Bytes.set_uint8 id i (Random.int 256)) id ;
  encode_id id

let filter_from ~address ~topics =
  Ethereum_types.Filter.
    {from_block = None; to_block = None; address; topics; block_hash = None}

let filter_logs ~bloom_filter ~receipt =
  Filter_helpers.filter_receipt bloom_filter receipt

let produce_logs_stream ~bloom_filter stream =
  Lwt_stream.map_list
    (fun receipt ->
      filter_logs ~bloom_filter ~receipt
      |> List.map (fun l -> Ethereum_types.Subscription.Logs l))
    stream

let eth_subscribe_direct ~(kind : Ethereum_types.Subscription.kind)
    (module Backend_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* stream, stopper =
    match kind with
    | NewHeads -> return @@ Lwt_watcher.create_stream Evm_context.head_watcher
    | Logs {address; topics} ->
        let filter = filter_from ~address ~topics in
        let* bloom_filter = Filter_helpers.validate_bloom_filter filter in
        let stream, stopper =
          Lwt_watcher.create_stream Evm_context.receipt_watcher
        in
        let stream = produce_logs_stream ~bloom_filter stream in
        return (stream, stopper)
    | NewPendingTransactions -> return @@ Tx_watcher.create_stream ()
    | Syncing ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7642 *)
        Stdlib.failwith "The websocket event [syncing] is not implemented yet."
    | NewIncludedTransactions ->
        let stream, stopper = Broadcast.create_broadcast_stream () in
        let stream =
          Lwt_stream.filter_map
            (function
              | Octez_telemetry.Traceparent.
                  {data = Broadcast.Included_transaction {tx; hash = _}; _} -> (
                  match tx with
                  | Common (Broadcast.Evm raw) -> (
                      let res = Transaction_object.decode raw in
                      match res with
                      | Ok txn ->
                          Some
                            (Ethereum_types.Subscription.NewIncludedTransactions
                               txn)
                      | Error _ -> None)
                  | Common (Broadcast.Michelson _raw) ->
                      (* TODO: L2-586 include tezlink transaction in preconfirmation stream *)
                      None
                  | _ -> None)
              | _ -> None)
            stream
        in
        return (stream, stopper)
    | NewPreconfirmedReceipts ->
        let stream, stopper = Broadcast.create_transaction_result_stream () in
        let stream =
          Lwt_stream.filter_map
            (function
              | Broadcast.{hash = _; result = Ok receipt} ->
                  Some
                    (Ethereum_types.Subscription.NewPreconfirmedReceipts receipt)
              | _ -> None)
            stream
        in
        return (stream, stopper)
    | Etherlink (L1_L2_levels from_l1_level) ->
        let* () =
          unless (Evm_events_follower.available ()) @@ fun () ->
          failwith "The EVM node does not follow a rollup node"
        in
        let* extra =
          match from_l1_level with
          | None -> return_nil
          | Some from_l1_level ->
              let+ l1_l2 = Backend_rpc.list_l1_l2_levels ~from_l1_level in
              List.rev_map
                (fun ( l1_level,
                       {
                         Evm_store.L1_l2_finalized_levels.start_l2_level;
                         end_l2_level;
                       } )
                   ->
                  {
                    Ethereum_types.Subscription.l1_level;
                    start_l2_level;
                    end_l2_level;
                  })
                l1_l2
              |> List.rev
        in
        let stream, stopper =
          Lwt_watcher.create_stream Evm_context.l1_l2_levels_watcher
        in
        let stream = Lwt_stream.append (Lwt_stream.of_list extra) stream in
        let stream =
          Lwt_stream.map
            (fun l -> Ethereum_types.Subscription.Etherlink (L1_l2_levels l))
            stream
        in
        return (stream, stopper)
  in
  return
    ( Lwt_stream.map Result.ok stream,
      fun () ->
        Lwt_watcher.shutdown stopper ;
        return true )

type 'a proxied_subscription = {
  watcher : 'a Lwt_watcher.input;
  unsubscribe : unit -> bool tzresult Lwt.t;
  mutable subscribers : int;
}

module WsClientTable = Hashtbl.Make (struct
  type t = Websocket_client.t * Ethereum_types.Subscription.kind

  let equal (ws1, k1) (ws2, k2) =
    Websocket_client.client_id ws1 = Websocket_client.client_id ws2 && k1 = k2

  let hash (ws, k) = Hashtbl.hash (Websocket_client.client_id ws, k)
end)

let proxied_subscriptions = WsClientTable.create 4

let create_proxied_subscription ws_client ~timeout kind =
  let open Lwt_result_syntax in
  let timeout = Websocket_client.{timeout; on_timeout = `Retry_forever} in
  let watcher = Lwt_watcher.create_input () in
  let upstream_unsubscribe = ref (fun () -> return_false) in
  let unsubscribe () =
    WsClientTable.remove proxied_subscriptions (ws_client, kind) ;
    !upstream_unsubscribe ()
  in
  let proxied = {watcher; unsubscribe; subscribers = 0} in
  WsClientTable.replace proxied_subscriptions (ws_client, kind) proxied ;
  let upstream_subscribe () =
    let rec resubscribe () =
      let open Lwt_syntax in
      if proxied.subscribers <= 0 then
        (* no more subscribers *)
        return_unit
      else
        (* Resubscribing after small delay *)
        let* () = Lwt_unix.sleep 1. in
        let* sub = Websocket_client.subscribe ws_client ~timeout kind in
        upstream_subscribe_loop sub
    and upstream_subscribe_loop =
      let open Lwt_syntax in
      function
      | Error e ->
          (* Notify error and attempt resubscription *)
          Lwt_watcher.notify watcher (Error e) ;
          resubscribe ()
      | Ok Websocket_client.{stream; unsubscribe} ->
          (* Subscription success, register new unsubscribe *)
          (* TODO: maybe add missing elements in the stream while
             disconnected. *)
          upstream_unsubscribe := unsubscribe ;
          Lwt.dont_wait
            (fun () ->
              let* () = Lwt_stream.iter (Lwt_watcher.notify watcher) stream in
              (* Upstream subscription stream stopped, resubscribe if there are
                  any subscribers left. *)
              resubscribe ())
            ignore ;
          return_unit
    in
    let* sub = Websocket_client.subscribe ws_client ~timeout kind in
    let*! () = upstream_subscribe_loop (Ok sub) in
    return_unit
  in
  let* () = upstream_subscribe () in
  return proxied

let get_proxied_subscription ws_client ~timeout
    (kind : Ethereum_types.Subscription.kind) =
  let open Lwt_result_syntax in
  let kind =
    match kind with
    | NewHeads -> (NewHeads : Ethereum_types.Subscription.kind)
    | Logs _ ->
        (* Subscribe to all logs *)
        Logs {address = None; topics = None}
    | NewPendingTransactions -> NewPendingTransactions
    | NewIncludedTransactions -> NewIncludedTransactions
    | NewPreconfirmedReceipts -> NewPreconfirmedReceipts
    | Syncing -> Syncing
    | Etherlink (L1_L2_levels _) ->
        (* Don't fetch historic levels through websocket *)
        Etherlink (L1_L2_levels None)
  in
  match WsClientTable.find proxied_subscriptions (ws_client, kind) with
  | Some proxied -> return proxied
  | None -> create_proxied_subscription ws_client ~timeout kind

(* In RPC mode, the EVM node acts as a proxy to the server for
   `eth_subscribe`. There is at most one subscription active per kind on the
   server. *)
let eth_subscribe_rpc_mode ~timeout ~(kind : Ethereum_types.Subscription.kind)
    (ws_client : Websocket_client.t)
    (module Backend_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* proxied = get_proxied_subscription ws_client ~timeout kind in
  let* stream, stopper =
    match kind with
    | NewHeads | NewPendingTransactions | Syncing | NewIncludedTransactions
    | NewPreconfirmedReceipts ->
        return @@ Lwt_watcher.create_stream proxied.watcher
    | Logs {address; topics} ->
        let stream, stopper = Lwt_watcher.create_stream proxied.watcher in
        let filter = filter_from ~address ~topics in
        let* bloom_filter = Filter_helpers.validate_bloom_filter filter in
        let stream =
          Lwt_stream.filter_map
            (function
              | Ok (Ethereum_types.Subscription.Logs log) ->
                  Filter_helpers.filter_one_log bloom_filter log
                  |> Option.map (fun l ->
                         Ok (Ethereum_types.Subscription.Logs l))
              | Ok _ -> None
              | Error e -> Some (Error e))
            stream
        in
        return (stream, stopper)
    | Etherlink (L1_L2_levels from_l1_level) ->
        let* extra =
          match from_l1_level with
          | None -> return_nil
          | Some from_l1_level ->
              let+ l1_l2 = Backend_rpc.list_l1_l2_levels ~from_l1_level in
              List.rev_map
                (fun ( l1_level,
                       {
                         Evm_store.L1_l2_finalized_levels.start_l2_level;
                         end_l2_level;
                       } )
                   ->
                  Ok
                    (Ethereum_types.Subscription.Etherlink
                       (L1_l2_levels {l1_level; start_l2_level; end_l2_level})))
                l1_l2
              |> List.rev
        in
        let stream, stopper = Lwt_watcher.create_stream proxied.watcher in
        let stream = Lwt_stream.append (Lwt_stream.of_list extra) stream in
        return (stream, stopper)
  in
  let unsubscribed = ref false in
  let unsubscribe () =
    if !unsubscribed then return_false
    else (
      unsubscribed := true ;
      Lwt_watcher.shutdown stopper ;
      proxied.subscribers <- proxied.subscribers - 1 ;
      if proxied.subscribers <= 0 then proxied.unsubscribe () else return_true)
  in
  proxied.subscribers <- proxied.subscribers + 1 ;
  return (stream, unsubscribe)

let eth_subscribe (config : Configuration.t) (mode : 'f Mode.t) ~kind backend =
  let open Lwt_result_syntax in
  let id = make_id ~id:(generate_id ()) in
  let* stream, stopper =
    match mode with
    | Rpc {websocket = Some ws_client; _} ->
        eth_subscribe_rpc_mode
          ~timeout:config.rpc_timeout
          ~kind
          ws_client
          backend
    | Rpc {websocket = None; _} ->
        failwith
          "Cannot call eth_subscribe on RPC node whose endpoint does not \
           support websockets."
    | _ -> eth_subscribe_direct ~kind backend
  in
  let stopper () =
    Stdlib.Hashtbl.remove subscriptions id ;
    stopper ()
  in
  Stdlib.Hashtbl.add subscriptions id {kind; stream; stopper} ;
  return (id, (stream, stopper))

let eth_unsubscribe ~id =
  match Stdlib.Hashtbl.find_opt subscriptions id with
  | Some {stopper; _} -> stopper ()
  | None -> Lwt_result_syntax.return_false

let decode : type a.
    (module METHOD with type input = a) -> Data_encoding.json -> a =
 fun (module M) v ->
  Opentelemetry.Trace.with_
    ~kind:Span_kind_server
    ~service_name:"HTTP_server"
    "Service.decode"
  @@ fun _ -> Data_encoding.Json.destruct M.input_encoding v

let encode : type a.
    (module METHOD with type output = a) -> a -> Data_encoding.json =
 fun (module M) v ->
  Opentelemetry.Trace.with_
    ~kind:Span_kind_server
    ~service_name:"HTTP_server"
    "Service.encode"
  @@ fun _ -> Data_encoding.Json.construct M.output_encoding v

let direct_rpc_value v = JSONRPC.Direct v

let lazy_rpc_value v = JSONRPC.Lazy v

let build : type input output.
    (module METHOD with type input = input and type output = output) ->
    f:(input option -> (output, Rpc_errors.t) Result.t tzresult Lwt.t) ->
    Data_encoding.json option ->
    JSONRPC.return_value Lwt.t =
 fun (module Method) ~f parameters ->
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let decoded = Option.map (decode (module Method)) parameters in
      let+ v = f decoded in
      match v with
      | Error err ->
          direct_rpc_value
            (Error
               (Rpc_errors.internal_error
               @@ Format.asprintf "%a" pp_print_trace err))
      | Ok value -> direct_rpc_value (Result.map (encode (module Method)) value))
    (fun exn ->
      Telemetry.Jsonrpc.return_error @@ Rpc_errors.invalid_request
      @@ Printexc.to_string exn)

let rpc_ok result = Lwt_result.return @@ Ok result

let rpc_error err = Lwt_result.return @@ Error err

let missing_parameter () = rpc_error Rpc_errors.invalid_input

let expect_input input f =
  match input with None -> missing_parameter () | Some v -> f v

let build_with_input method_ ~f parameters =
  build method_ ~f:(fun input -> expect_input input f) parameters

let build_with_lazy_output : type input output.
    (module METHOD with type input = input and type output = output) ->
    f:
      (input option ->
      (output, Rpc_errors.t) Result.t tzresult Lwt.t tzresult Lwt.t) ->
    Data_encoding.json option ->
    JSONRPC.return_value Lwt.t =
 fun (module Method) ~f parameters ->
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let decoded = Option.map (decode (module Method)) parameters in
      let+ v = f decoded in
      match v with
      | Error err ->
          direct_rpc_value
            (Error
               (Rpc_errors.internal_error
               @@ Format.asprintf "%a" pp_print_trace err))
      | Ok promise ->
          let encode = function
            | Ok v -> Result.map (encode (module Method)) v
            | Error err ->
                Error
                  (Rpc_errors.internal_error
                  @@ Format.asprintf "%a" pp_print_trace err)
          in
          lazy_rpc_value (Lwt.map encode promise))
    (fun exn ->
      Telemetry.Jsonrpc.return_error @@ Rpc_errors.invalid_request
      @@ Printexc.to_string exn)

let build_with_input_and_lazy_output method_ ~f parameters =
  let missing_parameter () =
    Lwt_result.return (Lwt.return (Ok (Error Rpc_errors.invalid_input)))
  in
  let expect_input input f =
    match input with None -> missing_parameter () | Some v -> f v
  in
  build_with_lazy_output
    method_
    ~f:(fun input -> expect_input input f)
    parameters

let get_fee_history block_count block_parameter config
    (module Backend_rpc : Services_backend_sig.S) =
  (* TODO: exclude 0 blocks *)
  let open Lwt_result_syntax in
  let open Ethereum_types in
  (* block count is bounded by configuration (default to Configuration.default_fee_history.max_count *)
  let block_count =
    match Configuration.(config.fee_history.max_count) with
    | Unlimited -> block_count
    | Limit block_count_limit -> Z.(min (of_int block_count_limit) block_count)
  in
  let* nb_latest =
    Backend_rpc.Etherlink_block_storage.current_block_number ()
  in
  let is_reachable nb =
    match Configuration.(config.fee_history.max_past) with
    | None -> true
    | Some delta ->
        let oldest_reachable = Z.(sub (Qty.to_z nb_latest) (of_int delta)) in
        Z.(gt (Qty.to_z nb) oldest_reachable)
  in
  let* newest_block =
    get_block_by_number
      ~full_transaction_object:false
      block_parameter
      (module Backend_rpc)
  in
  let* base_fee_per_gas_next_block =
    if newest_block.number = nb_latest then
      Backend_rpc.Etherlink.base_fee_per_gas ()
    else
      let next_block_number = Qty.next newest_block.number in
      let* next_block =
        get_block_by_number
          ~full_transaction_object:false
          (Block_parameter.Number next_block_number)
          (module Backend_rpc)
      in
      return (Option.value next_block.baseFeePerGas ~default:Qty.zero)
  in

  let rec get_fee_history_aux block_count block_parameter
      (history_acc : Fee_history.t) =
    if
      block_count <= Z.zero || block_parameter = Block_parameter.Number Qty.zero
    then return history_acc
    else
      let* block =
        get_block_by_number
          ~full_transaction_object:false
          block_parameter
          (module Backend_rpc)
      in
      let gas_used_ratio =
        Float.div
          (Z.to_float @@ Qty.to_z block.gasUsed)
          (Z.to_float @@ Qty.to_z block.gasLimit)
        :: history_acc.gas_used_ratio
      in
      (* 0 for block pre EIP-1559 *)
      let block_base_fee_per_gas =
        Option.value block.baseFeePerGas ~default:Qty.zero
      in
      let base_fee_per_gas =
        block_base_fee_per_gas :: history_acc.base_fee_per_gas
      in
      let oldest_block = block.number in
      let history_acc =
        Fee_history.{oldest_block; base_fee_per_gas; gas_used_ratio}
      in
      let next_block = Qty.pred block.number in
      if is_reachable next_block then
        get_fee_history_aux
          Z.(block_count - one)
          (Block_parameter.Number next_block)
          history_acc
      else return history_acc
  in
  let init_acc =
    Fee_history.
      {
        (* default value if no block (which is a terrible
           corner case) *)
        oldest_block = Qty.zero;
        base_fee_per_gas = [base_fee_per_gas_next_block];
        gas_used_ratio = [];
      }
  in
  get_fee_history_aux block_count block_parameter init_acc

let process_trace_result trace =
  match trace with
  | Ok trace -> rpc_ok trace
  | Error (Tracer_types.Not_supported :: _) ->
      rpc_error (Rpc_errors.method_not_supported Trace_transaction.method_)
  | Error (Tracer_types.Transaction_not_found hash :: _) ->
      rpc_error (Rpc_errors.trace_transaction_not_found hash)
  | Error (Tracer_types.Block_not_found number :: _) ->
      rpc_error (Rpc_errors.trace_block_not_found number)
  | Error (Tracer_types.Trace_not_found :: _) ->
      rpc_error Rpc_errors.trace_not_found
  | Error (Tracer_types.Tracer_not_implemented s :: _) ->
      rpc_error (Rpc_errors.tracer_not_implemented s)
  | Error e ->
      let msg = Format.asprintf "%a" pp_print_trace e in
      rpc_error (Rpc_errors.internal_error msg)

let inject_rpc_call (config : Configuration.t) request method_
    Mode.{evm_node_endpoint; _} =
  let open Lwt_result_syntax in
  let* res =
    Injector.call_singleton_request
      ~keep_alive:config.keep_alive
      ~timeout:config.rpc_timeout
      ~base:evm_node_endpoint
      method_
      request
  in
  match res with
  | Ok output -> rpc_ok output
  | Error reason -> rpc_error (Rpc_errors.internal_error reason)

let process_based_on_mode (type f) (mode : f Mode.t) ~on_rpc ~on_stateful_evm
    ~on_stateful_michelson =
  match mode with
  | Mode.Rpc rpc -> on_rpc rpc
  | Proxy (Evm_tx_container tx_container)
  | Sequencer (Evm_tx_container tx_container)
  | Observer (Evm_tx_container tx_container) ->
      on_stateful_evm tx_container
  | Proxy (Michelson_tx_container tx_container)
  | Sequencer (Michelson_tx_container tx_container)
  | Observer (Michelson_tx_container tx_container) ->
      on_stateful_michelson tx_container

let wait_confirmation_callback wait_confirmation_wakener =
 fun status ->
  let () =
    let open Result_syntax in
    match status with
    | `Accepted -> ()
    | `Confirmed -> Lwt.wakeup wait_confirmation_wakener (return return_unit)
    | `Dropped ->
        Lwt.wakeup wait_confirmation_wakener
        @@ return
        @@ fail (Format.asprintf "Transaction was dropped before confirmed.")
    | `Refused ->
        Lwt.wakeup wait_confirmation_wakener
        @@ return
        @@ fail (Format.asprintf "Transaction refused")
  in
  Lwt.return_unit

let send_raw_transaction (type f) (config : Configuration.t) (mode : f Mode.t)
    ?wait_confirmation_wakener =
  let open Lwt_result_syntax in
  let on_rpc raw_tx transaction_object Mode.{evm_node_private_endpoint; _} =
    let* res =
      Injector.inject_transaction
        ~wait_confirmation:false
        ~keep_alive:config.keep_alive
        ~timeout:config.rpc_timeout
        ~base:evm_node_private_endpoint
        ~tx_object:transaction_object
        ~raw_tx:(Ethereum_types.hex_to_bytes raw_tx)
    in
    let on_wait_confirmation hash wait_confirmation_wakener =
      let tx_confirmed =
        Injector.inject_wait_transaction_confirmation
          ~keep_alive:config.keep_alive
          ~timeout:config.rpc_timeout
          ~base:evm_node_private_endpoint
          ~hash
      in
      Lwt.dont_wait
        (fun () ->
          let*! tx_confirmed in
          Lwt.wakeup wait_confirmation_wakener tx_confirmed ;
          Lwt.return_unit)
        (fun exn ->
          Lwt.wakeup wait_confirmation_wakener Result_syntax.(fail [Exn exn]))
    in
    match res with
    | Ok hash ->
        Option.iter (on_wait_confirmation hash) wait_confirmation_wakener ;
        rpc_ok hash
    | Error reason -> rpc_error (Rpc_errors.internal_error reason)
  in
  let f raw_tx =
    let txn = Ethereum_types.hex_to_bytes raw_tx in
    let* is_valid = Prevalidator.prevalidate_raw_transaction txn in
    match is_valid with
    | Error err ->
        let*! () = Tx_pool_events.invalid_transaction ~transaction:raw_tx in
        rpc_error (Rpc_errors.transaction_rejected err None)
    | Ok {next_nonce; transaction_object} ->
        Octez_telemetry.Trace.(
          add_attrs (fun () ->
              Telemetry.Attributes.
                [Transaction.hash @@ Transaction_object.hash transaction_object])) ;
        process_based_on_mode
          mode
          ~on_rpc:(on_rpc raw_tx transaction_object)
          ~on_stateful_evm:(fun (module Tx_container) ->
            let* tx_hash =
              match wait_confirmation_wakener with
              | None -> Tx_container.add ~next_nonce transaction_object ~raw_tx
              | Some wait_confirmation_wakener ->
                  let callback =
                    wait_confirmation_callback wait_confirmation_wakener
                  in
                  Tx_container.add
                    ~next_nonce
                    transaction_object
                    ~raw_tx
                    ~callback
            in
            match tx_hash with
            | Ok tx_hash -> rpc_ok tx_hash
            | Error reason ->
                rpc_error (Rpc_errors.transaction_rejected reason None))
          ~on_stateful_michelson:(fun _ ->
            failwith "Unsupported JSONRPC method in Tezlink: sendRawTransaction")
  in
  f

let dispatch_request (type f) ~websocket
    (rpc_server_family : f Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) (config : Configuration.t) (mode : f Mode.t)
    ((module Backend_rpc : Services_backend_sig.S), _)
    ({method_; parameters; id} as request : JSONRPC.request) :
    JSONRPC.return_response Lwt.t =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  Telemetry.Jsonrpc.trace_dispatch_with
    ~websocket
    ~service_name:"public_rpc"
    method_
    id
  @@ fun _scope ->
  let*! return_value =
    match
      map_method_name ~rpc_server_family ~restrict:rpc.restricted_rpcs method_
    with
    | Unknown ->
        Metrics.inc_rpc_method ~name:"unknown" ;
        Telemetry.Jsonrpc.return_error (Rpc_errors.method_not_found method_)
    | Unsupported ->
        Metrics.inc_rpc_method ~name:"unsupported" ;
        Telemetry.Jsonrpc.return_error (Rpc_errors.method_not_supported method_)
    | Disabled ->
        Metrics.inc_rpc_method ~name:"disabled" ;
        Telemetry.Jsonrpc.return_error (Rpc_errors.method_disabled method_)
    (* Ethereum JSON-RPC API methods we support *)
    | Method (method_rpc, module_) -> (
        Metrics.inc_rpc_method ~name:method_ ;
        match method_rpc with
        | Accounts.Method ->
            let f (_ : unit option) = rpc_ok [] in
            build ~f module_ parameters
        | Network_id.Method ->
            let f (_ : unit option) =
              let open Lwt_result_syntax in
              let* (Chain_id chain_id) = Backend_rpc.chain_id () in
              rpc_ok (Z.to_string chain_id)
            in
            build ~f module_ parameters
        | Rpc_encodings.Chain_id.Method ->
            let f (_ : unit option) =
              let* chain_id = Backend_rpc.chain_id () in
              rpc_ok chain_id
            in
            build ~f module_ parameters
        | Rpc_encodings.Chain_family.Method ->
            let f chain_id =
              let* chain_family = Backend_rpc.chain_family chain_id in
              rpc_ok chain_family
            in
            build_with_input ~f module_ parameters
        | Get_balance.Method ->
            let f (address, block_param) =
              let* balance =
                Backend_rpc.Etherlink.balance address block_param
              in
              rpc_ok balance
            in
            build_with_input ~f module_ parameters
        | Tezosx.Get_tezos_ethereum_address.Method ->
            let f tezos_address =
              tezos_address |> Signature.V2.Public_key_hash.to_bytes
              |> Tezosx_mod.Ethereum_runtime.generate_alias |> rpc_ok
            in
            build_with_input ~f module_ parameters
        | Tezosx.Get_ethereum_tezos_address.Method ->
            let f ethereum_address =
              ethereum_address |> Ethereum_types.encode_address
              |> Tezosx_mod.Tezos_runtime.generate_alias |> rpc_ok
            in
            build_with_input ~f module_ parameters
        | Get_storage_at.Method ->
            let f (address, position, block_param) =
              let* value =
                Backend_rpc.Etherlink.storage_at address position block_param
              in
              rpc_ok value
            in
            build_with_input ~f module_ parameters
        | Generic_block_number.Method ->
            let f (_ : unit option) =
              let (Ex_chain_family chain_family) =
                Configuration.retrieve_chain_family
                  ~l2_chains:config.experimental_features.l2_chains
              in
              let root =
                Durable_storage_path.root_of_chain_family chain_family
              in
              let* block_number = Backend_rpc.current_block_number ~root in
              rpc_ok block_number
            in
            build ~f module_ parameters
        | Block_number.Method ->
            let f (_ : unit option) =
              let* block_number =
                Backend_rpc.Etherlink_block_storage.current_block_number ()
              in
              rpc_ok block_number
            in
            build ~f module_ parameters
        | Get_block_by_number.Method ->
            let f (block_param, full_transaction_object) =
              let* block =
                get_block_by_number
                  ~full_transaction_object
                  block_param
                  (module Backend_rpc)
              in
              rpc_ok block
            in
            build_with_input ~f module_ parameters
        | Get_block_by_hash.Method ->
            let f (block_hash, full_transaction_object) =
              let* block =
                Backend_rpc.Etherlink_block_storage.block_by_hash
                  ~full_transaction_object
                  block_hash
              in
              rpc_ok block
            in
            build_with_input ~f module_ parameters
        | Get_block_receipts.Method ->
            let f block_param =
              let* receipts =
                get_block_receipts block_param (module Backend_rpc)
              in
              rpc_ok receipts
            in
            build_with_input ~f module_ parameters
        | Get_code.Method ->
            let f (address, block_param) =
              let* code = Backend_rpc.Etherlink.code address block_param in
              rpc_ok code
            in
            build_with_input ~f module_ parameters
        | Gas_price.Method ->
            let f (_ : unit option) =
              (* We want `eth_gasPrice` to give a result that makes it very
                 unlikely that the transaction is refused by the sequencer
                 because of a gas price variation. As a consequence, we will
                 over-approximate the variation of the gas price by computing
                 the next gas price with these two inputs:

                   – Arbitrary double the backlog
                   – As if the block of inclusion is done in the very same
                     second as the latest block

                 This should lead `eth_gasPrice` to provide an
                 over-approximation when the gas price is not the default one,
                 but still keep the output of `eth_gasPrice` to the minimal one
                 when the chain is not overloaded.

                 In order to avoid approximating too much, we cap the gas price
                 increase to 33% of what it was before. For a current gas price
                 of 1.5Gwei, the result of `eth_gasPrice` cannot be larger than
                 2 Gwei (¾ * 1.5). *)
              let* minimum =
                Backend_rpc.Etherlink.minimum_base_fee_per_gas ()
              in
              let* (Qty latest_price) =
                Backend_rpc.Etherlink.base_fee_per_gas ()
              in
              let* backlog = Backend_rpc.Etherlink.backlog () in
              let* storage_version = Backend_rpc.storage_version () in
              let base_fee =
                let open Z in
                min
                  (latest_price * of_int 4 / of_int 3)
                  (G.price_from_backlog
                     ~version:storage_version
                     ~minimum
                     (backlog * of_int 2))
              in
              rpc_ok (Qty base_fee)
            in
            build ~f module_ parameters
        | Get_transaction_count.Method ->
            let f (address, block_param) =
              match block_param with
              | Ethereum_types.Block_parameter.(Block_parameter Pending) ->
                  process_based_on_mode
                    mode
                    ~on_rpc:(inject_rpc_call config request module_)
                    ~on_stateful_michelson:(fun _ ->
                      failwith
                        "Unsupported JSONRPC method in Tezlink: \
                         getTransactionCount")
                    ~on_stateful_evm:(fun (module Tx_container) ->
                      let* next_nonce =
                        Backend_rpc.Etherlink.nonce address block_param
                      in
                      let next_nonce =
                        Option.value ~default:Qty.zero next_nonce
                      in
                      let* nonce = Tx_container.nonce ~next_nonce address in
                      rpc_ok nonce)
              | _ ->
                  let* nonce =
                    Backend_rpc.Etherlink.nonce address block_param
                  in
                  let nonce = Option.value ~default:Qty.zero nonce in
                  rpc_ok nonce
            in
            build_with_input ~f module_ parameters
        | Get_block_transaction_count_by_hash.Method ->
            let f block_hash =
              let* block =
                Backend_rpc.Etherlink_block_storage.block_by_hash
                  ~full_transaction_object:false
                  block_hash
              in
              rpc_ok (block_transaction_count block)
            in
            build_with_input ~f module_ parameters
        | Get_block_transaction_count_by_number.Method ->
            let f block_param =
              let* block =
                get_block_by_number
                  ~full_transaction_object:false
                  block_param
                  (module Backend_rpc)
              in
              rpc_ok (block_transaction_count block)
            in
            build_with_input ~f module_ parameters
        | Get_uncle_count_by_block_hash.Method ->
            let f _block_param = rpc_ok Qty.zero in
            build_with_input ~f module_ parameters
        | Get_uncle_count_by_block_number.Method ->
            let f _block_param = rpc_ok Qty.zero in
            build_with_input ~f module_ parameters
        | Get_transaction_receipt.Method ->
            let f tx_hash =
              Octez_telemetry.Trace.add_attrs (fun () ->
                  Telemetry.Attributes.[Transaction.hash tx_hash]) ;

              let* receipt =
                Backend_rpc.Etherlink_block_storage.transaction_receipt tx_hash
              in
              rpc_ok receipt
            in
            build_with_input ~f module_ parameters
        | Get_transaction_gas_info.Method ->
            let f tx_hash =
              let* receipt =
                Backend_rpc.Etherlink_block_storage.transaction_receipt tx_hash
              in
              let* object_ =
                Backend_rpc.Etherlink_block_storage.transaction_object tx_hash
              in
              match (receipt, object_) with
              | Some receipt, Some object_ -> (
                  let tx_data =
                    Transaction_object.input object_
                    |> Ethereum_types.hex_to_real_bytes
                  in
                  let* block =
                    let (Qty number) = receipt.blockNumber in
                    Backend_rpc.Etherlink_block_storage.nth_block
                      ~full_transaction_object:false
                      number
                  in
                  let* state = Backend_rpc.Reader.get_state () in
                  let* da_fee_per_byte_bytes =
                    Backend_rpc.Reader.read
                      state
                      Durable_storage_path.da_fee_per_byte
                  in
                  match (da_fee_per_byte_bytes, block.baseFeePerGas) with
                  | Some da_fee_per_byte_bytes, Some (Qty base_fee_per_gas) ->
                      let da_fee_per_byte =
                        Helpers.decode_z_le da_fee_per_byte_bytes
                      in
                      let da_fees =
                        Fees.gas_used_for_da_fees
                          ~da_fee_per_byte:(Qty da_fee_per_byte)
                          ~base_fee_per_gas
                          tx_data
                      in
                      let (Qty gas_used) = receipt.gasUsed in
                      rpc_ok
                        (Some
                           {
                             execution_gas = Qty (Z.sub gas_used da_fees);
                             inclusion_gas = Qty da_fees;
                           })
                  | _, _ ->
                      rpc_error
                        (Rpc_errors.internal_error
                           "could not find all necessary inputs"))
              | _, _ -> rpc_ok None
            in
            build_with_input ~f module_ parameters
        | Get_transaction_by_hash.Method ->
            let f tx_hash =
              Octez_telemetry.Trace.(
                add_attrs (fun () ->
                    Telemetry.Attributes.[Transaction.hash tx_hash])) ;
              process_based_on_mode
                mode
                ~on_rpc:(inject_rpc_call config request module_)
                ~on_stateful_michelson:(fun _ ->
                  failwith
                    "Unsupported JSONRPC method in Tezlink: \
                     GetTransactionByHash")
                ~on_stateful_evm:(fun (module Tx_container) ->
                  let* transaction_object = Tx_container.find tx_hash in
                  match transaction_object with
                  | Some transaction_object -> rpc_ok (Some transaction_object)
                  | None ->
                      let* transaction_object =
                        Backend_rpc.Etherlink_block_storage.transaction_object
                          tx_hash
                      in
                      rpc_ok transaction_object)
            in
            build_with_input ~f module_ parameters
        | Get_transaction_by_block_hash_and_index.Method ->
            let f (block_hash, Qty index) =
              let* block =
                Backend_rpc.Etherlink_block_storage.block_by_hash
                  ~full_transaction_object:false
                  block_hash
              in
              let* transaction_object =
                get_transaction_from_index
                  block
                  (Z.to_int index)
                  (module Backend_rpc)
              in
              rpc_ok transaction_object
            in
            build_with_input ~f module_ parameters
        | Get_transaction_by_block_number_and_index.Method ->
            let f (block_number, Qty index) =
              let* block =
                get_block_by_number
                  ~full_transaction_object:false
                  block_number
                  (module Backend_rpc)
              in
              let* transaction_object =
                get_transaction_from_index
                  block
                  (Z.to_int index)
                  (module Backend_rpc)
              in
              rpc_ok transaction_object
            in
            build_with_input ~f module_ parameters
        | Get_uncle_by_block_hash_and_index.Method ->
            let f (_block_hash, _index) =
              (* A block cannot have uncles. *)
              rpc_ok None
            in
            build_with_input ~f module_ parameters
        | Get_uncle_by_block_number_and_index.Method ->
            let f (_block_number, _index) =
              (* A block cannot have uncles. *)
              rpc_ok None
            in
            build_with_input ~f module_ parameters
        | Send_raw_transaction.Method ->
            if not config.experimental_features.enable_send_raw_transaction then
              Telemetry.Jsonrpc.return_error
              @@ Rpc_errors.transaction_rejected
                   "the node is in read-only mode, it doesn't accept \
                    transactions"
                   None
            else
              let f raw_tx = send_raw_transaction config mode raw_tx in
              build_with_input ~f module_ parameters
        | Send_raw_transaction_sync.Method ->
            let wait_or_timeout timeout f =
              match timeout with
              | 0L -> f ()
              | timeout ->
                  let timeout = Int64.to_float timeout in
                  (*milliseconds to seconds*)
                  let timeout = timeout /. 1000. in
                  Lwt.catch
                    (fun () -> Lwt_unix.with_timeout timeout f)
                    (function
                      | Lwt_unix.Timeout ->
                          rpc_error
                            (Rpc_errors.transaction_rejected
                               (Format.sprintf
                                  "The transaction was added to the mempool \
                                   but wasn't processed in %d seconds"
                                  (int_of_float timeout))
                               None)
                      | exn -> Lwt.fail exn)
            in
            let f (raw_tx, timeout, block_parameter) =
              match block_parameter with
              (* Wait for the receipt in the stream *)
              | Ethereum_types.Block_parameter.Pending -> (
                  match mode with
                  | Rpc {evm_node_endpoint; _} -> (
                      (* For the RPC mode forward the call *)
                      let* res =
                        Injector.call_singleton_request
                          ~keep_alive:config.keep_alive
                          ~timeout:config.rpc_timeout
                          ~base:evm_node_endpoint
                          module_
                          request
                      in
                      return
                      @@
                      match res with
                      | Ok output -> rpc_ok output
                      | Error reason ->
                          rpc_error
                            (Rpc_errors.transaction_rejected reason None))
                  | _ -> (
                      let transaction_result_stream, stopper =
                        Broadcast.create_transaction_result_stream ()
                      in
                      let* hash = send_raw_transaction config mode raw_tx in
                      let receipt_from_stream hash =
                        let*! receipt =
                          Lwt_stream.find
                            (fun (r : Broadcast.transaction_result) ->
                              r.hash = hash)
                            transaction_result_stream
                        in
                        return
                          (Option.map
                             (fun Broadcast.{result; _} -> result)
                             receipt)
                      in
                      let close_stream_and_return k =
                        let lwt_promess =
                          Lwt.finalize k (fun () ->
                              Lwt_watcher.shutdown stopper ;
                              Lwt.return_unit)
                        in
                        return lwt_promess
                      in
                      close_stream_and_return @@ fun () ->
                      wait_or_timeout timeout @@ fun () ->
                      match hash with
                      | Error reason -> rpc_error reason
                      | Ok hash -> (
                          let* receipt = receipt_from_stream hash in
                          match receipt with
                          | Some (Ok receipt) -> rpc_ok receipt
                          | Some (Error reason) ->
                              rpc_error
                                (Rpc_errors.transaction_rejected
                                   reason
                                   (Some hash))
                          | None ->
                              rpc_error
                                (Rpc_errors.transaction_rejected
                                   "Transaction receipt not found"
                                   (Some hash)))))
              | Ethereum_types.Block_parameter.Latest -> (
                  (* Use normal execution infos *)
                  let wait_confirmation, wait_confirmation_wakener =
                    Lwt.wait ()
                  in
                  let* hash_res =
                    send_raw_transaction
                      config
                      mode
                      raw_tx
                      ~wait_confirmation_wakener
                  in
                  let receipt_from_backend hash =
                    let* receipt =
                      Backend_rpc.Etherlink_block_storage.transaction_receipt
                        hash
                    in
                    match receipt with
                    | Some receipt -> rpc_ok receipt
                    | None ->
                        rpc_error
                          (Rpc_errors.transaction_rejected
                             "Transaction receipt not found"
                             (Some hash))
                  in
                  let wait_confirmation_and_get_receipt hash =
                    let* wait_confirmation in
                    match wait_confirmation with
                    | Ok () -> receipt_from_backend hash
                    | Error reason ->
                        rpc_error
                        @@ Rpc_errors.transaction_rejected reason (Some hash)
                  in
                  return @@ wait_or_timeout timeout
                  @@ fun () ->
                  match hash_res with
                  | Error reason -> rpc_error reason
                  | Ok hash -> wait_confirmation_and_get_receipt hash)
              | _ ->
                  return @@ rpc_error
                  @@ Rpc_errors.invalid_params
                       "The block parameter for sendRawTransactionSync must be \
                        'latest' or 'pending'"
            in
            build_with_input_and_lazy_output ~f module_ parameters
        | Eth_call.Method ->
            let f (call, block_param, state_override) =
              let* call_result =
                Backend_rpc.Etherlink.simulate_call
                  ~overwrite_tick_limit:
                    config.experimental_features.overwrite_simulation_tick_limit
                  call
                  block_param
                  state_override
              in
              match call_result with
              | Ok (Ok {value = Some value; gas_used = _}) -> rpc_ok value
              | Ok (Ok {value = None; gas_used = _}) ->
                  rpc_ok (hash_of_string "")
              | Ok (Error reason) ->
                  if reason = Hash (Hex "") then
                    rpc_error
                    @@ Rpc_errors.transaction_rejected "execution reverted" None
                  else
                    rpc_error
                    @@ Rpc_errors.transaction_rejected
                         "execution reverted"
                         (Some reason)
              | Error reason ->
                  rpc_error (Rpc_errors.transaction_rejected reason None)
            in
            build_with_input ~f module_ parameters
        | Get_estimate_gas.Method ->
            let f (call, block_param, state_override) =
              let* result =
                Backend_rpc.Etherlink.estimate_gas
                  call
                  block_param
                  state_override
              in
              match result with
              | Ok (Ok {value = _; gas_used = Some gas}) -> rpc_ok gas
              | Ok (Ok {value = _; gas_used = None}) ->
                  rpc_error
                    (Rpc_errors.limit_exceeded
                       "Simulation failed before execution, cannot estimate \
                        gas."
                       None)
              | Ok (Error reason) ->
                  rpc_error
                  @@ Rpc_errors.limit_exceeded
                       "execution reverted"
                       (Some reason)
              | Error reason ->
                  rpc_error (Rpc_errors.limit_exceeded reason None)
            in
            build_with_input ~f module_ parameters
        | Txpool_content.Method ->
            let f (_ : unit option) =
              process_based_on_mode
                mode
                ~on_rpc:(inject_rpc_call config request module_)
                ~on_stateful_michelson:(fun _ ->
                  failwith
                    "Unsupported JSONRPC method in Tezlink: txpoolContent")
                ~on_stateful_evm:(fun (module Tx_container) ->
                  let* txpool_content = Tx_container.content () in
                  rpc_ok txpool_content)
            in
            build ~f module_ parameters
        | Web3_clientVersion.Method ->
            let f (_ : unit option) = rpc_ok client_version in
            build ~f module_ parameters
        | Web3_sha3.Method ->
            let f data =
              let open Ethereum_types in
              let (Hex h) = data in
              let bytes = Hex.to_bytes_exn (`Hex h) in
              let hash_bytes = Tezos_crypto.Hacl.Hash.Keccak_256.digest bytes in
              let hash = Hex.of_bytes hash_bytes |> Hex.show in
              rpc_ok (Hash (Hex hash))
            in
            build_with_input ~f module_ parameters
        | Get_logs.Method ->
            let f filter =
              let* logs =
                Filter_helpers.get_logs
                  config.log_filter
                  (module Backend_rpc)
                  filter
              in
              rpc_ok logs
            in
            build_with_input ~f module_ parameters
        (* Internal RPC methods *)
        | Kernel_version.Method ->
            let f (_ : unit option) =
              let* kernel_version = Backend_rpc.kernel_version () in
              rpc_ok kernel_version
            in
            build ~f module_ parameters
        | Kernel_root_hash.Method ->
            let f (_ : unit option) =
              let* kernel_root_hash = Backend_rpc.kernel_root_hash () in
              rpc_ok kernel_root_hash
            in
            build ~f module_ parameters
        | Rpc_encodings.Sequencer.Method ->
            let f block_param =
              let block =
                Option.value
                  ~default:(Block_parameter.Block_parameter Latest)
                  block_param
              in
              let* state = Backend_rpc.Reader.get_state ~block () in
              let* pk =
                Durable_storage.sequencer (Backend_rpc.Reader.read state)
              in
              rpc_ok pk
            in
            build ~f module_ parameters
        | Eth_max_priority_fee_per_gas.Method ->
            let f (_ : unit option) = rpc_ok Qty.zero in
            build ~f module_ parameters
        | Trace_transaction.Method ->
            let f ((hash, config) : Tracer_types.input) =
              let*! trace =
                Backend_rpc.Tracer_etherlink.trace_transaction hash config
              in
              process_trace_result trace
            in
            build_with_input ~f module_ parameters
        | Eth_fee_history.Method ->
            let f (Qty block_count, newest_block, _reward_percentile) =
              if block_count <= Z.zero then
                rpc_error
                  (Rpc_errors.invalid_params
                     "Number of block should be greater than 0.")
              else
                let* fee_history_result =
                  get_fee_history
                    block_count
                    newest_block
                    config
                    (module Backend_rpc)
                in
                rpc_ok fee_history_result
            in
            build_with_input ~f module_ parameters
        | Coinbase.Method ->
            let f (_ : unit option) =
              let open Lwt_result_syntax in
              let* coinbase = Backend_rpc.Etherlink.coinbase () in
              rpc_ok coinbase
            in
            build ~f module_ parameters
        | Trace_call.Method ->
            let f (((call, block), config) : Tracer_types.call_input) =
              let*! trace =
                Backend_rpc.Tracer_etherlink.trace_call call block config
              in
              process_trace_result trace
            in
            build_with_input ~f module_ parameters
        | Trace_block.Method ->
            let f ((block_param, config) : Tracer_types.block_input) =
              let* (Ethereum_types.Qty block_number) =
                Backend_rpc.block_param_to_block_number
                  ~chain_family:L2_types.EVM
                  (Block_parameter block_param)
              in
              let*! traces =
                Backend_rpc.Tracer_etherlink.trace_block
                  (Qty block_number)
                  config
              in
              process_trace_result traces
            in

            build_with_input ~f module_ parameters
        | Get_finalized_blocks_of_l1_level.Method ->
            let f l1_level =
              let open Lwt_result_syntax in
              let* l2_levels = Backend_rpc.l2_levels_of_l1_level l1_level in
              match l2_levels with
              | Some {start_l2_level; end_l2_level} ->
                  rpc_ok Rpc_encodings.{start_l2_level; end_l2_level}
              | None ->
                  rpc_error
                    (Rpc_errors.resource_not_found
                       (Format.sprintf "Unknown L1 block %ld" l1_level))
            in
            build_with_input ~f module_ parameters
        | _ ->
            Stdlib.failwith "The pattern matching of methods is not exhaustive")
  in
  Lwt.return JSONRPC.{return_value; id}

let dispatch_private_request (type f) ~websocket
    (rpc_server_family : f Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) (_config : Configuration.t) (mode : f Mode.t)
    ((module Backend_rpc : Services_backend_sig.S), _) ~block_production
    ({method_; parameters; id} : JSONRPC.request) :
    JSONRPC.return_response Lwt.t =
  let open Lwt_syntax in
  Telemetry.Jsonrpc.trace_dispatch_with
    ~websocket
    ~service_name:"private_rpc"
    method_
    id
  @@ fun _scope ->
  let unsupported () =
    Lwt.return
    @@ direct_rpc_value
         (Error
            JSONRPC.
              {
                code = -3200;
                message = "Method not supported";
                data = Some (`String method_);
              })
  in
  let* return_value =
    (* Private RPCs can only be accessed locally, they're not accessible to the
       end user. *)
    match
      map_method_name ~rpc_server_family ~restrict:rpc.restricted_rpcs method_
    with
    | Unknown ->
        Lwt.return
        @@ direct_rpc_value
             (Error
                JSONRPC.
                  {
                    code = -3200;
                    message = "Method not found";
                    data = Some (`String method_);
                  })
    | Unsupported -> unsupported ()
    | Disabled ->
        Telemetry.Jsonrpc.return_error (Rpc_errors.method_disabled method_)
    | Method (Produce_block.Method, _) when block_production <> `Single_node ->
        unsupported ()
    | Method (Produce_block.Method, module_) ->
        let f input =
          let open Lwt_result_syntax in
          let force, with_delayed_transactions =
            match input with
            | Some {timestamp = Some timestamp; with_delayed_transactions} ->
                ( Block_producer.With_timestamp timestamp,
                  with_delayed_transactions )
            | Some {timestamp = None; with_delayed_transactions} ->
                (True, with_delayed_transactions)
            | None -> (True, true)
          in
          let* has_produced_block =
            Block_producer.Internal_for_tests.produce_block
              ~with_delayed_transactions
              ~force
          in
          match has_produced_block with
          | `Block_produced nb_transactions ->
              rpc_ok (Ethereum_types.quantity_of_z @@ Z.of_int nb_transactions)
          | `No_block ->
              rpc_error
                (Rpc_errors.internal_error
                   (Format.sprintf "Block production failed"))
        in
        build ~f module_ parameters
    | Method (Propose_next_block_timestamp.Method, _module_)
      when block_production <> `Single_node ->
        unsupported ()
    | Method (Propose_next_block_timestamp.Method, module_) ->
        let f timestamp =
          let open Lwt_result_syntax in
          let* () =
            Block_producer.Internal_for_tests.propose_next_block_timestamp
              ~next_block_timestamp:timestamp
          in
          rpc_ok ()
        in
        build_with_input ~f module_ parameters
    | Method (Wait_transaction_confirmation.Method, module_) ->
        let open Lwt_result_syntax in
        let f hash =
          Octez_telemetry.Trace.(
            add_attrs (fun () -> Telemetry.Attributes.[Transaction.hash hash])) ;
          process_based_on_mode
            mode
            ~on_stateful_evm:(fun (module Tx_container) ->
              let wait_confirmation, wait_confirmation_wakener = Lwt.wait () in
              let callback = function
                | `Missing ->
                    let*! receipt =
                      Backend_rpc.Etherlink_block_storage.transaction_receipt
                        hash
                    in
                    let wakup_value =
                      Result.map
                        (function
                          | Some _ -> Ok ()
                          | None ->
                              Result.error
                                (Format.asprintf "Transaction was not found."))
                        receipt
                    in
                    Lwt.wakeup wait_confirmation_wakener wakup_value ;
                    Lwt.return_unit
                | (`Dropped | `Confirmed) as status ->
                    wait_confirmation_callback wait_confirmation_wakener status
              in
              let* () = Tx_container.add_pending_callback hash ~callback in
              let wait_on_confirmation () =
                let* wait_confirmation in
                match wait_confirmation with
                | Ok () -> rpc_ok ()
                | Error reason ->
                    rpc_error
                      (Rpc_errors.transaction_rejected reason (Some hash))
              in
              return (wait_on_confirmation ()))
            ~on_stateful_michelson:(fun _ ->
              return
              @@ failwith
                   "Unsupported JSONRPC method in Tezlink: \
                    Wait_transaction_confirmation")
            ~on_rpc:(fun _ ->
              return
              @@ failwith
                   "Unsupported JSONRPC method in Rpc: \
                    Wait_transaction_confirmation")
        in
        build_with_input_and_lazy_output ~f module_ parameters
    | Method (Inject_transaction.Method, module_) ->
        let open Lwt_result_syntax in
        let f
            ( (transaction_object : Transaction_object.t),
              raw_txn,
              wait_confirmation ) :
            (Ethereum_types.hash, JSONRPC.error) result tzresult Lwt.t tzresult
            Lwt.t =
          Octez_telemetry.Trace.(
            add_attrs (fun () ->
                Telemetry.Attributes.
                  [
                    Transaction.hash (Transaction_object.hash transaction_object);
                  ])) ;

          let* next_nonce =
            let* next_nonce =
              Backend_rpc.Etherlink.nonce
                (Transaction_object.sender transaction_object)
                (Block_parameter Latest)
            in
            return
            @@
            match next_nonce with
            | None -> Ethereum_types.Qty Z.zero
            | Some next_nonce -> next_nonce
          in
          let transaction = Ethereum_types.hex_encode_string raw_txn in
          process_based_on_mode
            mode
            ~on_stateful_evm:(fun (module Tx_container) ->
              if wait_confirmation then
                let wait_confirmation, wait_confirmation_wakener =
                  Lwt.wait ()
                in
                let callback =
                  wait_confirmation_callback wait_confirmation_wakener
                in
                let* tx_hash_res =
                  Tx_container.add
                    ~next_nonce
                    transaction_object
                    ~raw_tx:transaction
                    ~callback
                in
                let wait_on_confirmation hash =
                  let* wait_confirmation in
                  match wait_confirmation with
                  | Ok () -> rpc_ok hash
                  | Error reason ->
                      rpc_error
                      @@ Rpc_errors.transaction_rejected reason (Some hash)
                in
                return
                @@
                match tx_hash_res with
                | Error reason ->
                    rpc_error (Rpc_errors.transaction_rejected reason None)
                | Ok hash -> wait_on_confirmation hash
              else
                let* tx_hash_res =
                  Tx_container.add
                    ~next_nonce
                    transaction_object
                    ~raw_tx:transaction
                in
                return
                @@
                match tx_hash_res with
                | Error reason ->
                    rpc_error (Rpc_errors.transaction_rejected reason None)
                | Ok hash -> rpc_ok hash)
            ~on_rpc:(fun _ ->
              return
              @@ failwith "Unsupported JSONRPC method in Rpc: injectTransaction")
            ~on_stateful_michelson:(fun _ ->
              return
              @@ failwith
                   "Unsupported JSONRPC method in Tezlink: injectTransaction")
        in
        build_with_input_and_lazy_output ~f module_ parameters
    | Method (Inject_tezlink_operation.Method, module_) ->
        let open Lwt_result_syntax in
        let f ((op : Tezos_types.Operation.t), raw_op) =
          Octez_telemetry.Trace.(
            add_attrs (fun () ->
                (* TODO: https://gitlab.com/tezos/tezos/-/issues/8014
                   Add an attribute for Tezlink operations (in base58)
                   instead of reusing the Etherlink hexadecimal
                   identifier. *)
                Telemetry.Attributes.
                  [Transaction.hash (Tezos_types.Operation.hash_operation op)])) ;
          let* hash =
            let* (module Tx_container) =
              match mode with
              | Observer (Michelson_tx_container m)
              | Sequencer (Michelson_tx_container m) ->
                  return m
              | Observer (Evm_tx_container _m) | Sequencer (Evm_tx_container _m)
                ->
                  failwith
                    "Unsupported JSONRPC method in Etherlink: \
                     injectTezlinkOperation"
              | Proxy _ ->
                  failwith
                    "Unsupported JSONRPC method in proxy: \
                     injectTezlinkOperation"
              | Rpc _ ->
                  failwith
                    "Unsupported JSONRPC method in Rpc: injectTezlinkOperation"
            in
            Tx_container.add
              ~next_nonce:(Ethereum_types.Qty op.first_counter)
              op
              ~raw_tx:(Ethereum_types.hex_of_bytes raw_op)
          in
          match hash with
          | Ok hash -> rpc_ok hash
          | Error reason ->
              rpc_error (Rpc_errors.transaction_rejected reason None)
        in
        build_with_input ~f module_ parameters
    | Method (Durable_state_value.Method, module_) ->
        let f (path, block) =
          let open Lwt_result_syntax in
          let* state = Backend_rpc.Reader.get_state ~block () in
          let* value = Backend_rpc.Reader.read state path in
          rpc_ok value
        in
        build_with_input ~f module_ parameters
    | Method (Durable_state_subkeys.Method, module_) ->
        let f (path, block) =
          let open Lwt_result_syntax in
          let* state = Backend_rpc.Reader.get_state ~block () in
          let* value = Backend_rpc.Reader.subkeys state path in
          rpc_ok value
        in
        build_with_input ~f module_ parameters
    | Method (Replay_block.Method, module_) ->
        let f block_number =
          let open Lwt_result_syntax in
          let*? block_number =
            Option.to_result
              ~none:[error_of_fmt "missing block number"]
              block_number
          in
          let* block = Backend_rpc.Etherlink.replay block_number in
          rpc_ok block
        in
        build ~f module_ parameters
    | _ -> Stdlib.failwith "The pattern matching of methods is not exhaustive"
  in
  return JSONRPC.{return_value; id}

let can_process_batch size = function
  | Configuration.Limit l -> size <= l
  | Unlimited -> true

let dispatch_handler ~service_name (rpc : Configuration.rpc) ctx ~tick
    dispatch_request (input : JSONRPC.request batched_request) =
  let open Lwt_syntax in
  let wait_for_return_output JSONRPC.{return_value; id} =
    match return_value with
    | JSONRPC.Direct value -> return JSONRPC.{value; id}
    | JSONRPC.Lazy value ->
        let* value in
        return JSONRPC.{value; id}
  in
  match input with
  | Singleton request ->
      let* return_value = dispatch_request ctx request in
      let* _ = tick () in
      let* response = wait_for_return_output return_value in
      return (Singleton response)
  | Batch requests ->
      let batch_size = List.length requests in
      Telemetry.Jsonrpc.trace_batch_with ~service_name ~batch_size
      @@ fun _scope ->
      let process =
        if can_process_batch batch_size rpc.batch_limit then
          dispatch_request ctx
        else fun req ->
          let response =
            direct_rpc_value
              (Error Rpc_errors.(invalid_request "too many requests in batch"))
          in
          return JSONRPC.{return_value = response; id = req.id}
      in
      let* outputs_waiter = List.map_s process requests in
      let* _ = tick () in
      let* outputs = List.map_p wait_for_return_output outputs_waiter in
      return (Batch outputs)

let websocket_response_of_response response = {response; subscription = None}

let encode_subscription_response subscription r =
  let result =
    match r with
    | Ok r ->
        Data_encoding.Json.construct
          (Ethereum_types.Subscription.output_encoding
             Transaction_object.encoding
             Transaction_receipt.encoding)
          r
    | Error err ->
        let msg =
          match err with [] -> "" | e :: _ -> (find_info_of_error e).id
        in
        let data = Data_encoding.Json.construct trace_encoding err in
        Rpc_errors.internal_error msg ~data
        |> Data_encoding.Json.construct JSONRPC.error_encoding
  in
  Rpc_encodings.Subscription.{params = {result; subscription}}

let empty_stream =
  let stream, push = Lwt_stream.create () in
  push None ;
  (stream, fun () -> Lwt_result_syntax.return_false)

let empty_sid = Ethereum_types.(Subscription.Id (Hex ""))

let dispatch_websocket (rpc_server_family : _ Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) config mode ctx (input : JSONRPC.request) =
  let open Lwt_syntax in
  match
    map_method_name
      ~rpc_server_family
      ~restrict:rpc.restricted_rpcs
      input.method_
  with
  | Method (Subscribe.Method, module_) ->
      let sub_stream = ref empty_stream in
      let sid = ref empty_sid in
      let f (kind : Ethereum_types.Subscription.kind) =
        let open Lwt_result_syntax in
        let* id, stream = eth_subscribe config mode ~kind (fst ctx) in
        (* This is an optimization to avoid having to search in the map
           of subscriptions for `stream` and `id`. *)
        sub_stream := stream ;
        sid := id ;
        rpc_ok id
      in
      let* return_value = build_with_input ~f module_ input.parameters in
      let* value =
        match return_value with
        | JSONRPC.Direct value -> return value
        | JSONRPC.Lazy value -> value
      in
      let response = JSONRPC.{value; id = input.id} in
      let subscription_id = !sid in
      let stream, stopper = !sub_stream in
      let stream =
        Lwt_stream.map (encode_subscription_response subscription_id) stream
      in
      return
        {response; subscription = Some {id = subscription_id; stream; stopper}}
  | Method (Unsubscribe.Method, module_) ->
      let f (id : Ethereum_types.Subscription.id) =
        let open Lwt_result_syntax in
        let* status = eth_unsubscribe ~id in
        rpc_ok status
      in
      let* return_value = build_with_input ~f module_ input.parameters in
      let+ value =
        match return_value with
        | JSONRPC.Direct value -> return value
        | JSONRPC.Lazy value -> value
      in
      websocket_response_of_response JSONRPC.{value; id = input.id}
  | _ ->
      let* {return_value; id} =
        dispatch_request
          ~websocket:true
          rpc_server_family
          rpc
          config
          mode
          ctx
          input
      in
      let+ response =
        match return_value with
        | JSONRPC.Direct value -> return JSONRPC.{value; id}
        | JSONRPC.Lazy value ->
            let* value in
            return JSONRPC.{value; id}
      in
      websocket_response_of_response response

let dispatch_private_websocket
    (rpc_server_family : _ Rpc_types.rpc_server_family) ~block_production
    (rpc : Configuration.rpc) config mode ctx (input : JSONRPC.request) =
  let open Lwt_syntax in
  let* {return_value; id} =
    dispatch_private_request
      ~websocket:true
      rpc_server_family
      ~block_production
      rpc
      config
      mode
      ctx
      input
  in
  let+ response =
    match return_value with
    | JSONRPC.Direct value -> return JSONRPC.{value; id}
    | JSONRPC.Lazy value ->
        let* value in
        return JSONRPC.{value; id}
  in
  websocket_response_of_response response

let generic_dispatch ~service_name (rpc : Configuration.rpc) ctx dir path ~tick
    dispatch_request =
  Evm_directory.register0 dir (dispatch_batch_service ~path) (fun () input ->
      dispatch_handler ~service_name rpc ctx ~tick dispatch_request input
      |> Lwt_result.ok)

let dispatch_public (type f) (rpc_server_family : f Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) config (mode : f Mode.t) ctx dir ~tick =
  generic_dispatch
    ~service_name:"public_rpc"
    rpc
    ctx
    dir
    Path.root
    ~tick
    (dispatch_request ~websocket:false rpc_server_family rpc config mode)

let dispatch_private (type f)
    (rpc_server_family : _ Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) ~block_production config (mode : f Mode.t) ctx dir
    ~tick =
  generic_dispatch
    ~service_name:"private_rpc"
    rpc
    ctx
    dir
    Path.(add_suffix root "private")
    ~tick
    (dispatch_private_request
       ~websocket:false
       rpc_server_family
       ~block_production
       rpc
       config
       mode)

let generic_websocket_dispatch (config : Configuration.t) mode ctx dir path
    dispatch_websocket =
  match config.websockets with
  | None -> dir
  | Some {max_message_length; monitor_heartbeat; _} ->
      Evm_directory.jsonrpc_websocket_register
        ?monitor:monitor_heartbeat
        ~max_message_length
        dir
        path
        (dispatch_websocket config mode ctx)

let dispatch_websocket_public
    (rpc_server_family : _ Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) config mode ctx dir =
  generic_websocket_dispatch
    config
    mode
    ctx
    dir
    "/ws"
    (dispatch_websocket rpc_server_family rpc)

let dispatch_websocket_private
    (rpc_server_family : _ Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) ~block_production config mode ctx dir =
  generic_websocket_dispatch
    config
    mode
    ctx
    dir
    "/private/ws"
    (dispatch_private_websocket rpc_server_family ~block_production rpc)

let directory (type f) ~(rpc_server_family : f Rpc_types.rpc_server_family)
    (mode : f Mode.t) rpc config backend dir ~tick =
  let db_liveness_check () =
    let open Lwt_result_syntax in
    let (module Backend : Services_backend_sig.S) = fst backend in
    match rpc_server_family with
    | Rpc_types.Multichain_sequencer_rpc_server -> return_unit
    | Rpc_types.Single_chain_node_rpc_server f -> (
        match f with
        | EVM ->
            let* _ = Backend.Etherlink_block_storage.current_block_number () in
            return_unit
        | Michelson ->
            let* _ =
              Backend.Tezlink.current_level `Main (`Head 0l) ~offset:0l
            in
            return_unit)
  in

  dir |> version |> configuration config |> evm_mode config mode
  |> health_check config mode db_liveness_check
  |> dispatch_public rpc_server_family rpc config mode backend ~tick
  |> dispatch_websocket_public rpc_server_family rpc config mode backend

let private_directory ~rpc_server_family mode rpc config backend
    ~block_production ~tick =
  Evm_directory.empty config.experimental_features.rpc_server
  |> version |> evm_mode config mode
  |> dispatch_private
       rpc_server_family
       rpc
       config
       mode
       backend
       ~block_production
       ~tick
  |> dispatch_websocket_private
       rpc_server_family
       rpc
       config
       mode
       backend
       ~block_production
