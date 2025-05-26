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

type error += Node_is_bootstrapping

type error += Node_is_lagging

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
    (fun () -> Node_is_lagging)

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
    Configuration.{config.kernel_execution with preimages = hidden}
  in
  let sequencer =
    Option.map
      (fun (sequencer_config : sequencer) ->
        {sequencer_config with sequencer = Client_keys.sk_uri_of_string hidden})
      config.sequencer
  in
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
      rollup_node_endpoint = Uri.of_string hidden;
      kernel_execution;
      sequencer;
      threshold_encryption_sequencer = None;
      proxy;
      observer;
      private_rpc = None;
    }
  in

  Data_encoding.Json.construct
    ~include_default_fields:`Always
    (Configuration.encoding hidden)
    config

let health_check_handler ?delegate_to query =
  match delegate_to with
  | None ->
      let open Lwt_result_syntax in
      let* () = fail_when (Metrics.is_bootstrapping ()) Node_is_bootstrapping in
      let* () =
        fail_when
          Z.Compare.(
            Drift_monitor.last_observed_drift () > query.drift_threshold)
          Node_is_lagging
      in
      return_unit
  | Some evm_node_endpoint ->
      Rollup_services.call_service
        ~keep_alive:false
        ~base:evm_node_endpoint
        ~media_types:[Media_type.json]
        health_check_service
        ()
        query
        ()

let version dir =
  Evm_directory.register0 dir version_service (fun () () ->
      Lwt.return_ok client_version)

let configuration config dir =
  Evm_directory.register0 dir configuration_service (fun () () ->
      configuration_handler config |> Lwt.return_ok)

let health_check ?delegate_to dir =
  Evm_directory.register0 dir health_check_service (fun query () ->
      health_check_handler ?delegate_to query)

let get_block_by_number ~full_transaction_object block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty n) =
    Rollup_node_rpc.block_param_to_block_number (Block_parameter block_param)
  in
  Rollup_node_rpc.Etherlink_block_storage.nth_block ~full_transaction_object n

let get_block_receipts block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty n) =
    Rollup_node_rpc.block_param_to_block_number (Block_parameter block_param)
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
  stream : Transaction_object.t Ethereum_types.Subscription.output Lwt_stream.t;
  stopper : unit -> unit;
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

let logs_of_filter logs =
  match logs with
  | Ethereum_types.Filter.Log logs ->
      Some (Ethereum_types.Subscription.Logs logs)
  | Ethereum_types.Filter.Block_filter _
  | Ethereum_types.Filter.Pending_transaction_filter _ ->
      None

let filter_from ~address ~topics =
  Ethereum_types.Filter.
    {from_block = None; to_block = None; address; topics; block_hash = None}

let filter_logs ~bloom_filter ~receipt =
  let logs = Filter_helpers.filter_receipt bloom_filter receipt in
  let logs = Option.value ~default:[] logs in
  List.filter_map logs_of_filter logs

let produce_logs_stream ~bloom_filter stream =
  Lwt_stream.map_list (fun receipt -> filter_logs ~bloom_filter ~receipt) stream

let eth_subscribe ~(kind : Ethereum_types.Subscription.kind)
    (module Backend_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let id = make_id ~id:(generate_id ()) in
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
                       } ) ->
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
  let stopper () =
    Stdlib.Hashtbl.remove subscriptions id ;
    Lwt_watcher.shutdown stopper
  in
  Stdlib.Hashtbl.add subscriptions id {kind; stream; stopper} ;
  return (id, (stream, stopper))

let eth_unsubscribe ~id =
  match Stdlib.Hashtbl.find_opt subscriptions id with
  | Some {stopper; _} ->
      stopper () ;
      true
  | None -> false

let decode :
    type a. (module METHOD with type input = a) -> Data_encoding.json -> a =
 fun (module M) v -> Data_encoding.Json.destruct M.input_encoding v

let encode :
    type a. (module METHOD with type output = a) -> a -> Data_encoding.json =
 fun (module M) v -> Data_encoding.Json.construct M.output_encoding v

let build :
    type input output.
    (module METHOD with type input = input and type output = output) ->
    f:(input option -> (output, Rpc_errors.t) Result.t tzresult Lwt.t) ->
    Data_encoding.json option ->
    JSONRPC.value Lwt.t =
 fun (module Method) ~f parameters ->
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let decoded = Option.map (decode (module Method)) parameters in
      let+ v = f decoded in
      match v with
      | Error err ->
          Error
            (Rpc_errors.internal_error
            @@ Format.asprintf "%a" pp_print_trace err)
      | Ok value -> Result.map (encode (module Method)) value)
    (fun exn ->
      Lwt.return_error @@ Rpc_errors.invalid_request @@ Printexc.to_string exn)

let rpc_ok result = Lwt_result.return @@ Ok result

let rpc_error err = Lwt_result.return @@ Error err

let missing_parameter () = rpc_error Rpc_errors.invalid_input

let expect_input input f =
  match input with None -> missing_parameter () | Some v -> f v

let build_with_input method_ ~f parameters =
  build method_ ~f:(fun input -> expect_input input f) parameters

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

let dispatch_request (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) (validation : Validate.validation_mode)
    (config : Configuration.t)
    (module Tx_container : Services_backend_sig.Tx_container)
    ((module Backend_rpc : Services_backend_sig.S), _)
    ({method_; parameters; id} : JSONRPC.request) : JSONRPC.response Lwt.t =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  let*! value =
    match
      map_method_name ~rpc_server_family ~restrict:rpc.restricted_rpcs method_
    with
    | Unknown ->
        Metrics.inc_rpc_method ~name:"unknown" ;
        Lwt.return_error (Rpc_errors.method_not_found method_)
    | Unsupported ->
        Metrics.inc_rpc_method ~name:"unsupported" ;
        Lwt.return_error (Rpc_errors.method_not_supported method_)
    | Disabled ->
        Metrics.inc_rpc_method ~name:"disabled" ;
        Lwt.return_error (Rpc_errors.method_disabled method_)
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
        | Get_storage_at.Method ->
            let f (address, position, block_param) =
              let* value =
                Backend_rpc.Etherlink.storage_at address position block_param
              in
              rpc_ok value
            in
            build_with_input ~f module_ parameters
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
                  let* next_nonce =
                    Backend_rpc.Etherlink.nonce address block_param
                  in
                  let next_nonce = Option.value ~default:Qty.zero next_nonce in
                  let* nonce = Tx_container.nonce ~next_nonce address in
                  rpc_ok nonce
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
              let* receipt =
                Backend_rpc.Etherlink_block_storage.transaction_receipt tx_hash
              in
              rpc_ok receipt
            in
            build_with_input ~f module_ parameters
        | Get_transaction_by_hash.Method ->
            let f tx_hash =
              let* transaction_object = Tx_container.find tx_hash in
              let* transaction_object =
                match transaction_object with
                | Some transaction_object -> return_some transaction_object
                | None ->
                    Backend_rpc.Etherlink_block_storage.transaction_object
                      tx_hash
              in
              rpc_ok transaction_object
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
              Lwt.return_error
              @@ Rpc_errors.transaction_rejected
                   "the node is in read-only mode, it doesn't accept \
                    transactions"
                   None
            else
              let max_number_of_chunks =
                Option.map
                  (fun seq -> seq.Configuration.max_number_of_chunks)
                  config.sequencer
              in
              let f raw_tx =
                let txn = Ethereum_types.hex_to_bytes raw_tx in
                let* is_valid =
                  Validate.is_tx_valid
                    ?max_number_of_chunks
                    (module Backend_rpc)
                    ~mode:validation
                    txn
                in
                match is_valid with
                | Error err ->
                    let*! () =
                      Tx_pool_events.invalid_transaction ~transaction:raw_tx
                    in
                    rpc_error (Rpc_errors.transaction_rejected err None)
                | Ok (next_nonce, transaction_object) -> (
                    let* tx_hash =
                      Tx_container.add ~next_nonce transaction_object ~raw_tx
                    in
                    match tx_hash with
                    | Ok tx_hash -> rpc_ok tx_hash
                    | Error reason ->
                        rpc_error (Rpc_errors.transaction_rejected reason None))
              in
              build_with_input ~f module_ parameters
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
            let f (call, block) =
              let* result = Backend_rpc.Etherlink.estimate_gas call block in
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
              let* txpool_content = Tx_container.content () in
              rpc_ok txpool_content
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
  Lwt.return JSONRPC.{value; id}

let dispatch_private_request (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) (config : Configuration.t)
    (module Tx_container : Services_backend_sig.Tx_container)
    ((module Backend_rpc : Services_backend_sig.S), _) ~block_production
    ({method_; parameters; id} : JSONRPC.request) : JSONRPC.response Lwt.t =
  let open Lwt_syntax in
  let unsupported () =
    return
      (Error
         JSONRPC.
           {
             code = -3200;
             message = "Method not supported";
             data = Some (`String method_);
           })
  in
  let* value =
    (* Private RPCs can only be accessed locally, they're not accessible to the
       end user. *)
    match
      map_method_name ~rpc_server_family ~restrict:rpc.restricted_rpcs method_
    with
    | Unknown ->
        return
          (Error
             JSONRPC.
               {
                 code = -3200;
                 message = "Method not found";
                 data = Some (`String method_);
               })
    | Unsupported -> unsupported ()
    | Disabled -> Lwt.return_error (Rpc_errors.method_disabled method_)
    | Method (Produce_block.Method, _) when block_production <> `Single_node ->
        unsupported ()
    | Method (Produce_block.Method, module_) ->
        let f input =
          let open Lwt_result_syntax in
          let timestamp, with_delayed_transactions =
            match input with
            | Some {timestamp; with_delayed_transactions} ->
                (timestamp, with_delayed_transactions)
            | None -> (None, true)
          in
          let timestamp = Option.value timestamp ~default:(Misc.now ()) in
          let* has_produced_block =
            Block_producer.Internal_for_tests.produce_block
              ~with_delayed_transactions
              ~force:true
              ~timestamp
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
    | Method (Inject_transaction.Method, module_) ->
        let open Lwt_result_syntax in
        let f
            ( (transaction_object : Ethereum_types.legacy_transaction_object),
              raw_txn ) =
          let* is_valid =
            let get_nonce () =
              let* next_nonce =
                Backend_rpc.Etherlink.nonce
                  transaction_object.from
                  (Block_parameter Latest)
              in
              let next_nonce =
                match next_nonce with
                | None -> Ethereum_types.Qty Z.zero
                | Some next_nonce -> next_nonce
              in
              return @@ Ok (next_nonce, transaction_object)
            in
            (* If the tx_queue is enabled the `with_state` validation
               will be done in the block producer *)
            if Configuration.is_tx_queue_enabled config then get_nonce ()
            else
              let* mode = Tx_pool.mode () in
              match mode with
              | Sequencer ->
                  Validate.is_tx_valid
                    (module Backend_rpc)
                    ~mode:With_state
                    raw_txn
              | _ -> get_nonce ()
          in
          let transaction = Ethereum_types.hex_encode_string raw_txn in
          match is_valid with
          | Error err ->
              let*! () = Tx_pool_events.invalid_transaction ~transaction in
              rpc_error (Rpc_errors.transaction_rejected err None)
          | Ok (next_nonce, transaction_object) -> (
              let* tx_hash =
                Tx_container.add
                  ~next_nonce
                  transaction_object
                  ~raw_tx:transaction
              in
              match tx_hash with
              | Ok tx_hash -> rpc_ok tx_hash
              | Error reason ->
                  rpc_error (Rpc_errors.transaction_rejected reason None))
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
  return JSONRPC.{value; id}

let can_process_batch size = function
  | Configuration.Limit l -> size <= l
  | Unlimited -> true

let dispatch_handler (rpc : Configuration.rpc) config tx_container ctx
    dispatch_request (input : JSONRPC.request batched_request) =
  let open Lwt_syntax in
  match input with
  | Singleton request ->
      let* response = dispatch_request config tx_container ctx request in
      return (Singleton response)
  | Batch requests ->
      let process =
        if can_process_batch (List.length requests) rpc.batch_limit then
          dispatch_request config tx_container ctx
        else fun req ->
          let value =
            Error Rpc_errors.(invalid_request "too many requests in batch")
          in
          Lwt.return JSONRPC.{value; id = req.id}
      in
      let* outputs = List.map_s process requests in
      return (Batch outputs)

let websocket_response_of_response response = {response; subscription = None}

let encode_subscription_response subscription r =
  let result =
    Data_encoding.Json.construct
      (Ethereum_types.Subscription.output_encoding Transaction_object.encoding)
      r
  in
  Rpc_encodings.Subscription.{params = {result; subscription}}

let empty_stream =
  let stream, push = Lwt_stream.create () in
  push None ;
  (stream, fun () -> ())

let empty_sid = Ethereum_types.(Subscription.Id (Hex ""))

let dispatch_websocket (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) validation config tx_container ctx
    (input : JSONRPC.request) =
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
        let* id_stream = eth_subscribe ~kind (fst ctx) in
        let id, stream =
          match id_stream with
          | Ok id_stream -> id_stream
          | Error err ->
              Format.kasprintf
                Stdlib.failwith
                "The websocket `logs` event produced the following error: %a"
                pp_print_trace
                err
        in
        (* This is an optimization to avoid having to search in the map
           of subscriptions for `stream` and `id`. *)
        sub_stream := stream ;
        sid := id ;
        rpc_ok id
      in
      let* value = build_with_input ~f module_ input.parameters in
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
        let status = eth_unsubscribe ~id in
        rpc_ok status
      in
      let+ value = build_with_input ~f module_ input.parameters in
      websocket_response_of_response JSONRPC.{value; id = input.id}
  | _ ->
      let+ response =
        dispatch_request
          rpc_server_family
          rpc
          validation
          config
          tx_container
          ctx
          input
      in
      websocket_response_of_response response

let dispatch_private_websocket (rpc_server_family : Rpc_types.rpc_server_family)
    ~block_production (rpc : Configuration.rpc) config tx_container ctx
    (input : JSONRPC.request) =
  let open Lwt_syntax in
  let+ response =
    dispatch_private_request
      rpc_server_family
      ~block_production
      rpc
      config
      tx_container
      ctx
      input
  in
  websocket_response_of_response response

let generic_dispatch (rpc : Configuration.rpc) config tx_container ctx dir path
    dispatch_request =
  Evm_directory.register0 dir (dispatch_batch_service ~path) (fun () input ->
      dispatch_handler rpc config tx_container ctx dispatch_request input
      |> Lwt_result.ok)

let dispatch_public (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) validation config tx_container ctx dir =
  generic_dispatch
    rpc
    config
    tx_container
    ctx
    dir
    Path.root
    (dispatch_request rpc_server_family rpc validation)

let dispatch_private (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) ~block_production config tx_container ctx dir =
  generic_dispatch
    rpc
    config
    tx_container
    ctx
    dir
    Path.(add_suffix root "private")
    (dispatch_private_request rpc_server_family rpc ~block_production)

let generic_websocket_dispatch (config : Configuration.t) tx_container ctx dir
    path dispatch_websocket =
  if config.experimental_features.enable_websocket then
    Evm_directory.jsonrpc_websocket_register
      ?monitor:config.experimental_features.monitor_websocket_heartbeat
      ~max_message_length:
        config.experimental_features.max_websocket_message_length
      dir
      path
      (dispatch_websocket config tx_container ctx)
  else dir

let dispatch_websocket_public (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) validation config tx_container ctx dir =
  generic_websocket_dispatch
    config
    tx_container
    ctx
    dir
    "/ws"
    (dispatch_websocket rpc_server_family rpc validation)

let dispatch_websocket_private (rpc_server_family : Rpc_types.rpc_server_family)
    (rpc : Configuration.rpc) ~block_production config tx_container ctx dir =
  generic_websocket_dispatch
    config
    tx_container
    ctx
    dir
    "/private/ws"
    (dispatch_private_websocket rpc_server_family ~block_production rpc)

let directory ~rpc_server_family ?delegate_health_check_to rpc validation config
    tx_container backend dir =
  dir |> version |> configuration config
  |> health_check ?delegate_to:delegate_health_check_to
  |> dispatch_public
       rpc_server_family
       rpc
       validation
       config
       tx_container
       backend
  |> dispatch_websocket_public
       rpc_server_family
       rpc
       validation
       config
       tx_container
       backend

let private_directory ~rpc_server_family rpc config tx_container backend
    ~block_production =
  Evm_directory.empty config.experimental_features.rpc_server
  |> version
  |> dispatch_private
       rpc_server_family
       rpc
       config
       tx_container
       backend
       ~block_production
  |> dispatch_websocket_private
       rpc_server_family
       rpc
       config
       tx_container
       backend
       ~block_production
