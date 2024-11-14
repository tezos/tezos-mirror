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

open Tezos_rpc
open Rpc_encodings

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

let health_check_service =
  Service.get_service
    ~description:"Assess the health of the RPC server"
    ~query:Query.empty
    ~output:Data_encoding.empty
    Path.(root / "health_check")

type error += Node_is_bootstrapping

let () =
  register_error_kind
    `Temporary
    ~id:"node_is_bootstrapping"
    ~title:"Node is bootstrapping"
    ~description:
      "Node is bootstrapping and result from the RPC servers can be outdated"
    Data_encoding.empty
    (function Node_is_bootstrapping -> Some () | _ -> None)
    (fun () -> Node_is_bootstrapping)

let client_version =
  Format.sprintf
    "%s/%s-%s/%s/ocamlc.%s"
    "octez-evm-node"
    (Tezos_version.Version.to_string
       Tezos_version_value.Current_git_info.octez_evm_node_version)
    Tezos_version_value.Current_git_info.abbreviated_commit_hash
    Stdlib.Sys.os_type
    Stdlib.Sys.ocaml_version

let version dir =
  Directory.register0 dir version_service (fun () () ->
      Lwt.return_ok client_version)

let configuration config dir =
  Directory.register0 dir configuration_service (fun () () ->
      let open Configuration in
      (* Hide some parts of the configuration. *)
      let hidden = "hidden" in
      let kernel_execution =
        Configuration.{config.kernel_execution with preimages = hidden}
      in
      let sequencer =
        Option.map
          (fun (sequencer_config : sequencer) ->
            {
              sequencer_config with
              sequencer = Client_keys.sk_uri_of_string hidden;
            })
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
          Option.map
            (fun _ -> Uri.of_string hidden)
            config.proxy.evm_node_endpoint
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

      Lwt.return_ok
        (Data_encoding.Json.construct
           ~include_default_fields:`Always
           (Configuration.encoding hidden)
           config))

let health_check ?delegate_to dir =
  let handler =
    match delegate_to with
    | None ->
        fun () () ->
          let open Lwt_result_syntax in
          let* () =
            fail_when (Metrics.is_bootstrapping ()) Node_is_bootstrapping
          in
          return_unit
    | Some evm_node_endpoint ->
        fun () () ->
          Rollup_services.call_service
            ~keep_alive:false
            ~base:evm_node_endpoint
            ~media_types:[Media_type.json]
            health_check_service
            ()
            ()
            ()
  in
  Directory.register0 dir health_check_service handler

(* The node can either take a single request or multiple requests at
   once. *)
type 'a batched_request = Singleton of 'a | Batch of 'a list

let request_encoding kind =
  Data_encoding.(
    union
      [
        case
          ~title:"singleton"
          (Tag 0)
          kind
          (function Singleton i -> Some i | _ -> None)
          (fun i -> Singleton i);
        case
          ~title:"batch"
          (Tag 1)
          (list kind)
          (function Batch i -> Some i | _ -> None)
          (fun i -> Batch i);
      ])

let dispatch_service ~path =
  Service.post_service
    ~query:Query.empty
    ~input:(request_encoding JSONRPC.request_encoding)
    ~output:(request_encoding JSONRPC.response_encoding)
    path

let get_block_by_number ~full_transaction_object block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty n) =
    Rollup_node_rpc.block_param_to_block_number (Block_parameter block_param)
  in
  Rollup_node_rpc.Block_storage.nth_block ~full_transaction_object n

let get_block_receipts block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty n) =
    Rollup_node_rpc.block_param_to_block_number (Block_parameter block_param)
  in
  Rollup_node_rpc.Block_storage.block_receipts n

let get_transaction_from_index block index
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  match block.Ethereum_types.transactions with
  | TxHash l -> (
      match List.nth_opt l index with
      | None -> return_none
      | Some hash -> Rollup_node_rpc.Block_storage.transaction_object hash)
  | TxFull l -> return @@ List.nth_opt l index

let block_transaction_count block =
  Ethereum_types.quantity_of_z @@ Z.of_int
  @@
  match block.Ethereum_types.transactions with
  | TxHash l -> List.length l
  | TxFull l -> List.length l

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
  (* block count can be bounded in configuration *)
  let block_count =
    match Configuration.(config.fee_history.max_count) with
    | None -> block_count
    | Some count -> Z.(min (of_int count) block_count)
  in
  let* nb_latest = Backend_rpc.Block_storage.current_block_number () in
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
    if newest_block.number = nb_latest then Backend_rpc.base_fee_per_gas ()
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
    if block_count = Z.zero || block_parameter = Block_parameter.Number Qty.zero
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
  | Error e ->
      let msg = Format.asprintf "%a" pp_print_trace e in
      rpc_error (Rpc_errors.internal_error msg)

let dispatch_request (rpc : Configuration.rpc) (config : Configuration.t)
    ((module Backend_rpc : Services_backend_sig.S), _)
    ({method_; parameters; id} : JSONRPC.request) : JSONRPC.response Lwt.t =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  let*! value =
    match map_method_name ~restrict:rpc.restricted_rpcs method_ with
    | Unknown ->
        Prometheus.Counter.inc_one (Metrics.Rpc.method_ "unknown") ;
        Lwt.return_error (Rpc_errors.method_not_found method_)
    | Unsupported ->
        Prometheus.Counter.inc_one (Metrics.Rpc.method_ "unsupported") ;
        Lwt.return_error (Rpc_errors.method_not_supported method_)
    | Disabled ->
        Prometheus.Counter.inc_one (Metrics.Rpc.method_ "disabled") ;
        Lwt.return_error (Rpc_errors.method_disabled method_)
    (* Ethereum JSON-RPC API methods we support *)
    | Method (method_rpc, module_) -> (
        Prometheus.Counter.inc_one (Metrics.Rpc.method_ method_) ;
        match method_rpc with
        | Accounts.Method ->
            let f (_ : unit option) = rpc_ok [] in
            build ~f module_ parameters
        | Network_id.Method ->
            let f (_ : unit option) =
              let open Lwt_result_syntax in
              let* (Qty chain_id) = Backend_rpc.chain_id () in
              rpc_ok (Z.to_string chain_id)
            in
            build ~f module_ parameters
        | Chain_id.Method ->
            let f (_ : unit option) =
              let* chain_id = Backend_rpc.chain_id () in
              rpc_ok chain_id
            in
            build ~f module_ parameters
        | Get_balance.Method ->
            let f (address, block_param) =
              let* balance = Backend_rpc.balance address block_param in
              rpc_ok balance
            in
            build_with_input ~f module_ parameters
        | Get_storage_at.Method ->
            let f (address, position, block_param) =
              let* value =
                Backend_rpc.storage_at address position block_param
              in
              rpc_ok value
            in
            build_with_input ~f module_ parameters
        | Block_number.Method ->
            let f (_ : unit option) =
              let* block_number =
                Backend_rpc.Block_storage.current_block_number ()
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
                Backend_rpc.Block_storage.block_by_hash
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
              let* code = Backend_rpc.code address block_param in
              rpc_ok code
            in
            build_with_input ~f module_ parameters
        | Gas_price.Method ->
            let f (_ : unit option) =
              let* base_fee = Backend_rpc.base_fee_per_gas () in
              rpc_ok base_fee
            in
            build ~f module_ parameters
        | Get_transaction_count.Method ->
            let f (address, block_param) =
              match block_param with
              | Ethereum_types.Block_parameter.(Block_parameter Pending) ->
                  let* nonce = Tx_pool.nonce address in
                  rpc_ok nonce
              | _ ->
                  let* nonce = Backend_rpc.nonce address block_param in
                  let nonce = Option.value ~default:Qty.zero nonce in
                  rpc_ok nonce
            in
            build_with_input ~f module_ parameters
        | Get_block_transaction_count_by_hash.Method ->
            let f block_hash =
              let* block =
                Backend_rpc.Block_storage.block_by_hash
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
                Backend_rpc.Block_storage.transaction_receipt tx_hash
              in
              rpc_ok receipt
            in
            build_with_input ~f module_ parameters
        | Get_transaction_by_hash.Method ->
            let f tx_hash =
              let* transaction_object =
                let* transaction_object = Tx_pool.find tx_hash in
                match transaction_object with
                | Some transaction_object -> return_some transaction_object
                | None -> Backend_rpc.Block_storage.transaction_object tx_hash
              in
              rpc_ok transaction_object
            in
            build_with_input ~f module_ parameters
        | Get_transaction_by_block_hash_and_index.Method ->
            let f (block_hash, Qty index) =
              let* block =
                Backend_rpc.Block_storage.block_by_hash
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
              let f tx_raw =
                let txn = Ethereum_types.hex_to_bytes tx_raw in
                let* is_valid =
                  if config.experimental_features.node_transaction_validation
                  then Validate.is_tx_valid (module Backend_rpc) txn
                  else Backend_rpc.is_tx_valid txn
                in
                match is_valid with
                | Error err ->
                    let*! () =
                      Tx_pool_events.invalid_transaction ~transaction:tx_raw
                    in
                    rpc_error (Rpc_errors.transaction_rejected err None)
                | Ok (Either.Left transaction_object) -> (
                    let* (Qty balance) =
                      Backend_rpc.balance
                        transaction_object.from
                        (Block_parameter.Block_parameter Latest)
                    in
                    let total_cost =
                      let (Qty gas) = transaction_object.gas in
                      let (Qty gas_price) = transaction_object.gasPrice in
                      let (Qty value) = transaction_object.value in
                      Z.add (Z.mul gas gas_price) value
                    in
                    if total_cost > balance then
                      rpc_error
                        (Rpc_errors.transaction_rejected
                           "Not enough funds"
                           None)
                    else
                      let* tx_hash = Tx_pool.add transaction_object txn in
                      match tx_hash with
                      | Ok tx_hash -> rpc_ok tx_hash
                      | Error reason ->
                          rpc_error
                            (Rpc_errors.transaction_rejected reason None))
                | Ok (Either.Right _) ->
                    rpc_error
                      (Rpc_errors.internal_error
                         "Transaction validation in the kernel is deprecated")
              in
              build_with_input ~f module_ parameters
        | Eth_call.Method ->
            let f (call, block_param, state_override) =
              let* call_result =
                Backend_rpc.simulate_call
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
                  rpc_error
                  @@ Rpc_errors.transaction_rejected
                       "execution reverted"
                       (Some reason)
              | Error reason ->
                  rpc_error (Rpc_errors.transaction_rejected reason None)
            in
            build_with_input ~f module_ parameters
        | Get_estimate_gas.Method ->
            let f (call, _) =
              let* result = Backend_rpc.estimate_gas call in
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
              let* txpool_content = Tx_pool.get_tx_pool_content () in
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
              let*! trace = Backend_rpc.trace_transaction hash config in
              process_trace_result trace
            in
            build_with_input ~f module_ parameters
        | Eth_fee_history.Method ->
            let f (Qty block_count, newest_block, _reward_percentile) =
              if block_count = Z.zero then
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
              let* coinbase = Backend_rpc.coinbase () in
              rpc_ok coinbase
            in
            build ~f module_ parameters
        | Trace_call.Method ->
            let f (((call, block), config) : Tracer_types.call_input) =
              let*! trace = Backend_rpc.trace_call call block config in
              process_trace_result trace
            in
            build_with_input ~f module_ parameters
        | _ ->
            Stdlib.failwith "The pattern matching of methods is not exhaustive")
  in
  Lwt.return JSONRPC.{value; id}

let dispatch_private_request (rpc : Configuration.rpc)
    (_config : Configuration.t)
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
    match map_method_name ~restrict:rpc.restricted_rpcs method_ with
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
        let f (timestamp : Time.Protocol.t option) =
          let open Lwt_result_syntax in
          let timestamp = Option.value timestamp ~default:(Misc.now ()) in
          let* nb_transactions =
            Block_producer.produce_block ~force:true ~timestamp
          in
          rpc_ok (Ethereum_types.quantity_of_z @@ Z.of_int nb_transactions)
        in
        build ~f module_ parameters
    | Method (Produce_proposal.Method, _)
      when block_production <> `Threshold_encryption ->
        unsupported ()
    | Method (Produce_proposal.Method, module_) ->
        let f (timestamp : Time.Protocol.t option) =
          let open Lwt_result_syntax in
          let timestamp = Option.value timestamp ~default:(Misc.now ()) in
          let* _submitted =
            Threshold_encryption_proposals_handler.submit_next_proposal
              timestamp
          in
          rpc_ok ()
        in
        build ~f module_ parameters
    | Method (Inject_transaction.Method, module_) ->
        let open Lwt_result_syntax in
        let f (transaction_object, raw_txn) =
          let* tx_hash = Tx_pool.add transaction_object raw_txn in
          match tx_hash with
          | Ok _tx_hash -> rpc_ok ()
          | Error reason ->
              rpc_error (Rpc_errors.transaction_rejected reason None)
        in
        build_with_input ~f module_ parameters
    | Method (Durable_state_value.Method, module_) ->
        let f path =
          let open Lwt_result_syntax in
          let*? path =
            Option.to_result
              ~none:[error_of_fmt "missing params, please provide a path"]
              path
          in
          let* state = Backend_rpc.Reader.get_state () in
          let* value = Backend_rpc.Reader.read state path in
          rpc_ok value
        in
        build ~f module_ parameters
    | Method (Durable_state_subkeys.Method, module_) ->
        let f path =
          let open Lwt_result_syntax in
          let* state = Backend_rpc.Reader.get_state () in
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
          let* block = Backend_rpc.replay block_number in
          rpc_ok block
        in
        build ~f module_ parameters
    | _ -> Stdlib.failwith "The pattern matching of methods is not exhaustive"
  in
  return JSONRPC.{value; id}

let can_process_batch size = function
  | Configuration.Limit l -> size <= l
  | Unlimited -> true

let generic_dispatch (rpc : Configuration.rpc) config ctx dir path
    dispatch_request =
  Directory.register0 dir (dispatch_service ~path) (fun () input ->
      let open Lwt_result_syntax in
      match input with
      | Singleton request ->
          let*! response = dispatch_request config ctx request in
          return (Singleton response)
      | Batch requests ->
          let process =
            if can_process_batch (List.length requests) rpc.batch_limit then
              dispatch_request config ctx
            else fun req ->
              let value =
                Error Rpc_errors.(invalid_request "too many requests in batch")
              in
              Lwt.return JSONRPC.{value; id = req.id}
          in
          let*! outputs = List.map_s process requests in
          return (Batch outputs))

let dispatch_public (rpc : Configuration.rpc) config ctx dir =
  generic_dispatch rpc config ctx dir Path.root (dispatch_request rpc)

let dispatch_private (rpc : Configuration.rpc) ~block_production config ctx dir
    =
  generic_dispatch
    rpc
    config
    ctx
    dir
    Path.(add_suffix root "private")
    (dispatch_private_request rpc ~block_production)

let directory ?delegate_health_check_to rpc config
    ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address) =
  Directory.empty |> version |> configuration config
  |> health_check ?delegate_to:delegate_health_check_to
  |> dispatch_public
       rpc
       config
       ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address)

let private_directory rpc config
    ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address) =
  Directory.empty |> version
  |> dispatch_private
       rpc
       config
       ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address)

let call (type input output)
    (module R : Rpc_encodings.METHOD
      with type input = input
       and type output = output) ~keep_alive ~evm_node_endpoint (input : input)
    =
  let open Lwt_result_syntax in
  let* response =
    Rollup_services.call_service
      ~keep_alive
      ~base:evm_node_endpoint
      (dispatch_service ~path:Resto.Path.root)
      ()
      ()
      (Singleton
         JSONRPC.
           {
             method_ = R.method_;
             parameters =
               Some (Data_encoding.Json.construct R.input_encoding input);
             id = None;
           })
  in
  match response with
  | Singleton {value = Ok value; _} | Batch [{value = Ok value; _}] ->
      return (Data_encoding.Json.destruct R.output_encoding value)
  | Singleton {value = Error err; _} | Batch [{value = Error err; _}] ->
      failwith
        "Request failed with error %s"
        Data_encoding.Json.(
          to_string
            (construct (JSONRPC.error_encoding Data_encoding.Json.encoding) err))
  | Batch l ->
      failwith "request: unexpected number of responses (%d)" List.(length l)
