(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type error = {code : int; message : string; data : string option}

let pp_error ppf {code; message; data} =
  let pp_data ppf = function
    | None -> Format.fprintf ppf "null"
    | Some data -> Format.fprintf ppf "%s" data
  in
  Format.fprintf
    ppf
    "{code: %d, message: %S; data: %a}"
    code
    message
    pp_data
    data

type block_param =
  | Earliest
  | Latest
  | Pending
  | Number of int
  | Block_number of {number : int; require_canonical : bool}
  | Block_hash of {hash : string; require_canonical : bool}

let block_param_to_json = function
  | Earliest -> `String "earliest"
  | Latest -> `String "latest"
  | Pending -> `String "pending"
  | Number number -> `String (string_of_int number)
  | Block_number {number; require_canonical} ->
      `O
        [
          ("blockNumber", `String (string_of_int number));
          ("requireCanonical", `Bool require_canonical);
        ]
  | Block_hash {hash; require_canonical} ->
      `O
        [
          ("blockHash", `String hash);
          ("requireCanonical", `Bool require_canonical);
        ]

let decode_or_error decode json =
  match JSON.(json |-> "error" |> as_opt) with
  | Some json ->
      let code = JSON.(json |-> "code" |> as_int) in
      let message = JSON.(json |-> "message" |> as_string) in
      let data = JSON.(json |-> "data" |> as_opt |> Option.map as_string) in
      Error {code; message; data}
  | None -> Ok (decode json)

let eth_call_obj ~to_ ~data = `O [("to", `String to_); ("data", `String data)]

module Request = struct
  open Evm_node

  let eth_blockNumber = {method_ = "eth_blockNumber"; parameters = `A []}

  let generic_blockNumber = {method_ = "tez_blockNumber"; parameters = `A []}

  let eth_getBlockByNumber ~block ~full_tx_objects =
    {
      method_ = "eth_getBlockByNumber";
      parameters = `A [`String block; `Bool full_tx_objects];
    }

  let eth_getBlockByHash ~block ~full_tx_objects =
    {
      method_ = "eth_getBlockByHash";
      parameters = `A [`String block; `Bool full_tx_objects];
    }

  let produceBlock ?with_delayed_transactions ?timestamp () =
    let with_delayed_transactions =
      match with_delayed_transactions with
      | Some false -> [("with_delayed_transactions", `Bool false)]
      | _ -> []
    in
    let timestamp =
      match timestamp with
      | Some timestamp -> [("timestamp", `String timestamp)]
      | None -> []
    in
    let parameters = `O (with_delayed_transactions @ timestamp) in
    {method_ = "produceBlock"; parameters}

  let stateValue ?block path =
    let parameters =
      match block with
      | None -> `A [`String path]
      | Some block -> `A [`String path; `String block]
    in
    {method_ = "stateValue"; parameters}

  let stateSubkeys ?block path =
    let parameters =
      match block with
      | None -> `A [`String path]
      | Some block -> `A [`String path; `String block]
    in
    {method_ = "stateSubkeys"; parameters}

  let produceProposal ?timestamp () =
    let parameters =
      match timestamp with None -> `Null | Some timestamp -> `String timestamp
    in
    {method_ = "produceProposal"; parameters}

  let eth_sendRawTransaction ~raw_tx =
    {method_ = "eth_sendRawTransaction"; parameters = `A [`String raw_tx]}

  let eth_sendRawTransactionSync ~raw_tx ~timeout ~block =
    {
      method_ = "eth_sendRawTransactionSync";
      parameters =
        `A [`String raw_tx; `String timeout; block_param_to_json block];
    }

  let eth_getTransactionReceipt ~tx_hash =
    {method_ = "eth_getTransactionReceipt"; parameters = `A [`String tx_hash]}

  let eth_estimateGas ~eth_call ~block =
    {
      method_ = "eth_estimateGas";
      parameters = `A [`O eth_call; block_param_to_json block];
    }

  let eth_getTransactionCount ~address ~block =
    {
      method_ = "eth_getTransactionCount";
      parameters = `A [`String address; `String block];
    }

  let eth_getTransactionByHash ~transaction_hash =
    {
      method_ = "eth_getTransactionByHash";
      parameters = `A [`String transaction_hash];
    }

  let eth_getCode ~address ~block =
    {
      method_ = "eth_getCode";
      parameters = `A [`String address; block_param_to_json block];
    }

  let eth_getChainId = {method_ = "eth_chainId"; parameters = `A []}

  let tez_getChainFamily ~chain_id =
    {
      method_ = "tez_chainFamily";
      parameters = `A [`String (string_of_int chain_id)];
    }

  let tez_getTransactionGasInfo ~transaction_hash =
    {
      method_ = "tez_getTransactionGasInfo";
      parameters = `A [`String transaction_hash];
    }

  let net_version = {method_ = "net_version"; parameters = `A []}

  let tez_kernelVersion = {method_ = "tez_kernelVersion"; parameters = `Null}

  let tez_kernelRootHash = {method_ = "tez_kernelRootHash"; parameters = `Null}

  let eth_call ~block ~to_ ~data =
    {
      method_ = "eth_call";
      parameters = `A [eth_call_obj ~to_ ~data; block_param_to_json block];
    }

  let get_balance ~address ~block =
    {
      method_ = "eth_getBalance";
      parameters = `A [`String address; block_param_to_json block];
    }

  let get_storage_at ~address ~pos ~block =
    {
      method_ = "eth_getStorageAt";
      parameters = `A [`String address; `String pos; block_param_to_json block];
    }

  let eth_maxPriorityFeePerGas =
    {method_ = "eth_maxPriorityFeePerGas"; parameters = `Null}

  let replayBlock blockNumber =
    {
      method_ = "tez_replayBlock";
      parameters = `String (string_of_int blockNumber);
    }

  let txpool_content = {method_ = "txpool_content"; parameters = `A []}

  let trace_transaction ~transaction_hash ?tracer ?tracer_config () =
    let config =
      match tracer with
      | Some tracer -> [("tracer", `String tracer)]
      | None -> []
    in
    let config =
      match tracer_config with
      | None -> config
      | Some tracer_config -> config @ tracer_config
    in
    let parameters =
      match config with
      | [] -> `A [`String transaction_hash]
      | config -> `A [`String transaction_hash; `O config]
    in
    {method_ = "debug_traceTransaction"; parameters}

  let trace_call ~block ~to_ ~data ?tracer ?tracer_config () =
    let config =
      match tracer with
      | Some tracer -> [("tracer", `String tracer)]
      | None -> []
    in
    let config =
      match tracer_config with
      | None -> config
      | Some tracer_config -> config @ tracer_config
    in
    let parameters =
      match config with
      | [] -> `A [eth_call_obj ~to_ ~data; block_param_to_json block]
      | config ->
          `A [eth_call_obj ~to_ ~data; block_param_to_json block; `O config]
    in
    {method_ = "debug_traceCall"; parameters}

  let trace_block ~block ?tracer ?tracer_config () =
    let config =
      match tracer with
      | Some tracer -> [("tracer", `String tracer)]
      | None -> [("tracer", `String "callTracer")]
    in
    let config =
      match tracer_config with
      | None -> config
      | Some tracer_config -> config @ tracer_config
    in
    let parameters =
      match config with
      | [] -> `A [block_param_to_json block]
      | config -> `A [block_param_to_json block; `O config]
    in
    {method_ = "debug_traceBlockByNumber"; parameters}

  let eth_feeHistory ~block_count ~newest_block =
    {
      method_ = "eth_feeHistory";
      parameters =
        `A
          [
            `String block_count;
            `String newest_block;
            (* rewards query, not relevant to etherlink *)
            `A [`Float 0.; `Float 1.; `Float 2.; `Float 3.];
          ];
    }

  let coinbase = {method_ = "eth_coinbase"; parameters = `Null}

  type address = Single of string | Multi of string list

  type logs_input_param = {
    address : address option;
    topics : string list option;
  }

  let address_to_param = function
    | None -> ("address", `A [])
    | Some (Single address) -> ("address", `String address)
    | Some (Multi addresses) ->
        ("address", `A (List.map (fun address -> `String address) addresses))

  let topics_to_param = function
    | None -> ("topics", `A [])
    | Some topics ->
        ("topics", `A (List.map (fun topic -> `String topic) topics))

  type subscription_kind =
    | NewHeads
    | Logs of logs_input_param option
    | NewPendingTransactions
    | Syncing
    | NewIncludedTransactions
    | NewPreconfirmedReceipts

  let param_of_sub_kind = function
    | NewHeads -> `A [`String "newHeads"]
    | Logs (Some {address; topics}) ->
        `A
          [
            `String "logs"; `O [address_to_param address; topics_to_param topics];
          ]
    | Logs None -> `A [`String "logs"]
    | NewPendingTransactions -> `A [`String "newPendingTransactions"]
    | Syncing -> `A [`String "syncing"]
    | NewIncludedTransactions -> `A [`String "tez_newIncludedTransactions"]
    | NewPreconfirmedReceipts -> `A [`String "tez_newPreconfirmedReceipts"]

  let eth_subscribe ~kind =
    {method_ = "eth_subscribe"; parameters = param_of_sub_kind kind}

  let eth_unsubscribe ~id =
    {method_ = "eth_unsubscribe"; parameters = `A [`String id]}

  let eth_getLogs ?from_block ?to_block ?address ?topics ?block_hash () =
    let parse_topic = function
      | [] -> `Null
      | [t] -> `String t
      | l -> `A (List.map (fun s -> `String s) l)
    in
    let parameters : JSON.u =
      `A
        [
          `O
            (address_to_param address
            :: (Option.fold
                  ~none:[]
                  ~some:(fun f -> [("fromBlock", block_param_to_json f)])
                  from_block
               @ Option.fold
                   ~none:[]
                   ~some:(fun t -> [("toBlock", block_param_to_json t)])
                   to_block
               @ Option.fold
                   ~none:[]
                   ~some:(fun t -> [("topics", `A (List.map parse_topic t))])
                   topics
               @ Option.fold
                   ~none:[]
                   ~some:(fun t -> [("blockHash", `String t)])
                   block_hash));
        ]
    in
    {method_ = "eth_getLogs"; parameters}
end

let net_version ?websocket evm_node =
  let* json = Evm_node.jsonrpc ?websocket evm_node Request.net_version in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_string)) json)

let get_chain_id ?websocket evm_node =
  let* json = Evm_node.jsonrpc ?websocket evm_node Request.eth_getChainId in
  return (decode_or_error (fun json -> JSON.(json |-> "result" |> as_int)) json)

let get_chain_family ?websocket evm_node chain_id =
  let* json =
    Evm_node.jsonrpc ?websocket evm_node (Request.tez_getChainFamily ~chain_id)
  in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_string)) json)

let get_transaction_by_hash ?websocket ~transaction_hash evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getTransactionByHash ~transaction_hash)
  in
  return
    (decode_or_error
       (fun json ->
         JSON.(
           json |-> "result" |> fun json ->
           if is_null json then None
           else Some (Transaction.transaction_object_of_json json)))
       json)

let get_code ?websocket ~address evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getCode ~address ~block:Latest)
  in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_string)) json)

let get_logs ?websocket ?from_block ?to_block ?address ?topics ?block_hash
    evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getLogs
         ?from_block
         ?to_block
         ?address
         ?topics
         ?block_hash
         ())
  in
  return
    (decode_or_error
       (fun json ->
         JSON.(
           json |-> "result" |> as_list
           |> List.map (fun json -> Transaction.logs_of_json json)))
       json)

let block_number ?websocket evm_node =
  let* json = Evm_node.jsonrpc ?websocket evm_node Request.eth_blockNumber in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_int32)) json)

let block_number_opt ?websocket evm_node =
  let* json = Evm_node.jsonrpc ?websocket evm_node Request.eth_blockNumber in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> as_opt |> Option.map as_int32))
       json)

let generic_block_number ?websocket evm_node =
  let* json =
    Evm_node.jsonrpc ?websocket evm_node Request.generic_blockNumber
  in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_int32)) json)

let generic_block_number_opt ?websocket evm_node =
  let* json =
    Evm_node.jsonrpc ?websocket evm_node Request.generic_blockNumber
  in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> as_opt |> Option.map as_int32))
       json)

let get_block_by_number ?websocket ?(full_tx_objects = false) ~block evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getBlockByNumber ~block ~full_tx_objects)
  in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> Block.of_json))
       json)

let get_block_by_hash ?websocket ?(full_tx_objects = false) ~block evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getBlockByHash ~block ~full_tx_objects)
  in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> Block.of_json))
       json)

let get_gas_price ?websocket evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      {method_ = "eth_gasPrice"; parameters = `Null}
  in
  return JSON.(json |-> "result" |> as_string |> Int32.of_string)

let subscribe ?websocket ~kind evm_node =
  let* json =
    Evm_node.jsonrpc ?websocket evm_node (Request.eth_subscribe ~kind)
  in
  return JSON.(json |-> "result" |> as_string)

let unsubscribe ?websocket ~id evm_node =
  let* json =
    Evm_node.jsonrpc ?websocket evm_node (Request.eth_unsubscribe ~id)
  in
  return JSON.(json |-> "result" |> as_bool)

module Syntax = struct
  let ( let*@ ) x f =
    let* r = x in
    match r with
    | Ok x -> f x
    | Error err ->
        Test.fail "'let*@' expected a valid response but got %a" pp_error err

  let ( let*@! ) x f =
    let* r = x in
    match r with
    | Ok (Some x) -> f x
    | Ok None -> Test.fail "'let*@!' expected a Some but got None"
    | Error err ->
        Test.fail "'let*@!' expected a valid response but got %a" pp_error err

  let ( let*@? ) x f =
    let* r = x in
    match r with
    | Ok _ -> Test.fail "'let*@\\?' expected an error but got a valid response"
    | Error err -> f err
end

let produce_block ?websocket ?with_delayed_transactions ?timestamp evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      ~private_:true
      evm_node
      (Request.produceBlock ?with_delayed_transactions ?timestamp ())
  in
  return
  @@ decode_or_error
       (fun json -> Evm_node.extract_result json |> JSON.as_int)
       json

let produce_proposal ?websocket ?timestamp evm_node =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      ~private_:true
      evm_node
      (Request.produceProposal ?timestamp ())
  in
  return
  @@ decode_or_error
       (fun json ->
         Evm_node.extract_result json |> fun json ->
         if JSON.is_null json then () else ())
       json

let state_value ?websocket evm_node ?block path =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      ~private_:true
      evm_node
      (Request.stateValue ?block path)
  in
  return
  @@ decode_or_error
       (fun json -> Evm_node.extract_result json |> JSON.as_string_opt)
       json

let state_subkeys ?websocket evm_node ?block path =
  let* json =
    Evm_node.jsonrpc
      ?websocket
      ~private_:true
      evm_node
      (Request.stateSubkeys ?block path)
  in
  return
  @@ decode_or_error
       (fun json ->
         Evm_node.extract_result json |> JSON.as_list_opt |> function
         | Some l -> Some (List.map JSON.as_string l)
         | None -> None)
       json

let send_raw_transaction ?websocket ~raw_tx evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_sendRawTransaction ~raw_tx)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let eth_send_raw_transaction_sync ?websocket ~raw_tx ?(timeout = 0)
    ?(block = Latest) evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_sendRawTransactionSync
         ~raw_tx
         ~timeout:(string_of_int timeout)
         ~block)
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response
         |> Transaction.transaction_receipt_of_json)
       response

let get_transaction_receipt ?websocket ~tx_hash evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getTransactionReceipt ~tx_hash)
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response
         |> JSON.as_opt
         |> Option.map Transaction.transaction_receipt_of_json)
       response

let get_transaction_gas_info ?websocket ~tx_hash evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.tez_getTransactionGasInfo ~transaction_hash:tx_hash)
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response
         |> JSON.as_opt
         |> Option.map (fun result ->
                let inclusion_gas =
                  JSON.(result |-> "inclusion_gas" |> as_int64)
                in
                let execution_gas =
                  JSON.(result |-> "execution_gas" |> as_int64)
                in
                (`Inclusion_gas inclusion_gas, `Execution_gas execution_gas)))
       response

let estimate_gas ?websocket eth_call ?(block = Latest) evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_estimateGas ~eth_call ~block)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_int64)
       response

let get_transaction_count ?websocket ?(block = "latest") ~address evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_getTransactionCount ~address ~block)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_int64)
       response

let tez_kernelVersion ?websocket evm_node =
  let* response =
    Evm_node.jsonrpc ?websocket evm_node Request.tez_kernelVersion
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let tez_kernelRootHash ?websocket evm_node =
  let* response =
    Evm_node.jsonrpc ?websocket evm_node Request.tez_kernelRootHash
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string_opt)
       response

let call ?websocket ~to_ ~data ?(block = Latest) evm_node =
  let* response =
    Evm_node.jsonrpc ?websocket evm_node (Request.eth_call ~block ~to_ ~data)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let get_balance ?websocket ~address ?(block = Latest) evm_node =
  let* response =
    Evm_node.jsonrpc ?websocket evm_node (Request.get_balance ~address ~block)
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response |> JSON.as_string |> Wei.of_string)
       response

let get_storage_at ?websocket ~address ?(block = Latest) ~pos evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.get_storage_at ~address ~pos ~block)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let get_max_priority_fee_per_gas ?websocket evm_node =
  let* json =
    Evm_node.jsonrpc ?websocket evm_node Request.eth_maxPriorityFeePerGas
  in
  return JSON.(json |-> "result" |> as_int32)

let replay_block ?websocket blockNumber evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      ~private_:true
      evm_node
      (Request.replayBlock blockNumber)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> Block.of_json)
       response

type txpool_slot = {address : string; transactions : (int64 * JSON.t) list}

let txpool_content ?websocket evm_node =
  let* response = Evm_node.jsonrpc ?websocket evm_node Request.txpool_content in
  let parse txpool field =
    let open JSON in
    let pool = txpool |-> field in

    (* `|->` returns `Null if the field does not exists, and `Null is
       interpreted as the empty list by `as_object`. As such, we must ensure the
       field exists. *)
    if is_null pool then Test.fail "%s must exists" field
    else
      pool |> as_object
      |> List.map (fun (address, transactions) ->
             let transactions =
               transactions |> as_object
               |> List.map (fun (nonce, tx) -> (Int64.of_string nonce, tx))
             in
             {address; transactions})
  in
  return
  @@ decode_or_error
       (fun response ->
         let txpool = Evm_node.extract_result response in
         (parse txpool "pending", parse txpool "queued"))
       response

let trace_transaction ?websocket ~transaction_hash ?tracer ?tracer_config
    evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.trace_transaction ~transaction_hash ?tracer ?tracer_config ())
  in
  return
  @@ decode_or_error (fun response -> Evm_node.extract_result response) response

let trace_call ?websocket ~block ~to_ ~data ?tracer ?tracer_config evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.trace_call ~block ~to_ ~data ?tracer ?tracer_config ())
  in
  return
  @@ decode_or_error (fun response -> Evm_node.extract_result response) response

let trace_block ?websocket ~block ?tracer ?tracer_config evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.trace_block ~block ?tracer ?tracer_config ())
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_list)
       response

type fee_history = {
  oldest_block : int64;
  base_fee_per_gas : int64 list;
  gas_used_ratio : float list;
}

let fee_history ?websocket block_count newest_block evm_node =
  let* response =
    Evm_node.jsonrpc
      ?websocket
      evm_node
      (Request.eth_feeHistory ~block_count ~newest_block)
  in

  let decode_fee_history_result response =
    let oldest_block =
      JSON.(response |-> "result" |-> "oldestBlock" |> as_int64)
    in
    let base_fee_per_gas =
      JSON.(
        response |-> "result" |-> "baseFeePerGas" |> as_list
        |> List.map as_int64)
    in
    let gas_used_ratio =
      JSON.(
        response |-> "result" |-> "gasUsedRatio" |> as_list |> List.map as_float)
    in
    {oldest_block; base_fee_per_gas; gas_used_ratio}
  in

  return @@ decode_or_error decode_fee_history_result response

let coinbase ?websocket evm_node =
  let* response = Evm_node.jsonrpc ?websocket evm_node Request.coinbase in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let configuration evm_node =
  Curl.get (Evm_node.endpoint evm_node ^ "/configuration") |> Runnable.run
