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

  let eth_getBlockByNumber ~block ~full_tx_objects =
    {
      method_ = "eth_getBlockByNumber";
      parameters = `A [`String block; `Bool full_tx_objects];
    }

  let produceBlock ?timestamp () =
    let parameters =
      match timestamp with None -> `Null | Some timestamp -> `String timestamp
    in
    {method_ = "produceBlock"; parameters}

  let stateValue path = {method_ = "stateValue"; parameters = `String path}

  let stateSubkeys path = {method_ = "stateSubkeys"; parameters = `String path}

  let produceProposal ?timestamp () =
    let parameters =
      match timestamp with None -> `Null | Some timestamp -> `String timestamp
    in
    {method_ = "produceProposal"; parameters}

  let eth_sendRawTransaction ~raw_tx =
    {method_ = "eth_sendRawTransaction"; parameters = `A [`String raw_tx]}

  let eth_getTransactionReceipt ~tx_hash =
    {method_ = "eth_getTransactionReceipt"; parameters = `A [`String tx_hash]}

  let eth_estimateGas ~eth_call ~block =
    {method_ = "eth_estimateGas"; parameters = `A [`O eth_call; `String block]}

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
end

let net_version evm_node =
  let* json = Evm_node.call_evm_rpc evm_node Request.net_version in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_string)) json)

let get_transaction_by_hash ~transaction_hash evm_node =
  let* json =
    Evm_node.call_evm_rpc
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

let get_code ~address evm_node =
  let* json =
    Evm_node.call_evm_rpc evm_node (Request.eth_getCode ~address ~block:Latest)
  in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_string)) json)

let block_number evm_node =
  let* json = Evm_node.call_evm_rpc evm_node Request.eth_blockNumber in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_int32)) json)

let block_number_opt evm_node =
  let* json = Evm_node.call_evm_rpc evm_node Request.eth_blockNumber in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> as_opt |> Option.map as_int32))
       json)

let get_block_by_number ?(full_tx_objects = false) ~block evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      (Request.eth_getBlockByNumber ~block ~full_tx_objects)
  in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> Block.of_json))
       json)

let get_gas_price evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      {method_ = "eth_gasPrice"; parameters = `Null}
  in
  return JSON.(json |-> "result" |> as_string |> Int32.of_string)

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

let produce_block ?timestamp evm_node =
  let* json =
    Evm_node.call_evm_rpc
      ~private_:true
      evm_node
      (Request.produceBlock ?timestamp ())
  in
  return
  @@ decode_or_error
       (fun json -> Evm_node.extract_result json |> JSON.as_int)
       json

let produce_proposal ?timestamp evm_node =
  let* json =
    Evm_node.call_evm_rpc
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

let state_value evm_node path =
  let* json =
    Evm_node.call_evm_rpc ~private_:true evm_node (Request.stateValue path)
  in
  return
  @@ decode_or_error
       (fun json -> Evm_node.extract_result json |> JSON.as_string_opt)
       json

let state_subkeys evm_node path =
  let* json =
    Evm_node.call_evm_rpc ~private_:true evm_node (Request.stateSubkeys path)
  in
  return
  @@ decode_or_error
       (fun json ->
         Evm_node.extract_result json |> JSON.as_list_opt |> function
         | Some l -> Some (List.map JSON.as_string l)
         | None -> None)
       json

let send_raw_transaction ~raw_tx evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (Request.eth_sendRawTransaction ~raw_tx)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let get_transaction_receipt ~tx_hash evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (Request.eth_getTransactionReceipt ~tx_hash)
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response
         |> JSON.as_opt
         |> Option.map Transaction.transaction_receipt_of_json)
       response

let estimate_gas eth_call evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      (Request.eth_estimateGas ~eth_call ~block:"latest")
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_int64)
       response

let get_transaction_count ?(block = "latest") ~address evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      (Request.eth_getTransactionCount ~address ~block)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_int64)
       response

let tez_kernelVersion evm_node =
  let* response = Evm_node.call_evm_rpc evm_node Request.tez_kernelVersion in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let tez_kernelRootHash evm_node =
  let* response = Evm_node.call_evm_rpc evm_node Request.tez_kernelRootHash in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string_opt)
       response

let call ~to_ ~data ?(block = Latest) evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (Request.eth_call ~block ~to_ ~data)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let get_balance ~address ?(block = Latest) evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (Request.get_balance ~address ~block)
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response |> JSON.as_string |> Wei.of_string)
       response

let get_storage_at ~address ?(block = Latest) ~pos evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (Request.get_storage_at ~address ~pos ~block)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let get_max_priority_fee_per_gas evm_node =
  let* json = Evm_node.call_evm_rpc evm_node Request.eth_maxPriorityFeePerGas in
  return JSON.(json |-> "result" |> as_int32)

let replay_block blockNumber evm_node =
  let* response =
    Evm_node.call_evm_rpc
      ~private_:true
      evm_node
      (Request.replayBlock blockNumber)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> Block.of_json)
       response

type txpool_slot = {address : string; transactions : (int64 * JSON.t) list}

let txpool_content evm_node =
  let* response = Evm_node.call_evm_rpc evm_node Request.txpool_content in
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

let trace_transaction ~transaction_hash ?tracer ?tracer_config evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      (Request.trace_transaction ~transaction_hash ?tracer ?tracer_config ())
  in
  return
  @@ decode_or_error (fun response -> Evm_node.extract_result response) response

let trace_call ~block ~to_ ~data ?tracer ?tracer_config evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      (Request.trace_call ~block ~to_ ~data ?tracer ?tracer_config ())
  in
  return
  @@ decode_or_error (fun response -> Evm_node.extract_result response) response

type fee_history = {
  oldest_block : int64;
  base_fee_per_gas : int64 list;
  gas_used_ratio : float list;
}

let fee_history block_count newest_block evm_node =
  let* response =
    Evm_node.(
      call_evm_rpc evm_node (Request.eth_feeHistory ~block_count ~newest_block))
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

let coinbase evm_node =
  let* response = Evm_node.call_evm_rpc evm_node Request.coinbase in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let configuration evm_node =
  Curl.get (Evm_node.endpoint evm_node ^ "/configuration") |> Runnable.run
