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
    {method_ = "eth_getCode"; parameters = `A [`String address; `String block]}

  let net_version = {method_ = "net_version"; parameters = `A []}

  let tez_kernelVersion = {method_ = "tez_kernelVersion"; parameters = `Null}

  let eth_call ~to_ ~data =
    {
      method_ = "eth_call";
      parameters = `A [eth_call_obj ~to_ ~data; `String "latest"];
    }
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
         JSON.(json |-> "result" |> Transaction.transaction_object_of_json))
       json)

let get_code ~address evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      (Request.eth_getCode ~address ~block:"latest")
  in
  return
    (decode_or_error (fun json -> JSON.(json |-> "result" |> as_string)) json)

let block_number evm_node =
  let* json = Evm_node.call_evm_rpc evm_node Request.eth_blockNumber in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> as_string |> Int32.of_string))
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
    | Ok _ -> Test.fail "'let*@?' expected an error but got a valid response"
    | Error err -> f err
end

let produce_block ?timestamp evm_node =
  let* json =
    Evm_node.call_evm_rpc
      ~private_:true
      evm_node
      (Request.produceBlock ?timestamp ())
  in
  return JSON.(json |-> "result" |> as_string |> Int32.of_string)

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

let get_transaction_count ~address evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      (Request.eth_getTransactionCount ~address ~block:"latest")
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

let call ~to_ ~data evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (Request.eth_call ~to_ ~data)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response
