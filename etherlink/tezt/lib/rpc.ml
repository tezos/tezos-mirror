(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error = {code : int; message : string}

let pp_error ppf {code; message} =
  Format.fprintf ppf "{code: %d, message: %S}" code message

let decode_or_error decode json =
  match JSON.(json |-> "error" |> as_opt) with
  | Some json ->
      let code = JSON.(json |-> "code" |> as_int) in
      let message = JSON.(json |-> "message" |> as_string) in
      Error {code; message}
  | None -> Ok (decode json)

let block_number evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      {method_ = "eth_blockNumber"; parameters = `A []}
  in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> as_string |> Int32.of_string))
       json)

let get_block_by_number ?(full_tx_objects = false) ~block evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      {
        method_ = "eth_getBlockByNumber";
        parameters = `A [`String block; `Bool full_tx_objects];
      }
  in
  return
    (decode_or_error
       (fun json -> JSON.(json |-> "result" |> Block.of_json))
       json)

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

let produce_block_request ?timestamp () =
  let parameters =
    match timestamp with None -> `Null | Some timestamp -> `String timestamp
  in
  Evm_node.{method_ = "produceBlock"; parameters}

let produce_block ?timestamp evm_node =
  let* json =
    Evm_node.call_evm_rpc
      ~private_:true
      evm_node
      (produce_block_request ?timestamp ())
  in
  return JSON.(json |-> "result" |> as_string |> Int32.of_string)

let inject_upgrade_request payload =
  Evm_node.{method_ = "injectUpgrade"; parameters = `String payload}

let inject_upgrade ~payload evm_node =
  let* _json =
    Evm_node.call_evm_rpc
      ~private_:true
      evm_node
      (inject_upgrade_request payload)
  in
  return ()

let send_raw_transaction_request raw_tx =
  Evm_node.
    {method_ = "eth_sendRawTransaction"; parameters = `A [`String raw_tx]}

let send_raw_transaction ~raw_tx evm_node =
  let* response =
    Evm_node.call_evm_rpc evm_node (send_raw_transaction_request raw_tx)
  in
  return
  @@ decode_or_error
       (fun response -> Evm_node.extract_result response |> JSON.as_string)
       response

let get_transaction_receipt ~tx_hash evm_node =
  let* response =
    Evm_node.call_evm_rpc
      evm_node
      {method_ = "eth_getTransactionReceipt"; parameters = `A [`String tx_hash]}
  in
  return
  @@ decode_or_error
       (fun response ->
         Evm_node.extract_result response
         |> JSON.as_opt
         |> Option.map Transaction.transaction_receipt_of_json)
       response
