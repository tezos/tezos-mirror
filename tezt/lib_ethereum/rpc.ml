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

module Syntax = struct
  let ( let*@ ) x f =
    let* r = x in
    match r with
    | Ok x -> f x
    | Error err ->
        Test.fail "'let*@' expected a valid response but got %a" pp_error err

  let ( let*@? ) x f =
    let* r = x in
    match r with
    | Ok _ -> Test.fail "'let*@?' expected an error but got a valid response"
    | Error err -> f err
end
