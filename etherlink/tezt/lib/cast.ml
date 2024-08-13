(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines how tezt tests will interact with cast client.
   Cast is an ethereum client to interact with the node provided by Foundry
   https://book.getfoundry.sh/cast *)

type tx =
  | CallTo of {
      signature : string option;
      arguments : string list;
      address : string;
    }

let spawn arguments = Process.spawn "cast" arguments

let spawn_command_and_read_string ?expect_failure arguments =
  let process = spawn arguments in
  let* data = Process.check_and_read_stdout ?expect_failure process in
  return (String.trim data)

let version () = spawn_command_and_read_string ["--version"]

let cast_transaction ?expect_failure ~source_private_key ?endpoint ?chain_id
    ?nonce ?value ?gas ?gas_price ?priority_fee tx =
  let arguments =
    match tx with
    | CallTo {signature; arguments; address} ->
        address :: (Option.to_list signature @ arguments)
  in
  let options =
    ["--private-key"; source_private_key]
    @ Cli_arg.optional_switch "legacy" (Option.is_none priority_fee)
    @ Cli_arg.optional_arg "rpc-url" Fun.id endpoint
    @ Cli_arg.optional_arg "chain" Int.to_string chain_id
    @ Cli_arg.optional_arg "nonce" Int.to_string nonce
    @ Cli_arg.optional_arg "value" Wei.to_string value
    @ Cli_arg.optional_arg "gas-limit" Int.to_string gas
    @ Cli_arg.optional_arg "priority-gas-price" Int.to_string priority_fee
    @ Cli_arg.optional_arg "gas-price" Int.to_string gas_price
    @ arguments
  in
  spawn_command_and_read_string ?expect_failure ("mktx" :: options)

let craft_tx ~source_private_key ~chain_id ~nonce ~value ~gas ~gas_price
    ?(legacy = true) ~address ?signature ?(arguments = []) () =
  let priority_fee = if legacy then None else Some 1 in
  let tx = CallTo {signature; arguments; address} in
  let* encoded_tx =
    cast_transaction
      ~source_private_key
      ~chain_id
      ~nonce
      ~value
      ~gas
      ~gas_price
      ?priority_fee
      tx
  in
  return (String.sub encoded_tx 2 (String.length encoded_tx - 2))