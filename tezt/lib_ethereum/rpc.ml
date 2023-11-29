(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let block_number evm_node =
  let* json =
    Evm_node.call_evm_rpc
      evm_node
      {method_ = "eth_blockNumber"; parameters = `A []}
  in
  return JSON.(json |-> "result" |> as_string |> Int32.of_string)
