(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let block_to_string bytes =
  let decoded = Ethereum_types.block_from_rlp bytes in
  Data_encoding.Json.(
    construct Ethereum_types.block_encoding decoded |> to_string)

let () =
  Octez_smart_rollup_wasm_debugger_plugin.Encodings.register
    "evm.block"
    block_to_string
