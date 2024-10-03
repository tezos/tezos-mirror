(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let run tree =
  let open Lwt_syntax in
  return (Evm_node_wasm_runtime.wasm_runtime_id tree)
