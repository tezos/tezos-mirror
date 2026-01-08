(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let evm_type =
  "or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes"

let next_rollup_node_level ~sc_rollup_node ~client =
  let* l1_level = Client.bake_for_and_wait_level ~keys:[] client in
  Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node l1_level
