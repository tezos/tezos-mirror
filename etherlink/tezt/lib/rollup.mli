(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Michelson type to use when originating the EVM rollup. *)
val evm_type : string

(** [next_rollup_node_level ~sc_rollup_node ~client] bakes a new L1 block using
    [client] and wait for [sc_rollup_node] to have processed it. *)
val next_rollup_node_level :
  sc_rollup_node:Sc_rollup_node.t -> client:Client.t -> int Lwt.t
