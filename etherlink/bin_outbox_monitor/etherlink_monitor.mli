(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [start db ~evm_node_endpoint ~rollup_node_endpoint ~l1_node_endpoint] starts
    the Etherlink withdrawal monitor by subscribing to the relevant logs on
    [evm_node_endpoint] and storing information in the database [db]. *)
val start :
  Db.t ->
  evm_node_endpoint:Uri.t ->
  rollup_node_endpoint:Uri.t ->
  l1_node_endpoint:Uri.t ->
  unit tzresult Lwt.t
