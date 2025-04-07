(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** The RPC mode exposes a read-only JSON RPC API endpoint, which reuses an
    existing data-dir. It can be used in conjunction with a node running in
    sequencer or observer mode. If [evm_node_private_endpoint] is provided transactions
    are sent forward to the private endpoint of its conjunct node. *)
val main :
  data_dir:string ->
  evm_node_endpoint:Uri.t ->
  ?evm_node_private_endpoint:Uri.t ->
  config:Configuration.t ->
  unit ->
  unit tzresult Lwt.t
