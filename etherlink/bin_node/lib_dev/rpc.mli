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
  evm_node_endpoint:Uri.t ->
  evm_node_private_endpoint:Uri.t ->
  config:Configuration.t ->
  unit ->
  unit tzresult Lwt.t

(** [spawn_main ~exposed_port ~redirection_endpoint ~data_dir] spawn an RPC process that listens
    to [exposed_port], connects to [protected_endpoint] and [private_endpoint] when specified,
    and reads the config in [data_dir]. *)
val spawn_main :
  exposed_port:int ->
  protected_endpoint:Uri.t ->
  ?private_endpoint:Uri.t ->
  data_dir:string ->
  unit ->
  Rpc_server.finalizer Lwt.t
