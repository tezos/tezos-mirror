(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

val get_blueprint :
  evm_node_endpoint:Uri.t ->
  Ethereum_types.quantity ->
  Blueprint_types.payload tzresult Lwt.t

val register : Evm_context.t -> unit Directory.t -> unit Directory.t
