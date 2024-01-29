(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

val get_smart_rollup_address :
  evm_node_endpoint:Uri.t ->
  Tezos_crypto.Hashed.Smart_rollup_address.t tzresult Lwt.t

val get_blueprint :
  evm_node_endpoint:Uri.t ->
  Ethereum_types.quantity ->
  Blueprint_types.payload tzresult Lwt.t

val register : Evm_context.t -> unit Directory.t -> unit Directory.t
