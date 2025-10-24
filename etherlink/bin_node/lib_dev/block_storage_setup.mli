(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [enable ~keep_alive ~timeout ?evm_node_endpoint store] setup the EVM node to
    rely on data external to the kernel (its local store, or an upstream EVM
    node endpoint) to validate blueprints of blocks which are not compatible
    with the new block storage relying on the SQLite store. *)
val enable :
  keep_alive:bool ->
  timeout:float ->
  ?evm_node_endpoint:Uri.t ->
  Evm_store.t ->
  unit
