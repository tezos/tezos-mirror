(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [build_rpc_directory ~node_version] builds the Tezos RPC directory for the
    rpc process. RPCs handled here are not forwarded to the node.
*)
val build_rpc_directory :
  Tezos_version.Node_version.t -> unit Tezos_rpc.Directory.t
