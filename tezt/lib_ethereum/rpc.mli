(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [block_number evm_node] calls [eth_blockNumber]. *)
val block_number : Evm_node.t -> int32 Lwt.t
