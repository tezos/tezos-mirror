(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [start config] starts the tx-pool. The [config] represents the 
    Rollup_node rpc module and the address of the smart rollup. *)
val start : (module Rollup_node.S) * string -> unit tzresult Lwt.t
