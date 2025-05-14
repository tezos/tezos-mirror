(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t

val create : P2p.config * P2p_limits.t -> (t, tztrace) result Lwt.t

val shutdown : t -> unit Lwt.t

(** [build_rpc_directory p2p_node] builds a Tezos RPC
    directory for the P2P node by gathering all the
    subdirectories. [p2p_node] contains all
    information required to build such a directory. *)
val build_rpc_directory : t -> unit Tezos_rpc.Directory.t
