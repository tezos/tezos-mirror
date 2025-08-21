(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Default endpoint to contact the node. Based on the
    [Octez_node_config.Config_file.default_rpc_port]. *)
val default_node_endpoint : string

(** Default logs path for the agnostic baker. *)
val default_daily_logs_path : string option

(** Number of extra levels to keep the old baker alive before shutting it down.
    This extra time is used to avoid halting the chain in cases such as
    reorganization or high round migration blocks. *)
val extra_levels_for_old_baker : int

(** Status of a protocol, based on [Manifest/Product_octez/Protocol]. A
    protocol is considered as [Active] while it is running on a network.
    Otherwise, the protocol is [Frozen].

    Warning, it is needed to update status for each new protocol added.
*)
type status = Active | Frozen

val pp_status : Format.formatter -> status -> unit

val status_encoding : status t

(** [protocol_status proto_hash] returns whether the given [proto_hash] is
    [Active] or [Frozen]. *)
val protocol_status : Protocol_hash.t -> status
