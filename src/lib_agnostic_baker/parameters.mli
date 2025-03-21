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

val log_config : base_dir:string -> Tezos_base.Internal_event_config.t

(** Status of a protocol, based on Manifest/Product_octez/Protocol. A
    protocol is considered as [Active] while it is running on a network,
    and thus, have dedicated binaries. Otherwise, the protocol is
    [Frozen] as not running anymore and no associated binaries.

    Warning, it is needed to update status for each new protocol added.
*)
type status = Active | Frozen

val pp_status : Format.formatter -> status -> unit

val status_encoding : status t

val protocol_short_hash : Protocol_hash.t -> string

val protocol_status : Protocol_hash.t -> status
