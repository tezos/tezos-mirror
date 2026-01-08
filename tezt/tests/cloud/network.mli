(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type public =
  [ `Mainnet
  | `Ghostnet
  | `Shadownet
  | `Nextnet of string
    (* date of the genesis block of the current weeklynet;
       format is YYYYMMDD *)
  | `Weeklynet of string
    (* date of the genesis block of the current weeklynet;
       typically it is last wednesday.
       format is YYYY-MM-DD *)
  | `Seoulnet
  | `Tallinnnet ]

type t = [public | `Sandbox]

(** ["mainnet" | "ghostnet" | "shadownet" | "seoulnet" | "tallinnnet" | "nextnet-%s" | "weeklynet-%s" | "sandbox"] *)
val to_string : [< t] -> string

val public_encoding : public Data_encoding.t

val encoding : t Data_encoding.t

val is_public : t -> bool

val to_public : t -> public

(** Parse the given [string] into an available network option. *)
val parse : string -> t option

(** Known protocol used by the network *)
val default_protocol : t -> Protocol.t

(** Block time value for the network *)
val block_time : t -> int

val next_protocol : t -> Protocol.t

(** Endpoint publicly available with RPC opened *)
val public_rpc_endpoint : public -> Endpoint.t

(** URL of the tzinit snapshot to download.
    You can't use it by itself. Add ["/full"] or ["/rolling"] suffix. *)
val snapshot_service : public -> string

(** Argument to give to the --network option of `octez-node config init`. *)
val to_octez_network_options : t -> string

(** Some node to connect to when bootstrapping your node *)
val default_bootstrap : public -> string

val default_dal_bootstrap : public -> string

(** Get current HEAD level via [rpc get /chains/main/blocks/block/header/shell] *)
val get_level : Endpoint.t -> int Lwt.t

(** Expected proof of work level when generating identity *)
val expected_pow : t -> float

(** Associate delegate aliases with octez version used (according to tzkt).
    Only works for mainnet and ghostnet. Return empty table for other networks.

    Returns [None] when [Tzkt] did not respond or respond with an unexpected
    output.
*)
val versions : t -> (string, string) Hashtbl.t option Lwt.t

(** List of delegates as [(alias, public key hash, public key)] known
    by tzkt for a given network. [?accounts] is only used for
    [`Sandbox] network (and the only source of data for this kind of
    network).

    Returns [None] when [Tzkt] did not respond or respond with an unexpected
    output. *)
val delegates :
  ?accounts:Account.key list ->
  t ->
  (string option * string * string) list option Lwt.t

(** Table of delegates as [(pkh,alias)] known by tzkt for a given network
    [?accounts] is only used for [`Sandbox] network
    (and the only source of data for this kind of network).

    Returns [None] when [Tzkt] did not respond or respond with a bad
    output. *)
val aliases :
  ?accounts:Account.key list -> t -> (string, string) Hashtbl.t option Lwt.t
