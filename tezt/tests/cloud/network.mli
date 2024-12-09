(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type public =
  [ `Mainnet
  | `Ghostnet
  | `Weeklynet of
    string
    (* date of the genesis block of the current weeklynet;
       typically it is last wednesday. *) ]

type t = [public | `Sandbox]

(** ["mainnet" | "ghostnet" | "weeklynet-%s" | "sandbox"] *)
val to_string : t -> string

(** Known protocol used by the network *)
val default_protocol : t -> Protocol.t

(** Endpoint publicly available with RPC opened *)
val public_rpc_endpoint : public -> Endpoint.t

(** URL of the tzinit snapshot to download.
    You can't use it by itself. Add ["/full"] or ["/rolling"] suffix. *)
val snapshot_service : public -> string

(** Argument to give to the --network option of `octez-node config init`. *)
val to_octez_network_options : public -> string

(** Some node to connect to when bootstrapping your node *)
val default_bootstrap : public -> string

val default_dal_bootstrap : public -> string

(** Get current HEAD level via [rpc get /chains/main/blocks/block/header/shell] *)
val get_level : Endpoint.t -> int Lwt.t

(** Expected proof of work level when generating identity *)
val expected_pow : t -> float

(** Associate delegate aliases with octez version used (according to tzkt).
    Only works for mainnet and ghosnet. Return empty table for other networks. *)
val versions : t -> (string, string) Hashtbl.t Lwt.t

(** List of delegates as [(alias, public key hash, public key)] known by tzkt
    for a given network.
    [?accounts] is only used for [`Sandbox] network
    (and the only source of data for this kind of network) *)
val delegates :
  ?accounts:Account.key list ->
  t ->
  (string option * string * string) list Lwt.t

(** Table of delegates as [(pkh,alias)] known by tzkt for a given network
    [?accounts] is only used for [`Sandbox] network
    (and the only source of data for this kind of network) *)
val aliases :
  ?accounts:Account.key list -> t -> (string, string) Hashtbl.t Lwt.t
