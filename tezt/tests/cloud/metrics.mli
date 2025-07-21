(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Tezt‑cloud metrics helpers. *)

(** A baker’s public key hash used as a map key. *)
type public_key_hash = PKH of string

(** Information about a single published DAL commitment. *)
type commitment_info = {commitment : string; publisher_pkh : string}

type per_level_info = {
  level : int;
  published_commitments : (int, commitment_info) Hashtbl.t;
  attestations : (public_key_hash, Dal_node_helpers.dal_status) Hashtbl.t;
  attested_commitments : Z.t;
  etherlink_operator_balance_sum : Tez.t;
}

type t

(** Default starting value for metrics. *)
val default : t

(** [aliases] maps delegate PKHs to their human‑readable aliases. *)
val aliases : (string, string) Hashtbl.t

(** [merge_aliases aliases_map] adds new aliases from [aliases_map] into {!aliases}. *)
val merge_aliases : (string, string) Hashtbl.t option -> unit

(** Pretty-printing function for metrics. *)
val pp : bakers:Baker_helpers.baker list -> t -> unit

(** [push ~versions ~cloud metrics] pushes all metrics into Cloud’s Prometheus
    registry, attaching optional [versions] labels for each baker PKH. *)
val push : versions:(string, string) Hashtbl.t -> cloud:Cloud.t -> t -> unit

(** [get ~first_level ~attestation_lag ~dal_node_producers ~number_of_slots
    ~infos infos_per_level metrics] updates the [metrics] statistics. *)
val get :
  first_level:int ->
  attestation_lag:int ->
  dal_node_producers:'a list ->
  number_of_slots:int ->
  infos:(int, per_level_info) Hashtbl.t ->
  per_level_info ->
  t ->
  t
