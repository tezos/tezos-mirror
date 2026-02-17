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

(** Cumulative status of a baker for a given published level, accumulated
    across all attested levels in the attestation window.

    The variants are ordered by priority: when merging a per-level status
    into an existing cumulative status, the higher-priority variant wins. *)
type baker_window_status =
  | With_DAL of bool array
      (** Baker is in the DAL committee and sent a DAL payload at at least one
          attested level. Carries per-slot cumulative attestation bits (OR'd
          across the window). *)
  | Without_DAL
      (** Baker is in the DAL committee and sent at least one attestation
          operation but never with a DAL payload. *)
  | In_committee
      (** Baker is in the DAL committee, but never sent an attestation operation
          during the window. *)
  | Out_of_committee
      (** Baker is not in the DAL committee and sent at least one attestation
          operation. *)

type per_level_info = {
  level : int;
  published_commitments : (int, commitment_info) Hashtbl.t;
  attested_commitments : bool array array;
      (** Per-lag attestation data decoded from block metadata.
          [attested_commitments.(lag_index).(slot_index)] is [true] when
          slot [slot_index] was newly confirmed as attested for the
          published level corresponding to [lag_index]. *)
  etherlink_operator_balance_sum : Tez.t;
  echo_rollup_fetched_data : (int, int) Hashtbl.t;
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

(** [get ~first_level ~attestation_lags ~dal_node_producers ~number_of_slots
    ~infos ~cumulative_protocol_attestations ~cumulative_baker_window_status
    infos_per_level metrics] updates the [metrics] statistics.

    [attestation_lags] is the list of lags at which a DAL slot can be attested
    after publication. The maximum lag determines when a published level's
    attestation window closes.

    [cumulative_protocol_attestations] maps each published level to the per-slot
    cumulative attestation status accumulated across all levels in the
    attestation window.

    [cumulative_baker_window_status] maps each published level to a per-baker
    table of {!baker_window_status}, recording committee membership,
    attestation behavior, and per-slot attestation bits across the full
    attestation window. *)
val get :
  first_level:int ->
  attestation_lags:int list ->
  dal_node_producers:'a list ->
  number_of_slots:int ->
  infos:(int, per_level_info) Hashtbl.t ->
  cumulative_protocol_attestations:(int, bool array) Hashtbl.t ->
  cumulative_baker_window_status:
    (int, (public_key_hash, baker_window_status) Hashtbl.t) Hashtbl.t ->
  per_level_info ->
  t ->
  t
