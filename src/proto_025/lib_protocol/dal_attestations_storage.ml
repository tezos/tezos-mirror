(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Raw_context

let record_number_of_attested_shards ctxt ~delegate ~attested_level attestation
    committee_level_to_shard_count =
  let c = (constants ctxt).dal in
  let slot_accountability = Dal.slot_accountability ctxt in
  let slot_accountability =
    Dal_attestations_repr.Accountability.record_number_of_attested_shards
      slot_accountability
      ~number_of_slots:c.number_of_slots
      ~attestation_lag:c.attestation_lag
      ~lags:c.attestation_lags
      ~delegate
      ~attested_level
      attestation
      committee_level_to_shard_count
  in
  Dal.record_slot_accountability ctxt slot_accountability

let attestations = Dal.attestations

let record_attestation = Dal.record_attestation

(** [committee_level_of ctxt ~published_level] computes the committee level
    (a.k.a. shard assignment level) for a given [published_level].

    The committee level is defined as
    [published_level + attestation_lag - 1], where [attestation_lag] is
    retrieved from the DAL parameters that were active at [published_level].
    This makes the function protocol-change-aware: if a protocol migration
    changed [attestation_lag], the correct value for the given
    [published_level] is used (via {!Dal_storage.parameters}). *)
let committee_level_of ctxt ~published_level =
  let open Lwt_result_syntax in
  let* dal_params = Dal_storage.parameters ctxt published_level in
  return (Raw_level_repr.add published_level (dal_params.attestation_lag - 1))
