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
