(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context

(** type used for conflicting operation. *)
type operation_conflict =
  | Operation_conflict of {
      existing : Operation_hash.t;
      new_operation : Operation_hash.t;
    }

(** Errors that may arise while validating a consensus operation. *)
module Consensus : sig
  type consensus_operation_kind =
    | Preattestation
    | Attestation
    | Attestations_aggregate
    | Preattestations_aggregate

  (** Errors for preattestations and attestations. *)
  type error +=
    | Forbidden_delegate of Signature.Public_key_hash.t
    | Consensus_operation_not_allowed
    | Consensus_operation_for_old_level of {
        kind : consensus_operation_kind;
        expected : Raw_level.t;
        provided : Raw_level.t;
      }
    | Consensus_operation_for_future_level of {
        kind : consensus_operation_kind;
        expected : Raw_level.t;
        provided : Raw_level.t;
      }
    | Consensus_operation_for_old_round of {
        kind : consensus_operation_kind;
        expected : Round.t;
        provided : Round.t;
      }
    | Consensus_operation_for_future_round of {
        kind : consensus_operation_kind;
        expected : Round.t;
        provided : Round.t;
      }
    | Wrong_payload_hash_for_consensus_operation of {
        kind : consensus_operation_kind;
        expected : Block_payload_hash.t;
        provided : Block_payload_hash.t;
      }
    | Unexpected_preattestation_in_block
    | Unexpected_attestation_in_block
    | Preattestation_round_too_high of {
        block_round : Round.t;
        provided : Round.t;
      }
    | Wrong_slot_used_for_consensus_operation of {
        kind : consensus_operation_kind;
      }
    | Conflicting_consensus_operation of {
        kind : consensus_operation_kind;
        conflict : operation_conflict;
      }
    | Missing_companion_key_for_bls_dal of Consensus_key.t
    | Aggregate_disabled
    | Aggregate_in_mempool
    | Aggregate_not_implemented
    | Non_bls_key_in_aggregate
    | Public_key_aggregation_failure
    | Unaggregated_eligible_operation of {
        kind : consensus_operation_kind;
        hash : Operation_hash.t;
      }
    | Empty_aggregation_committee
end

(** Errors that may arise while validating a voting operation. *)
module Voting : sig
  type error +=
    | (* Shared voting errors *)
        Wrong_voting_period_index of {
        expected : int32;
        provided : int32;
      }
    | Wrong_voting_period_kind of {
        current : Voting_period.kind;
        expected : Voting_period.kind list;
      }
    | Source_not_in_vote_listings
    | (* Proposals errors *)
        Empty_proposals
    | Proposals_contain_duplicate of {proposal : Protocol_hash.t}
    | Already_proposed of {proposal : Protocol_hash.t}
    | Too_many_proposals of {previous_count : int; operation_count : int}
    | Conflicting_proposals of operation_conflict
    | Testnet_dictator_multiple_proposals
    | Proposals_from_unregistered_delegate of Signature.Public_key_hash.t
    | (* Ballot errors *)
        Ballot_for_wrong_proposal of {
        current : Protocol_hash.t;
        submitted : Protocol_hash.t;
      }
    | Already_submitted_a_ballot
    | Ballot_from_unregistered_delegate of Signature.Public_key_hash.t
    | Conflicting_ballot of operation_conflict
end

(** Errors that may arise while validating an anonymous operation. *)
module Anonymous : sig
  type denunciation_kind = Misbehaviour.kind

  type error +=
    | Invalid_activation of {pkh : Ed25519.Public_key_hash.t}
    | Conflicting_activation of {
        edpkh : Ed25519.Public_key_hash.t;
        conflict : operation_conflict;
      }
    | Invalid_denunciation of denunciation_kind
    | Invalid_double_baking_evidence of {
        hash1 : Block_hash.t;
        level1 : Raw_level.t;
        round1 : Round.t;
        hash2 : Block_hash.t;
        level2 : Raw_level.t;
        round2 : Round.t;
      }
    | Inconsistent_denunciation of {
        kind : denunciation_kind;
        delegate1 : Signature.Public_key_hash.t;
        delegate2 : Signature.Public_key_hash.t;
      }
    | Already_denounced of {
        kind : denunciation_kind;
        delegate : Signature.Public_key_hash.t;
        level : Level.t;
      }
    | Conflicting_denunciation of {
        kind : denunciation_kind;
        conflict : operation_conflict;
      }
    | Too_early_denunciation of {
        kind : denunciation_kind;
        level : Raw_level.t;
        current : Raw_level.t;
      }
    | Outdated_denunciation of {
        kind : denunciation_kind;
        level : Raw_level.t;
        last_cycle : Cycle.t;
      }
    | Invalid_accusation_inconsistent_consensus_slot
    | Invalid_accusation_of_preattestation
    | Too_early_dal_denunciation of {level : Raw_level.t; current : Raw_level.t}
    | Outdated_dal_denunciation of {level : Raw_level.t; last_cycle : Cycle.t}
    | Invalid_shard_index of {given : int; min : int; max : int}
    | Dal_already_denounced of {
        delegate : Signature.Public_key_hash.t;
        level : Raw_level.t;
      }
    | Invalid_accusation_no_dal_content of {
        tb_slot : Slot.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
      }
    | Invalid_accusation_slot_not_attested of {
        tb_slot : Slot.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
      }
    | Invalid_accusation_shard_is_not_trap of {
        delegate : Signature.Public_key_hash.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
        shard_index : int;
      }
    | Invalid_accusation_wrong_shard_owner of {
        delegate : Signature.Public_key_hash.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
        shard_index : int;
        shard_owner : Signature.Public_key_hash.t;
      }
    | Invalid_accusation_slot_not_published of {
        delegate : Signature.Public_key_hash.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
      }
    | Accusation_validity_error_cannot_get_slot_headers of {
        delegate : Signature.Public_key_hash.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
      }
    | Accusation_validity_error_levels_mismatch of {
        delegate : Signature.Public_key_hash.t;
        level : Raw_level.t;
        slot_index : Dal.Slot_index.t;
        accusation_published_level : Raw_level.t;
        store_published_level : Raw_level.t;
      }
    | Conflicting_dal_entrapment of operation_conflict
    | Conflicting_nonce_revelation of operation_conflict
    | Conflicting_vdf_revelation of operation_conflict
    | Drain_delegate_on_unregistered_delegate of Signature.Public_key_hash.t
    | Invalid_drain_delegate_inactive_key of {
        delegate : Signature.Public_key_hash.t;
        consensus_key : Signature.Public_key_hash.t;
        active_consensus_key : Signature.Public_key_hash.t;
      }
    | Invalid_drain_delegate_no_consensus_key of Signature.Public_key_hash.t
    | Invalid_drain_delegate_noop of Signature.Public_key_hash.t
    | Invalid_drain_delegate_insufficient_funds_for_burn_or_fees of {
        delegate : Signature.Public_key_hash.t;
        destination : Signature.Public_key_hash.t;
        min_amount : Tez.t;
      }
    | Conflicting_drain_delegate of {
        delegate : Signature.Public_key_hash.t;
        conflict : operation_conflict;
      }
    | Aggregate_denunciation_not_implemented
end

(** Errors that may arise while validating a manager operation. *)
module Manager : sig
  type error +=
    | Manager_restriction of {
        source : Signature.Public_key_hash.t;
        conflict : operation_conflict;
      }
    | Inconsistent_sources of {
        expected_source : public_key_hash;
        source : public_key_hash;
      }
    | Inconsistent_counters of {
        source : public_key_hash;
        previous_counter : Manager_counter.t;
        counter : Manager_counter.t;
      }
    | Incorrect_reveal_position
    | Missing_bls_proof of {
        kind : Operation_repr.public_key_kind;
        source : public_key_hash;
        public_key : public_key;
      }
    | Incorrect_bls_proof of {
        kind : Operation_repr.public_key_kind;
        public_key : public_key;
        proof : Bls.t;
      }
    | Unused_bls_proof of {
        kind : Operation_repr.public_key_kind;
        source : public_key_hash;
        public_key : public_key;
      }
    | Update_companion_key_not_tz4 of {
        source : public_key_hash;
        public_key : public_key;
      }
    | Insufficient_gas_for_manager
    | Gas_quota_exceeded_init_deserialize
    | Sc_rollup_arith_pvm_disabled
    | Sc_rollup_riscv_pvm_disabled
    | Zk_rollup_feature_disabled
end

type error += Failing_noop_error

module Block : sig
  type error +=
    | Not_enough_attestations of {required : int64; provided : int64}
    | Inconsistent_validation_passes_in_block of {
        expected : int;
        provided : int;
      }
    | Invalid_payload_hash of {
        expected : Block_payload_hash.t;
        provided : Block_payload_hash.t;
      }
    | Locked_round_after_block_round of {
        locked_round : Round.t;
        round : Round.t;
      }
    | Insufficient_locked_round_evidence of {
        total_attesting_power : int64;
        consensus_threshold : int64;
      }
end
