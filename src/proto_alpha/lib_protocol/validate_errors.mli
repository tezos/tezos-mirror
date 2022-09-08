(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Errors that may arise while validating a consensus operation. *)
module Consensus : sig
  type consensus_operation_kind =
    | Preendorsement
    | Endorsement
    | Grandparent_endorsement

  (** Errors for preendorsements and endorsements. *)
  type error +=
    | Zero_frozen_deposits of Signature.Public_key_hash.t
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
    | Wrong_consensus_operation_branch of {
        kind : consensus_operation_kind;
        expected : Block_hash.t;
        provided : Block_hash.t;
      }
    | Wrong_payload_hash_for_consensus_operation of {
        kind : consensus_operation_kind;
        expected : Block_payload_hash.t;
        provided : Block_payload_hash.t;
      }
    | Unexpected_preendorsement_in_block
    | Unexpected_endorsement_in_block
    | Preendorsement_round_too_high of {
        block_round : Round.t;
        provided : Round.t;
      }
    | Wrong_slot_used_for_consensus_operation of {
        kind : consensus_operation_kind;
      }
    | Conflicting_consensus_operation of {kind : consensus_operation_kind}
    | Conflicting_dal_slot_availability of {
        endorser : Signature.Public_key_hash.t;
      }
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
    | Too_many_proposals
    | Already_proposed of {proposal : Protocol_hash.t}
    | Conflict_too_many_proposals of {
        max_allowed : int;
        count_previous_blocks : int;
        count_current_block : int;
        count_operation : int;
        conflicting_operations : Operation_hash.t list;
      }
    | Conflict_already_proposed of {
        proposal : Protocol_hash.t;
        conflicting_operation : Operation_hash.t;
      }
    | Conflicting_dictator_proposals of Operation_hash.t
    | Testnet_dictator_multiple_proposals
    | Testnet_dictator_conflicting_operation
    | Proposals_from_unregistered_delegate of Signature.Public_key_hash.t
    | (* Ballot errors *)
        Ballot_for_wrong_proposal of {
        current : Protocol_hash.t;
        submitted : Protocol_hash.t;
      }
    | Already_submitted_a_ballot
    | Conflicting_ballot of {conflicting_operation : Operation_hash.t}
    | Ballot_from_unregistered_delegate of Signature.Public_key_hash.t
end

(** Errors that may arise while validating an anonymous operation. *)
module Anonymous : sig
  type denunciation_kind = Preendorsement | Endorsement | Block

  type error +=
    | Invalid_activation of {pkh : Ed25519.Public_key_hash.t}
    | Conflicting_activation of Ed25519.Public_key_hash.t * Operation_hash.t
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
        delegate : Signature.Public_key_hash.t;
        level : Level.t;
        hash : Operation_hash.t;
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
    | Conflicting_nonce_revelation
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
    | Conflicting_drain of {delegate : Signature.Public_key_hash.t}
end

(** Errors that may arise while validating a manager operation. *)
module Manager : sig
  type error +=
    | Manager_restriction of Signature.Public_key_hash.t * Operation_hash.t
    | Inconsistent_sources
    | Inconsistent_counters
    | Incorrect_reveal_position
    | Insufficient_gas_for_manager
    | Gas_quota_exceeded_init_deserialize
    | Tx_rollup_feature_disabled
    | Sc_rollup_feature_disabled
    | Zk_rollup_feature_disabled
end

type error += Failing_noop_error

module Block : sig
  type error +=
    | Not_enough_endorsements of {required : int; provided : int}
    | Inconsistent_validation_passes_in_block of {
        expected : int;
        provided : int;
      }
end
