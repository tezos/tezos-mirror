(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Checks whether all bakers are allowed (and expected) to attest
    blocks at the [attested_level].

    Note that when handling a block's attestations, what matters is
    whether all-bakers-attest was active at the level written in the
    attestations' content, not at the level of the block containing
    the attestations (which is one level higher), hence the
    [attested_level] label.

    When handling preattestations, the [attested_level] is the level
    written in the preattestations's content, which is the same as the
    level of the block containing them.
*)
val check_all_bakers_attest_at_level :
  Raw_context.t -> attested_level:Level_repr.t -> bool

(** Returns the minimal required attesting power for a block to be
    valid.

    If all-bakers-attest is not yet active at [attested_level], this
    is the [consensus_threshold_size] (equal to 4667 slots out of the
    7000 available on mainnet as of protocol T).

    If all-bakers-attest is active, this is instead the total baking
    power of all bakers for the cycle of [attested_level], multiplied
    by the [consensus_threshold_size] constant and divided by the
    [consensus_committee_size] constant (approximately 2/3).

    Note that, as in {!check_all_bakers_attest_at_level},
    [attested_level] is the level of the consensus operations of
    interest, which may not be the same as the level of the block
    being validated.
*)
val consensus_threshold :
  Raw_context.t ->
  attested_level:Level_repr.t ->
  (Raw_context.t * int64) tzresult Lwt.t

(** Returns the total attesting power over all bakers that have rights
    at the [attested_level].

    If all-bakers-attest is not yet active at [attested_level], this
    is the [consensus_committee_size] (equal to 7000 on mainnet as of
    protocol T).

    If all-bakers-attest is active, this is instead the total baking
    power of all bakers that have rights for the cycle of
    [attested_level] (so it is identical for all levels in the same
    cycle).

    Note that, as in {!check_all_bakers_attest_at_level},
    [attested_level] is the level of the consensus operations of
    interest, which may not be the same as the level of the block
    being validated.
*)
val consensus_committee :
  Raw_context.t ->
  attested_level:Level_repr.t ->
  (Raw_context.t * int64) tzresult Lwt.t

(** Returns true IFF the first level of the given cycle is greater
    than or equal to the activation level of all-bakers-attest.

    This is used by some mechanisms that must do something consistent
    accross the whole cycle, such as cycle rewards or missed
    attestations tracking.

    Remark: the activation level will always be set to the first level
    of a cycle anyway. *)
val is_all_bakers_attest_enabled_for_cycle :
  Raw_context.t -> Cycle_repr.t -> bool
