(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** SWRR (Stake-Weighted Round Robin) Baker Selection

    This module implements a deterministic baker selection algorithm for
    consensus in Tezos. Unlike the previous alias sampler which uses random
    sampling, SWRR precomputes an ordered sequence of bakers at the end of
    each cycle, ensuring deterministic and fair selection proportional to stake.

    {1 Storage Responsibility}

    This module is responsible for maintaining and accessing the following
    storage fields:
    - [Storage.Stake.Selected_bakers]: precomputed baker arrays per cycle
    - [Storage.Contract.SWRR_credit]: accumulated credits per delegate

    Other modules should NOT access these storage fields directly. All SWRR
    storage operations must go through the functions provided in this interface
    to maintain proper abstraction and encapsulation.

    {1 Algorithm Overview}

    The SWRR algorithm is a weighted round-robin lottery that selects bakers
    for each block in a cycle. The key idea is to maintain a "credit" balance
    for each delegate that accumulates based on their stake.

    At cycle end, for each of [blocks_per_cycle] levels:
    1. Increase all delegates' credits by their stake
    2. Select the delegate with maximum credit to be this level's round 0 baker.
       In case of a tie, one is chosen deterministically.
    3. Subtract the total active stake from the selected delegate's credit

    This creates a "debt" mechanism: delegates with higher stake are selected
    more frequently but accumulate negative credit proportionally, preventing
    monopolization. Over many iterations, this guarantees selection frequency
    proportional to stake.

    {2 Fairness Property}

    A delegate with stake [s] out of total stake [T] receives on average
    [S = (s/T) × blocks_per_cycle] selections. The algorithm ensures fairness
    over time by using arbitrary precision integers (Z.t) for credit tracking,
    preventing any rounding errors or overflow. It ensures that the number of
    selections is between [S-1] and [S+1] for given cycle, which reduce
    variance compare to alias method.

    {2 Credit Persistence}

    Credits persist across cycles to maintain long-term fairness. This means
    that fractional stake weights carry over, ensuring delegates aren't
    disadvantaged by rounding in any particular cycle. Credits are only reset
    when a delegate is deactivated (see [reset_credit_for_deactivated_delegates]).

    {2 Determinism}

    Unlike the alias sampler's probabilistic selection, SWRR produces the same
    baker sequence for all nodes given the same stake distribution. This
    determinism simplifies reasoning about the protocol and eliminates
    randomness-related edge cases.

    {2 Performance}

    Baker selection ([get_baker]) is O(1) using precomputed FallbackArray
    indexed by [(cycle_position + 3 × round) mod array_length]. Precomputation
    happens once per cycle (O(blocks_per_cycle × num_delegates)), not on the
    critical path for block production. *)

(** [select_bakers_at_cycle_end ctxt ~target_cycle] precomputes the list
    of bakers for round 0 of each level in [target_cycle] using SWRR lottery.

    Side effects:
    - Stores selected bakers in [Storage.Stake.Selected_bakers]
    - Updates all delegates' credits in [Storage.Contract.SWRR_credit]
    - Caches result with [Swrr_selected_distribution] (cache index 3)

    Called during cycle finalization from [Delegate_sampler.select_distribution_for_cycle].

    Complexity: O(blocks_per_cycle × num_active_delegates) *)
val select_bakers_at_cycle_end :
  Raw_context.t -> target_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

(** [get_baker ctxt level round] returns the consensus key for the baker
    assigned to [level] and [round].

    Uses precomputed SWRR array with formula:
    idx = (level.cycle_position + 3 * round) mod array_length

    Returns [None] if:
    - SWRR is disabled ([swrr_new_baker_lottery_enable = false])
    - Precomputed data not found (e.g., during protocol migration)
    - Cycle data not yet computed

    Complexity: O(1) - critical path operation *)
val get_baker :
  Raw_context.t ->
  Level_repr.t ->
  Round_repr.round ->
  (Raw_context.t * Delegate_consensus_key.pk option) tzresult Lwt.t

(** [reset_credit_for_deactivated_delegates ctxt deactivated_delegates]
    resets SWRR credits to zero for [deactivated_delegates].

    Ensures fair restart when delegate reactivates.

    Called from [Delegate_cycles.unfreeze_deposits] during cycle finalization,
    before [select_bakers_at_cycle_end] for next cycle. *)
val reset_credit_for_deactivated_delegates :
  Raw_context.t ->
  Signature.Public_key_hash.t list ->
  Raw_context.t tzresult Lwt.t

(** [remove_outdated_cycle ctxt cycle] removes SWRR sampling data for [cycle]
    from both storage and cache.

    This function is essential to prevent unbounded storage growth. SWRR creates
    precomputed baker arrays.
    Without cleanup, storage would grow indefinitely as new cycles are added.

    What is removed:
    - [Storage.Stake.Selected_bakers] for the cycle (FallbackArray of bakers)
    - Cache entry at index 3 (in-memory copy)

    When to call:
    - Called from [Delegate_sampler.clear_outdated_sampling_data]
    - After [preserved_cycles] window (cycles beyond what consensus needs)
    - Typically 5+ cycles old, no longer needed for block validation

    The function uses [remove] not [remove_existing] to handle cases where
    SWRR was disabled during [cycle], making cleanup safe regardless of whether
    data exists. See implementation for details. *)
val remove_outdated_cycle : Raw_context.t -> Cycle_repr.t -> Raw_context.t Lwt.t
