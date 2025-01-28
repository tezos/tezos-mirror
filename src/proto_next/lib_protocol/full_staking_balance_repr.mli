(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t

val init :
  own_frozen:Tez_repr.t ->
  staked_frozen:Tez_repr.t ->
  delegated:Tez_repr.t ->
  current_level:Level_repr.t ->
  t

val encoding : t Data_encoding.t

(** The weight of a delegate used for voting rights. *)
val voting_weight : t -> Int64.t tzresult

val apply_slashing : percentage:Percentage.t -> t -> t

(** The delegate's own frozen funds. *)
val own_frozen : t -> Tez_repr.t

(** The total frozen funds from all external stakers.

    Does not take the [limit_of_staking_over_baking] into account. *)
val staked_frozen : t -> Tez_repr.t

(** The total delegated funds from all delegators.

    Not adjusted considering overdelegation / overstaking. *)
val current_delegated : t -> Tez_repr.t

(** Returns the minimal value taken by {!current_delegated} during the current cycle.

    This only takes into account the value of {!current_delegated} in
    between blocks, not in the middle of applying one block's
    operations. In other words, it returns the minimum at the end of
    any block from the first block of the current cycle up until the
    [current_level].

    This is the value used for the delegated part when computing the
    baking rights. This has replaced the old snapshot system, and
    still prevents delegators from briefly increasing their balance at
    a well chosen time in order to generate more baking rights. *)
val min_delegated_in_cycle :
  cycle_eras:Level_repr.cycle_eras ->
  current_level:Level_repr.t ->
  t ->
  Tez_repr.t

(** Sum of [own_frozen] and [staked_frozen]. *)
val total_frozen : t -> Tez_repr.t tzresult

(** Sum of [own_frozen], [staked_frozen], and [current_delegated]. *)
val current_total : t -> Tez_repr.t tzresult

(** The portion of {!staked_frozen} that actually counts as staking
    when computing baking rights, considering both the global and the
    delegate's [limit_of_staking_over_baking].

    It is equal to the minimum of:

    - {!staked_frozen}

    - {!own_frozen} scaled by the delegate's [limit_of_staking_over_baking]

    - {!own_frozen} scaled by the global [limit_of_staking_over_baking] *)
val allowed_staked_frozen :
  adaptive_issuance_global_limit_of_staking_over_baking:int ->
  delegate_limit_of_staking_over_baking_millionth:int32 ->
  t ->
  Tez_repr.t

(** Computes [(num, den)] representing the ratio of [own_frozen] over
    [own_frozen + allowed_staked_frozen].

    If [allowed_staked_frozen] is zero, returns [(1L, 1L)].

    If [own_frozen] is zero, returns [(0L, 1L)]. *)
val own_ratio :
  adaptive_issuance_global_limit_of_staking_over_baking:int ->
  delegate_limit_of_staking_over_baking_millionth:int32 ->
  t ->
  int64 * int64

val has_minimal_frozen_stake : minimal_frozen_stake:Tez_repr.t -> t -> bool

val has_minimal_stake_to_be_considered : minimal_stake:Tez_repr.t -> t -> bool

val remove_delegated :
  cycle_eras:Level_repr.cycle_eras ->
  current_level:Level_repr.t ->
  amount:Tez_repr.t ->
  t ->
  t tzresult

val remove_own_frozen : amount:Tez_repr.t -> t -> t tzresult

val remove_staked_frozen : amount:Tez_repr.t -> t -> t tzresult

val add_delegated :
  cycle_eras:Level_repr.cycle_eras ->
  current_level:Level_repr.t ->
  amount:Tez_repr.t ->
  t ->
  t tzresult

val add_own_frozen : amount:Tez_repr.t -> t -> t tzresult

val add_staked_frozen : amount:Tez_repr.t -> t -> t tzresult

module Internal_for_tests_and_RPCs : sig
  val min_delegated_and_level :
    cycle_eras:Level_repr.cycle_eras ->
    current_level:Level_repr.t ->
    t ->
    Tez_repr.t * Level_repr.t

  val last_modified_level : t -> Level_repr.t

  val previous_min : t -> (Tez_repr.t * Level_repr.t) option

  val init_raw :
    own_frozen:Tez_repr.t ->
    staked_frozen:Tez_repr.t ->
    delegated:Tez_repr.t ->
    last_modified_level:Level_repr.t ->
    previous_min:(Tez_repr.t * Level_repr.t) option ->
    t
end
