(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t

val init :
  own_frozen:Tez_repr.t -> staked_frozen:Tez_repr.t -> delegated:Tez_repr.t -> t

val encoding : t Data_encoding.t

(** The weight of a delegate used for voting rights. *)
val voting_weight : t -> Int64.t tzresult

val apply_slashing : percentage:Int_percentage.t -> t -> t

val own_frozen : t -> Tez_repr.t

val staked_frozen : t -> Tez_repr.t

val delegated : t -> Tez_repr.t

(** Sum of [own_frozen] and [staked_frozen]. *)
val total_frozen : t -> Tez_repr.t tzresult

(** Sum of [own_frozen], [staked_frozen], and [delegated]. *)
val total : t -> Tez_repr.t tzresult

(** [own_ratio full_staking_balance] returns [(num, den)] representing the
ratio of [own_frozen] over [total_frozen] for [full_staking_balance].
If [total_frozen] is zero, the returned ratio is [(1L, 1L)]. *)
val own_ratio : t -> Int64.t * Int64.t

val has_minimal_frozen_stake : minimal_frozen_stake:Tez_repr.t -> t -> bool

val has_minimal_stake : minimal_stake:Tez_repr.t -> t -> bool

val remove_delegated : amount:Tez_repr.t -> t -> t tzresult

val remove_own_frozen : amount:Tez_repr.t -> t -> t tzresult

val remove_staked_frozen : amount:Tez_repr.t -> t -> t tzresult

val add_delegated : amount:Tez_repr.t -> t -> t tzresult

val add_own_frozen : amount:Tez_repr.t -> t -> t tzresult

val add_staked_frozen : amount:Tez_repr.t -> t -> t tzresult
