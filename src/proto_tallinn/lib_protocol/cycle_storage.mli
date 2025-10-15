(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module hosts cycle functions which depend on the context. *)

(** Current cycle, that is, the cycle of the current level. *)
val current : Raw_context.t -> Cycle_repr.t

(** The oldest cycle on which the context keeps sampling data. *)
val oldest_cycle_with_sampling_data : Raw_context.t -> Cycle_repr.t

(** The cycle whose sampling data should be cleared when [new_cycle]
    starts.

    Returns [None] when the computed cycle would be negative: this
    means there are no cycles that should be cleared when [new_cycle]
    starts. *)
val cycle_to_clear_of_sampling_data :
  new_cycle:Cycle_repr.t -> Cycle_repr.t option

(** Highest cycle whose unstake requests are currently finalizable.

    Returns [None] when the computed cycle would be negative: this
    means there are no cycles yet whose unstake request can be
    finalized. *)
val greatest_unstake_finalizable_cycle : Raw_context.t -> Cycle_repr.t option
