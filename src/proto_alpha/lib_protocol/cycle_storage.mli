(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module hosts cycle functions which depend on the context. *)

(** Current cycle, that is, the cycle of the current level. *)
val current : Raw_context.t -> Cycle_repr.t

(** Highest cycle whose unstake requests are currently finalizable.

    Returns [None] when the computed cycle would be negative: this
    means there are no cycles yet whose unstake request can be
    finalized. *)
val greatest_unstake_finalizable_cycle : Raw_context.t -> Cycle_repr.t option
