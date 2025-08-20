(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [check_all_bakers_attest_at_level ctxt level] checks that at the given
    level, all bakers were allowed (and expected) to attest. *)
val check_all_bakers_attest_at_level : Raw_context.t -> Level_repr.t -> bool

val consensus_threshold : Raw_context.t -> Level_repr.t -> int

val consensus_committee : Raw_context.t -> Level_repr.t -> int
