(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [check_all_bakers_attest_at_level ctxt level] checks that at the given
    level, all bakers were allowed (and expected) to attest. *)
val check_all_bakers_attest_at_level : Raw_context.t -> Level_repr.t -> bool

val consensus_threshold :
  Raw_context.t -> Level_repr.t -> (Raw_context.t * int64) tzresult Lwt.t

val consensus_committee :
  Raw_context.t -> Level_repr.t -> (Raw_context.t * int64) tzresult Lwt.t

(** Returns true IFF the first level of the given cycle is greater
    than or equal to the activation level of all-bakers-attest.

    This is used by some mechanisms that must do something consistent
    accross the whole cycle, such as cycle rewards or missed
    attestations tracking.

    Remark: the activation level will always be set to the first level
    of a cycle anyway. *)
val is_all_bakers_attest_enabled_for_cycle :
  Raw_context.t -> Cycle_repr.t -> bool
