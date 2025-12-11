(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(**  [select_bakers_at_cycle_end ctxt ~target_cycle] called to select the bakers at the end of a cycle
    and it will precompute the list of round 0 bakers for the [target_cycle].
*)
val select_bakers_at_cycle_end :
  Raw_context.t -> target_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val get_baker :
  Raw_context.t ->
  Level_repr.t ->
  Round_repr.round ->
  (Raw_context.t * Delegate_consensus_key.pk option) tzresult Lwt.t

val reset_credit_for_deactivated_delegates :
  Raw_context.t ->
  Signature.Public_key_hash.t list ->
  Raw_context.t tzresult Lwt.t

val remove_outdated_cycle : Raw_context.t -> Cycle_repr.t -> Raw_context.t Lwt.t
