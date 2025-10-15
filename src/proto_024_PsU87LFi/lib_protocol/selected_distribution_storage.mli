(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)
module Selected_distribution_for_cycle : sig
  val identifier_of_cycle : Cycle_repr.cycle -> string

  val init :
    Raw_context.t ->
    Cycle_repr.cycle ->
    (Signature.public_key_hash * Stake_repr.t) list ->
    Raw_context.t tzresult Lwt.t

  val get :
    Raw_context.t ->
    Cycle_repr.cycle ->
    (Raw_context.t * (Signature.public_key_hash * Stake_repr.t) list) tzresult
    Lwt.t

  val find :
    Raw_context.t ->
    Cycle_repr.cycle ->
    (Raw_context.t * (Signature.public_key_hash * Stake_repr.t) list option)
    tzresult
    Lwt.t

  val remove_existing :
    Raw_context.t -> Cycle_repr.cycle -> Raw_context.t tzresult Lwt.t
end

val set_selected_distribution_for_cycle :
  Raw_context.t ->
  Cycle_repr.cycle ->
  (Signature.public_key_hash * Stake_repr.t) list ->
  Stake_repr.t ->
  Raw_context.t tzresult Lwt.t

val get_selected_distribution :
  Raw_context.t ->
  Cycle_repr.cycle ->
  (Raw_context.t * (Signature.public_key_hash * Stake_repr.t) list) tzresult
  Lwt.t

val find_selected_distribution :
  Raw_context.t ->
  Cycle_repr.cycle ->
  (Raw_context.t * (Signature.public_key_hash * Stake_repr.t) list option)
  tzresult
  Lwt.t

val get_selected_distribution_as_map :
  Raw_context.t ->
  Cycle_repr.cycle ->
  (Raw_context.t * Stake_repr.t Signature.Public_key_hash.Map.t) tzresult Lwt.t

val prepare_stake_distribution : Raw_context.t -> Raw_context.t tzresult Lwt.t

val get_total_active_stake :
  Raw_context.t -> Cycle_repr.t -> Stake_repr.t tzresult Lwt.t
