(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This module draws random values for a cycle based on the {!Seed_repr.seed}
   associated that cycle. These random values are only delegates associated with
   slots.
   The selection of delegates is done by {i sampling} from a particular
   distribution of the stake among the active delegates.

   This module is responsible for maintaining the table
   {!Storage.Delegate_sampler_state}. *)

type error += Invalid_slot of {level : Level_repr.t; slot : Slot_repr.t}

(** Participation slots potentially associated to accounts. The
   accounts that didn't place a deposit will be excluded from this
   list. This function should only be used to compute the deposits to
   freeze or initialize the protocol while stitching. RPCs can use this
   function to predict an approximation of long term future slot
   allocations. It shouldn't be used in the baker. *)
val attestation_slot_owner :
  all_bakers_attest_enabled:bool ->
  Raw_context.t ->
  Level_repr.t ->
  Slot_repr.t ->
  (Raw_context.t * Delegate_consensus_key.pk) tzresult Lwt.t

val baking_rights_owner :
  Raw_context.t ->
  Level_repr.t ->
  round:Round_repr.round ->
  (Raw_context.t * Slot_repr.t * Delegate_consensus_key.pk) tzresult Lwt.t

(** [load_sampler_for_cycle ctxt cycle] caches the seeded stake
    sampler for [cycle] in [ctxt]. If the sampler was already cached,
    then [ctxt] is returned unchanged.

    This function has the same effect on [ctxt] as {!slot_owner} and
    {!baking_rights_owner}. *)
val load_sampler_for_cycle :
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

(** [stake_info_for_cycle ctxt cycle] reads the stake info for [cycle] from
    [ctxt] if it has been previously initialized. Otherwise it initializes
    the sampler and caches it in [ctxt] with
    [Raw_context.set_stake_info_for_cycle].
    Returns the updated context, the total staking power active for the cycle,
    and the list of all delegates with their respective active staking power. *)
val stake_info_for_cycle :
  Raw_context.t ->
  Cycle_repr.t ->
  (Raw_context.t * Raw_context.stake_info) tzresult Lwt.t

(** Same as [stake_info_for_cycle], but for the given level (uses the level's cycle) *)
val stake_info :
  Raw_context.t ->
  Level_repr.t ->
  (Raw_context.t * Raw_context.stake_info) tzresult Lwt.t

val select_new_distribution_at_cycle_end :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val clear_outdated_sampling_data :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val select_distribution_for_cycle :
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

(** [attesting_power ctxt level] returns a map of the delegates to
    their attesting power for the given level. Fails if the
    given level is in a cycle for which the seed is not in the storage *)
val attesting_power :
  all_bakers_attest_enabled:bool ->
  Raw_context.t ->
  Level_repr.t ->
  (Raw_context.t * int64 Signature.Public_key_hash.Map.t) tzresult Lwt.t

module For_RPC : sig
  (** The baking power for a given delegate computed from its current
    stake. *)
  val delegate_current_baking_power :
    Raw_context.t -> Signature.public_key_hash -> int64 tzresult Lwt.t

  val total_baking_power : Raw_context.t -> Cycle_repr.t -> int64 tzresult Lwt.t
end
