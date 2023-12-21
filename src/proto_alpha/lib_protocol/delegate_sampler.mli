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

(** Participation slots potentially associated to accounts. The
   accounts that didn't place a deposit will be excluded from this
   list. This function should only be used to compute the deposits to
   freeze or initialize the protocol while stitching. RPCs can use this
   function to predict an approximation of long term future slot
   allocations. It shouldn't be used in the baker. *)
val slot_owner :
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

(** [compute_snapshot_index ctxt cycle max_snapshot_index] Returns the index of
    the selected snapshot for the [cycle] passed as argument, and for the max
    index of snapshots taken so far, [max_snapshot_index] (see
    [Stake_storage.max_snapshot_index]. *)
val compute_snapshot_index :
  Raw_context.t -> Cycle_repr.t -> max_snapshot_index:int -> int tzresult Lwt.t

val select_new_distribution_at_cycle_end :
  Raw_context.t ->
  slashings:Int_percentage.t Signature.Public_key_hash.Map.t ->
  new_cycle:Cycle_repr.t ->
  Raw_context.t tzresult Lwt.t

val clear_outdated_sampling_data :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val select_distribution_for_cycle :
  Raw_context.t ->
  slashings:Int_percentage.t Signature.Public_key_hash.Map.t ->
  Cycle_repr.t ->
  Raw_context.t tzresult Lwt.t

module For_RPC : sig
  (** The baking power for a given delegate computed from its current
    stake. *)
  val delegate_current_baking_power :
    Raw_context.t -> Signature.public_key_hash -> int64 tzresult Lwt.t
end
