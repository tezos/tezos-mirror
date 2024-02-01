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

(** This module maintains the storage related to slashing of delegates for
   double signing. In particular, it is responsible for maintaining the
   {!Storage.Already_denounced}, {!Storage.Contract.Slashed_deposits}, and
   {!Storage.Pending_denunciations} tables.
*)

(** Returns true if the given delegate has already been denounced
    for double baking for the given level. *)
val already_denounced_for_double_baking :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  Round_repr.t ->
  bool tzresult Lwt.t

(** Returns true if the given delegate has already been denounced
    for double preattesting or double attesting for the given level. *)
val already_denounced_for_double_attesting :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  Round_repr.t ->
  bool tzresult Lwt.t

(** The [reward_and_burn] type embeds amounts involved when slashing a
    delegate for double attesting or double baking. *)
type reward_and_burn = {reward : Tez_repr.t; amount_to_burn : Tez_repr.t}

(** The [punishing_amounts] type embeds amounts involved when slashing a
    delegate for double attesting or double baking. *)
type punishing_amounts = {
  staked : reward_and_burn;
  unstaked : (Cycle_repr.t * reward_and_burn) list;
}

(** Record in the context that the given delegate is both marked for
    slashing for the given misbehaviour, and forbidden from taking
    part in the consensus process (baking/attesting).

    [operation_hash] corresponds to the denunciation that prompted
    this punishment. The level argument is the level of the duplicate
    blocks, or the level that the duplicate (pre)attestations point
    to, **not** the level of the block that contains the denunciation.

    This function asserts that the delegate has not already been
    denounced for the same misbehaviour at the same level. Indeed, if
    this were the case, then the current denunciation operation should
    have been rejected by {!Validate}. *)
val punish_double_signing :
  Raw_context.t ->
  operation_hash:Operation_hash.t ->
  Misbehaviour_repr.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  rewarded:Signature.public_key_hash ->
  Raw_context.t tzresult Lwt.t

(** Clear the part of {!Storage.Already_denounced} about the cycle
    [new_cycle - max_slashable_period]. Indeed, denunciations on
    events which happened during this cycle are no longer allowed. *)
val clear_outdated_already_denounced :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t Lwt.t

val apply_and_clear_denunciations :
  Raw_context.t -> (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

val update_slashing_storage_for_p : Raw_context.t -> Raw_context.t Lwt.t
