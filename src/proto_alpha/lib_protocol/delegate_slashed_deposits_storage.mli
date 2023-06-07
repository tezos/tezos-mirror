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
   {!Storage.Slashed_deposits} and {!Storage.Contract.Slashed_deposits} tables.
*)

(** Returns true if the given delegate has already been slashed
    for double baking for the given level. *)
val already_slashed_for_double_baking :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  bool tzresult Lwt.t

(** Returns true if the given delegate has already been slashed
    for double preendorsing or double endorsing for the given level. *)
val already_slashed_for_double_endorsing :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  bool tzresult Lwt.t

(** The [reward_and_burn] type embeds amounts involved when slashing a
    delegate for double endorsing or double baking. *)
type reward_and_burn = {reward : Tez_repr.t; amount_to_burn : Tez_repr.t}

(** The [punishing_amounts] type embeds amounts involved when slashing a
    delegate for double endorsing or double baking. *)
type punishing_amounts = {
  staked : reward_and_burn;
  unstaked : (Cycle_repr.t * reward_and_burn) list;
}

(** Record in the context that the given delegate has now been slashed
    for double endorsing for the given level and return the amounts to
    burn and to reward. If the delegate has no remaining frozen
    deposits, this will also forbid it to bake or endorse until a new
    deposit is frozen.

    Fails with [Unrequired_denunciation] if the given delegate has
    already been slashed for double endorsing for the given level.  *)
val punish_double_endorsing :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  (Raw_context.t * punishing_amounts) tzresult Lwt.t

(** Record in the context that the given delegate has now been slashed
    for double baking for the given level and returns the amounts to
    burn and to reward. If the delegate has no remaining frozen
    deposits, this will also forbid it to bake or endorse until a new
    deposit is frozen.

    Fails with [Unrequired_denunciation] if the given delegate has
    already been slashed for double baking for the given level.  *)
val punish_double_baking :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Level_repr.t ->
  (Raw_context.t * punishing_amounts) tzresult Lwt.t

val clear_outdated_slashed_deposits :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t Lwt.t
