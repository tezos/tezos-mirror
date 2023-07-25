(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Functions on stake and depending on the context. *)

(** Apply the delegation_over_baking and staking_over_baking limits of
    a delegate. Overstaked tez count as delegated, overdelegated tez
    do not count at all.  *)
val apply_limits :
  Raw_context.t ->
  Staking_parameters_repr.t ->
  Stake_repr.Full.t ->
  Stake_repr.t tzresult

(** The weight of a staker or a set of stakers. When adaptive
    issuance is active, the delegated tez weight
    edge_of_staking_over_delegation less than frozen ones. Since this
    function is applied on a [Stake_repr.t], the limits should already
    have been applied using [apply_limits] if necessary. *)
val staking_weight : Raw_context.t -> Stake_repr.t -> int64

(** The weight of a delegate used for voting rights. *)
val voting_weight : Raw_context.t -> Stake_repr.Full.t -> int64 tzresult

(** The weight of a baker used for baking and attesting rights. *)
val baking_weight :
  Raw_context.t ->
  Staking_parameters_repr.t ->
  Stake_repr.Full.t ->
  int64 tzresult

val compare : Raw_context.t -> Stake_repr.t -> Stake_repr.t -> int
