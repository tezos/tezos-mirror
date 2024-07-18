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

(** Stake of a delegate.

    It has the invariants enforced by {!Stake_context.apply_limits}:
    [frozen] excludes any overstaked tez, and [weighted_delegated]
    includes overstaked tez but excludes overdelegated tez. *)
type t = private {frozen : Tez_repr.t; weighted_delegated : Tez_repr.t}

val zero : t

(** Builds a {!t}. Should only be called in
    {!Stake_context.apply_limits} to enforce the invariants. *)
val make : frozen:Tez_repr.t -> weighted_delegated:Tez_repr.t -> t

val encoding : t Data_encoding.t

(** Returns only the frozen part of a stake. This includes the frozen
    balances from the delegate and any stakers, but excludes any
    overstaked tez. *)
val get_frozen : t -> Tez_repr.t

val ( +? ) : t -> t -> t tzresult

(** The weight of a staker or a set of stakers. Since this
    function is applied on a [Stake_repr.t], the limits should already
    have been applied using [apply_limits] if necessary. *)
val staking_weight : t -> int64

val compare : t -> t -> int

val has_minimal_stake_to_participate : minimal_stake:Tez_repr.t -> t -> bool
