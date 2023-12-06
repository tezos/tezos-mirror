(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Datatype for a map from cycle to deposits, where all unslashable cycles
    are squashed.
    
    Expected to be used for a small number of cycles at a time, typically
    bounded by [preserved_cycles + max_slashing_period] plus a small constant.

    See {!Unstaked_frozen_deposits_storage} for more info on unstaked frozen
    deposits.
*)

(** Storable version. *)
type t

(** To be used locally, do not preserve values of this type over cycles. *)
type squashed = private {unslashable_cycle : Cycle_repr.t option; t : t}

val empty : unslashable_cycle:Cycle_repr.t option -> squashed

val encoding : t Data_encoding.t

(** Once read, [t] must be converted to [squashed] with [squash_unslashable]
    to be used efficiently.
    For a given [unslashable_cycle], [squash_unslashable ~unslashable_cycle] is
    idempotent. *)
val squash_unslashable :
  unslashable_cycle:Cycle_repr.t option -> t -> squashed tzresult

val get : Cycle_repr.t -> squashed -> Deposits_repr.t

val update :
  f:(Deposits_repr.t -> Deposits_repr.t tzresult) ->
  Cycle_repr.t ->
  squashed ->
  squashed tzresult
