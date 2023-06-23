(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module defines [Tick.t], an execution step counter for
    smart-contract rollups. *)

(** A tick is a counter for the execution step of a smart-contract rollup. *)
type t

(** The initial tick. *)
val initial : t

(** [next tick] returns the counter successor of [tick]. *)
val next : t -> t

(** [jump tick k] moves [tick] by [k] (possibly negative) steps.
    The move stops at [initial] when going back in time. *)
val jump : t -> Z.t -> t

(** [distance t1 t2] is the absolute value of the difference between [t1] and [t2]. *)
val distance : t -> t -> Z.t

(** [of_int x] returns [Some tick] for the rollup [x]-th execution
    step if [x] is non-negative. Returns [None] otherwise. *)
val of_int : int -> t option

(** [to_int tick] converts the [tick] into an integer. *)
val to_int : t -> int option

(** [of_number_of_ticks] converts from the bounded int type defined in
    the [Sc_rollup_repr] module. [Number_of_ticks] is used inside of
    commitments to limit the maximum possible storage requirement. It is
    bounded between one and [max_int] meaning that this can never return
    a negative number so an [option] isn't required. *)
val of_number_of_ticks : Sc_rollup_repr.Number_of_ticks.t -> t

val of_z : Z.t -> t

val to_z : t -> Z.t

(** [size_in_bytes tick] is the size in bytes of [tick]'s internal
    representation. This function is used by the gas model. *)
val size_in_bytes : t -> int

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

include Compare.S with type t := t

module Map : Map.S with type key = t
