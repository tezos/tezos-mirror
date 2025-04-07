(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** {1 Slot index}

   A slot index is a possible value for a slot index with an upper
   bound. If a choice is ever made to increase the size of available
   slots in the protocol, we also need to change this module to
   accommodate for higher values. *)
type t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val zero : t

type error += Invalid_slot_index of {given : t; min : t; max : t}

val check_is_in_range : number_of_slots:int -> t -> unit tzresult

(** [of_int ~number_of_slots n] constructs a value of type {!t} from [n]. Returns
      {!Invalid_slot_index} in case the given value is not in the interval
      [[zero, number_of_slots-1]]. *)
val of_int : number_of_slots:int -> int -> t tzresult

(** [of_int_opt ~number_of_slots n] constructs a value of type {!t} from [n]. Returns [None]
      in case the given value is not in the interval [[zero, number_of_slots-1]]. *)
val of_int_opt : number_of_slots:int -> int -> t option

val to_int : t -> int

val to_int_list : t list -> int list

(** [slots_range ~number_of_slots ~lower ~upper] returns the list of slots indexes between
      [lower] and [upper].

      If [lower] is negative or [upper] is bigger than or equal to [number_of_slots], the function
      returns {!Invalid_slot_index}. *)
val slots_range :
  number_of_slots:int -> lower:int -> upper:int -> t list tzresult

(** [slots_range_opt ~number_of_slots ~lower ~upper] is similar to {!slots_range}, but return
    [None] instead of an error. *)
val slots_range_opt :
  number_of_slots:int -> lower:int -> upper:int -> t list option

(** [is_succ elt ~succ] returns true if and only if elt + 1 = succ. *)
val is_succ : t -> succ:t -> bool

include Compare.S with type t := t

module Index : Storage_description.INDEX with type t = t
