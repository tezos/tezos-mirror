(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

(* Representation of range-checks in Plompiler

   Range checks represent a set of constraints applied to the wires, defining
   an upper bound that is a power of 2. There is at most one constraint per
   wire index.
*)

type t

val empty : t

(* [is_empty rc] returns true if there is no range-checks in [rc], false
   otherwise *)
val is_empty : t -> bool

(* [mem i rc] returns true if [i] is range-checked in [rc], false otherwise *)
val mem : int -> t -> bool

(* [find_opt i rc] returns None if [i] is not range-checked in [rc], and
   [Some nb_bits] otherwise, where [nb_bits] is the logarithm in base 2 of the
   bound for the value at index [i] *)
val find_opt : int -> t -> int option

(* [remove i rc] removes [i] from the indices to range-check given by [rc] *)
val remove : int -> t -> t

(* [add ~nb_bits i rc] adds [i] with the bound [2^nb_bits] to [rc].
   This function raises [Invalid_argument] if the index to add is
   already range-checked. *)
val add : nb_bits:int -> int -> t -> t
