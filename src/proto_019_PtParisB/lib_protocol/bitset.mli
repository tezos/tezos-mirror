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

(** A bitset is a compact structure to store a set of integers. *)
type t

type error += Invalid_position of int

val encoding : t Data_encoding.t

(** A bitset encoding the empty set. *)
val empty : t

(** [mem field i] returns [true] iff [i] has been added in [field].

    This functions returns [Invalid_input i] if [i] is negative. *)
val mem : t -> int -> bool tzresult

(** [add field i] returns a new bitset which contains [i] in
    addition to the previous integers of [field].

    This functions returns [Invalid_input i] if [i] is negative. *)
val add : t -> int -> t tzresult

(** [from_list positions] folds [add] over the [positions] starting from [empty].
    This function returns [Invalid_input i] if [i] is negative and appears in
    [positions]. *)
val from_list : int list -> t tzresult

(** [fill ~length] is equivalent to setting all bits for positions in
    [0, length - 1] to [one]. i.e., to [from_list (0 -- size -1)] or to
    [(2 ^ length) - 1]. But it's more efficient than folding on individual
    positions to set them.

    The function returns [Invalid_position length] if [length] is negative.
*)
val fill : length:int -> t tzresult

(** [inter set_l set_r] returns [set] which is result of the
    intersection of [set_l] and [set_r]. *)
val inter : t -> t -> t

(** [diff set_l set_r] returns a [set] containing fiels in [set_l]
    that are not in [set_r]. *)
val diff : t -> t -> t

(** [occupied_size_in_bits bitset] returns the current number of bits
   occupied by the [bitset]. *)
val occupied_size_in_bits : t -> int

(** [hamming_weight bitset] returns the Hamming weight of [bitset]. *)
val hamming_weight : t -> int

(** [to_z t] Returns the sum of powers of two of the given bitset. *)
val to_z : t -> Z.t
