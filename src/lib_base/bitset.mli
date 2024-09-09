(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

(** A bitset is a compact structure to store a set of integers. *)
type t

type error += Invalid_position of int

val encoding : t Data_encoding.t

(** A bitset encoding the empty set. *)
val empty : t

(** [is_empty i] is [true] if [i] is empty. *)
val is_empty : t -> bool

(** [equal i j] is [true] if [i] and [j] are identical. *)
val equal : t -> t -> bool

(** [mem field i] returns [true] iff [i] has been added in [field].

    This functions returns [Invalid_input i] if [i] is negative. *)
val mem : t -> int -> bool tzresult

(** [add field i] returns a new bitset which contains [i] in
    addition to the previous integers of [field].

    This functions returns [Invalid_input i] if [i] is negative. *)
val add : t -> int -> t tzresult

(** [remove field i] returns a new bitset in which [i] is
    removed from [field].

    This functions returns [Invalid_input i] if [i] is negative. *)
val remove : t -> int -> t tzresult

(** [from_list positions] folds [add] over the [positions] starting from [empty].
    This function returns [Invalid_input i] if [i] is negative and appears in
    [positions]. *)
val from_list : int list -> t tzresult

(** [to_list t] returns the list of int in the bitset. *)
val to_list : t -> int list

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

(** [cardinat bitset] returns the number of elements in the [bitsest]. *)
val cardinal : t -> int

(** [to_z t] Returns the sum of powers of two of the given bitset. *)
val to_z : t -> Z.t