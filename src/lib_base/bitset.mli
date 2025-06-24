(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

(** A bitset is a compact structure to store a set of integers. *)
type t

type error +=
  | Invalid_position of int
  | Invalid_range of {pos : int; length : int}
  | Invalid_input of string

val encoding : t Data_encoding.t

(** A bitset encoding the empty set. *)
val empty : t

(** [is_empty i] is [true] if [i] is empty. *)
val is_empty : t -> bool

(** [equal i j] is [true] if [i] and [j] are identical. *)
val equal : t -> t -> bool

(** [mem bitset i] returns [true] iff [i] has been added in [bitset].

    This functions returns [Invalid_position i] if [i] is negative. *)
val mem : t -> int -> bool tzresult

(** [add bitset i] returns a new bitset which contains [i] in
    addition to the previous integers of [bitset].

    This functions returns [Invalid_position i] if [i] is negative. *)
val add : t -> int -> t tzresult

(** [add_many bitset i length] returns a new bitset which contains
    [i], [i+1], ..., [i+length-1] in addition to the previous integers
    of [bitset].

    This functions returns [Invalid_range {pos = i; length}] if [i] is
    negative or length is not positive. *)
val add_many : t -> int -> int -> t tzresult

(** [remove bitset i] returns a new bitset in which [i] is
    removed from [bitset].

    This functions returns [Invalid_position i] if [i] is negative. *)
val remove : t -> int -> t tzresult

(** [remove_many bitset i length] returns a new bitset in which [i],
    [i+1], ... [i+length-1] are removed from [bitset].

    This functions returns [Invalid_range {pos = i; length}] if [i] is
    negative or length is not positive. *)
val remove_many : t -> int -> int -> t tzresult

(** [shift_right bitset ~offset] returns a new bitset [bitset'] such
    that for any [i] we have:

    [mem (i - offset) bitset'] if and only if [i >= offset] and [mem i bitset].

    In other words, positions smaller than `offset` are removed and positions larger
    or equal than `offset` are decreased by `offset`.

    This functions returns [Invalid_input "shift_right"] if [offset] is negative. *)
val shift_right : t -> offset:int -> t tzresult

(** [from_list positions] folds [add] over the [positions] starting from [empty].
    This function returns [Invalid_position i] if [i] is negative and appears in
    [positions]. *)
val from_list : int list -> t tzresult

(** [to_list t] returns the list of integers in the bitset. *)
val to_list : t -> int list

(** [fill ~length] is equivalent to setting all bits for positions in [0, length
    - 1] to one, or to [from_list (0 -- size-1)], or to [from_z ((2 ^ length) -
    1)]. But it's more efficient than folding on individual positions to set
    them.

    The function returns [Invalid_input "fill"] if [length] is negative.
*)
val fill : length:int -> t tzresult

(** [inter set_l set_r] returns [set] which is result of the
    intersection of [set_l] and [set_r]. *)
val inter : t -> t -> t

(** [diff set_l set_r] returns a bitset containing integers in [set_l] that are
    not in [set_r]. *)
val diff : t -> t -> t

(** [occupied_size_in_bits bitset] returns the current number of bits
   occupied by the [bitset]. *)
val occupied_size_in_bits : t -> int

(** [cardinat bitset] returns the number of elements in the [bitsest]. *)
val cardinal : t -> int

(** [to_z t] returns the sum of powers of two of the integers in the given
    bitset. *)
val to_z : t -> Z.t

(** [from_z] builds a bitset from its integer representation. Returns
    [Invalid_input "from_z"] if the given argument is negative. *)
val from_z : Z.t -> t tzresult
