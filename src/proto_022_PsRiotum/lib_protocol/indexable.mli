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

(** In transaction rollups, some values can be replaced by indexes in
    the messages sent from the layer-1 to the layer-2.

    This module provides various type-safe helpers to manipulate these
    particular values. *)

type value_only = Value_only

type index_only = Index_only

type unknown = Unknown

(** An indexable value is a value which can be replaced by an
    integer. The first type parameter determines whether or not this
    replacement has happened already. *)
type (_, 'a) t = private
  | Value : 'a -> (value_only, 'a) t
  | Hidden_value : 'a -> (unknown, 'a) t
  | Index : int32 -> (index_only, 'a) t
  | Hidden_index : int32 -> (unknown, 'a) t

(** The type of indexable values identified as not being indexes. *)
type 'a value = (value_only, 'a) t

(** The type of indexable values identified as being indexes. *)
type 'a index = (index_only, 'a) t

(** The type of indexable values whose content is still unknown. *)
type 'a either = (unknown, 'a) t

(** [value v] wraps [v] into an indexable value identified as not
    being an index. *)
val value : 'a -> 'a value

(** [from_value v] wraps [v] into an indexable value, but forget about
    the nature of the content of the result. *)
val from_value : 'a -> 'a either

(** [index i] wraps [i] into an indexable value identified as being an
    index.

    Returns the error [Index_cannot_be_negative] iff [i <= 0l]. *)
val index : int32 -> 'a index tzresult

(** [from_index i] wraps [i] into an indexable value, but forget about the
    nature of the content of the result.

    Returns the error [Index_cannot_be_negative] iff [i <= 0l]. *)
val from_index : int32 -> 'a either tzresult

(** [index_exn i] wraps [i] into an indexable value identified as
    being an index.

    @raise Invalid_argument iff [i <= 0l]. *)
val index_exn : int32 -> 'a index

(** [from_index_exn i] wraps [i] into an indexable value, but forget
    about the nature of the content of the result.

    @raise Invalid_argument iff [i <= 0l]. *)
val from_index_exn : int32 -> 'a either

(** [compact val_encoding] is a combinator to derive a compact
    encoding for an indexable value of type ['a] from an encoding for
    ['a]. It uses two bits in the shared tag. [00] is used for indexes
    fitting in one byte, [01] for indexes fitting in two bytes, [10]
    for indexes fitting in four bytes, and [11] for the values of type
    ['a]. *)
val compact : 'a Data_encoding.t -> (unknown, 'a) t Data_encoding.Compact.t

val encoding : 'a Data_encoding.t -> (unknown, 'a) t Data_encoding.t

val pp :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> ('state, 'a) t -> unit

(** [destruct x] returns either the index or the (unwrapped) value
    contained in [x].

    {b Note:} If you want to manipulate a value of type {!type-value},
    you can use {!val-value}. *)
val destruct : ('state, 'a) t -> ('a index, 'a) Either.t

(** [forget x] returns an indexable value whose kind of contents has
    been forgotten. *)
val forget : ('state, 'a) t -> (unknown, 'a) t

(** [to_int32 x] unwraps and returns the integer behind [x]. *)
val to_int32 : 'a index -> int32

(** [to_value x] unwraps and returns the value behind [x]. *)
val to_value : 'a value -> 'a

(** [is_value_e err x] unwraps and returns the value behind [x], and
    throws an [err] if [x] is an index. *)
val is_value_e : error:'trace -> ('state, 'a) t -> ('a, 'trace) result

(** [in_memory_size a] returns the number of bytes allocated in RAM for [a]. *)
val in_memory_size :
  ('a -> Cache_memory_helpers.sint) ->
  ('state, 'a) t ->
  Cache_memory_helpers.sint

(** [size a] returns the number of bytes allocated in an inbox to store [a]. *)
val size : ('a -> int) -> ('state, 'a) t -> int

(** [compare f x y] is a total order on indexable values, which
    proceeds as follows.

    {ul {li If both [x] and [y] are a value, then use [f] to compare them.}
        {li If both [x] and [y] are indexes, then uses the
            [Int32.compare] function to compare them.}
        {li Finally, if [x] and [y] have not the same kind, the logic
            is that indexes are smaller than values.}}

    {b Note:} This can be dangerous, as you may end up comparing two
    things that are equivalent (a value and its index) but declare
    they are not equal. *)
val compare : ('a -> 'a -> int) -> ('state, 'a) t -> ('state', 'a) t -> int

(** [compare_values f x y] compares the value [x] and [y] using [f],
    and relies on the type system of OCaml to ensure that [x] and [y]
    are indeed both values. *)
val compare_values : ('a -> 'a -> int) -> 'a value -> 'a value -> int

(** [compare_indexes x y] compares the indexes [x] and [y], and relies
    on the type system of OCaml to ensure that [x] and [y] are indeed
    both indexes. *)
val compare_indexes : 'a index -> 'a index -> int

module type VALUE = sig
  type t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module Make (V : VALUE) : sig
  type nonrec 'state t = ('state, V.t) t

  type nonrec index = V.t index

  type nonrec value = V.t value

  type nonrec either = V.t either

  val value : V.t -> value

  val index : int32 -> index tzresult

  val index_exn : int32 -> index

  val compact : either Data_encoding.Compact.t

  val encoding : either Data_encoding.t

  val index_encoding : index Data_encoding.t

  val value_encoding : value Data_encoding.t

  val compare : 'state t -> 'state' t -> int

  val compare_values : value -> value -> int

  val compare_indexes : index -> index -> int

  val pp : Format.formatter -> 'state t -> unit
end

type error += Index_cannot_be_negative of int32
