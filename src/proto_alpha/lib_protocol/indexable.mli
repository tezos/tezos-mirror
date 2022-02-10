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

(** An indexable value is a value which can be replaced by an
    integer. The first type parameter determines whether or not this
    replacement has happened already. *)
type (_, 'a) t = private
  | Value : 'a -> ([> `Value], 'a) t
  | Index : int32 -> ([> `Id], 'a) t

type value_only = [`Value]

type index_only = [`Id]

type unknown = [value_only | index_only]

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

(** [index_exn i] wraps [i] into an indexable value identified as
    being an index.

    @raise Invalid_argument iff [i <= 0l]. *)
val index_exn : int32 -> 'a index

(** [from_index i] wraps [i] into an indexable value, but forget about the
    nature of the content of the result.

    Returns the error [Index_cannot_be_negative] iff [i <= 0l]. *)
val from_index : int32 -> 'a either tzresult

(** [from_index_exn i] wraps [i] into an indexable value, but forget
    about the nature of the content of the result.

    @raise Invalid_argument iff [i <= 0l]. *)
val from_index_exn : int32 -> 'a either

(** [forget_value v] returns an indexable value whose content is
    unknown, {i i.e.}, such that the content of [v] has been forgotten
    by the type system. *)
val forget_value : 'a value -> 'a either

(** [forget_index i] returns an indexable value whose content is
    unknown, {i i.e.}, such that the content of [v] has been forgotten
    by the type system. *)
val forget_index : 'a index -> 'a either

val encoding : 'a Data_encoding.t -> 'a either Data_encoding.t

val pp :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a either -> unit

(** [prepare_index h v] computes the index of [v], if necessary. That
    is, if [v] is already an index, it is returned as-is. Otherwise,
    the index is computed by using the handler [h].

    Returns [Index_cannot_be_negative] iff the result computed by [h]
    is negative, or any errors returned by [h]. *)
val prepare_index :
  ('a -> int32 tzresult Lwt.t) -> 'a either -> 'a index tzresult Lwt.t

(** [prepare_value h v] computes the value associated to the indexed
    value [v], if necessary. That is, if [v] is already a value, it is
    returned as-is. Otherwise, the value is computed by using the
    handler [h], or any errors returned by [h]. *)
val prepare_value :
  (int32 -> 'a tzresult Lwt.t) -> 'a either -> 'a value tzresult Lwt.t

(** [in_memory_size a] returns the number of bytes allocated in RAM for [a]. *)
val in_memory_size :
  ('a -> Cache_memory_helpers.sint) -> 'a either -> Cache_memory_helpers.sint

(** [size a] returns the number of bytes allocated in an inbox to store [a]. *)
val size : ('a -> int) -> 'a either -> int

val compare : ('a -> 'a -> int) -> 'a either -> 'a either -> int

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

  val forget_value : value -> either

  val forget_index : index -> either

  val value : V.t -> value

  val index : int32 -> index tzresult

  val index_exn : int32 -> index

  val from_value : V.t -> either

  val from_index : int32 -> either tzresult

  val from_index_exn : int32 -> either

  val prepare_index :
    (V.t -> int32 tzresult Lwt.t) -> either -> index tzresult Lwt.t

  val prepare_value :
    (int32 -> V.t tzresult Lwt.t) -> either -> value tzresult Lwt.t

  val encoding : either Data_encoding.t

  val index_encoding : index Data_encoding.t

  val compare : either -> either -> int

  val pp : Format.formatter -> either -> unit
end

type error += Index_cannot_be_negative of int32
