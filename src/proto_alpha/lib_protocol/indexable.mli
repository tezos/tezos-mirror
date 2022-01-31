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
    integer. The first type parameter determines whether or not
    this replacement has happened already. *)
type ('state, 'a) indexable =
  | Value : 'a -> ([> `Value], 'a) indexable
  | Index : int32 -> ([> `Id], 'a) indexable

type unknown = [`Value | `Id]

type index_only = [`Id]

type 'a t = (unknown, 'a) indexable

(** A value of type ['a index] is necessarily an index, that is, not
    an value. *)
type 'a index = (index_only, 'a) indexable

val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** [index h m] computes the index of [m], if necessary. That is, if
    [m] is already an index, it is returned as-is. Otherwise, the
    index is computed using the handler [h]. *)
val index :
  ('a -> (int32, error trace) result Lwt.t) -> 'a t -> 'a index tzresult Lwt.t

(** [in_memory_size a] returns the number of bytes allocated in RAM for [a]. *)
val in_memory_size :
  ('a -> Cache_memory_helpers.sint) -> 'a t -> Cache_memory_helpers.sint

(** [size a] returns the number of bytes allocated in an inbox to store [a]. *)
val size : ('a -> int) -> 'a t -> int

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

module type VALUE = sig
  type t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module Make (V : VALUE) : sig
  type nonrec 'state indexable = ('state, V.t) indexable

  type nonrec index = V.t index

  include VALUE with type t = V.t t
end
