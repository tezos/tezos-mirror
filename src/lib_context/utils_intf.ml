(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tarides <contact@tarides.com>                          *)
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

module type Arena = sig
  type t

  (** [create ~elt_length:len ~initial_capacity:n] is an empty arena of strings
      of length [len], with space sufficient to store [n] values. *)
  val create : elt_length:int -> initial_capacity:int -> t

  (** [is_full t] is true iff [t] has no remaining space for elements. *)
  val is_full : t -> bool

  (** [expand t n] re-allocates arena [t] to support storing up to [n]-many
      elements. Existing elements in the arena retain their original {!id}s.

      @raise Invalid_argument if [n] is less than the current capacity of [t]. *)
  val expand : t -> int -> unit

  (** The type of references to allocated elements in an arena. *)
  type id

  (** [allocate t s] adds the string [s] to arena [t], returning a reference to
      the storage location that may later be {!dereference}d to get back [s].

      @raise Invalid_argument if [t] {!is_full}. The behaviour is undefined if
      the length of [s] is not equal to the [elt_length] of [t]. *)
  val allocate : t -> string -> id

  (** [dereference t id] is the string that was passed to the {!allocate} call
      that returned [id]. The behaviour is undefined if [id] was not created by
      an allocation within [t]. *)
  val dereference : t -> id -> string

  (** [elt_equal t id s] is equivalent to [String.equal (dereference t id) s],
      but more efficient. *)
  val elt_equal : t -> id -> string -> bool
end

module type Small_list = sig
  (** This API has the same semantics as that of [List]. *)

  type +'a t

  val empty : _ t

  val cons : 'a -> 'a t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit

  val exists : ('a -> bool) -> 'a t -> bool
end

module type String_set = sig
  type t

  (** [create ~elt_length:len ~initial_capacity:n] is a set of strings of length
      [len], capable of storing [n] values without internal reallocation. *)
  val create : elt_length:int -> initial_capacity:int -> t

  (** [add t elt] adds [elt] to [t]. *)
  val add : t -> string -> unit

  (** [mem t elt] is true iff the string [elt] has been added to [t]. *)
  val mem : t -> string -> bool
end

module type Seq_lwt = sig
  type +'a node = Nil | Cons of 'a * 'a t

  and 'a t = unit -> ('a node, error trace) result Lwt.t

  val fold_left :
    ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> ('a, error trace) result Lwt.t

  val unfold :
    ('b -> (('a * 'b) option, error trace) result Lwt.t) -> 'b -> 'a t
end

module type Intf = sig
  (** An implementation of append-only arenas of fixed-length strings, with
      support for manual expansion. *)
  module Arena : Arena

  (** A list type optimised for memory efficiency of small list sizes (~ 1-6). *)
  module Small_list : Small_list

  (** A mutable set implementation optimised for storing fixed-length strings. *)
  module String_set : String_set

  (** Similar to [Seq] but uses Lwt.t for the delayed elements of the sequence. *)
  module Seq_lwt : Seq_lwt
end
