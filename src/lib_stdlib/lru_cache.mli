(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Least-Recently Used values cache implementation

    This cache is implemented as a doubly linked list used as a queue
    with a hash table for efficient access and fast updates.

    The last read or pushed element [e] is promoted as the most recent
    element and will be removed from the cache if [capacity] elements
    have been pushed without [e] being accessed.
*)

module Make (Table : Hashtbl.S) : sig
  type key = Table.key

  type 'a t

  (** [create ~capacity] creates a LRU cache with a maximum
      [capacity].  Raise [Invalid_argument] if [capacity] is below or
      equal 0. *)
  val create : capacity:int -> 'a t

  (** Returns the number of elements currently stored. *)
  val size : 'a t -> int

  (** Tests that the given value is cached *)
  val is_cached : 'a t -> key -> bool

  (** [get cache fetch_f key] retrieves the value in the cache if it is
      present or call [fetch_f] otherwise. The resulting value is then
      promoted to the most recently accessed element. *)
  val get : 'a t -> (key -> 'a) -> key -> 'a

  (** Same as [get] but with a lwt fetching function. *)
  val get_lwt : 'a t -> (key -> 'a Lwt.t) -> key -> 'a Lwt.t

  (** Same as [get] but with a lwt fetching function with optional
      result. *)
  val get_opt_lwt : 'a t -> (key -> 'a option Lwt.t) -> key -> 'a option Lwt.t

  (** [push cache key value] manually pushes a [value] in the cache
      and promotes it as the most recent used element. Removes the
      oldest used when the cache is at maximum capacity. If [key] is
      already present in the cache, [value] is unused and [key] is
      still associated to the previous value. *)
  val push : 'a t -> key -> 'a -> unit

  (** [remove cache key] forces the removal of the [key] entry from
      the cache if present, do nothing otherwise. *)
  val remove : 'a t -> key -> unit

  (** [bindings cache] returns all the cached bindings. *)
  val bindings : 'a t -> (key * 'a) list

  (** [clear cache] removes all cached values. *)
  val clear : 'a t -> unit

  (**/**)

  (** For testing purpose only *)
  val check_consistency : 'a t -> unit
end
