(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Bounded queues combined with hash-tables, based on {!Ringo}.

    A {e hash queue} is a structure where one can add elements to the back of
    the queue, while associating them to keys. This allows for efficiently
    retrieving elements based on the key and for removing elements anywhere in
    the queue.
*)

module Make
    (K : Hashtbl.HashedType) (V : sig
      type t
    end) : sig
  (** The type of hash queues holding bindings from [K.t] to [V.t] *)
  type t

  (** [create n] creates an empty hash queue of capacity [n]. New
      elements added to a full hash queue push the oldest ones out. *)
  val create : int -> t

  (** [remove q k] removes the binding from [k] in [q]. If [k] is not bound in
      [c], it does nothing. *)
  val remove : t -> K.t -> unit

  (** [replace q k v] binds the key [k] to the value [v] in the queue [q]. This
      may or may not cause another binding to be removed, depending on the number
      of bindings already present in [q]. *)
  val replace : t -> K.t -> V.t -> unit

  (** [find_opt q k] is [Some v] if [k] is bound to [v] in [q]. It is [None]
      otherwise. *)
  val find_opt : t -> K.t -> V.t option

  (** [filter q f] retains only the bindings [(k, v)] such that [f k v = true]. *)
  val filter : t -> (K.t -> V.t -> bool) -> unit

  (** [length q] is the number of bindings held by [q]. *)
  val length : t -> int

  (** [capacity q] is the number of bindings [q] can hold:
      [capacity (create n) = n] *)
  val capacity : t -> int

  (** [clear q] removes all bindings from [q]. *)
  val clear : t -> unit

  (** [fold f q init] folds the function [f] over the bindings
      of [q]. The elements are iterated from oldest to newest. *)
  val fold : (K.t -> V.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** Folding in the Lwt monad, from oldest to newest. *)
  val fold_s : (K.t -> V.t -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t

  (** Folding in the error monad, from oldest to newest. *)
  val fold_es :
    (K.t -> V.t -> 'a -> ('a, 'error) result Lwt.t) ->
    t ->
    'a ->
    ('a, 'error) result Lwt.t

  (** Returns the oldest element of the queue when not empty. Returns [None]
      when empty. *)
  val peek : t -> V.t option

  (** [take q] removes and returns the oldest element in queue [q], or returns
      [None] if the queue is empty. *)
  val take : t -> V.t option

  (** [peek_at_most q n] returns the oldest n elements of the queue [q]. If the
      queue has less than [n] elements, returns all elements of the queue. *)
  val peek_at_most : t -> int -> V.t list

  (** [take_at_most q n] removes and returns the oldest n elements of the queue
      [q]. If the queue has less than [n] elements, removes and returns all
      elements of the queue. *)
  val take_at_most : t -> int -> V.t list

  (** Returns the elements from oldest to newest. *)
  val elements : t -> V.t list
end
