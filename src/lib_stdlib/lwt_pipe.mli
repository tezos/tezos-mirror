(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Data queues similar to the [Pipe] module in Jane Street's [Async]
    library. They are implemented with [Queue]s, limited in size, and
    use Lwt primitives for concurrent access. *)

(** Type of queues holding values of type ['a]. *)
type 'a t

(** [create ~size:(max_size, compute_size)] is an empty queue that can
    hold max [size] bytes of data, using [compute_size] to compute the
    size of a datum. If want to count allocated bytes precisely, you
    need to add [push_overhead] to the result of[compute_size].
    When no [size] argument is provided, the queue is unbounded. *)
val create : ?size:int * ('a -> int) -> unit -> 'a t

(** [push q v] is a promise that blocks while [q] contains more
    than [size] elements, then adds [v] at the end of [q]. *)
val push : 'a t -> 'a -> unit Lwt.t

(** [pop q] is a promise that blocks while [q] is empty, then
    removes and returns the first element in [q]. *)
val pop : 'a t -> 'a Lwt.t

(** [pop_with_timeout t q] is a promise that blocks while [q] is empty, then
    removes the first element [v] in [q] and returns [Some v].

    If no message can be popped before [t] resolves, it returns [None].
    As concurrent readers are allowed, [None] does not
    necessarily mean that no value has been pushed.

    [t] is canceled (i.e., it fails with [Canceled]) if an element is returned.
*)
val pop_with_timeout : unit Lwt.t -> 'a t -> 'a option Lwt.t

(** [pop_all q] is a promise that blocks while [q] is empty, then
    removes and returns all the elements in [q] (in the order in which they
    were inserted). *)
val pop_all : 'a t -> 'a list Lwt.t

(** [pop_all_now q] removes and returns all the element in [q] (in the order in
    which they were inserted). If [q] is empty, [[]] is returned. *)
val pop_all_now : 'a t -> 'a list

(** [peek q] returns the same value as [pop q] but does not remove the first
    element. *)
val peek : 'a t -> 'a Lwt.t

(** [peek_all q] returns the elements in the [q] (oldest first),
    or [[]] if empty. *)
val peek_all : 'a t -> 'a list

(** [values_available q] is a promise that blocks while [q] is empty. *)
val values_available : 'a t -> unit Lwt.t

(** [push_now q v] either
    - adds [v] at the ends of [q] immediately and returns [true], or
    - if [q] is full, returns [false]. *)
val push_now : 'a t -> 'a -> bool

exception Full

(** [push_now q v] either
    - adds [v] at the ends of [q] immediately, or
    - if [q] is full, raises [Full].

    @raise [Full] if [q] does not have enough space to hold [v]. *)
val push_now_exn : 'a t -> 'a -> unit

(** [safe_push_now q v] adds [v] to [q] if [q] is not [Closed] and has enough
    space available, otherwise it does nothing. *)
val safe_push_now : 'a t -> 'a -> unit

(** [pop_now q] may remove and return the first element in [q] if
    [q] contains at least one element. *)
val pop_now : 'a t -> 'a option

exception Empty

(** [pop_now_exn q] removes and returns the first element in [q] if
    [q] contains at least one element, or raise [Empty] otherwise.

    @raise [Empty] if [q] holds no elements. *)
val pop_now_exn : 'a t -> 'a

(** [length q] is the number of elements in [q]. *)
val length : 'a t -> int

(** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)
val is_empty : 'a t -> bool

(** [empty q] is a promise that resolves when [q] becomes empty. *)
val empty : 'a t -> unit Lwt.t

(** [iter q ~f] pops all elements of [q] and applies [f] on them. *)
val iter : 'a t -> f:('a -> unit Lwt.t) -> unit Lwt.t

exception Closed

(** [close q] the write-end of [q]:

    * Future write attempts will fail with [Closed].
    * If there are reads blocked, they will unblock and fail with [Closed].
    * Future read attempts will drain the data until there is no data left.

    Thus, after a pipe has been closed, reads never block.
    The [close] function is idempotent.
*)
val close : 'a t -> unit

(** The allocated size in bytes when pushing in the queue. *)
val push_overhead : int
