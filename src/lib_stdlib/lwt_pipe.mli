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

(** [create ~size:(max_size, compute_size) ()] is an empty queue that can
    hold max [size] "bytes" of data, using [compute_size] to compute the
    number of "bytes" in a datum.

    Note that you can use [size] to actually limit the size in byte (i.e., the
    memory foot-print of the structure (in this case, consider using
    {!push_overhead} to account for the boilerplate memory), but you can also
    use [size] to limit the foot-print of the structure for some other resource.
    E.g., you can spin up tasks in separate processes and limit the number of
    concurrently running processes.

    Also note that the size bound is not inclusive. So with [size] set to
    [(2, fun _ -> 1)] you can add one (1) element and then the pipe is full. (It
    is full because adding any other element would take the total size to [2]
    which is not strictly smaller than the [max_size] bound.)

    If you do not provide a [size] argument, the queue is unbounded. *)
val create : ?size:int * ('a -> int) -> unit -> 'a t

(** [push q v] is a promise that is pending until there is enough space in [q]
    to accommodate [v]. When this happens [v] is added to the end of [q] and the
    promise resolves.

    If there is enough space in [q] to accommodate [v] when the call is made,
    then the [v] is added immediately and an already resolved promise is
    returned.

    Note that if several writes are stuck because the pipe is full. These
    writes will succeed in an order that might be different from the order the
    write attempts were made. Specifically, when pushing elements of different
    computed sizes, smaller pushes may be resolved earlier if enough space is
    freed.

    @raise {!Closed} if [q] is closed. More specifically, the promise is
    rejected with {!Closed} if [q] is closed. *)
val push : 'a t -> 'a -> unit Lwt.t

(** [pop q] is a promise that is pending until there is an element in [q]. When
    this happens an element is removed and the promise is fulfilled with it.

    If there is already an element in [q] when the call is made, the element is
    removed immediately and an already resolved promise is returned.

    @raise {!Closed} if [q] is empty and closed. More specifically, the promise
    is rejected with {!Closed} if [q] is empty and closed. *)
val pop : 'a t -> 'a Lwt.t

(** [pop_with_timeout t q] is a promise that behaves similarly to [pop q]
    except that it resolves with [None] if [t] resolves before there is an
    element in [q] to pop.

    Note that there can be multiple promises that are awaiting for an element to
    pop from the queue. As a result, it is possible that [pop_with_timeout] is
    fulfilled with [None] even though values have been pushed to the [q].

    [t] is canceled (i.e., it fails with [Canceled]) if an element is returned.

    @raise {!Closed} if [q] is empty and closed. More specifically, the promise
    is rejected with {!Closed} if [q] is empty and closed. *)
val pop_with_timeout : unit Lwt.t -> 'a t -> 'a option Lwt.t

(** [pop_all q] is a promise that is pending until there is an element in [q].
    When this happens, all the elements of [q] are removed and the promise is
    fulfilled with the list of elements (in the order in which they were
    inserted).

    If there is already an element in [q] when the call is made, the elements
    are removed immediately and an already resolved promise is returned.

    @raise {!Closed} if [q] is empty and closed. More specifically, the promise
    is rejected with {!Closed} if [q] is empty and closed. *)
val pop_all : 'a t -> 'a list Lwt.t

(** [pop_all_now q] removes and returns all the elements in [q] (in the order in
    which they were inserted). If [q] is empty, [[]] is returned.

    @raise {!Closed} if [q] is empty and closed. *)
val pop_all_now : 'a t -> 'a list

(** [peek q] returns the same value as [pop q] but does not remove the returned
    element.

    @raise {!Closed} if [q] is empty and closed. More specifically, the promise
    is rejected with {!Closed} if [q] is empty and closed. *)
val peek : 'a t -> 'a Lwt.t

(** [peek_all q] returns the elements in the [q] (oldest first), or [[]] if
    empty. It does not remove elements from [q].

    @raise {!Closed} if [q] is empty and closed. *)
val peek_all : 'a t -> 'a list

(** [push_now q v] either
    - adds [v] at the ends of [q] immediately and returns [true], or
    - if [q] is full, returns [false]. *)
val push_now : 'a t -> 'a -> bool

(** [pop_now q] may remove and return the first element in [q] if
    [q] contains at least one element. *)
val pop_now : 'a t -> 'a option

(** [length q] is the number of elements in [q]. *)
val length : 'a t -> int

(** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)
val is_empty : 'a t -> bool

(** [empty q] is a promise that resolves when [q] becomes empty. *)
val empty : 'a t -> unit Lwt.t

exception Closed

(** [close q] the write-end of [q]:

    * Future write attempts will fail with {!Closed}.
    * If there are pending reads, they will become rejected with {!Closed}.
    * Future read attempts will drain the data until there is no data left (at
      which point {!Closed} may be raised).

    The [close] function is idempotent. *)
val close : 'a t -> unit

val is_closed : 'a t -> bool

(** The number of bytes used in the internal representation to hold an element
    in the queue. *)
val push_overhead : int
