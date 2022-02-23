(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

exception Closed

module Bounded : sig
  (** Data queues similar to the [Pipe] module in Jane Street's [Async]
    library. They are implemented with [Queue]s, limited in size, and
    use Lwt primitives for concurrent access. *)

  (** Type of queues holding values of type ['a]. *)
  type 'a t

  (** [create ~max_size ~compute_size ()] is an empty queue that can hold at
      most [max_size] "bytes" of data, using [compute_size] to compute the
      number of "bytes" in a datum.

      Note that you can use [max_size]/[compute_size] to actually limit the size
      in byte (i.e., the memory footprint of the structure (in this case,
      consider using {!push_overhead} to account for the boilerplate memory),
      but you can also use [max_size]/[compute_size] to limit the footprint of
      the structure for some other resource. E.g., you can spin up tasks in
      separate processes and limit the number of concurrently running processes.

      Also note that the size bound is not inclusive. So with [max_size] set to
      [2] and [compute_size] to [fun _ -> 1] you can add one (1) element and
      then the pipe is full. (It is full because adding any other element would
      take the total size to [2] which is not strictly smaller than the
      [max_size] bound.)

      Finally, note that when the pipe is empty, inserting an element will
      always succeed immediately, even if its size exceed the size bound. For
      the same reason, inserting an element bigger than the size bound will
      eventually succeeds, but only once the pipe has been emptied. Once such an
      element has been inserted, no other element can be pushed until the
      bigger-than-bound element is popped. This behaviour breaks the invariant
      guaranteed by the module, but is kept for backwards compatibility. It may
      be changed in the future, but only after a careful review of the potential
      impact. *)
  val create : max_size:int -> compute_size:('a -> int) -> unit -> 'a t

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

  (** [pop_all q] is a promise that is pending until there is at least one element
    in [q]. When this happens, all the elements of [q] are removed and the
    promise is fulfilled with the list of elements (in the order in which they
    were inserted).

    If there is already one or more elements in [q] when the call is made, the
    elements are removed immediately and an already resolved promise is
    returned.

    @raise {!Closed} if [q] is empty and closed. More specifically, the promise
    is rejected with {!Closed} if [q] is empty and closed.

    In practice, this function returns a promise that either:
    - is pending and will resolve with a single-element list,
    - is already resolved with a list of at least one element,
    - or will be rejected with {!Closed}. *)
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

  (** [peek_all_now q] returns the elements in the [q] (oldest first), or [[]]
      if empty. It does not remove elements from [q].

      @raise {!Closed} if [q] is empty and closed. *)
  val peek_all_now : 'a t -> 'a list

  (** [push_now q v] either
      - adds [v] at the ends of [q] immediately and returns [true], or
      - if [q] is full, returns [false].

      @raise [Closed] if [q] is closed. *)
  val push_now : 'a t -> 'a -> bool

  (** [pop_now q] may remove and return the first element in [q] if
    [q] contains at least one element.

    @raise [Closed] if [q] is closed. *)
  val pop_now : 'a t -> 'a option

  (** [length q] is the number of elements in [q]. *)
  val length : 'a t -> int

  (** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)
  val is_empty : 'a t -> bool

  (** [close q] the write-end of [q]:

    - Pending and future write attempts will fail with {!Closed}.
    - If there is data left in the pipe, then future read attempts will be
      resolved until the remaining data is drained, after which further reads
      will fail with {!Closed}.
    - If there is no data left in the pipe, then pending and future reads will
      fail with {!Closed}.

    The [close] function is idempotent. *)
  val close : 'a t -> unit

  (** [is_closed q] is [true] if [close q] has been called.
    It is [false] otherwise. *)
  val is_closed : 'a t -> bool

  (** The number of bytes used in the internal representation to hold an element
    in the queue. *)
  val push_overhead : int
end

module Unbounded : sig
  (** [Unbounded] is a variant of {!Bounded} where there is no size
    limit. It is equivalent to using {!Bounded} with
    [compute_size = Fun.const 0] but some functions are specialised to this
    particular setup. *)

  (** Type of queues holding values of type ['a]. *)
  type 'a t

  (** [create ()] is an empty unbounded queue. *)
  val create : unit -> 'a t

  (** [push q v]: [v] is added immediately to [q].

      Note that pushing never needs to wait for room to be available inside the
      pipe. As a result, [push] returns [unit]. This is unlike {!Bounded.push},
      where {!Bounded.push} may need to wait for some space to be freed inside
      the pipe and thus returns [unit Lwt.t].

      @raise {!Closed} if [q] is closed. *)
  val push : 'a t -> 'a -> unit

  (** [pop q] is a promise that is pending until there is at least one element in
    [q]. When this happens an element is removed and the promise is fulfilled
    with it.

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

  (** [pop_all q] is a promise that is pending until there is at least one element
    in [q]. When this happens, all the elements of [q] are removed and the
    promise is fulfilled with the list of elements (in the order in which they
    were inserted).

    If there is already one or more elements in [q] when the call is made, the
    elements are removed immediately and an already resolved promise is
    returned.

    @raise {!Closed} if [q] is empty and closed. More specifically, the promise
    is rejected with {!Closed} if [q] is empty and closed.

    In practice, this function returns a promise that either:
    - is pending and will resolve with a single-element list,
    - is already resolved with a list of at least one element,
    - or will be rejected with {!Closed}. *)
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

  (** [peek_all_now q] returns the elements in the [q] (oldest first), or [[]]
      if empty. It does not remove elements from [q].

      @raise {!Closed} if [q] is empty and closed. *)
  val peek_all_now : 'a t -> 'a list

  (** [pop_now q] may remove and return the first element in [q] if
    [q] contains at least one element.

    @raise [Closed] if [q] is closed. *)
  val pop_now : 'a t -> 'a option

  (** [length q] is the number of elements in [q]. *)
  val length : 'a t -> int

  (** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)
  val is_empty : 'a t -> bool

  (** [close q] the write-end of [q]:

    - Future write attempts will fail with {!Closed}.
    - If there is data left in the pipe, then future read attempts will be
      resolved until the remaining data is drained, after which further reads
      will fail with {!Closed}.
    - If there is no data left in the pipe, then pending and future reads will
      fail with {!Closed}.

    The [close] function is idempotent. *)
  val close : 'a t -> unit

  (** [is_closed q] is [true] if [close q] has been called.
    It is [false] otherwise. *)
  val is_closed : 'a t -> bool
end

module Maybe_bounded : sig
  (** [Maybe_bounded] are pipes that may or may not be bounded. This is decided
       when [create]ing the pipe and can be queried with the [bounded] function.
    *)

  (** Type of queues holding values of type ['a]. *)
  type 'a t

  (** [create ~bound:(max_size, compute_size) ()] is an empty queue that can
      hold max [bound] "bytes" of data, using [compute_size] to compute the
      number of "bytes" in a datum. I.e., it is equivalent to
      [Bounded.create ~max_size ~compute_size] but the functions below make no
      assumptions about the bound leading to a slightly different interface and
      potentially worse performances.

      [create ()], with the [bound] argument no set, is an empty queue that is
      unbounded. I.e., it is equivalent to [Unbounded.create ()] but the
      functions below make no assumptions about the bound leading to a slightly
      different interface and potentially worse performances. *)
  val create : ?bound:int * ('a -> int) -> unit -> 'a t

  (** [bounded t] is [true] iff [t] was [create]d with a set [bound]. *)
  val bounded : 'a t -> bool

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

  (** [peek_all_now q] returns the elements in the [q] (oldest first), or [[]]
      if empty. It does not remove elements from [q].

      @raise {!Closed} if [q] is empty and closed. *)
  val peek_all_now : 'a t -> 'a list

  (** [push_now q v] either
      - adds [v] at the ends of [q] immediately and returns [true], or
      - if [q] is full, returns [false].

      @raise [Closed] if [q] is closed. *)
  val push_now : 'a t -> 'a -> bool

  (** [pop_now q] may remove and return the first element in [q] if
    [q] contains at least one element.

    @raise [Closed] if [q] is closed. *)
  val pop_now : 'a t -> 'a option

  (** [length q] is the number of elements in [q]. *)
  val length : 'a t -> int

  (** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)
  val is_empty : 'a t -> bool

  (** [close q] the write-end of [q]:

    - Pending and future write attempts will fail with {!Closed}.
    - If there is data left in the pipe, then future read attempts will be
      resolved until the remaining data is drained, after which further reads
      will fail with {!Closed}.
    - If there is no data left in the pipe, then pending and future reads will
      fail with {!Closed}.

    The [close] function is idempotent. *)
  val close : 'a t -> unit

  val is_closed : 'a t -> bool

  (** The number of bytes used in the internal representation to hold an element
    in the queue. *)
  val push_overhead : int
end
