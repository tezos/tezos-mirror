(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** {1 List}

    A replacement for {!Stdlib.List} which:
    - replaces the exception-raising functions by exception-safe variants,
    - provides Lwt-, result- and Lwt-result-aware traversors.

    [List] is intended to shadow both {!Stdlib.List} and {!Lwt_list}. *)

(** {2 Basics}

    Checkout {!Lwtreslib} for an introduction to the naming and semantic
    convention of Lwtreslib. In a nutshell:
    - Stdlib functions that raise exceptions are replaced by safe variants
      (typically returning [option]).
    - The [_e] suffix is for result-aware traversors ("e" stands for "error"),
      [_s] and [_p] are for Lwt-aware, and [_es] and [_ep] are for
      Lwt-result-aware.
    - [_e], [_s], and [_es] traversors are {i fail-early}: they stop traversal
      as soon as a failure ([Error] or [Fail]) occurs; [_p] and [_ep]
      traversors are {i best-effort}: they only resolve once all of the
      intermediate promises have, even if a failure occurs. *)

(** {2 Double-traversal and combine}

    Note that double-list traversors ([iter2], [map2], etc., and also [combine])
    take an additional [when_different_lengths] parameter. This is to control
    the error that is returned when the two lists passed as arguments have
    different lengths.

    This mechanism is a replacement for {!Stdlib.List.iter2} (etc.) raising
    [Invalid_argument].

    Note that, as per the fail-early behaviour mentioned above, [_e], [_s], and
    [_es] traversors will have already processed the common-prefix before the
    error is returned.

    Because the best-effort behaviour of [_p] and [_ep] is unsatisfying for this
    failure case, double parallel traversors are omitted from this library.
    (Specifically, it is not obvious whether nor how the
    [when_different_lengths] error should be composed with the other errors.)

    To obtain a different behaviour for sequential traversors, or to process
    two lists in parallel, you can use {!combine} or any of the alternatives
    that handles the error differently: {!combine_drop},
    {!combine_with_leftovers}. Finally, the {!rev_combine} is provided to allow
    to avoid multiple-reversing.

    {3 Special considerations}

    Because they traverse the list from right-to-left, the {!fold_right2}
    function and all its variants fail with [when_different_lengths] before any
    of the processing starts. Whilst this is still within the fail-early
    behaviour, it may be surprising enough that it requires mentioning here.

    Because they may return early, {!for_all2} and {!exists2} and all their
    variants may return [Ok _] even though the arguments have different lengths.
*)

module type S = sig
  (** {3 Trivial values} *)

  type 'a t = 'a list = [] | ( :: ) of 'a * 'a list

  (** in-monad, preallocated nil *)

  (** [nil] is [[]] *)
  val nil : 'a list

  (** [nil_e] is [Ok []] *)
  val nil_e : ('a list, 'trace) result

  (** [nil_s] is [Lwt.return_nil] *)
  val nil_s : 'a list Lwt.t

  (** [nil_es] is [Lwt.return (Ok [])] *)
  val nil_es : ('a list, 'trace) result Lwt.t

  (** {3 Safe wrappers}

      Shadowing unsafe functions to avoid all exceptions. *)

  (** {4 Safe lookups, scans, retrievals}

      Return option rather than raise [Not_found], [Failure _], or
      [Invalid_argument _] *)

  (** [hd xs] is the head (first element) of the list or [None] if the list is
      empty. *)
  val hd : 'a list -> 'a option

  (** [tl xs] is the tail of the list (the whole list except the first element)
      or [None] if the list is empty. *)
  val tl : 'a list -> 'a list option

  (** [nth xs n] is the [n]th element of the list or [None] if the list has
      fewer than [n] elements.

      [nth xs 0 = hd xs] *)
  val nth : 'a list -> int -> 'a option

  (** [nth_opt] is an alias for [nth] provided for backwards compatibility. *)
  val nth_opt : 'a list -> int -> 'a option

  (** [last x xs] is the last element of the list [xs] or [x] if [xs] is empty.

      The primary intended use for [last] is after destructing a list:
      [match l with | None -> … | Some x :: xs -> last x xs]
      but it can also be used for a default value:
      [last default_value_if_empty xs]. *)
  val last : 'a -> 'a list -> 'a

  (** [last_opt xs] is the last element of the list [xs] or [None] if the list
      [xs] is empty. *)
  val last_opt : 'a list -> 'a option

  (** [find predicate xs] is the first element [x] of the list [xs] such that
      [predicate x] is [true] or [None] if the list [xs] has no such element. *)
  val find : ('a -> bool) -> 'a list -> 'a option

  (** [find_opt] is an alias for [find] provided for backwards compatibility. *)
  val find_opt : ('a -> bool) -> 'a list -> 'a option

  (** [mem ~equal a l] is [true] iff there is an element [e] of [l] such that
      [equal a e]. *)
  val mem : equal:('a -> 'a -> bool) -> 'a -> 'a list -> bool

  (** [assoc ~equal k kvs] is [Some v] such that [(k', v)] is the first pair in
      the list such that [equal k' k] or [None] if the list contains no such
      pair. *)
  val assoc : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

  (** [assoc_opt] is an alias for [assoc] provided for backwards compatibility. *)
  val assoc_opt : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

  (** [assq k kvs] is the same as [assoc ~equal:Stdlib.( == ) k kvs]: it uses
      the physical equality. *)
  val assq : 'a -> ('a * 'b) list -> 'b option

  (** [assq_opt] is an alias for [assq] provided for backwards compatibility. *)
  val assq_opt : 'a -> ('a * 'b) list -> 'b option

  (** [mem_assoc ~equal k l] is equivalent to
      [Option.is_some @@ assoc ~equal k l]. *)
  val mem_assoc : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> bool

  (** [mem_assq k l] is [mem_assoc ~equal:Stdlib.( == ) k l]. *)
  val mem_assq : 'a -> ('a * 'b) list -> bool

  (** [remove_assoc ~equal k l] is [l] without the first element [(k', _)] such
      that [equal k k']. *)
  val remove_assoc :
    equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> ('a * 'b) list

  (** [remove_assoq k l] is [remove_assoc ~equal:Stdlib.( == ) k l]. *)
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list

  (** {4 Initialisation} *)

  (** [init ~when_negative_length n f] is [Error when_negative_length] if [n] is
      strictly negative and [Ok (Stdlib.List.init n f)] otherwise. *)
  val init :
    when_negative_length:'trace ->
    int ->
    (int -> 'a) ->
    ('a list, 'trace) result

  (** {4 Basic traversal} *)

  val length : 'a list -> int

  val rev : 'a list -> 'a list

  val concat : 'a list list -> 'a list

  val append : 'a list -> 'a list -> 'a list

  val rev_append : 'a list -> 'a list -> 'a list

  val flatten : 'a list list -> 'a list

  (** {4 Double-list traversals}

      These safe-wrappers take an explicit value to handle the case of lists of
      unequal length.
  *)

  (** [combine ~when_different_lengths l1 l2] is either
      - [Error when_different_lengths] if [List.length l1 <> List.length l2]
      - a list of pairs of elements from [l1] and [l2]

      E.g., [combine ~when_different_lengths [] [] = Ok []]

      E.g., [combine ~when_different_lengths [1; 2] ['a'; 'b'] = Ok [(1,'a'); (2, 'b')]]

      E.g., [combine ~when_different_lengths:() [1] [] = Error ()]

      Note: [combine ~when_different_lengths l1 l2] is equivalent to
      [try Ok (Stdlib.List.combine l1 l2)
       with Invalid_argument _ -> when_different_lengths]

      The same equivalence almost holds for the other double traversors below.
      The notable difference is if the functions passed as argument to the
      traversors raise the [Invalid_argument _] exception. *)
  val combine :
    when_different_lengths:'trace ->
    'a list ->
    'b list ->
    (('a * 'b) list, 'trace) result

  (** [rev_combine ~when_different_lengths xs ys] is
      [rev (combine ~when_different_lengths xs ys)] but more efficient. *)
  val rev_combine :
    when_different_lengths:'trace ->
    'a list ->
    'b list ->
    (('a * 'b) list, 'trace) result

  val split : ('a * 'b) list -> 'a list * 'b list

  val iter2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> unit) ->
    'a list ->
    'b list ->
    (unit, 'trace) result

  val map2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result

  val rev_map2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result

  val fold_left2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'a) ->
    'a ->
    'b list ->
    'c list ->
    ('a, 'trace) result

  (** This function is not tail-recursive *)
  val fold_right2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'c) ->
    'a list ->
    'b list ->
    'c ->
    ('c, 'trace) result

  (** [fold_left_map f a xs] is a combination of [fold_left] and [map] that maps
      over all elements of [xs] and threads an accumulator with initial value [a]
      through calls to [f]. *)
  val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

  val for_all2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool) ->
    'a list ->
    'b list ->
    (bool, 'trace) result

  val exists2 :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool) ->
    'a list ->
    'b list ->
    (bool, 'trace) result

  (** {3 Monad-aware variants}

      The functions below are strict extensions of the standard {!Stdlib.List}
      module. It is for result-, lwt- and lwt-result-aware variants. The meaning
      of the suffix is as described above, in {!Lwtreslib}, and in {!Sigs.Seq}. *)

  (** {4 Initialisation variants}

      Note that for asynchronous variants ([_s], [_es], [_p], and [_ep]), if the
      length parameter is negative, then the promise is returned already
      fulfilled with [Error when_different_lengths]. *)

  val init_e :
    when_negative_length:'trace ->
    int ->
    (int -> ('a, 'trace) result) ->
    ('a list, 'trace) result

  val init_s :
    when_negative_length:'trace ->
    int ->
    (int -> 'a Lwt.t) ->
    ('a list, 'trace) result Lwt.t

  val init_es :
    when_negative_length:'trace ->
    int ->
    (int -> ('a, 'trace) result Lwt.t) ->
    ('a list, 'trace) result Lwt.t

  val init_ep :
    when_negative_length:'error ->
    int ->
    (int -> ('a, 'error) result Lwt.t) ->
    ('a list, 'error list) result Lwt.t

  val init_p :
    when_negative_length:'trace ->
    int ->
    (int -> 'a Lwt.t) ->
    ('a list, 'trace) result Lwt.t

  (** {4 Query variants} *)

  val find_e :
    ('a -> (bool, 'trace) result) -> 'a list -> ('a option, 'trace) result

  val find_s : ('a -> bool Lwt.t) -> 'a list -> 'a option Lwt.t

  val find_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a option, 'trace) result Lwt.t

  val filter : ('a -> bool) -> 'a list -> 'a list

  (** [rev_filter f l] is [rev (filter f l)] but more efficient. *)
  val rev_filter : ('a -> bool) -> 'a list -> 'a list

  val rev_filter_some : 'a option list -> 'a list

  val filter_some : 'a option list -> 'a list

  val rev_filter_ok : ('a, 'b) result list -> 'a list

  val filter_ok : ('a, 'b) result list -> 'a list

  val rev_filter_error : ('a, 'b) result list -> 'b list

  val filter_error : ('a, 'b) result list -> 'b list

  val rev_filter_e :
    ('a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

  val filter_e :
    ('a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

  val rev_filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

  val filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

  val rev_filter_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a list, 'trace) result Lwt.t

  val filter_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a list, 'trace) result Lwt.t

  val filter_ep :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a list, 'trace list) result Lwt.t

  val filter_p : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

  val rev_partition : ('a -> bool) -> 'a list -> 'a list * 'a list

  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

  val rev_partition_result : ('a, 'b) result list -> 'a list * 'b list

  val partition_result : ('a, 'b) result list -> 'a list * 'b list

  val rev_partition_e :
    ('a -> (bool, 'trace) result) ->
    'a list ->
    ('a list * 'a list, 'trace) result

  val partition_e :
    ('a -> (bool, 'trace) result) ->
    'a list ->
    ('a list * 'a list, 'trace) result

  val rev_partition_s :
    ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

  val partition_s : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

  val rev_partition_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a list * 'a list, 'trace) result Lwt.t

  val partition_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a list * 'a list, 'trace) result Lwt.t

  val partition_ep :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    ('a list * 'a list, 'trace list) result Lwt.t

  val partition_p : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

  (** {4 Traversal variants} *)
  val iter : ('a -> unit) -> 'a list -> unit

  val iter_e : ('a -> (unit, 'trace) result) -> 'a list -> (unit, 'trace) result

  val iter_s : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t

  val iter_es :
    ('a -> (unit, 'trace) result Lwt.t) ->
    'a list ->
    (unit, 'trace) result Lwt.t

  val iter_ep :
    ('a -> (unit, 'trace) result Lwt.t) ->
    'a list ->
    (unit, 'trace list) result Lwt.t

  val iter_p : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t

  val iteri : (int -> 'a -> unit) -> 'a list -> unit

  val iteri_e :
    (int -> 'a -> (unit, 'trace) result) -> 'a list -> (unit, 'trace) result

  val iteri_s : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

  val iteri_es :
    (int -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a list ->
    (unit, 'trace) result Lwt.t

  val iteri_ep :
    (int -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a list ->
    (unit, 'trace list) result Lwt.t

  val iteri_p : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

  val map : ('a -> 'b) -> 'a list -> 'b list

  val map_e : ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

  val map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val map_es :
    ('a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace) result Lwt.t

  val map_ep :
    ('a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace list) result Lwt.t

  val map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  val mapi_e :
    (int -> 'a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

  val mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val mapi_es :
    (int -> 'a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace) result Lwt.t

  val mapi_ep :
    (int -> 'a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace list) result Lwt.t

  val mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val rev_map : ('a -> 'b) -> 'a list -> 'b list

  val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

  val rev_map_e :
    ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

  val rev_map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val rev_map_es :
    ('a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace) result Lwt.t

  val rev_map_ep :
    ('a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace list) result Lwt.t

  val rev_map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val rev_mapi_e :
    (int -> 'a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

  val rev_mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val rev_mapi_es :
    (int -> 'a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace) result Lwt.t

  val rev_mapi_ep :
    (int -> 'a -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace list) result Lwt.t

  val rev_mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

  val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val rev_filter_map_e :
    ('a -> ('b option, 'trace) result) -> 'a list -> ('b list, 'trace) result

  val filter_map_e :
    ('a -> ('b option, 'trace) result) -> 'a list -> ('b list, 'trace) result

  val rev_filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list

  val filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

  val rev_filter_map_es :
    ('a -> ('b option, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace) result Lwt.t

  val filter_map_es :
    ('a -> ('b option, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace) result Lwt.t

  val filter_map_ep :
    ('a -> ('b option, 'trace) result Lwt.t) ->
    'a list ->
    ('b list, 'trace list) result Lwt.t

  val filter_map_p : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list

  val concat_map_s : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t

  val concat_map_e :
    ('a -> ('b list, 'error) result) -> 'a list -> ('b list, 'error) result

  val concat_map_es :
    ('a -> ('b list, 'error) result Lwt.t) ->
    'a list ->
    ('b list, 'error) result Lwt.t

  val concat_map_p : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t

  val concat_map_ep :
    ('a -> ('b list, 'error) result Lwt.t) ->
    'a list ->
    ('b list, 'error list) result Lwt.t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val fold_left_e :
    ('a -> 'b -> ('a, 'trace) result) -> 'a -> 'b list -> ('a, 'trace) result

  val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

  val fold_left_es :
    ('a -> 'b -> ('a, 'trace) result Lwt.t) ->
    'a ->
    'b list ->
    ('a, 'trace) result Lwt.t

  (** [fold_left_map_e f a xs] is a combination of [fold_left_e] and [map_e] that
      maps over all elements of [xs] and threads an accumulator with initial
      value [a] through calls to [f]. The list is traversed from left to right
      and the first encountered error is returned. *)
  val fold_left_map_e :
    ('a -> 'b -> ('a * 'c, 'trace) result) ->
    'a ->
    'b list ->
    ('a * 'c list, 'trace) result

  (** [fold_left_map_s f a xs] is a combination of [fold_left_s] and [map_s] that
      maps over all elements of [xs] and threads an accumulator with initial
      value [a] through calls to [f]. *)
  val fold_left_map_s :
    ('a -> 'b -> ('a * 'c) Lwt.t) -> 'a -> 'b list -> ('a * 'c list) Lwt.t

  (** [fold_left_map_es f a xs] is a combination of [fold_left_es] and [map_es] that
      maps over all elements of [xs] and threads an accumulator with initial
      value [a] through calls to [f]. The list is traversed from left to right
      and the first encountered error is returned. *)
  val fold_left_map_es :
    ('a -> 'b -> ('a * 'c, 'trace) result Lwt.t) ->
    'a ->
    'b list ->
    ('a * 'c list, 'trace) result Lwt.t

  val fold_left_i : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val fold_left_i_e :
    (int -> 'a -> 'b -> ('a, 'trace) result) ->
    'a ->
    'b list ->
    ('a, 'trace) result

  val fold_left_i_s : (int -> 'a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

  val fold_left_i_es :
    (int -> 'a -> 'b -> ('a, 'trace) result Lwt.t) ->
    'a ->
    'b list ->
    ('a, 'trace) result Lwt.t

  (** This function is not tail-recursive *)
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

  (** This function is not tail-recursive *)
  val fold_right_e :
    ('a -> 'b -> ('b, 'trace) result) -> 'a list -> 'b -> ('b, 'trace) result

  (** This function is not tail-recursive *)
  val fold_right_s : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b -> 'b Lwt.t

  (** This function is not tail-recursive *)
  val fold_right_es :
    ('a -> 'b -> ('b, 'trace) result Lwt.t) ->
    'a list ->
    'b ->
    ('b, 'trace) result Lwt.t

  (** {4 Double-traversal variants}

      As mentioned above, there are no [_p] and [_ep] double-traversors. Use
      {!combine} (and variants) to circumvent this. *)

  val iter2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> (unit, 'trace) result) ->
    'a list ->
    'b list ->
    (unit, 'trace) result

  val iter2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> unit Lwt.t) ->
    'a list ->
    'b list ->
    (unit, 'trace) result Lwt.t

  val iter2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> (unit, 'trace) result Lwt.t) ->
    'a list ->
    'b list ->
    (unit, 'trace) result Lwt.t

  val map2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result

  val map2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c Lwt.t) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result Lwt.t

  val map2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result Lwt.t) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result Lwt.t

  val rev_map2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result

  val rev_map2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c Lwt.t) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result Lwt.t

  val rev_map2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> ('c, 'trace) result Lwt.t) ->
    'a list ->
    'b list ->
    ('c list, 'trace) result Lwt.t

  val fold_left2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('a, 'trace) result) ->
    'a ->
    'b list ->
    'c list ->
    ('a, 'trace) result

  val fold_left2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'a Lwt.t) ->
    'a ->
    'b list ->
    'c list ->
    ('a, 'trace) result Lwt.t

  val fold_left2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('a, 'trace) result Lwt.t) ->
    'a ->
    'b list ->
    'c list ->
    ('a, 'trace) result Lwt.t

  (** This function is not tail-recursive *)
  val fold_right2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('c, 'trace) result) ->
    'a list ->
    'b list ->
    'c ->
    ('c, 'trace) result

  (** This function is not tail-recursive *)
  val fold_right2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> 'c Lwt.t) ->
    'a list ->
    'b list ->
    'c ->
    ('c, 'trace) result Lwt.t

  (** This function is not tail-recursive *)
  val fold_right2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> 'c -> ('c, 'trace) result Lwt.t) ->
    'a list ->
    'b list ->
    'c ->
    ('c, 'trace) result Lwt.t

  (** {4 Scanning variants} *)

  val for_all : ('a -> bool) -> 'a list -> bool

  val for_all_e :
    ('a -> (bool, 'trace) result) -> 'a list -> (bool, 'trace) result

  val for_all_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

  val for_all_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    (bool, 'trace) result Lwt.t

  val for_all_ep :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    (bool, 'trace list) result Lwt.t

  val for_all_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

  val exists : ('a -> bool) -> 'a list -> bool

  val exists_e :
    ('a -> (bool, 'trace) result) -> 'a list -> (bool, 'trace) result

  val exists_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

  val exists_es :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    (bool, 'trace) result Lwt.t

  val exists_ep :
    ('a -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    (bool, 'trace list) result Lwt.t

  val exists_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

  (** {4 Double-scanning variants}

      As mentioned above, there are no [_p] and [_ep] double-scanners. Use
      {!combine} (and variants) to circumvent this. *)

  val for_all2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result) ->
    'a list ->
    'b list ->
    (bool, 'trace) result

  val for_all2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool Lwt.t) ->
    'a list ->
    'b list ->
    (bool, 'trace) result Lwt.t

  val for_all2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    'b list ->
    (bool, 'trace) result Lwt.t

  val exists2_e :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result) ->
    'a list ->
    'b list ->
    (bool, 'trace) result

  val exists2_s :
    when_different_lengths:'trace ->
    ('a -> 'b -> bool Lwt.t) ->
    'a list ->
    'b list ->
    (bool, 'trace) result Lwt.t

  val exists2_es :
    when_different_lengths:'trace ->
    ('a -> 'b -> (bool, 'trace) result Lwt.t) ->
    'a list ->
    'b list ->
    (bool, 'trace) result Lwt.t

  (** {3 Combine variants}

      These are primarily intended to be used for preprocessing before applying
      a traversor to the resulting list of pairs. They give alternatives to the
      [when_different_lengths] mechanism of the immediate double-traversors
      above.

      In case the semantic of, say, [map2_es] was unsatisfying, one can use
      [map_es] on a [combine]-preprocessed pair of lists. The different variants
      of [combine] give different approaches to different-length handling. *)

  (** [combine_drop ll lr] is a list [l] of pairs of elements taken from the
      common-length prefix of [ll] and [lr]. The suffix of whichever list is
      longer (if any) is dropped.

      More formally [nth l n] is:
      - [None] if [n >= min (length ll) (length lr)]
      - [Some (Option.get @@ nth ll n, Option.get @@ nth lr n)] otherwise
      *)
  val combine_drop : 'a list -> 'b list -> ('a * 'b) list

  (** A type like [result] but which is symmetric *)
  type ('a, 'b) left_or_right_list = [`Left of 'a list | `Right of 'b list]

  (** [combine_with_leftovers ll lr] is a tuple [(combined, leftover)]
      where [combined] is [combine_drop ll lr]
      and [leftover] is either [`Left lsuffix] or [`Right rsuffix] depending on
      which of [ll] or [lr] is longer. [leftover] is [None] if the two lists
      have the same length. *)
  val combine_with_leftovers :
    'a list -> 'b list -> ('a * 'b) list * ('a, 'b) left_or_right_list option

  (** {3 compare / equal} *)

  val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

  val compare_lengths : 'a list -> 'b list -> int

  val compare_length_with : 'a list -> int -> int

  val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

  (** {3 Sorting} *)

  val sort : ('a -> 'a -> int) -> 'a list -> 'a list

  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list

  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list

  val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

  (** {3 conversion} *)

  val to_seq : 'a list -> 'a Stdlib.Seq.t

  val of_seq : 'a Stdlib.Seq.t -> 'a list
end
