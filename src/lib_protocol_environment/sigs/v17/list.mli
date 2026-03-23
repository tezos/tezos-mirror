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

(** {2 API} *)

(** {3 The list type} *)
type 'a t = 'a list = [] | ( :: ) of 'a * 'a list

  (** {3 Constructors and some such} *)

(** [nil] is [[]] *)
val nil : 'a list

(** [nil_e] is [Ok []] *)
val nil_e : ('a list, 'trace) result

(** [nil_s] is [Lwt.return_nil] *)
val nil_s : 'a list Lwt.t

(** [nil_es] is [Lwt.return (Ok [])] *)
val nil_es : ('a list, 'trace) result Lwt.t

(** [cons x xs] is [x :: xs] *)
val cons : 'a -> 'a list -> 'a list

(** [is_empty xs] is [true] iff [xs] is [[]] *)
val is_empty : 'a list -> bool

(** {3 Safe wrappers}

    This part of the module simply shadows some functions from {!Stdlib.List}
    with exceptionless variants. As per the design principles of Lwtreslib,

    - functions which may fail with [Not_found] or otherwise from
      unavailability of data return an [option] instead,
    - function which may fail with [Invalid_argument _] or otherwise from
      malformedness of input receive an additional parameter to return as an
      [Error] instead,
    - functions which perform polymorphic comparison receive an additional
      parameter for monomorphic comparison instead. *)

(** [hd xs] is the head (first element) of the list or [None] if the list is
    empty. *)
val hd : 'a list -> 'a option

(** [tl xs] is the tail of the list (the whole list except the first element)
    or [None] if the list is empty. *)
val tl : 'a list -> 'a list option

(** [nth xs n] is the [n]th element of the list or [None] if the list has
    fewer than [n] elements.

    For example, [nth xs 0 = hd xs] and [nth ['x'; 'y'] 1 = Some 'y']. *)
val nth : 'a list -> int -> 'a option

(** [nth_opt] is an alias for [nth] provided for compatibility with
      {!Stdlib.List}. *)
val nth_opt : 'a list -> int -> 'a option

(** [last x xs] is the last element of the list [xs] or [x] if [xs] is empty.

    The primary intended use for [last] is after destructing a list:
      [match l with | [] -> … | x :: xs -> last x xs]
    but it can also be used for a default value:
    [last default_value_if_empty xs]. *)
val last : 'a -> 'a list -> 'a

(** [last_opt xs] is the last element of the list [xs] or [None] if the list
    [xs] is empty. *)
val last_opt : 'a list -> 'a option

(** [find predicate xs] is the first element [x] of the list [xs] such that
    [predicate x] is [true] or [None] if the list [xs] has no such element. *)
val find : ('a -> bool) -> 'a list -> 'a option

(** [find_opt] is an alias for [find] provided for compatibility with
    {!Stdlib.List}. *)
val find_opt : ('a -> bool) -> 'a list -> 'a option

(** [find_index f xs] returns [Some i], where [i] is the index of the first
    element of the list [xs] that satisfies [f x], if there is such an
    element. It returns [None] if there is no such element.

    E.g., [find_index (fun x -> x >= 10) [1; 5; 10; 15]] is [Some 2]. *)
val find_index : ('a -> bool) -> 'a list -> int option

(** [find_map f xs] applies [f] to each of the elements of [xs] until it
    returns [Some _] at which point it is returned. If no such elements are
    found then it returns [None].

    Note that it only applies [f] to a prefix of [xs]. It doesn't apply [f] to
    the elements of [xs] which are after the found element. Consequently,
    [find_map f xs] has better performance and a different semantic than
    calling [map] and [find] separately. *)
val find_map : ('a -> 'b option) -> 'a list -> 'b option

(** [find_mapi f xs] applies [f] to each of the elements of [xs] and their
    index (starting from 0) until it returns [Some _] at which point it is
    returned. If no such element is found then it returns [None].

    E.g., [find_mapi (fun i x -> if i >= 2 then Some x else None) ['a'; 'b';
    'c'; 'd']] is [Some 'c']. *)
val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option

(** [mem ~equal a l] is [true] iff there is an element [e] of [l] such that
    [equal a e]. *)
val mem : equal:('a -> 'a -> bool) -> 'a -> 'a list -> bool

(** [assoc ~equal k kvs] is [Some v] such that [(k', v)] is the first pair in
    the list such that [equal k' k] or [None] if the list contains no such
    pair. *)
val assoc : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

(** [assoc_opt] is an alias for [assoc] provided for compatibility with
    {!Stdlib.List}. *)
val assoc_opt : equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

(** [assq k kvs] is the same as [assoc ~equal:Stdlib.( == ) k kvs]: it uses
    the physical equality. *)
val assq : 'a -> ('a * 'b) list -> 'b option

(** [assq_opt] is an alias for [assq] provided for compatibility with
    {!Stdlib.List}. *)
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

(** {3 Initialisation} *)

(** [init ~when_negative_length n f] is a list of [n] elements [f 0], [f 1],
    etc.

    If [n] is negative, it is [Error when_negative_length] instead. *)
val init :
  when_negative_length:'trace ->
  int ->
  (int -> 'a) ->
  ('a list, 'trace) result

(** {3 Basic traversal} *)

(** [length xs] is the number of elements in [xs].

    [length []] is [0], [length ['x']] is [1], etc. *)
val length : 'a list -> int

(** [rev xs] is a list with the elements appearing in the reverse order as in
    [xs].

    [rev ['x'; 'y']] is ['y'; 'x'] *)
val rev : 'a list -> 'a list

(** [concat xs] is a list containing the elements of the elements of [xs].

    [concat [['x'; 'y']; ['a'; 'b']]] is [['x'; 'y'; 'a'; 'b']] *)
val concat : 'a list list -> 'a list

(** [append xs ys] is a list containing the elements of [xs] and the elements
    of [ys], in this order.

    [concat ['x'; 'y'] ['a'; 'b']] is [['x'; 'y'; 'a'; 'b']] *)
val append : 'a list -> 'a list -> 'a list

(** [rev_append xs ys] is [append (rev xs) ys] but more efficient. In other
    words, [rev_append xs ys] is a list containing the elements of xs in
    reverse order followed by the elements of [ys].

    There are two main use-cases for [rev_append]. First, you should use
    [rev_append] when the order of elements is unimportant. In this case you
    simply replace [append xs ys] with [rev_append xs ys].

    Second, you can use [rev_append] on an already reversed list. You may
    obtain an already reversed list from any of the other [rev_*] functions of
    this module, or simply via your own traversal. In this case, you replace,
    say, [append (map f xs) ys] with [rev_append (rev_map f xs) ys]. *)
val rev_append : 'a list -> 'a list -> 'a list

  (** [flatten] is an alias for {!concat}. *)
val flatten : 'a list list -> 'a list

(** {3 Double-list traversals}

    These safe-wrappers take an explicit value to handle the case of lists of
    unequal length. This value is passed as a named parameter:
    [when_different_lengths].

    Note that the traversal function passed as argument (if any) is applied to
    the common prefix of the two lists, even if they are of different lengths.
    E.g., in [map2 f ['x', 'y'] ['a']] the call [f 'x' 'a'] is made and all
    its side-effects are performed before the value
    [Error when_different_lengths] is returned
*)

(** [combine ~when_different_lengths l1 l2] is either
    - [Error when_different_lengths] if [List.length l1 <> List.length l2]
    - a list of pairs of elements from [l1] and [l2]

      E.g., [combine ~when_different_lengths [] []] is [Ok []]

      E.g., [combine ~when_different_lengths [1; 2] ['a'; 'b']] is [Ok [(1,'a'); (2, 'b')]]

      E.g., [combine ~when_different_lengths:"wrong" [1] []] is [Error "wrong"]

    Note: [combine ~when_different_lengths l1 l2] is equivalent to
      [try Ok (Stdlib.List.combine l1 l2) with Invalid_argument _ -> when_different_lengths]

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

(** [split xs] is [(List.map fst xs, List.map snd xs)] but more efficient. *)
val split : ('a * 'b) list -> 'a list * 'b list

(** [iter2 ~when_different_lengths f xs ys] is [f x0 y0; f x1 y1; …].

    Remember that, even if the lists are of different lengths, the function
    [f] is applied to the common prefix of [xs] and [ys]. This is true for
    other traversals, but especially relevant to [iter] which is commonly used
    for side-effects. *)
val iter2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> unit) ->
  'a list ->
  'b list ->
  (unit, 'trace) result

(** [map2 ~when_different_lengths f xs ys] is a list with elements [f x0 y0],
    [f x1 y1], etc.

    Remember that, even if the lists are of different lengths, the function
    [f] is applied to the common prefix of [xs] and [ys]. Beware of
    side-effects and computational cost. *)
val map2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result

(** [rev_map2 ~when_different_lengths f xs ys] is
    [Result.map rev @@ map2 ~when_different_lengths f xs ys] but more
    efficient.

    Remember that, even if the lists are of different lengths, the function
    [f] is applied to the common prefix of [xs] and [ys]. Beware of
    side-effects and computational cost. *)
val rev_map2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c) ->
  'a list ->
  'b list ->
  ('c list, 'trace) result

(** [fold_left2 ~when_different_lengths f init xs ys] is
    [… (f (f init x0 y0) x1 y1)].

    Remember that, even if the lists are of different lengths, the function
    [f] is applied to the common prefix of [xs] and [ys]. Beware of
    side-effects and computational cost. *)
val fold_left2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> 'c -> 'a) ->
  'a ->
  'b list ->
  'c list ->
  ('a, 'trace) result

(** [for_all2 ~when_different_lengths f xs ys] is
    [f x0 y0 && f x1 y1 && …].

    The function stops early if it encounters elements [xn], [yn] such that [f
    xn yn] is [false]. (This is consistent with the short-circuit, lazy
    evaluation strategy of [&&] in the description above.)

    Also note that, if such an element is found in the common prefix of [xs]
    and [ys], then the function returns [Ok false] even if [xs] and [ys] are
    of different lengths.

    Examples:

    [for_all2 ~when_different_lengths (=) [] []] is [Ok true]

    [for_all2 ~when_different_lengths (=) ['x'] ['a']] is [Ok false]

    [for_all2 ~when_different_lengths (=) ['x'; 'y'] ['a']] is [Ok false]

    [for_all2 ~when_different_lengths (=) ['x'] ['x']] is [Ok true]

    [for_all2 ~when_different_lengths (=) ['x'; 'y'] ['x']] is [Error when_different_lengths]

    [for_all2 ~when_different_lengths (=) ['x'; 'y'] ['x'; 'b']] is [Ok false]

    [for_all2 ~when_different_lengths (=) ['x'; 'y'] ['x'; 'y'; 'c']] is
    [Error when_different_lengths]

    Remember that, when it returns [Error when_different_lengths], the
    function [f] has already been applied to the common prefix of [xs] and
    [ys]. Beware of side-effects and computational cost. *)
val for_all2 :
  when_different_lengths:'trace ->
  ('a -> 'b -> bool) ->
  'a list ->
  'b list ->
  (bool, 'trace) result

(** [exists2 ~when_different_lengths f xs ys] is
    [f x0 y0 || f x1 y1 || …].

    The function stops early if it encounters elements [xn], [yn] such that [f
    xn yn] is [true]. (This is consistent with the short-circuit, lazy
    evaluation strategy of [||] in the description above.)

    Also note that, if such an element is found in the common prefix of [xs]
    and [ys], then the function returns [Ok true] even if [xs] and [ys] are of
    different lengths.

    Examples:

    [exists2 ~when_different_lengths (=) [] []] is [Ok false]

    [exists2 ~when_different_lengths (=) ['x'] ['a']] is [Ok false]

    [exists2 ~when_different_lengths (=) ['x'; 'y'] ['a']] is [Error when_different_lengths]

    [exists2 ~when_different_lengths (=) ['x'] ['x']] is [Ok true]

    [exists2 ~when_different_lengths (=) ['x'; 'y'] ['x']] is [Ok true]

    Remember that, when it returns [Error when_different_lengths], the
    function [f] has already been applied to the common prefix of [xs] and
    [ys]. Beware of side-effects and computational cost. *)
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

(** {3 Initialisation variants}

    Note that for asynchronous variants ([_s], [_es], [_p], and [_ep]), if the
    length parameter is negative, then the promise is returned already
    fulfilled with [Error when_different_lengths]. *)

(** [init_e] is a Result-aware variant of {!init}. *)
val init_e :
  when_negative_length:'trace ->
  int ->
  (int -> ('a, 'trace) result) ->
  ('a list, 'trace) result

(** [init_s] is an Lwt-aware variant of {!init}. *)
val init_s :
  when_negative_length:'trace ->
  int ->
  (int -> 'a Lwt.t) ->
  ('a list, 'trace) result Lwt.t

(** [init_es] is an Lwt-Result-aware variant of {!init}. *)
val init_es :
  when_negative_length:'trace ->
  int ->
  (int -> ('a, 'trace) result Lwt.t) ->
  ('a list, 'trace) result Lwt.t

(** [init_p] is a variant of {!init_s} where the promises are evaluated
    concurrently. *)
val init_p :
  when_negative_length:'trace ->
  int ->
  (int -> 'a Lwt.t) ->
  ('a list, 'trace) result Lwt.t

(** {3 Query variants} *)

(** [find_e] is a Result-aware variant of {!find}. *)
val find_e :
  ('a -> (bool, 'trace) result) -> 'a list -> ('a option, 'trace) result

(** [find_s] is an Lwt-aware variant of {!find}. *)
val find_s : ('a -> bool Lwt.t) -> 'a list -> 'a option Lwt.t

(** [find_es] is an Lwt-Result-aware variant of {!find}. *)
val find_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a option, 'trace) result Lwt.t

(** [find_index_e] is a Result-aware variant of {!find_index}. *)
val find_index_e :
  ('a -> (bool, 'trace) result) -> 'a list -> (int option, 'trace) result

(** [find_index_s] is an Lwt-aware variant of {!find_index}. *)
val find_index_s : ('a -> bool Lwt.t) -> 'a list -> int option Lwt.t

(** [find_index_es] is an Lwt-Result-aware variant of {!find_index}. *)
val find_index_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  (int option, 'trace) result Lwt.t

(** [find_map_e] is a Result-aware variant of {!find_map}. *)
val find_map_e :
  ('a -> ('b option, 'trace) result) -> 'a list -> ('b option, 'trace) result

(** [find_map_s] is an Lwt-aware variant of {!find_map}. *)
val find_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b option Lwt.t

(** [find_map_es] is an Lwt-Result-aware variant of {!find_map}. *)
val find_map_es :
  ('a -> ('b option, 'trace) result Lwt.t) ->
  'a list ->
  ('b option, 'trace) result Lwt.t

(** [find_mapi_e] is a Result-aware variant of {!find_mapi}. *)
val find_mapi_e :
  (int -> 'a -> ('b option, 'trace) result) ->
  'a list ->
  ('b option, 'trace) result

(** [find_mapi_s] is an Lwt-aware variant of {!find_mapi}. *)
val find_mapi_s : (int -> 'a -> 'b option Lwt.t) -> 'a list -> 'b option Lwt.t

(** [find_mapi_es] is an Lwt-Result-aware variant of {!find_mapi}. *)
val find_mapi_es :
  (int -> 'a -> ('b option, 'trace) result Lwt.t) ->
  'a list ->
  ('b option, 'trace) result Lwt.t

(** [filter f xs] is the list of all the elements [xn] of [xs] such that
    [f xn] is [true].

    [filter (fun x -> x > 10) [0; 2; 19; 22; -1; 3; 11]] is [[19; 22; 11]] *)
val filter : ('a -> bool) -> 'a list -> 'a list

(** [filteri] is similar to {!filter} but the predicate also receives the
    element's index as an argument. *)
val filteri : (int -> 'a -> bool) -> 'a list -> 'a list

(** [find_all] is an alias for {!filter}. *)
val find_all : ('a -> bool) -> 'a list -> 'a list

(** [rev_filter f l] is [rev (filter f l)] but more efficient. *)
val rev_filter : ('a -> bool) -> 'a list -> 'a list

(** [rev_filteri f l] is [rev (filteri f l)] but more efficient. *)
val rev_filteri : (int -> 'a -> bool) -> 'a list -> 'a list

(** [rev_filter_some xs] is [rev @@ filter_some xs] but more efficient. *)
val rev_filter_some : 'a option list -> 'a list

(** [filter_some] extracts all the payloads of the [Some] variants.
    The order is preserved.

    [filter_some [None; Some 'a'; None; None; Some 'z'; Some 'u']] is
    [['a'; 'z'; 'u']]. *)
val filter_some : 'a option list -> 'a list

(** [rev_filter_ok rs] is [rev @@ filter_ok rs] but more efficient. *)
val rev_filter_ok : ('a, 'b) result list -> 'a list

(** [filter_ok] extracts all the payloads of the [Ok] variants.
    The order is preserved.

    [filter_ok [Error 3; Ok 'a'; Error 3; Error 5; Ok 'z'; Ok 'u']] is
    [['a'; 'z'; 'u']]. *)
val filter_ok : ('a, 'b) result list -> 'a list

(** [rev_filter_error rs] is [rev @@ filter_error rs] but more efficient. *)
val rev_filter_error : ('a, 'b) result list -> 'b list

(** [filter_error] extracts all the payloads of the [Error] variants.
    The order is preserved.

    [filter_ok [Error 3; Ok 'a'; Error 3; Error 5; Ok 'z'; Ok 'u']] is
    [[3; 3; 5]]. *)
val filter_error : ('a, 'b) result list -> 'b list

(** [rev_filter_left es] is [rev @@ filter_left es] but more efficient. *)
val rev_filter_left : ('a, 'b) Either.t list -> 'a list

(** [filter_left] extracts all the payloads of the [Left] variants.
    The order is preserved.

    [filter_left [Right 3; Left 'a'; Right 3; Right 5; Left 'z'; Left 'u']] is
    [['a'; 'z'; 'u']]. *)
val filter_left : ('a, 'b) Either.t list -> 'a list

(** [rev_filter_right es] is [rev @@ filter_right es] but more efficient. *)
val rev_filter_right : ('a, 'b) Either.t list -> 'b list

(** [filter_right] extracts all the payloads of the [Right] variants.
    The order is preserved.

    [filter_right [Right 3; Left 'a'; Right 3; Right 5; Left 'z'; Left 'u']] is
    [[3; 3; 5]]. *)
val filter_right : ('a, 'b) Either.t list -> 'b list

(** [rev_filter_e] is a Result-aware variant of {!rev_filter}. *)
val rev_filter_e :
  ('a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

(** [filter_e] is a Result-aware variant of {!filter}. *)
val filter_e :
  ('a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

(** [rev_filter_s] is an Lwt-aware variant of {!rev_filter}. *)
val rev_filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

(** [filter_s] is an Lwt-aware variant of {!filter}. *)
val filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

(** [rev_filter_es] is an Lwt-Result-aware variant of {!rev_filter}. *)
val rev_filter_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list, 'trace) result Lwt.t

(** [filter_es] is an Lwt-Result-aware variant of {!filter}. *)
val filter_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list, 'trace) result Lwt.t

(** [rev_filteri_e] is a Result-aware variant of {!rev_filteri}. *)
val rev_filteri_e :
  (int -> 'a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

(** [filteri_e] is a Result-aware variant of {!filteri}. *)
val filteri_e :
  (int -> 'a -> (bool, 'trace) result) -> 'a list -> ('a list, 'trace) result

(** [rev_filteri_s] is an Lwt-aware variant of {!rev_filteri}. *)
val rev_filteri_s : (int -> 'a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

(** [filteri_s] is an Lwt-aware variant of {!filteri}. *)
val filteri_s : (int -> 'a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t

(** [rev_filteri_es] is an Lwt-Result-aware variant of {!rev_filteri}. *)
val rev_filteri_es :
  (int -> 'a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list, 'trace) result Lwt.t

(** [filteri_es] is an Lwt-Result-aware variant of {!filteri}. *)
val filteri_es :
  (int -> 'a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list, 'trace) result Lwt.t

(** [rev_partition f xs] is [let rt, rf = partition f xs in (rev rt, rev rf)]
    but more efficient. *)
val rev_partition : ('a -> bool) -> 'a list -> 'a list * 'a list

(** [partition f xs] is a couple of lists [(ts, fs)] where [ts] contains all
    the elements of [xs] such that [f x] is [true] and [fs] contains all the
    elements of [xs] such that [f x] is [false].

    The function [f] is applied once to each element of [xs]. *)
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list

(** [rev_partition_map f xs] is
    [let rt, rf = partition_map f xs in (rev rt, rev rf)]
    but more efficient. *)
val rev_partition_map :
  ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list

(** [partition_map f xs] applies [f] to each of the element of [xs] and
    returns a couple of lists [(ls, rs)] where [ls] contains all
    the [l] such that [f x] is [Left l] and [rs] contains all
    the [r] such that [f x] is [Right r]. *)
val partition_map : ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list

(** [rev_partition_result rs] is [partition_result @@ rev rs] but more
    efficient. *)
val rev_partition_result : ('a, 'b) result list -> 'a list * 'b list

(** [partition_result rs] is a tuple of lists [(os, es)] where [os] contains
    all the payloads of [Ok] variants of [rs] and [es] contains all the
    payloads of [Error] variants of [rs].

    [partition_result rs] is [(filter_ok rs, filter_error rs)] but more
    efficient. *)
val partition_result : ('a, 'b) result list -> 'a list * 'b list

(** [rev_partition_either rs] is [partition_either @@ rev rs] but more
    efficient. *)
val rev_partition_either : ('a, 'b) Either.t list -> 'a list * 'b list

(** [partition_either es] is a tuple of lists [(ls, rs)] where [ls] contains
    all the payloads of [Left] variants of [ls] and [rs] contains all the
    payloads of [Right] variants of [es].

    [partition_either es] is [(filter_left es, filter_right es)] but more
    efficient. *)
val partition_either : ('a, 'b) Either.t list -> 'a list * 'b list

(** [rev_partition_e] is a Result-aware variant of {!rev_partition}. *)
val rev_partition_e :
  ('a -> (bool, 'trace) result) ->
  'a list ->
  ('a list * 'a list, 'trace) result

(** [partition_e] is a Result-aware variant of {!partition}. *)
val partition_e :
  ('a -> (bool, 'trace) result) ->
  'a list ->
  ('a list * 'a list, 'trace) result

(** [rev_partition_s] is an Lwt-aware variant of {!rev_partition}. *)
val rev_partition_s :
  ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

(** [partition_s] is an Lwt-aware variant of {!partition}. *)
val partition_s : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

(** [rev_partition_es] is an Lwt-Result-aware variant of {!rev_partition}. *)
val rev_partition_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list * 'a list, 'trace) result Lwt.t

(** [partition_es] is an Lwt-Result-aware variant of {!partition}. *)
val partition_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  ('a list * 'a list, 'trace) result Lwt.t

(** [partition_p] is a variant of {!partition_s} where the promises are
    evaluated concurrently. *)
val partition_p : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t

(** [rev_partition_map_e] is a Result-aware variant of {!rev_partition_map}. *)
val rev_partition_map_e :
  ('a -> (('b, 'c) Either.t, 'trace) result) ->
  'a list ->
  ('b list * 'c list, 'trace) result

(** [partition_map_e] is a Result-aware variant of {!partition_map}. *)
val partition_map_e :
  ('a -> (('b, 'c) Either.t, 'trace) result) ->
  'a list ->
  ('b list * 'c list, 'trace) result

(** [rev_partition_map_s] is an Lwt-aware variant of {!rev_partition_map}. *)
val rev_partition_map_s :
  ('a -> ('b, 'c) Either.t Lwt.t) -> 'a list -> ('b list * 'c list) Lwt.t

(** [partition_map_s] is an Lwt-aware variant of {!partition_map}. *)
val partition_map_s :
  ('a -> ('b, 'c) Either.t Lwt.t) -> 'a list -> ('b list * 'c list) Lwt.t

(** [rev_partition_map_es] is an Lwt-Result-aware variant of
  {!rev_partition_map}. *)
val rev_partition_map_es :
  ('a -> (('b, 'c) Either.t, 'trace) result Lwt.t) ->
  'a list ->
  ('b list * 'c list, 'trace) result Lwt.t

(** [partition_map_es] is an Lwt-Result-aware variant of {!partition_map}. *)
val partition_map_es :
  ('a -> (('b, 'c) Either.t, 'trace) result Lwt.t) ->
  'a list ->
  ('b list * 'c list, 'trace) result Lwt.t

(** {3 Traversal variants} *)

(** [iter f xs] is [f x0; f x1; …]. *)
val iter : ('a -> unit) -> 'a list -> unit

(** [iter_e] is a Result-aware variant of {!iter}. *)
val iter_e : ('a -> (unit, 'trace) result) -> 'a list -> (unit, 'trace) result

(** [iter_s] is an Lwt-aware variant of {!iter}. *)
val iter_s : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t

(** [iter_es] is an Lwt-Result-aware variant of {!iter}. *)
val iter_es :
  ('a -> (unit, 'trace) result Lwt.t) ->
  'a list ->
  (unit, 'trace) result Lwt.t

(** [iter_p] is a variant of {!iter_s} where the promises are evaluated
    concurrently. *)
val iter_p : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t

(** [iteri f xs] is [f 0 x0; f 1 x1; …]. *)
val iteri : (int -> 'a -> unit) -> 'a list -> unit

(** [iteri_e] is a Result-aware variant of {!iteri}. *)
val iteri_e :
  (int -> 'a -> (unit, 'trace) result) -> 'a list -> (unit, 'trace) result

(** [iteri_s] is an Lwt-aware variant of {!iteri}. *)
val iteri_s : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

(** [iteri_es] is an Lwt-Result-aware variant of {!iteri}. *)
val iteri_es :
  (int -> 'a -> (unit, 'trace) result Lwt.t) ->
  'a list ->
  (unit, 'trace) result Lwt.t

(** [iteri_p] is a variant of {!iteri_s} where the promises are evaluated
    concurrently. *)
val iteri_p : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t

(** [map f xs] is the list [[f x0; f x1; …]]. *)
val map : ('a -> 'b) -> 'a list -> 'b list

(** [map_e] is a Result-aware variant of {!map}. *)
val map_e : ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

(** [map_s] is an Lwt-aware variant of {!map}. *)
val map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [map_es] is an Lwt-Result-aware variant of {!map}. *)
val map_es :
  ('a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

(** [map_p] is a variant of {!map_s} where the promises are evaluated
    concurrently. *)
val map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [mapi f xs] is the list [[f 0 x0; f 1 x1; …]]. *)
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** [mapi_e] is a Result-aware variant of {!mapi}. *)
val mapi_e :
  (int -> 'a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

(** [mapi_s] is an Lwt-aware variant of {!mapi}. *)
val mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [mapi_es] is an Lwt-Result-aware variant of {!mapi}. *)
val mapi_es :
  (int -> 'a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

(** [mapi_p] is a variant of {!mapi_s} where the promises are evaluated
    concurrently. *)
val mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [rev_map f xs] is [rev @@ map f xs] but more efficient. *)
val rev_map : ('a -> 'b) -> 'a list -> 'b list

(** [rev_mapi f xs] is [rev @@ mapi f xs] but more efficient. *)
val rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** [rev_map_e] is a Result-aware variant of {!rev_map}. *)
val rev_map_e :
  ('a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

(** [rev_map_s] is an Lwt-aware variant of {!rev_map}. *)
val rev_map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [rev_map_es] is an Lwt-Result-aware variant of {!rev_map}. *)
val rev_map_es :
  ('a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

(** [rev_map_p] is a variant of {!rev_map_s} where the promises are evaluated
    concurrently. *)
val rev_map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [rev_mapi_e] is a Result-aware variant of {!rev_mapi}. *)
val rev_mapi_e :
  (int -> 'a -> ('b, 'trace) result) -> 'a list -> ('b list, 'trace) result

(** [rev_mapi_s] is an Lwt-aware variant of {!rev_mapi}. *)
val rev_mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [rev_mapi_es] is an Lwt-Result-aware variant of {!rev_mapi}. *)
val rev_mapi_es :
  (int -> 'a -> ('b, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

(** [rev_mapi_p] is a variant of {!rev_mapi_s} where the promises are
    evaluated concurrently. *)
val rev_mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t

(** [rev_filter_map f xs] is [rev @@ filter_map f xs] but more efficient. *)
val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list

(** [rev_filter_map_e] is a Result-aware variant of {!rev_filter_map}. *)
val rev_filter_map_e :
  ('a -> ('b option, 'trace) result) -> 'a list -> ('b list, 'trace) result

(** [filter_map_e] is a Result-aware variant of {!filter_map}. *)
val filter_map_e :
  ('a -> ('b option, 'trace) result) -> 'a list -> ('b list, 'trace) result

(** [rev_filter_map_s] is an Lwt-aware variant of {!rev_filter_map}. *)
val rev_filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

(** [filter_map f xs] is [filter_some @@ map f xs] but more efficient. *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(** [filter_map_s] is an Lwt-aware variant of {!filter_map}. *)
val filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

(** [rev_filter_map_es] is an Lwt-Result-aware variant of {!rev_filter_map}. *)
val rev_filter_map_es :
  ('a -> ('b option, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

(** [filter_map_es] is an Lwt-Result-aware variant of {!filter_map}. *)
val filter_map_es :
  ('a -> ('b option, 'trace) result Lwt.t) ->
  'a list ->
  ('b list, 'trace) result Lwt.t

(** [filter_map_p] is a variant of {!filter_map_s} where the promises are evaluated concurrently. *)
val filter_map_p : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t

(** [concat_map f xs] is [concat (map f xs)] but more efficient. *)
val concat_map : ('a -> 'b list) -> 'a list -> 'b list

(** [concat_map_s] is an Lwt-aware variant of {!concat_map}. *)
val concat_map_s : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t

(** [concat_map_e] is a Result-aware variant of {!concat_map}. *)
val concat_map_e :
  ('a -> ('b list, 'error) result) -> 'a list -> ('b list, 'error) result

(** [concat_map_es] is an Lwt-Result-aware variant of {!concat_map}. *)
val concat_map_es :
  ('a -> ('b list, 'error) result Lwt.t) ->
  'a list ->
  ('b list, 'error) result Lwt.t

(** [concat_map_p] is a variant of {!concat_map_s} where the promises are evaluated concurrently. *)
val concat_map_p : ('a -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** [fold_left_e] is a Result-aware variant of {!fold_left}. *)
val fold_left_e :
  ('a -> 'b -> ('a, 'trace) result) -> 'a -> 'b list -> ('a, 'trace) result

(** [fold_left_s] is an Lwt-aware variant of {!fold_left}. *)
val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t

(** [fold_left_es] is an Lwt-Result-aware variant of {!fold_left}. *)
val fold_left_es :
  ('a -> 'b -> ('a, 'trace) result Lwt.t) ->
  'a ->
  'b list ->
  ('a, 'trace) result Lwt.t

(** [fold_left_map f a xs] is a combination of [fold_left] and [map] that maps
    over all elements of [xs] and threads an accumulator with initial value [a]
    through calls to [f]. *)
val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

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

(** {3 Double-traversal variants}

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

(** {3 Scanning variants} *)

val for_all : ('a -> bool) -> 'a list -> bool

val for_all_e :
  ('a -> (bool, 'trace) result) -> 'a list -> (bool, 'trace) result

val for_all_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val for_all_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  (bool, 'trace) result Lwt.t

val for_all_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val exists : ('a -> bool) -> 'a list -> bool

val exists_e :
  ('a -> (bool, 'trace) result) -> 'a list -> (bool, 'trace) result

val exists_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

val exists_es :
  ('a -> (bool, 'trace) result Lwt.t) ->
  'a list ->
  (bool, 'trace) result Lwt.t

val exists_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t

(** {3 Double-scanning variants}

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

(** [combine_with_leftovers ll lr] is a tuple [(combined, leftover)]
    where [combined] is [combine_drop ll lr]
      and [leftover] is either [Either.Left lsuffix] or [Either.Right rsuffix]
      depending on which of [ll] or [lr] is longer. [leftover] is [None] if the
      two lists have the same length. *)
val combine_with_leftovers :
    'a list -> 'b list -> ('a * 'b) list * ('a list, 'b list) Either.t option

(** {3 Product} *)

(** [product xs ys] is the cartesian product of [xs] and [ys].

    In other words [product xs ys] is a list containing all the pairs [(x, y)]
    where [x] is an element of [xs] and [y] is an element of [ys].

    The order of the elements in the returned list is unspecified. *)
val product : 'a list -> 'b list -> ('a * 'b) list

  (** {3 Comparison and equality}

      The comparison and equality functions are those of the OCaml [Stdlib]. *)

val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

val compare_lengths : 'a list -> 'b list -> int

val compare_length_with : 'a list -> int -> int

val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

  (** {3 Sorting}

      The sorting functions are those of the OCaml [Stdlib]. *)

val sort : ('a -> 'a -> int) -> 'a list -> 'a list

val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list

val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list

val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list

  (** {3 Conversion}

      The conversion functions are those of the OCaml [Stdlib]. *)

val to_seq : 'a list -> 'a Seq.t

val of_seq : 'a Seq.t -> 'a list

val init_ep :
  when_negative_length:'error ->
  int ->
  (int -> ('a, 'error Error_monad.trace) result Lwt.t) ->
  ('a list, 'error Error_monad.trace) result Lwt.t

val filter_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('a list, 'error Error_monad.trace) result Lwt.t

val partition_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('a list * 'a list, 'error Error_monad.trace) result Lwt.t

val partition_map_ep :
  ('a -> (('b, 'c) Either.t, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list * 'c list, 'error Error_monad.trace) result Lwt.t

val iter_ep :
  ('a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (unit, 'error Error_monad.trace) result Lwt.t

val iteri_ep :
  (int -> 'a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (unit, 'error Error_monad.trace) result Lwt.t

val map_ep :
  ('a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val mapi_ep :
  (int -> 'a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val rev_map_ep :
  ('a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val rev_mapi_ep :
  (int -> 'a -> ('b, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val filter_map_ep :
  ('a -> ('b option, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error Error_monad.trace) result Lwt.t

val concat_map_ep :
  ('a -> ('b list, 'error trace) result Lwt.t) ->
  'a list ->
  ('b list, 'error trace) result Lwt.t

val for_all_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (bool, 'error Error_monad.trace) result Lwt.t

val exists_ep :
  ('a -> (bool, 'error Error_monad.trace) result Lwt.t) ->
  'a list ->
  (bool, 'error Error_monad.trace) result Lwt.t
