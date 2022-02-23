(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Wrap QCheck tests into Alcotests. *)
val qcheck_wrap :
  ?verbose:bool ->
  ?long:bool ->
  ?rand:Random.State.t ->
  QCheck.Test.t list ->
  unit Alcotest.test_case list

(** [qcheck_eq_tests ~eq ~arb ~eq_name] returns
 *  three tests of [eq]: reflexivity, symmetry, and transitivity.
 *
 *  [eq_name] should be the name of the function defining [eq].
 *  For example, given an equality function defined as [let mytype_eq = ...],
 *  call [qcheck_eq_tests mytype_eq arb "mytype_eq"]. [eq_name] is
 *  only used for logging. *)
val qcheck_eq_tests :
  eq:('a -> 'a -> bool) ->
  arb:'a QCheck.arbitrary ->
  eq_name:string ->
  QCheck.Test.t list

(** [qcheck_eq pp cmp eq a b] evaluates whether [a] and [b] are equal, and if they
    are not, raises a failure and prints an error message.
    Equality is evaluated as follows:
    - use a provided [eq]
    - if no [eq] is provided, use a provided [cmp]
    - if neither [eq] nor [cmp] is provided, use {!Stdlib.compare}

    If [pp] is provided, use this to print [x] and [y] if they are not equal. *)
val qcheck_eq :
  ?pp:(Format.formatter -> 'a -> unit) ->
  ?cmp:('a -> 'a -> int) ->
  ?eq:('a -> 'a -> bool) ->
  'a ->
  'a ->
  bool

(** Labeled variant of {!qcheck_eq}. The [unit] argument is necessary as OCaml
    requires at least one positional (non-labeled) argument in case of optional
    arguments. *)
val qcheck_eq' :
  ?pp:(Format.formatter -> 'a -> unit) ->
  ?cmp:('a -> 'a -> int) ->
  ?eq:('a -> 'a -> bool) ->
  expected:'a ->
  actual:'a ->
  unit ->
  bool

(** [int64_range_gen a b] generates an [int64] between [a] inclusive
    and [b] inclusive.

    Poorman's implementation until
    https://github.com/c-cube/qcheck/issues/105 is done.

    This probably spectacularly crashes if [(b - a) > Int64.max_int]. *)
val int64_range_gen : int64 -> int64 -> int64 QCheck.Gen.t

val int64_range : int64 -> int64 -> int64 QCheck.arbitrary

(** [int_strictly_positive_gen x] generates an [int] between [1] inclusive
    and [x] inclusive.

    This will fail if [x] is not strictly positive. *)
val int_strictly_positive_gen : int -> int QCheck.Gen.t

(** [int64_strictly_positive_gen x] generates an [int64] between [1] inclusive
    and [x] inclusive.

    This will fail if [x] is not strictly positive. *)
val int64_strictly_positive_gen : int64 -> int64 QCheck.Gen.t

(** [of_option_gen gen] converts a generator [gen] of optional values into a
    generator of values by rerunning the generator if the generated value
    was a [None] until a [Some] is generated.

    Be careful: if [None] is always returned, this hangs forever! *)
val of_option_gen : 'a option QCheck.Gen.t -> 'a QCheck.Gen.t

(** [of_option_arb arb] converts an arbitrary [arb] of optional values into
    an arbitrary of values.

    - Generation of values is delegated to {!of_option_gen} (retries on
      [None] values until a [Some] is generated).
    - Shrinking uses the input shrinker but ignores [None] values.

    Be careful: if [None] is always returned, this hangs forever!
*)
val of_option_arb : 'a option QCheck.arbitrary -> 'a QCheck.arbitrary

(** [uint16] is an arbitrary of unsigned [int16] arbitrary *)
val uint16 : int QCheck.arbitrary

(** [int16] is an arbitrary of signed int16 arbitrary *)
val int16 : int QCheck.arbitrary

(** [uint8] is an arbitrary of unsigned [int8] values *)
val uint8 : int QCheck.arbitrary

(** [int8] is an arbitrary of signed int8 values  *)
val int8 : int QCheck.arbitrary

(** [string_fixed n] is an arbitrary of string of length [n]. *)
val string_fixed : int -> string QCheck.arbitrary

(** [of_option_shrink shrink_opt] returns a shrinker from an optional one.
    This is typically useful when extracting a shrinker from an [arbitrary]
    to compose a bigger shrinker.

    If [shrink_opt] is [None] then no shrinking happens. *)
val of_option_shrink : 'a QCheck.Shrink.t option -> 'a QCheck.Shrink.t

(** [bytes_arb] is a [QCheck.arbitrary] for [bytes]. *)
val bytes_arb : bytes QCheck.arbitrary

(** [endpoint_arb] is a [QCheck.arbitrary] for endpoints (such as
    [tezos-client]'s [--endpoint] flag). It returns URLs of the form:
    [(http|https)://(string\.)+(:port)?]. It is by no means the most
    general [Uri.t] generator. Generalize it if needed. *)
val endpoint_arb : Uri.t QCheck.arbitrary

(** A generator that returns a sublist of the given list. Lists returned
    by this generator are not in the same order as the given list
    (they are shuffled). This generator can return a list equal to the input list
    (this generator does not guarantee to return strict sublists of the input list). *)
val sublist : 'a list -> 'a list QCheck.Gen.t

(** A generator that returns lists whose elements are from the given list,
    preserving the order. For example, given the input list [0, 1, 2],
    this generator can produce [], [0], [0, 2], [1, 2], [1], etc. *)
val holey : 'a list -> 'a list QCheck.Gen.t

(** Map-related arbitraries/generators. *)
module MakeMapArb (Map : Stdlib.Map.S) : sig
  (** [arb_of_size size_gen key_arb val_arb] is an arbitrary of Map
      where the keys are generated with [key_arb] and the values with [val_arb].

      The number of entries in the map is decided by [size_gen].

      The arbitrary shrinks on the number of entries as well as on entries
      if either the key or value arbitrary has a shrinker. *)
  val arb_of_size :
    int QCheck.Gen.t ->
    Map.key QCheck.arbitrary ->
    'v QCheck.arbitrary ->
    'v Map.t QCheck.arbitrary

  (** [arb key_arb val_arb] is an arbitrary of Map where the keys are
      generated with [key_arb] and the values with [val_arb].

      The arbitrary shrinks on the number of entries as well as on entries
      if either the key or value arbitrary has a shrinker. *)
  val arb :
    Map.key QCheck.arbitrary -> 'v QCheck.arbitrary -> 'v Map.t QCheck.arbitrary

  (** [gen_of_size size_gen key_gen val_gen] is a generator of Map where the keys
      are generated with [key_gen] and the values with [val_gen].
      The number of entries in the map is decided by [size_gen]. *)
  val gen_of_size :
    int QCheck.Gen.t ->
    Map.key QCheck.Gen.t ->
    'v QCheck.Gen.t ->
    'v Map.t QCheck.Gen.t

  (** [gen key_gen arb_gen] is a generator of Map where the keys
      are generated with [key_arb] and the values with [val_arb]. *)
  val gen : Map.key QCheck.Gen.t -> 'v QCheck.Gen.t -> 'v Map.t QCheck.Gen.t

  (** [shrink ?key ?value] shrinks a Map using the optional [key] and [value]
      shrinkers. Shrinking first attempts smaller Maps (less entries), then
      shrinks keys and values. *)
  val shrink :
    ?key:Map.key QCheck.Shrink.t ->
    ?value:'v QCheck.Shrink.t ->
    'v Map.t QCheck.Shrink.t
end
