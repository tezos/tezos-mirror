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

(* This module is deprecated, use [Qcheck2_helpers_no_alcotest] and
   [Qcheck_tezt]. *)

(** Wrap QCheck tests into Alcotest. *)
val qcheck_wrap :
  ?verbose:bool ->
  ?long:bool ->
  ?rand:Random.State.t ->
  QCheck2.Test.t list ->
  unit Alcotest.test_case list

(** Wrap QCheck tests into Alcotest_lwt. *)
val qcheck_wrap_lwt :
  ?verbose:bool ->
  ?long:bool ->
  ?rand:Random.State.t ->
  QCheck2.Test.t list ->
  (string * [`Quick | `Slow] * (unit -> unit Lwt.t)) list

(** [qcheck_make_result ?print ?pp_error ?count ?check ~name ~gen f]
    is a wrapper around {!QCheck2.Test.make} where [f] returns a
    result type. If [check] is not provided and if the result of [f] is
    an error, {!Qcheck2.Test.fail_reportf} is called and the error is
    shown if [pp_error] is provided. *)
val qcheck_make_result :
  ?count:int ->
  ?print:'a QCheck2.Print.t ->
  ?pp_error:(Format.formatter -> 'b -> unit) ->
  ?check:((bool, 'b) result -> bool) ->
  name:string ->
  gen:'a QCheck2.Gen.t ->
  ('a -> (bool, 'b) result) ->
  QCheck2.Test.t

(** [qcheck_make_lwt ?print ?count ~extract ~name ~gen f] is a wrapper
    around {!QCheck2.Test.make} where [f] returns a [bool Lwt.t].
    [extract] needs to be provided to extract the [bool] from [Lwt], e.g.
    [Lwt_main.run]. *)
val qcheck_make_lwt :
  ?count:int ->
  ?print:'a QCheck2.Print.t ->
  extract:(bool Lwt.t -> bool) ->
  name:string ->
  gen:'a QCheck2.Gen.t ->
  ('a -> bool Lwt.t) ->
  QCheck2.Test.t

(** [qcheck_make_result_lwt ?print ?count ~extract ~name ~gen f] is
    the combination of [qcheck_make_result] and [qcheck_make_lwt]. *)
val qcheck_make_result_lwt :
  ?count:int ->
  ?print:'a QCheck2.Print.t ->
  ?pp_error:(Format.formatter -> 'b -> unit) ->
  ?check:((bool, 'b) result -> bool) ->
  extract:((bool, 'b) result Lwt.t -> (bool, 'b) result) ->
  name:string ->
  gen:'a QCheck2.Gen.t ->
  ('a -> (bool, 'b) result Lwt.t) ->
  QCheck2.Test.t

(** [qcheck_eq_tests ~eq ~gen ~eq_name] returns
    three tests of [eq]: reflexivity, symmetry, and transitivity.

    [eq_name] should be the name of the function defining [eq].
    For example, given an equality function defined as [let mytype_eq = ...],
    call [qcheck_eq_tests mytype_eq gen "mytype_eq"]. [eq_name] is
    only used for logging. *)
val qcheck_eq_tests :
  eq:('a -> 'a -> bool) ->
  gen:'a QCheck2.Gen.t ->
  eq_name:string ->
  QCheck2.Test.t list

(** [qcheck_eq pp cmp eq a b] evaluates whether [a] and [b] are equal, and if they
    are not, raises a failure and prints an error message.
    Equality is evaluated as follows:
    - use a provided [eq]
    - if no [eq] is provided, use a provided [cmp]
    - if neither [eq] nor [cmp] is provided, use {!Stdlib.compare}

    If [pp] is provided, use this to print [x] and [y] if they are not equal.

    If [__LOC__] is provided, print it at the beginning of the error message
    when applicable. *)
val qcheck_eq :
  ?pp:(Format.formatter -> 'a -> unit) ->
  ?cmp:('a -> 'a -> int) ->
  ?eq:('a -> 'a -> bool) ->
  ?__LOC__:string ->
  'a ->
  'a ->
  bool

(** Similar to {!qcheck_eq} but tests that two values are {e not} equal. *)
val qcheck_neq :
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

(** [qcheck_cond pp cond a] evaluate [cond a], if this condition is false,
    raises a failure and prints an error message.

    If [pp] is provided, use this to print [a] if [cond a] is false. *)
val qcheck_cond :
  ?pp:(Format.formatter -> 'a -> unit) ->
  cond:('a -> bool) ->
  'a ->
  unit ->
  bool

(** [int64_range_gen a b] generates an [int64] between [a] inclusive
    and [b] inclusive.

    Poorman's implementation until
    https://github.com/c-cube/qcheck/issues/105 is done. *)
val int64_range_gen : int64 -> int64 -> int64 QCheck2.Gen.t

(** [int32_range_gen a b] generates an [int32] between [a] inclusive
    and [b] inclusive.

    Poorman's implementation until
    https://github.com/c-cube/qcheck/issues/105 is done. *)
val int32_range_gen : int32 -> int32 -> int32 QCheck2.Gen.t

(** [int64_strictly_positive_gen x] generates an [int64] between [1] inclusive
    and [x] inclusive.

    This will fail if [x] is not strictly positive. *)
val int64_strictly_positive_gen : int64 -> int64 QCheck2.Gen.t

(** [int_strictly_positive_gen x] generates an [int] between [1] inclusive
    and [x] inclusive.

    This will fail if [x] is not strictly positive. *)
val int_strictly_positive_gen : int -> int QCheck2.Gen.t

(** [uint16] is a generator of unsigned int16 values *)
val uint16 : int QCheck2.Gen.t

(** [int16] is a generator of signed int16 values *)
val int16 : int QCheck2.Gen.t

(** [uint8] is a generator of unsigned int8 values *)
val uint8 : int QCheck2.Gen.t

(** [int8] is a generator of signed int8 values  *)
val int8 : int QCheck2.Gen.t

(** [string_fixed n] is a generator of strings of length [n]. *)
val string_fixed : int -> string QCheck2.Gen.t

(** [bytes_gen] is a [QCheck2.Gen.t] for [bytes]. *)
val bytes_gen : bytes QCheck2.Gen.t

(** [small_bytes_gen] is a [QCheck2.Gen.t] for [bytes] of small size. *)
val small_bytes_gen : bytes QCheck2.Gen.t

(** [bytes_fixed_gen n] is a [QCheck2.Gen.t] for [bytes] of length [n]. *)
val bytes_fixed_gen : int -> bytes QCheck2.Gen.t

(** [endpoint_gen] is a [QCheck2.Gen.t] for endpoints (such as
    [octez-client]'s [--endpoint] flag). It returns URLs of the form:
    [(http|https)://(string\.)+(:port)?]. It is by no means the most
    general [Uri.t] generator. Generalize it if needed. *)
val endpoint_gen : Uri.t QCheck2.Gen.t

(** A generator that returns a sublist of the given list. Lists returned
    by this generator are not in the same order as the given list
    (they are shuffled). This generator can return a list equal to the input list
    (this generator does not guarantee to return strict sublists of the input list). *)
val sublist : 'a list -> 'a list QCheck2.Gen.t

(** A generator that returns lists whose elements are from the given list,
    preserving the order. For example, given the input list [0, 1, 2],
    this generator can produce [], [0], [0, 2], [1, 2], [1], etc. *)
val holey : 'a list -> 'a list QCheck2.Gen.t

(** [of_option_gen gen] converts a generator [gen] of optional values into a
    generator of values by rerunning the generator if the generated value
    was a [None] until a [Some] is generated.

    Be careful: if [None] is always returned, this hangs forever! *)
val of_option_gen : 'a option QCheck2.Gen.t -> 'a QCheck2.Gen.t

(** Map-related generators. *)
module MakeMapGen (Map : sig
  type 'a t

  type key

  val of_seq : (key * 'a) Seq.t -> 'a t
end) : sig
  (** [gen_of_size size_gen key_gen val_gen] is a generator of Map where the keys
      are generated with [key_gen] and the values with [val_gen].
      The number of entries in the map is decided by [size_gen]. *)
  val gen_of_size :
    int QCheck2.Gen.t ->
    Map.key QCheck2.Gen.t ->
    'v QCheck2.Gen.t ->
    'v Map.t QCheck2.Gen.t

  (** [gen key_gen gen_gen] is a generator of Map where the keys
      are generated with [key_gen] and the values with [val_gen]. *)
  val gen : Map.key QCheck2.Gen.t -> 'v QCheck2.Gen.t -> 'v Map.t QCheck2.Gen.t
end

(** Test the roundtripness of an encoding both in JSON and binary formats. *)
val test_roundtrip :
  count:int ->
  title:string ->
  gen:'a QCheck2.Gen.t ->
  eq:('a -> 'a -> bool) ->
  'a Data_encoding.t ->
  QCheck2.Test.t

(** Test the roundtripness of two encodings in binary formats. *)
val test_roundtrip_through_binary :
  count:int ->
  title:string ->
  gen:'a QCheck2.Gen.t ->
  eq:('a -> 'a -> bool) ->
  'a Data_encoding.t ->
  'a Data_encoding.t ->
  QCheck2.Test.t
