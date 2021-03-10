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
val qcheck_wrap : QCheck.Test.t list -> unit Alcotest.test_case list

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

(** [int64_range a b] generates an [int64] between [a] inclusive and [b] inclusive.

    Poorman's implementation until https://github.com/c-cube/qcheck/issues/105 is done.

    This probably spectacularly crashes if [(b - a) > Int64.max_int]. *)
val int64_range : int64 -> int64 -> int64 QCheck.arbitrary

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
