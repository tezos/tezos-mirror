(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/pbt/saturation_fuzzing.exe
    Subject:      Operations in Saturation_repr
*)

open Protocol.Saturation_repr
open Lib_test.Qcheck_helpers

(** A generator that returns a [t] that cannot be [saturated] *)
let unsatured_arb = of_option_arb @@ QCheck.map of_int_opt QCheck.int

(** The general generator for [t]: generates both unsaturated values
    and [saturated]. *)
let t_arb : may_saturate t QCheck.arbitrary =
  QCheck.frequency [(1, QCheck.always saturated); (4, unsatured_arb)]

(* Test.
 * Tests that [add] commutes.
 *)
let test_add_commutes =
  QCheck.Test.make
    ~name:"t1 + t2 = t2 + t1"
    (QCheck.pair t_arb t_arb)
    (fun (t1, t2) ->
      let t1_plus_t2 = add t1 t2 in
      let t2_plus_t1 = add t2 t1 in
      qcheck_eq ~pp t1_plus_t2 t2_plus_t1)

(* Test.
 * Tests that [mul] commutes.
 *)
let test_mul_commutes =
  QCheck.Test.make
    ~name:"t1 * t2 = t2 * t1"
    (QCheck.pair t_arb t_arb)
    (fun (t1, t2) ->
      let t1_times_t2 = mul t1 t2 in
      let t2_times_t1 = mul t2 t1 in
      qcheck_eq ~pp t1_times_t2 t2_times_t1)

(* Test.
 * Tests that [zero] is neutral for [add].
 *)
let test_add_zero =
  QCheck.Test.make ~name:"t + 0 = t" t_arb (fun t ->
      let t_plus_zero = add t zero in
      qcheck_eq' ~pp ~expected:t ~actual:t_plus_zero ())

(* Test.
 * Tests that t1 + t2 >= t1
 *)
let test_add_neq =
  QCheck.Test.make
    ~name:"t1 + t2 >= t1"
    (QCheck.pair t_arb t_arb)
    (fun (t1, t2) ->
      let t1_plus_t2 = add t1 t2 in
      t1_plus_t2 >= t1)

(* Test.
 * Tests that 1 is neutral for [mul].
 *)
let test_mul_one =
  let one = safe_int 1 in
  QCheck.Test.make ~name:"t * 1 = t" t_arb (fun t ->
      let t_times_one = mul t one in
      qcheck_eq' ~pp ~expected:t ~actual:t_times_one ())

(* Test.
 * Tests that [t] times [0] equals [0].
 *)
let test_mul_zero =
  QCheck.Test.make ~name:"t * 0 = 0" t_arb (fun t ->
      let t_times_zero = mul t zero in
      qcheck_eq' ~pp ~expected:zero ~actual:t_times_zero ())

(* Test.
 * Tests that [t] [sub] [zero] equals [t].
 *)
let test_sub_zero =
  QCheck.Test.make ~name:"t - 0 = t" t_arb (fun t ->
      let t_sub_zero = sub t zero in
      qcheck_eq' ~pp ~expected:t ~actual:t_sub_zero ())

(* Test.
 * Tests that [t] [sub] [t] equals [zero].
 *)
let test_sub_itself =
  QCheck.Test.make ~name:"t - t = 0" t_arb (fun t ->
      let t_sub_t = sub t t in
      qcheck_eq' ~pp ~expected:zero ~actual:t_sub_t ())

(* Test.
 * Tests that t1 - t2 <= t1
 *)
let test_sub_neq =
  QCheck.Test.make
    ~name:"t1 - t2 <= t1"
    (QCheck.pair t_arb t_arb)
    (fun (t1, t2) ->
      let t1_minus_t2 = sub t1 t2 in
      t1_minus_t2 <= t1)

(* Test.
 * Tests that (t1 + t2) - t2 <= t1
 *)
let test_add_sub =
  QCheck.Test.make
    ~name:"(t1 + t2) - t2 <= t1"
    (QCheck.pair t_arb t_arb)
    (fun (t1, t2) ->
      let lhs = sub (add t1 t2) t2 in
      lhs <= t1)

(* Test.
 * Tests that (t1 - t2) + t2 >= t1
 *)
let test_sub_add =
  QCheck.Test.make
    ~name:"(t1 - t2) + t2 >= t1"
    (QCheck.pair t_arb t_arb)
    (fun (t1, t2) ->
      let lhs = add (sub t1 t2) t2 in
      lhs >= t1)

(* Test.
 * Tests that [saturated] >= t
 *)
let test_leq_saturated =
  QCheck.Test.make ~name:"t <= saturated" t_arb (fun t -> saturated >= t)

(* Test.
 * Tests that [zero] <= t
 *)
let test_geq_zero = QCheck.Test.make ~name:"t >= 0" t_arb (fun t -> zero <= t)

let tests_add = [test_add_commutes; test_add_zero; test_add_neq]

let tests_mul = [test_mul_commutes; test_mul_one; test_mul_zero]

let tests_sub = [test_sub_zero; test_sub_itself; test_sub_neq]

let tests_add_sub = [test_add_sub; test_sub_add]

let tests_boundaries = [test_leq_saturated; test_geq_zero]

let () =
  Alcotest.run
    "Saturation"
    [
      ("add", qcheck_wrap tests_add);
      ("mul", qcheck_wrap tests_mul);
      ("sub", qcheck_wrap tests_sub);
      ("add and sub", qcheck_wrap tests_add_sub);
      ("<= and >=", qcheck_wrap tests_boundaries);
    ]
