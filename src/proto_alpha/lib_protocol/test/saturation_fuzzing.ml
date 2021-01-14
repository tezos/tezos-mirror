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
    Invocation:   dune build @src/proto_alpha/lib_protocol/runtest_saturation_fuzzing
    Subject:      Operations in Saturation_repr
*)

open Protocol.Saturation_repr

let gen_unsaturated =
  let open Crowbar in
  map [int] safe_int

let gen_t =
  let open Crowbar in
  choose
    [ const saturated;
      gen_unsaturated;
      gen_unsaturated;
      gen_unsaturated;
      gen_unsaturated ]

(* Test.
 * Tests that [f] commutes.
 *)
let test_commutes f t1 t2 =
  let fapp = f t1 t2 in
  let fapp' = f t2 t1 in
  Crowbar.check_eq ~pp fapp fapp'

(* Test.
 * Tests that [e] is neutral for [f].
 *)
let test_neutral f e t =
  let fapp = f e t in
  let fapp' = f t e in
  Crowbar.check_eq ~pp fapp fapp'

(* Test.
 * Tests that [t] times [1] equals [t].
 *)
let test_mul_one t = Crowbar.check_eq ~pp (mul t @@ safe_int 1) t

(* Test.
 * Tests that [t] times [0] equals [0].
 *)
let test_mul_zero t = Crowbar.check_eq ~pp (mul t zero) (zero |> may_saturate)

(* Test.
 * Tests that [t] minus [zero] equals [t].
 *)
let test_sub_zero t = Crowbar.check_eq ~pp (sub t zero) t

let tests =
  Crowbar.add_test ~name:"add commutes" [gen_t; gen_t] (test_commutes add) ;
  Crowbar.add_test ~name:"mul commutes" [gen_t; gen_t] (test_commutes mul) ;
  Crowbar.add_test
    ~name:"0 is neutral for add"
    [gen_t]
    (test_neutral add (zero |> may_saturate)) ;
  Crowbar.add_test
    ~name:"1 is neutral for mul"
    [gen_t]
    (test_neutral mul (safe_int 1)) ;
  Crowbar.add_test ~name:"t * 0 = 0" [gen_t] test_mul_zero ;
  Crowbar.add_test ~name:"t * 1 = t" [gen_t] test_mul_one ;
  Crowbar.add_test ~name:"t - 0 = t" [gen_t] test_sub_zero
