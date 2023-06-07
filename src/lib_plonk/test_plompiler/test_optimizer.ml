(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Plompiler
open Plonk_test
module CS = Plonk.Circuit
open Helpers

module Optimizer : Test =
functor
  (L : LIB)
  ->
  struct
    open L

    open Utils (L)

    let test_long_sum inputs expected () =
      let* inputs = input ~kind:`Public inputs in
      let* expected = input expected in
      let* o =
        foldM Num.add (List.hd (of_list inputs)) (List.tl (of_list inputs))
      in
      assert_equal o expected

    let test_vectors =
      List.map (fun n -> List.init n (fun _ -> S.random ())) [100; 500]

    let inputs : scalar list Input.t list =
      List.map (fun vs -> Input.(list @@ List.map scalar vs)) test_vectors

    let expected =
      List.map
        (fun vs -> Input.scalar @@ List.fold_left S.add S.zero vs)
        test_vectors

    let tests_long_sum =
      List.map2
        (fun i e ->
          test ~valid:true ~name:"Optimizer.test_long_sum" @@ test_long_sum i e)
        inputs
        expected

    let test_poseidon_rounds (a1, b1, c1) expected () =
      let s i = S.of_z (Z.of_int i) in
      let* a1 = input a1 in
      let* b1 = input b1 in
      let* c1 = input c1 in
      let abc = to_list [a1; b1; c1] in
      let* expected = input expected in
      let* a2 = Num.add_list ~qc:(s 0) ~coeffs:[s 1; s 5; s 25] abc in
      let* b2 = Num.add_list ~qc:(s 0) ~coeffs:[s 6; s 7; s 49] abc in
      let* c2 = Num.add_list ~qc:(s 0) ~coeffs:[s 9; s 9; s 9] abc in
      let a3 = a2 in
      let b3 = b2 in
      let* c3 = Num.pow5 c2 in
      let abc' = to_list [a3; b3; c3] in
      let* c4 = Num.add_list ~qc:(s 1) ~coeffs:[s 9; s 9; s 9] abc' in
      let* c5 = Num.pow5 c4 in
      assert_equal c5 expected

    let tests_poseidon_rounds =
      [
        test ~valid:true ~name:"Optimizer.test_poseidon_rounds"
        @@ test_poseidon_rounds (si 0, si 0, si 0) (si 1);
      ]

    let test_ecc_ed_cond_add p q () =
      let module ECC = JubjubEdwards (L) in
      let* zero = input ~kind:`Public (Input.bool false) in
      let* one = input ~kind:`Public (Input.bool true) in
      let* p = ECC.input_point ~kind:`Public p in
      let* q = ECC.input_point ~kind:`Public q in
      let* r = ECC.cond_add p q one in
      let* t = ECC.cond_add r p zero in
      let* z = ECC.cond_add t t one in
      let* _ = ECC.cond_add z z one in
      assert_equal r t

    let tests_ecc_ed_cond_add =
      [
        test ~valid:true ~name:"Optimizer.test_ecc_ed_cond_add"
        @@ test_ecc_ed_cond_add (S.zero, S.one) (S.zero, S.one);
      ]

    let tests = tests_long_sum @ tests_poseidon_rounds @ tests_ecc_ed_cond_add
  end

let tests =
  let both = [("Optimizer", (module Optimizer : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
