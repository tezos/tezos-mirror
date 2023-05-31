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

module JubjubWeierstrass (L : LIB) = struct
  module MecJubJub = Mec.Curve.Jubjub.AffineWeierstrass
  open L

  open Helpers.Utils (L)

  open Plompiler.JubjubWeierstrass (L)

  let point_of_mec p =
    let x = MecJubJub.get_x_coordinate p in
    let y = MecJubJub.get_y_coordinate p in
    ( S.of_bytes_exn (MecJubJub.Base.to_bytes x),
      S.of_bytes_exn (MecJubJub.Base.to_bytes y) )

  let add_circuit p q expected () =
    let* p = input_point ~kind:`Public p in
    let* q = input_point ~kind:`Public q in
    let* r = input_point expected in
    let* o = add p q in
    assert_equal o r

  let test_vectors =
    List.init 10 (fun _ -> (MecJubJub.random (), MecJubJub.random ()))

  let inputs =
    (* p + q = r *)
    List.map (fun (p1, p2) -> (point_of_mec p1, point_of_mec p2)) test_vectors

  let expected =
    (* p + q = r *)
    List.map
      (fun (input1, input2) ->
        let res = MecJubJub.add input1 input2 in
        point_of_mec res)
      test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_add =
    List.map2
      (fun (p1, p2) e ->
        test ~valid:true ~name:"Weierstrass.test_add" @@ add_circuit p1 p2 e)
      inputs
      expected
    @ List.map2
        (fun i w -> test ~valid:false @@ add_circuit (fst i) (snd i) w)
        inputs
        wrong

  let double_circuit p expected () =
    let* p = input_point ~kind:`Public p in
    let* r = input_point expected in
    let* o = double p in
    assert_equal o r

  let test_vectors = List.init 10 (fun _ -> MecJubJub.random ())

  let inputs =
    (* p + p = r *)
    List.map point_of_mec test_vectors

  let expected =
    (* p + p = r *)
    List.map
      (fun input ->
        let res = MecJubJub.double input in
        point_of_mec res)
      test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_double =
    List.map2
      (fun i e ->
        test ~valid:true ~name:"Weierstrass.test_double" @@ double_circuit i e)
      inputs
      expected
    @ List.map2
        (fun i w -> test ~valid:false @@ double_circuit i w)
        inputs
        wrong

  let input_bytes bytes = input Input.(list (List.map (fun b -> bool b) bytes))

  let scalar_mul_circuit p input_b expected () =
    let* p = input_point p in
    let* b = input_bytes input_b in
    let* r = input_point expected in
    let* o = scalar_mul b p in
    assert_equal r o

  let test_vectors =
    let size = Z.log2 S.order in
    List.init 2 (fun _ ->
        let bool_list = List.init size (fun _ -> Stdlib.Random.bool ()) in
        (MecJubJub.random (), bool_list))

  let inputs =
    (* p^k = r *)
    List.map
      (fun (input_p, input_b) -> (point_of_mec input_p, input_b))
      test_vectors

  let expected =
    (* p^k = r *)
    List.map
      (fun (input_p, input_b) ->
        let exponent = Plompiler.Utils.bool_list_to_scalar input_b in
        let exponent = MecJubJub.Scalar.of_bytes_exn (S.to_bytes exponent) in
        let res = MecJubJub.mul input_p exponent in
        point_of_mec res)
      test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_scalar_mul =
    List.map2
      (fun (p, b) e ->
        test ~valid:true ~name:"Weierstrass.test_scalar_mul"
        @@ scalar_mul_circuit p b e)
      inputs
      expected
    @ List.map2
        (fun (p, b) w -> test ~valid:false @@ scalar_mul_circuit p b w)
        inputs
        wrong

  let is_on_curve_circuit p () =
    let* p = input_point ~kind:`Public p in
    assert_is_on_curve p

  let test_vectors = List.init 10 (fun _ -> MecJubJub.random ())

  let inputs = List.map point_of_mec test_vectors

  let wrong = List.map (fun _ -> (S.random (), S.random ())) expected

  let test_is_on_curve =
    List.map
      (fun i ->
        test ~valid:true ~name:"Weierstrass.is_on_curve_circuit"
        @@ is_on_curve_circuit i)
      inputs
    @ List.map (fun w -> test ~valid:false @@ is_on_curve_circuit w) wrong

  let tests = test_add @ test_double @ test_scalar_mul @ test_is_on_curve
end

let tests =
  let open Helpers in
  let both = [("JubJubWeierstrass", (module JubjubWeierstrass : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
