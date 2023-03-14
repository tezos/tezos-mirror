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

module Fr = Bls12_381.Fr
module SMap = Plonk.SMap
module Domain = Bls12_381_polynomial.Domain
module P = Bls12_381_polynomial.Polynomial
module Eval = Plonk.Evaluations_map.Make (Bls12_381_polynomial.Evaluations)

let powers = Octez_bls12_381_polynomial.Fr_carray.powers

open Eval

let test_linear () =
  let p_1 = P.generate_biased_random_polynomial 10 in
  let p_2 = P.generate_biased_random_polynomial 10 in
  let domain = Domain.build_power_of_two 6 in
  let eval1 = evaluation_fft domain p_1 in
  let eval2 = evaluation_fft domain p_2 in
  let cst, linear_coeff_1, linear_coeff_2 =
    (Fr.random (), Fr.random (), Fr.random ())
  in
  let p_1_comp =
    let caml = P.to_dense_coefficients p_1 in
    let pow_g = powers (Array.length caml) (Domain.get domain 1) in
    Array.map2 Fr.mul caml pow_g |> P.of_dense
  in
  let expected_res =
    P.(
      mul_by_scalar linear_coeff_1 p_1_comp
      + mul_by_scalar linear_coeff_2 p_2
      + mul_by_scalar cst P.one)
  in
  let evaluations = SMap.of_list [("1", eval1); ("2", eval2)] in
  let res_eval =
    linear
      ~evaluations
      ~poly_names:["1"; "2"]
      ~add_constant:cst
      ~linear_coeffs:[linear_coeff_1; linear_coeff_2]
      ~composition_gx:([1; 0], 64)
      ()
  in
  let res = interpolation_fft domain res_eval in
  assert (P.equal res expected_res)

let test_mul () =
  let p_1 = P.generate_biased_random_polynomial 10 in
  let p_2 = P.generate_biased_random_polynomial 10 in
  let domain = Domain.build_power_of_two 6 in
  let eval1 = evaluation_fft domain p_1 in
  let eval2 = evaluation_fft domain p_2 in
  let p_2_comp =
    let caml = P.to_dense_coefficients p_2 in
    let pow_g = powers (Array.length caml) (Domain.get domain 1) in
    Array.map2 Fr.mul caml pow_g |> P.of_dense
  in
  let evaluations = SMap.of_list [("1", eval1); ("2", eval2)] in
  let res_eval =
    mul ~evaluations ~poly_names:["1"; "2"] ~composition_gx:([0; 1], 64) ()
  in
  let expected_res = P.mul p_1 p_2_comp in
  let res = interpolation_fft domain res_eval in
  assert (P.equal res expected_res)

let tests =
  [
    Alcotest.test_case "test_linear" `Quick test_linear;
    Alcotest.test_case "test_mul" `Quick test_mul;
  ]
