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
module Domain = Octez_bls12_381_polynomial.Domain
module Poly_c = Octez_bls12_381_polynomial.Polynomial
module Eval = Octez_bls12_381_polynomial.Evaluations

let powers = Octez_bls12_381_polynomial.Fr_carray.powers

(* computes p(g·x) *)
let make_composition_gx g p_c =
  (* p(x) = a_n·x^n + ... + a_1·x + a_0 *)
  (* p(g·x) = (a_n·g^n)·x^n + ... + (a_1·g)·x + (a_0·g^0) *)
  let p_caml = Poly_c.to_dense_coefficients p_c in
  let pow_g = powers (Array.length p_caml) g in
  Array.map2 Fr.mul p_caml pow_g |> Poly_c.of_dense

let test_of_array_zero () =
  let p_zero = Poly_c.zero in
  let domain = Domain.build_power_of_two 5 in
  let eval_expected = Eval.evaluation_fft domain p_zero in
  let eval_res = Eval.of_array (-1, Eval.to_array eval_expected) in
  assert (Eval.equal eval_res eval_expected)

let test_of_array_const () =
  let const = Fr.(add one @@ random ()) in
  let p = Poly_c.constant const in
  let domain = Domain.build_power_of_two 5 in
  let eval_expected = Eval.evaluation_fft domain p in
  let eval_res = Eval.of_array (0, Eval.to_array eval_expected) in
  assert (Eval.equal eval_res eval_expected)

let test_of_array () =
  let p = Poly_c.generate_biased_random_polynomial 30 in
  let degree_p = Poly_c.degree p in
  let domain = Domain.build_power_of_two 5 in
  let eval_expected = Eval.evaluation_fft domain p in
  let eval_res = Eval.of_array (degree_p, Eval.to_array eval_expected) in
  assert (Eval.equal eval_res eval_expected)

let test_zero () =
  let eval_zero = Eval.zero in
  let domain = Domain.build_power_of_two 5 in
  let res = Eval.interpolation_fft domain eval_zero in
  assert (Poly_c.equal res Poly_c.zero)

let test_is_zero () =
  let p_zero = Poly_c.zero in
  let domain = Domain.build_power_of_two 5 in
  let eval_zero = Eval.evaluation_fft domain p_zero in
  assert (Eval.is_zero eval_zero) ;
  assert (Eval.is_zero Eval.zero) ;
  let p_one = Poly_c.one in
  let eval_one = Eval.evaluation_fft domain p_one in
  assert (not (Eval.is_zero eval_one))

let test_copy () =
  let p = Poly_c.generate_biased_random_polynomial 30 in
  let domain = Domain.build_power_of_two 5 in
  let eval = Eval.evaluation_fft domain p in
  let res = Eval.copy eval in
  assert (Eval.equal eval res)

let test_copy_inplace () =
  let p = Poly_c.generate_biased_random_polynomial 30 in
  let domain = Domain.build_power_of_two 5 in
  let eval = Eval.evaluation_fft domain p in
  let res = Eval.create (Eval.length eval) in
  let res = Eval.copy ~res eval in
  assert (Eval.equal eval res)

let test_mul_by_scalar () =
  let p = Poly_c.generate_biased_random_polynomial 30 in
  let domain = Domain.build_power_of_two 5 in
  let eval = Eval.evaluation_fft domain p in
  let scalar = Fr.random () in
  let res_eval = Eval.mul_by_scalar scalar eval in
  let res = Eval.interpolation_fft domain res_eval in
  let res_expected = Poly_c.mul_by_scalar scalar p in
  assert (Poly_c.equal res_expected res)

let test_mul_zero () =
  (* res = p_1 * zero = zero *)
  let p_1 = Poly_c.generate_biased_random_polynomial 30 in
  let p_2 = Poly_c.zero in
  let domain = Domain.build_power_of_two 5 in
  let eval_1 = Eval.evaluation_fft domain p_1 in
  let eval_2 = Eval.evaluation_fft domain p_2 in
  let res_eval = Eval.mul_c ~evaluations:[eval_1; eval_2] () in
  let res = Eval.interpolation_fft domain res_eval in
  assert (Poly_c.equal res Poly_c.zero)

let test_mul_one () =
  (* res = p_1 * one = p_1 *)
  let p_1 = Poly_c.generate_biased_random_polynomial 30 in
  let p_2 = Poly_c.one in
  let domain = Domain.build_power_of_two 5 in
  let eval_1 = Eval.evaluation_fft domain p_1 in
  let eval_2 = Eval.evaluation_fft domain p_2 in
  let res_eval = Eval.mul_c ~evaluations:[eval_1; eval_2] () in
  let res = Eval.interpolation_fft domain res_eval in
  assert (Poly_c.equal res p_1)

let test_mul_commutativity () =
  (* p_1 * p_2 = p_2 * p_1 *)
  let p_1 = Poly_c.generate_biased_random_polynomial 30 in
  let p_2 = Poly_c.generate_biased_random_polynomial 30 in
  let domain_1 = Domain.build_power_of_two 7 in
  let domain_2 = Domain.build_power_of_two 6 in
  let eval_1 = Eval.evaluation_fft domain_1 p_1 in
  let eval_2 = Eval.evaluation_fft domain_2 p_2 in
  let res_eval = Eval.mul_c ~evaluations:[eval_1; eval_2] () in
  let res = Eval.interpolation_fft domain_2 res_eval in
  let res_eval_swap = Eval.mul_c ~evaluations:[eval_2; eval_1] () in
  let res_swap = Eval.interpolation_fft domain_2 res_eval_swap in
  let res_expected = Poly_c.mul p_1 p_2 in
  assert (Poly_c.equal res res_expected) ;
  assert (Poly_c.equal res_swap res_expected)

let test_mul_diff_size () =
  let n = 5 in
  let polys =
    List.init n (fun _i ->
        Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  (* 500 < 2^9; degree (p_1 * p_2 * ... * p_5) = sum (degree p_i) = 100 * 5 = 500 *)
  let domain_1 = Domain.build_power_of_two 9 in
  let domain_2 = Domain.build_power_of_two 10 in
  let evals =
    List.mapi
      (fun i p_i ->
        if i mod 2 = 0 then Eval.evaluation_fft domain_1 p_i
        else Eval.evaluation_fft domain_2 p_i)
      polys
  in
  let res_eval = Eval.mul_c ~evaluations:evals () in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let res_expected = List.fold_left Poly_c.mul Poly_c.one polys in
  assert (Poly_c.equal res res_expected)

let test_mul_diff_size_composition_gx () =
  let n = 5 in
  let polys =
    List.init n (fun _i ->
        Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  (* 500 < 2^9; degree (p_1 * p_2 * ... * p_5) = sum (degree p_i) = 100 * 5 = 500 *)
  let domain_1 = Domain.build_power_of_two 9 in
  let domain_2 = Domain.build_power_of_two 10 in
  let evals =
    List.mapi
      (fun i p_i ->
        if i mod 2 = 0 then Eval.evaluation_fft domain_1 p_i
        else Eval.evaluation_fft domain_2 p_i)
      polys
  in
  let res_eval =
    Eval.mul_c ~evaluations:evals ~composition_gx:([0; 0; 1; 0; 1], 512) ()
  in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let generator = Domain.get domain_1 1 in
  let polys =
    List.mapi
      (fun i p -> if i = 2 || i = 4 then make_composition_gx generator p else p)
      polys
  in
  let res_expected = List.fold_left Poly_c.mul Poly_c.one polys in
  assert (Poly_c.equal res res_expected)

let test_mul () =
  let n = 5 in
  let polys =
    List.init n (fun _ ->
        Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  let powers = List.init n (fun i -> i + 1) in
  (* 1500 < 2^11; degree (p_1 * p_2^2 * ... * p_5^5) =
     sum ((i + 1) * degree p_i) = 100 + 200 + 300 + 400 + 500 = 1500 *)
  let domain_1 = Domain.build_power_of_two 11 in
  let domain_2 = Domain.build_power_of_two 12 in
  let evals =
    List.mapi
      (fun i p_i ->
        if i mod 2 = 0 then Eval.evaluation_fft domain_1 p_i
        else Eval.evaluation_fft domain_2 p_i)
      polys
  in
  let res_eval =
    Eval.mul_c
      ~evaluations:evals
      ~composition_gx:([0; 0; 1; 0; 1], 2048)
      ~powers
      ()
  in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let generator = Domain.get domain_1 1 in
  let polys =
    List.mapi
      (fun i p -> if i = 2 || i = 4 then make_composition_gx generator p else p)
      polys
  in
  (* this is a naive implementation of exponentiation
     as we don't call it with larger n *)
  let rec power_poly p n =
    match n with
    | 0 -> Poly_c.one
    | 1 -> p
    | _ -> Poly_c.mul p (power_poly p (n - 1))
  in
  let res_expected =
    List.fold_left2
      (fun acc poly power ->
        assert (power >= 0) ;
        Poly_c.mul acc (power_poly poly power))
      Poly_c.one
      polys
      powers
  in
  assert (Poly_c.equal res res_expected)

let test_linear_zero () =
  (* p_i = zero; res = p_1 + p_2 + p_3 = zero *)
  let domain = Domain.build_power_of_two 5 in
  let res_eval =
    Eval.linear_c ~evaluations:[Eval.zero; Eval.zero; Eval.zero] ()
  in
  let res = Eval.interpolation_fft domain res_eval in
  assert (Poly_c.equal res Poly_c.zero)

let test_linear_zero_const () =
  (* p_i = zero; res = p_1 + p_2 + p_3 + const = const *)
  let domain = Domain.build_power_of_two 5 in
  let const = Fr.add Fr.one @@ Fr.random () in
  let res_eval =
    Eval.linear_c
      ~evaluations:[Eval.zero; Eval.zero; Eval.zero]
      ~add_constant:const
      ()
  in
  let res = Eval.interpolation_fft domain res_eval in
  assert (Poly_c.equal res (Poly_c.constant const))

let test_linear_zero_composition_gx () =
  (* p_i(x) = zero; res = p_1(g·x) + p_2(x) + p_3(x) = zero *)
  let domain = Domain.build_power_of_two 5 in
  let res_eval =
    Eval.linear_c
      ~evaluations:[Eval.zero; Eval.zero; Eval.zero]
      ~composition_gx:([1; 0; 0], 32)
      ()
  in
  let res = Eval.interpolation_fft domain res_eval in
  assert (Poly_c.equal res Poly_c.zero)

let test_linear_diff_size () =
  let n = 5 in
  let polys =
    List.init n (fun _i ->
        Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  (* 100 < 2^7; degree (p_1 + p_2 + ... + p_5) = max (degree p_i) = 100 *)
  let domain_1 = Domain.build_power_of_two 7 in
  let domain_2 = Domain.build_power_of_two 9 in
  let evals =
    List.mapi
      (fun i p_i ->
        if i mod 2 = 0 then Eval.evaluation_fft domain_1 p_i
        else Eval.evaluation_fft domain_2 p_i)
      polys
  in
  let linear_coeffs = List.init n (fun _i -> Fr.add Fr.one (Fr.random ())) in
  let const = Fr.add Fr.one (Fr.random ()) in
  let res_eval =
    Eval.linear_c ~evaluations:evals ~linear_coeffs ~add_constant:const ()
  in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let res_expected =
    List.fold_left2
      (fun acc poly coeff -> Poly_c.add acc (Poly_c.mul_by_scalar coeff poly))
      (Poly_c.constant const)
      polys
      linear_coeffs
  in
  assert (Poly_c.equal res res_expected)

let test_linear () =
  let n = 5 in
  let polys =
    List.init n (fun _i ->
        Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  (* 100 < 2^7; degree (p_1 + p_2 + ... + p_5) = max (degree p_i) = 100 *)
  let domain_1 = Domain.build_power_of_two 7 in
  let domain_2 = Domain.build_power_of_two 9 in
  let evals =
    List.mapi
      (fun i p_i ->
        if i mod 2 = 0 then Eval.evaluation_fft domain_1 p_i
        else Eval.evaluation_fft domain_2 p_i)
      polys
  in
  let linear_coeffs = List.init n (fun _i -> Fr.add Fr.one (Fr.random ())) in
  let const = Fr.add Fr.one (Fr.random ()) in
  let res_eval =
    Eval.linear_c
      ~evaluations:evals
      ~linear_coeffs
      ~add_constant:const
      ~composition_gx:([0; 0; 1; 0; 1], 128)
      ()
  in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let generator = Domain.get domain_1 1 in
  let polys =
    List.mapi
      (fun i p -> if i = 2 || i = 4 then make_composition_gx generator p else p)
      polys
  in
  let res_expected =
    List.fold_left2
      (fun acc poly coeff -> Poly_c.add acc (Poly_c.mul_by_scalar coeff poly))
      (Poly_c.constant const)
      polys
      linear_coeffs
  in
  assert (Poly_c.equal res res_expected)

let test_add () =
  let p_1 = Poly_c.generate_biased_random_polynomial (Random.int 40) in
  let p_2 = Poly_c.generate_biased_random_polynomial (Random.int 30) in
  let domain_1 = Domain.build_power_of_two 7 in
  let domain_2 = Domain.build_power_of_two 8 in
  let eval_1 = Eval.evaluation_fft domain_1 p_1 in
  let eval_2 = Eval.evaluation_fft domain_2 p_2 in
  let res_eval = Eval.add eval_1 eval_2 in
  let res_eval_swap = Eval.add eval_2 eval_1 in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let res_swap = Eval.interpolation_fft domain_1 res_eval_swap in
  let res_expected = Poly_c.add p_1 p_2 in
  assert (Poly_c.equal res res_expected) ;
  assert (Poly_c.equal res_swap res_expected)

let test_add_zero () =
  let p_1 = Poly_c.generate_biased_random_polynomial (Random.int 40) in
  let p_2 = Poly_c.zero in
  let domain_1 = Domain.build_power_of_two 7 in
  let domain_2 = Domain.build_power_of_two 8 in
  let eval_1 = Eval.evaluation_fft domain_1 p_1 in
  let eval_2 = Eval.evaluation_fft domain_2 p_2 in
  let res_eval = Eval.add eval_1 eval_2 in
  let res = Eval.interpolation_fft domain_1 res_eval in
  assert (Poly_c.equal res p_1)

let test_linear_with_powers_equal_length () =
  let n = 5 in
  let polys =
    List.init n (fun _ -> Poly_c.generate_biased_random_polynomial 100)
  in
  let domain = Domain.build_power_of_two 8 in
  let evals = List.map (Eval.evaluation_fft domain) polys in
  let coeff = Fr.add Fr.one (Fr.random ()) in
  let coeffs = powers n coeff |> Array.to_list in
  let res_eval = Eval.linear_with_powers evals coeff in
  let res_eval_linear =
    Eval.linear_c ~evaluations:evals ~linear_coeffs:coeffs ()
  in
  let res = Eval.interpolation_fft domain res_eval in
  let res_linear = Eval.interpolation_fft domain res_eval_linear in
  let res_expected =
    List.fold_left2
      (fun acc coeff poly -> Poly_c.add acc @@ Poly_c.mul_by_scalar coeff poly)
      Poly_c.zero
      coeffs
      polys
  in
  assert (Poly_c.equal res res_expected) ;
  assert (Poly_c.equal res_linear res_expected)

let test_linear_with_powers () =
  let n = 5 in
  let polys =
    List.init n (fun _ -> Poly_c.generate_biased_random_polynomial 100)
  in
  let domain_1 = Domain.build_power_of_two 7 in
  let domain_2 = Domain.build_power_of_two 9 in
  let evals =
    List.mapi
      (fun i p_i ->
        if i mod 2 = 0 then Eval.evaluation_fft domain_1 p_i
        else Eval.evaluation_fft domain_2 p_i)
      polys
  in
  let coeff = Fr.add Fr.one (Fr.random ()) in
  let coeffs = powers n coeff |> Array.to_list in
  let res_eval = Eval.linear_with_powers evals coeff in
  let res = Eval.interpolation_fft domain_1 res_eval in
  let res_expected =
    List.fold_left2
      (fun acc coeff poly -> Poly_c.add acc @@ Poly_c.mul_by_scalar coeff poly)
      Poly_c.zero
      coeffs
      polys
  in
  assert (Poly_c.equal res res_expected)

let test_linear_with_powers_zeros () =
  let n = 5 in
  let polys = List.init n (fun _ -> Poly_c.zero) in
  let domain = Domain.build_power_of_two 8 in
  let evals = List.map (Eval.evaluation_fft domain) polys in
  let coeff = Fr.add Fr.one (Fr.random ()) in
  let coeffs = powers n coeff |> Array.to_list in
  let res_eval = Eval.linear_with_powers evals coeff in
  let res_eval_linear =
    Eval.linear_c ~evaluations:evals ~linear_coeffs:coeffs ()
  in
  let res = Eval.interpolation_fft domain res_eval in
  let res_linear = Eval.interpolation_fft domain res_eval_linear in
  let res_expected =
    List.fold_left2
      (fun acc coeff poly -> Poly_c.add acc @@ Poly_c.mul_by_scalar coeff poly)
      Poly_c.zero
      coeffs
      polys
  in
  assert (Poly_c.equal res res_expected) ;
  assert (Poly_c.equal res_linear res_expected) ;
  assert (Poly_c.equal res Poly_c.zero)

(* TODO: test inplace operations: mul_c; linear_c; add;
   when we deal with the zero evaluation *)
let tests =
  let repetitions = 30 in
  List.map
    (fun (name, f) ->
      Alcotest.test_case name `Quick (fun () -> Helpers.repeat repetitions f))
    [
      ("test_of_array_zero", test_of_array_zero);
      ("test_of_array_const", test_of_array_const);
      ("test_of_array", test_of_array);
      ("test_zero", test_zero);
      ("test_is_zero", test_is_zero);
      ("test_copy", test_copy);
      ("test_copy_inplace", test_copy_inplace);
      ("test_mul_by_scalar", test_mul_by_scalar);
      ("test_mul_zero", test_mul_zero);
      ("test_mul_one", test_mul_one);
      ("test_mul_commutativity", test_mul_commutativity);
      ("test_mul_diff_size", test_mul_diff_size);
      ("test_mul_diff_size_composition_gx", test_mul_diff_size_composition_gx);
      ("test_mul", test_mul);
      ("test_linear_zero", test_linear_zero);
      ("test_linear_zero_const", test_linear_zero_const);
      ("test_linear_zero_composition_gx", test_linear_zero_composition_gx);
      ("test_linear_diff_size", test_linear_diff_size);
      ("test_linear", test_linear);
      ("test_add", test_add);
      ("test_add_zero", test_add_zero);
      ( "test_linear_with_powers_equal_length",
        test_linear_with_powers_equal_length );
      ("test_linear_with_powers", test_linear_with_powers);
      ("test_linear_with_powers_zeros", test_linear_with_powers_zeros);
    ]
