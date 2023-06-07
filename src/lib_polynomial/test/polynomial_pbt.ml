(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f)

let rec non_null_int bound =
  let r = Random.int bound in
  if r = 0 then non_null_int bound else r

module MakeTestConstant
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_zero () = assert (Poly.is_constant Poly.zero)

  let test_random () =
    assert (Poly.is_constant (Poly.constants (Scalar.random ())))

  let test_random_polynomials () =
    assert (
      not
        (Poly.is_constant
           (Poly.generate_random_polynomial
              (Polynomial.Natural (non_null_int 100)))))

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Tests for constant polynomials, field order = %s"
        (Z.to_string Scalar.order),
      [
        test_case "zero polynomial is constant" `Quick test_zero;
        test_case "Constant random value" `Quick (repeat 100 test_random);
        test_case
          "Non constant polynomial"
          `Quick
          (repeat 100 test_random_polynomials);
      ] )
end

module MakeTestDegree
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_degree_zero_is_infinity () =
    assert (Poly.degree Poly.zero = Polynomial.Infinity)

  let test_degree_of_constants_is_one () =
    assert (
      Poly.degree (Poly.constants (Scalar.random ())) = Polynomial.Infinity)

  let test_degree_int_test_vectors () =
    let vectors =
      [
        (Poly.zero, -1);
        (Poly.generate_random_polynomial (Polynomial.Natural 10), 10);
        (Poly.generate_random_polynomial (Polynomial.Natural 100), 100);
        (Poly.generate_random_polynomial (Polynomial.Natural 0), 0);
        (Poly.generate_random_polynomial (Polynomial.Natural 42), 42);
      ]
    in
    List.iter
      (fun (p, expected_result) -> assert (Poly.degree_int p = expected_result))
      vectors

  let test_have_same_degree () =
    let rec generate_random_non_null () =
      let r = Scalar.random () in
      if Scalar.is_zero r then generate_random_non_null () else r
    in
    let random_non_null = generate_random_non_null () in
    let test_vectors =
      [
        (Poly.zero, Poly.zero, true);
        (Poly.zero, Poly.constants random_non_null, false);
        (Poly.constants random_non_null, Poly.zero, false);
        (Poly.constants random_non_null, Poly.constants random_non_null, true);
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          true );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.zero,
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.constants (Scalar.random ()),
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 20),
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 20),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          false );
      ]
    in
    List.iter
      (fun (p, q, expected_result) ->
        assert (Poly.have_same_degree p q = expected_result))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Tests on degrees, field order = %s"
        (Z.to_string Scalar.order),
      [
        test_case
          "degree of zero is infinity"
          `Quick
          test_degree_zero_is_infinity;
        test_case
          "degree of constants is one"
          `Quick
          test_degree_zero_is_infinity;
        test_case "degree int test vectors" `Quick test_degree_int_test_vectors;
        test_case "have same degree" `Quick test_have_same_degree;
      ] )
end

module MakeTestEvaluation
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_eval_random_point_zero_polynomial () =
    assert (Scalar.is_zero (Poly.evaluation Poly.zero (Scalar.random ())))

  let test_eval_at_zero_of_zero_polynomial () =
    assert (Scalar.is_zero (Poly.evaluation Poly.zero Scalar.zero))

  let test_eval_at_zero_point_of_random_constant_polynomial () =
    let constant = Scalar.random () in
    assert (
      Scalar.eq (Poly.evaluation (Poly.constants constant) Scalar.zero) constant)

  let test_eval_random_point_constant_polynomial () =
    let constant = Scalar.random () in
    assert (
      Scalar.eq
        (Poly.evaluation (Poly.constants constant) (Scalar.random ()))
        constant)

  let test_eval_x_to_random_point () =
    let p = Scalar.random () in
    assert (
      Scalar.eq (Poly.evaluation (Poly.of_coefficients [(Scalar.one, 1)]) p) p)

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Test evaluation, field order = %s"
        (Z.to_string Scalar.order),
      [
        test_case
          "evaluation at any point of the zero polynomial"
          `Quick
          (repeat 100 test_eval_random_point_zero_polynomial);
        test_case
          "evaluation at any point of a random constant polynomial"
          `Quick
          (repeat 100 test_eval_random_point_constant_polynomial);
        test_case
          "evaluation at zero of a random constant polynomial"
          `Quick
          (repeat 100 test_eval_at_zero_point_of_random_constant_polynomial);
        test_case
          "evaluation at zero of the zero polynomial"
          `Quick
          (repeat 100 test_eval_at_zero_of_zero_polynomial);
        test_case
          "evaluation at any point of the polynomial X"
          `Quick
          (repeat 100 test_eval_x_to_random_point);
      ] )
end

module MakeTestLagrangeInterpolation
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let rec test_with_random_number_of_points () =
    let rec generate_evaluation_points i n acc =
      if i < n then
        let r = Scalar.random () in
        if List.mem r acc then generate_evaluation_points i n acc
        else generate_evaluation_points (i + 1) n (r :: acc)
      else acc
    in
    let n = Random.int 30 in
    if n <= 0 then test_with_random_number_of_points ()
    else
      let points =
        List.combine
          (generate_evaluation_points 0 n [])
          (List.init n (fun _i -> Scalar.random ()))
      in
      let interpolated_polynomial = Poly.lagrange_interpolation points in
      match Poly.degree interpolated_polynomial with
      | Polynomial.Infinity ->
          if
            List.length points = 1
            &&
            let _, x = List.hd points in
            Scalar.is_zero x
          then assert true
          else assert false
      | Natural n ->
          assert (n <= List.length points - 1) ;
          List.iter
            (fun (x, y) ->
              assert (Scalar.eq (Poly.evaluation interpolated_polynomial x) y))
            points

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Test lagrange interpolation, prime field order %s"
        (Z.to_string Scalar.order),
      [
        test_case
          "test random number of points"
          `Quick
          (repeat 10 test_with_random_number_of_points);
      ] )
end

module MakeTestEuclidianDivision
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_verify_equality_with_random () =
    let a = Poly.generate_random_polynomial (Polynomial.Natural 100) in
    let b = Poly.generate_random_polynomial (Polynomial.Natural 50) in
    let res = Poly.euclidian_division_opt a b in
    match res with
    | None -> assert false
    | Some (q, r) ->
        assert (Poly.equal a (Poly.add (Poly.polynomial_multiplication b q) r))

  let test_verify_equality_with_random_divided_by_constant () =
    let a =
      Poly.generate_random_polynomial (Polynomial.Natural (Random.int 1000))
    in
    let b = Poly.generate_random_polynomial (Polynomial.Natural 0) in
    let res = Poly.euclidian_division_opt a b in
    match res with
    | None -> assert false
    | Some (q, r) ->
        assert (Poly.equal a (Poly.add (Poly.polynomial_multiplication b q) r))

  let rec test_with_constants () =
    let a = Scalar.random () in
    let b = Scalar.random () in
    if Scalar.is_zero b || Scalar.is_zero a then test_with_constants ()
    else
      let res =
        Poly.euclidian_division_opt (Poly.constants a) (Poly.constants b)
      in
      match res with
      | None -> assert false
      | Some (q, r) ->
          assert (Poly.equal (Poly.constants Scalar.(a / b)) q && Poly.is_null r)

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Euclidian division for prime field %s"
        (Z.to_string Scalar.order),
      [
        test_case
          "test vectors for random"
          `Quick
          (repeat 10 test_verify_equality_with_random);
        test_case "test with constants" `Quick (repeat 10 test_with_constants);
        test_case
          "test vectors for random divided by constant"
          `Quick
          (repeat 10 test_verify_equality_with_random_divided_by_constant);
      ] )
end

module MakeTestDensifiedPolynomial
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_vectors () =
    let rec generate_non_null () =
      let r = Scalar.random () in
      if Scalar.is_zero r then generate_non_null () else r
    in
    let x = generate_non_null () in
    let zero = Scalar.zero in
    let test_vectors =
      [
        (Poly.zero, [Scalar.zero]);
        (Poly.constants x, [x]);
        (Poly.of_coefficients [(x, 2)], [x; zero; zero]);
        (Poly.of_coefficients [(x, 1)], [x; zero]);
        (Poly.of_coefficients [(x, 3); (x, 1)], [x; zero; x; zero]);
        (Poly.of_coefficients [(x, 4); (x, 1)], [x; zero; zero; x; zero]);
        ( Poly.of_coefficients [(x, 17); (x, 14); (x, 3); (x, 1); (x, 0)],
          [
            x;
            zero;
            zero;
            x;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            x;
            zero;
            x;
            x;
          ] );
      ]
    in
    List.iter
      (fun (v, expected_result) ->
        let r = Poly.get_dense_polynomial_coefficients v in
        assert (List.for_all2 Scalar.eq expected_result r))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( (Printf.sprintf "Dense polynomial coefficients for prime field %s")
        (Z.to_string Scalar.order),
      [test_case "test vectors" `Quick (repeat 10 test_vectors)] )
end

module MakeTestDensifiedPolynomialWithDegree
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_vectors () =
    let rec generate_non_null () =
      let r = Scalar.random () in
      if Scalar.is_zero r then generate_non_null () else r
    in
    let x = generate_non_null () in
    let zero = Scalar.zero in
    let test_vectors =
      [
        (Poly.zero, [(Scalar.zero, 0)]);
        (Poly.constants x, [(x, 0)]);
        (Poly.of_coefficients [(x, 2)], [(x, 2); (zero, 1); (zero, 0)]);
        (Poly.of_coefficients [(x, 1)], [(x, 1); (zero, 0)]);
        ( Poly.of_coefficients [(x, 3); (x, 1)],
          [(x, 3); (zero, 2); (x, 1); (zero, 0)] );
        ( Poly.of_coefficients [(x, 4); (x, 1)],
          [(x, 4); (zero, 3); (zero, 2); (x, 1); (zero, 0)] );
        ( Poly.of_coefficients [(x, 17); (x, 14); (x, 3); (x, 1); (x, 0)],
          [
            (x, 17);
            (zero, 16);
            (zero, 15);
            (x, 14);
            (zero, 13);
            (zero, 12);
            (zero, 11);
            (zero, 10);
            (zero, 9);
            (zero, 8);
            (zero, 7);
            (zero, 6);
            (zero, 5);
            (zero, 4);
            (x, 3);
            (zero, 2);
            (x, 1);
            (x, 0);
          ] );
      ]
    in
    List.iter
      (fun (v, expected_result) ->
        let r = Poly.get_dense_polynomial_coefficients_with_degree v in
        assert (
          List.for_all2
            (fun (e1, p1) (e2, p2) -> Scalar.eq e1 e2 && p1 = p2)
            expected_result
            r))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( (Printf.sprintf
         "Dense polynomial coefficients with degree for prime field %s")
        (Z.to_string Scalar.order),
      [test_case "test vectors" `Quick (repeat 10 test_vectors)] )
end

module MakeTestExtendedEuclide
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_random_properties () =
    let test poly_1 poly_2 =
      let gcd1, u1, v1 = Poly.extended_euclide poly_1 poly_2 in
      let gcd2, u2, v2 = Poly.extended_euclide poly_2 poly_1 in
      assert (Poly.equal gcd1 gcd2) ;
      assert (
        Poly.equal
          (Poly.add
             (Poly.polynomial_multiplication poly_1 u1)
             (Poly.polynomial_multiplication poly_2 v1))
          gcd1) ;
      assert (
        Poly.equal
          (Poly.add
             (Poly.polynomial_multiplication poly_2 u2)
             (Poly.polynomial_multiplication poly_1 v2))
          gcd2) ;
      if not (Poly.equal gcd1 Poly.zero) then (
        let remainder_poly_1 =
          Poly.euclidian_division_opt poly_1 gcd1 |> Option.get |> snd
        in
        assert (Poly.is_null remainder_poly_1) ;
        let remainder_poly_2 =
          Poly.euclidian_division_opt poly_2 gcd1 |> Option.get |> snd
        in
        assert (Poly.is_null remainder_poly_2))
    in
    let n = Random.int 100 in
    let m = Random.int 50 in
    let poly_1 = Poly.generate_random_polynomial (Polynomial.Natural n) in
    let poly_2 = Poly.generate_random_polynomial (Polynomial.Natural m) in
    let poly_3 = Poly.generate_random_polynomial (Polynomial.Natural n) in

    test poly_1 poly_2 ;
    test poly_1 Poly.zero ;
    test Poly.zero poly_1 ;
    test poly_1 poly_3 ;
    test poly_3 poly_1 ;
    test Poly.zero Poly.zero

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Extended Euclide alogrithm for prime field %s"
        (Z.to_string Scalar.order),
      [
        test_case
          "test properties on random polynomials"
          `Quick
          (repeat 10 test_random_properties);
      ] )
end

module MakeTestPolynomialMultiplication
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_multiply_by_zero_is_zero () =
    let r = Poly.generate_random_polynomial (Natural (Random.int 1000)) in
    assert (Poly.equal (Poly.polynomial_multiplication r Poly.zero) Poly.zero) ;
    assert (Poly.equal (Poly.polynomial_multiplication Poly.zero r) Poly.zero)

  let test_communitativity () =
    let p = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    let q = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    assert (
      Poly.equal
        (Poly.polynomial_multiplication p q)
        (Poly.polynomial_multiplication q p))

  let test_distributivity () =
    let a = Scalar.random () in
    let b = Scalar.random () in
    let p = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    let q = Poly.generate_random_polynomial (Natural (Random.int 30)) in
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar a p)
           (Poly.mult_by_scalar b q))
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar a q)
           (Poly.mult_by_scalar b p))) ;
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar Scalar.(a * b) p)
           q)
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar Scalar.(a * b) q)
           p)) ;
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           p
           (Poly.mult_by_scalar Scalar.(a * b) q))
        (Poly.polynomial_multiplication
           q
           (Poly.mult_by_scalar Scalar.(a * b) p))) ;
    assert (
      Poly.equal
        (Poly.polynomial_multiplication
           (Poly.mult_by_scalar a p)
           (Poly.mult_by_scalar b q))
        Poly.(mult_by_scalar Scalar.(a * b) (polynomial_multiplication p q)))

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Polynomial multiplication for prime field %s"
        (Z.to_string Scalar.order),
      [
        test_case
          "test properties nullifier 0 * P = P * 0 = 0"
          `Quick
          (repeat 10 test_multiply_by_zero_is_zero);
        test_case
          "test properties commutativity p * q = p * q"
          `Quick
          (repeat 10 test_communitativity);
        test_case
          "test properties distributivity and communtativity a p * b q = (a * \
           b) (p * q) = (b p) * (a q) = p * (a * b) q"
          `Quick
          (repeat 10 test_distributivity);
      ] )
end

module MakeTestInterpolationFFT
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_interpolation_fft_random_values_against_lagrange_interpolation
      ~generator ~power () =
    let random_polynomial =
      Poly.generate_random_polynomial (Polynomial.Natural (power - 1))
    in
    let evaluation_points =
      Poly.get_dense_polynomial_coefficients random_polynomial
    in
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let expected_results =
      Poly.lagrange_interpolation
        (List.combine (Array.to_list domain) evaluation_points)
    in
    let results = Poly.interpolation_fft ~domain evaluation_points in
    assert (Poly.equal results expected_results)

  let test_interpolation_fft_with_only_roots ~generator ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    (* only roots *)
    let evaluation_points = List.init power (fun _i -> Scalar.zero) in
    let expected_results =
      Poly.lagrange_interpolation
        (List.combine (Array.to_list domain) evaluation_points)
    in
    let results = Poly.interpolation_fft ~domain evaluation_points in
    assert (Poly.equal results expected_results) ;
    assert (Poly.is_null results)

  let get_tests ~domains () =
    let domains = List.map (fun (g, p) -> (Scalar.of_z g, p)) domains in
    let open Alcotest in
    ( Printf.sprintf "Inverse FFT for prime field %s" (Z.to_string Scalar.order),
      List.(
        flatten
          (map
             (fun (generator, power) ->
               [
                 test_case
                   "test interpolation at random points"
                   `Quick
                   (repeat
                      10
                      (test_interpolation_fft_random_values_against_lagrange_interpolation
                         ~generator
                         ~power));
                 test_case
                   "test interpolation with only roots"
                   `Quick
                   (repeat
                      10
                      (test_interpolation_fft_with_only_roots ~generator ~power));
               ])
             domains)) )
end

module MakeTestEvaluationFFT
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_evaluation_fft_zero ~generator ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let polynomial = Poly.zero in
    let results = Poly.evaluation_fft ~domain polynomial in
    let expected_results = List.init power (fun _ -> Scalar.zero) in
    if not (List.for_all2 Scalar.eq results expected_results) then
      let expected_values =
        String.concat "; " (List.map Scalar.to_string expected_results)
      in
      let values = String.concat "; " (List.map Scalar.to_string results) in
      Alcotest.failf
        "Expected values [%s]\nComputed [%s]"
        expected_values
        values

  let test_evaluation_fft_constant ~generator ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let s = Scalar.random () in
    let polynomial = Poly.constants s in
    let results = Poly.evaluation_fft ~domain polynomial in
    let expected_results = List.init power (fun _ -> s) in
    if not (List.for_all2 Scalar.eq results expected_results) then
      let expected_values =
        String.concat "; " (List.map Scalar.to_string expected_results)
      in
      let values = String.concat "; " (List.map Scalar.to_string results) in
      Alcotest.failf
        "Expected values [%s]\nComputed [%s]"
        expected_values
        values

  let test_evaluation_fft_random_values_against_normal_evaluation ~generator
      ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let polynomial =
      Poly.generate_random_polynomial (Polynomial.Natural (power - 1))
    in
    let expected_results =
      List.map (fun x -> Poly.evaluation polynomial x) (Array.to_list domain)
    in
    let results = Poly.evaluation_fft ~domain polynomial in
    if not (List.for_all2 Scalar.eq results expected_results) then
      let expected_values =
        String.concat "; " (List.map Scalar.to_string expected_results)
      in
      let values = String.concat "; " (List.map Scalar.to_string results) in
      Alcotest.failf
        "Expected values [%s]\nComputed [%s]"
        expected_values
        values

  let test_evaluation_fft_random_values_with_larger_polynomial ~generator ~power
      () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let polynomial =
      Poly.generate_random_polynomial
        (Polynomial.Natural (power + Random.int 100))
    in
    let expected_results =
      List.map (fun x -> Poly.evaluation polynomial x) (Array.to_list domain)
    in
    let results = Poly.evaluation_fft ~domain polynomial in
    if not (List.for_all2 Scalar.eq results expected_results) then
      let expected_values =
        String.concat "; " (List.map Scalar.to_string expected_results)
      in
      let values = String.concat "; " (List.map Scalar.to_string results) in
      Alcotest.failf
        "Expected values [%s]\nComputed [%s]"
        expected_values
        values

  let test_evaluation_fft_random_values_with_smaller_polynomial ~generator
      ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let polynomial =
      Poly.generate_random_polynomial
        (Polynomial.Natural (Random.int (power - 1)))
    in
    let expected_results =
      List.map (fun x -> Poly.evaluation polynomial x) (Array.to_list domain)
    in
    let results = Poly.evaluation_fft ~domain polynomial in
    if not (List.for_all2 Scalar.eq results expected_results) then
      let expected_values =
        String.concat "; " (List.map Scalar.to_string expected_results)
      in
      let values = String.concat "; " (List.map Scalar.to_string results) in
      Alcotest.failf
        "Expected values [%s]\nComputed [%s]"
        expected_values
        values

  let get_tests ~domains () =
    let domains = List.map (fun (g, p) -> (Scalar.of_z g, p)) domains in
    let open Alcotest in
    ( Printf.sprintf
        "Evaluation FFT for prime field %s"
        (Z.to_string Scalar.order),
      List.flatten
        (List.map
           (fun (generator, power) ->
             [
               test_case
                 "test evaluation at random points"
                 `Quick
                 (repeat
                    10
                    (test_evaluation_fft_random_values_against_normal_evaluation
                       ~generator
                       ~power));
               test_case
                 "test evaluation with zero polynomial"
                 `Quick
                 (test_evaluation_fft_zero ~generator ~power);
               test_case
                 "test evaluation with smaller polynomial"
                 `Quick
                 (repeat
                    10
                    (test_evaluation_fft_random_values_with_smaller_polynomial
                       ~generator
                       ~power));
               test_case
                 "test evaluation with larger polynomial"
                 `Quick
                 (repeat
                    10
                    (test_evaluation_fft_random_values_with_larger_polynomial
                       ~generator
                       ~power));
             ])
           domains) )
end

module MakeTestPolynomialMultiplicationFFT
    (Scalar : Bls12_381.Ff_sig.PRIME)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_with_zero_polynomial ~generator ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let p = Poly.zero in
    let q_is_zero = Random.bool () in
    let degree_q = if q_is_zero then -1 else Random.int power in
    let q =
      if q_is_zero then Poly.zero
      else Poly.generate_random_polynomial (Polynomial.Natural degree_q)
    in
    let p_times_q_fft = Poly.polynomial_multiplication_fft ~domain p q in
    assert (Poly.degree_int p_times_q_fft = -1) ;
    assert (Poly.is_null p_times_q_fft)

  let test_degree_non_null_polynomials ~generator ~power () =
    (* We generate two polynomials with any degree whose the sum is smaller than
       the domain size and we check the resulting polynomial has the expected degree.
    *)
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let degree_p = Random.int power in
    let degree_q = Random.int (power - degree_p) in
    let p = Poly.generate_random_polynomial (Polynomial.Natural degree_p) in
    let q = Poly.generate_random_polynomial (Polynomial.Natural degree_q) in
    let p_times_q_fft = Poly.polynomial_multiplication_fft ~domain p q in
    let p_times_q_fft_degree = Poly.degree_int p_times_q_fft in
    if p_times_q_fft_degree != degree_p + degree_q then
      Alcotest.failf
        "Expected degree of FFT multiplication: %d (%d + %d). Computed: %d"
        (degree_p + degree_q)
        degree_p
        degree_q
        p_times_q_fft_degree

  let test_commutativity ~generator ~power () =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let degree_p = Random.int power in
    let degree_q = Random.int (power - degree_p) in
    let p = Poly.generate_random_polynomial (Polynomial.Natural degree_p) in
    let q = Poly.generate_random_polynomial (Polynomial.Natural degree_q) in
    let q_times_p_fft = Poly.polynomial_multiplication_fft ~domain q p in
    let p_times_q_fft = Poly.polynomial_multiplication_fft ~domain p q in
    assert (Poly.equal q_times_p_fft p_times_q_fft)

  let test_random_values_fft_against_normal_multiplication ~generator ~power ()
      =
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) power generator
    in
    let degree_p = Random.int power in
    let degree_q = Random.int (power - degree_p) in
    let p = Poly.generate_random_polynomial (Polynomial.Natural degree_p) in
    let q = Poly.generate_random_polynomial (Polynomial.Natural degree_q) in
    let p_times_q = Poly.polynomial_multiplication p q in
    let p_times_q_fft = Poly.polynomial_multiplication_fft ~domain p q in
    if not (Poly.equal p_times_q_fft p_times_q) then
      Alcotest.failf
        "Fail on p = @[%s@] and q = @[%s@].@,Expected result is %s, computed %s"
        (Poly.to_string p)
        (Poly.to_string q)
        (Poly.to_string p_times_q)
        (Poly.to_string p_times_q_fft)

  let get_tests ~domains () =
    let domains = List.map (fun (g, p) -> (Scalar.of_z g, p)) domains in
    let open Alcotest in
    ( Printf.sprintf
        "Polynomial multiplication FFT for prime field %s"
        (Z.to_string Scalar.order),
      List.flatten
        (List.map
           (fun (generator, power) ->
             [
               test_case
                 "Compare FFT version of polynomial multiplication against \
                  normal polynomial multiplication"
                 `Quick
                 (repeat
                    20
                    (test_random_values_fft_against_normal_multiplication
                       ~generator
                       ~power));
               test_case
                 "At least one of the polynomial is zero"
                 `Quick
                 (repeat 20 (test_with_zero_polynomial ~generator ~power));
               test_case
                 "Verify the degree of P * Q is correct"
                 `Quick
                 (repeat
                    100
                    (test_degree_non_null_polynomials ~generator ~power));
               test_case
                 "Commutativity of polynomial multiplication using FFT"
                 `Quick
                 (repeat 20 (test_commutativity ~generator ~power));
             ])
           domains) )
end
