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
module Fr_generation = Octez_bls12_381_polynomial.Fr_carray
module Domain = Octez_bls12_381_polynomial.Domain.Domain_unsafe

let factors_naive n =
  let rec factors_aux n factor result =
    if n = Z.one then result
    else if Z.divisible n factor then
      let n, valuation = Z.(remove n factor) in
      factors_aux n Z.(nextprime factor) ((factor, valuation) :: result)
    else factors_aux n Z.(nextprime factor) result
  in
  factors_aux n Z.(succ one) []

(* Samples a primitive p^e-th root of unity for p prime. *)
let primitive_root_of_unity_prime_power group_order p e =
  let exponent = Z.divexact group_order p in
  let rec find_element r =
    if Fr.(eq one (pow r exponent)) then find_element Fr.(add r one) else r
  in
  Fr.pow (find_element (Fr.of_int 5330)) (Z.divexact group_order (Z.pow p e))

(* Samples a primitive [n]-th root of unity. *)
let primitive_nth_root_of_unity n factorization_n =
  let group_order = Z.(Fr.order - one) in
  if not (Z.divisible group_order n) then
    raise
      (Invalid_argument
         (Format.sprintf
            "There do not exist %s-th roots of unity"
            (Z.to_string n)))
  else
    (* multiplies primitive roots of unity of order p^e from the factorization
       of n to obtain a primitive n-th root of unity. *)
    List.fold_left
      (fun primroot (p, e) ->
        let root_factor = primitive_root_of_unity_prime_power group_order p e in
        Fr.mul primroot root_factor)
      Fr.one
      factorization_n

let test_generator () =
  let generator = Fr_generation.generator in
  let factorization_n =
    (* output of {[ Fr_generation.factors_naive Z.(Bls12_381.Fr.order - one) |> List.map (fun (i,j) -> Z.to_string i, j) ]} *)
    [
      ("254760293", 2);
      ("52437899", 1);
      ("2529403", 1);
      ("2508409", 1);
      ("906349", 2);
      ("859267", 1);
      ("125527", 1);
      ("10177", 1);
      ("19", 1);
      ("11", 1);
      ("3", 1);
      ("2", 32);
    ]
    |> List.map (fun (i, j) -> (Z.of_string i, j))
  in
  (* |Fr^*|-th primitive root of unity, or primitive element of Fr^*,
     aka. generator of Fr^* *)
  let primitive_root =
    primitive_nth_root_of_unity Z.(Fr.order - one) factorization_n
  in
  let is_generator =
    Z.(equal (gcd (Fr.to_z primitive_root) (Fr.to_z generator)) one)
  in
  (* [generator] is a generator of Fr^* if [generator] is a primitive |Fr^*|-th root of unity,
     ie. [generator] and a primitive |Fr^*|-th root of unity (here [primitive_root]) are coprime *)
  assert is_generator

let test_build_domain () =
  let n = 19 in
  let domain_c = Domain.build n in
  let expected_domain = Fr_generation.build_domain n in
  let domain = Domain.to_array domain_c in
  assert (Array.for_all2 Fr.eq expected_domain domain)

let test_build_domain_power_of_two () =
  let log = 5 in
  let domain_c = Domain.build_power_of_two log in
  let expected_domain = Fr_generation.build_domain_power_of_two ~log in
  let domain = Domain.to_array domain_c in
  assert (Array.for_all2 Fr.eq expected_domain domain)

let test_domain_to_array () =
  let domain = Domain.build_power_of_two 4 in
  let domain_caml = Domain.to_array domain in
  let domain_expected = Fr_generation.build_domain_power_of_two ~log:4 in
  assert (Array.for_all2 Fr.eq domain_caml domain_expected)

let tests =
  let repetitions = 100 in
  List.map
    (fun (name, f) ->
      Alcotest.test_case name `Quick (fun () -> Helpers.repeat repetitions f))
    [
      ("test_generator", test_generator);
      ("build_domain", test_build_domain);
      ("build_domain_power_of_two", test_build_domain_power_of_two);
      ("to_domain_array", test_domain_to_array);
    ]
