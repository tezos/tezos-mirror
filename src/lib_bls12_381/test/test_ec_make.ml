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

open Utils

(* The [~jsoo] argument is used to generate smaller test inputs when
   running with js_of_ocaml so that testing time stays reasonable. *)
let random_int ~jsoo n =
  match Sys.backend_type with
  | Other "js_of_ocaml" -> Random.int jsoo
  | Native | Bytecode | Other _ -> Random.int n

module MakeBulkOperations (G : Bls12_381.CURVE) = struct
  let test_bulk_add () =
    let n = 10 + random_int ~jsoo:50 1_000 in
    let xs = List.init n (fun _ -> G.random ()) in
    assert (G.(eq (List.fold_left G.add G.zero xs) (G.add_bulk xs)))

  let test_pippenger () =
    let n = 1 + Random.int 5 in
    let start = Random.int n in
    let len = 1 + Random.int (n - start) in
    let ps = Array.init n (fun _ -> G.random ()) in
    let ss = Array.init n (fun _ -> G.Scalar.random ()) in
    let left =
      let ps = Array.sub ps start len in
      let ss = Array.sub ss start len in
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger ~start ~len ps ss in
    assert (G.(eq left right))

  let test_pippenger_different_size () =
    let n_ps = 1 + Random.int 10 in
    let n_ss = 1 + Random.int 10 in
    let ps = Array.init n_ps (fun _ -> G.random ()) in
    let ss = Array.init n_ss (fun _ -> G.Scalar.random ()) in
    let left =
      let n = min n_ps n_ss in
      let ps = Array.sub ps 0 n in
      let ss = Array.sub ss 0 n in
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger ps ss in
    assert (G.(eq left right))

  let test_to_affine_array () =
    let n = 1 + random_int 1000 ~jsoo:50 in
    let p = Array.init n (fun _ -> G.random ()) in
    let p' = G.of_affine_array (G.to_affine_array p) in
    let p = Array.to_list p in
    let p' = Array.to_list p' in
    assert (List.for_all2 G.eq p p')

  let test_size_of_affine_array () =
    let n = 1 + random_int 1000 ~jsoo:50 in
    let p = Array.init n (fun _ -> G.random ()) in
    let p = G.to_affine_array p in
    assert (G.size_of_affine_array p = n)

  let test_pippenger_contiguous () =
    let n = 1 + Random.int 10 in
    let ps = Array.init n (fun _ -> G.random ()) in
    let ps_contiguous = G.to_affine_array ps in
    let ss = Array.init n (fun _ -> G.Scalar.random ()) in
    let left =
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger_with_affine_array ps_contiguous ss in
    if not (G.eq left right) then
      Alcotest.failf
        "n = %d. Expected output is %s, computed %s"
        n
        (Hex.show (Hex.of_bytes (G.to_bytes left)))
        (Hex.show (Hex.of_bytes (G.to_bytes right)))

  let test_pippenger_contiguous_chunk () =
    let n = 10 + random_int 1000 ~jsoo:50 in
    let nb_chunks = 1 + Random.int 10 in
    let chunk_size = n / nb_chunks in
    let rest = n mod nb_chunks in
    let ps = Array.init n (fun _ -> G.random ()) in
    let ps_contiguous = G.to_affine_array ps in
    let ss = Array.init n (fun _ -> G.Scalar.random ()) in
    let left =
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right =
      let rec aux i acc =
        if i = nb_chunks then
          if rest <> 0 then
            let start = i * chunk_size in
            let len = rest in
            let res =
              G.pippenger_with_affine_array ~start ~len ps_contiguous ss
            in
            res :: acc
          else acc
        else
          let start = i * chunk_size in
          let len = chunk_size in
          let res =
            G.pippenger_with_affine_array ~start ~len ps_contiguous ss
          in
          let acc = res :: acc in
          aux (i + 1) acc
      in
      let l = aux 0 [] in
      List.fold_left G.add G.zero l
    in
    if not (G.eq left right) then
      Alcotest.failf
        "n = %d, chunk_size = %d, nb_chunks = %d. Expected output is %s, \
         computed %s"
        n
        chunk_size
        nb_chunks
        (Hex.show (Hex.of_bytes (G.to_bytes left)))
        (Hex.show (Hex.of_bytes (G.to_bytes right)))

  let test_pippenger_contiguous_different_size () =
    let n_ps = 1 + Random.int 10 in
    let n_ss = 1 + Random.int 10 in
    let ps = Array.init n_ps (fun _ -> G.random ()) in
    let ps_contiguous = G.to_affine_array ps in
    let ss = Array.init n_ss (fun _ -> G.Scalar.random ()) in
    let left =
      let n = min n_ps n_ss in
      let ps = Array.sub ps 0 n in
      let ss = Array.sub ss 0 n in
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger_with_affine_array ps_contiguous ss in
    if not (G.eq left right) then
      Alcotest.failf
        "n_ss = %d, n_ps = %d. Expected output is %s, computed %s"
        n_ss
        n_ps
        (Hex.show (Hex.of_bytes (G.to_bytes left)))
        (Hex.show (Hex.of_bytes (G.to_bytes right)))

  let test_pippenger_contiguous_with_start_argument () =
    let n_ps = 1 + Random.int 10 in
    let n_ss = 1 + Random.int 10 in
    let ps = Array.init n_ps (fun _ -> G.random ()) in
    let ps_contiguous = G.to_affine_array ps in

    let ss = Array.init n_ss (fun _ -> G.Scalar.random ()) in
    let n = min n_ps n_ss in
    let start = Random.int n in
    let left =
      let ps = Array.sub ps start (n - start) in
      let ss = Array.sub ss start (n - start) in
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger_with_affine_array ~start ps_contiguous ss in
    if not (G.eq left right) then
      Alcotest.failf
        "n = %d, start = %d. Expected output is %s, computed %s"
        n
        start
        (Hex.show (Hex.of_bytes (G.to_bytes left)))
        (Hex.show (Hex.of_bytes (G.to_bytes right)))

  let test_pippenger_contiguous_with_len_argument () =
    let n_ps = 1 + Random.int 10 in
    let n_ss = 1 + Random.int 10 in
    let n = min n_ps n_ss in
    let len = 1 + Random.int n in
    let ps = Array.init n_ps (fun _ -> G.random ()) in
    let ps_contiguous = G.to_affine_array ps in

    let ss = Array.init n_ss (fun _ -> G.Scalar.random ()) in
    let left =
      let ps = Array.sub ps 0 len in
      let ss = Array.sub ss 0 len in
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger_with_affine_array ~len ps_contiguous ss in
    assert (G.(eq left right))

  let test_pippenger_contiguous_with_start_and_len_argument () =
    let n_ps = 1 + Random.int 10 in
    let n_ss = 1 + Random.int 10 in
    let ps = Array.init n_ps (fun _ -> G.random ()) in
    let ps_contiguous = G.to_affine_array ps in
    let ss = Array.init n_ss (fun _ -> G.Scalar.random ()) in
    let n = min n_ps n_ss in
    let start = Random.int n in
    let len = 1 + Random.int (n - start) in
    let left =
      let ps = Array.sub ps start len in
      let ss = Array.sub ss start len in
      let xs = List.combine (Array.to_list ps) (Array.to_list ss) in
      List.fold_left (fun acc (g, n) -> G.add acc (G.mul g n)) G.zero xs
    in
    let right = G.pippenger_with_affine_array ~start ~len ps_contiguous ss in
    assert (G.(eq left right))

  let get_tests () =
    let open Alcotest in
    ( "Bulk operations",
      [
        test_case "bulk add" `Quick (repeat 10 test_bulk_add);
        test_case "to_affine_array" `Quick (repeat 10 test_to_affine_array);
        test_case
          "size_of_affine_array"
          `Quick
          (repeat 10 test_size_of_affine_array);
        test_case "pippenger" `Quick (repeat 10 test_pippenger);
        test_case
          "pippenger continuous chunk size"
          `Quick
          (repeat 10 test_pippenger_contiguous_chunk);
        test_case
          "pippenger different size"
          `Quick
          (repeat 10 test_pippenger_different_size);
        test_case
          "pippenger contiguous"
          `Quick
          (repeat 10 test_pippenger_contiguous);
        test_case
          "pippenger contiguous with different size"
          `Quick
          (repeat 10 test_pippenger_contiguous_different_size);
        test_case
          "pippenger contiguous with start argument"
          `Quick
          (repeat 10 test_pippenger_contiguous_with_start_argument);
        test_case
          "pippenger contiguous with start and len argument"
          `Quick
          (repeat 10 test_pippenger_contiguous_with_start_and_len_argument);
        test_case
          "pippenger contiguous with len argument"
          `Quick
          (repeat 10 test_pippenger_contiguous_with_len_argument);
      ] )
end

module MakeInplaceOperations (G : Bls12_381.CURVE) = struct
  let test_mul_inplace () =
    let n = G.Scalar.random () in
    let g = G.random () in
    let res = G.mul g n in
    G.mul_inplace g n ;
    assert (G.eq g res)

  let test_add_inplace () =
    let x = G.random () in
    let y = G.random () in
    let res = G.add x y in
    G.add_inplace x y ;
    assert (G.eq x res)

  let get_tests () =
    let txt = "Inplace operations" in
    let open Alcotest in
    ( txt,
      [
        test_case "mul inplace" `Quick (repeat 100 test_mul_inplace);
        test_case "add inplace" `Quick (repeat 100 test_add_inplace);
      ] )
end

module MakeEquality (G : Bls12_381.CURVE) = struct
  (** Verify the equality is correct with the value zero *)
  let zero () = assert (G.eq G.zero G.zero)

  (** Verify the equality is correct with the value one *)
  let one () = assert (G.eq G.one G.one)

  (** Verify the equality of two random values created invidually *)
  let random_same_objects () =
    let random = G.random () in
    assert (G.eq random random)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "equality",
      [
        test_case "zero" `Quick (repeat 1 zero);
        test_case "one" `Quick (repeat 1 one);
        test_case "random_same_objects" `Quick (repeat 100 random_same_objects);
      ] )
end

module MakeValueGeneration (G : Bls12_381.CURVE) = struct
  let random () = ignore @@ G.random ()

  let negation_with_random () =
    let random = G.random () in
    ignore @@ G.negate random

  let negation_with_zero () = ignore @@ G.negate G.zero

  let negation_with_one () = ignore @@ G.negate G.one

  let double_with_zero () = ignore @@ G.double G.zero

  let double_with_one () = ignore @@ G.double G.one

  let double_with_random () =
    let g = G.random () in
    ignore @@ G.double g

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "value generation",
      [
        test_case "random" `Quick (repeat 100 random);
        test_case "negate_with_one" `Quick (repeat 1 negation_with_one);
        test_case "negate_with_zero" `Quick (repeat 1 negation_with_zero);
        test_case "negate_with_random" `Quick (repeat 100 negation_with_random);
        test_case "double_with_random" `Quick (repeat 100 double_with_random);
        test_case "double_with_one" `Quick (repeat 1 double_with_one);
        test_case "double_with_zero" `Quick (repeat 100 double_with_zero);
      ] )
end

module MakeIsZero (G : Bls12_381.CURVE) = struct
  let with_zero_value () = assert (G.is_zero G.zero = true)

  let with_one_value () = assert (G.is_zero G.one = false)

  let with_random_value () = assert (G.is_zero (G.random ()) = false)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "is_zero",
      [
        test_case "with zero value" `Quick (repeat 1 with_zero_value);
        test_case "with one value" `Quick (repeat 1 with_one_value);
        test_case "with random value" `Quick (repeat 100 with_random_value);
      ] )
end

module MakeECProperties (G : Bls12_381.CURVE) = struct
  (** Verify that a random point is valid *)
  let check_bytes_random () = assert (G.(check_bytes @@ to_bytes @@ random ()))

  (** Verify that the zero point is valid *)
  let check_bytes_zero () = assert (G.(check_bytes @@ to_bytes @@ zero))

  (** Verify that the fixed generator point is valid *)
  let check_bytes_one () = assert (G.(check_bytes @@ to_bytes @@ one))

  (** Verify that doubling a random point gives a valid point *)
  let check_bytes_random_double () =
    assert (G.(check_bytes @@ to_bytes @@ double (random ())))

  (** Verify that the sum of random points is valid *)
  let check_bytes_random_sum () =
    assert (G.(check_bytes @@ to_bytes @@ add (random ()) (random ())))

  (** Verify that multiplying a random point by a scalar gives a valid point *)
  let check_bytes_random_multiplication () =
    assert (G.(check_bytes @@ to_bytes @@ mul (random ()) (Scalar.random ())))

  (** Verify 0_S * g_EC = 0_EC where 0_S is the zero of the scalar field, 0_EC
      is the point at infinity and g_EC is an element of the EC *)
  let zero_scalar_nullifier_random () =
    let random = G.random () in
    assert (G.is_zero (G.mul random G.Scalar.zero))

  (** Verify 0_S * 0_EC = 0_EC where 0_S is the zero of the scalar field and
      0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_zero () =
    assert (G.is_zero (G.mul G.zero G.Scalar.zero))

  (** Verify 0_S * 1_EC = 0_EC where 0_S is the 0 of the scalar field, 1_EC is a
      fixed generator and 0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_one () =
    assert (G.is_zero (G.mul G.one G.Scalar.zero))

  let multiply_by_one_does_nothing () =
    let g = G.random () in
    assert (G.(eq (mul g Scalar.one) g))

  (** Verify -(-g) = g where g is an element of the EC *)
  let opposite_of_opposite () =
    let random = G.random () in
    assert (G.eq (G.negate (G.negate random)) random)

  let opposite_of_opposite_using_scalar () =
    let r = G.random () in
    assert (G.(eq r (mul r (Scalar.negate (Scalar.negate Scalar.one)))))

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_zero_is_zero () = assert (G.eq (G.negate G.zero) G.zero)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_opposite_of_zero_is_zero () =
    assert (G.eq (G.negate (G.negate G.zero)) G.zero)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_opposite_of_one_is_one () =
    assert (G.eq (G.negate (G.negate G.one)) G.one)

  (** Verify g1 + (g2 + g3) = (g1 + g2) + g3 where g1, g2 and g3 are elements of
      the EC *)
  let additive_associativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    let g3 = G.random () in
    assert (G.eq (G.add (G.add g1 g2) g3) (G.add (G.add g2 g3) g1))

  (** Verify that g + (-g) = 0 *)
  let opposite_existential_property () =
    let g = G.random () in
    assert (G.(eq (add g (negate g)) zero))

  (** Verify a (g1 + g2) = a * g1 + a * g2 where a is a scalar, g1, g2 two
      elements of the EC *)
  let distributivity () =
    let s = G.Scalar.random () in
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.mul (G.add g1 g2) s) (G.add (G.mul g1 s) (G.mul g2 s)))

  (** Verify (a + -a) * g = a * g - a * g = 0 *)
  let opposite_equality () =
    let a = G.Scalar.random () in
    let g = G.random () in
    assert (G.(eq (mul g (Scalar.add a (Scalar.negate a))) zero)) ;
    assert (G.(eq zero (add (mul g a) (mul g (Scalar.negate a))))) ;
    assert (
      G.(
        eq
          (mul g (Scalar.add a (Scalar.negate a)))
          (add (mul g a) (mul g (Scalar.negate a)))))

  (** a g + b + g = (a + b) g*)
  let additive_associativity_with_scalar () =
    let a = G.Scalar.random () in
    let b = G.Scalar.random () in
    let g = G.random () in
    let left = G.(add (mul g a) (mul g b)) in
    let right = G.(mul g (Scalar.add a b)) in
    assert (G.(eq left right))

  (** (a * b) g = a (b g) = b (a g) *)
  let multiplication_properties_on_base_field_element () =
    let a = G.Scalar.random () in
    let b = G.Scalar.random () in
    let g = G.random () in
    assert (G.(eq (mul g (Scalar.mul a b)) (mul (mul g a) b))) ;
    assert (G.(eq (mul g (Scalar.mul a b)) (mul (mul g b) a)))

  let random_is_in_prime_subgroup () =
    let g = G.random () in
    (* [order] g = 0 *)
    assert (G.(eq (mul g Scalar.(of_z order)) zero)) ;
    (* [order - 1 ] g = [-1] g *)
    assert (G.(eq (mul g Scalar.(of_z (Z.pred order))) (G.negate g)))

  (** Verify (-s) * g = s * (-g) *)
  let opposite_of_scalar_is_opposite_of_ec () =
    let s = G.Scalar.random () in
    let g = G.random () in
    let left = G.mul g (G.Scalar.negate s) in
    let right = G.mul (G.negate g) s in
    assert (G.eq left right)

  (** Verify 2*g = g + g *)
  let double () =
    let s = G.random () in
    assert (G.(eq (double s) (add s s)))

  let test_bulk_add () =
    let n = 10 + random_int 1000 ~jsoo:50 in
    let xs = List.init n (fun _ -> G.random ()) in
    assert (G.(eq (List.fold_left G.add G.zero xs) (G.add_bulk xs)))

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "Curve properties",
      [
        test_case "check_bytes_random" `Quick (repeat 100 check_bytes_random);
        test_case "check_bytes_zero" `Quick (repeat 1 check_bytes_zero);
        test_case "check_bytes_one" `Quick (repeat 1 check_bytes_one);
        test_case "bulk add" `Quick (repeat 100 test_bulk_add);
        test_case
          "check_bytes_random_double"
          `Quick
          (repeat 100 check_bytes_random_double);
        test_case
          "check_bytes_random_sum"
          `Quick
          (repeat 100 check_bytes_random_sum);
        test_case
          "check_bytes_random_multiplication"
          `Quick
          (repeat 100 check_bytes_random_multiplication);
        test_case
          "zero_scalar_nullifier_one"
          `Quick
          (repeat 1 zero_scalar_nullifier_one);
        test_case
          "zero_scalar_nullifier_zero"
          `Quick
          (repeat 1 zero_scalar_nullifier_zero);
        test_case
          "zero_scalar_nullifier_random"
          `Quick
          (repeat 100 zero_scalar_nullifier_random);
        test_case
          "multiply_by_one_does_nothing"
          `Quick
          (repeat 100 multiply_by_one_does_nothing);
        test_case
          "opposite_of_opposite"
          `Quick
          (repeat 100 opposite_of_opposite);
        test_case
          "opposite_of_opposite_using_scalar"
          `Quick
          (repeat 100 opposite_of_opposite_using_scalar);
        test_case
          "opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_zero_is_zero);
        test_case
          "opposite_of_opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_opposite_of_zero_is_zero);
        test_case
          "opposite_of_opposite_of_one_is_one"
          `Quick
          (repeat 1 opposite_of_opposite_of_one_is_one);
        test_case "opposite_equality" `Quick (repeat 1 opposite_equality);
        test_case "distributivity" `Quick (repeat 100 distributivity);
        test_case
          "opposite_of_scalar_is_opposite_of_ec"
          `Quick
          (repeat 100 opposite_of_scalar_is_opposite_of_ec);
        test_case
          "opposite_existential_property"
          `Quick
          (repeat 100 opposite_existential_property);
        test_case
          "multiplication_properties_on_base_field_element"
          `Quick
          (repeat 100 multiplication_properties_on_base_field_element);
        test_case "double" `Quick (repeat 100 double);
        test_case
          "additive_associativity_with_scalar"
          `Quick
          (repeat 100 additive_associativity_with_scalar);
        test_case
          "random elements are generated in the prime subgroup"
          `Quick
          (repeat 100 random_is_in_prime_subgroup);
        test_case
          "additive_associativity"
          `Quick
          (repeat 100 additive_associativity);
      ] )
end

module MakeCompressedRepresentation (G : Bls12_381.CURVE) = struct
  let test_recover_correct_point_uncompressed () =
    let g = G.random () in
    let compressed_bytes = G.to_compressed_bytes g in
    let uncompressed_g = G.of_compressed_bytes_exn compressed_bytes in
    assert (G.eq g uncompressed_g)

  (* it is correct to test this for BLS12-381 *)
  let test_compressed_version_is_half_the_size () =
    let g = G.random () in
    assert (Bytes.length (G.to_compressed_bytes g) = G.compressed_size_in_bytes)

  let test_most_significant_bit_is_set_to_1 () =
    let g = G.random () in
    let compressed_g_bytes = G.to_compressed_bytes g in
    let first_byte = int_of_char @@ Bytes.get compressed_g_bytes 0 in
    assert (first_byte land 0b10000000 = 0b10000000)

  let test_check_second_most_significant_bit_is_set_to_1_for_zero () =
    let g = G.zero in
    let compressed_g_bytes = G.to_compressed_bytes g in
    let first_byte = int_of_char @@ Bytes.get compressed_g_bytes 0 in
    assert (first_byte land 0b01000000 = 0b01000000)

  let test_compressed_version_x_in_big_endian () =
    (* The compressed version fully carries the x coordinate. For G1, the
       compressed version is 384 bits with the 3 most significant bits being
       used to carry information to compute the y coordinate. For G2, as it is
       built on Fp2, the compressed version is (384 * 2) bits and the most
       significant 3 bits carries the same information. The bits 385, 386 and
       387 (i.e. the last 3 bits of the constant coefficient of the x
       coordinate) are set to zero/unused. *)
    let g = G.random () in
    let g_bytes = G.to_bytes g in
    let x_bytes_be = Bytes.sub g_bytes 0 G.compressed_size_in_bytes in
    let compressed_g_bytes = G.to_compressed_bytes g in
    let compressed_g_bytes_first_byte = Bytes.get compressed_g_bytes 0 in
    (* Get rid of the last 3 bits as it carries information unrelated to x *)
    let compressed_g_bytes_first_byte =
      int_of_char compressed_g_bytes_first_byte land 0b00011111
    in
    Bytes.set compressed_g_bytes 0 (char_of_int compressed_g_bytes_first_byte) ;
    assert (Bytes.equal x_bytes_be compressed_g_bytes)

  let get_tests () =
    let open Alcotest in
    ( "Compressed representation",
      [
        test_case
          "Recover correct point"
          `Quick
          test_recover_correct_point_uncompressed;
        test_case
          "Most significant bit is set to 1"
          `Quick
          (repeat 100 test_most_significant_bit_is_set_to_1);
        test_case
          "Second most significant bit is set to 1 for the identity element"
          `Quick
          test_check_second_most_significant_bit_is_set_to_1_for_zero;
        test_case
          "Verify x is fully in the compressed version and in big endian"
          `Quick
          (repeat 100 test_compressed_version_x_in_big_endian);
        test_case
          "Compressed version is half the size"
          `Quick
          test_compressed_version_is_half_the_size;
      ] )
end
