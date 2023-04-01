let rec repeat n f () =
  if n > 0 then (
    f () ;
    repeat (n - 1) f ())

module MakeEquality (G : Ec_sig.BASE) = struct
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

module MakeValueGeneration (G : Ec_sig.BASE) = struct
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

  let addition_generates_valid_point () =
    assert (G.(check_bytes (to_bytes (add (random ()) (random ())))))

  let double_generates_valid_point () =
    assert (G.(check_bytes (to_bytes (double (random ())))))

  let scalar_multiplication_generates_valid_point () =
    assert (G.(check_bytes (to_bytes (mul (random ()) (Scalar.random ())))))

  let check_bytes_random_with_to_bytes () =
    let g = G.random () in
    assert (G.(check_bytes (to_bytes g)))

  let negate_generates_a_valid_point () =
    let g = G.random () in
    assert (G.(check_bytes (to_bytes (negate g))))

  let of_bytes_with_to_bytes_are_inverse_functions () =
    let g = G.random () in
    assert (G.(eq (of_bytes_exn (to_bytes g)) g))

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
        test_case
          "negate generates a valid point"
          `Quick
          (repeat 100 negate_generates_a_valid_point);
        test_case
          "addition generates a valid point"
          `Quick
          (repeat 100 addition_generates_valid_point);
        test_case
          "double generates a valid point"
          `Quick
          (repeat 100 double_generates_valid_point);
        test_case
          "scalar multiplication generates a valid point"
          `Quick
          (repeat 100 scalar_multiplication_generates_valid_point);
        test_case
          "of_bytes_exn and to_bytes are inverse functions"
          `Quick
          (repeat 100 of_bytes_with_to_bytes_are_inverse_functions);
        test_case
          "check bytes on random with to_bytes"
          `Quick
          (repeat 100 check_bytes_random_with_to_bytes);
        test_case "double_with_one" `Quick (repeat 1 double_with_one);
        test_case "double_with_zero" `Quick (repeat 100 double_with_zero);
      ] )
end

module MakeIsZero (G : Ec_sig.BASE) = struct
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

module MakeECProperties (G : Ec_sig.BASE) = struct
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

  (** Verify g1 + (g2 + g3) = (g1 + g2) + g3 *)
  let additive_associativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    let g3 = G.random () in
    assert (G.eq (G.add (G.add g1 g2) g3) (G.add g1 (G.add g2 g3)))

  (** Verify (g1 + g2) = (g2 + g1) *)
  let additive_commutativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.add g1 g2) (G.add g2 g1))

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

  (** Verify (-s) * g = s * (-g) *)
  let opposite_of_scalar_is_opposite_of_ec () =
    let s = G.Scalar.random () in
    let g = G.random () in
    let left = G.mul g (G.Scalar.negate s) in
    let right = G.mul (G.negate g) s in
    assert (G.eq left right)

  let generator_is_of_prime_order () =
    assert (G.(eq (mul one (G.Scalar.of_z G.Scalar.order)) zero))

  let mul_by_order_of_scalar_field_equals_zero () =
    let s = G.Scalar.random () in
    let g = G.random () in
    (* (g * s) * order = zero *)
    assert (G.(eq (mul (mul g s) (G.Scalar.of_z G.Scalar.order)) zero)) ;
    (* (one * s) * order = zero *)
    assert (G.(eq (mul (mul one s) (G.Scalar.of_z G.Scalar.order)) zero))

  (** Verify 2*g = g + g *)
  let double () =
    let s = G.random () in
    assert (G.(eq (double s) (add s s)))

  let inverse_on_scalar () =
    let g = G.random () in
    let a = G.Scalar.random () in
    let inv_a = G.Scalar.inverse_exn a in
    let ga = G.mul g a in
    let ga_inv = G.mul g inv_a in
    (* g * (a * a^(-1)) *)
    let res1 = G.mul g (G.Scalar.mul inv_a a) in
    (* (g * a^(-1)) * a *)
    let res2 = G.mul ga_inv a in
    (* (g * a) * a^(-1) *)
    let res3 = G.mul ga inv_a in
    assert (G.(eq res2 res3)) ;
    (* g * (a * a^(-1)) = g *)
    assert (G.(eq res1 g)) ;
    (* (g * a^(-1)) * a = g *)
    assert (G.(eq res2 g)) ;
    (* (g * a) * a^(-1) = g *)
    assert (G.(eq res3 g))

  let zero_is_the_identity () =
    let g = G.random () in
    assert (G.(eq (add g zero) (add zero g))) ;
    assert (G.(eq (add g zero) g))

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "Group properties",
      [
        test_case "check_bytes_random" `Quick (repeat 100 check_bytes_random);
        test_case "check_bytes_zero" `Quick (repeat 1 check_bytes_zero);
        test_case "check_bytes_one" `Quick (repeat 1 check_bytes_one);
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
        test_case
          "zero is the identity"
          `Quick
          (repeat 100 zero_is_the_identity);
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
          "mul_by_order_of_base_field_equals_element"
          `Quick
          (repeat 100 mul_by_order_of_scalar_field_equals_zero);
        test_case
          "multiplication_properties_on_base_field_element"
          `Quick
          (repeat 100 multiplication_properties_on_base_field_element);
        test_case "double" `Quick (repeat 100 double);
        test_case
          "additive_associativity_with_scalar"
          `Quick
          (repeat 100 additive_associativity_with_scalar);
        test_case "inverse on scalar" `Quick (repeat 100 inverse_on_scalar);
        test_case
          "additive_associativity"
          `Quick
          (repeat 100 additive_associativity);
        test_case
          "additive_commutativity"
          `Quick
          (repeat 100 additive_commutativity);
        test_case
          "Generator is of prime order"
          `Quick
          (repeat 1 generator_is_of_prime_order);
      ] )
end

module MakeEdwardsCurveProperties (G : Ec_sig.AffineEdwardsT) = struct
  let rec test_of_bytes_and_check_bytes_with_different_size_of_bytes () =
    (* Generate a random number of bytes between 0 and 10 * G.size_in_bytes. If
       the random value is the correct number of bytes, we ignore
    *)
    let b_size = Random.int (G.size_in_bytes * 10) in
    if b_size = G.size_in_bytes then
      test_of_bytes_and_check_bytes_with_different_size_of_bytes ()
    else
      let b = Bytes.create b_size in
      assert (not (G.check_bytes b)) ;
      assert (Option.is_none (G.of_bytes_opt b)) ;
      try
        ignore @@ G.of_bytes_exn b ;
        assert false
      with
      | G.Not_on_curve exn_bytes -> assert (Bytes.equal exn_bytes b)
      | _ -> assert false

  let test_unsafe_from_coordinates_do_not_check () =
    let u = G.Base.random () in
    let v = G.Base.random () in
    ignore @@ G.unsafe_from_coordinates ~u ~v

  let get_tests () =
    let open Alcotest in
    ( "Group properties of Edwards curve",
      [
        test_case
          "unsafe_from_coordinates do not check the point is on the curve"
          `Quick
          test_unsafe_from_coordinates_do_not_check;
        test_case
          "Test check_bytes and of_bytes_[exn/opt] with a different number of \
           bytes than expected"
          `Quick
          test_of_bytes_and_check_bytes_with_different_size_of_bytes;
      ] )
end

module MakeSerialisationProperties (G : Ec_sig.BASE) = struct
  let test_of_bytes_exn_to_bytes_are_inverse_functions () =
    let r = G.random () in
    assert (G.(eq (of_bytes_exn (to_bytes r)) r))

  let test_of_bytes_opt_to_bytes_are_inverse_functions () =
    let r = G.random () in
    assert (G.(eq (Option.get (of_bytes_opt (to_bytes r))) r))

  let get_tests () =
    let open Alcotest in
    ( "Serialisation",
      [
        test_case
          "of_bytes_exn and to_bytes are inverse functions"
          `Quick
          test_of_bytes_exn_to_bytes_are_inverse_functions;
        test_case
          "of_bytes_opt and to_bytes are inverse functions"
          `Quick
          test_of_bytes_opt_to_bytes_are_inverse_functions;
      ] )
end

module MakeCompressedSerialisationAffine (G : sig
  include Ec_sig.BASE

  val of_compressed_bytes_exn : Bytes.t -> t

  val of_compressed_bytes_opt : Bytes.t -> t option

  val to_compressed_bytes : t -> Bytes.t
end) =
struct
  let test_zero () =
    let expected_zero_bytes_compressed =
      Bytes.make (G.size_in_bytes / 2) '\000'
    in
    assert (
      Bytes.(
        equal (G.to_compressed_bytes G.zero) expected_zero_bytes_compressed))

  let test_of_compressed_bytes_exn_recover_correct_point_from_uncompressed_representation
      () =
    let g = G.random () in
    let compressed_bytes = G.to_compressed_bytes g in
    let uncompressed_g = G.of_compressed_bytes_exn compressed_bytes in
    assert (G.eq g uncompressed_g)

  let test_of_compressed_bytes_opt_recover_correct_point_from_uncompressed_representation
      () =
    let g = G.random () in
    let compressed_bytes = G.to_compressed_bytes g in
    let uncompressed_g =
      Option.get (G.of_compressed_bytes_opt compressed_bytes)
    in
    assert (G.eq g uncompressed_g)

  (* it is correct to test this for BLS12-381 *)
  let test_compressed_version_is_half_the_size () =
    let g = G.random () in
    assert (Bytes.length (G.to_compressed_bytes g) = G.size_in_bytes / 2)

  let test_of_compressed_bytes_exn_and_opt_do_not_accept_uncompressed_bytes_representation
      () =
    let x = G.random () in
    let x_uncompressed_bytes = G.to_bytes x in
    assert (Option.is_none (G.of_compressed_bytes_opt x_uncompressed_bytes)) ;
    try
      ignore @@ G.of_compressed_bytes_exn x_uncompressed_bytes ;
      assert false
    with G.Not_on_curve _b -> ()

  let test_of_bytes_exn_and_opt_do_not_accept_compressed_bytes_representation ()
      =
    let x = G.random () in
    let x_compressed_bytes = G.to_compressed_bytes x in
    assert (Option.is_none (G.of_bytes_opt x_compressed_bytes)) ;
    try
      ignore @@ G.of_bytes_exn x_compressed_bytes ;
      assert false
    with G.Not_on_curve _b -> ()

  let get_tests () =
    let open Alcotest in
    ( "Compressed representation",
      [
        test_case
          "Compressed representation of zero is the bs with zeroes"
          `Quick
          test_zero;
        test_case
          "of_compressed_bytes_exn recovers correct point from uncompressed \
           representation"
          `Quick
          (repeat
             100
             test_of_compressed_bytes_exn_recover_correct_point_from_uncompressed_representation);
        test_case
          "of_compressed_bytes_opt recovers correct point from uncompressed \
           representation"
          `Quick
          (repeat
             100
             test_of_compressed_bytes_opt_recover_correct_point_from_uncompressed_representation);
        test_case
          "Compressed version is half the size"
          `Quick
          test_compressed_version_is_half_the_size;
        test_case
          "of_compressed_bytes_exn/opt do not accept uncompressed bytes \
           representation"
          `Quick
          (repeat
             100
             test_of_compressed_bytes_exn_and_opt_do_not_accept_uncompressed_bytes_representation);
        test_case
          "of_bytes_exn/opt do not accept compressed bytes representation"
          `Quick
          (repeat
             100
             test_of_bytes_exn_and_opt_do_not_accept_compressed_bytes_representation);
      ] )
end
