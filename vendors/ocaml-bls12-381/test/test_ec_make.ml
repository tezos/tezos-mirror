let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module MakeEquality (G : Bls12_381.Elliptic_curve_sig.T) = struct
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
      [ test_case "zero" `Quick (repeat 1 zero);
        test_case "one" `Quick (repeat 1 one);
        test_case "random_same_objects" `Quick (repeat 100 random_same_objects)
      ] )
end

module MakeValueGeneration (G : Bls12_381.Elliptic_curve_sig.T) = struct
  let random () = ignore @@ G.random ()

  let negation_with_random () =
    let random = G.random () in
    ignore @@ G.negate random

  let negation_with_zero () = ignore @@ G.negate G.zero

  let negation_with_one () = ignore @@ G.negate G.one

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "value generation",
      [ test_case "random" `Quick (repeat 100 random);
        test_case "negate_with_one" `Quick (repeat 1 negation_with_one);
        test_case "negate_with_zero" `Quick (repeat 1 negation_with_zero);
        test_case "negate_with_random" `Quick (repeat 100 negation_with_random)
      ] )
end

module MakeIsZero (G : Bls12_381.Elliptic_curve_sig.T) = struct
  let with_zero_value () = assert (G.is_zero G.zero = true)

  let with_random_value () = assert (G.is_zero (G.random ()) = false)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "is_zero",
      [ test_case "with zero value" `Quick (repeat 100 with_zero_value);
        test_case "with random value" `Quick (repeat 100 with_random_value) ]
    )
end

module MakeECProperties (G : Bls12_381.Elliptic_curve_sig.T) = struct
  (** Verify that a random point is valid *)
  let check_bytes_random () = assert (G.(check_bytes @@ to_bytes @@ random ()))

  (** Verify that the zero point is valid *)
  let check_bytes_zero () = assert (G.(check_bytes @@ to_bytes @@ zero))

  (** Verify that the one point is valid *)
  let check_bytes_one () = assert (G.(check_bytes @@ to_bytes @@ one))

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

  (** Verify -(-g) = g where g is an element of the EC *)
  let opposite_of_opposite () =
    let random = G.random () in
    assert (G.eq (G.negate (G.negate random)) random)

  (** Verify -(-0_EC) = 0_EC where 0_EC is the point at infinity of the EC *)
  let opposite_of_zero_is_zero () = assert (G.eq (G.negate G.zero) G.zero)

  (** Verify g1 + (g2 + g3) = (g1 + g2) + g3 where g1, g2 and g3 are elements of the EC *)
  let additive_associativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    let g3 = G.random () in
    assert (G.eq (G.add (G.add g1 g2) g3) (G.add (G.add g2 g3) g1))

  (** Verify a (g1 + g2) = a * g1 + a * g2 where a is a scalar, g1, g2 two
  elements of the EC *)
  let distributivity () =
    let s = G.Scalar.random () in
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.mul (G.add g1 g2) s) (G.add (G.mul g1 s) (G.mul g2 s)))

  (** Verify (-s) * g = s * (-g) *)
  let opposite_of_scalar_is_opposite_of_ec () =
    let s = G.Scalar.random () in
    let g = G.random () in
    assert (G.eq (G.mul g (G.Scalar.negate s)) (G.mul (G.negate g) s))

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "Field properties",
      [ test_case "check_bytes_random" `Quick (repeat 100 check_bytes_random);
        test_case "check_bytes_zero" `Quick (repeat 1 check_bytes_zero);
        test_case "check_bytes_one" `Quick (repeat 1 check_bytes_one);
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
          "opposite_of_opposite"
          `Quick
          (repeat 100 opposite_of_opposite);
        test_case
          "opposite_of_zero_is_zero"
          `Quick
          (repeat 1 opposite_of_zero_is_zero);
        test_case "distributivity" `Quick (repeat 100 distributivity);
        test_case
          "opposite_of_scalar_is_opposite_of_ec"
          `Quick
          (repeat 100 opposite_of_scalar_is_opposite_of_ec);
        test_case
          "additive_associativity"
          `Quick
          (repeat 100 additive_associativity) ] )
end
