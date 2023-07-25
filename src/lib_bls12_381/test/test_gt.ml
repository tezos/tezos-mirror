open Utils

module type GROUP = module type of Bls12_381.GT

module MakeEquality (G : GROUP) = struct
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

module MakeValueGeneration (G : GROUP) = struct
  let test_random () = ignore @@ G.random ()

  let test_negation_with_random () =
    let random = G.random () in
    ignore @@ G.negate random

  let test_negation_with_zero () = ignore @@ G.negate G.zero

  let test_negation_with_one () = ignore @@ G.negate G.one

  let get_tests () =
    let open Alcotest in
    ( "value generation",
      [
        test_case "random" `Quick (repeat 100 test_random);
        test_case "negate_with_one" `Quick (repeat 1 test_negation_with_one);
        test_case "negate_with_zero" `Quick (repeat 1 test_negation_with_zero);
        test_case
          "negate_with_random"
          `Quick
          (repeat 100 test_negation_with_random);
      ] )
end

module MakeIsZero (G : GROUP) = struct
  let with_zero_value () = assert (G.is_zero G.zero = true)

  let with_one_value () = assert (G.is_zero G.one = false)

  let with_random_value () = assert (G.is_zero (G.random ()) = false)

  let get_tests () =
    let open Alcotest in
    ( "is_zero",
      [
        test_case "with zero value" `Quick (repeat 1 with_zero_value);
        test_case "with one value" `Quick (repeat 1 with_one_value);
        test_case "with random value" `Quick (repeat 100 with_random_value);
      ] )
end

module MakeIsOne (G : GROUP) = struct
  let with_zero_value () = assert (G.is_one G.zero = false)

  let with_one_value () = assert (G.is_one G.one = true)

  let with_random_value () = assert (G.is_one (G.random ()) = false)

  let get_tests () =
    let open Alcotest in
    ( "is_one",
      [
        test_case "with zero value" `Quick (repeat 1 with_zero_value);
        test_case "with one value" `Quick (repeat 1 with_one_value);
        test_case "with random value" `Quick (repeat 100 with_random_value);
      ] )
end

module MakeGroupProperties (G : GROUP) = struct
  (** Verify that a random point is valid *)
  let check_bytes_random () = assert (G.(check_bytes @@ to_bytes @@ random ()))

  (** Verify that the zero point is valid *)
  let check_bytes_zero () = assert (G.(check_bytes @@ to_bytes @@ zero))

  (** Verify that the fixed generator point is valid *)
  let check_bytes_one () = assert (G.(check_bytes @@ to_bytes @@ one))

  (** Verify that the sum of random points is valid *)
  let check_bytes_random_sum () =
    assert (G.(check_bytes @@ to_bytes @@ add (random ()) (random ())))

  (** Verify that multiplying a random point by a scalar gives a valid point *)
  let check_bytes_random_multiplication () =
    assert (
      G.(check_bytes @@ to_bytes @@ mul (random ()) (Bls12_381.Fr.random ())))

  (** Verify 0_S * g_EC = 0_EC where 0_S is the zero of the scalar field, 0_EC
      is the point at infinity and g_EC is an element of the EC *)
  let zero_scalar_nullifier_random () =
    let random = G.random () in
    assert (G.is_zero (G.mul random Bls12_381.Fr.zero))

  (** Verify 0_S * 0_EC = 0_EC where 0_S is the zero of the scalar field and
      0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_zero () =
    assert (G.is_zero (G.mul G.zero Bls12_381.Fr.zero))

  (** Verify 0_S * 1_EC = 0_EC where 0_S is the 0 of the scalar field, 1_EC is a
      fixed generator and 0_EC is the point at infinity of the EC *)
  let zero_scalar_nullifier_one () =
    assert (G.is_zero (G.mul G.one Bls12_381.Fr.zero))

  let multiply_by_one_does_nothing () =
    let g = G.random () in
    assert (G.(eq (mul g Bls12_381.Fr.one) g))

  (** Verify -(-g) = g where g is an element of the EC *)
  let opposite_of_opposite () =
    let random = G.random () in
    assert (G.eq (G.negate (G.negate random)) random)

  let opposite_of_opposite_using_scalar () =
    let r = G.random () in
    assert (
      G.(
        eq
          r
          (mul r (Bls12_381.Fr.negate (Bls12_381.Fr.negate Bls12_381.Fr.one)))))

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
    let s = Bls12_381.Fr.random () in
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.mul (G.add g1 g2) s) (G.add (G.mul g1 s) (G.mul g2 s)))

  (** Verify g1 + g2 = g2 + g1 where g1, g2 two are elements of the EC *)
  let commutativity () =
    let g1 = G.random () in
    let g2 = G.random () in
    assert (G.eq (G.add g1 g2) (G.add g2 g1))

  (** Verify (a + -a) * g = a * g - a * g = 0 *)
  let opposite_equality () =
    let a = Bls12_381.Fr.random () in
    let g = G.random () in
    assert (G.(eq (mul g (Bls12_381.Fr.add a (Bls12_381.Fr.negate a))) zero)) ;
    assert (G.(eq zero (add (mul g a) (mul g (Bls12_381.Fr.negate a))))) ;
    assert (
      G.(
        eq
          (mul g (Bls12_381.Fr.add a (Bls12_381.Fr.negate a)))
          (add (mul g a) (mul g (Bls12_381.Fr.negate a)))))

  (** a g + b + g = (a + b) g*)
  let additive_associativity_with_scalar () =
    let a = Bls12_381.Fr.random () in
    let b = Bls12_381.Fr.random () in
    let g = G.random () in
    let left = G.(add (mul g a) (mul g b)) in
    let right = G.(mul g (Bls12_381.Fr.add a b)) in
    assert (G.(eq left right))

  (** (a * b) g = a (b g) = b (a g) *)
  let multiplication_properties_on_base_field_element () =
    let a = Bls12_381.Fr.random () in
    let b = Bls12_381.Fr.random () in
    let g = G.random () in
    assert (G.(eq (mul g (Bls12_381.Fr.mul a b)) (mul (mul g a) b))) ;
    assert (G.(eq (mul g (Bls12_381.Fr.mul a b)) (mul (mul g b) a)))

  let random_is_in_prime_subgroup () =
    let g = G.random () in
    (* [order] g = 0 *)
    assert (G.(eq (mul g Bls12_381.Fr.(of_z order)) zero)) ;
    (* [order - 1 ] g = [-1] g *)
    assert (G.(eq (mul g Bls12_381.Fr.(of_z (Z.pred order))) (G.negate g)))

  (** Verify (-s) * g = s * (-g) *)
  let opposite_of_scalar_is_opposite_of_ec () =
    let s = Bls12_381.Fr.random () in
    let g = G.random () in
    let left = G.mul g (Bls12_381.Fr.negate s) in
    let right = G.mul (G.negate g) s in
    assert (G.eq left right)

  (** Returns the tests to be used with Alcotest *)
  let get_tests () =
    let open Alcotest in
    ( "Group properties",
      [
        test_case "check_bytes_random" `Quick (repeat 100 check_bytes_random);
        test_case "check_bytes_zero" `Quick (repeat 1 check_bytes_zero);
        test_case "check_bytes_one" `Quick (repeat 1 check_bytes_one);
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
        test_case "commutativity" `Quick (repeat 100 commutativity);
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

module Constructors = struct
  let test_value_in_fq12_but_not_in_prime_subgroup () =
    (* High probability a random point in Fq12 is not in the prime subgroup *)
    let values = [Bls12_381.Fq12.zero; Bls12_381.Fq12.random ()] in
    List.iter
      (fun x ->
        let x_bytes = Bls12_381.Fq12.to_bytes x in
        assert (not @@ Bls12_381.GT.check_bytes x_bytes) ;
        assert (Option.is_none (Bls12_381.GT.of_bytes_opt x_bytes)) ;
        try
          ignore @@ Bls12_381.GT.of_bytes_exn x_bytes ;
          assert false
        with Bls12_381.GT.Not_in_group _ -> ())
      values

  let get_tests () =
    let open Alcotest in
    ( "Constructors properties",
      [
        test_case
          "Values not in the prime subgroup"
          `Quick
          test_value_in_fq12_but_not_in_prime_subgroup;
      ] )
end

module ValueGeneration = MakeValueGeneration (Bls12_381.GT)
module IsZero = MakeIsZero (Bls12_381.GT)
module IsOne = MakeIsOne (Bls12_381.GT)
module Equality = MakeEquality (Bls12_381.GT)
module GroupProperties = MakeGroupProperties (Bls12_381.GT)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "GT"
    [
      IsZero.get_tests ();
      IsOne.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      Constructors.get_tests ();
      GroupProperties.get_tests ();
    ]
