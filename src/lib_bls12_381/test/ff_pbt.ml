open Bls12_381

let max_output_alcotest = 100

let rec repeat ?(n = 100) f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat ~n:(n - 1) f)

(** Check the routine generators do not raise any exception *)
module MakeValueGeneration (FiniteField : Ff_sig.BASE) = struct
  let zero () = ignore @@ FiniteField.zero

  let random () = ignore @@ FiniteField.random ()

  let non_null_random () =
    ignore @@ not (FiniteField.is_zero (FiniteField.non_null_random ()))

  let one () = ignore @@ FiniteField.one

  let inverse_with_random_not_null () =
    let r = FiniteField.non_null_random () in
    ignore @@ FiniteField.inverse_exn r

  let inverse_with_one () = ignore @@ FiniteField.inverse_exn FiniteField.one

  let negation_with_random () =
    let random = FiniteField.random () in
    ignore @@ FiniteField.negate random

  let negation_with_zero () = ignore @@ FiniteField.negate FiniteField.zero

  let negation_with_one () = ignore @@ FiniteField.negate FiniteField.one

  let square_with_one () = ignore @@ FiniteField.square FiniteField.one

  let square_with_random () =
    let g = FiniteField.random () in
    ignore @@ FiniteField.square g

  let double_with_zero () = ignore @@ FiniteField.double FiniteField.zero

  let double_with_one () = ignore @@ FiniteField.double FiniteField.one

  let double_with_random () =
    let g = FiniteField.random () in
    ignore @@ FiniteField.double g

  let double_is_same_than_multiply_by_same_element () =
    let g = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.double g) (FiniteField.add g g))

  let get_tests () =
    let txt =
      Printf.sprintf
        "Value generation for field of order %s"
        (Z.to_string FiniteField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case "zero" `Quick (repeat zero);
        test_case "random" `Quick (repeat random);
        test_case "non null random" `Quick (repeat ~n:100 non_null_random);
        test_case
          "inverse_random_not_null"
          `Quick
          (repeat inverse_with_random_not_null);
        test_case "negate_with_one" `Quick (repeat negation_with_one);
        test_case "negate_with_zero" `Quick (repeat negation_with_zero);
        test_case "double_with_one" `Quick (repeat double_with_one);
        test_case "double_with_zero" `Quick (repeat double_with_zero);
        test_case "double_with_random" `Quick (repeat double_with_random);
        test_case "square_with_one" `Quick (repeat square_with_one);
        test_case "square_with_random" `Quick (repeat square_with_random);
        test_case "negate_with_random" `Quick (repeat negation_with_random);
        test_case
          "double_is_same_than_multiply_by_same_element"
          `Quick
          (repeat double_is_same_than_multiply_by_same_element);
        test_case "inverse_one" `Quick (repeat inverse_with_one);
      ] )
end

module MakeIsZero (FiniteField : Ff_sig.BASE) = struct
  let with_zero_value () = assert (FiniteField.is_zero FiniteField.zero = true)

  let rec with_random_value () =
    let x = FiniteField.random () in
    if FiniteField.is_zero x then with_random_value ()
    else assert (FiniteField.is_zero x = false)

  let get_tests () =
    let txt =
      Printf.sprintf
        "is_zero for field of order %s"
        (Z.to_string FiniteField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case "with zero value" `Quick (repeat with_zero_value);
        test_case "with random value" `Quick (repeat with_random_value);
      ] )
end

module MakeEquality (FiniteField : Ff_sig.BASE) = struct
  let zero_same_objects () =
    assert (FiniteField.eq FiniteField.zero FiniteField.zero)

  let one_same_objects () =
    assert (FiniteField.eq FiniteField.one FiniteField.one)

  let random_same_objects () =
    let random = FiniteField.random () in
    assert (FiniteField.eq random random)

  let get_tests () =
    let txt =
      Printf.sprintf
        "Equality for field of order %s"
        (Z.to_string FiniteField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case "zero_same_objects" `Quick (repeat zero_same_objects);
        test_case "one_same_objects" `Quick (repeat one_same_objects);
        test_case "random_same_objects" `Quick (repeat random_same_objects);
      ] )
end

module MakeFieldProperties (FiniteField : Ff_sig.BASE) = struct
  let zero_nullifier_random () =
    (* 0 * g = 0 *)
    let random = FiniteField.random () in
    assert (FiniteField.is_zero (FiniteField.mul FiniteField.zero random))

  let zero_nullifier_zero () =
    (* Special case 0 * 0 = 0 *)
    assert (
      FiniteField.is_zero (FiniteField.mul FiniteField.zero FiniteField.zero))

  let zero_nullifier_one () =
    (* Special case 0 * 1 = 0 *)
    assert (
      FiniteField.is_zero (FiniteField.mul FiniteField.zero FiniteField.one))

  let rec inverse_property () =
    let random = FiniteField.random () in
    if FiniteField.is_zero random then inverse_property ()
    else
      assert (
        FiniteField.eq
          (FiniteField.mul (FiniteField.inverse_exn random) random)
          FiniteField.one)

  let inverse_of_one_is_one () =
    assert (
      FiniteField.eq (FiniteField.inverse_exn FiniteField.one) FiniteField.one)

  let zero_has_no_inverse () =
    match FiniteField.inverse_opt FiniteField.zero with
    | Some _ -> assert false
    | None -> assert true

  let rec inverse_of_non_null_does_exist () =
    let random = FiniteField.random () in
    if FiniteField.is_zero random then inverse_of_non_null_does_exist ()
    else
      match FiniteField.inverse_opt random with
      | Some _ -> assert true
      | None -> assert false

  let rec inverse_of_inverse () =
    let random = FiniteField.random () in
    if FiniteField.is_zero random then inverse_of_inverse ()
    else
      assert (
        FiniteField.eq
          (FiniteField.inverse_exn (FiniteField.inverse_exn random))
          random)

  let opposite_property () =
    let random = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.add (FiniteField.negate random) random)
        FiniteField.zero)

  let opposite_of_opposite () =
    let random = FiniteField.random () in
    assert (
      FiniteField.eq (FiniteField.negate (FiniteField.negate random)) random)

  let opposite_of_zero_is_zero () =
    assert (
      FiniteField.eq (FiniteField.negate FiniteField.zero) FiniteField.zero)

  let additive_associativity () =
    let g1 = FiniteField.random () in
    let g2 = FiniteField.random () in
    let g3 = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.add (FiniteField.add g1 g2) g3)
        (FiniteField.add (FiniteField.add g2 g3) g1))

  let distributivity () =
    let g1 = FiniteField.random () in
    let g2 = FiniteField.random () in
    let g3 = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.mul (FiniteField.add g1 g2) g3)
        (FiniteField.add (FiniteField.mul g1 g3) (FiniteField.mul g2 g3)))

  let multiplicative_associativity () =
    let g1 = FiniteField.random () in
    let g2 = FiniteField.random () in
    let g3 = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.mul (FiniteField.mul g1 g2) g3)
        (FiniteField.mul (FiniteField.mul g2 g3) g1))

  (** 0**0 = 1 *)
  let pow_zero_to_zero_is_one () =
    assert (
      FiniteField.eq (FiniteField.pow FiniteField.zero Z.zero) FiniteField.one)

  (** 0 ** n = 0, n != 0 *)
  let pow_zero_to_non_null_exponent_is_zero () =
    let n = Z.of_int (Random.int 1_000_000_000) in
    assert (FiniteField.eq (FiniteField.pow FiniteField.zero n) FiniteField.zero)

  let pow_zero_on_random_equals_one () =
    let r = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.pow r Z.zero) FiniteField.one)

  let pow_zero_on_one_equals_one () =
    assert (
      FiniteField.eq (FiniteField.pow FiniteField.one Z.zero) FiniteField.one)

  let pow_one_on_random_element_equals_the_random_element () =
    let e = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.pow e Z.one) e)

  let pow_two_on_random_element_equals_the_square () =
    let e = FiniteField.random () in
    assert (
      FiniteField.eq (FiniteField.pow e (Z.succ Z.one)) (FiniteField.square e))

  (** x**(-n) = x**(g - 1 - n) where g is the order of the additive group *)
  let pow_to_negative_exponent () =
    let x = FiniteField.random () in
    let n = Z.of_int (Random.int 1_000_000_000) in
    assert (
      FiniteField.eq
        (FiniteField.pow x (Z.neg n))
        (FiniteField.pow x (Z.sub (Z.pred FiniteField.order) n)))

  let pow_addition_property () =
    let g = FiniteField.random () in
    let x = Z.of_int (Random.int 1_000_000_000) in
    let y = Z.of_int (Random.int 1_000_000_000) in
    assert (
      FiniteField.eq
        (FiniteField.pow g (Z.add x y))
        (FiniteField.mul (FiniteField.pow g x) (FiniteField.pow g y)))

  (** x**g = x where g = |(F, +, 0)| *)
  let pow_to_the_additive_group_order_equals_same_element () =
    let x = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.pow x FiniteField.order) x)

  (** x**g = 1 where g = |(F, *, 1)| *)
  let rec pow_to_the_multiplicative_group_order_equals_one () =
    let x = FiniteField.random () in
    if FiniteField.is_zero x then
      pow_to_the_multiplicative_group_order_equals_one ()
    else
      assert (
        FiniteField.eq
          (FiniteField.pow x (Z.pred FiniteField.order))
          FiniteField.one)

  (** x**(n + g) = x**n where g = |(F, *, 1)| *)
  let pow_add_multiplicative_group_order_to_a_random_power () =
    let x = FiniteField.random () in
    let n = Z.of_int (Random.int 1_000_000_000) in
    let order = Z.pred FiniteField.order in
    assert (
      FiniteField.eq (FiniteField.pow x (Z.add n order)) (FiniteField.pow x n))

  let sub_definition () =
    let x = FiniteField.random () in
    let y = FiniteField.random () in
    assert (FiniteField.(sub x y = add x (negate y)))

  let get_tests () =
    let txt =
      Printf.sprintf
        "Field properties for field of order %s"
        (Z.to_string FiniteField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case "zero_nullifier_one" `Quick (repeat zero_nullifier_one);
        test_case "zero_nullifier_zero" `Quick (repeat zero_nullifier_zero);
        test_case "zero_nullifier_random" `Quick (repeat zero_nullifier_random);
        test_case
          "inverse_of_non_null_does_exist"
          `Quick
          (repeat inverse_of_non_null_does_exist);
        test_case "inverse_of_one_is_one" `Quick (repeat inverse_of_one_is_one);
        test_case "zero_has_no_inverse" `Quick (repeat zero_has_no_inverse);
        test_case "inverse_of_inverse" `Quick (repeat inverse_of_inverse);
        test_case "opposite_of_opposite" `Quick (repeat opposite_of_opposite);
        test_case
          "opposite_of_zero_is_zero"
          `Quick
          (repeat opposite_of_zero_is_zero);
        test_case
          "additive_associativity"
          `Quick
          (repeat additive_associativity);
        test_case "distributivity" `Quick (repeat distributivity);
        test_case
          "pow zero on random element equals one"
          `Quick
          (repeat pow_zero_on_random_equals_one);
        test_case
          "pow zero on one equals one"
          `Quick
          (repeat pow_zero_on_one_equals_one);
        test_case
          "pow one on random element equals the same element"
          `Quick
          (repeat pow_one_on_random_element_equals_the_random_element);
        test_case
          "pow two on random element equals the square"
          `Quick
          (repeat pow_one_on_random_element_equals_the_random_element);
        test_case
          "pow element to the additive group order"
          `Quick
          (repeat pow_to_the_additive_group_order_equals_same_element);
        test_case
          "pow element to the multiplicative group order"
          `Quick
          (repeat pow_to_the_multiplicative_group_order_equals_one);
        test_case
          "pow element to a random power plus the additive group order"
          `Quick
          (repeat pow_add_multiplicative_group_order_to_a_random_power);
        test_case
          "pow zero to zero is one"
          `Quick
          (repeat ~n:1 pow_zero_to_zero_is_one);
        test_case
          "pow zero to non null exponent is zero"
          `Quick
          (repeat pow_zero_to_non_null_exponent_is_zero);
        test_case
          "pow to negative exponent"
          `Quick
          (repeat pow_to_negative_exponent);
        test_case "opposite property" `Quick (repeat opposite_property);
        test_case "inverse property" `Quick (repeat inverse_property);
        test_case "pow addition property" `Quick (repeat pow_addition_property);
        test_case "sub definition" `Quick (repeat sub_definition);
        test_case
          "multiplicative_associativity"
          `Quick
          (repeat multiplicative_associativity);
      ] )
end

module MakeMemoryRepresentation (FiniteField : Ff_sig.BASE) = struct
  let test_to_bytes_has_correct_size () =
    let x = FiniteField.random () in
    let x_bytes = FiniteField.to_bytes x in
    assert (Bytes.length x_bytes = FiniteField.size_in_bytes)

  let test_to_bytes_of_bytes_inverse () =
    let x = FiniteField.random () in
    let x_bytes = FiniteField.to_bytes x in
    assert (FiniteField.eq x (FiniteField.of_bytes_exn x_bytes))

  let get_tests () =
    let txt =
      Printf.sprintf
        "Memory representation for field of order %s"
        (Z.to_string FiniteField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case
          "to_bytes returns the correct number of bytes"
          `Quick
          (repeat test_to_bytes_has_correct_size);
        test_case
          "to_bytes and of bytes are inverses"
          `Quick
          (repeat test_to_bytes_of_bytes_inverse);
      ] )
end

module MakeQuadraticResidue (PrimeField : Ff_sig.PRIME) = struct
  let test_is_quadratic_residue () =
    let r = PrimeField.random () in
    assert (PrimeField.(is_quadratic_residue (r * r)))

  let get_tests () =
    let txt =
      Printf.sprintf
        "Quadratic residue tests for prime field of order %s"
        (Z.to_string PrimeField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case
          "With random elements and using its square"
          `Quick
          (repeat ~n:1000 test_is_quadratic_residue);
      ] )
end

module MakeRootOfUnity (PrimeField : Ff_sig.PRIME_WITH_ROOT_OF_UNITY) = struct
  let test_get_nth_root_of_unity_is_consistent_with_is_nth_root_of_unity () =
    let pred_order = Z.pred PrimeField.order in
    let n = Z.(ediv pred_order (one + one)) in
    assert (PrimeField.(is_nth_root_of_unity n (get_nth_root_of_unity n)))

  let get_tests () =
    let txt =
      Printf.sprintf
        "Root of unity tests for prime field of order %s"
        (Z.to_string PrimeField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) max_output_alcotest),
      [
        test_case
          "get_nth_root_of_unity is consistent with is_nth_root_of_unity"
          `Quick
          (repeat
             ~n:1000
             test_get_nth_root_of_unity_is_consistent_with_is_nth_root_of_unity);
      ] )
end

module MakeSquareRoot (PrimeField : Ff_sig.PRIME) = struct
  let test_square_root_on_random () =
    let r = PrimeField.random () in
    let res = Option.get @@ PrimeField.(sqrt_opt (square r)) in
    let res_neg = PrimeField.negate res in
    assert (PrimeField.(res = r || res = negate r)) ;
    assert (PrimeField.(res_neg = r || res_neg = negate r))

  let get_tests () =
    let txt =
      Printf.sprintf
        "Square root on finite field of order %s"
        (Z.to_string PrimeField.order)
    in
    let open Alcotest in
    ( String.sub txt 0 (min (String.length txt) 100),
      [
        test_case
          "With random elements and using its square"
          `Quick
          (repeat ~n:1000 test_square_root_on_random);
      ] )
end

module MakeAdditionalConstructors (F : Ff_sig.PRIME) = struct
  let test_positive_values_as_documented () =
    let n = Random.int 1_000_000 in
    let n_fr = F.of_int n in
    assert (F.(eq (of_z (Z.of_int n)) n_fr))

  let test_negative_values_as_documented () =
    let n = -Random.int 1_000_000 in
    let n_fr = F.of_int n in
    assert (F.(eq (of_z (Z.of_int n)) n_fr))

  let get_tests () =
    let open Alcotest in
    let txt =
      Printf.sprintf
        "Additional constructors on finite field of order %s"
        (Z.to_string F.order)
    in
    ( String.sub txt 0 (min (String.length txt) 100),
      [
        test_case
          "with positive values as documented"
          `Quick
          (repeat ~n:100 test_positive_values_as_documented);
        test_case
          "with negative values as documented"
          `Quick
          (repeat ~n:100 test_negative_values_as_documented);
      ] )
end

module MakeAll (FiniteField : Ff_sig.BASE) = struct
  module ValueGeneration = MakeValueGeneration (FiniteField)
  module IsZero = MakeIsZero (FiniteField)
  module Equality = MakeEquality (FiniteField)
  module FieldProperties = MakeFieldProperties (FiniteField)
  module MemoryRepresentation = MakeMemoryRepresentation (FiniteField)

  let get_tests () =
    [
      ValueGeneration.get_tests ();
      IsZero.get_tests ();
      Equality.get_tests ();
      FieldProperties.get_tests ();
      MemoryRepresentation.get_tests ();
    ]
end

module MakeAllPrime (FiniteField : Ff_sig.PRIME) = struct
  module ValueGeneration = MakeValueGeneration (FiniteField)
  module IsZero = MakeIsZero (FiniteField)
  module Equality = MakeEquality (FiniteField)
  module FieldProperties = MakeFieldProperties (FiniteField)
  module MemoryRepresentation = MakeMemoryRepresentation (FiniteField)
  module AdditionalConstructors = MakeAdditionalConstructors (FiniteField)
  module QuadraticResidueTests = MakeQuadraticResidue (FiniteField)
  module SquareRoot = MakeSquareRoot (FiniteField)

  let get_tests () =
    [
      ValueGeneration.get_tests ();
      IsZero.get_tests ();
      Equality.get_tests ();
      FieldProperties.get_tests ();
      QuadraticResidueTests.get_tests ();
      SquareRoot.get_tests ();
      AdditionalConstructors.get_tests ();
      MemoryRepresentation.get_tests ();
    ]
end
