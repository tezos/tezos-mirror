open Mec.Hash

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Poseidon = Poseidon128.Make (Scalar)

let test_perm_is_consistent () =
  let x = Array.make Poseidon128.Constant.width (Scalar.of_string "17") in
  let y = Array.make Poseidon128.Constant.width (Scalar.of_string "17") in
  let z = Array.make Poseidon128.Constant.width (Scalar.of_string "19") in

  let state_x = Poseidon.Strategy.init x in
  let state_y = Poseidon.Strategy.init y in
  let state_z = Poseidon.Strategy.init z in

  Poseidon.Strategy.apply_perm state_x ;
  Poseidon.Strategy.apply_perm state_y ;
  Poseidon.Strategy.apply_perm state_z ;

  let res_x = Poseidon.Strategy.get state_x in
  let res_y = Poseidon.Strategy.get state_y in
  let res_z = Poseidon.Strategy.get state_z in
  assert (Array.for_all2 Scalar.eq res_x res_y) ;
  assert (not @@ Array.for_all2 Scalar.eq res_x res_z)

let test_vectors_poseidon128 () =
  let open Poseidon in
  let test_inputs =
    [|
      "bb67ed265bf1db490ded2e1ede55c0d14c55521509dc73f9c354e98ab76c9625";
      "7e74220084d75e10c89e9435d47bb5b8075991b2e29be3b84421dac3b1ee6007";
      "5ce5481a4d78cca03498f72761da1b9f1d2aa8fb300be39f0e4fe2534f9d4308";
      "b1e710e3c4a8c35154b0ce4e4f4af6f498ebd79f8e7cdf3150372c7501be250b";
      "33c9e2025f86b5d82149f1ab8e20a168fc3d99d09b48cbce0286db8752cc3306";
      "e98206bfdce791e4e5144079b997d4fc25006194b35655f0e48490b26e24ea35";
      "86d2a95cc552de8d5bb20bd4a407fee5ffdc314e93dfe6b2dc792bc71fd8cc2d";
      "4edd8307ce28a8c70963d20a7bc28df1e1720bbbc93878a18bd07fad7d51fa15";
      "eabc7a296704a68aa01f95adc85f6dd758b175745336d8fc795a17984024b21e";
      "cfc108673c93df305e31c283b9c767b7097ae4e174a223e0c24b15a67b701a3a";
    |]
  in
  let test_inputs =
    Array.map (fun s -> Scalar.of_bytes_exn (Hex.to_bytes (`Hex s))) test_inputs
  in
  let inner points expected_res =
    let ctxt = Hash.init () in
    let ctxt = Hash.digest ctxt points in
    let v = Hash.get ctxt in
    let exp_res = Scalar.of_bytes_exn (Hex.to_bytes (`Hex expected_res)) in
    if not (Scalar.eq v exp_res) then
      Alcotest.failf
        "Expected result %s, but computed %s"
        Hex.(show (of_bytes (Scalar.to_bytes exp_res)))
        Hex.(show (of_bytes (Scalar.to_bytes v)))
  in
  inner [||] "3cad8b2e06121cd27c6577e87d0386be4807f5a9c9515e0dd80cb6961317945a" ;
  inner
    (Array.sub test_inputs 0 3)
    "8ac95ac433d0de78ea7aaf5075091ab4490cc7ac183bf97efc190eec9a015a3f" ;
  inner
    (Array.sub test_inputs 0 4)
    "d59bd4fe7fb7941ffbfe34ef8aaa94f4d90bb2d0c7d1b0aa50674ec8515c7f19" ;

  inner
    (Array.sub test_inputs 0 5)
    "ac16f0eec1c9ddacdce21241e6fc72d1430b6f47eaf6a5caf7f4052890287155" ;

  inner
    (Array.sub test_inputs 0 6)
    "5ad4b11f058b622cf4d9bdbb256745a6878e98615b65edc60d3f183df3f00a61" ;

  inner
    (Array.sub test_inputs 0 8)
    "e633a1d2f4366743e0a641b0a8d84d4d1ac9107bf6849f443d28aa0432da481d" ;

  inner
    (Array.sub test_inputs 0 10)
    "fc214ce126e686a08a607ee8a755a53a9c2afae186c2d282e488ad77003cac70"

let () =
  Alcotest.run
    ~__FILE__
    "Poseidon128"
    [
      ( "Properties",
        [Alcotest.test_case "Perm is consistent" `Quick test_perm_is_consistent]
      );
      ( "Test vectors for Poseidon128",
        [
          Alcotest.test_case
            "Regression test vectors for poseidon128"
            `Quick
            test_vectors_poseidon128;
        ] );
    ]
