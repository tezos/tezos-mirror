open Mec.Curve
open Mec.Utils

(* let two_z = Z.succ Z.one *)

(* The test vectors have been generated from https://github.com/zcash/orchard,
   b4a82211cee82ceb02d2e0e99b7566a967804a6c

   The following rust code has been added in primitives/sinsemilla.rs:

    ```rust
    #[test]
    fn test_vectors() {
        use crate::primitives::sinsemilla::HashDomain;
        use group::GroupEncoding;

        let mut rng = OsRng;
        let hasher = HashDomain::new("hasher");
        println!("Initial value: {:?}", hex::encode(hasher.Q.to_bytes()));
        // Change the size here
        let size = 100;
        let bits: Vec<bool> = (0..1086).map(|_| rng.gen()).collect();
        println!("Inputs: {:?}", &bits[..size]);
        println!(
            "Result: {:?}",
            hasher.hash(bits[..size].iter().cloned()).unwrap()
        );
    }
    ```

    and the result can be printed on stdout using:
    ```shell
    cargo test "primitives::sinsemilla::tests::test_vectors" -- --nocapture
    ```
*)
let iv =
  Pallas.Affine.of_compressed_bytes_exn
    (Hex.to_bytes
       (`Hex "857d3f841a79306bc85c63b4da7547fb302557d09e39bc9b9856b67d5b618910"))

let test_inputs =
  [
    [true; false; false; false; false; false; false; false; false; true];
    [true];
    [true; false; false; false; false; false; false; false; false];
    [true; false; false; false; false];
    [true; false; false];
    [
      false;
      false;
      false;
      true;
      true;
      false;
      true;
      false;
      false;
      false;
      true;
      false;
      true;
      true;
      false;
      true;
      false;
      false;
      false;
      false;
      false;
      false;
      true;
      false;
      true;
      false;
      true;
      true;
      false;
      true;
      true;
      true;
      true;
      true;
      true;
      false;
      true;
      true;
      true;
      false;
      false;
      true;
      false;
      true;
      true;
      true;
      false;
      true;
      true;
      false;
      true;
      false;
      false;
      true;
      false;
      false;
      true;
      true;
      true;
      false;
      true;
      false;
      true;
      false;
      true;
      false;
      true;
      false;
      false;
      false;
      false;
      true;
      true;
      false;
      true;
      true;
      true;
      true;
      true;
      false;
      false;
      true;
      true;
      false;
      false;
      false;
      true;
      false;
      false;
      true;
      true;
      false;
      false;
      false;
      false;
      false;
      false;
      true;
      false;
      false;
    ];
  ]

let test_outputs =
  [
    "080056d41fd433913bb29359803e9eab30fc328fd0bccaa6792d4fad68c516cd";
    "31bad63b65ecce065c51860c168b8c09916ab092c9ba3c5467955cd90e20e9c2";
    "31bad63b65ecce065c51860c168b8c09916ab092c9ba3c5467955cd90e20e9c2";
    "31bad63b65ecce065c51860c168b8c09916ab092c9ba3c5467955cd90e20e9c2";
    "31bad63b65ecce065c51860c168b8c09916ab092c9ba3c5467955cd90e20e9c2";
    "34fd57c58f73de765ad9627ced0405fb0868ca6e7e9ced5c311cc120379abc6b";
  ]

let inverse_endianness bs =
  let length = Bytes.length bs in
  Bytes.init length (fun i -> Bytes.get bs (length - i - 1))

module Sinsemilla = Mec.Hash.Sinsemilla.Zcash

let test_vectors_sinsemilla_orchard () =
  let open Sinsemilla in
  List.iter2
    (fun input expected_output ->
      let it = Iterator.Bit.of_bool_list input in
      let expected_output = Hex.to_bytes (`Hex expected_output) in
      let expected_output = inverse_endianness expected_output in
      let output = hash_exn iv it in
      let output = Pallas.Affine.Base.to_bytes output in
      if not (output = expected_output) then
        Alcotest.failf
          "On input [%s], computed output is %s but expected %s"
          (String.concat ", " (List.map string_of_bool input))
          Hex.(show (of_bytes expected_output))
          Hex.(show (of_bytes output)))
    test_inputs
    test_outputs

module Properties = struct
  let test_collision_when_padding_with_zeroes_last_chunk () =
    let chunk_size = 1 + Random.int 10 in
    let generators =
      Array.init (1 lsl chunk_size) (fun _ -> Pallas.Affine.random ())
    in
    let iv = Pallas.Affine.random () in
    let module Sinsemilla = Mec.Hash.Sinsemilla.MakeSinsemilla (struct
      let chunk_size = chunk_size

      let generators = generators

      let iv = iv
    end) in
    let nb_chunk_input = 1 + Random.int 10 in
    let input =
      List.init ((nb_chunk_input * chunk_size) + 1) (fun _ -> Random.bool ())
    in
    let exp_output = Sinsemilla.hash_exn (Iterator.Bit.of_bool_list input) in
    for i = 0 to chunk_size - 1 do
      let input_padded = input @ List.init i (fun _ -> false) in
      let output =
        Sinsemilla.hash_exn (Iterator.Bit.of_bool_list input_padded)
      in
      assert (Pallas.Affine.Base.eq output exp_output)
    done

  let get_tests () =
    ( "Properties",
      [
        Alcotest.test_case
          "Collision when padding last chunk with zeroes"
          `Quick
          (Mec.Curve.Utils.PBT.repeat
             100
             test_collision_when_padding_with_zeroes_last_chunk);
      ] )
end

let () =
  Alcotest.run
    ~__FILE__
    "Sinsemilla"
    [
      ( "Test vectors for Sinsemilla",
        [
          Alcotest.test_case
            "Test vectors from zcash-ochard/zcash-test-vectors"
            `Quick
            test_vectors_sinsemilla_orchard;
        ] );
      Properties.get_tests ();
    ]
