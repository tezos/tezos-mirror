module ValueGeneration =
  Test_ec_make.MakeValueGeneration (Bls12_381.G2.Uncompressed)
module IsZero = Test_ec_make.MakeIsZero (Bls12_381.G2.Uncompressed)
module Equality = Test_ec_make.MakeEquality (Bls12_381.G2.Uncompressed)
module ECProperties = Test_ec_make.MakeECProperties (Bls12_381.G2.Uncompressed)
module ValueGenerationCompressed =
  Test_ec_make.MakeValueGeneration (Bls12_381.G2.Compressed)
module IsZeroCompressed = Test_ec_make.MakeIsZero (Bls12_381.G2.Compressed)
module EqualityCompressed = Test_ec_make.MakeEquality (Bls12_381.G2.Compressed)
module ECPropertiesCompressed =
  Test_ec_make.MakeECProperties (Bls12_381.G2.Compressed)

module Constructors = struct
  let test_of_z_one () =
    (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/fq.rs#L18 *)
    let x =
      ( Z.of_string
          "352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160",
        Z.of_string
          "3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758"
      )
    in
    let y =
      ( Z.of_string
          "1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905",
        Z.of_string
          "927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582"
      )
    in
    let g = Bls12_381.G2.Uncompressed.of_z_opt ~x ~y in
    match g with
    | Some g ->
        assert (
          Bls12_381.G2.Uncompressed.eq (Bls12_381.G2.Uncompressed.one ()) g )
    | None ->
        assert false

  let test_vectors_random_points_not_on_curve () =
    let x = (Z.of_string "90809235435", Z.of_string "09809345809345809") in
    let y =
      (Z.of_string "8090843059809345", Z.of_string "908098039459089345")
    in
    match Bls12_381.G2.Uncompressed.of_z_opt ~x ~y with
    | Some _ ->
        assert false
    | None ->
        assert true

  let get_tests () =
    let open Alcotest in
    ( "From Z elements",
      [ test_case "one (generator)" `Quick test_of_z_one;
        test_case
          "random points not on curve"
          `Quick
          test_vectors_random_points_not_on_curve ] )
end

let () =
  let open Alcotest in
  run
    "G2 uncompressed"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      Constructors.get_tests ();
      ECProperties.get_tests () ] ;
  run
    "G2 compressed"
    [ IsZeroCompressed.get_tests ();
      ValueGenerationCompressed.get_tests ();
      EqualityCompressed.get_tests ();
      ECPropertiesCompressed.get_tests () ]
