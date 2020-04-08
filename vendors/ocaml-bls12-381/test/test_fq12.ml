module ValueGeneration = Test_ff_make.MakeValueGeneration (Bls12_381.Fq12)
module IsZero = Test_ff_make.MakeIsZero (Bls12_381.Fq12)
module Equality = Test_ff_make.MakeEquality (Bls12_381.Fq12)
module ECProperties = Test_ff_make.MakeFieldProperties (Bls12_381.Fq12)

let () =
  let open Alcotest in
  run
    "Fq12"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      ECProperties.get_tests () ]
