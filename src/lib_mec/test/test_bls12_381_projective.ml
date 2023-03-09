open Mec.Curve
module G1ValueGeneration =
  Utils.PBT.MakeValueGeneration (BLS12_381.G1.Projective)
module G1Equality = Utils.PBT.MakeEquality (BLS12_381.G1.Projective)
module G1ECProperties = Utils.PBT.MakeECProperties (BLS12_381.G1.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BLS12-381 G1 projective form"
    [
      G1ValueGeneration.get_tests ();
      G1Equality.get_tests ();
      G1ECProperties.get_tests ();
    ]
