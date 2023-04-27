open Mec.Curve
module G1ValueGeneration = Utils.PBT.MakeValueGeneration (BLS12_381.G1.Affine)
module G1Equality = Utils.PBT.MakeEquality (BLS12_381.G1.Affine)
module G1ECProperties = Utils.PBT.MakeECProperties (BLS12_381.G1.Affine)
module G1Representation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine (BLS12_381.G1.Affine)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "BLS12-381 G1 affine form"
    [
      G1ValueGeneration.get_tests ();
      G1Equality.get_tests ();
      G1ECProperties.get_tests ();
      G1Representation.get_tests ();
    ]
