module BN254ProjectiveValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.BN254.Projective)
module BN254ProjectiveEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.BN254.Projective)
module BN254ProjectiveECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.BN254.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BN254 projective form"
    [
      BN254ProjectiveValueGeneration.get_tests ();
      BN254ProjectiveEquality.get_tests ();
      BN254ProjectiveECProperties.get_tests ();
    ]
