module BN254AffineValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.BN254.Affine)
module BN254AffineEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.BN254.Affine)
module BN254AffineECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.BN254.Affine)
module BN254AffineRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine (Mec.Curve.BN254.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "BN254 affine form"
    [
      BN254AffineValueGeneration.get_tests ();
      BN254AffineEquality.get_tests ();
      BN254AffineECProperties.get_tests ();
      BN254AffineRepresentation.get_tests ();
    ]
