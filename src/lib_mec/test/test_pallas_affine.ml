module PallasValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Pallas.Affine)
module PallasEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Pallas.Affine)
module PallasECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Pallas.Affine)
module PallasRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine (Mec.Curve.Pallas.Affine)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Pallas affine coordinates"
    [
      PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests ();
      PallasRepresentation.get_tests ();
    ]
