module JubjubAffineValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Jubjub.AffineWeierstrass)
module JubjubAffineEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Jubjub.AffineWeierstrass)
module JubjubAffineECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Jubjub.AffineWeierstrass)
module JubjubAffineRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine
    (Mec.Curve.Jubjub.AffineWeierstrass)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Jubjub - Weierstrass - Affine coordinates"
    [
      JubjubAffineValueGeneration.get_tests ();
      JubjubAffineEquality.get_tests ();
      JubjubAffineECProperties.get_tests ();
      JubjubAffineRepresentation.get_tests ();
    ]
