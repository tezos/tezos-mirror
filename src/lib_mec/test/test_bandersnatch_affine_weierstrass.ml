module BandersnatchAffineValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration
    (Mec.Curve.Bandersnatch.AffineWeierstrass)
module BandersnatchAffineEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Bandersnatch.AffineWeierstrass)
module BandersnatchAffineECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Bandersnatch.AffineWeierstrass)
module BandersnatchAffineRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine
    (Mec.Curve.Bandersnatch.AffineWeierstrass)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Bandersnatch Weierstrass form, affine coordinates"
    [
      BandersnatchAffineValueGeneration.get_tests ();
      BandersnatchAffineEquality.get_tests ();
      BandersnatchAffineECProperties.get_tests ();
      BandersnatchAffineRepresentation.get_tests ();
    ]
