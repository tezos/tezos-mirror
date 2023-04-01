module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Bandersnatch.AffineEdwards)
module Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Bandersnatch.AffineEdwards)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Bandersnatch.AffineEdwards)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties
    (Mec.Curve.Bandersnatch.AffineEdwards)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Bandersnatch Edwards form, affine coordinates"
    [
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests ();
    ]
