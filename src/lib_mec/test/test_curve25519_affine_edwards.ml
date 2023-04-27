module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Curve25519.AffineEdwards)
module Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Curve25519.AffineEdwards)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Curve25519.AffineEdwards)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties
    (Mec.Curve.Curve25519.AffineEdwards)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Curve25519 edwards form and affine coordinates"
    [
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests ();
    ]
