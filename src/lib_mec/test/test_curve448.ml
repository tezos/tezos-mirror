module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Curve448.Affine)
module Equality = Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Curve448.Affine)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Curve448.Affine)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties (Mec.Curve.Curve448.Affine)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Curve448"
    [
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests ();
    ]
