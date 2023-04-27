module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration
    (Mec.Curve.Bandersnatch.AffineMontgomery)
module Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Bandersnatch.AffineMontgomery)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Bandersnatch.AffineMontgomery)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Bandersnatch Montgomery form and affine coordinates"
    [
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      Equality.get_tests ();
    ]
