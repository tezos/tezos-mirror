module VestaValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Vesta.Projective)
module VestaEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Vesta.Projective)
module VestaECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Vesta.Projective)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Vesta projective coordinates"
    [
      VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests ();
    ]
