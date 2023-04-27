module VestaValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Vesta.Jacobian)
module VestaEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Vesta.Jacobian)
module VestaECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Vesta.Jacobian)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Vesta jacobian coordinates"
    [
      VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests ();
    ]
