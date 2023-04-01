module PallasValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Pallas.Jacobian)
module PallasEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Pallas.Jacobian)
module PallasECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Pallas.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Pallas jacobian coordinates"
    [
      PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests ();
    ]
