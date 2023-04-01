module PallasValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Pallas.Projective)
module PallasEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Pallas.Projective)
module PallasECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Pallas.Projective)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Pallas projective coordinates"
    [
      PallasValueGeneration.get_tests ();
      PallasEquality.get_tests ();
      PallasECProperties.get_tests ();
    ]
