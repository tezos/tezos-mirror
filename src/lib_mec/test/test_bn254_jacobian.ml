module BN254JacobianValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.BN254.Jacobian)
module BN254JacobianEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.BN254.Jacobian)
module BN254JacobianECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.BN254.Jacobian)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "BN254 jacobian coordinates"
    [
      BN254JacobianValueGeneration.get_tests ();
      BN254JacobianEquality.get_tests ();
      BN254JacobianECProperties.get_tests ();
    ]
