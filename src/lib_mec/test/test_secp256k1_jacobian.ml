module Secp256r1ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Secp256r1.Jacobian)
module Secp256r1Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Secp256r1.Jacobian)
module Secp256r1ECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Secp256r1.Jacobian)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "secp256r1 jacobian coordinates"
    [
      Secp256r1ValueGeneration.get_tests ();
      Secp256r1Equality.get_tests ();
      Secp256r1ECProperties.get_tests ();
    ]
