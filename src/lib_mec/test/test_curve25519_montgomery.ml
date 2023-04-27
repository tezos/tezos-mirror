module ValueGeneration_Mt =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Curve25519.AffineMontgomery)
module Equality_Mt =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Curve25519.AffineMontgomery)
module Properties_Mt =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Curve25519.AffineMontgomery)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Curve25519"
    [
      ValueGeneration_Mt.get_tests ();
      Properties_Mt.get_tests ();
      Equality_Mt.get_tests ();
    ]
