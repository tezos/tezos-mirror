module TweedledeeValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledee.Jacobian)
module TweedledeeEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledee.Jacobian)
module TweedledeeECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledee.Jacobian)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Tweedledee jacobian coordinates"
    [
      TweedledeeValueGeneration.get_tests ();
      TweedledeeEquality.get_tests ();
      TweedledeeECProperties.get_tests ();
    ]
