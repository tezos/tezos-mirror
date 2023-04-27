module TweedledumValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Jacobian)
module TweedledumEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledum.Jacobian)
module TweedledumECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledum.Jacobian)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Tweedledum jacobian coordinates"
    [
      TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests ();
    ]
