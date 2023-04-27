module TweedledumValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Projective)
module TweedledumEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledum.Projective)
module TweedledumECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledum.Projective)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Tweedledum projective coordinates"
    [
      TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests ();
    ]
