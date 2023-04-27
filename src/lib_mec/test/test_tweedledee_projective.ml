module TweedledeeValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledee.Projective)
module TweedledeeEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledee.Projective)
module TweedledeeECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledee.Projective)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Tweedledee projective coordinates"
    [
      TweedledeeValueGeneration.get_tests ();
      TweedledeeEquality.get_tests ();
      TweedledeeECProperties.get_tests ();
    ]
