module TweedledumValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Tweedledum.Affine)
module TweedledumEquality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Tweedledum.Affine)
module TweedledumECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Tweedledum.Affine)
module TweedledumRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine
    (Mec.Curve.Tweedledum.Affine)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Tweedledum affine coordinates"
    [
      TweedledumValueGeneration.get_tests ();
      TweedledumEquality.get_tests ();
      TweedledumECProperties.get_tests ();
      TweedledumRepresentation.get_tests ();
    ]
