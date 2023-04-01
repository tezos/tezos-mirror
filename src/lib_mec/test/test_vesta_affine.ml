module VestaValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Vesta.Affine)
module VestaEquality = Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Vesta.Affine)
module VestaECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Vesta.Affine)
module VestaRepresentation =
  Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine (Mec.Curve.Vesta.Affine)

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Vesta affine coordinates"
    [
      VestaValueGeneration.get_tests ();
      VestaEquality.get_tests ();
      VestaECProperties.get_tests ();
      VestaRepresentation.get_tests ();
    ]
