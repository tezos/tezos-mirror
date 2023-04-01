open Mec.Curve
module IsoPallasValueGeneration =
  Utils.PBT.MakeValueGeneration (Pallas.Iso.Affine)
module IsoPallasEquality = Utils.PBT.MakeEquality (Pallas.Iso.Affine)
module IsoPallasECProperties = Utils.PBT.MakeECProperties (Pallas.Iso.Affine)

let test_isogeny () =
  let r = Pallas.Iso.Affine.random () in
  ignore @@ Pallas.iso_map r

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Iso Pallas affine form"
    [
      ("Test isogeny", [test_case "With random point" `Quick test_isogeny]);
      IsoPallasValueGeneration.get_tests ();
      IsoPallasEquality.get_tests ();
      IsoPallasECProperties.get_tests ();
    ]
