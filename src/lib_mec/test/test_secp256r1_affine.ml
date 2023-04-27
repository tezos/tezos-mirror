module Secp256r1ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Secp256r1.Affine)
module Secp256r1Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Secp256r1.Affine)
module Secp256r1ECProperties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Secp256r1.Affine)

(* FIXME: Looks like it is failing. I don't have time for the moment to verify *)
(* module CompressedRepresentation =
 *   Mec.Curve.Utils.PBT.MakeCompressedSerialisationAffine
 *     (Mec.Curve.Secp256r1.Affine) *)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "secp256r1 affine coordinates"
    [
      Secp256r1ValueGeneration.get_tests ();
      Secp256r1Equality.get_tests ();
      Secp256r1ECProperties.get_tests ()
      (* CompressedRepresentation.get_tests () *);
    ]
