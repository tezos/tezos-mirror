module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.BabyJubjubReduced.Affine)
module Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.BabyJubjubReduced.Affine)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.BabyJubjubReduced.Affine)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties
    (Mec.Curve.BabyJubjubReduced.Affine)
module Serialisation =
  Mec.Curve.Utils.PBT.MakeSerialisationProperties
    (Mec.Curve.BabyJubjubReduced.Affine)

let test_random_points_not_on_curve () =
  (* pick random values u and v and test constructors fail *)
  let u = Mec.Curve.BabyJubjubReduced.Affine.Base.random () in
  let v = Mec.Curve.BabyJubjubReduced.Affine.Base.random () in
  let bytes =
    Bytes.concat
      Bytes.empty
      [
        Mec.Curve.BabyJubjubReduced.Affine.Base.to_bytes u;
        Mec.Curve.BabyJubjubReduced.Affine.Base.to_bytes v;
      ]
  in
  (* check_bytes *)
  assert (not (Mec.Curve.BabyJubjubReduced.Affine.check_bytes bytes)) ;
  (* of_bytes_opt *)
  assert (Option.is_none (Mec.Curve.BabyJubjubReduced.Affine.of_bytes_opt bytes)) ;
  (* of_bytes_exn *)
  (try
     ignore (Mec.Curve.BabyJubjubReduced.Affine.of_bytes_exn bytes) ;
     assert false
   with
  | Mec.Curve.BabyJubjubReduced.Affine.Not_on_curve _ -> ()
  | _ -> assert false) ;
  (* from_coordinates_opt *)
  assert (
    Option.is_none
      (Mec.Curve.BabyJubjubReduced.Affine.from_coordinates_opt ~u ~v)) ;
  (* from_coordinates_exn *)
  try
    ignore (Mec.Curve.BabyJubjubReduced.Affine.from_coordinates_exn ~u ~v) ;
    assert false
  with
  | Mec.Curve.BabyJubjubReduced.Affine.Not_on_curve _ -> ()
  | _ -> assert false

let () =
  let open Alcotest in
  run
    ~__FILE__
    "BabyJubjub reduced twisted edwards form"
    [
      ( "Vectors",
        [
          Alcotest.test_case
            "test random coordinates u, v do not give a point on the curve"
            `Quick
            (Mec.Curve.Utils.PBT.repeat 100 test_random_points_not_on_curve);
        ] );
      ValueGeneration.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Properties.get_tests ();
      Serialisation.get_tests ();
      Equality.get_tests ();
    ]
