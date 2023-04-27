module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.BabyJubjub.Affine)
module Equality = Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.BabyJubjub.Affine)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.BabyJubjub.Affine)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties (Mec.Curve.BabyJubjub.Affine)
module Serialisation =
  Mec.Curve.Utils.PBT.MakeSerialisationProperties (Mec.Curve.BabyJubjub.Affine)

let test_doubling () =
  let vectors =
    [
      ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
          "2626589144620713026669568689430873010625803728049924121243784502389097019475"
        ),
        ( "6890855772600357754907169075114257697580319025794532037257385534741338397365",
          "4338620300185947561074059802482547481416142213883829469920100239455078257889"
        ) );
    ]
  in
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
      let x1 = Mec.Curve.BabyJubjub.Affine.Base.of_string x1 in
      let y1 = Mec.Curve.BabyJubjub.Affine.Base.of_string y1 in
      let x2 = Mec.Curve.BabyJubjub.Affine.Base.of_string x2 in
      let y2 = Mec.Curve.BabyJubjub.Affine.Base.of_string y2 in
      let p1 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x2 ~v:y2 in
      assert (Mec.Curve.BabyJubjub.Affine.(eq (double p1) p2)))
    vectors

let test_addition () =
  let vectors =
    [
      ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
          "2626589144620713026669568689430873010625803728049924121243784502389097019475"
        ),
        ( "16540640123574156134436876038791482806971768689494387082833631921987005038935",
          "20819045374670962167435360035096875258406992893633759881276124905556507972311"
        ),
        ( "7916061937171219682591368294088513039687205273691143098332585753343424131937",
          "14035240266687799601661095864649209771790948434046947201833777492504781204499"
        ) );
    ]
  in
  List.iter
    (fun ((x1, y1), (x2, y2), (x3, y3)) ->
      let x1 = Mec.Curve.BabyJubjub.Affine.Base.of_string x1 in
      let y1 = Mec.Curve.BabyJubjub.Affine.Base.of_string y1 in
      let x2 = Mec.Curve.BabyJubjub.Affine.Base.of_string x2 in
      let y2 = Mec.Curve.BabyJubjub.Affine.Base.of_string y2 in
      let x3 = Mec.Curve.BabyJubjub.Affine.Base.of_string x3 in
      let y3 = Mec.Curve.BabyJubjub.Affine.Base.of_string y3 in
      let p1 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x2 ~v:y2 in
      let p3 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x3 ~v:y3 in
      assert (Mec.Curve.BabyJubjub.Affine.(eq (add p1 p2) p3)) ;
      assert (Mec.Curve.BabyJubjub.Affine.(eq (add p2 p1) p3)) ;
      assert (Mec.Curve.BabyJubjub.Affine.(not (eq (add p1 p2) p1))))
    vectors

let test_mul_scalar () =
  let vectors =
    [
      ( ( "17777552123799933955779906779655732241715742912184938656739573121738514868268",
          "2626589144620713026669568689430873010625803728049924121243784502389097019475"
        ),
        "14035240266687799601661095864649209771790948434046947201833777492504781204499",
        ( "17070357974431721403481313912716834497662307308519659060910483826664480189605",
          "4014745322800118607127020275658861516666525056516280575712425373174125159339"
        ) );
    ]
  in
  List.iter
    (fun ((x1, y1), n, (x2, y2)) ->
      let x1 = Mec.Curve.BabyJubjub.Affine.Base.of_string x1 in
      let y1 = Mec.Curve.BabyJubjub.Affine.Base.of_string y1 in
      let x2 = Mec.Curve.BabyJubjub.Affine.Base.of_string x2 in
      let y2 = Mec.Curve.BabyJubjub.Affine.Base.of_string y2 in
      let n = Mec.Curve.BabyJubjub.Affine.Scalar.of_string n in
      let p1 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x1 ~v:y1 in
      let p2 = Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u:x2 ~v:y2 in
      assert (Mec.Curve.BabyJubjub.Affine.(eq (mul p1 n) p2)))
    vectors

let test_random_points_not_on_curve () =
  (* pick random values u and v and test constructors fail *)
  let u = Mec.Curve.BabyJubjub.Affine.Base.random () in
  let v = Mec.Curve.BabyJubjub.Affine.Base.random () in
  let bytes =
    Bytes.concat
      Bytes.empty
      [
        Mec.Curve.BabyJubjub.Affine.Base.to_bytes u;
        Mec.Curve.BabyJubjub.Affine.Base.to_bytes v;
      ]
  in
  (* check_bytes *)
  assert (not (Mec.Curve.BabyJubjub.Affine.check_bytes bytes)) ;
  (* of_bytes_opt *)
  assert (Option.is_none (Mec.Curve.BabyJubjub.Affine.of_bytes_opt bytes)) ;
  (* of_bytes_exn *)
  (try
     ignore (Mec.Curve.BabyJubjub.Affine.of_bytes_exn bytes) ;
     assert false
   with
  | Mec.Curve.BabyJubjub.Affine.Not_on_curve _ -> ()
  | _ -> assert false) ;
  (* from_coordinates_opt *)
  assert (
    Option.is_none (Mec.Curve.BabyJubjub.Affine.from_coordinates_opt ~u ~v)) ;
  (* from_coordinates_exn *)
  try
    ignore (Mec.Curve.BabyJubjub.Affine.from_coordinates_exn ~u ~v) ;
    assert false
  with
  | Mec.Curve.BabyJubjub.Affine.Not_on_curve _ -> ()
  | _ -> assert false

let () =
  let open Alcotest in
  run
    ~__FILE__
    "BabyJubjub"
    [
      ( "Vectors",
        [
          Alcotest.test_case "test vectors addition" `Quick test_addition;
          Alcotest.test_case "test scalar multiplication" `Quick test_mul_scalar;
          Alcotest.test_case
            "test random coordinates u, v do not give a point on the curve"
            `Quick
            (Mec.Curve.Utils.PBT.repeat 100 test_random_points_not_on_curve);
          Alcotest.test_case "test vectors doubling" `Quick test_doubling;
        ] );
      ValueGeneration.get_tests ();
      Properties.get_tests ();
      Serialisation.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Equality.get_tests ();
    ]
