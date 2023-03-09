module ValueGeneration =
  Mec.Curve.Utils.PBT.MakeValueGeneration (Mec.Curve.Jubjub.AffineEdwards)
module Equality =
  Mec.Curve.Utils.PBT.MakeEquality (Mec.Curve.Jubjub.AffineEdwards)
module Properties =
  Mec.Curve.Utils.PBT.MakeECProperties (Mec.Curve.Jubjub.AffineEdwards)
module EdwardsCurveProperties =
  Mec.Curve.Utils.PBT.MakeEdwardsCurveProperties (Mec.Curve.Jubjub.AffineEdwards)

let test_vectors () =
  (* Coming from
     https://github.com/zcash/librustzcash/blob/1b4aab0b76d465e4fe548f230b7c3ebdc398a1a5/zcash_primitives/src/constants.rs *)
  let points =
    [
      ( "0x73c016a42ded9578b5ea25de7ec0e3782f0c718f6f0fbadd194e42926f661b51",
        "0x289e87a2d3521b5779c9166b837edc5ef9472e8bc04e463277bfabd432243cca" );
      ( "0x15a36d1f0f390d8852a35a8c1908dd87a361ee3fd48fdf77b9819dc82d90607e",
        "0x015d8c7f5b43fe33f7891142c001d9251f3abeeb98fad3e87b0dc53c4ebf1891" );
      ( "0x664321a58246e2f6eb69ae39f5c84210bae8e5c46641ae5c76d6f7c2b67fc475",
        "0x362e1500d24eee9ee000a46c8e8ce8538bb22a7f1784b49880ed502c9793d457" );
      ( "0x323a6548ce9d9876edc5f4a9cff29fd57d02d50e654b87f24c767804c1c4a2cc",
        "0x2f7ee40c4b56cad891070acbd8d947b75103afa1a11f6a8584714beca33570e9" );
      ( "0x3bd2666000b5479689b64b4e03362796efd5931305f2f0bf46809430657f82d1",
        "0x494bc52103ab9d0a397832381406c9e5b3b9d8095859d14c99968299c3658aef" );
      ( "0x63447b2ba31bb28ada049746d76d3ee51d9e5ca21135ff6fcb3c023258d32079",
        "0x64ec4689e8bfb6e564cdb1070a136a28a80200d2c66b13a7436082119f8d629a" );
    ]
  in
  List.iter
    (fun (u, v) ->
      (* from_coordinates_exn *)
      ignore
      @@ Mec.Curve.Jubjub.AffineEdwards.from_coordinates_exn
           ~u:(Mec.Curve.Jubjub.AffineEdwards.Base.of_string u)
           ~v:(Mec.Curve.Jubjub.AffineEdwards.Base.of_string v) ;
      (* from_coordinates_opt *)
      assert (
        Option.is_some
          (Mec.Curve.Jubjub.AffineEdwards.from_coordinates_opt
             ~u:(Mec.Curve.Jubjub.AffineEdwards.Base.of_string u)
             ~v:(Mec.Curve.Jubjub.AffineEdwards.Base.of_string v))) ;
      (* convert to bytes. Use Zarith for simplicity as points are given in hexa *)
      let bytes =
        Bytes.concat
          Bytes.empty
          [
            (Bytes.of_string @@ Z.(to_bits (of_string u)));
            (Bytes.of_string @@ Z.(to_bits (of_string v)));
          ]
      in
      (* of_bytes_opt *)
      assert (Option.is_some (Mec.Curve.Jubjub.AffineEdwards.of_bytes_opt bytes)) ;
      (* of_bytes_exn *)
      ignore @@ Mec.Curve.Jubjub.AffineEdwards.of_bytes_exn bytes ;
      (* check_bytes *)
      assert (Mec.Curve.Jubjub.AffineEdwards.check_bytes bytes))
    points

let test_random_points_not_on_curve () =
  (* pick random values u and v and test constructors fail *)
  let u = Mec.Curve.Jubjub.AffineEdwards.Base.random () in
  let v = Mec.Curve.Jubjub.AffineEdwards.Base.random () in
  let bytes =
    Bytes.concat
      Bytes.empty
      [
        Mec.Curve.Jubjub.AffineEdwards.Base.to_bytes u;
        Mec.Curve.Jubjub.AffineEdwards.Base.to_bytes v;
      ]
  in
  (* check_bytes *)
  assert (not (Mec.Curve.Jubjub.AffineEdwards.check_bytes bytes)) ;
  (* of_bytes_opt *)
  assert (Option.is_none (Mec.Curve.Jubjub.AffineEdwards.of_bytes_opt bytes)) ;
  (* of_bytes_exn *)
  (try
     ignore (Mec.Curve.Jubjub.AffineEdwards.of_bytes_exn bytes) ;
     assert false
   with
  | Mec.Curve.Jubjub.AffineEdwards.Not_on_curve _ -> ()
  | _ -> assert false) ;
  (* from_coordinates_opt *)
  assert (
    Option.is_none (Mec.Curve.Jubjub.AffineEdwards.from_coordinates_opt ~u ~v)) ;
  (* from_coordinates_exn *)
  try
    ignore (Mec.Curve.Jubjub.AffineEdwards.from_coordinates_exn ~u ~v) ;
    assert false
  with
  | Mec.Curve.Jubjub.AffineEdwards.Not_on_curve _ -> ()
  | _ -> assert false

let test_compressed_uncompressed_zero () =
  let expected_encoding_of_zero =
    Bytes.sub Mec.Curve.Jubjub.AffineEdwards.(to_bytes zero) 32 32
  in
  assert (
    Mec.Curve.Jubjub.AffineEdwards.(
      Bytes.equal (to_compressed zero) expected_encoding_of_zero)) ;
  assert (
    Mec.Curve.Jubjub.AffineEdwards.(
      eq zero (of_compressed_exn expected_encoding_of_zero))) ;
  assert (
    Mec.Curve.Jubjub.AffineEdwards.(
      eq zero (Option.get @@ of_compressed_opt expected_encoding_of_zero)))

let test_compressed_and_uncompressed_exn () =
  let p = Mec.Curve.Jubjub.AffineEdwards.random () in
  let compressed_p = Mec.Curve.Jubjub.AffineEdwards.to_compressed p in
  let uncompressed_p =
    Mec.Curve.Jubjub.AffineEdwards.of_compressed_exn compressed_p
  in
  assert (Mec.Curve.Jubjub.AffineEdwards.eq p uncompressed_p)

let test_compressed_gives_32_bytes () =
  let compressed_p =
    Mec.Curve.Jubjub.AffineEdwards.(to_compressed (random ()))
  in
  assert (Bytes.length compressed_p = 32)

let test_compressed_and_uncompressed_opt () =
  let p = Mec.Curve.Jubjub.AffineEdwards.random () in
  let compressed_p = Mec.Curve.Jubjub.AffineEdwards.to_compressed p in
  let uncompressed_p =
    Option.get @@ Mec.Curve.Jubjub.AffineEdwards.of_compressed_opt compressed_p
  in
  assert (Mec.Curve.Jubjub.AffineEdwards.eq p uncompressed_p)

let rec test_uncompressed_fail_on_random_values () =
  let nb_bytes =
    Random.int (Mec.Curve.Jubjub.AffineEdwards.size_in_bytes * 10)
  in
  if nb_bytes = Mec.Curve.Jubjub.AffineEdwards.size_in_bytes then
    test_uncompressed_fail_on_random_values ()
  else
    let b = Bytes.create nb_bytes in
    assert (Option.is_none (Mec.Curve.Jubjub.AffineEdwards.of_compressed_opt b)) ;
    try ignore @@ Mec.Curve.Jubjub.AffineEdwards.of_compressed_exn b with
    | Mec.Curve.Jubjub.AffineEdwards.Not_on_curve exn_b ->
        assert (Bytes.equal b exn_b)
    | _ -> assert false

let test_vector_compressed_and_uncompressed () =
  let u_bytes, v_bytes =
    ( "0x62edcbb8bf3787c88b0f03ddd60a8187caf55d1b29bf81afe4b3d35df1a7adfe",
      "0x000000000000000000000000000000000000000000000000000000000000000b" )
  in
  let full_generator =
    Mec.Curve.Jubjub.AffineEdwards.from_coordinates_exn
      ~u:(Mec.Curve.Jubjub.AffineEdwards.Base.of_string u_bytes)
      ~v:(Mec.Curve.Jubjub.AffineEdwards.Base.of_string v_bytes)
  in
  let gen =
    Mec.Curve.Jubjub.AffineEdwards.mul
      full_generator
      (Mec.Curve.Jubjub.AffineEdwards.Scalar.of_z
         Mec.Curve.Jubjub.AffineEdwards.cofactor)
  in
  let vectors_as_int =
    [
      [
        203;
        85;
        12;
        213;
        56;
        234;
        12;
        193;
        19;
        132;
        128;
        64;
        142;
        110;
        170;
        185;
        179;
        108;
        97;
        63;
        13;
        211;
        247;
        120;
        79;
        219;
        110;
        234;
        131;
        123;
        19;
        215;
      ];
      [
        113;
        154;
        240;
        230;
        224;
        198;
        208;
        170;
        104;
        15;
        59;
        126;
        151;
        222;
        233;
        195;
        203;
        195;
        167;
        129;
        89;
        121;
        240;
        142;
        51;
        166;
        64;
        250;
        184;
        202;
        154;
        177;
      ];
      [
        197;
        41;
        93;
        209;
        203;
        55;
        164;
        174;
        88;
        0;
        90;
        199;
        1;
        156;
        149;
        141;
        240;
        29;
        14;
        82;
        86;
        225;
        126;
        129;
        186;
        157;
        148;
        162;
        219;
        51;
        156;
        199;
      ];
      [
        182;
        117;
        250;
        241;
        81;
        196;
        199;
        227;
        151;
        74;
        243;
        17;
        221;
        97;
        200;
        139;
        192;
        83;
        231;
        35;
        214;
        14;
        95;
        69;
        130;
        201;
        4;
        116;
        177;
        19;
        179;
        0;
      ];
      [
        118;
        41;
        29;
        200;
        60;
        189;
        119;
        252;
        78;
        40;
        230;
        18;
        208;
        221;
        38;
        214;
        176;
        250;
        4;
        10;
        77;
        101;
        26;
        216;
        193;
        198;
        226;
        84;
        25;
        177;
        230;
        185;
      ];
      [
        226;
        189;
        227;
        208;
        112;
        117;
        136;
        98;
        72;
        38;
        211;
        167;
        254;
        82;
        174;
        113;
        112;
        166;
        138;
        171;
        166;
        113;
        52;
        251;
        129;
        197;
        138;
        45;
        195;
        7;
        61;
        140;
      ];
      [
        38;
        198;
        156;
        196;
        146;
        225;
        55;
        163;
        138;
        178;
        157;
        128;
        115;
        135;
        204;
        215;
        0;
        33;
        171;
        20;
        60;
        32;
        142;
        209;
        33;
        233;
        125;
        146;
        207;
        12;
        16;
        24;
      ];
      [
        17;
        187;
        231;
        83;
        165;
        36;
        232;
        184;
        140;
        205;
        195;
        252;
        166;
        85;
        59;
        86;
        3;
        226;
        211;
        67;
        179;
        29;
        238;
        181;
        102;
        142;
        58;
        63;
        57;
        89;
        174;
        138;
      ];
      [
        210;
        159;
        80;
        16;
        181;
        39;
        221;
        204;
        224;
        144;
        145;
        79;
        54;
        231;
        8;
        140;
        142;
        216;
        93;
        190;
        183;
        116;
        174;
        63;
        33;
        242;
        177;
        118;
        148;
        40;
        241;
        203;
      ];
      [
        0;
        143;
        107;
        102;
        149;
        187;
        27;
        124;
        18;
        10;
        98;
        28;
        113;
        123;
        121;
        185;
        29;
        152;
        14;
        130;
        149;
        28;
        87;
        35;
        135;
        135;
        153;
        54;
        112;
        53;
        54;
        68;
      ];
      [
        178;
        131;
        85;
        160;
        214;
        51;
        208;
        157;
        196;
        152;
        247;
        93;
        202;
        56;
        81;
        239;
        155;
        122;
        59;
        188;
        237;
        253;
        11;
        169;
        208;
        236;
        12;
        4;
        163;
        211;
        88;
        97;
      ];
      [
        246;
        194;
        231;
        195;
        159;
        101;
        180;
        133;
        80;
        21;
        185;
        220;
        195;
        115;
        144;
        12;
        90;
        150;
        44;
        117;
        8;
        156;
        168;
        248;
        206;
        41;
        60;
        82;
        67;
        75;
        57;
        67;
      ];
      [
        212;
        205;
        171;
        153;
        113;
        16;
        194;
        241;
        224;
        43;
        177;
        110;
        190;
        248;
        22;
        201;
        208;
        166;
        2;
        83;
        134;
        130;
        85;
        129;
        166;
        136;
        185;
        191;
        163;
        38;
        54;
        10;
      ];
      [
        8;
        60;
        190;
        39;
        153;
        222;
        119;
        23;
        142;
        237;
        12;
        110;
        146;
        9;
        19;
        219;
        143;
        64;
        161;
        99;
        199;
        77;
        39;
        148;
        70;
        213;
        246;
        227;
        150;
        178;
        237;
        178;
      ];
      [
        11;
        114;
        217;
        160;
        101;
        37;
        100;
        220;
        56;
        114;
        42;
        31;
        138;
        33;
        84;
        157;
        214;
        167;
        73;
        233;
        115;
        81;
        124;
        134;
        15;
        31;
        181;
        60;
        184;
        130;
        175;
        159;
      ];
      [
        141;
        238;
        235;
        202;
        241;
        32;
        210;
        10;
        127;
        230;
        54;
        31;
        146;
        80;
        247;
        9;
        107;
        124;
        0;
        26;
        203;
        16;
        237;
        34;
        214;
        147;
        133;
        15;
        29;
        236;
        37;
        88;
      ];
    ]
  in
  let serialised_vectors =
    List.map (fun int_list -> List.map char_of_int int_list) vectors_as_int
  in
  let serialised_vectors =
    List.map
      (fun int_list -> Bytes.init 32 (List.nth int_list))
      serialised_vectors
  in
  ignore
  @@ List.fold_left
       (fun p expected_bytes ->
         (* Test of_compressed *)
         let expected_p =
           Mec.Curve.Jubjub.AffineEdwards.of_compressed_exn expected_bytes
         in
         assert (Mec.Curve.Jubjub.AffineEdwards.eq p expected_p) ;
         (* Test to_compressed *)
         let serialised_p = Mec.Curve.Jubjub.AffineEdwards.to_compressed p in
         assert (Bytes.equal serialised_p expected_bytes) ;
         Mec.Curve.Jubjub.AffineEdwards.add p gen)
       gen
       serialised_vectors

let () =
  let open Alcotest in
  run
    ~verbose:true
    "Jubjub"
    [
      ( "Vectors",
        [Alcotest.test_case "test vectors elements" `Quick test_vectors] );
      ( "Compressed/Uncompressed",
        [
          Alcotest.test_case
            "Correct point; uncompressed_exn"
            `Quick
            test_compressed_and_uncompressed_exn;
          Alcotest.test_case
            "Random values must not be accepted"
            `Quick
            test_uncompressed_fail_on_random_values;
          Alcotest.test_case
            "Compressed gives 32 bytes"
            `Quick
            test_compressed_gives_32_bytes;
          Alcotest.test_case
            "Correct point; uncompressed_opt"
            `Quick
            test_compressed_and_uncompressed_opt;
          Alcotest.test_case
            "Test vectors"
            `Quick
            test_vector_compressed_and_uncompressed;
          Alcotest.test_case
            "Encoding of zero"
            `Quick
            test_compressed_uncompressed_zero;
        ] );
      ( "Tests random",
        [
          Alcotest.test_case
            "test random coordinates do not give a point on the curve"
            `Quick
            (Mec.Curve.Utils.PBT.repeat 100 test_random_points_not_on_curve);
        ] );
      ValueGeneration.get_tests ();
      EdwardsCurveProperties.get_tests ();
      Properties.get_tests ();
      Equality.get_tests ();
    ]
