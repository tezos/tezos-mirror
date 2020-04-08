(** Check the routine generators do not raise any exception *)
module ValueGeneration =
  Test_ec_make.MakeValueGeneration (Bls12_381.G1.Uncompressed)

module IsZero = Test_ec_make.MakeIsZero (Bls12_381.G1.Uncompressed)
module Equality = Test_ec_make.MakeEquality (Bls12_381.G1.Uncompressed)
module ECProperties = Test_ec_make.MakeECProperties (Bls12_381.G1.Uncompressed)
module ValueGenerationCompressed =
  Test_ec_make.MakeValueGeneration (Bls12_381.G1.Compressed)
module IsZeroCompressed = Test_ec_make.MakeIsZero (Bls12_381.G1.Compressed)
module EqualityCompressed = Test_ec_make.MakeEquality (Bls12_381.G1.Compressed)
module ECPropertiesCompressed =
  Test_ec_make.MakeECProperties (Bls12_381.G1.Compressed)

module Constructors = struct
  let test_of_z_one () =
    (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/fq.rs#L18 *)
    let x =
      Z.of_string
        "3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"
    in
    let y =
      Z.of_string
        "1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569"
    in
    let g = Bls12_381.G1.Uncompressed.of_z_opt ~x ~y in
    match g with
    | Some g ->
        assert (
          Bls12_381.G1.Uncompressed.eq (Bls12_381.G1.Uncompressed.one ()) g )
    | None ->
        assert false

  (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/ec.rs#L1196 *)
  (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/ec.rs#L1245 *)
  let test_vectors_1 () =
    let x =
      Z.of_string
        "52901198670373960614757979459866672334163627229195745167587898707663026648445040826329033206551534205133090753192"
    in
    let y =
      Z.of_string
        "1711275103908443722918766889652776216989264073722543507596490456144926139887096946237734327757134898380852225872709"
    in
    let g = Bls12_381.G1.Uncompressed.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let test_vectors_2 () =
    let x =
      Z.of_string
        "128100205326445210408953809171070606737678357140298133325128175840781723996595026100005714405541449960643523234125"
    in
    let y =
      Z.of_string
        "2291134451313223670499022936083127939567618746216464377735567679979105510603740918204953301371880765657042046687078"
    in
    let g = Bls12_381.G1.Uncompressed.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let test_vectors_3 () =
    let x =
      Z.of_string
        "3821408151224848222394078037104966877485040835569514006839342061575586899845797797516352881516922679872117658572470"
    in
    let y =
      Z.of_string
        "2291134451313223670499022936083127939567618746216464377735567679979105510603740918204953301371880765657042046687078"
    in
    let g = Bls12_381.G1.Uncompressed.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let test_vectors_add () =
    let x_1 =
      Z.of_string
        "128100205326445210408953809171070606737678357140298133325128175840781723996595026100005714405541449960643523234125"
    in
    let y =
      Z.of_string
        "2291134451313223670499022936083127939567618746216464377735567679979105510603740918204953301371880765657042046687078"
    in
    let p1 = Bls12_381.G1.Uncompressed.of_z_opt ~x:x_1 ~y in
    let x_2 =
      Z.of_string
        "3821408151224848222394078037104966877485040835569514006839342061575586899845797797516352881516922679872117658572470"
    in
    let p2 = Bls12_381.G1.Uncompressed.of_z_opt ~x:x_2 ~y in
    let x_res =
      Z.of_string
        "52901198670373960614757979459866672334163627229195745167587898707663026648445040826329033206551534205133090753192"
    in
    let y_res =
      Z.of_string
        "1711275103908443722918766889652776216989264073722543507596490456144926139887096946237734327757134898380852225872709"
    in
    let res = Bls12_381.G1.Uncompressed.of_z_opt ~x:x_res ~y:y_res in
    match (res, p1, p2) with
    | (Some res, Some p1, Some p2) ->
        assert (
          Bls12_381.G1.Uncompressed.eq
            res
            (Bls12_381.G1.Uncompressed.add p1 p2) )
    | _ ->
        assert false

  let test_vectors_zero_and_2_not_on_curve () =
    let x = Z.of_string "0" in
    let y = Z.of_string "2" in
    match Bls12_381.G1.Uncompressed.of_z_opt ~x ~y with
    | Some _ ->
        assert false
    | None ->
        assert true

  let test_vectors_zero_and_minus_2_not_on_curve () =
    let x = Z.of_string "0" in
    let y = Z.neg @@ Z.of_string "2" in
    match Bls12_381.G1.Uncompressed.of_z_opt ~x ~y with
    | Some _ ->
        assert false
    | None ->
        assert true

  let test_vectors_random_points_not_on_curve () =
    let x = Z.of_string "90809235435" in
    let y = Z.neg @@ Z.of_string "8090843059809345" in
    match Bls12_381.G1.Uncompressed.of_z_opt ~x ~y with
    | Some _ ->
        assert false
    | None ->
        assert true

  let test_vectors_generator_from_bytes () =
    let x_cs =
      [| Char.chr 0x5c;
         Char.chr 0xb3;
         Char.chr 0x87;
         Char.chr 0x90;
         Char.chr 0xfd;
         Char.chr 0x53;
         Char.chr 0x0c;
         Char.chr 0x16;
         Char.chr 0x78;
         Char.chr 0x17;
         Char.chr 0xfc;
         Char.chr 0x67;
         Char.chr 0x99;
         Char.chr 0x76;
         Char.chr 0xff;
         Char.chr 0xf5;
         Char.chr 0x15;
         Char.chr 0x4f;
         Char.chr 0x95;
         Char.chr 0xc7;
         Char.chr 0x14;
         Char.chr 0x3b;
         Char.chr 0xa1;
         Char.chr 0xc1;
         Char.chr 0xf0;
         Char.chr 0xae;
         Char.chr 0x6a;
         Char.chr 0xcd;
         Char.chr 0xf3;
         Char.chr 0xd0;
         Char.chr 0xe7;
         Char.chr 0x47;
         Char.chr 0xed;
         Char.chr 0xce;
         Char.chr 0x6e;
         Char.chr 0xcc;
         Char.chr 0x21;
         Char.chr 0xdb;
         Char.chr 0xf4;
         Char.chr 0x40;
         Char.chr 0x12;
         Char.chr 0x01;
         Char.chr 0x77;
         Char.chr 0x41;
         Char.chr 0x9e;
         Char.chr 0x0b;
         Char.chr 0xfb;
         Char.chr 0x75 |]
    in
    assert (Array.length x_cs = 48) ;
    let _x = Bytes.init 48 (fun i -> x_cs.(i)) in
    let y_cs =
      [| Char.chr 0xba;
         Char.chr 0xac;
         Char.chr 0x93;
         Char.chr 0xd5;
         Char.chr 0x0c;
         Char.chr 0xe7;
         Char.chr 0x22;
         Char.chr 0x71;
         Char.chr 0x8c;
         Char.chr 0x22;
         Char.chr 0x63;
         Char.chr 0x1a;
         Char.chr 0x79;
         Char.chr 0x18;
         Char.chr 0xfd;
         Char.chr 0x8e;
         Char.chr 0xdd;
         Char.chr 0x59;
         Char.chr 0x5f;
         Char.chr 0x13;
         Char.chr 0x57;
         Char.chr 0x07;
         Char.chr 0x25;
         Char.chr 0xce;
         Char.chr 0x51;
         Char.chr 0xac;
         Char.chr 0x58;
         Char.chr 0x29;
         Char.chr 0x50;
         Char.chr 0x40;
         Char.chr 0x51;
         Char.chr 0x94;
         Char.chr 0x0e;
         Char.chr 0x1c;
         Char.chr 0x8c;
         Char.chr 0x3f;
         Char.chr 0xad;
         Char.chr 0x00;
         Char.chr 0x59;
         Char.chr 0xc0;
         Char.chr 0x0b;
         Char.chr 0xbc;
         Char.chr 0x3e;
         Char.chr 0xfc;
         Char.chr 0x50;
         Char.chr 0x08;
         Char.chr 0xa2;
         Char.chr 0x6a |]
    in
    assert (Array.length y_cs = 48) ;
    let _y = Bytes.init 48 (fun i -> y_cs.(i)) in
    let x = Z.of_bits (Bytes.to_string _x) in
    let y = Z.of_string (Bytes.to_string _y) in
    let g = Bls12_381.G1.Uncompressed.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let get_tests () =
    let open Alcotest in
    ( "From Z elements",
      [ test_case "one (generator)" `Quick test_of_z_one;
        (* test_case "one (generator)" `Quick test_vectors_generator_from_bytes; *)
        test_case "test vectors 1" `Quick test_vectors_1;
        test_case "test vectors 2" `Quick test_vectors_2;
        test_case "test vectors 3" `Quick test_vectors_3;
        test_case
          "test random points not on curve"
          `Quick
          test_vectors_random_points_not_on_curve;
        test_case
          "test vectors zero and 2 not on curve"
          `Quick
          test_vectors_zero_and_2_not_on_curve;
        test_case
          "test vectors zero and minus 2 not on curve"
          `Quick
          test_vectors_zero_and_minus_2_not_on_curve;
        test_case "test vectors add" `Quick test_vectors_add ] )
end

let () =
  let open Alcotest in
  run
    "G1 Uncompressed"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      ECProperties.get_tests ();
      Constructors.get_tests () ] ;
  run
    "G1 Compressed"
    [ IsZeroCompressed.get_tests ();
      ValueGenerationCompressed.get_tests ();
      EqualityCompressed.get_tests ();
      ECPropertiesCompressed.get_tests () ]
