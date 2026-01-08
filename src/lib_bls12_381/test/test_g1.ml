(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Check the routine generators do not raise any exception *)

let () = Random.self_init ()

module G1 = Bls12_381.G1
module ValueGeneration = Test_ec_make.MakeValueGeneration (G1)
module IsZero = Test_ec_make.MakeIsZero (G1)
module Equality = Test_ec_make.MakeEquality (G1)
module ECProperties = Test_ec_make.MakeECProperties (G1)
module BulkOperations = Test_ec_make.MakeBulkOperations (G1)
module InplaceOperations = Test_ec_make.MakeInplaceOperations (G1)

module Memory = struct
  let test_copy () =
    let x = Bls12_381.G1.random () in
    let y = Bls12_381.G1.copy x in
    assert (Bls12_381.G1.eq x y)

  let test_size_in_memory () =
    match Sys.backend_type with
    | Native | Bytecode -> assert (Bls12_381.G1.size_in_memory = 160)
    | Other _ -> ()

  let get_tests () =
    let txt = "Memory" in
    let open Alcotest in
    ( txt,
      [
        test_case "copy" `Quick (Utils.repeat 100 test_copy);
        test_case "size in memory" `Quick test_size_in_memory;
      ] )
end

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
    let g = G1.of_z_opt ~x ~y in
    match g with
    | Some g ->
        Printf.printf "%s\n" Hex.(show (of_bytes (G1.to_bytes g))) ;
        Printf.printf "%s\n" Hex.(show (of_bytes (G1.to_bytes G1.one))) ;
        assert (G1.eq G1.one g)
    | None -> assert false

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
    let g = G1.of_z_opt ~x ~y in
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
    let g = G1.of_z_opt ~x ~y in
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
    let g = G1.of_z_opt ~x ~y in
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
    let p1 = G1.of_z_opt ~x:x_1 ~y in
    let x_2 =
      Z.of_string
        "3821408151224848222394078037104966877485040835569514006839342061575586899845797797516352881516922679872117658572470"
    in
    let p2 = G1.of_z_opt ~x:x_2 ~y in
    let x_res =
      Z.of_string
        "52901198670373960614757979459866672334163627229195745167587898707663026648445040826329033206551534205133090753192"
    in
    let y_res =
      Z.of_string
        "1711275103908443722918766889652776216989264073722543507596490456144926139887096946237734327757134898380852225872709"
    in
    let res = G1.of_z_opt ~x:x_res ~y:y_res in
    match (res, p1, p2) with
    | Some res, Some p1, Some p2 -> assert (G1.eq res (G1.add p1 p2))
    | _ -> assert false

  let test_vectors_zero_and_2_not_on_curve () =
    let x = Z.of_string "0" in
    let y = Z.of_string "2" in
    match G1.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let test_vectors_zero_and_minus_2_not_on_curve () =
    let x = Z.of_string "0" in
    let y = Z.neg @@ Z.of_string "2" in
    match G1.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let test_vectors_random_points_not_on_curve () =
    let x = Z.of_string "90809235435" in
    let y = Z.neg @@ Z.of_string "8090843059809345" in
    match G1.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let get_tests () =
    let open Alcotest in
    ( "From Z elements",
      [
        test_case "one (generator)" `Quick test_of_z_one;
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
        test_case "test vectors add" `Quick test_vectors_add;
      ] )
end

module UncompressedRepresentation = struct
  let test_uncompressed_zero_has_first_byte_at_64 () =
    assert (int_of_char (Bytes.get G1.(to_bytes zero) 0) = 64)

  let test_uncompressed_random_has_first_byte_strictly_lower_than_64 () =
    assert (int_of_char (Bytes.get G1.(to_bytes (random ())) 0) < 64)

  let test_of_bytes_exn_to_bytes_consistent_on_random () =
    let r = G1.random () in
    assert (G1.(eq (of_bytes_exn (to_bytes r)) r))

  let test_bytes () =
    let test_vectors =
      [
        ( G1.one,
          Bytes.of_string
            "\023\241\211\1671\151\215\148&\149c\140O\169\172\015\195h\140O\151t\185\005\161N:?\023\027\172XlU\232?\249z\026\239\251:\240\n\
             \219\"\198\187\b\179\244\129\227\170\160\241\160\1580\237t\029\138\228\252\245\224\149\213\208\n\
             \246\000\219\024\203,\004\179\237\208<\199D\162\136\138\228\012\170#)F\197\231\225"
        );
        ( G1.zero,
          Bytes.of_string
            "@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
        );
      ]
    in
    List.iter
      (fun (v, expected_value) ->
        assert (G1.(eq v (G1.of_bytes_exn expected_value))))
      test_vectors

  let test_of_bytes_exn_to_bytes_consistent_on_one () =
    let r = G1.one in
    assert (G1.(eq (of_bytes_exn (to_bytes r)) r))

  let test_of_bytes_exn_to_bytes_consistent_on_zero () =
    let r = G1.zero in
    assert (G1.(eq (of_bytes_exn (to_bytes r)) r))

  let test_of_bytes_checks_the_point_is_in_the_prime_subgroup () =
    (* These elements have been generated by bls12-381-unix-blst, commit
       0bea1bf03a7633a4fbf6d551223c6b44a9452acc, by removing the multiplication
       by the cofactor in the random generator. It can be verified the elements
       are not in the prime subgroup by checking multiplying by Fr.order does
       not give zero. *)
    let test_vectors =
      [
        "10376aaf0c54a25eb3e35b0b8d6ab854e6bee26ceaf1a394603e290bc6aaa71fe5233d9af32a74fab9943acf3c6f7ff016cc99cdedd2216ae8d072bdab899c357af72ac44d8d30f487789ad982d592525dda3f1db6e384cd7d8dd542d3c7c080";
        "18bc8b49201ed26cb02e961fc5215c53c14a6bd782ccb900c3e93d546779334c3e04701772f9bda82d3b7eab59ced4ed13dd4e036b9e354cbcaad14fa707f0ee9fd9c5563aaf644031a2bb6887d47984abf6c65ace5a17b7c69cac3394fc6dfa";
        "138909ede51274512d2e970658db689a9dcfba39505627be8282bdc979fa4abd9c0cabe1ee8e92c6ebcc8ad2b6610526099ea73c790741c16e7b38dc09cb87a9e1b097f3324935c70b80a01446014d3ec83437555afdb244d9de5f7242a26824";
        "17c7029fa502f41befa17cc6f3c2ab84b26f77ef97876e1d48476eb94a719e3b353c8e9f4742f4f8ef52738f82c3400c1809130d8c3bf07e38bcf9598e65b458dba5aa550dfdc4d8d9266cba6dd8fbf1985d8876843ea2ec69327cac09b8a333";
        "18c11e829a971d8ef68a8a0d0b1159837d554f7947344ca569a4cbb4f8ae9dd2c94d44faafc45989a422002cc5ce5a820ddad78558f779c08e0c52304f54e28b812e06ccc562fdb805497d3ce011123739fdd70f34f74a33452cc746b5d18873";
        "16db77aaf61a73940cd936e91640613d262e0652a95e4a23b0ce5574cea04f55d268421c07377af44b8a58ccca16ef5100d52746d4e28dbfae072f09b5ac2dfe538b5b9c1e62331f318da15a57abad3937c563ec5e33c0a91b1ce1dfd6630263";
        "0d8d88f63245a412b53a4ee8e4371109e5ef241a210b2087fbbb9abc5b829296c7461788b2a9d5f250b204ac70dd138708aae49479a816872e226cba1382c8b637231ac299898157b0fee042cab25e76f53977161b56096c3b81ec9601091042";
        "186de98a6aee211f0c96e0958cd5a8e82909b1489db2ae7ebfa3a4c56c297832f726da4e1abbacbef9eae74f8fde0f4a15ef7de2a341c2a152dd626c366716ee22304e60c61a32c11579f2c30b0ad3f4fa1455a8dc4a70802b6982315d8f879c";
      ]
    in
    List.iter
      (fun p_str ->
        try
          let p_bytes = Hex.to_bytes (`Hex p_str) in
          ignore @@ Bls12_381.G1.of_bytes_exn p_bytes ;
          assert false
        with Bls12_381.G1.Not_on_curve _p_bytes -> ())
      test_vectors ;
    List.iter
      (fun p_str ->
        let p_bytes = Hex.to_bytes (`Hex p_str) in
        assert (Option.is_none (Bls12_381.G1.of_bytes_opt p_bytes)))
      test_vectors

  let test_of_bytes_exn_and_opt_do_not_accept_compressed_bytes_representation ()
      =
    let x = Bls12_381.G1.random () in
    let x_compressed_bytes = Bls12_381.G1.to_compressed_bytes x in
    assert (Option.is_none (Bls12_381.G1.of_bytes_opt x_compressed_bytes)) ;
    try
      ignore @@ Bls12_381.G1.of_bytes_exn x_compressed_bytes ;
      assert false
    with Bls12_381.G1.Not_on_curve _b -> ()

  let get_tests () =
    let open Alcotest in
    ( "Representation of G1 Uncompressed",
      [
        test_case
          "zero has first byte at 64"
          `Quick
          test_uncompressed_zero_has_first_byte_at_64;
        test_case
          "of bytes and to bytes are consistent on random"
          `Quick
          (Utils.repeat 1000 test_of_bytes_exn_to_bytes_consistent_on_random);
        test_case "bytes encoding" `Quick test_bytes;
        test_case
          "of_bytes_[opt/exn] verifies the point is in the prime subgroup"
          `Quick
          test_of_bytes_checks_the_point_is_in_the_prime_subgroup;
        test_case
          "of bytes and to bytes are consistent on one"
          `Quick
          test_of_bytes_exn_to_bytes_consistent_on_one;
        test_case
          "of bytes and to bytes are consistent on zero"
          `Quick
          test_of_bytes_exn_to_bytes_consistent_on_zero;
        test_case
          "of_bytes_opt/exn do not accept compressed bytes representation"
          `Quick
          (Utils.repeat
             1000
             test_of_bytes_exn_and_opt_do_not_accept_compressed_bytes_representation);
        test_case
          "random has first byte strictly lower than 64"
          `Quick
          (Utils.repeat
             1000
             test_uncompressed_random_has_first_byte_strictly_lower_than_64);
      ] )
end

module CompressedRepresentation = struct
  include Test_ec_make.MakeCompressedRepresentation (G1)

  let test_vectors () =
    let vectors =
      [
        ( Hex.to_bytes
            (`Hex
               "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"),
          G1.to_compressed_bytes G1.one );
        ( Hex.to_bytes
            (`Hex
               "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
          G1.to_compressed_bytes G1.zero );
      ]
    in
    List.iter
      (fun (expected_bytes, computed_bytes) ->
        assert (Bytes.equal expected_bytes computed_bytes))
      vectors

  (* Random points generated for regression tests ```ocaml let () = let x =
     Bls12_381.G1.random () in Printf.printf "Compressed: %s\nUncompressed:
     %s\n" Hex.(show (of_bytes (Bls12_381.G1.to_compressed_bytes x))) Hex.(show
     (of_bytes (Bls12_381.G1.to_bytes x))) ``` *)
  let regression_tests () =
    let vectors =
      [
        ( G1.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                  "0cb0050dcf2f82c75c431f1fd4eb44acc526dbc70842c173bf6e10e48482d54c334b25256e1c619e095443a139c159c715a8f06580d0c818ab623b8a7e31d961c28b8e1ee33ab2e60089ac887db42790f704043d3cb7a5bad69e3b4cf312c1be"),
          Hex.to_bytes
            (`Hex
               "acb0050dcf2f82c75c431f1fd4eb44acc526dbc70842c173bf6e10e48482d54c334b25256e1c619e095443a139c159c7")
        );
        ( G1.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                  "15066d35ee5ff3d2b7c6f56e59770cb74eeb0cec96e964bc06faa95fe9ad43adfd77eed86c79af4bd70fdafab703daf8008b67f7b37d345969daed600bd1f6032ab209b0793dd8029e587aa18f463fe63e2909868fb719eab6fbd659dfb3910a"),
          Hex.to_bytes
            (`Hex
               "95066d35ee5ff3d2b7c6f56e59770cb74eeb0cec96e964bc06faa95fe9ad43adfd77eed86c79af4bd70fdafab703daf8")
        );
        ( G1.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                  "12a57cbec8a77a1d2e78397910bfff2c53e2a42add9219ab7282d37d668e7e55573a0195baad77c8c4daab5cb777306812919ee53ce703dc9564a2c60dd43641b4950feec26f6146d69bd00e4d329c8a2db86e294d549dd6a04f00a085880517"),
          Hex.to_bytes
            (`Hex
               "b2a57cbec8a77a1d2e78397910bfff2c53e2a42add9219ab7282d37d668e7e55573a0195baad77c8c4daab5cb7773068")
        );
      ]
    in
    List.iter
      (fun (p, expected_bytes) ->
        let computed_bytes = G1.to_compressed_bytes p in
        assert (Bytes.equal expected_bytes computed_bytes))
      vectors

  let test_of_compressed_bytes_exn_and_opt_do_not_accept_uncompressed_bytes_representation
      () =
    let x = Bls12_381.G1.random () in
    let x_uncompressed_bytes = Bls12_381.G1.to_bytes x in
    assert (
      Option.is_none (Bls12_381.G1.of_compressed_bytes_opt x_uncompressed_bytes)) ;
    try
      ignore @@ Bls12_381.G1.of_compressed_bytes_exn x_uncompressed_bytes ;
      assert false
    with Bls12_381.G1.Not_on_curve _b -> ()

  let test_of_compressed_bytes_checks_the_point_is_in_the_prime_subgroup () =
    (* These elements have been generated by bls12-381-unix-blst, commit
       ffa2ad5f1c882f05d64c7cb1633c6256b08513bf, by removing the multiplication
       by the cofactor in the random generator. It can be verified the elements
       are not in the prime subgroup by checking multiplying by Fr.order does
       not give zero. *)
    let test_vectors =
      [
        "a4c9678ad327129f4388e7f7ff781fc8e98d181add820b79d15facdca422b3ee7fb20f7082a7f9b7c7915053191cb013";
        "97ae9b4dc6a05cda8bc833dfb983e41423d224bbf6954ce4721a50364a2b37643e18a276ce19b07b83a333f90e2de6c2";
        "b1be8c9f94c1435227b9a18fb57a6d9932c1670d16c514d2d9d67839cc0cc19afdcd114d6e06bf8eb8394061bf880bd4";
        "b173357ce7e2340dc64c6a5633e6800683fb0a6c0f4af92b383425bd76d915819252ac9459e79a1bae530ea0145338cb";
        "a53944773013669c2722949399322703c0b92d877e52b95e0309bdf286d8290314763d61952d6812da50c1826bcaf4c3";
        "8fd2557441f4076917ffe8dfb0e12270994351661600e72f48fe654198199f6cc625a041ce3c9b7c765b32cb53e77192";
      ]
    in
    List.iter
      (fun bytes ->
        let bytes = Hex.to_bytes (`Hex bytes) in
        assert (Option.is_none (Bls12_381.G1.of_compressed_bytes_opt bytes)) ;
        try
          ignore @@ Bls12_381.G1.of_compressed_bytes_exn bytes ;
          assert false
        with Bls12_381.G1.Not_on_curve _b -> ())
      test_vectors

  let get_tests () =
    let open Alcotest in
    let name, common_tests = get_tests () in
    ( name,
      test_case "vectors" `Quick test_vectors
      :: test_case
           "of_compressed_bytes_[opt/exn] verifies the point is in the prime \
            subgroup"
           `Quick
           test_of_compressed_bytes_checks_the_point_is_in_the_prime_subgroup
      :: test_case
           "of_compressed_bytes_opt/exn do not accept uncompressed bytes \
            representation"
           `Quick
           (Utils.repeat
              1000
              test_of_compressed_bytes_exn_and_opt_do_not_accept_uncompressed_bytes_representation)
      :: test_case "Regression tests" `Quick regression_tests
      :: common_tests )
end

module ArithmeticRegressionTests = struct
  (* The test vectors in this module have been generated by the scripts in
     utils, using the implementation bls12-381-unix, commit
     c35cbb3406570ce1465fbf8826cd2595ef02ab8e *)

  let test_vectors () =
    let v =
      [
        ( "0636dad9d3ab31140fad0ed850578de589c15eeb8d7ac5f85a44075dffefbbb7d0ae841f76ab1014fff1c9059ef69e4a0234316e9f283f0ab8e133ba880a316c35cadc41e1966cefe58ffc9c364d497930383eda284a86ade94f2db2782f8a74",
          "0ea80f3dd8c3b3c3d28f83daea887ed5ef3564ca7a92c224184032eba9beffc6cb8536625e3295cbf6a85e785b0fcf871473d91cd3cae9d77c0d2d160007afcac8b19466be73d7abe49ed48209628d0938280beec3e9af9f3dc37b0655539808",
          "e224a11b0f7e8180f789b973f0d10c70b966d53331f39acd04961a25e446c01f",
          "0b3dfaac448a1b46f0fbba7ed37173465400288c9f39640c786f46213256343cd4509e93ae633596a36b3688ac659ffd09df951525a7bbd6ed484cba1ab44e5d5cf00be02727a196b78b9b74dd3f8f351cbe98e94215ac3a1b6961756aeb7bfc",
          "0636dad9d3ab31140fad0ed850578de589c15eeb8d7ac5f85a44075dffefbbb7d0ae841f76ab1014fff1c9059ef69e4a17cce07b9a57a78f923a73fbbb417b6b2eac6f4311eea5cf81a0d604c063acaaee73c12489097951d0afd24d87d02037",
          "0f96516b3d94cf7140c7d95977c3d14b713a4647b3df3e04cef363f66b0e4a18845e72b65de0c13c67cced5abbe4b674021dd10839812fd03dfe15f4dee07f3267d077944c401f6bdd415661faed3631f9ef862db61e3e8d6dfde385568e6d3f"
        );
        ( "030424c53dfd02c4e2b440493407e33dac623c955e75a308e45818c433bc7ea494756ab0ff74eeefbac1bbbf8bc3458a131bd1ecdddd45cfe3be9bf1bca8ffd46e8c733d8a19e874d56c540ac19fbefa244f0b74ed29e1f99afd984138f7c297",
          "0be8e6076cf22432d2b362748f487d1c9e938ae54aa181b1bfb44f040723e07ef6383660a0535c768d8dbec3651aa50d08e94d74e80b840eb17a95c69ffba586f5a5134299d36f67838e5924999b372ff15f7373524743ff0deca9cd7b7daa21",
          "9d4496ab3c69cf0564cabaad7690e0e62c9d57ec175f8c60349facc639458f5c",
          "08666583cb10b573e351ff50b3fd62c1b028b31b9342db9d53cc4c9ec7375037a3cdf77789e7acd82cd8b7a08136a31515de839cdd0a20605676c47dfd5ae5d1d3e92376516debfa045afafd8dc8625551ff32ceb67c56c8fe0355b650293646",
          "030424c53dfd02c4e2b440493407e33dac623c955e75a308e45818c433bc7ea494756ab0ff74eeefbac1bbbf8bc3458a06e53ffd5ba2a0ca675d0bc486a2ad02f5ead847696b2a4a91c47e9635113729fa5cf489c42a1e061f0167bec707e814",
          "10c1dcf84b473bb5caff6e1d4620fee8c54d390546fa64d411a2e0624145230a8e1fb04e08f2765084322b83b7be8f2d0514b4d4812831fe6488493a3e03a6a0bef0ced75399461c5e3c2593955ab873bc91cb8feed5b123f2d29d3dc75a7ecb"
        );
      ]
    in
    List.iter
      (fun (g1, g2, s, g1_plus_g2, minus_g1, s_g1) ->
        let g1 = G1.of_bytes_exn (Hex.to_bytes (`Hex g1)) in
        let g2 = G1.of_bytes_exn (Hex.to_bytes (`Hex g2)) in
        let s = Bls12_381.Fr.of_bytes_exn (Hex.to_bytes (`Hex s)) in
        let g1_plus_g2 = G1.of_bytes_exn (Hex.to_bytes (`Hex g1_plus_g2)) in
        let minus_g1 = G1.of_bytes_exn (Hex.to_bytes (`Hex minus_g1)) in
        let s_g1 = G1.of_bytes_exn (Hex.to_bytes (`Hex s_g1)) in
        assert (G1.(eq g1_plus_g2 (add g1 g2))) ;
        assert (G1.(eq minus_g1 (negate g1))) ;
        assert (G1.(eq s_g1 (mul g1 s))))
      v

  let get_tests () =
    let open Alcotest in
    ( "Regression tests for arithmetic",
      [test_case "Regression tests" `Quick test_vectors] )
end

let () =
  let open Alcotest in
  run
    ~__FILE__
    "G1"
    [
      IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      ECProperties.get_tests ();
      Memory.get_tests ();
      BulkOperations.get_tests ();
      UncompressedRepresentation.get_tests ();
      CompressedRepresentation.get_tests ();
      InplaceOperations.get_tests ();
      ArithmeticRegressionTests.get_tests ();
      Constructors.get_tests ();
    ]
