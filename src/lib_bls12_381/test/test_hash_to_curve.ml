open Utils

let test_vectors_g1_from_bls_sigs_ref_files () =
  let aux filename =
    let contents = read_file filename in
    let ciphersuite = Bytes.make 1 (char_of_int 1) in
    List.iter
      (fun content ->
        let contents = String.split_on_char ' ' content in
        let msg_str, expected_result_str =
          (List.nth contents 0, List.nth contents 2)
        in
        let msg = Hex.(to_bytes (`Hex msg_str)) in
        let res = Bls12_381.G1.hash_to_curve msg ciphersuite in
        let expected_result =
          Bls12_381.G1.of_compressed_bytes_exn
            Hex.(to_bytes (`Hex expected_result_str))
        in
        if not @@ Bls12_381.G1.eq res expected_result then
          Alcotest.failf
            "Expected result is %s on input %s, but computed %s"
            Hex.(show (of_bytes (Bls12_381.G1.to_bytes expected_result)))
            msg_str
            Hex.(show (of_bytes (Bls12_381.G1.to_bytes res))))
      contents
  in
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_B233")) ;

  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_B283")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_B409")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_B571")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_K233")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_K409")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_K571")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_P224")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_P256")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_P384")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_fips_186_3_P521")) ;
  aux (path_test_vectors ("hash_to_curve_g1" // "g1_rfc6979"))

let test_vectors_g2_from_bls_sigs_ref_files () =
  let aux filename =
    let contents = read_file filename in
    let ciphersuite = Bytes.make 1 (char_of_int 2) in
    List.iter
      (fun content ->
        let contents = String.split_on_char ' ' content in
        let msg_str, expected_result_str =
          (List.nth contents 0, List.nth contents 2)
        in
        let msg = Hex.(to_bytes (`Hex msg_str)) in
        let res = Bls12_381.G2.hash_to_curve msg ciphersuite in
        let expected_result =
          Bls12_381.G2.of_compressed_bytes_exn
            Hex.(to_bytes (`Hex expected_result_str))
        in
        if not @@ Bls12_381.G2.eq res expected_result then
          Alcotest.failf
            "Expected result is %s on input %s, but computed %s"
            Hex.(show (of_bytes (Bls12_381.G2.to_bytes expected_result)))
            msg_str
            Hex.(show (of_bytes (Bls12_381.G2.to_bytes res))))
      contents
  in
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_B233")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_B283")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_B409")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_B571")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_K233")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_K409")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_K571")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_P224")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_P256")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_P384")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_fips_186_3_P521")) ;
  aux (path_test_vectors ("hash_to_curve_g2" // "g2_rfc6979"))

(** These values have been generated using utils/generate_hash_to_curve_vectors
    using commit b23c84a69187f81302d173f67fc65470fe90f810 *)
let regression_test () =
  let v =
    [
      ( "70b66877f3150ab25211757142a8a31f7572846af9c4107890b00c6e00ba1c900be38f08b3275482f5c607d202ee5840a49dffc29f89c9056b7ddae3bf33445af7037707ccf79d5935913aedc85ba12a56ff39b433ca8f6b666eec7c7089eaf3de90c7dce1e1acee1127b55ade7f43d277079af2f3d4be5e8c637f346f75360c2df6c5ac2c582c6107f210075ddc3ae0858565576faf4851f14ffa6667d80345167ff6f0d27f2d7a10a2f0599fec2106e9d080db136a6c1849d41b961c24b61765f9031959686e9c375fad0494725ee0146d577645",
        "6f4f",
        "01cc208142d332d59bde72628e8815067df2fa27f91f924b3b6799797d716275db43318bab58c50d59019fae5f902916078b411fecf76fff583dd2f85bb713368a6de3f8c3a55e6da96332f9523808b97c371345fc310edbfff3be80e2a2b2dd",
        "04777bdb478f10a448766dc28b763377e5b6a18545292cf9e2001f9b5ca819de2c63e7486f8276ce6d5d95456ba2d8a01929ee0ff20679ca60d506798e4e4dcae03540155819078b7d4458982bfd032eea179725c9c59aa6f57893ce43086f320ffa1fa42c2c56a65bedb4f62a724a543639bb465981cca5a67ea31c74144aacd2cd669f2100111b3394b5d704552bb9198601887622ba0b06e8d1a5a8c0c7bf69ed2b451c677117e85cfa274fd86e2e2f05c92acc77f68e4e942a0dd65b1e2c"
      );
      ( "09d92f852e18db6488b1b7f15dad5008c51621fd169ccab5db63e676bfcb9eb054a6f87768f559cc09ef97a499fcb6fa37579af9b15d16df2f07f6efd27616d5599964aa9f99e5dd09bd85997c83b3f7c294775820ac908eb9e4644b6c689e7581fc693e838bbf3317219734d6c0cef4a16f51f529a24f0779de0600ed90509ba606c3f21f7522eeb7fe8d0c6f677b4750cd478c09",
        "f319946f",
        "0e0a2932d95910e38af251bcce22c39a33797e440e420ec5798b5c4d21a3336a72e248c56fb7c5969febd2735765675e010eca9472c04880b5709c979e3df86c05610c05ea7111260dab0fde8ba2042cb33a9d2acb85cfd9194de484d12d3b81",
        "0d097c6e12d98dcff792c81dd3d7a813992f7c963dbd2a2f251469b36244c1021cdb6a3c6bbc7869ca437d540f77d713182d06b7208e4ccbdb585cc0c47552083990c8d329e3470dfbaa951173c12dee59feef0db3980fbf85a2c083d94bb24f0768d9b7448732805b3068baaa2c636a0a65bd87e86b0f90e3342706e306abf65bb0017f90d75c8fdb8b27fa64716f4b09dc95edeb267ede6047d4af506a2bb6c607348abfdcdf3869735abf91eed5f27177be39c83289380d4916edfe2dc7b7"
      );
      ( "a5f753f4761de36136fd22f75a528c1bc7a49ce17721f1eda5cb8593976985dd980cbb473184d03dad9b21a57053d8fbeb5325391c1daf54b2dc8205827654c808f8318ed2524a8b43af9de27ab8e5aa2bb7c7a590392f34dd2c58bb112bfead7b634149aa17802efcce1e94c894fbe17e62b82cb078fde314783cda724936c20dd33715d570de3689e2355fca1f0b53880ae8c0a7ef9d86df83251d4ac7ed0c9d43a1b0f1c22aff60d8896a900847976e13860f33c952e6485d23123dae2f363ac9f4bfd64942eee46e3b3aebd9194cb1c0157c9550e8cf9ecdb22a8a501dac897c3e5a6691845085c341159c5a390d13428ec40d140ccb8eaae8669968a283dd7a925052a398389052761cb02bcea11931e8a8732571161b10aa0ceb40d46e50dd5ef0e6c1539fa206e8c435e464a6e8e022098a1299db743c5243e2719ce239cdf496e21e4cb644e667b31fc6b8f8f7d211756f6f5ab7129e3fdf45fb63dcdedc741f19bd6c29cd8cc8a5ca55cb68f95abe6ce5f5ab35642f6c2707aa9421cdd6c51a8b190ac65cd81d45be0b160eede6313de421fc4008cd3e0f66075ea0579fc85f65d342431ec3c4a902ba426a19ab77e5cb9a68d99a1919e714626ba36acdb523ea5d4e05ac03da",
        "10ad7e03b7613f4415daa22053bc5fddfb37da",
        "137010ddaec91358b78ff5a0bee606cec3b54f4f123638a227af1589807bfea526954e2697dfe5d768b0cc922f8bc2a30e379bf86063051fce93c8153480581297b7bc8b2dd90c210d68432963429fca3b747f8d9c06f71281f7c4e24ba191c2",
        "0b45851b0762e2a23e5e0be0b6cd2bb65d5ce6f202478fbb530afa83b39ab604927e1eda138ce3e8f39b292aae76160913e79780a8ad3f806c23a880d067c2245902eae7599f6e02df0ac541dd9593989d5ee9fe3ec4a496a513ca3c867ce6410764f0166109f69a0331e4b8440782bf312ce13108883cf4ba76cda8752754b70143c0323728264231e3c13887bce96819003da296db8d216947af2db1f3b5366f47142d5fbaf68f349f5051e6eb218d5eed424de5ef981d87b33e977ae11b31"
      );
      ( "145521830cb8448b569df9c4e8de7c6519963ebdb55d4bf91e64c228cf9bdb89bc7e5c52a148b2e8fea8b31e161fb6469162ff1fcd1e8931adfeaae93c2d0d180653209d23315d0344ecd6ba79043fa5d6db5499a058be6863fe43e671be0cebfcdaa73f3f0b5cc657d2c93921d2c876bd098cce7edc717a437568a3f45cff44d776e59f247c7e23ab22c2de2871f23802cdc9e4857361273b1bc61b6a4a1d2444aade444756944e26489f8c4769ab5f2e3e8e6fba5c671596c3a991b4805af9e2b30467cc827c3d086382ced0228f5948517b24ad8041e4726fd10bd8e24c03fc95f521ed27893798173d85680a64d12e1cb9530188d46f2bd889b17cb42402148cfbb45b0d82ed67a9b9208aaabf9f8b915cd8801f73175c72da72e7bd29a4932337d31af6b995c565748d367f0bc665f18cefce9334760fc11837cea98c08c7e5f99ca871304ff4ca56d58384dda75b636bb63b6d28a0f24606e78e468e924e7323d2d53c2663c1f0d25b201122612e84e21a28b8bcca3a017731c1b5d24c46709b6ad2f51106573c0b5f8e4d0c1ce1f30cb01145698d64ac0b94d7f9c2245277c0f3f7456f77c3ee6bb9f4c8f2170b82d3d9ef38d1b00eaa0b9adad03c93a3d84c819c8e8ec2d972730315559e7936e45db91eadde650ed57bd920a505",
        "57f811338364bcf0293dad562ef3255f7475cd8a",
        "0c475c8b9ab3e274f3a11b5e17bb3e0382f9bea8464382fb79a450aff60f1cb1e7f064aeed53a06231967475d43b5c5f0fd203fdc52a9874844ad25cc3c101dca859d99ebd355c322b5b2c5dfecc00388f8ab091c1bc99b3b425244733969b49",
        "08b463fc305b3f9490b7eb512b3e0bec6e46d75fc9373ddbe3f5d509f0fea50aff32a966049761c536a0427f74d255580865fe82684e96c400524b5af069d317a5758f9fdef682daf08f006005f8b994597932fcc3fe53c4e48f6ea358bb07a00664190a0f3ac8dc262887fc39eeea64a7706a75dadbf7583d6375b0be7b1a43ea6c2c348996287fbcacbaadb80840bc18ee1c8a8b463335f7d421e98ae8a27709481d7c022c7b551727e9132b6dc9419a121115237eab7dc948a5dbeea4eed4"
      );
    ]
  in
  List.iter
    (fun (msg_hex, dst_hex, exp_res_g1_hex, exp_res_g2_hex) ->
      let msg = Hex.(to_bytes (`Hex msg_hex)) in
      let dst = Hex.(to_bytes (`Hex dst_hex)) in
      let exp_res_g1 =
        Bls12_381.G1.of_bytes_exn (Hex.to_bytes (`Hex exp_res_g1_hex))
      in
      let exp_res_g2 =
        Bls12_381.G2.of_bytes_exn (Hex.to_bytes (`Hex exp_res_g2_hex))
      in
      let res_g1 = Bls12_381.G1.hash_to_curve msg dst in
      let res_g2 = Bls12_381.G2.hash_to_curve msg dst in
      if not (Bls12_381.G1.eq res_g1 exp_res_g1) then
        Alcotest.failf
          "On msg %s and dst %s, expected value for G1 is %s but computed %s"
          msg_hex
          dst_hex
          exp_res_g1_hex
          Hex.(show (of_bytes (Bls12_381.G1.to_bytes res_g1))) ;
      if not (Bls12_381.G2.eq res_g2 exp_res_g2) then
        Alcotest.failf
          "On msg %s and dst %s, expected value for G2 is %s but computed %s"
          msg_hex
          dst_hex
          exp_res_g2_hex
          Hex.(show (of_bytes (Bls12_381.G2.to_bytes res_g2))))
    v

let test_hash_to_curve_accepts_dst_longer_than_255_characters () =
  let dst = generate_random_bytes (256 + Random.int 1000) in
  let msg = generate_random_bytes (Random.int 10000) in
  ignore @@ Bls12_381.G1.hash_to_curve msg dst ;
  ignore @@ Bls12_381.G2.hash_to_curve msg dst

let () =
  let open Alcotest in
  run
    ~__FILE__
    "hash_to_curve"
    [
      ( "From bls_sigs_ref",
        [
          test_case "G1" `Quick test_vectors_g1_from_bls_sigs_ref_files;
          test_case "G2" `Quick test_vectors_g2_from_bls_sigs_ref_files;
        ] );
      ("Regression tests", [test_case "Using this repo" `Quick regression_test]);
      ( "Special cases",
        [
          test_case
            "DST can be longer than 255 characters"
            `Quick
            (Utils.repeat
               1000
               test_hash_to_curve_accepts_dst_longer_than_255_characters);
        ] );
    ]
