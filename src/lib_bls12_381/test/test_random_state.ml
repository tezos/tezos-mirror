let test_fr () =
  let expected_value =
    if Utils.is_ocaml_5 then
      Bls12_381.Fr.of_string
        "22989950997422305335515836398089845873697376379127775848923176217833397845914"
    else
      Bls12_381.Fr.of_string
        "14994049377928826363495223704381650737411020059923663878974688106580230994440"
  in
  let state = Random.State.make [|42|] in
  let x = Bls12_381.Fr.random ~state () in
  if not @@ Bls12_381.Fr.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      (Bls12_381.Fr.to_string expected_value)
      (Bls12_381.Fr.to_string x)

let test_fq12 () =
  let state = Random.State.make [|42|] in
  let x = Bls12_381.Fq12.random ~state () in
  let expected_value_str =
    if Utils.is_ocaml_5 then
      "61539da8ffdaa730e4924b378e1bbe6bcc88be76dbec9648f9a12d92f6e9a7a7651fd678f452e78e8a1273b8da85231239cbd7777069f8c1f0b7bc5be8d9cfd6c0a6b19d107ad6b3b5e3bc4e3b3a69bc93855e6e0e1d15abab4a86b89ac12913917279d75e51eb55e328a5efaaabce8cc6efc20b74d11575878cfbee79629e5e1e41b90df930002d3979fc08cab834120a566adf9793d2f7a38ac8acae157695314d9262958e8c3e6599fb69a53a79f1b8f298ef20f9a5eac7e9b22696258c1767bbb5ccdb9e6eb0fb4e38753f8b28e229a2ee7e8ec9f94253b782535016e03cc671a85c0f61ab7ae493cbeb715a6f10671c02099222afe5aa5d40efb3a11bcc27dce99a295733bee44811427698f4510e25769efcbae2719bf2b90f497ae70c655527fafd360c58f650c30de22dc1c61b5df1848a601c31f3b9104bfc30a5cc82873344930de0ab08e1efba939b6004ae9428eaabb4b59623e22317dbc54edbf22f83e9a89c6a8984ef6b1613846bc1c9cc4c21630f71d60bade712e527eb0b720cfd41ec7852c697c8ff1fe81459b3c75fff67fecab7fea0113d38344e937cf4a1abce2fe3ad894523fce7ad8d4a1214ae5e2378c481456288a71e09e603027c7750abdad084284331da06e2c5630ab7b16625d7ef9866f855b9134d44f40d4fbb256856d7cecfb305bd6c55b2430ee84534d1279d8a66230e625ecda966b42dfcc58b7b68b8097c581e4eb16473023cd93e0fb220856a66d066fe6104dc8e12cbda2b7b2d09fcf4a0b1d78950a2455f2831a5c00336db76b9e54e5a2a0c12"
    else
      "25f768c109cbed9bdca2f5246b4048e629577e67f0b7b60017b93a676df4525e26571c8bf615a2db454e607d791a900d44547785055f24d6de2fb554b3afb75fe403ce99ca64e9446469f610ef9912cb15a97da27933143fd520e489df4225071672b08c7a9bff225de298772bb2b2b48ccffedb8d95fe7ef8c64c467931d84af028a9e583874c4d3f765d5a83d944059016a069f336b5a029ff3a93ccd3170fd64ff5c103528ffdc03315216aa779017d921fd1072c2576f9b4d5e97f8c9c0db0ab0cdacf46de2fc0b87c5f7309dd69e6b44d4e41c16533ab3114aba08517e7fd25f04a6e5908f9a33ba1da82327008e1d3ae9069f091b0f127daefdae4bb3b25822c0644b265892f9989196f5ae8f1195cbbd7e847af7547f43923afd7c1073808aff305be9fab1ec9b709bdcfd109b0b2354d39ab5747493405c1e97d27ab9185369e1371d7df4dc990c294322c0fb2fc8fce727ca6db9cb5e4a4ca9b250c6b2b2b494e9c5bac9bec4ae79151560afe53eede1c657f5a459434d59e6cbf00f8645d0d9ae1c2165ef3433c6c0362725c64a495d167df503e33e05147646fef45ffd762cb77370669cc0259524f280f3755a09cc1104ea8c3c958186f6f7748608bef4edc700517b6bd310be3cf0229b9d8f93021b099504627b1b9aaa092158be839cbdda3be9fa3d27ba8773106d98cca2018841fe2f11ec0beb53630db7f5552cf0df4b65d272d5de25164a4301898d723df680776dfdf795fa3c0ec7182292a8b77ecc243fabf8cf49378aaa1e2cd6865311ab888a3c10a71d5a91d9109"
  in
  let expected_value =
    Bls12_381.Fq12.of_bytes_exn (Hex.to_bytes (`Hex expected_value_str))
  in
  if not @@ Bls12_381.Fq12.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      Hex.(show (of_bytes (Bls12_381.Fq12.to_bytes expected_value)))
      Hex.(show (of_bytes (Bls12_381.Fq12.to_bytes x)))

let test_g1 () =
  let state = Random.State.make [|42|] in
  let x = Bls12_381.G1.random ~state () in
  let expected_value_str =
    if Utils.is_ocaml_5 then
      "0b88058e33258eebb07cc801e81ca0ef4a1c721a03bcd7dd8c9cca912026eb61e54081c0504d267cf0cee5eeb5eda0ee0dc4b1a1dc1ad6cd2adbc8582b1142ee96a6cda2f8df0c7c287c9df765f9476d8392a794ed4251fc8f0347fecc2009ca"
    else
      "00e9a2b6bb8b06be174a7d3cbe03e8f4bf2371149124ebb2d11dba1364c588705871df293ef1be2be7f93638b004c3a806993a04535895a92a05e37dedcc0819a7362475eab3befd786569aa9f915a4179f1d60eb0437665dc13dcb2084bc3ef"
  in
  let expected_value =
    Bls12_381.G1.of_bytes_exn (Hex.to_bytes (`Hex expected_value_str))
  in
  if not @@ Bls12_381.G1.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      Hex.(show (of_bytes (Bls12_381.G1.to_bytes expected_value)))
      Hex.(show (of_bytes (Bls12_381.G1.to_bytes x)))

let test_g2 () =
  let state = Random.State.make [|42|] in
  let x = Bls12_381.G2.random ~state () in
  let expected_value_str =
    if Utils.is_ocaml_5 then
      "12e3c3a32110c944aee689cf7de24c4aeb9fc24884721e881ba3153d831a5cbcb498142ed0adb28fd79fd0b6fe1223f70e3deb301dd6f087d91112e4682f2f2ba03eabbf1519de0c0fea822ec094c7a9830268039276f58db26f1daf0d55be0500b531b7dcadfaad956e307ccf798d86a57924ba49cd42adc1ab06aa8300b4a1431a4dd280a5858555fe82a34adfadd60df8c05e2b6d2ce8cebae092c04442d8103507b45694c317785351321e5e46b818f92ff44399e24c3c4c08104d6ae786"
    else
      "18f91ca488ba88919f6fc26b9275535a35e9946df913caa0a6ad89334a0e40b99b3898e0658693c2b6b1d764c3d556e417dbba86d110240b503ba58eb56d462a88f6d6c9b49735c76c641fc616c18997928db9cdc6b95e543c944e684ae73f0d08a0e3fd91e9155c705f293c4efc1073b864f26b8b799f140e4389a6d9ae1f82a9d89613fa20b34fd555e2f323a09f7b0b4a56ecd554ea0f84fc30d1e93f26b956d9e78d04583a5bd044f71772bc5a39e187e9c2e9527b2821b1f51435edd77a"
  in
  let expected_value =
    Bls12_381.G2.of_bytes_exn (Hex.to_bytes (`Hex expected_value_str))
  in
  if not @@ Bls12_381.G2.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      Hex.(show (of_bytes (Bls12_381.G2.to_bytes expected_value)))
      Hex.(show (of_bytes (Bls12_381.G2.to_bytes x)))

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Random state"
    [
      ( "Random state",
        [
          test_case "Fr" `Quick test_fr;
          test_case "Fq12" `Quick test_fq12;
          test_case "G1" `Quick test_g1;
          test_case "G2" `Quick test_g2;
        ] );
    ]
