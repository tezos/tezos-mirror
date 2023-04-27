let test_state_getter_setter () =
  let parameters =
    Bls12_381_hash.Permutation.Griffin.Parameters.security_128_state_size_3
  in
  let ctxt = Bls12_381_hash.Permutation.Griffin.allocate_ctxt parameters in

  let state =
    Array.init
      parameters.Bls12_381_hash.Permutation.Griffin.Parameters.state_size
      (fun _ -> Bls12_381.Fr.random ())
  in
  let () = Bls12_381_hash.Permutation.Griffin.set_state ctxt state in
  assert (
    Array.for_all2
      Bls12_381.Fr.eq
      state
      (Bls12_381_hash.Permutation.Griffin.get_state ctxt))

let test_vectors_griffin_3 () =
  (* Generated from https://extgit.iaik.tugraz.at/krypto/zkfriendlyhashzoo *)
  let vectors =
    [
      ( ("0", "0", "0"),
        ( "841791784119451836895521356553213353441862647772555419706984344393580307020",
          "2113910108688023830687972208474679779076805829875515720963977502550499149602",
          "39803944790582685391308803209116565255311062071666522256011667391677817204123"
        ) );
      ( ( "18463500124224312515875567238728244985257614546546778836630989065496961733332",
          "46981865534249059754749826182943352598004016487267289209329045182268251827732",
          "281220935108567092124864882450372048777669128036755944610462739083424420854"
        ),
        ( "312967684503808125264008501181907359079558761116291453411613667947766899516",
          "45085022860794304016331869598892373488807386198128958652149726446059364221856",
          "3143235844449445682654083063998365970447678809821256700746892060010723945844"
        ) );
      ( ( "37430619512013660924725326078209379186383782215505557594723583515547878680657",
          "15993191006246128054961715163349476792514546237817331760637228653648805884929",
          "586854312443779697715508748423011917816841144091125416169048666119872372790"
        ),
        ( "17795078392989753588187047645247938619792251973934963507233142728921502730375",
          "23947216748739774482602221821766081757111537527154723423518134779107549197638",
          "10733328928829291297000046981966971033540811270220965825105168354718419883281"
        ) );
    ]
  in
  List.iter
    (fun ((x1_s, x2_s, x3_s), (exp_res1_s, exp_res2_s, exp_res3_s)) ->
      let x1 = Bls12_381.Fr.of_string x1_s in
      let x2 = Bls12_381.Fr.of_string x2_s in
      let x3 = Bls12_381.Fr.of_string x3_s in
      let exp_res1 = Bls12_381.Fr.of_string exp_res1_s in
      let exp_res2 = Bls12_381.Fr.of_string exp_res2_s in
      let exp_res3 = Bls12_381.Fr.of_string exp_res3_s in
      let ctxt =
        Bls12_381_hash.Permutation.Griffin.allocate_ctxt
          Bls12_381_hash.Permutation.Griffin.Parameters
          .security_128_state_size_3
      in
      let () =
        Bls12_381_hash.Permutation.Griffin.set_state ctxt [|x1; x2; x3|]
      in
      let () = Bls12_381_hash.Permutation.Griffin.apply_permutation ctxt in
      let res = Bls12_381_hash.Permutation.Griffin.get_state ctxt in
      let res1, res2, res3 = (res.(0), res.(1), res.(2)) in
      let b =
        Bls12_381.Fr.eq exp_res1 res1
        && Bls12_381.Fr.eq exp_res2 res2
        && Bls12_381.Fr.eq exp_res3 res3
      in
      if not b then
        Alcotest.failf
          "Expected result = (%s, %s, %s), computed result = (%s, %s, %s), \
           input = (%s, %s, %s)"
          exp_res1_s
          exp_res2_s
          exp_res3_s
          (Bls12_381.Fr.to_string res1)
          (Bls12_381.Fr.to_string res2)
          (Bls12_381.Fr.to_string res3)
          x1_s
          x2_s
          x3_s)
    vectors

let test_vectors_griffin_4 () =
  (* Generated from https://extgit.iaik.tugraz.at/krypto/zkfriendlyhashzoo *)
  let vectors =
    [
      ( ("0", "0", "0", "0"),
        ( "42055013899249843001963096382529769773374725832883030314842616909329673256554",
          "37539438133362431743003153246954584523646613658726962536114018819275530248977",
          "48674648934505632020893033170300663189772692405048902597301372423269515724906",
          "26051996228945922008001164667516225310485844262139985745397463874220219812431"
        ) );
    ]
  in
  List.iter
    (fun ( (x1_s, x2_s, x3_s, x4_s),
           (exp_res1_s, exp_res2_s, exp_res3_s, exp_res4_s) ) ->
      let x1 = Bls12_381.Fr.of_string x1_s in
      let x2 = Bls12_381.Fr.of_string x2_s in
      let x3 = Bls12_381.Fr.of_string x3_s in
      let x4 = Bls12_381.Fr.of_string x4_s in
      let exp_res1 = Bls12_381.Fr.of_string exp_res1_s in
      let exp_res2 = Bls12_381.Fr.of_string exp_res2_s in
      let exp_res3 = Bls12_381.Fr.of_string exp_res3_s in
      let exp_res4 = Bls12_381.Fr.of_string exp_res4_s in
      let ctxt =
        Bls12_381_hash.Permutation.Griffin.(
          allocate_ctxt Parameters.security_128_state_size_4)
      in
      let () =
        Bls12_381_hash.Permutation.Griffin.set_state ctxt [|x1; x2; x3; x4|]
      in
      let () = Bls12_381_hash.Permutation.Griffin.apply_permutation ctxt in
      let res = Bls12_381_hash.Permutation.Griffin.get_state ctxt in
      let res1, res2, res3, res4 = (res.(0), res.(1), res.(2), res.(3)) in
      let b =
        Bls12_381.Fr.eq exp_res1 res1
        && Bls12_381.Fr.eq exp_res2 res2
        && Bls12_381.Fr.eq exp_res3 res3
        && Bls12_381.Fr.eq exp_res4 res4
      in
      if not b then
        Alcotest.failf
          "Expected result = (%s, %s, %s, %s), computed result = (%s, %s, %s, \
           %s), input = (%s, %s, %s, %s)"
          exp_res1_s
          exp_res2_s
          exp_res3_s
          exp_res4_s
          (Bls12_381.Fr.to_string res1)
          (Bls12_381.Fr.to_string res2)
          (Bls12_381.Fr.to_string res3)
          (Bls12_381.Fr.to_string res4)
          x1_s
          x2_s
          x3_s
          x4_s)
    vectors

(*
    Bls12_381.Fr.
      [| of_string
           "42055013899249843001963096382529769773374725832883030314842616909329673256554";
         of_string
           "37539438133362431743003153246954584523646613658726962536114018819275530248977";
         of_string
           "48674648934505632020893033170300663189772692405048902597301372423269515724906";
         of_string
           "26051996228945922008001164667516225310485844262139985745397463874220219812431"
      |] 
   *)
let () =
  let open Alcotest in
  run
    ~__FILE__
    "Griffin"
    [
      ( "Test vectors",
        [
          test_case
            "from reference implementation: Griffin 3"
            `Quick
            test_vectors_griffin_3;
          test_case
            "from reference implementation: Griffin 4"
            `Quick
            test_vectors_griffin_4;
        ] );
      ("State", [test_case "get and set" `Quick test_state_getter_setter]);
    ]
