let test_state_getter_setter () =
  let open Bls12_381_hash.Permutation.Poseidon.Parameters in
  let state =
    Array.init security_128_state_size_3.state_size (fun _ ->
        Bls12_381.Fr.random ())
  in
  let ctxt =
    Bls12_381_hash.Permutation.Poseidon.allocate_ctxt security_128_state_size_3
  in
  let () = Bls12_381_hash.Permutation.Poseidon.set_state ctxt state in
  assert (
    Array.for_all2
      Bls12_381.Fr.eq
      state
      (Bls12_381_hash.Permutation.Poseidon.get_state ctxt))

let test_consistent_with_mec () =
  let test_vectors =
    [
      ( [|
          "0";
          "19540886853600136773806888540031779652697522926951761090609474934921975120659";
          "27368034540955591518185075247638312229509481411752400387472688330662143761856";
        |],
        [|
          "17943489144262435388134690770306545365190731633977654215868012824127324198151";
          "2231754119684576552235072561055622129225837122807214026821170668631716242147";
          "29261523742327067247029179638981197564247814302680832614540814949720900275190";
        |] );
      ( [|
          "0";
          "27079498335589470388559429012071885383086029562052523482197446658383111072774";
          "42808036164195249275280963312025828986508786508614910971333518929197538998773";
        |],
        [|
          "24183560910605453752776639786227559025963231226657372504816371318001117542043";
          "35029299207990307567943227124638286593042324757574009533577874051172424731112";
          "5149101922766693039542544793826724317233738847055587332789635646881987279038";
        |] );
      ( [|
          "0";
          "2280773603130921897588628948561054337050485605305725986334019017364524534576";
          "49742584797721038216392695876666332237687899609664289853866233942594829409245";
        |],
        [|
          "35816535646044617407808534693371034506444808081714503610242949185965092595026";
          "5363956515446161539161780954417765912037041392866393510569016892172498529944";
          "32254025883747625406036521648265940411925032697688307242655649591762898002497";
        |] );
      ( [|
          "0";
          "47345255170739768354940339244069904962490289137071838723660628082786560244227";
          "12508134994244913485311518548620293355291296251148491744230743543606532994206";
        |],
        [|
          "9091234811689308509102172807464454311569090414440587970689563456088495182507";
          "14174922240502925080674757917320110469967806338655989126339180218846318661667";
          "36498938306563858591852167046454653325721089316694704626906932069658540502064";
        |] );
      ( [|
          "0";
          "22266149478348354835731057366322705807112053199389518651299197937563769914341";
          "46391019025260596444368872653036215340017923491388854958773789052412558961328";
        |],
        [|
          "38438670864363407523537730683343143710087993977705604857656216937687834130603";
          "16221205305575053191586076875532883527874413609923551381849833895354625853752";
          "21204380361849499989864581430501306982675221862819264096834120630459940923576";
        |] );
    ]
  in
  List.iter
    (fun (state, expected_output) ->
      let open Bls12_381_hash.Permutation.Poseidon.Parameters in
      let state = Array.map Bls12_381.Fr.of_string state in
      let ctxt =
        Bls12_381_hash.Permutation.Poseidon.allocate_ctxt
          security_128_state_size_3
      in
      let () = Bls12_381_hash.Permutation.Poseidon.set_state ctxt state in
      let () = Bls12_381_hash.Permutation.Poseidon.apply_permutation ctxt in
      let output = Bls12_381_hash.Permutation.Poseidon.get_state ctxt in
      let expected_output = Array.map Bls12_381.Fr.of_string expected_output in
      Array.iter2
        (fun a b ->
          if not (Bls12_381.Fr.eq a b) then
            Alcotest.failf
              "Expected output is %s, computed %s\n"
              (Bls12_381.Fr.to_string a)
              (Bls12_381.Fr.to_string b))
        expected_output
        output)
    test_vectors

let test_poseidon128_with_different_batch_size () =
  let open Bls12_381_hash.Permutation.Poseidon.Parameters in
  let state =
    Array.init security_128_state_size_3.state_size (fun _ ->
        Bls12_381.Fr.random ())
  in
  let compute_output () =
    let batch_size' =
      2 + Random.int security_128_state_size_3.nb_of_partial_rounds
    in
    let parameters =
      {security_128_state_size_3 with batch_size = batch_size'}
    in
    let ctxt = Bls12_381_hash.Permutation.Poseidon.allocate_ctxt parameters in
    let () = Bls12_381_hash.Permutation.Poseidon.set_state ctxt state in
    let () = Bls12_381_hash.Permutation.Poseidon.apply_permutation ctxt in
    let output = Bls12_381_hash.Permutation.Poseidon.get_state ctxt in
    output
  in
  let output = compute_output () in
  let output' = compute_output () in
  Array.iter2
    (fun a b ->
      if not (Bls12_381.Fr.eq a b) then
        Alcotest.failf
          "Output is %s, output' is %s\n"
          (Bls12_381.Fr.to_string a)
          (Bls12_381.Fr.to_string b))
    output
    output'

let test_random_instanciations_of_poseidon_with_different_batch_size () =
  let open Bls12_381_hash.Permutation.Poseidon.Parameters in
  let state_size = 1 + Random.int 10 in
  let nb_of_full_rounds = (1 + Random.int 10) * 2 in
  let nb_of_partial_rounds = 2 + Random.int 100 in
  let ark_length = state_size * (nb_of_full_rounds + nb_of_partial_rounds) in
  let ark = Array.init ark_length (fun _ -> Bls12_381.Fr.random ()) in
  let mds =
    Array.init state_size (fun _ ->
        Array.init state_size (fun _ -> Bls12_381.Fr.random ()))
  in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.random ()) in
  let compute_output () =
    let batch_size = 1 + Random.int nb_of_partial_rounds in
    let parameters =
      {
        state_size;
        round_constants = ark;
        linear_layer = mds;
        nb_of_full_rounds;
        nb_of_partial_rounds;
        batch_size;
      }
    in

    let ctxt = Bls12_381_hash.Permutation.Poseidon.allocate_ctxt parameters in
    let () = Bls12_381_hash.Permutation.Poseidon.set_state ctxt state in
    let () = Bls12_381_hash.Permutation.Poseidon.apply_permutation ctxt in
    let output = Bls12_381_hash.Permutation.Poseidon.get_state ctxt in
    output
  in
  let output = compute_output () in
  let output' = compute_output () in
  Array.iter2
    (fun a b ->
      if not (Bls12_381.Fr.eq a b) then
        Alcotest.failf
          "Output is %s, output' is %s\n"
          (Bls12_381.Fr.to_string a)
          (Bls12_381.Fr.to_string b))
    output
    output'

let test_regression_tests_for_poseidon252 () =
  let open Bls12_381_hash.Permutation.Poseidon.Parameters in
  let vectors =
    [
      ( Array.make
          security_256_state_size_5.state_size
          (Bls12_381.Fr.of_string "19"),
        [|
          "2f26f38f20a624eb7ddc58a28f94a868824a320a64a05c7b028be716c3d47938";
          "577a6555ceb8acfcec1024f76a647a63bef97ef490fa875d5d8d640e9c477973";
          "d3c9f03664b22c12a49a428cd13bf60c397105ae18039208598f00270b71472f";
          "968c4eeb53cb2888a565bf27bc7eb23c648c05f595b1a39fbe11a7aaaba57c4a";
          "e6ddc232b1895b132931211f1052df5a9945ef7c62011a45c5509490cf8cb001";
        |] );
    ]
  in
  List.iter
    (fun (state, expected_output) ->
      let expected_output =
        Array.map
          (fun x -> Bls12_381.Fr.of_bytes_exn (Hex.to_bytes (`Hex x)))
          expected_output
      in
      let ctxt =
        Bls12_381_hash.Permutation.Poseidon.allocate_ctxt
          security_256_state_size_5
      in
      let () = Bls12_381_hash.Permutation.Poseidon.set_state ctxt state in
      let () = Bls12_381_hash.Permutation.Poseidon.apply_permutation ctxt in
      let output = Bls12_381_hash.Permutation.Poseidon.get_state ctxt in
      Array.iter2
        (fun a b ->
          if not (Bls12_381.Fr.eq a b) then
            Alcotest.failf
              "Expected output is %s, computed is %s\n"
              (Bls12_381.Fr.to_string a)
              (Bls12_381.Fr.to_string b))
        expected_output
        output)
    vectors

let () =
  let open Alcotest in
  run
    "Poseidon"
    [
      ("State", [test_case "get and set" `Quick test_state_getter_setter]);
      ( "Batch size consistency",
        [
          test_case
            "Poseidon128 with random batch sizes"
            `Quick
            test_poseidon128_with_different_batch_size;
          test_case
            "Random instance of Poseidon"
            `Quick
            test_random_instanciations_of_poseidon_with_different_batch_size;
        ] );
      ( "Test vectors",
        [
          test_case
            "Poseidon252 (Dusk)"
            `Quick
            test_regression_tests_for_poseidon252;
          test_case
            "Poseidon128 (consistency with MEC)"
            `Quick
            test_consistent_with_mec;
        ] );
    ]
