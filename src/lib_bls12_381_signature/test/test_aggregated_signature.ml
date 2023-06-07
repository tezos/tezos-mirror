open Utils

let () = Random.self_init ()

module type SIGNATURE_INSTANTIATION = module type of Bls12_381_signature.MinPk

module MakeAggregatedSignatureTest (SignatureM : SIGNATURE_INSTANTIATION) =
struct
  module type SIG_SCHEME = sig
    val sign : Bls12_381_signature.sk -> Bytes.t -> SignatureM.signature

    (* val verify : SignatureM.pk -> Bytes.t -> SignatureM.signature -> bool *)

    val aggregate_verify :
      (SignatureM.pk * Bytes.t) list -> SignatureM.signature -> bool
  end

  module MakeProperties (Scheme : sig
    include SIG_SCHEME

    val name : string
  end) =
  struct
    let test_sign_and_verify_correct_signature_with_correct_pks_and_msgs () =
      let random_values =
        List.init
          (1 + Random.int 10)
          (fun _ ->
            let msg = generate_random_bytes (1 + Random.int 512) in
            let ikm = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let pk = SignatureM.derive_pk sk in
            let signature = Scheme.sign sk msg in
            (pk, msg, signature))
      in
      let signatures = List.map (fun (_, _, s) -> s) random_values in
      let pks_with_msgs =
        List.map (fun (pk, msg, _) -> (pk, msg)) random_values
      in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (Scheme.aggregate_verify pks_with_msgs aggregated_signatures)

    let test_sign_and_verify_correct_signature_with_correct_pks_and_some_incorrect_msgs
        () =
      let random_values =
        List.init
          (2 + Random.int 10)
          (fun i ->
            let msg = generate_random_bytes (1 + Random.int 512) in
            let msg' =
              if i mod 2 = 0 then msg
              else generate_random_bytes (1 + Random.int 512)
            in
            let ikm = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let pk = SignatureM.derive_pk sk in
            let signature = Scheme.sign sk msg in
            (pk, msg', signature))
      in
      let signatures = List.map (fun (_, _, s) -> s) random_values in
      let pks_with_msgs =
        List.map (fun (pk, msg, _) -> (pk, msg)) random_values
      in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (not (Scheme.aggregate_verify pks_with_msgs aggregated_signatures))

    let test_sign_and_verify_correct_signature_with_some_incorrect_pks_and_correct_msgs
        () =
      let random_values =
        List.init
          (2 + Random.int 10)
          (fun i ->
            let msg = generate_random_bytes (1 + Random.int 512) in
            let ikm = generate_random_bytes 32 in
            let ikm' = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let pk = SignatureM.derive_pk sk in
            let pk' =
              if i mod 2 = 0 then pk
              else SignatureM.(derive_pk (Bls12_381_signature.generate_sk ikm'))
            in
            let signature = Scheme.sign sk msg in
            (pk', msg, signature))
      in
      let signatures = List.map (fun (_, _, s) -> s) random_values in
      let pks_with_msgs =
        List.map (fun (pk, msg, _) -> (pk, msg)) random_values
      in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (not (Scheme.aggregate_verify pks_with_msgs aggregated_signatures))

    let get_tests () =
      let open Alcotest in
      ( Printf.sprintf "Properties for %s" Scheme.name,
        [
          test_case
            "Sign and verify corret signature with correct pks and msgs"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_correct_pks_and_msgs);
          test_case
            "Sign and verify corret signature with correct pks and some \
             incorrect msgs"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_correct_pks_and_some_incorrect_msgs);
          test_case
            "Sign and verify corret signature with some incorrect pks and \
             correct msgs"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_some_incorrect_pks_and_correct_msgs);
        ] )
  end

  module BasicProperties = struct
    include MakeProperties (struct
      include SignatureM.Basic

      let name = "Basic"
    end)

    let test_verify_not_distinct_messages_should_raise_invalid_argument () =
      let n = 2 + Random.int 10 in
      let msg = generate_random_bytes (1 + Random.int 512) in
      let pks_with_sigs =
        List.init n (fun _ ->
            let ikm = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let pk = SignatureM.derive_pk sk in
            let signature = SignatureM.Basic.sign sk msg in
            (pk, signature))
      in
      let pks_with_msgs = List.map (fun (pk, _) -> (pk, msg)) pks_with_sigs in
      let aggregated_signature =
        Option.get
          (SignatureM.aggregate_signature_opt (List.map snd pks_with_sigs))
      in
      ignore
      @@ Alcotest.check_raises
           ""
           (Invalid_argument "Messages must be distinct")
           (fun () ->
             ignore
             @@ SignatureM.Basic.aggregate_verify
                  pks_with_msgs
                  aggregated_signature)

    let get_tests () =
      let open Alcotest in
      let specific_tests =
        [
          test_case
            "Verify signatures with the same message"
            `Quick
            test_verify_not_distinct_messages_should_raise_invalid_argument;
        ]
      in
      let desc, tests = get_tests () in
      (desc, List.concat [specific_tests; tests])
  end

  module AugProperties = struct
    include MakeProperties (struct
      include SignatureM.Aug

      let name = "Message augmentation"
    end)

    let test_verify_same_msg () =
      let n = 2 + Random.int 10 in
      let msg = generate_random_bytes (1 + Random.int 512) in
      let pks_with_sigs =
        List.init n (fun _ ->
            let ikm = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let pk = SignatureM.derive_pk sk in
            let signature = SignatureM.Aug.sign sk msg in
            (pk, signature))
      in
      let pks_with_msgs = List.map (fun (pk, _) -> (pk, msg)) pks_with_sigs in
      let aggregated_signature =
        Option.get
          (SignatureM.aggregate_signature_opt (List.map snd pks_with_sigs))
      in
      ignore
      @@ SignatureM.Aug.aggregate_verify pks_with_msgs aggregated_signature

    let get_tests () =
      let open Alcotest in
      let specific_tests =
        [
          test_case
            "Verify signatures with the same message is allowed"
            `Quick
            test_verify_same_msg;
        ]
      in
      let desc, tests = get_tests () in
      (desc, List.concat [specific_tests; tests])
  end

  module PopProperties = struct
    let test_sign_and_verify_correct_signature_with_correct_pks_pops_and_msg ()
        =
      let msg = generate_random_bytes (1 + Random.int 512) in
      let random_values =
        List.init
          (1 + Random.int 10)
          (fun _ ->
            let ikm = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let proof = SignatureM.Pop.pop_prove sk in
            let pk = SignatureM.derive_pk sk in
            let signature = SignatureM.Pop.sign sk msg in
            ((pk, proof), signature))
      in
      let pks_with_proofs = List.map fst random_values in
      let signatures = List.map snd random_values in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (
        SignatureM.Pop.aggregate_verify
          pks_with_proofs
          msg
          aggregated_signatures)

    let test_sign_and_verify_correct_signature_with_correct_pks_pops_and_incorrect_msg
        () =
      let msg = generate_random_bytes (1 + Random.int 512) in
      let msg' = generate_random_bytes (1 + Random.int 512) in

      let random_values =
        List.init
          (2 + Random.int 10)
          (fun _ ->
            let ikm = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let proof = SignatureM.Pop.pop_prove sk in
            let pk = SignatureM.derive_pk sk in
            let signature = SignatureM.Pop.sign sk msg in
            ((pk, proof), signature))
      in
      let pks_with_proofs = List.map fst random_values in
      let signatures = List.map snd random_values in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (
        not
          (SignatureM.Pop.aggregate_verify
             pks_with_proofs
             msg'
             aggregated_signatures))

    let test_sign_and_verify_incorrect_signature_with_correct_pks_pops_msg () =
      let msg = generate_random_bytes (1 + Random.int 512) in
      let random_values =
        List.init
          (2 + Random.int 10)
          (fun i ->
            let ikm = generate_random_bytes 32 in
            let ikm' = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let sk' = Bls12_381_signature.generate_sk ikm' in
            let proof = SignatureM.Pop.pop_prove sk in
            let pk = SignatureM.derive_pk sk in
            let signature =
              if i mod 2 = 0 then SignatureM.Pop.sign sk msg
              else SignatureM.Pop.sign sk' msg
            in
            ((pk, proof), signature))
      in
      let pks_with_proofs = List.map fst random_values in
      let signatures = List.map snd random_values in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (
        not
          (SignatureM.Pop.aggregate_verify
             pks_with_proofs
             msg
             aggregated_signatures))

    let test_sign_and_verify_correct_signature_with_some_correct_pks_and_incorrect_pops_and_msg
        () =
      let msg = generate_random_bytes (1 + Random.int 512) in
      let random_values =
        List.init
          (2 + Random.int 10)
          (fun i ->
            let ikm = generate_random_bytes 32 in
            let ikm' = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let sk' = Bls12_381_signature.generate_sk ikm' in
            let pk = SignatureM.derive_pk sk in
            let proof = SignatureM.Pop.pop_prove sk in
            let proof' =
              if i mod 2 = 0 then proof else SignatureM.Pop.pop_prove sk'
            in
            let signature = SignatureM.Pop.sign sk msg in
            ((pk, proof'), signature))
      in
      let pks_with_proofs = List.map fst random_values in
      let signatures = List.map snd random_values in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (
        not
          (SignatureM.Pop.aggregate_verify
             pks_with_proofs
             msg
             aggregated_signatures))

    let test_sign_and_verify_correct_signature_with_some_incorrect_pks_pops_and_correct_msg
        () =
      let msg = generate_random_bytes (1 + Random.int 512) in
      let random_values =
        List.init
          (2 + Random.int 10)
          (fun i ->
            let ikm = generate_random_bytes 32 in
            let ikm' = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let sk' = Bls12_381_signature.generate_sk ikm' in
            let proof = SignatureM.Pop.pop_prove sk in
            let proof' =
              if i mod 2 = 1 then proof else SignatureM.Pop.pop_prove sk'
            in
            let pk = SignatureM.derive_pk sk in
            let pk' =
              if i mod 2 = 0 then pk
              else SignatureM.(derive_pk (Bls12_381_signature.generate_sk ikm'))
            in
            let signature = SignatureM.Pop.sign sk msg in
            ((pk', proof'), signature))
      in
      let pks_with_proofs = List.map fst random_values in
      let signatures = List.map snd random_values in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (
        not
          (SignatureM.Pop.aggregate_verify
             pks_with_proofs
             msg
             aggregated_signatures))

    let test_sign_and_verify_correct_signature_with_some_incorrect_pks_and_correct_pops_and_msg
        () =
      let msg = generate_random_bytes (1 + Random.int 512) in
      let random_values =
        List.init
          (2 + Random.int 10)
          (fun i ->
            let ikm = generate_random_bytes 32 in
            let ikm' = generate_random_bytes 32 in
            let sk = Bls12_381_signature.generate_sk ikm in
            let proof = SignatureM.Pop.pop_prove sk in
            let pk = SignatureM.derive_pk sk in
            let pk' =
              if i mod 2 = 0 then pk
              else SignatureM.(derive_pk (Bls12_381_signature.generate_sk ikm'))
            in
            let signature = SignatureM.Pop.sign sk msg in
            ((pk', proof), signature))
      in
      let pks_with_proofs = List.map fst random_values in
      let signatures = List.map snd random_values in
      let aggregated_signatures =
        Option.get (SignatureM.aggregate_signature_opt signatures)
      in
      assert (
        not
          (SignatureM.Pop.aggregate_verify
             pks_with_proofs
             msg
             aggregated_signatures))

    let get_tests () =
      let open Alcotest in
      ( "Properties for Proof of possession",
        [
          test_case
            "Sign and verify correct signature with correct pks, pops and msg"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_correct_pks_pops_and_msg);
          test_case
            "Sign and verify incorrect signature with correct pks, pops and msg"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_incorrect_signature_with_correct_pks_pops_msg);
          test_case
            "Sign and verify correct signature with correct pks, pops and \
             incorrect msg"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_correct_pks_pops_and_incorrect_msg);
          test_case
            "Sign and verify correct signature with some incorrect pks and \
             correct pops and msg"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_some_incorrect_pks_and_correct_pops_and_msg);
          test_case
            "Sign and verify correct signature with some incorrect pks and \
             pops and correct msg"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_some_incorrect_pks_pops_and_correct_msg);
          test_case
            "Sign and verify correct signature with correct pks and some \
             incorrect pops and correct msg"
            `Quick
            (Utils.repeat
               10
               test_sign_and_verify_correct_signature_with_some_correct_pks_and_incorrect_pops_and_msg);
        ] )
  end
end

let () =
  let open Alcotest in
  let module MinPkTest = MakeAggregatedSignatureTest (Bls12_381_signature.MinPk) in
  let module MinSigTest =
    MakeAggregatedSignatureTest (Bls12_381_signature.MinSig) in
  let minpk_tests =
    [
      MinPkTest.BasicProperties.get_tests ();
      MinPkTest.AugProperties.get_tests ();
      MinPkTest.PopProperties.get_tests ();
    ]
  in
  let minpk_tests =
    List.map (fun (desc, tests) -> ("minPk " ^ desc, tests)) minpk_tests
  in
  let minsig_tests =
    [
      MinSigTest.BasicProperties.get_tests ();
      MinSigTest.AugProperties.get_tests ();
      MinSigTest.PopProperties.get_tests ();
    ]
  in
  let minsig_tests =
    List.map (fun (desc, tests) -> ("minSig " ^ desc, tests)) minsig_tests
  in
  let all_tests = List.concat [minpk_tests; minsig_tests] in
  run "BLS Aggregated Signature" all_tests
