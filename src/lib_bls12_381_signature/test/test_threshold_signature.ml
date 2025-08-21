(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
   -------
   Component:    BLS12_381 signature
   Invocation:   dune exec src/lib_bls12_381_signature/test/main.exe \
                  -- --file test_threshold_signature.ml
   Subject:      Test BLS12_381 threshold signature
*)

open Utils

let () = Random.self_init ()

module type SIGNATURE_INSTANTIATION = module type of Bls12_381_signature.MinPk

module MakeThresholdSignatureTest (SignatureM : SIGNATURE_INSTANTIATION) =
struct
  let possible_values_for_n = List.init 5 (fun i -> i + 2)

  (* The same function as in lib_crypto/bls.ml but with different
     source of randomness. *)
  let share_secret_key secret_key ~m ~n =
    if not (1 < m && m <= n) then
      raise
        (Invalid_argument
           (Printf.sprintf "Invalid parameters for N = %d and M = %d" n m)) ;

    let s_0 = secret_key in
    (* Secret polynomial:
       s(x) = s_0 + s_1 * x + s_2 * x^2 + .. + s_{m-1} * x^{m-1}
       - s_0 is a master secret key
       - s_1, .., s_{m-1} are random secret keys *)
    let poly_s =
      s_0
      :: List.init (m - 1) (fun _i ->
             Bls12_381_signature.generate_sk @@ generate_random_bytes 32)
    in
    Bls12_381_signature.share_secret_key poly_s ~n

  let generate_threshold_key sk ~m ~n =
    let pk = SignatureM.derive_pk sk in
    let secret_shares = share_secret_key sk ~n ~m in
    (pk, secret_shares)

  let shuffle_list l =
    let a = Array.of_list l in
    Array.shuffle ~rand:Random.int a ;
    Array.to_list a

  let generate_keys ~m ~n =
    let master_sk =
      Bls12_381_signature.generate_sk @@ generate_random_bytes 32
    in
    let group_pk, all_signers = generate_threshold_key master_sk ~m ~n in
    let all_signers = shuffle_list all_signers in
    (master_sk, group_pk, all_signers)

  (* k is a number of signature shares passed for threshold_signature_opt *)
  let sign_and_verify_signature_with_correct_pk_and_msg ~is_valid ~k ~m ~n =
    let master_sk, group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signature =
      let signers = List.filteri (fun i _ -> i < k) all_signers in
      List.map (fun (id, sk) -> (id, SignatureM.Pop.sign sk msg)) signers
      |> SignatureM.threshold_signature_opt |> Option.get
    in
    let signature_expected = SignatureM.Pop.sign master_sk msg in
    if is_valid then (
      assert (signature = signature_expected) ;
      assert (SignatureM.Pop.verify group_pk msg signature))
    else (
      assert (not (signature = signature_expected)) ;
      assert (not (SignatureM.Pop.verify group_pk msg signature)))

  (* Sufficient number of signature shares needed to recover
     a signature: 1 < m ≤ k ≤ n *)
  let test_sign_and_verify_correct_number_of_shares () =
    List.iter
      (fun n ->
        List.iter
          (fun m ->
            List.iter
              (fun k ->
                sign_and_verify_signature_with_correct_pk_and_msg
                  ~is_valid:true
                  ~n
                  ~m
                  ~k)
              (List.init (n - m + 1) (fun i -> i + m)))
          (List.init (n - 1) (fun i -> i + 2)))
      possible_values_for_n

  (* Insufficient number of signature shares to recover a signature:
     1 < k < m <= n *)
  let test_sign_and_verify_incorrect_number_of_shares () =
    List.iter
      (fun n ->
        List.iter
          (fun m ->
            List.iter
              (fun k ->
                sign_and_verify_signature_with_correct_pk_and_msg
                  ~is_valid:false
                  ~n
                  ~m
                  ~k)
              (List.init (m - 2) (fun i -> i + 2)))
          (List.init (n - 1) (fun i -> i + 2)))
      possible_values_for_n

  let test_sign_for_all_k f =
    List.iter
      (fun n ->
        List.iter
          (fun m ->
            List.iter (fun k -> f ~n ~m ~k) (List.init (n - 2) (fun i -> i + 2)))
          (List.init (n - 1) (fun i -> i + 2)))
      possible_values_for_n

  let test_sign_for_all_k_not_equal_n f =
    List.iter
      (fun n ->
        if n <= 2 then ()
        else
          List.iter
            (fun m ->
              List.iter
                (fun k -> f ~n ~m ~k)
                (List.init (n - 3) (fun i -> i + 2)))
            (List.init (n - 1) (fun i -> i + 2)))
      possible_values_for_n

  (* (id, signature_id) is repeated twice *)
  let test_sign_repeated_signature ~n ~m ~k =
    let _master_sk, _group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signers = List.hd signers :: signers in
    let signature =
      List.map (fun (id, sk) -> (id, SignatureM.Pop.sign sk msg)) signers
      |> SignatureM.threshold_signature_opt
    in
    assert (Option.is_none signature)

  let test_sign_with_repeated_signature () =
    test_sign_for_all_k test_sign_repeated_signature

  (* id = 0 *)
  let test_sign_id_zero ~n ~m ~k =
    let _master_sk, _group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signature =
      List.mapi
        (fun i (id, sk) ->
          let signature_id = SignatureM.Pop.sign sk msg in
          if i = 1 then (0, signature_id) else (id, signature_id))
        signers
      |> SignatureM.threshold_signature_opt
    in
    assert (Option.is_none signature)

  let test_sign_with_id_zero () = test_sign_for_all_k test_sign_id_zero

  (* id is repeated twice *)
  let test_sign_repeated_id ~n ~m ~k =
    let _master_sk, _group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signature =
      List.mapi
        (fun i (id, sk) ->
          let signature_id = SignatureM.Pop.sign sk msg in
          if i = 1 then (fst @@ List.hd signers, signature_id)
          else (id, signature_id))
        signers
      |> SignatureM.threshold_signature_opt
    in
    assert (Option.is_none signature)

  let test_sign_with_repeated_id () = test_sign_for_all_k test_sign_repeated_id

  (* incorrect id but within range [1; n] *)
  let test_sign_and_verify_incorrect_id_within_range ~n ~m ~k =
    let master_sk, group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signature =
      List.mapi
        (fun i (id, sk) ->
          let signature_id = SignatureM.Pop.sign sk msg in
          if i = 1 then (fst @@ List.nth all_signers k, signature_id)
          else (id, signature_id))
        signers
      |> SignatureM.threshold_signature_opt |> Option.get
    in
    let signature_expected = SignatureM.Pop.sign master_sk msg in
    assert (not (signature = signature_expected)) ;
    assert (not (SignatureM.Pop.verify group_pk msg signature))

  let test_sign_and_verify_with_incorrect_id_within_range () =
    test_sign_for_all_k_not_equal_n
      test_sign_and_verify_incorrect_id_within_range

  (* incorrect id but not in range [1; n] *)
  let test_sign_and_verify_incorrect_id_not_in_range ~n ~m ~k =
    let master_sk, group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signature =
      List.mapi
        (fun i (id, sk) ->
          let signature_id = SignatureM.Pop.sign sk msg in
          if i = 1 then (n + 10, signature_id) else (id, signature_id))
        signers
      |> SignatureM.threshold_signature_opt |> Option.get
    in
    let signature_expected = SignatureM.Pop.sign master_sk msg in
    assert (not (signature = signature_expected)) ;
    assert (not (SignatureM.Pop.verify group_pk msg signature))

  let test_sign_and_verify_with_incorrect_id_not_in_range () =
    test_sign_for_all_k test_sign_and_verify_incorrect_id_not_in_range

  (* signature_id is repeated twice *)
  let test_sign_and_verify_repeated_signature_id ~n ~m ~k =
    let master_sk, group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signature =
      List.mapi
        (fun i (id, sk) ->
          let signature_id = SignatureM.Pop.sign sk msg in
          if i = 1 then (id, SignatureM.Pop.sign (snd @@ List.hd signers) msg)
          else (id, signature_id))
        signers
      |> SignatureM.threshold_signature_opt |> Option.get
    in
    let signature_expected = SignatureM.Pop.sign master_sk msg in
    assert (not (signature = signature_expected)) ;
    assert (not (SignatureM.Pop.verify group_pk msg signature))

  let test_sign_and_verify_with_repeated_signature_id () =
    test_sign_for_all_k test_sign_and_verify_repeated_signature_id

  (* signature_id is incorrect *)
  let test_sign_and_verify_incorrect_signature_id ~n ~m ~k =
    let master_sk, group_pk, all_signers = generate_keys ~m ~n in
    let msg = generate_random_bytes (1 + Random.int 512) in
    let msg' = generate_random_bytes (1 + Random.int 512) in
    let signers = List.filteri (fun i _ -> i < k) all_signers in
    let signature =
      List.mapi
        (fun i (id, sk) ->
          let signature_id = SignatureM.Pop.sign sk msg in
          if i = 1 then (id, SignatureM.Pop.sign sk msg') else (id, signature_id))
        signers
      |> SignatureM.threshold_signature_opt |> Option.get
    in
    let signature_expected = SignatureM.Pop.sign master_sk msg in
    assert (not (signature = signature_expected)) ;
    assert (not (SignatureM.Pop.verify group_pk msg signature))

  let test_sign_and_verify_with_incorrect_signature_id () =
    test_sign_for_all_k test_sign_and_verify_incorrect_signature_id

  let get_tests () =
    let open Alcotest in
    List.map
      (fun (name, test_f) -> test_case name `Quick (Utils.repeat 10 test_f))
      [
        ( "Sign and verify with correct number of shares",
          test_sign_and_verify_correct_number_of_shares );
        ( "Sign and verify with incorrect number of shares",
          test_sign_and_verify_incorrect_number_of_shares );
        ( "Sign with repeated (id, signature_id)",
          test_sign_with_repeated_signature );
        ("Sign with identifier equal to 0", test_sign_with_id_zero);
        ("Sign with repeated identifier", test_sign_with_repeated_id);
        ( "Sign and verify with incorrect identifier but within range [1; n]",
          test_sign_and_verify_with_incorrect_id_within_range );
        ( "Sign and verify with incorrect identifier but not in range [1; n]",
          test_sign_and_verify_with_incorrect_id_not_in_range );
        ( "Sign and verify with repeated signature_id",
          test_sign_and_verify_with_repeated_signature_id );
        ( "Sign and verify with incorrect signature_id",
          test_sign_and_verify_with_incorrect_signature_id );
      ]
end

let () =
  let open Alcotest in
  let module MinPkTest = MakeThresholdSignatureTest (Bls12_381_signature.MinPk) in
  let module MinSigTest = MakeThresholdSignatureTest (Bls12_381_signature.MinSig) in
  let minpk_tests = [MinPkTest.get_tests ()] in
  let minpk_tests = List.map (fun tests -> ("minPk ", tests)) minpk_tests in
  let minsig_tests = [MinSigTest.get_tests ()] in
  let minsig_tests = List.map (fun tests -> ("minSig ", tests)) minsig_tests in
  let all_tests = List.concat [minpk_tests; minsig_tests] in
  run ~__FILE__ "BLS Threshold Signature" all_tests
