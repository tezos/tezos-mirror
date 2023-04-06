open Tezos_error_monad.Error_monad
module R = Rustzcash

let test_get_memo_size () =
  let open Core.Raw in
  let sk = List.nth Keys.xsks_raw 0 in
  let vk = Viewing_key.of_sk sk in
  let address = Viewing_key.(dummy_address ()) in
  let rcm = Rcm.random () in
  let memo = Bytes.empty in
  let esk = DH.esk_random () in
  let cv = Stdlib.Option.get (CV.of_bytes (Tezos_crypto.Hacl.Rand.gen 32)) in
  let cm = Commitment.of_bytes_exn (Tezos_crypto.Hacl.Rand.gen 32) in
  let epk = DH.derive_ephemeral address esk in
  let cipher = Ciphertext.encrypt 0L address vk rcm memo (cv, cm, epk) esk in
  assert (Ciphertext.get_memo_size cipher = 0)

let test_proof_raw () =
  let open Core.Raw in
  let module S = Storage.Make_Storage (Core.Raw) in
  let module T = S.Tree in
  let xsk = List.nth Keys.xsks_raw 0 in
  let vlue = 100L in
  let pos = 0L in
  let rcm = Rcm.random () in
  let xfvk = Viewing_key.of_sk xsk in
  let _, address = Viewing_key.(new_address xfvk default_index) in
  let nf = Nullifier.compute address xfvk ~amount:vlue rcm ~position:pos in
  let cm = Commitment.compute address ~amount:vlue rcm in
  let esk = DH.esk_random () in
  let epk = DH.derive_ephemeral address esk in
  let tree = T.add T.empty [cm] in
  let root_alt = T.get_root tree in
  let root = root_alt in
  let witness = Stdlib.Option.get @@ T.get_witness tree 0L in
  let tohash = Bytes.make 32 '1' in
  let hash =
    Tezos_crypto.Blake2B.(
      to_bytes (hash_bytes ~key:(Bytes.of_string "Sighash_Tezos") [tohash]))
  in
  let sighash = R.to_sighash hash in
  let ar = R.to_ar @@ R.generate_r () in
  let signature = R.spend_sig xsk.expsk.ask ar sighash in
  R.init_params () ;
  let ctx_prove = R.proving_ctx_init () in
  let ctx_verif = R.verification_ctx_init () in
  let cv_spend, rk, zkproof_spend =
    R.spend_proof
      ctx_prove
      xfvk.fvk.ak
      xsk.expsk.nsk
      address.diversifier
      rcm
      ar
      ~amount:vlue
      ~root
      ~witness
  in
  let check_spend =
    R.check_spend ctx_verif cv_spend root nf rk zkproof_spend signature sighash
  in
  assert check_spend ;
  let cv_output, zkproof_output =
    R.output_proof
      ctx_prove
      esk
      address.diversifier
      address.pkd
      rcm
      ~amount:vlue
  in
  let bindingsig = R.make_binding_sig ctx_prove ~balance:Int64.zero sighash in
  let check_output = R.check_output ctx_verif cv_output cm epk zkproof_output in
  assert check_output ;
  let final_check = R.final_check ctx_verif 0L bindingsig sighash in
  assert final_check ;
  let () = R.verification_ctx_free ctx_verif in
  let () = R.proving_ctx_free ctx_prove in
  ()

(* In this test 3 users 1,2,3 make several transactions. 1 gives 10 to himself
   out of thin air (i.e. from burnt tezos in real life). Then he gives 10 to B.
   Then B gives 5 to C, and burns 5 (i.e. unburns the 5 tez in real life).
   We do it "by hand" and using the function verify_update that will be in the
   smart contract and forge_transaction that will be used in the wallet *)
let test_full_transaction () =
  let module S = Storage.Make_Storage (Core.Raw) in
  let module T = S.Tree in
  let open Core.Raw in
  let key = "SaplingForTezosV1" in
  let xsk1 = List.nth Keys.xsks_raw 0 in
  let xsk2 = List.nth Keys.xsks_raw 1 in
  let xsk3 = List.nth Keys.xsks_raw 2 in
  let xfvk1 = Viewing_key.of_sk xsk1 in
  let xfvk2 = Viewing_key.of_sk xsk2 in
  let xfvk3 = Viewing_key.of_sk xsk3 in
  let _, addr1 = Viewing_key.(new_address xfvk1 default_index) in
  let _, addr2 = Viewing_key.(new_address xfvk2 default_index) in
  let _, addr3 = Viewing_key.(new_address xfvk3 default_index) in
  (* creation of the first note *)
  let rcm_1 = Rcm.random () in
  let cm_1 = Commitment.compute addr1 ~amount:10L rcm_1 in
  (* insertion of the first note in the merkle tree (ie. 1 created 10 out of thin air) *)
  let t_1 = T.add T.empty [cm_1] in

  (* ---------------1 transfers 10 to B--------------------- *)

  (* 1 gets pkd2 and diversifier_2 from 2 offline *)
  (* 1 computes and generates necessary values *)

  (* randomness for the created commitment which will belong to 2 *)
  (* commitment for 2 *)
  let rcm_2 = Rcm.random () in
  let cm_2 = Commitment.compute addr2 ~amount:10L rcm_2 in
  (* root of the current state of the blockchain *)
  let root_1 = T.get_root t_1 in
  (* witness to show that what we spend (cm_1) belongs to the
     current state of the blockchain in pos 0 *)
  let witness_1 = Stdlib.Option.get @@ T.get_witness t_1 0L in
  (* randomness to randomize the signature key *)
  let ar_1 = Proving.ar_random () in
  (* randomness for the key exchange done with 2 *)
  let esk_1 = DH.esk_random () in
  let epk_1 = DH.derive_ephemeral addr2 esk_1 in
  (* nullifier to 'destroy' cm_1 *)
  let nf_1 = Nullifier.compute addr1 xfvk1 ~amount:10L rcm_1 ~position:0L in
  R.init_params () ;
  (* Creation of a context to kep track of some info *)
  let ctx_prove_1 = R.proving_ctx_init () in
  (* Commitment value, randomised signature key, ZK proof that cm_1 is in the
     blockchain and has correct stuff *)
  let cv_spend_1, rk_1, zkproof_spend_1 =
    Proving.spend_proof
      ctx_prove_1
      xfvk1
      xsk1
      addr1
      rcm_1
      ar_1
      ~amount:10L
      ~root:root_1
      ~witness:witness_1
  in
  (* Commitment value of the created note, ZK proof that everything is correct *)
  let cv_output_1, zkproof_output_1 =
    Proving.output_proof ctx_prove_1 esk_1 addr2 rcm_2 ~amount:10L
  in
  (* Hash of the spend description *)
  let sighash_2 = UTXO.hash_input cv_spend_1 nf_1 rk_1 zkproof_spend_1 key in
  (* Signature of the input *)
  let signature_1 = R.spend_sig xsk1.expsk.ask ar_1 sighash_2 in
  (* encryption of the ciphertext *)
  let nonce_1 = Tezos_crypto.Crypto_box.random_nonce () in
  let plaintext =
    Data_encoding.Binary.to_bytes_exn
      Ciphertext.plaintext_encoding
      {
        diversifier = addr1.diversifier;
        amount = 10L;
        rcm = rcm_1;
        memo = Bytes.empty;
      }
  in
  let key_agreed = DH.symkey_sender esk_1 addr1.pkd in
  let ciphertext_1 =
    Tezos_crypto.Crypto_box.Secretbox.secretbox key_agreed plaintext nonce_1
  in
  (* Hash of the whole transaction *)
  let tohash_all =
    let open R in
    Bytes.concat
      Bytes.empty
      [
        of_cv cv_spend_1;
        of_nullifier nf_1;
        of_rk rk_1;
        of_spend_proof zkproof_spend_1;
        of_cv cv_output_1;
        of_commitment cm_2;
        of_epk epk_1;
        of_output_proof zkproof_output_1;
        ciphertext_1;
      ]
  in
  let sighash_1 =
    R.to_sighash
      Tezos_crypto.Blake2B.(
        to_bytes
          (hash_bytes ~key:(Bytes.of_string "SaplingForTezosV1") [tohash_all]))
  in
  (* Signature of (sum of inputs) - (sum of outputs) which is O here since 1
     sends the 10 he had to 2.
     The context ctx_prove has to be given in argument when we compute the
     output and spend proofs *)
  let bindingsig_1 = R.make_binding_sig ctx_prove_1 ~balance:0L sighash_1 in
  (* The context can now be freed *)
  let () = R.proving_ctx_free ctx_prove_1 in

  (* 1 puts on the blockchain:
     For spending cm_1 : cv_spend_1, root_1, nf_1, rk_1, zkproof_spend_1, signature_1
     For creating cm_2 : cv_output_1, cm_2, epk, zkproof_output_1,
     The encryption under key_agreed of diversifier_2 the value of the created
     note (10 here), rcm_2, and some optional natural language plaintext.
     For the overall balance : bindingsig_1
  *)

  (* The validator checks the following with the help of a context *)
  let ctx_verif_1 = R.verification_ctx_init () in
  let check_spend_1 =
    R.check_spend
      ctx_verif_1
      cv_spend_1
      root_1
      nf_1
      rk_1
      zkproof_spend_1
      signature_1
      sighash_2
  in
  assert check_spend_1 ;
  let check_output_1 =
    R.check_output ctx_verif_1 cv_output_1 cm_2 epk_1 zkproof_output_1
  in
  assert check_output_1 ;
  let final_check_1 = R.final_check ctx_verif_1 0L bindingsig_1 sighash_1 in
  assert final_check_1 ;
  (* free the context *)
  let () = R.verification_ctx_free ctx_verif_1 in
  (* The validator updates the merkle tree *)
  let t_2 = T.add t_1 [cm_2] in

  (* The validator insert the nullifier in a list, and check that is wasn't
     inserted before *)

  (* 2 transfers 5 to 3 and gets 5 from the shielded pool *)

  (* 2 gets the root and the witness for position 1 (ie. for cm_2) from the new merkle tree *)
  let root_2 = T.get_root t_2 in
  let witness_2 = Stdlib.Option.get @@ T.get_witness t_2 1L in
  (* 2 gets a pkd and d from 3 and computes as before the necessary stuff *)
  let ar_2 = Proving.ar_random () in
  let esk_2 = DH.esk_random () in
  let epk_2 = DH.derive_ephemeral addr3 esk_2 in
  let nf_2 = Nullifier.compute addr2 xfvk2 ~amount:10L rcm_2 ~position:1L in
  let ctx_prove_2 = R.proving_ctx_init () in
  let sighash_3 = R.to_sighash @@ Bytes.create 32 in
  let sighash_4 = R.to_sighash @@ Bytes.create 32 in
  (* Bytes.fill sighash_4 '1' ; *)
  let signature_2 = R.spend_sig xsk2.expsk.ask ar_2 sighash_4 in
  let rcm_3 = Rcm.random () in
  let cm_3 = Commitment.compute addr3 ~amount:5L rcm_3 in
  (* the shared secret is here unnecessary since in our example 3 won't spend money
     It has to be done in real though *)
  let cv_spend_2, rk_2, zkproof_spend_2 =
    R.spend_proof
      ctx_prove_2
      xfvk2.fvk.ak
      xsk2.expsk.nsk
      addr2.diversifier
      rcm_2
      ar_2
      ~amount:10L
      ~root:root_2
      ~witness:witness_2
  in
  let cv_output_2, zkproof_output_2 =
    R.output_proof
      ctx_prove_2
      esk_2
      addr3.diversifier
      addr3.pkd
      rcm_3
      ~amount:5L
  in
  (* Here we put 5, meaning 5 is getting out of the shielded pos_in
                                             A regular transaction will give 5 to an address *)
  let bindingsig_2 = R.make_binding_sig ctx_prove_2 ~balance:5L sighash_3 in
  let () = R.proving_ctx_free ctx_prove_2 in
  (* The validator checks and updates the state *)
  let ctx_verif_2 = R.verification_ctx_init () in
  let check_spend_2 =
    R.check_spend
      ctx_verif_2
      cv_spend_2
      root_2
      nf_2
      rk_2
      zkproof_spend_2
      signature_2
      sighash_4
  in
  assert check_spend_2 ;
  let check_output_2 =
    R.check_output ctx_verif_2 cv_output_2 cm_3 epk_2 zkproof_output_2
  in
  assert check_output_2 ;
  let final_check_2 = R.final_check ctx_verif_2 5L bindingsig_2 sighash_3 in
  assert final_check_2 ;
  let () = R.verification_ctx_free ctx_verif_2 in
  (* The validator updates the merkle tree *)
  let _t_3 = T.add t_2 [cm_3] in
  (* The validator inserts the nullifier in a list, and check that is wasn't inserted before *)
  ()

let test_forge () =
  let open Lwt_result_syntax in
  let module Core = Core.Client in
  let key = "SaplingForTezosV1" in
  let sk1 = List.nth Keys.xsks 0 in
  let sk2 = List.nth Keys.xsks 1 in
  let vk1 = Core.Viewing_key.of_sk sk1 in
  let vk2 = Core.Viewing_key.of_sk sk2 in
  let _, addr1 = Core.Viewing_key.(new_address vk1 default_index) in
  let _, addr2 = Core.Viewing_key.(new_address vk2 default_index) in
  let output = Forge.make_output addr1 10L Bytes.empty in
  let state = Storage.empty ~memo_size:0 in
  let t1 =
    Forge.forge_transaction [] [output] sk1 key ~bound_data:"pkh" state
  in
  let* _, state = Example.Validator.verify_update t1 state key in
  let forge_input_opt = Forge.Input.get state 0L vk1 in
  let _msg, forge_input = Stdlib.Option.get @@ forge_input_opt in
  let forge_output = Forge.make_output addr2 10L Bytes.empty in
  let transaction =
    Forge.forge_transaction
      [forge_input]
      [forge_output]
      sk1
      key
      ~bound_data:""
      state
  in
  let*! r = Example.Validator.verify_update transaction state key in
  match r with
  | Error l ->
      pp_print_trace Format.err_formatter l ;
      assert false
  | Ok (_, new_state) -> (
      let nf = (List.hd transaction.inputs).nf in
      assert (Storage.mem_nullifier new_state nf) ;
      (* Check that twice the same input fails*)
      let transaction =
        Forge.forge_transaction
          [forge_input; forge_input]
          [forge_output]
          sk1
          key
          ~bound_data:""
          state
      in
      let*! r = Example.Validator.verify_update transaction state key in
      match r with
      | Ok _ -> assert false
      | Error l ->
          assert (
            List.mem
              (Example.Validator.Input_spent (List.nth transaction.inputs 1))
              l) ;
          return_unit)

let test_simple_client () =
  let open Lwt_result_syntax in
  let open Example.Client in
  (*String that has to be equal to the one of the smart contract/verify_update*)
  let key = "SaplingForTezosV1" in
  let wa = new_wallet (List.nth Keys.xsks 0) in
  let wb = new_wallet (List.nth Keys.xsks 1) in
  (* create state with nothing *)
  let state = Storage.empty ~memo_size:2 in
  let addr_b = new_address wb in
  (*a gives 2 to b and 1 (of change) to himself with 3 transparent money*)
  let t1, wa = pay wa addr_b 2L ~memo:"t1" 3L state key in
  let* balance, state = Example.Validator.verify_update t1 state key in
  assert (balance = -3L) ;
  let wb = scan wb state in
  assert (wb.balance = 2L) ;
  let wa = scan wa state in
  assert (wa.balance = 1L) ;
  let addr_a = new_address wa in
  (* b gives 1 to a and 1 (of change) to himself with 2 transparent money*)
  let t2, wb = pay wb addr_a 1L ~memo:"t2" 2L state key in
  let* balance, state = Example.Validator.verify_update t2 state key in
  assert (balance = -2L) ;
  (* before scanning b still has 2*)
  assert (wb.balance = 2L) ;
  let wb = scan wb state in
  assert (wb.balance = 3L) ;
  let wa = scan wa state in
  assert (wa.balance = 2L) ;
  (*  b gives 1 to a with shielded money *)
  let addr_a = new_address wa in
  let t3, wb = pay wb addr_a 1L ~memo:"t3" 0L state key in
  let* balance, state = Example.Validator.verify_update t3 state key in
  assert (balance = 0L) ;
  let wb = scan wb state in
  assert (wb.balance = 2L) ;
  let wa = scan wa state in
  assert (wa.balance = 3L) ;
  (* a burns 1 shielded money *)
  let addr_a = new_address wa in
  let t4, wa = pay wa addr_a 0L ~memo:"t4" Int64.minus_one state key in
  assert (wa.balance = 2L) ;
  let* balance, state = Example.Validator.verify_update t4 state key in
  assert (balance = 1L) ;
  let l_a =
    scan_ovk
      (Obj.magic (Core.Viewing_key.ovk_of_xfvk wa.vk) : Core.Spending_key.ovk)
      state
  in
  let l_a_mess, _l_a_forge_input = List.split l_a in
  List.iter
    (fun x -> assert (List.mem (Bytes.of_string x) l_a_mess))
    ["t1"; "t4"] ;
  let l_b =
    scan_ovk
      (Obj.magic (Core.Viewing_key.ovk_of_xfvk wb.vk) : Core.Spending_key.ovk)
      state
  in
  let l_b_mess, _l_b_forge_input = List.split l_b in
  List.iter
    (fun x -> assert (List.mem (Bytes.of_string x) l_b_mess))
    ["t2"; "t3"] ;
  return_unit

(* We sign and verify with two different strings and check that it fails *)
let test_replay () =
  let open Lwt_result_syntax in
  let open Example.Client in
  let right_string = "SaplingForTezosV1" in
  let wrong_string = "SaplingForTezosVaezf1" in
  let wa = new_wallet (List.nth Keys.xsks 0) in
  let state = Storage.empty ~memo_size:2 in
  let addr = new_address wa in
  let t1, _ = pay wa addr 2L ~memo:"t1" 3L state right_string in
  let*! r = Example.Validator.verify_update t1 state wrong_string in
  match r with Error _ -> return_unit | _ -> assert false

(* A transaction is signed using "right" as bound_data and a different
   one with bound_data "wrong" and the same original signature is
   passed to verify_update. Verify_update should verify the signature
   and reject the modified transaction. *)
let test_wrong_bound_data () =
  let open Lwt_result_syntax in
  let open Example.Client in
  let key = "SaplingForTezosV1" in
  let wa = new_wallet (List.nth Keys.xsks 0) in
  let state = Storage.empty ~memo_size:2 in
  let addr = new_address wa in
  let t1, _ = pay wa addr 2L ~memo:"t1" ~bound_data:"right" 3L state key in
  let t1_wrong = {t1 with bound_data = "wrong"} in
  let*! r = Example.Validator.verify_update t1_wrong state key in
  match r with
  | Error [Example.Validator.Binding_sig_incorrect _] -> return_unit
  | _ -> assert false

let tests =
  [
    Alcotest_lwt.test_case_sync "test_get_memo_size" `Quick test_get_memo_size;
    Alcotest_lwt.test_case_sync "full_transaction" `Quick test_full_transaction;
    Alcotest_lwt.test_case_sync "proof_raw" `Quick test_proof_raw;
    Tztest.tztest "forge" `Quick test_forge;
    Tztest.tztest "simple_client" `Quick test_simple_client;
    Tztest.tztest "anti-replay" `Quick test_replay;
    Tztest.tztest "wrong_bound_data" `Quick test_wrong_bound_data;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "sapling" [("sapling", tests)] |> Lwt_main.run
