(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Crypto
    Invocation:   dune build @src/lib_crypto/runtest
    Dependencies: src/lib_crypto/test/timelock.ml
    Subject:      On timelock implementation
*)

(* 2048 bit RSA modulus challenge: https://en.wikipedia.org/wiki/RSA_numbers#RSA-617 *)
let default_challenge =
  "22701801293785014193580405120204586741061235962766583907094021879215171483119139894870133091111044901683400949483846818299518041763507948922590774925466088171879259465921026597046700449819899096862039460017743094473811056991294128542891880855362707407670722593737772666973440977361243336397308051763091506836310795312607239520365290032105848839507981452307299417185715796297454995023505316040919859193718023307414880446217922800831766040938656344571034778553457121080530736394535923932651866030515041060966437313323672831539323500067937107541955437362433248361242525945868802353916766181532375855504886901432221349733"

let test_raw_scenario time () =
  let open Timelock in
  (* Creator creating chest. *)
  let timelock_precomputed_tuple = precompute_timelock ~time () in
  let locked, proof =
    proof_of_vdf_tuple rsa2048 ~time timelock_precomputed_tuple
  in
  (* Not creator opening chest. *)
  assert (verify rsa2048 ~time locked proof) ;
  let proof_2 = unlock_and_prove rsa2048 ~time locked in
  assert (verify rsa2048 ~time locked proof_2) ;
  let sym_key_1 = timelock_proof_to_symmetric_key rsa2048 proof in
  let sym_key_2 = timelock_proof_to_symmetric_key rsa2048 proof_2 in
  assert (sym_key_1 = sym_key_2) ;
  let message = Bytes.of_string "rzersef" in
  let c = encrypt sym_key_2 message in
  let message_decrypted_opt = decrypt sym_key_2 c in
  match message_decrypted_opt with
  | None -> assert false
  | Some message_decrypted -> assert (message = message_decrypted)

let bench () =
  let open Timelock in
  let locked = gen_locked_value_unsafe rsa2048 in
  (* Corresponds to ~1s, increases linearly *)
  let time = 10_000 in
  let proof = unlock_and_prove rsa2048 ~time locked in
  let start_bench = Unix.gettimeofday () in
  for _i = 0 to 100 do
    let _ = prove rsa2048 ~time locked proof.vdf_tuple.unlocked_value in
    ()
  done ;
  let end_bench = Unix.gettimeofday () in
  Printf.printf "\n without secret: %f" (end_bench -. start_bench)

let test_high_level_scenario time () =
  let open Timelock in
  let payload = Bytes.of_string "zrethgfdsq" in
  let chest, chest_key_1 = create_chest_and_chest_key ~payload ~time () in
  let chest_key_2 = create_chest_key ~time chest in
  let opening_result_1 = open_chest chest chest_key_1 ~time in
  let opening_result_2 = open_chest chest chest_key_2 ~time in
  let expected_opening_result = Correct payload in
  assert (opening_result_1 = opening_result_2) ;
  assert (opening_result_1 = expected_opening_result)

let test_high_level_negative () =
  let open Timelock in
  let payload = Bytes.of_string "fdgfnhfd" and time = 10 in
  let chest, chest_key = create_chest_and_chest_key ~payload ~time () in
  (* Opening Bogus *)
  (* The opener, opens to garbage *)
  let wrong_time = 1000 in
  let proof_wrong =
    unlock_and_prove rsa2048 ~time:wrong_time chest.locked_value
  in
  let unlocked_wrong = proof_wrong.vdf_tuple.unlocked_value in
  let vdf_wrong = proof_wrong.vdf_tuple.vdf_proof in
  let proof_incorrect_unlocked =
    {
      vdf_tuple = {chest_key.vdf_tuple with unlocked_value = unlocked_wrong};
      nonce = chest_key.nonce;
    }
  in
  let proof_incorrect_vdf =
    {
      vdf_tuple = {chest_key.vdf_tuple with vdf_proof = vdf_wrong};
      nonce = chest_key.nonce;
    }
  in
  let proof_incorrect_rand =
    {vdf_tuple = chest_key.vdf_tuple; nonce = Z.zero}
  in
  let opening_result_wrong_expected = Bogus_opening in
  let opening_result_wrong = open_chest chest ~time proof_incorrect_unlocked in
  assert (opening_result_wrong = opening_result_wrong_expected) ;
  let opening_result_wrong = open_chest chest ~time proof_incorrect_vdf in
  assert (opening_result_wrong = opening_result_wrong_expected) ;
  let opening_result_wrong = open_chest chest ~time proof_incorrect_rand in
  assert (opening_result_wrong = opening_result_wrong_expected) ;
  ()

let test_low_level_negative () =
  let open Timelock in
  let payload = Bytes.of_string "fdgfnhfd" and time = 10 in
  let chest, chest_key = create_chest_and_chest_key ~payload ~time () in
  let proof = unlock_and_prove chest.rsa_public ~time chest.locked_value in
  let incorrect_proofs =
    let open Internal_for_tests in
    let g, g' =
      let locked = locked_value_to_z chest_key.vdf_tuple.locked_value in
      (locked |> Z.to_string, Z.(locked + one |> to_string))
    in
    let c, c' =
      let challenge = unlocked_value_to_z chest_key.vdf_tuple.unlocked_value in
      (challenge |> Z.to_string, Z.(challenge + one |> to_string))
    in
    let pi, pi' =
      let proof = vdf_proof_to_z chest_key.vdf_tuple.vdf_proof in
      (proof |> Z.to_string, Z.(proof + one |> to_string))
    in
    [
      {vdf_tuple = to_vdf_tuple_unsafe g' c pi; nonce = chest_key.nonce};
      {vdf_tuple = to_vdf_tuple_unsafe g c' pi; nonce = chest_key.nonce};
      {vdf_tuple = to_vdf_tuple_unsafe g c pi'; nonce = chest_key.nonce};
      {vdf_tuple = chest_key.vdf_tuple; nonce = Z.(chest_key.nonce + one)};
      {vdf_tuple = chest_key.vdf_tuple; nonce = proof.nonce};
    ]
  in
  assert (
    List.for_all
      (fun proof -> open_chest chest ~time proof = Bogus_opening)
      incorrect_proofs)

let test_sampler_and_get_plaintext_size () =
  let open Timelock in
  let rng_state = Random.get_state () in
  let time = Int.shift_left 1 10 in
  let chest, chest_key = chest_sampler ~rng_state ~plaintext_size:100 ~time in
  assert (get_plaintext_size chest = 100) ;
  match open_chest chest chest_key ~time with
  | Correct _ -> ()
  | _ -> assert false

(* Unit test checking that the memory efficient Wesolowski proof generation is
   correct *)
(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5629
   Turn this test into a PBT. *)
let test_wesolowski () =
  let open Timelock in
  let open Internal_for_tests in
  let payload = Bytes.of_string "fdgfnhfd" and time = 10 in
  let chest, chest_key = create_chest_and_chest_key ~payload ~time () in
  let rsa, g, c, pi =
    ( rsa_public_to_z chest.rsa_public,
      locked_value_to_z chest_key.vdf_tuple.locked_value,
      unlocked_value_to_z chest_key.vdf_tuple.unlocked_value,
      vdf_proof_to_z chest_key.vdf_tuple.vdf_proof )
  in
  (* Memory intensive proof generation *)
  let pi_high_memory =
    let l =
      hash_to_prime
        chest.rsa_public
        ~time
        chest.locked_value
        chest_key.vdf_tuple.unlocked_value
    in
    let exponent = Z.(pow (of_int 2) time / l) in
    Z.powm g exponent rsa
  in
  let tuple_high_memory =
    to_vdf_tuple_unsafe
      (Z.to_string g)
      (Z.to_string c)
      (Z.to_string pi_high_memory)
  in
  assert (Z.(equal pi pi_high_memory)) ;
  assert (verify_wesolowski chest.rsa_public ~time tuple_high_memory) ;
  ()

let tests =
  [
    ( "timelock",
      [
        ("raw scenario - precomputed 1000", `Quick, test_raw_scenario 1000);
        ("raw scenario - precomputed 30_000", `Quick, test_raw_scenario 30000);
        ("raw scenario - short", `Quick, test_raw_scenario 5);
        ("raw scenario - long", `Slow, test_raw_scenario 100000);
        ("high level scenario - short", `Quick, test_high_level_scenario 1000);
        ("high level scenario - long", `Quick, test_high_level_scenario 3456);
        ("bench", `Slow, bench);
        ("negative test - high level", `Quick, test_high_level_negative);
        ("negative test - low level", `Quick, test_low_level_negative);
        ("sampler test", `Quick, test_sampler_and_get_plaintext_size);
        ("test wesolowski", `Quick, test_wesolowski);
      ] );
  ]
