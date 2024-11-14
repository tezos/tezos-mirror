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
    Invocation:   dune exec src/lib_crypto/test/main.exe
    Dependencies: src/lib_crypto/test/timelock_legacy.ml
    Subject:      On timelock implementation
*)

let test_raw_scenario time () =
  let open Timelock_legacy in
  let public, secret = gen_rsa_keys () in
  let locked_value = gen_locked_value public in
  let unlocked, proof_1 =
    unlock_and_prove_with_secret secret ~time locked_value
  in
  let same_unlocked, proof_2 =
    unlock_and_prove_without_secret public ~time locked_value
  in
  assert (proof_1 = proof_2) ;
  let sym_key = unlocked_value_to_symmetric_key unlocked in
  let message = Bytes.of_string "rzersef" in
  let c = encrypt sym_key message in
  let message_decrypted_opt = decrypt sym_key c in
  match message_decrypted_opt with
  | None -> assert false
  | Some message_decrypted ->
      assert (message = message_decrypted) ;
      assert (unlocked = same_unlocked) ;
      assert (verify_timelock public ~time locked_value unlocked proof_1) ;
      assert (verify_timelock public ~time locked_value unlocked proof_2)

let bench () =
  let open Timelock_legacy in
  let time = 10_000 in
  let public, secret = gen_rsa_keys () in
  let locked_value = gen_locked_value public in
  let unlocked_value = unlock_with_secret secret ~time locked_value in
  let start = Unix.gettimeofday () in
  for _i = 0 to 100 do
    let _ = prove_with_secret secret ~time locked_value unlocked_value in
    ()
  done ;
  let ende = Unix.gettimeofday () in
  Printf.printf "\n with secret: %f" (ende -. start) ;
  let start = Unix.gettimeofday () in
  for _i = 0 to 100 do
    let _ = prove_without_secret public ~time locked_value unlocked_value in
    ()
  done ;
  let ende = Unix.gettimeofday () in
  Printf.printf "\n without secret: %f" (ende -. start)

let test_high_level_scenario () =
  let open Timelock_legacy in
  let payload = Bytes.of_string "zrethgfdsq" and time = 3456 in
  let chest, chest_key_1 = create_chest_and_chest_key ~payload ~time in
  let chest_key_2 = create_chest_key ~time chest in
  let opening_result_1 = open_chest chest chest_key_1 ~time in
  let opening_result_2 = open_chest chest chest_key_2 ~time in
  let expected_opening_result = Correct payload in
  assert (opening_result_1 = opening_result_2) ;
  assert (opening_result_1 = expected_opening_result)

let test_negative () =
  let open Timelock_legacy in
  let payload = Bytes.of_string "fdgfnhfd" and time = 10 in
  let wrong_time = 1000 in
  let rsa_public, rsa_secret = gen_rsa_keys () in
  let locked_value = gen_locked_value rsa_public in
  let sym_key =
    locked_value_to_symmetric_key_with_secret rsa_secret ~time locked_value
  in
  let ciphertext = encrypt sym_key payload in
  let chest = {locked_value; rsa_public; ciphertext} in
  (* the opener does garbage*)
  let unlocked_value_wrong, proof_wrong =
    unlock_and_prove_without_secret
      rsa_public
      ~time:wrong_time
      chest.locked_value
  in
  let opening_result_wrong_1 =
    open_chest
      chest
      ~time
      {unlocked_value = unlocked_value_wrong; proof = proof_wrong}
  in
  let opening_result_wrong_expected_1 = Bogus_opening in
  assert (opening_result_wrong_1 = opening_result_wrong_expected_1) ;
  (* the chest creator does garbage. We need to go trough a wrong sym_key. *)
  let sym_key_wrong =
    locked_value_to_symmetric_key_with_secret
      rsa_secret
      ~time:wrong_time
      locked_value
  in
  let ciphertext_wrong = encrypt sym_key_wrong payload in
  let chest_wrong = {locked_value; rsa_public; ciphertext = ciphertext_wrong} in
  let opening_result_wrong_2 =
    open_chest chest_wrong (create_chest_key ~time chest) ~time
  in
  let opening_result_wrong_expected_2 = Bogus_cipher in
  assert (opening_result_wrong_2 = opening_result_wrong_expected_2) ;
  ()

let test_sampler_and_get_plaintext_size () =
  let open Timelock_legacy in
  let rng_state = Random.get_state () in
  (* used to check determinism*)
  let rng_state_same = Random.get_state () in
  let time = 1000 in
  let chest, chest_key = chest_sampler ~rng_state ~plaintext_size:100 ~time in
  assert (get_plaintext_size chest = 100) ;
  let chest_same, chest_key_same =
    chest_sampler ~rng_state:rng_state_same ~plaintext_size:100 ~time
  in
  (* Check determinism*)
  assert (chest = chest_same && chest_key = chest_key_same) ;
  match open_chest chest chest_key ~time with
  | Correct _ -> ()
  | _ -> assert false

let tests =
  [
    ( "timelock legacy",
      [
        ("timelock legacy: raw scenario short", `Quick, test_raw_scenario 5);
        ("timelock legacy: raw scenario", `Quick, test_raw_scenario 1000);
        ("timelock legacy: raw scenario long", `Slow, test_raw_scenario 100000);
        ( "timelock legacy: high level scenario",
          `Quick,
          test_high_level_scenario );
        ("timelock legacy: bench", `Slow, bench);
        ("timelock legacy: negative test", `Quick, test_negative);
        ( "timelock legacy: samplertest",
          `Quick,
          test_sampler_and_get_plaintext_size );
      ] );
  ]
