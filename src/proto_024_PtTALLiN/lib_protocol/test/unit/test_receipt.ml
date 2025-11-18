(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    Component:  Protocol (token)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/unit/main.exe \
                  -- --file test_receipt.ml
    Subject:    Test receipt endocings.
*)

open Protocol
open Alpha_context
open Data_encoding

let random_amount () =
  match Tez.of_mutez (Int64.add 1L (Random.int64 100L)) with
  | None -> assert false
  | Some x -> x

(** Test that [decode (encode balance_updates) = balance_updates]. *)
let test_encodings balance =
  let open Lwt_result_syntax in
  Random.init 0 ;
  let am = random_amount () in
  let r1 = Receipt.item balance (Debited am) Protocol_migration in
  let r2 = Receipt.item balance (Credited am) Protocol_migration in
  let r3 = Receipt.item balance (Debited am) Subsidy in
  let r4 = Receipt.item balance (Credited am) Subsidy in
  let r5 = Receipt.item balance (Debited am) Simulation in
  let r6 = Receipt.item balance (Credited am) Simulation in
  let r7 = Receipt.item balance (Debited am) Block_application in
  let r8 = Receipt.item balance (Credited am) Block_application in
  let coded =
    Json.construct
      Receipt.balance_updates_encoding
      [r1; r2; r3; r4; r5; r6; r7; r8]
  in
  let decoded = Json.destruct Receipt.balance_updates_encoding coded in
  match decoded with
  | [r1'; r2'; r3'; r4'; r5'; r6'; r7'; r8'] ->
      assert (
        r1' = r1 && r2' = r2 && r3' = r3 && r4' = r4 && r5 = r5' && r6 = r6'
        && r7 = r7' && r8 = r8') ;
      return_unit
  | _ -> assert false

let test_encodings () =
  let open Lwt_result_syntax in
  let open Receipt in
  let pkh = Signature.Public_key_hash.zero in
  let pkh2, _pk, _sk = Signature.generate_key () in
  let staker1 = Receipt.frozen_shared_between_stakers ~delegate:pkh in
  let staker2 = Receipt.frozen_baker pkh in
  let staker3 =
    Receipt.frozen_single_staker ~staker:(Contract.Implicit pkh2) ~delegate:pkh
  in
  let* () = test_encodings (Contract (Contract.Implicit pkh)) in
  let* () = test_encodings Block_fees in
  let* () = test_encodings (Deposits staker1) in
  let* () = test_encodings (Deposits staker2) in
  let* () = test_encodings (Deposits staker3) in
  let* () = test_encodings Nonce_revelation_rewards in
  let* () = test_encodings Attesting_rewards in
  let* () = test_encodings Baking_rewards in
  let* () = test_encodings Baking_bonuses in
  let* () = test_encodings Storage_fees in
  let* () = test_encodings Double_signing_punishments in
  let* () =
    test_encodings
      (Lost_attesting_rewards (pkh, Random.bool (), Random.bool ()))
  in
  let* () = test_encodings Liquidity_baking_subsidies in
  let* () = test_encodings Burned in
  let* () = test_encodings (Commitments Blinded_public_key_hash.zero) in
  let* () = test_encodings Bootstrap in
  let* () = test_encodings Invoice in
  let* () = test_encodings Initial_commitments in
  let* () = test_encodings Minted in
  let* () = test_encodings Sc_rollup_refutation_punishments in
  test_encodings Sc_rollup_refutation_rewards

let tests = Tztest.[tztest "receipt - encoding" `Quick test_encodings]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("receipt", tests)] |> Lwt_main.run
