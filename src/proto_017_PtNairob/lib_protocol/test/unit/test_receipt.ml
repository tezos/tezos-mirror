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
    Invocation: dune exec src/proto_017_PtNairob/lib_protocol/test/unit/main.exe \
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
  Random.init 0 ;
  let am = random_amount () in
  let r1 = Receipt.(balance, Debited am, Protocol_migration) in
  let r2 = Receipt.(balance, Credited am, Protocol_migration) in
  let r3 = Receipt.(balance, Debited am, Subsidy) in
  let r4 = Receipt.(balance, Credited am, Subsidy) in
  let r5 = Receipt.(balance, Debited am, Simulation) in
  let r6 = Receipt.(balance, Credited am, Simulation) in
  let r7 = Receipt.(balance, Debited am, Block_application) in
  let r8 = Receipt.(balance, Credited am, Block_application) in
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
  let open Receipt in
  let pkh = Signature.Public_key_hash.zero in
  test_encodings (Contract (Contract.Implicit pkh)) >>=? fun () ->
  test_encodings Block_fees >>=? fun () ->
  test_encodings (Deposits pkh) >>=? fun () ->
  test_encodings Nonce_revelation_rewards >>=? fun () ->
  test_encodings Double_signing_evidence_rewards >>=? fun () ->
  test_encodings Endorsing_rewards >>=? fun () ->
  test_encodings Baking_rewards >>=? fun () ->
  test_encodings Baking_bonuses >>=? fun () ->
  test_encodings Storage_fees >>=? fun () ->
  test_encodings Double_signing_punishments >>=? fun () ->
  test_encodings (Lost_endorsing_rewards (pkh, Random.bool (), Random.bool ()))
  >>=? fun () ->
  test_encodings Liquidity_baking_subsidies >>=? fun () ->
  test_encodings Burned >>=? fun () ->
  test_encodings (Commitments Blinded_public_key_hash.zero) >>=? fun () ->
  test_encodings Bootstrap >>=? fun () ->
  test_encodings Invoice >>=? fun () ->
  test_encodings Initial_commitments >>=? fun () ->
  test_encodings Minted >>=? fun () ->
  test_encodings Sc_rollup_refutation_punishments >>=? fun () ->
  test_encodings Sc_rollup_refutation_rewards

let tests = Tztest.[tztest "receipt - encoding" `Quick test_encodings]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("receipt", tests)] |> Lwt_main.run
