(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Protocol Library
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/pbt/main.exe \
                  -- --file test_balance_updates_encoding.ml
    Subject:      Encoding for balance_updates
*)

open Protocol
open QCheck2
open Qcheck2_helpers

(** {2 Generators}  *)
let contract =
  let pkh, _, _ = Signature.generate_key () in
  Receipt_repr.Contract (Contract_repr.Implicit pkh)

let deposits =
  let pkh, _, _ = Signature.generate_key () in
  Receipt_repr.Deposits pkh

let lost_endorsing_rewards =
  let pkh, _, _ = Signature.generate_key () in
  Receipt_repr.Lost_attesting_rewards (pkh, false, false)

let unstaked_deposits =
  let pkh, _, _ = Signature.generate_key () in
  Receipt_repr.Unstaked_deposits (pkh, Cycle_repr.root)

let commitments = Receipt_repr.Commitments Blinded_public_key_hash.zero

let frozen_bonds =
  let pkh, _, _ = Signature.generate_key () in
  let bond_id =
    Bond_id_repr.Sc_rollup_bond_id
      (Sc_rollup_repr.Address.of_b58check_exn
         "sr1JPVatbbPoGp4vb6VfQ1jzEPMrYFcKq6VG")
  in
  Receipt_repr.Frozen_bonds (Contract_repr.Implicit pkh, bond_id)

let generate_balance =
  let open Gen in
  let open Receipt_repr in
  oneofl
    [
      contract;
      Block_fees;
      deposits;
      unstaked_deposits;
      Nonce_revelation_rewards;
      Attesting_rewards;
      Baking_rewards;
      Baking_bonuses;
      Storage_fees;
      Double_signing_punishments;
      lost_endorsing_rewards;
      Liquidity_baking_subsidies;
      Burned;
      commitments;
      Bootstrap;
      Invoice;
      Initial_commitments;
      Minted;
      frozen_bonds;
      Sc_rollup_refutation_punishments;
      Sc_rollup_refutation_rewards;
    ]

let generate_balance_update =
  let open Gen in
  let open Receipt_repr in
  let* i = big_nat in
  let tez = Tez_repr.of_mutez_exn (Int64.of_int i) in
  oneofl [Debited tez; Credited tez]

let generate_update_origin =
  let open Gen in
  let open Receipt_repr in
  oneofl [Block_application; Protocol_migration; Subsidy; Simulation]

let generate_balance_updates : Receipt_repr.balance_updates Gen.t =
  Gen.list
    (Gen.tup3 generate_balance generate_balance_update generate_update_origin)

(** {2 Tests} *)
let eq balance_updates1 balance_updates2 =
  let open Receipt_repr in
  let res =
    List.for_all2
      ~when_different_lengths:()
      (fun (b1, bu1, uo1) (b2, bu2, uo2) ->
        compare b1 b2 = 0
        && (match (bu1, bu2) with
           | Debited tz1, Debited tz2 | Credited tz1, Credited tz2 ->
               Tez_repr.equal tz1 tz2
           | Debited tz1, Credited tz2 | Credited tz1, Debited tz2 ->
               Tez_repr.(equal tz1 zero) && Tez_repr.(equal tz2 zero))
        && compare_update_origin uo1 uo2 = 0)
      balance_updates1
      balance_updates2
  in
  match res with Ok b -> b | Error _ -> false

let test_balance_updates encoding =
  let gen = generate_balance_updates in
  test_roundtrip ~count:2000 ~title:"Balance_updates" ~gen ~eq encoding

let test_binary_balance_updates encoding1 encoding2 =
  let gen = generate_balance_updates in
  test_roundtrip_through_binary
    ~count:2000
    ~title:"Balance_updates"
    ~gen
    ~eq
    encoding1
    encoding2

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": Operation_encoding")
    [
      ( "roundtrip",
        qcheck_wrap [test_balance_updates Receipt_repr.balance_updates_encoding]
      );
      ( "legacy : roundtrip",
        qcheck_wrap
          [
            test_balance_updates
              Receipt_repr.balance_updates_encoding_with_legacy_attestation_name;
          ] );
      ( "roundtrip 2 encodings",
        qcheck_wrap
          [
            test_binary_balance_updates
              Receipt_repr.balance_updates_encoding
              Receipt_repr.balance_updates_encoding_with_legacy_attestation_name;
          ] );
    ]
