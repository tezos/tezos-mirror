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
let default_delegate =
  let pkh, _, _ = Signature.generate_key () in
  pkh

let default_contract =
  let pkh, _, _ = Signature.generate_key () in
  Contract_repr.Implicit pkh

let contract = Receipt_repr.Contract default_contract

let unstaked_frozen_staker =
  let open Gen in
  oneofl
    [
      Unstaked_frozen_staker_repr.Shared default_delegate;
      Unstaked_frozen_staker_repr.Single
        (Contract_repr.Implicit default_delegate, default_delegate);
      Unstaked_frozen_staker_repr.Single (default_contract, default_delegate);
    ]

let frozen_staker =
  let open Gen in
  oneofl
    [
      Frozen_staker_repr.shared_between_stakers ~delegate:default_delegate;
      Frozen_staker_repr.baker default_delegate;
      Frozen_staker_repr.single_staker
        ~staker:default_contract
        ~delegate:default_delegate;
      Frozen_staker_repr.baker_edge default_delegate;
    ]

let deposits =
  let open Gen in
  let+ staker = frozen_staker in
  Receipt_repr.Deposits staker

let lost_attesting_rewards =
  let pkh, _, _ = Signature.generate_key () in
  Receipt_repr.Lost_attesting_rewards (pkh, false, false)

let unstaked_deposits =
  let open Gen in
  let+ staker = unstaked_frozen_staker in
  Receipt_repr.Unstaked_deposits (staker, Cycle_repr.root)

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
  oneof
    [
      return contract;
      return Block_fees;
      deposits;
      unstaked_deposits;
      return Nonce_revelation_rewards;
      return Attesting_rewards;
      return Baking_rewards;
      return Baking_bonuses;
      return Storage_fees;
      return Double_signing_punishments;
      return lost_attesting_rewards;
      return Liquidity_baking_subsidies;
      return Burned;
      return commitments;
      return Bootstrap;
      return Invoice;
      return Initial_commitments;
      return Minted;
      return frozen_bonds;
      return Sc_rollup_refutation_punishments;
      return Sc_rollup_refutation_rewards;
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
    (Gen.map
       (fun (balance, balance_update, update_origin) ->
         Receipt_repr.item balance balance_update update_origin)
       (Gen.tup3
          generate_balance
          generate_balance_update
          generate_update_origin))

(** {2 Tests} *)
let eq balance_updates1 balance_updates2 =
  let open Receipt_repr in
  let res =
    List.for_all2
      ~when_different_lengths:()
      (fun (Balance_update_item (b1, bu1, uo1))
           (Balance_update_item (b2, bu2, uo2))
         ->
        (let token1 = token_of_balance b1 in
         let token2 = token_of_balance b2 in
         match Token.eq token1 token2 with
         | None -> false
         | Some Refl -> (
             compare b1 b2 = 0
             &&
             match (bu1, bu2) with
             | Debited tz1, Debited tz2 | Credited tz1, Credited tz2 ->
                 Token.equal token1 tz1 tz2
             | Debited tz1, Credited tz2 | Credited tz1, Debited tz2 ->
                 Token.is_zero token1 tz1 && Token.is_zero token1 tz2))
        && compare_update_origin uo1 uo2 = 0)
      balance_updates1
      balance_updates2
  in
  match res with Ok b -> b | Error _ -> false

let test_balance_updates encoding =
  let gen = generate_balance_updates in
  test_roundtrip ~count:2000 ~title:"Balance_updates" ~gen ~eq encoding

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": Operation_encoding")
    [
      ( "roundtrip",
        qcheck_wrap [test_balance_updates Receipt_repr.balance_updates_encoding]
      );
    ]
