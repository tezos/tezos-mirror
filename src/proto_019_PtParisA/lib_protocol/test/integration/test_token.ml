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
    Invocation: dune exec src/proto_019_PtParisA/lib_protocol/test/integration/main.exe \
                 -- --file test_token.ml
    Subject:    Token movements in the protocol.
*)

open Protocol
open Alpha_context
open Test_tez

(** Creates a context with a single account. Returns the context and the public
    key hash of the account. *)
let create_context () =
  let open Lwt_result_syntax in
  let (Account.{pkh; _} as account) = Account.new_account () in
  let bootstrap_account = Account.make_bootstrap_account account in
  let* ctxt = Block.alpha_context [bootstrap_account] in
  return (ctxt, pkh)

let random_amount () =
  match Tez.of_mutez (Int64.add 1L (Random.int64 100L)) with
  | None -> assert false
  | Some x -> x

let nonce = Origination_nonce.Internal_for_tests.initial Operation_hash.zero

let sc_rollup () = Sc_rollup.Internal_for_tests.originated_sc_rollup nonce

(** Check balances for a simple transfer from [bootstrap] to new [Implicit]. *)
let test_simple_balances () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  let* ctxt, pkh = create_context () in
  let giver = `Contract (Contract.Implicit pkh) in
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = `Contract (Contract.Implicit pkh) in
  let amount = Tez.one in
  let*@ ctxt', _ = Token.transfer ctxt giver receiver amount in
  let*@ ctxt, bal_giver = Token.Internal_for_tests.balance ctxt giver in
  let*@ ctxt', bal_giver' = Token.Internal_for_tests.balance ctxt' giver in
  let*@ _, bal_receiver = Token.Internal_for_tests.balance ctxt receiver in
  let*@ _, bal_receiver' = Token.Internal_for_tests.balance ctxt' receiver in
  let*? add_bal_giver'_amount = bal_giver' +? amount in
  let*? add_bal_receiver_amount = bal_receiver +? amount in
  let* () = Assert.equal_tez ~loc:__LOC__ bal_giver add_bal_giver'_amount in
  Assert.equal_tez ~loc:__LOC__ bal_receiver' add_bal_receiver_amount

(** Check balance updates for a simple transfer from [bootstrap] to new
    [Implicit]. *)
let test_simple_balance_updates () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  let* ctxt, pkh = create_context () in
  let giver = Contract.Implicit pkh in
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = Contract.Implicit pkh in
  let amount = Tez.one in
  let*@ _, bal_updates =
    Token.transfer ctxt (`Contract giver) (`Contract receiver) amount
  in
  Alcotest.(
    check
      bool
      "Missing balance update for giver contract."
      (List.mem
         ~equal:( = )
         (Receipt.item (Contract giver) (Debited amount) Block_application)
         bal_updates)
      true) ;
  Alcotest.(
    check
      bool
      "Missing balance update for receiver contract."
      (List.mem
         ~equal:( = )
         (Receipt.item (Contract receiver) (Credited amount) Block_application)
         bal_updates)
      true) ;
  return_unit

let test_allocated_and_deallocated ctxt receiver initial_status
    status_when_empty =
  let open Lwt_result_wrap_syntax in
  let receiver_container = (receiver :> Token.container) in
  let*@ ctxt, allocated =
    Token.Internal_for_tests.allocated ctxt receiver_container
  in
  let* () = Assert.equal_bool ~loc:__LOC__ allocated initial_status in
  let amount = Tez.one in
  let*@ ctxt', _ = Token.transfer ctxt `Minted receiver_container amount in
  let*@ ctxt', allocated =
    Token.Internal_for_tests.allocated ctxt' receiver_container
  in
  let* () = Assert.equal_bool ~loc:__LOC__ allocated true in
  let*@ ctxt', bal_receiver' =
    Token.Internal_for_tests.balance ctxt' receiver
  in
  let*@ ctxt', _ =
    Token.transfer ctxt' receiver_container `Burned bal_receiver'
  in
  let*@ _, allocated =
    Token.Internal_for_tests.allocated ctxt' receiver_container
  in
  let* () = Assert.equal_bool ~loc:__LOC__ allocated status_when_empty in
  return_unit

let test_allocated_and_deallocated_when_empty ctxt receiver =
  test_allocated_and_deallocated ctxt receiver false false

let test_allocated_and_still_allocated_when_empty ctxt receiver initial_status =
  test_allocated_and_deallocated ctxt receiver initial_status true

let test_allocated () =
  let open Lwt_result_syntax in
  Random.init 0 ;
  let* ctxt, baker_pkh = create_context () in
  let receiver = `Contract (Contract.Implicit baker_pkh) in
  let* () = test_allocated_and_still_allocated_when_empty ctxt receiver true in
  (* Generate a fresh, empty, non-allocated implicit account. *)
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = `Contract (Contract.Implicit pkh) in
  let* () = test_allocated_and_deallocated_when_empty ctxt receiver in
  let receiver = `Collected_commitments Blinded_public_key_hash.zero in
  let* () = test_allocated_and_deallocated_when_empty ctxt receiver in
  let receiver = `Block_fees in
  let* () = test_allocated_and_still_allocated_when_empty ctxt receiver true in
  let receiver =
    let bond_id = Bond_id.Sc_rollup_bond_id (sc_rollup ()) in
    `Frozen_bonds (Contract.Implicit pkh, bond_id)
  in
  test_allocated_and_deallocated_when_empty ctxt receiver

let check_receiver_balances ctxt ctxt' receiver amount =
  let open Lwt_result_wrap_syntax in
  let*@ _, bal_receiver = Token.Internal_for_tests.balance ctxt receiver in
  let*@ _, bal_receiver' = Token.Internal_for_tests.balance ctxt' receiver in
  let*? add_bal_receiver_amount = bal_receiver +? amount in
  Assert.equal_tez ~loc:__LOC__ bal_receiver' add_bal_receiver_amount

let test_transferring_to_receiver ctxt receiver amount expected_bupds =
  let open Lwt_result_wrap_syntax in
  (* Transferring zero must be a noop, and must not return balance updates. *)
  let*@ ctxt', bupds = Token.transfer ctxt `Minted receiver Tez.zero in
  let* () = Assert.equal_bool ~loc:__LOC__ (ctxt == ctxt' && bupds = []) true in
  (* Test transferring a non null amount. *)
  let*@ ctxt', bupds = Token.transfer ctxt `Minted receiver amount in
  let* () = check_receiver_balances ctxt ctxt' receiver amount in
  let expected_bupds =
    Receipt.item Minted (Debited amount) Block_application :: expected_bupds
  in
  Alcotest.(
    check bool "Balance updates do not match." (bupds = expected_bupds) true) ;
  (* Test transferring to go beyond capacity. *)
  let*@ ctxt', bal = Token.Internal_for_tests.balance ctxt' receiver in
  let amount = Tez.of_mutez_exn Int64.max_int -! bal +! Tez.one_mutez in
  let*!@ res = Token.transfer ctxt' `Minted receiver amount in
  Assert.proto_error_with_info ~loc:__LOC__ res "Overflowing tez addition"

let test_transferring_to_contract ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = Contract.Implicit pkh in
  let amount = random_amount () in
  test_transferring_to_receiver
    ctxt
    (`Contract receiver)
    amount
    Receipt.[item (Contract receiver) (Credited amount) Block_application]

let test_transferring_to_collected_commitments ctxt =
  let amount = random_amount () in
  let bpkh = Blinded_public_key_hash.zero in
  test_transferring_to_receiver
    ctxt
    (`Collected_commitments bpkh)
    amount
    Receipt.[item (Commitments bpkh) (Credited amount) Block_application]

let test_transferring_to_collected_fees ctxt =
  let amount = random_amount () in
  test_transferring_to_receiver
    ctxt
    `Block_fees
    amount
    Receipt.[item Block_fees (Credited amount) Block_application]

let test_transferring_to_burned ctxt =
  let open Lwt_result_wrap_syntax in
  let amount = random_amount () in
  let minted_bupd = Receipt.item Minted (Debited amount) Block_application in
  let*@ _, bupds = Token.transfer ctxt `Minted `Burned amount in
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (bupds
      = Receipt.[minted_bupd; item Burned (Credited amount) Block_application])
      true
  in
  let*@ _, bupds = Token.transfer ctxt `Minted `Storage_fees amount in
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (bupds
      = Receipt.
          [minted_bupd; item Storage_fees (Credited amount) Block_application])
      true
  in
  let*@ _, bupds =
    Token.transfer ctxt `Minted `Double_signing_punishments amount
  in
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (bupds
      = Receipt.
          [
            minted_bupd;
            item Double_signing_punishments (Credited amount) Block_application;
          ])
      true
  in
  let pkh = Signature.Public_key_hash.zero in
  let p, r = (Random.bool (), Random.bool ()) in
  let*@ _, bupds =
    Token.transfer ctxt `Minted (`Lost_attesting_rewards (pkh, p, r)) amount
  in
  let* () =
    Assert.equal_bool
      ~loc:__LOC__
      (bupds
      = Receipt.
          [
            minted_bupd;
            item
              (Lost_attesting_rewards (pkh, p, r))
              (Credited amount)
              Block_application;
          ])
      true
  in
  let*@ _, bupds =
    Token.transfer ctxt `Minted `Sc_rollup_refutation_punishments amount
  in
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = Receipt.
        [
          minted_bupd;
          item
            Sc_rollup_refutation_punishments
            (Credited amount)
            Block_application;
        ])
    true

let test_transferring_to_frozen_bonds ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let contract = Contract.Implicit pkh in
  let sc_rollup = sc_rollup () in
  let bond_id = Bond_id.Sc_rollup_bond_id sc_rollup in
  let amount = random_amount () in
  test_transferring_to_receiver
    ctxt
    (`Frozen_bonds (contract, bond_id))
    amount
    Receipt.
      [
        item
          (Frozen_bonds (contract, bond_id))
          (Credited amount)
          Block_application;
      ]

let test_transferring_to_receiver () =
  let open Lwt_result_syntax in
  Random.init 0 ;
  let* ctxt, _ = create_context () in
  let* () = test_transferring_to_contract ctxt in
  let* () = test_transferring_to_collected_commitments ctxt in
  let* () = test_transferring_to_collected_fees ctxt in
  let* () = test_transferring_to_burned ctxt in
  test_transferring_to_frozen_bonds ctxt

let check_giver_balances ctxt ctxt' giver amount =
  let open Lwt_result_wrap_syntax in
  let*@ _, bal_giver = Token.Internal_for_tests.balance ctxt giver in
  let*@ _, bal_giver' = Token.Internal_for_tests.balance ctxt' giver in
  let*? add_bal_giver'_amount = bal_giver' +? amount in
  Assert.equal_tez ~loc:__LOC__ bal_giver add_bal_giver'_amount

let test_transferring_from_infinite_source ctxt giver expected_bupds =
  let open Lwt_result_wrap_syntax in
  (* Transferring zero must not return balance updates. *)
  let*@ _, bupds = Token.transfer ctxt giver `Burned Tez.zero in
  let* () = Assert.equal_bool ~loc:__LOC__ (bupds = []) true in
  (* Test transferring a non null amount. *)
  let amount = random_amount () in
  let*@ _, bupds = Token.transfer ctxt giver `Burned amount in
  let expected_bupds =
    expected_bupds amount
    @ Receipt.[item Burned (Credited amount) Block_application]
  in
  let* () = Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true in
  return_unit

(* Returns the balance of [account] if [account] is allocated, and returns
   [Tez.zero] otherwise. *)
let balance_no_fail ctxt account =
  let open Lwt_result_wrap_syntax in
  let*@ ctxt, allocated =
    Token.Internal_for_tests.allocated ctxt (account :> Token.container)
  in
  if allocated then wrap (Token.Internal_for_tests.balance ctxt account)
  else return (ctxt, Tez.zero)

let test_transferring_from_container ctxt giver amount expected_bupds =
  let open Lwt_result_wrap_syntax in
  let* ctxt, balance = balance_no_fail ctxt giver in
  let* () = Assert.equal_tez ~loc:__LOC__ balance Tez.zero in
  (* Test transferring from an empty account. *)
  let*!@ res = Token.transfer ctxt giver `Burned Tez.one in
  let error_title =
    match giver with
    | `Contract _ -> "Balance too low"
    | `Frozen_bonds _ -> "Storage error (fatal internal error)"
    | _ -> "Underflowing tez subtraction"
  in
  let* () = Assert.proto_error_with_info ~loc:__LOC__ res error_title in
  (* Transferring zero must be a noop, and must not return balance updates. *)
  let*@ ctxt', bupds = Token.transfer ctxt giver `Burned Tez.zero in
  let* () = Assert.equal_bool ~loc:__LOC__ (ctxt == ctxt' && bupds = []) true in
  (* Test transferring everything. *)
  let*@ ctxt, _ = Token.transfer ctxt `Minted giver amount in
  let*@ ctxt', bupds = Token.transfer ctxt giver `Burned amount in
  let* () = check_giver_balances ctxt ctxt' giver amount in
  let expected_bupds =
    expected_bupds @ Receipt.[item Burned (Credited amount) Block_application]
  in
  let* () = Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true in
  (* Test transferring a smaller amount. *)
  let*@ ctxt, _ = Token.transfer ctxt `Minted giver amount in
  let* () =
    match giver with
    | `Frozen_bonds _ ->
        let*!@ res = Token.transfer ctxt giver `Burned amount in
        let error_title = "Partial spending of frozen bonds" in
        Assert.proto_error_with_info ~loc:__LOC__ res error_title
    | _ ->
        let*@ ctxt', bupds = Token.transfer ctxt giver `Burned amount in
        let* () = check_giver_balances ctxt ctxt' giver amount in
        Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true
  in
  (* Test transferring more than available. *)
  let*@ ctxt, balance = Token.Internal_for_tests.balance ctxt giver in
  let*!@ res = Token.transfer ctxt giver `Burned (balance +! Tez.one) in
  let error_title =
    match giver with
    | `Contract _ -> "Balance too low"
    | `Frozen_bonds _ -> "Partial spending of frozen bonds"
    | _ -> "Underflowing tez subtraction"
  in
  Assert.proto_error_with_info ~loc:__LOC__ res error_title

let test_transferring_from_contract ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let giver = Contract.Implicit pkh in
  let amount = random_amount () in
  test_transferring_from_container
    ctxt
    (`Contract giver)
    amount
    Receipt.[item (Contract giver) (Debited amount) Block_application]

let test_transferring_from_collected_commitments ctxt =
  let amount = random_amount () in
  let bpkh = Blinded_public_key_hash.zero in
  test_transferring_from_container
    ctxt
    (`Collected_commitments bpkh)
    amount
    Receipt.[item (Commitments bpkh) (Debited amount) Block_application]

let test_transferring_from_collected_fees ctxt =
  let amount = random_amount () in
  test_transferring_from_container
    ctxt
    `Block_fees
    amount
    Receipt.[item Block_fees (Debited amount) Block_application]

let test_transferring_from_frozen_bonds ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let contract = Contract.Implicit pkh in
  let sc_rollup = sc_rollup () in
  let bond_id = Bond_id.Sc_rollup_bond_id sc_rollup in
  let amount = random_amount () in
  test_transferring_from_container
    ctxt
    (`Frozen_bonds (contract, bond_id))
    amount
    Receipt.
      [
        item
          (Frozen_bonds (contract, bond_id))
          (Debited amount)
          Block_application;
      ]

let test_transferring_from_giver () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  let* ctxt, _ = create_context () in
  let* () =
    test_transferring_from_infinite_source ctxt `Invoice (fun am ->
        Receipt.[item Invoice (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Bootstrap (fun am ->
        Receipt.[item Bootstrap (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Initial_commitments (fun am ->
        Receipt.[item Initial_commitments (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Revelation_rewards (fun am ->
        Receipt.[item Nonce_revelation_rewards (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Attesting_rewards (fun am ->
        Receipt.[item Attesting_rewards (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Baking_rewards (fun am ->
        Receipt.[item Baking_rewards (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Baking_bonuses (fun am ->
        Receipt.[item Baking_bonuses (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source ctxt `Minted (fun am ->
        Receipt.[item Minted (Debited am) Block_application])
  in
  let* () =
    test_transferring_from_infinite_source
      ctxt
      `Liquidity_baking_subsidies
      (fun am ->
        Receipt.[item Liquidity_baking_subsidies (Debited am) Block_application])
  in
  let* () = test_transferring_from_contract ctxt in
  let* () = test_transferring_from_collected_commitments ctxt in
  let* () = test_transferring_from_collected_fees ctxt in
  test_transferring_from_frozen_bonds ctxt

let cast_to_container_type x =
  match x with
  | `Burned | `Invoice | `Bootstrap | `Initial_commitments | `Minted
  | `Liquidity_baking_subsidies ->
      None
  | `Contract _ as x -> Some x
  | `Collected_commitments _ as x -> Some x
  | `Block_fees as x -> Some x
  | `Frozen_bonds _ as x -> Some x

(** Generates all combinations of constructors. *)
let build_test_cases () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, pkh = create_context () in
  let origin = `Contract (Contract.Implicit pkh) in
  let user1, _, _ = Signature.generate_key () in
  let user1c = `Contract (Contract.Implicit user1) in
  let user2, _, _ = Signature.generate_key () in
  let user2c = `Contract (Contract.Implicit user2) in
  let baker1, baker1_pk, _ = Signature.generate_key () in
  let baker1c = `Contract (Contract.Implicit baker1) in
  let baker2, baker2_pk, _ = Signature.generate_key () in
  let baker2c = `Contract (Contract.Implicit baker2) in
  (* Allocate contracts for user1, user2, baker1, and baker2. *)
  let*@ ctxt, _ = Token.transfer ctxt origin user1c (random_amount ()) in
  let*@ ctxt, _ = Token.transfer ctxt origin user2c (random_amount ()) in
  let*@ ctxt, _ = Token.transfer ctxt origin baker1c (random_amount ()) in
  let*@ ctxt, _ = Token.transfer ctxt origin baker2c (random_amount ()) in
  (* Configure baker1, and baker2 as delegates by self-delegation, for which
     revealing their manager key is a prerequisite. *)
  let*@ ctxt = Contract.reveal_manager_key ctxt baker1 baker1_pk in
  let*@ ctxt =
    Contract.Delegate.set ctxt (Contract.Implicit baker1) (Some baker1)
  in
  let*@ ctxt = Contract.reveal_manager_key ctxt baker2 baker2_pk in
  let*@ ctxt =
    Contract.Delegate.set ctxt (Contract.Implicit baker2) (Some baker2)
    (* Let user1 delegate to baker2. *)
  in
  let*@ ctxt =
    Contract.Delegate.set ctxt (Contract.Implicit user1) (Some baker2)
  in
  let sc_rollup1 = sc_rollup () in
  let bond_id1 = Bond_id.Sc_rollup_bond_id sc_rollup1 in
  let sc_rollup2 = sc_rollup () in
  let bond_id2 = Bond_id.Sc_rollup_bond_id sc_rollup2 in
  let user1ic = Contract.Implicit user1 in
  let baker2ic = Contract.Implicit baker2 in
  let giver_list =
    [
      (`Invoice, random_amount ());
      (`Bootstrap, random_amount ());
      (`Initial_commitments, random_amount ());
      (`Minted, random_amount ());
      (`Liquidity_baking_subsidies, random_amount ());
      (`Collected_commitments Blinded_public_key_hash.zero, random_amount ());
      (`Block_fees, random_amount ());
      (user1c, random_amount ());
      (user2c, random_amount ());
      (baker1c, random_amount ());
      (baker2c, random_amount ());
      (`Frozen_bonds (user1ic, bond_id1), random_amount ());
      (`Frozen_bonds (baker2ic, bond_id2), random_amount ());
    ]
  in
  let receiver_list =
    [
      `Collected_commitments Blinded_public_key_hash.zero;
      `Block_fees;
      user1c;
      user2c;
      baker1c;
      baker2c;
      `Frozen_bonds (user1ic, bond_id1);
      `Frozen_bonds (baker2ic, bond_id2);
      `Burned;
    ]
  in
  return (ctxt, List.product giver_list receiver_list)

let check_giver_balances ctxt ctxt' giver amount =
  let open Lwt_result_syntax in
  match cast_to_container_type giver with
  | None -> return_unit
  | Some giver -> check_giver_balances ctxt ctxt' giver amount

let check_receiver_balances ctxt ctxt' receiver amount =
  let open Lwt_result_syntax in
  match cast_to_container_type receiver with
  | None -> return_unit
  | Some receiver -> check_receiver_balances ctxt ctxt' receiver amount

let check_balances ctxt ctxt' giver receiver amount =
  let open Lwt_result_wrap_syntax in
  match (cast_to_container_type giver, cast_to_container_type receiver) with
  | None, None -> return_unit
  | Some giver, Some receiver when giver = receiver ->
      (* giver and receiver are the same contract *)
      let*@ _, bal_receiver = Token.Internal_for_tests.balance ctxt receiver in
      let*@ _, bal_receiver' =
        Token.Internal_for_tests.balance ctxt' receiver
      in
      Assert.equal_tez ~loc:__LOC__ bal_receiver bal_receiver'
  | Some giver, None -> check_giver_balances ctxt ctxt' giver amount
  | None, Some receiver -> check_receiver_balances ctxt ctxt' receiver amount
  | Some giver, Some receiver ->
      let* () = check_giver_balances ctxt ctxt' giver amount in
      check_receiver_balances ctxt ctxt' receiver amount

let test_all_combinations_of_givers_and_receivers () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  let* ctxt, cases = build_test_cases () in
  List.iter_es
    (fun ((giver, amount), receiver) ->
      let* ctxt =
        match cast_to_container_type giver with
        | None -> return ctxt
        | Some giver ->
            let*@ ctxt, _ = Token.transfer ctxt `Minted giver amount in
            return ctxt
      in
      let*@ ctxt', _ = Token.transfer ctxt giver receiver amount in
      check_balances ctxt ctxt' giver receiver amount)
    cases

(** [coalesce (account, Credited am1, origin) (account, Credited am2, origin)
    = Some (account, Credited (am1+am2), origin)]

    [coalesce (account, Debited am1, origin) (account, Debited am2, origin)
    = Some (account, Debited (am1+am2), origin)]

    Fails if bu1 and bu2 have different accounts or different origins, or
    if one is a credit while the other is a debit. *)
let coalesce_balance_updates bu1 bu2 =
  let open Receipt in
  let (Balance_update_item (bu1_bal, bu1_balupd, bu1_origin)) = bu1 in
  let (Balance_update_item (bu2_bal, bu2_balupd, bu2_origin)) = bu2 in
  assert (bu1_origin = bu2_origin) ;
  let token1 = token_of_balance bu1_bal in
  let token2 = token_of_balance bu2_bal in
  let Refl =
    match Receipt.Token.eq token1 token2 with
    | None -> assert false
    | Some refl -> refl
  in
  assert (bu1_bal = bu2_bal) ;
  match (bu1_balupd, bu2_balupd) with
  | Credited bu1_am, Credited bu2_am ->
      let bu_am =
        match Receipt.Token.add token1 bu1_am bu2_am with
        | Ok am -> am
        | _ -> assert false
      in
      item bu1_bal (Credited bu_am) bu1_origin
  | Debited bu1_am, Debited bu2_am ->
      let bu_am =
        match Receipt.Token.add token1 bu1_am bu2_am with
        | Ok am -> am
        | _ -> assert false
      in
      item bu1_bal (Debited bu_am) bu1_origin
  | Credited _, Debited _ | Debited _, Credited _ -> assert false

(** Check that elt has the same balance in ctxt1 and ctxt2. *)
let check_balances_are_consistent ctxt1 ctxt2 elt =
  let open Lwt_result_syntax in
  match elt with
  | #Token.Internal_for_tests.container_with_balance as elt ->
      let* _, elt_bal1 = Token.Internal_for_tests.balance ctxt1 elt in
      let* _, elt_bal2 = Token.Internal_for_tests.balance ctxt2 elt in
      assert (elt_bal1 = elt_bal2) ;
      return_unit
  | `Frozen_deposits _ | `Unstaked_frozen_deposits _ | `Invoice | `Bootstrap
  | `Initial_commitments | `Minted | `Liquidity_baking_subsidies | `Burned ->
      return_unit

(** Test that [transfer_n] is equivalent to n debits followed by n credits. *)
let test_transfer_n ctxt (giver : ([< Token.container] * Tez.t) list)
    (receiver : [< Token.container]) =
  let open Lwt_result_syntax in
  (* Run transfer_n. *)
  let* ctxt1, bal_updates1 = Token.transfer_n ctxt giver receiver in
  (* Debit all givers. *)
  let* ctxt, debit_logs =
    List.fold_left_es
      (fun (ctxt, bal_updates) (giver, am) ->
        let* ctxt, debit_logs = Token.transfer ctxt giver `Burned am in
        return (ctxt, bal_updates @ debit_logs))
      (ctxt, [])
      giver
  in
  (* remove burning balance updates *)
  let debit_logs =
    List.filter
      (fun b ->
        match b with
        | Receipt.Balance_update_item (Burned, _, _) -> false
        | _ -> true)
      debit_logs
  in
  (* Credit the receiver for each giver. *)
  let* ctxt2, credit_logs =
    List.fold_left_es
      (fun (ctxt, bal_updates) (_, am) ->
        let* ctxt, credit_logs = Token.transfer ctxt `Minted receiver am in
        return (ctxt, bal_updates @ credit_logs))
      (ctxt, [])
      giver
  in
  (* remove minting balance updates *)
  let credit_logs =
    List.filter
      (fun b ->
        match b with
        | Receipt.Balance_update_item (Minted, _, _) -> false
        | _ -> true)
      credit_logs
  in
  (* Check equivalence of balance updates. *)
  let credit_logs =
    match credit_logs with
    | [] -> []
    | head :: tail -> [List.fold_left coalesce_balance_updates head tail]
  in
  assert (bal_updates1 = debit_logs @ credit_logs) ;
  (* Check balances are the same in ctxt1 and ctxt2. *)
  let* () =
    List.(iter_es (check_balances_are_consistent ctxt1 ctxt2) (map fst giver))
  in
  check_balances_are_consistent ctxt1 ctxt2 receiver

let test_transfer_n_with_no_giver () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  let* ctxt, pkh = create_context () in
  let*@ () = test_transfer_n ctxt [] `Block_fees in
  let receiver = `Contract (Contract.Implicit pkh) in
  let*@ () = test_transfer_n ctxt [] receiver in
  return_unit

let test_transfer_n_with_several_givers () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  let* ctxt, pkh = create_context () in
  let origin = `Contract (Contract.Implicit pkh) in
  let user1, _, _ = Signature.generate_key () in
  let user1c = `Contract (Contract.Implicit user1) in
  let user2, _, _ = Signature.generate_key () in
  let user2c = `Contract (Contract.Implicit user2) in
  let user3, _, _ = Signature.generate_key () in
  let user3c = `Contract (Contract.Implicit user3) in
  let user4, _, _ = Signature.generate_key () in
  let user4c = `Contract (Contract.Implicit user4) in
  (* Allocate contracts for user1, user2, user3, and user4. *)
  let amount =
    match Tez.of_mutez 1000L with None -> assert false | Some x -> x
  in
  let*@ ctxt, _ = Token.transfer ctxt origin user1c amount in
  let*@ ctxt, _ = Token.transfer ctxt origin user2c amount in
  let*@ ctxt, _ = Token.transfer ctxt origin user3c amount in
  let*@ ctxt, _ = Token.transfer ctxt origin user4c (random_amount ()) in
  let givers =
    [
      (user2c, random_amount ());
      (user3c, random_amount ());
      (user4c, random_amount ());
    ]
  in
  let*@ () = test_transfer_n ctxt givers user1c in
  let*@ () =
    test_transfer_n ctxt ((user1c, random_amount ()) :: givers) user1c
  in
  return_unit

let tests =
  Tztest.
    [
      tztest "transfer - balances" `Quick test_simple_balances;
      tztest "transfer - balance updates" `Quick test_simple_balance_updates;
      tztest "transfer - test allocated" `Quick test_allocated;
      tztest
        "transfer - test transfer to receiver"
        `Quick
        test_transferring_to_receiver;
      tztest
        "transfer - test transfer from giver"
        `Quick
        test_transferring_from_giver;
      tztest
        "transfer - test all (givers x receivers)"
        `Quick
        test_all_combinations_of_givers_and_receivers;
      tztest
        "transfer - test from no giver to a receiver"
        `Quick
        test_transfer_n_with_no_giver;
      tztest
        "transfer - test from n givers to a receiver"
        `Quick
        test_transfer_n_with_several_givers;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("token movements", tests)]
  |> Lwt_main.run
