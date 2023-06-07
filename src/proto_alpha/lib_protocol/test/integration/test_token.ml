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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                 -- --file test_token.ml
    Subject:    Token movements in the protocol.
*)

open Protocol
open Alpha_context
open Test_tez

(** Creates a context with a single account. Returns the context and the public
    key hash of the account. *)
let create_context () =
  let (Account.{pkh; _} as account) = Account.new_account () in
  let bootstrap_account = Account.make_bootstrap_account account in
  Block.alpha_context [bootstrap_account] >>=? fun ctxt -> return (ctxt, pkh)

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
  create_context () >>=? fun (ctxt, pkh) ->
  let giver = `Contract (Contract.Implicit pkh) in
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = `Contract (Contract.Implicit pkh) in
  let amount = Tez.one in
  wrap (Token.transfer ctxt giver receiver amount) >>=? fun (ctxt', _) ->
  wrap (Token.balance ctxt giver) >>=? fun (ctxt, bal_giver) ->
  wrap (Token.balance ctxt' giver) >>=? fun (ctxt', bal_giver') ->
  wrap (Token.balance ctxt receiver) >>=? fun (_, bal_receiver) ->
  wrap (Token.balance ctxt' receiver) >>=? fun (_, bal_receiver') ->
  bal_giver' +? amount >>?= fun add_bal_giver'_amount ->
  bal_receiver +? amount >>?= fun add_bal_receiver_amount ->
  Assert.equal_tez ~loc:__LOC__ bal_giver add_bal_giver'_amount >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ bal_receiver' add_bal_receiver_amount

(** Check balance updates for a simple transfer from [bootstrap] to new
    [Implicit]. *)
let test_simple_balance_updates () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  let giver = Contract.Implicit pkh in
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = Contract.Implicit pkh in
  let amount = Tez.one in
  wrap (Token.transfer ctxt (`Contract giver) (`Contract receiver) amount)
  >>=? fun (_, bal_updates) ->
  Alcotest.(
    check
      bool
      "Missing balance update for giver contract."
      (List.mem
         ~equal:( = )
         Receipt.(Contract giver, Debited amount, Block_application)
         bal_updates)
      true) ;
  Alcotest.(
    check
      bool
      "Missing balance update for receiver contract."
      (List.mem
         ~equal:( = )
         Receipt.(Contract receiver, Credited amount, Block_application)
         bal_updates)
      true) ;
  return_unit

let test_allocated_and_deallocated ctxt receiver initial_status
    status_when_empty =
  let open Lwt_result_wrap_syntax in
  wrap (Token.allocated ctxt receiver) >>=? fun (ctxt, allocated) ->
  Assert.equal_bool ~loc:__LOC__ allocated initial_status >>=? fun () ->
  let amount = Tez.one in
  wrap (Token.transfer ctxt `Minted receiver amount) >>=? fun (ctxt', _) ->
  wrap (Token.allocated ctxt' receiver) >>=? fun (ctxt', allocated) ->
  Assert.equal_bool ~loc:__LOC__ allocated true >>=? fun () ->
  wrap (Token.balance ctxt' receiver) >>=? fun (ctxt', bal_receiver') ->
  wrap (Token.transfer ctxt' receiver `Burned bal_receiver')
  >>=? fun (ctxt', _) ->
  wrap (Token.allocated ctxt' receiver) >>=? fun (_, allocated) ->
  Assert.equal_bool ~loc:__LOC__ allocated status_when_empty >>=? fun () ->
  return_unit

let test_allocated_and_deallocated_when_empty ctxt receiver =
  test_allocated_and_deallocated ctxt receiver false false

let test_allocated_and_still_allocated_when_empty ctxt receiver initial_status =
  test_allocated_and_deallocated ctxt receiver initial_status true

let test_allocated () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, baker_pkh) ->
  let receiver = `Contract (Contract.Implicit baker_pkh) in
  test_allocated_and_still_allocated_when_empty ctxt receiver true
  >>=? fun () ->
  (* Generate a fresh, empty, non-allocated implicit account. *)
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = `Contract (Contract.Implicit pkh) in
  test_allocated_and_deallocated_when_empty ctxt receiver >>=? fun () ->
  let receiver = `Collected_commitments Blinded_public_key_hash.zero in
  test_allocated_and_deallocated_when_empty ctxt receiver >>=? fun () ->
  let receiver = `Frozen_deposits pkh in
  test_allocated_and_still_allocated_when_empty ctxt receiver true
  >>=? fun () ->
  let receiver = `Block_fees in
  test_allocated_and_still_allocated_when_empty ctxt receiver true
  >>=? fun () ->
  let receiver =
    let bond_id = Bond_id.Sc_rollup_bond_id (sc_rollup ()) in
    `Frozen_bonds (Contract.Implicit pkh, bond_id)
  in
  test_allocated_and_deallocated_when_empty ctxt receiver

let check_receiver_balances ctxt ctxt' receiver amount =
  let open Lwt_result_wrap_syntax in
  wrap (Token.balance ctxt receiver) >>=? fun (_, bal_receiver) ->
  wrap (Token.balance ctxt' receiver) >>=? fun (_, bal_receiver') ->
  bal_receiver +? amount >>?= fun add_bal_receiver_amount ->
  Assert.equal_tez ~loc:__LOC__ bal_receiver' add_bal_receiver_amount

let test_transferring_to_receiver ctxt receiver amount expected_bupds =
  let open Lwt_result_wrap_syntax in
  (* Transferring zero must be a noop, and must not return balance updates. *)
  wrap (Token.transfer ctxt `Minted receiver Tez.zero)
  >>=? fun (ctxt', bupds) ->
  Assert.equal_bool ~loc:__LOC__ (ctxt == ctxt' && bupds = []) true
  >>=? fun () ->
  (* Test transferring a non null amount. *)
  wrap (Token.transfer ctxt `Minted receiver amount) >>=? fun (ctxt', bupds) ->
  check_receiver_balances ctxt ctxt' receiver amount >>=? fun () ->
  let expected_bupds =
    Receipt.(Minted, Debited amount, Block_application) :: expected_bupds
  in
  Alcotest.(
    check bool "Balance updates do not match." (bupds = expected_bupds) true) ;
  (* Test transferring to go beyond capacity. *)
  wrap (Token.balance ctxt' receiver) >>=? fun (ctxt', bal) ->
  let amount = Tez.of_mutez_exn Int64.max_int -! bal +! Tez.one_mutez in
  wrap (Token.transfer ctxt' `Minted receiver amount) >>= fun res ->
  Assert.proto_error_with_info ~loc:__LOC__ res "Overflowing tez addition"

let test_transferring_to_contract ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let receiver = Contract.Implicit pkh in
  let amount = random_amount () in
  test_transferring_to_receiver
    ctxt
    (`Contract receiver)
    amount
    [(Contract receiver, Credited amount, Block_application)]

let test_transferring_to_collected_commitments ctxt =
  let amount = random_amount () in
  let bpkh = Blinded_public_key_hash.zero in
  test_transferring_to_receiver
    ctxt
    (`Collected_commitments bpkh)
    amount
    [(Commitments bpkh, Credited amount, Block_application)]

let test_transferring_to_frozen_deposits ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let amount = random_amount () in
  test_transferring_to_receiver
    ctxt
    (`Frozen_deposits pkh)
    amount
    [(Deposits pkh, Credited amount, Block_application)]

let test_transferring_to_collected_fees ctxt =
  let amount = random_amount () in
  test_transferring_to_receiver
    ctxt
    `Block_fees
    amount
    [(Block_fees, Credited amount, Block_application)]

let test_transferring_to_burned ctxt =
  let open Lwt_result_wrap_syntax in
  let amount = random_amount () in
  let minted_bupd = Receipt.(Minted, Debited amount, Block_application) in
  wrap (Token.transfer ctxt `Minted `Burned amount) >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds = [minted_bupd; (Burned, Credited amount, Block_application)])
    true
  >>=? fun () ->
  wrap (Token.transfer ctxt `Minted `Storage_fees amount) >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds = [minted_bupd; (Storage_fees, Credited amount, Block_application)])
    true
  >>=? fun () ->
  wrap (Token.transfer ctxt `Minted `Double_signing_punishments amount)
  >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = [
        minted_bupd;
        (Double_signing_punishments, Credited amount, Block_application);
      ])
    true
  >>=? fun () ->
  let pkh = Signature.Public_key_hash.zero in
  let p, r = (Random.bool (), Random.bool ()) in
  wrap
    (Token.transfer ctxt `Minted (`Lost_endorsing_rewards (pkh, p, r)) amount)
  >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = [
        minted_bupd;
        (Lost_endorsing_rewards (pkh, p, r), Credited amount, Block_application);
      ])
    true
  >>=? fun () ->
  wrap (Token.transfer ctxt `Minted `Sc_rollup_refutation_punishments amount)
  >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = [
        minted_bupd;
        (Sc_rollup_refutation_punishments, Credited amount, Block_application);
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
    [(Frozen_bonds (contract, bond_id), Credited amount, Block_application)]

let test_transferring_to_receiver () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, _) ->
  test_transferring_to_contract ctxt >>=? fun () ->
  test_transferring_to_collected_commitments ctxt >>=? fun () ->
  test_transferring_to_frozen_deposits ctxt >>=? fun () ->
  test_transferring_to_collected_fees ctxt >>=? fun () ->
  test_transferring_to_burned ctxt >>=? fun () ->
  test_transferring_to_frozen_bonds ctxt

let check_giver_balances ctxt ctxt' giver amount =
  let open Lwt_result_wrap_syntax in
  wrap (Token.balance ctxt giver) >>=? fun (_, bal_giver) ->
  wrap (Token.balance ctxt' giver) >>=? fun (_, bal_giver') ->
  bal_giver' +? amount >>?= fun add_bal_giver'_amount ->
  Assert.equal_tez ~loc:__LOC__ bal_giver add_bal_giver'_amount

let test_transferring_from_infinite_source ctxt giver expected_bupds =
  let open Lwt_result_wrap_syntax in
  (* Transferring zero must not return balance updates. *)
  wrap (Token.transfer ctxt giver `Burned Tez.zero) >>=? fun (_, bupds) ->
  Assert.equal_bool ~loc:__LOC__ (bupds = []) true >>=? fun () ->
  (* Test transferring a non null amount. *)
  let amount = random_amount () in
  wrap (Token.transfer ctxt giver `Burned amount) >>=? fun (_, bupds) ->
  let expected_bupds =
    expected_bupds amount
    @ Receipt.[(Burned, Credited amount, Block_application)]
  in
  Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true >>=? fun () ->
  return_unit

(* Returns the balance of [account] if [account] is allocated, and returns
   [Tez.zero] otherwise. *)
let balance_no_fail ctxt account =
  let open Lwt_result_wrap_syntax in
  wrap (Token.allocated ctxt account) >>=? fun (ctxt, allocated) ->
  if allocated then wrap (Token.balance ctxt account)
  else return (ctxt, Tez.zero)

let test_transferring_from_container ctxt giver amount expected_bupds =
  let open Lwt_result_wrap_syntax in
  balance_no_fail ctxt giver >>=? fun (ctxt, balance) ->
  Assert.equal_tez ~loc:__LOC__ balance Tez.zero >>=? fun () ->
  (* Test transferring from an empty account. *)
  wrap (Token.transfer ctxt giver `Burned Tez.one) >>= fun res ->
  let error_title =
    match giver with
    | `Contract _ -> "Balance too low"
    | `Frozen_bonds _ -> "Storage error (fatal internal error)"
    | _ -> "Underflowing tez subtraction"
  in
  Assert.proto_error_with_info ~loc:__LOC__ res error_title >>=? fun () ->
  (* Transferring zero must be a noop, and must not return balance updates. *)
  wrap (Token.transfer ctxt giver `Burned Tez.zero) >>=? fun (ctxt', bupds) ->
  Assert.equal_bool ~loc:__LOC__ (ctxt == ctxt' && bupds = []) true
  >>=? fun () ->
  (* Test transferring everything. *)
  wrap (Token.transfer ctxt `Minted giver amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt giver `Burned amount) >>=? fun (ctxt', bupds) ->
  check_giver_balances ctxt ctxt' giver amount >>=? fun () ->
  let expected_bupds =
    expected_bupds @ Receipt.[(Burned, Credited amount, Block_application)]
  in
  Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true >>=? fun () ->
  (* Test transferring a smaller amount. *)
  wrap (Token.transfer ctxt `Minted giver amount) >>=? fun (ctxt, _) ->
  (match giver with
  | `Frozen_bonds _ ->
      wrap (Token.transfer ctxt giver `Burned amount) >>= fun res ->
      let error_title = "Partial spending of frozen bonds" in
      Assert.proto_error_with_info ~loc:__LOC__ res error_title
  | _ ->
      wrap (Token.transfer ctxt giver `Burned amount) >>=? fun (ctxt', bupds) ->
      check_giver_balances ctxt ctxt' giver amount >>=? fun () ->
      Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true)
  >>=? fun () ->
  (* Test transferring more than available. *)
  wrap (Token.balance ctxt giver) >>=? fun (ctxt, balance) ->
  wrap (Token.transfer ctxt giver `Burned (balance +! Tez.one)) >>= fun res ->
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
    [(Contract giver, Debited amount, Block_application)]

let test_transferring_from_collected_commitments ctxt =
  let amount = random_amount () in
  let bpkh = Blinded_public_key_hash.zero in
  test_transferring_from_container
    ctxt
    (`Collected_commitments bpkh)
    amount
    [(Commitments bpkh, Debited amount, Block_application)]

let test_transferring_from_frozen_deposits ctxt =
  let pkh, _pk, _sk = Signature.generate_key () in
  let amount = random_amount () in
  test_transferring_from_container
    ctxt
    (`Frozen_deposits pkh)
    amount
    [(Deposits pkh, Debited amount, Block_application)]

let test_transferring_from_collected_fees ctxt =
  let amount = random_amount () in
  test_transferring_from_container
    ctxt
    `Block_fees
    amount
    [(Block_fees, Debited amount, Block_application)]

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
    [(Frozen_bonds (contract, bond_id), Debited amount, Block_application)]

let test_transferring_from_giver () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, _) ->
  test_transferring_from_infinite_source ctxt `Invoice (fun am ->
      [(Invoice, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Bootstrap (fun am ->
      [(Bootstrap, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Initial_commitments (fun am ->
      [(Initial_commitments, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Revelation_rewards (fun am ->
      [(Nonce_revelation_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source
    ctxt
    `Double_signing_evidence_rewards
    (fun am ->
      [(Double_signing_evidence_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Endorsing_rewards (fun am ->
      [(Endorsing_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Baking_rewards (fun am ->
      [(Baking_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Baking_bonuses (fun am ->
      [(Baking_bonuses, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source ctxt `Minted (fun am ->
      [(Minted, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_infinite_source
    ctxt
    `Liquidity_baking_subsidies
    (fun am -> [(Liquidity_baking_subsidies, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_contract ctxt >>=? fun () ->
  test_transferring_from_collected_commitments ctxt >>=? fun () ->
  test_transferring_from_frozen_deposits ctxt >>=? fun () ->
  test_transferring_from_collected_fees ctxt >>=? fun () ->
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
  create_context () >>=? fun (ctxt, pkh) ->
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
  wrap (Token.transfer ctxt origin user1c (random_amount ()))
  >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user2c (random_amount ()))
  >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin baker1c (random_amount ()))
  >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin baker2c (random_amount ()))
  >>=? fun (ctxt, _) ->
  (* Configure baker1, and baker2 as delegates by self-delegation, for which
     revealing their manager key is a prerequisite. *)
  wrap (Contract.reveal_manager_key ctxt baker1 baker1_pk) >>=? fun ctxt ->
  wrap (Contract.Delegate.set ctxt (Contract.Implicit baker1) (Some baker1))
  >>=? fun ctxt ->
  wrap (Contract.reveal_manager_key ctxt baker2 baker2_pk) >>=? fun ctxt ->
  wrap (Contract.Delegate.set ctxt (Contract.Implicit baker2) (Some baker2))
  (* Let user1 delegate to baker2. *)
  >>=? fun ctxt ->
  wrap (Contract.Delegate.set ctxt (Contract.Implicit user1) (Some baker2))
  >>=? fun ctxt ->
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
  match cast_to_container_type giver with
  | None -> return_unit
  | Some giver -> check_giver_balances ctxt ctxt' giver amount

let check_receiver_balances ctxt ctxt' receiver amount =
  match cast_to_container_type receiver with
  | None -> return_unit
  | Some receiver -> check_receiver_balances ctxt ctxt' receiver amount

let check_balances ctxt ctxt' giver receiver amount =
  let open Lwt_result_wrap_syntax in
  match (cast_to_container_type giver, cast_to_container_type receiver) with
  | None, None -> return_unit
  | Some giver, Some receiver when giver = receiver ->
      (* giver and receiver are the same contract *)
      wrap (Token.balance ctxt receiver) >>=? fun (_, bal_receiver) ->
      wrap (Token.balance ctxt' receiver) >>=? fun (_, bal_receiver') ->
      Assert.equal_tez ~loc:__LOC__ bal_receiver bal_receiver'
  | Some giver, None -> check_giver_balances ctxt ctxt' giver amount
  | None, Some receiver -> check_receiver_balances ctxt ctxt' receiver amount
  | Some giver, Some receiver ->
      check_giver_balances ctxt ctxt' giver amount >>=? fun () ->
      check_receiver_balances ctxt ctxt' receiver amount

let test_all_combinations_of_givers_and_receivers () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  build_test_cases () >>=? fun (ctxt, cases) ->
  List.iter_es
    (fun ((giver, amount), receiver) ->
      (match cast_to_container_type giver with
      | None -> return ctxt
      | Some giver ->
          wrap (Token.transfer ctxt `Minted giver amount) >>=? fun (ctxt, _) ->
          return ctxt)
      >>=? fun ctxt ->
      wrap (Token.transfer ctxt giver receiver amount) >>=? fun (ctxt', _) ->
      check_balances ctxt ctxt' giver receiver amount)
    cases

(** [coalesce (account, Credited am1, origin) (account, Credited am2, origin)
    = Some (account, Credited (am1+am2), origin)]

    [coalesce (account, Debited am1, origin) (account, Debited am2, origin)
    = Some (account, Debited (am1+am2), origin)]

    Fails if bu1 and bu2 have different accounts or different origins, or
    if one is a credit while the other is a debit. *)
let coalesce_balance_updates bu1 bu2 =
  match (bu1, bu2) with
  | (bu1_bal, bu1_balupd, bu1_origin), (bu2_bal, bu2_balupd, bu2_origin) -> (
      assert (bu1_bal = bu2_bal) ;
      assert (bu1_origin = bu2_origin) ;
      let open Receipt in
      match (bu1_balupd, bu2_balupd) with
      | Credited bu1_am, Credited bu2_am ->
          let bu_am =
            match bu1_am +? bu2_am with Ok am -> am | _ -> assert false
          in
          (bu1_bal, Credited bu_am, bu1_origin)
      | Debited bu1_am, Debited bu2_am ->
          let bu_am =
            match bu1_am +? bu2_am with Ok am -> am | _ -> assert false
          in
          (bu1_bal, Debited bu_am, bu1_origin)
      | Credited _, Debited _ | Debited _, Credited _ -> assert false)

(** Check that elt has the same balance in ctxt1 and ctxt2. *)
let check_balances_are_consistent ctxt1 ctxt2 elt =
  match elt with
  | #Token.container as elt ->
      Token.balance ctxt1 elt >>=? fun (_, elt_bal1) ->
      Token.balance ctxt2 elt >>=? fun (_, elt_bal2) ->
      assert (elt_bal1 = elt_bal2) ;
      return_unit
  | `Invoice | `Bootstrap | `Initial_commitments | `Minted
  | `Liquidity_baking_subsidies | `Burned ->
      return_unit

(** Test that [transfer_n] is equivalent to n debits followed by n credits. *)
let test_transfer_n ctxt giver receiver =
  (* Run transfer_n. *)
  Token.transfer_n ctxt giver receiver >>=? fun (ctxt1, bal_updates1) ->
  (* Debit all givers. *)
  List.fold_left_es
    (fun (ctxt, bal_updates) (giver, am) ->
      Token.transfer ctxt giver `Burned am >>=? fun (ctxt, debit_logs) ->
      return (ctxt, bal_updates @ debit_logs))
    (ctxt, [])
    giver
  >>=? fun (ctxt, debit_logs) ->
  (* remove burning balance updates *)
  let debit_logs =
    List.filter
      (fun b -> match b with Receipt.Burned, _, _ -> false | _ -> true)
      debit_logs
  in
  (* Credit the receiver for each giver. *)
  List.fold_left_es
    (fun (ctxt, bal_updates) (_, am) ->
      Token.transfer ctxt `Minted receiver am >>=? fun (ctxt, credit_logs) ->
      return (ctxt, bal_updates @ credit_logs))
    (ctxt, [])
    giver
  >>=? fun (ctxt2, credit_logs) ->
  (* remove minting balance updates *)
  let credit_logs =
    List.filter
      (fun b -> match b with Receipt.Minted, _, _ -> false | _ -> true)
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
  List.(iter_es (check_balances_are_consistent ctxt1 ctxt2) (map fst giver))
  >>=? fun () -> check_balances_are_consistent ctxt1 ctxt2 receiver

let test_transfer_n_with_no_giver () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  wrap (test_transfer_n ctxt [] `Block_fees) >>=? fun () ->
  let receiver = `Contract (Contract.Implicit pkh) in
  wrap (test_transfer_n ctxt [] receiver)

let test_transfer_n_with_several_givers () =
  let open Lwt_result_wrap_syntax in
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
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
  wrap (Token.transfer ctxt origin user1c amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user2c amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user3c amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user4c (random_amount ()))
  >>=? fun (ctxt, _) ->
  let givers =
    [
      (user2c, random_amount ());
      (user3c, random_amount ());
      (user4c, random_amount ());
    ]
  in
  wrap (test_transfer_n ctxt givers user1c) >>=? fun () ->
  wrap (test_transfer_n ctxt ((user1c, random_amount ()) :: givers) user1c)

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
