(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
                 -- --file test_frozen_bonds.ml
    Subject:    Frozen bonds applicable to contracts and part of their stake.
*)

open Protocol
open Alpha_context
open Test_tez

let big_random_amount () =
  match Tez.of_mutez (Int64.add 100_000L (Random.int64 1_000_000L)) with
  | None -> assert false
  | Some x -> x

let small_random_amount () =
  match Tez.of_mutez (Int64.add 1_000L (Random.int64 10_000L)) with
  | None -> assert false
  | Some x -> x

let very_small_random_amount () =
  match Tez.of_mutez (Int64.add 1L (Random.int64 100L)) with
  | None -> assert false
  | Some x -> x

let nonce_zero =
  Origination_nonce.Internal_for_tests.initial Operation_hash.zero

let mk_sc_rollup ?(nonce = nonce_zero) () =
  ( Sc_rollup.Internal_for_tests.originated_sc_rollup nonce,
    Origination_nonce.Internal_for_tests.incr nonce )

(** Creates a context with a single account. Returns the context and the public
    key hash of the account. *)
let create_context () =
  let open Lwt_result_syntax in
  let (Parameters.{public_key_hash; _} as bootstrap_account) =
    Account.(new_account () |> make_bootstrap_account)
  in
  let+ ctxt = Block.alpha_context [bootstrap_account] in
  (ctxt, public_key_hash)

(** Creates a context, a user contract, and a delegate.
    Returns the context, the user contract, the user account, and the
    delegate's pkh. *)
let init_test ~user_is_delegate =
  let open Lwt_result_wrap_syntax in
  let* ctxt, _ = create_context () in
  let delegate, delegate_pk, _ = Signature.generate_key () in
  let delegate_contract = Contract.Implicit delegate in
  let delegate_account = `Contract (Contract.Implicit delegate) in
  let user_contract =
    if user_is_delegate then delegate_contract
    else
      let user, _, _ = Signature.generate_key () in
      Contract.Implicit user
  in
  let user_account = `Contract user_contract in
  (* Allocate contracts for user and delegate. *)
  let user_balance = big_random_amount () in
  let*@ ctxt, _ = Token.transfer ctxt `Minted user_account user_balance in
  let delegate_balance = big_random_amount () in
  let*@ ctxt, _ =
    Token.transfer ctxt `Minted delegate_account delegate_balance
  in
  (* Configure delegate, as a delegate by self-delegation, for which
     revealing its manager key is a prerequisite. *)
  let*@ ctxt = Contract.reveal_manager_key ctxt delegate delegate_pk in
  let*@ ctxt = Contract.Delegate.set ctxt delegate_contract (Some delegate) in
  return (ctxt, user_contract, user_account, delegate)

(** Tested scenario :
    1. user contract delegates to 'delegate',
    2. freeze a deposit,
    3. check that staking balance of delegate has not changed,
    4. remove delegation,
    5. check staking balance decreased accordingly,
    6. unfreeze the deposit,
    7. check that staking balance is unchanged,
    8. check that user's balance is unchanged. *)
let test_delegate_then_freeze_deposit () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, user_contract, user_account, delegate =
    init_test ~user_is_delegate:false
  in
  (* Fetch user's initial balance before freeze. *)
  let*@ ctxt, user_balance =
    Token.Internal_for_tests.balance ctxt user_account
  in
  (* Let user delegate to "delegate". *)
  let*@ ctxt = Contract.Delegate.set ctxt user_contract (Some delegate) in
  (* Fetch staking balance after delegation and before freeze. *)
  let*@ staking_balance = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Freeze a sc-rollup deposit. *)
  let sc_rollup, _ = mk_sc_rollup () in
  let bond_id = Bond_id.Sc_rollup_bond_id sc_rollup in
  let deposit_amount = small_random_amount () in
  let deposit_account = `Frozen_bonds (user_contract, bond_id) in
  let*@ ctxt, _ =
    Token.transfer ctxt user_account deposit_account deposit_amount
  in
  (* Fetch staking balance after freeze. *)
  let*@ staking_balance' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Ensure staking balance did not change. *)
  let* () = Assert.equal_tez ~loc:__LOC__ staking_balance' staking_balance in
  (* Remove delegation. *)
  let*@ ctxt = Contract.Delegate.set ctxt user_contract None in
  (* Fetch staking balance after delegation removal. *)
  let*@ staking_balance'' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Ensure staking balance decreased by user's initial balance. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      staking_balance''
      (staking_balance' -! user_balance)
  in
  (* Unfreeze the deposit. *)
  let*@ ctxt, _ =
    Token.transfer ctxt deposit_account user_account deposit_amount
  in
  (* Fetch staking balance of delegate. *)
  let*@ staking_balance''' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Ensure that staking balance is unchanged. *)
  let* () =
    Assert.equal_tez ~loc:__LOC__ staking_balance''' staking_balance''
  in
  (* Fetch user's balance again. *)
  let*@ _, user_balance' = Token.Internal_for_tests.balance ctxt user_account in
  (* Ensure user's balance is unchanged. *)
  Assert.equal_tez ~loc:__LOC__ user_balance' user_balance

(** Tested scenario:
    1. freeze a deposit,
    2. user contract delegate to 'delegate',
    3. check that staking balance of delegate has increased as expected,
    4. unfreeze the deposit,
    5. check that staking balance has not changed,
    6. remove delegation,
    7. check that staking balance has decreased as expected,
    8. check that the user's balance is unchanged. *)
let test_freeze_deposit_then_delegate () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, user_contract, user_account, delegate =
    init_test ~user_is_delegate:false
  in
  (* Fetch user's initial balance before freeze. *)
  let*@ ctxt, user_balance =
    Token.Internal_for_tests.balance ctxt user_account
  in
  (* Freeze a sc-rollup deposit. *)
  let sc_rollup, _ = mk_sc_rollup () in
  let bond_id = Bond_id.Sc_rollup_bond_id sc_rollup in
  let deposit_amount = small_random_amount () in
  let deposit_account = `Frozen_bonds (user_contract, bond_id) in
  let*@ ctxt, _ =
    Token.transfer ctxt user_account deposit_account deposit_amount
  in
  (* Here, user balance has decreased.
     Now, fetch staking balance before delegation and after freeze. *)
  let*@ staking_balance = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Let user delegate to "delegate". *)
  let*@ ctxt = Contract.Delegate.set ctxt user_contract (Some delegate) in
  (* Fetch staking balance after delegation. *)
  let*@ staking_balance' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* ensure staking balance increased by the user's balance. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      staking_balance'
      (user_balance +! staking_balance)
  in
  (* Unfreeze the deposit. *)
  let*@ ctxt, _ =
    Token.transfer ctxt deposit_account user_account deposit_amount
  in
  (* Fetch staking balance after unfreeze. *)
  let*@ staking_balance'' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Ensure that staking balance is unchanged. *)
  let* () = Assert.equal_tez ~loc:__LOC__ staking_balance'' staking_balance' in
  (* Remove delegation. *)
  let*@ ctxt = Contract.Delegate.set ctxt user_contract None in
  (* Fetch staking balance. *)
  let*@ staking_balance''' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Check that staking balance has decreased by the user's initial balance. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      staking_balance'''
      (staking_balance'' -! user_balance)
  in
  (* Fetch user's balance. *)
  let*@ _, user_balance' = Token.Internal_for_tests.balance ctxt user_account in
  (* Ensure user's balance is unchanged. *)
  Assert.equal_tez ~loc:__LOC__ user_balance' user_balance

(** Tested scenario:
    1. freeze a deposit (with deposit amount = balance),
    2. check that the user contract is still allocated,
    3. punish the user contract,
    4. check that the user contract is unallocated, except if it's a delegate. *)
let test_allocated_when_frozen_deposits_exists ~user_is_delegate () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, user_contract, user_account, _delegate =
    init_test ~user_is_delegate
  in
  (* Fetch user's initial balance before freeze. *)
  let*@ ctxt, user_balance =
    Token.Internal_for_tests.balance ctxt user_account
  in
  let* () = Assert.equal_bool ~loc:__LOC__ Tez.(user_balance > zero) true in
  (* Freeze a sc-rollup deposit. *)
  let sc_rollup, _ = mk_sc_rollup () in
  let bond_id = Bond_id.Sc_rollup_bond_id sc_rollup in
  let deposit_amount = user_balance in
  let deposit_account = `Frozen_bonds (user_contract, bond_id) in
  let*@ ctxt, _ =
    Token.transfer ctxt user_account deposit_account deposit_amount
  in
  (* Check that user contract is still allocated, despite a null balance. *)
  let*@ ctxt, balance = Token.Internal_for_tests.balance ctxt user_account in
  let* () = Assert.equal_tez ~loc:__LOC__ balance Tez.zero in
  let*@ ctxt, user_allocated =
    Token.Internal_for_tests.allocated ctxt (user_account :> Token.container)
  in
  let*@ ctxt, dep_allocated =
    Token.Internal_for_tests.allocated ctxt deposit_account
  in
  let* () =
    Assert.equal_bool ~loc:__LOC__ (user_allocated && dep_allocated) true
  in
  (* Punish the user contract. *)
  let*@ ctxt, _ = Token.transfer ctxt deposit_account `Burned deposit_amount in
  (* Check that user and deposit accounts have been unallocated. *)
  let*@ ctxt, user_allocated =
    Token.Internal_for_tests.allocated ctxt (user_account :> Token.container)
  in
  let*@ _, dep_allocated =
    Token.Internal_for_tests.allocated ctxt deposit_account
  in
  if user_is_delegate then
    Assert.equal_bool ~loc:__LOC__ (user_allocated && not dep_allocated) true
  else Assert.equal_bool ~loc:__LOC__ (user_allocated || dep_allocated) false

(** Tested scenario:
    1. freeze two deposits for the user contract,
    2. check that the stake of the user contract is balance + two deposits,
    3. punish for one of the deposits,
    4. check that the stake of the user contract balance + deposit,
    5. punish for the other deposit,
    6. check that the stake of the user contract is equal to balance. *)
let test_total_stake ~user_is_delegate () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, user_contract, user_account, _delegate =
    init_test ~user_is_delegate
  in
  (* Fetch user's initial balance before freeze. *)
  let*@ ctxt, user_balance =
    Token.Internal_for_tests.balance ctxt user_account
  in
  let* () = Assert.equal_bool ~loc:__LOC__ Tez.(user_balance > zero) true in
  (* Freeze 2 sc-rollup deposits. *)
  let sc_rollup, nonce = mk_sc_rollup () in
  let bond_id1 = Bond_id.Sc_rollup_bond_id sc_rollup in
  let sc_rollup, _ = mk_sc_rollup ~nonce () in
  let bond_id2 = Bond_id.Sc_rollup_bond_id sc_rollup in
  let deposit_amount = small_random_amount () in
  let deposit_account1 = `Frozen_bonds (user_contract, bond_id1) in
  let*@ ctxt, _ =
    Token.transfer ctxt user_account deposit_account1 deposit_amount
  in
  let deposit_account2 = `Frozen_bonds (user_contract, bond_id2) in
  let*@ ctxt, _ =
    Token.transfer ctxt user_account deposit_account2 deposit_amount
  in
  (* Test folding on bond ids. *)
  let*! bond_ids =
    Bond_id.Internal_for_tests.fold_on_bond_ids
      ctxt
      user_contract
      ~init:[]
      ~order:`Sorted
      ~f:(fun id l -> Lwt.return (id :: l))
  in
  let* () =
    Assert.assert_equal_list
      ~loc:__LOC__
      (fun id1 id2 -> Bond_id.compare id1 id2 = 0)
      "Unexpected bond identifiers."
      Bond_id.pp
      (List.sort Bond_id.compare bond_ids)
      (List.sort Bond_id.compare [bond_id1; bond_id2])
  in
  (* Check that the stake of user contract is balance + two deposits. *)
  let*@ stake = Contract.get_balance_and_frozen_bonds ctxt user_contract in
  let*@ frozen_bonds = Contract.get_frozen_bonds ctxt user_contract in
  let*@ ctxt, balance = Token.Internal_for_tests.balance ctxt user_account in
  let* () = Assert.equal_tez ~loc:__LOC__ (stake -! balance) frozen_bonds in
  let* () =
    Assert.equal_tez ~loc:__LOC__ (stake -! balance) (deposit_amount *! 2L)
  in
  (* Punish for one deposit. *)
  let*@ ctxt, _ = Token.transfer ctxt deposit_account2 `Burned deposit_amount in
  (* Check that stake of contract is balance + deposit. *)
  let*@ stake = Contract.get_balance_and_frozen_bonds ctxt user_contract in
  let*@ frozen_bonds = Contract.get_frozen_bonds ctxt user_contract in
  let* () = Assert.equal_tez ~loc:__LOC__ (stake -! balance) frozen_bonds in
  let* () = Assert.equal_tez ~loc:__LOC__ (stake -! balance) deposit_amount in
  (* Punish for the other deposit. *)
  let*@ ctxt, _ = Token.transfer ctxt deposit_account1 `Burned deposit_amount in
  (* Check that stake of contract is equal to balance. *)
  let*@ stake = Contract.get_balance_and_frozen_bonds ctxt user_contract in
  Assert.equal_tez ~loc:__LOC__ stake balance

let check_delegated_balance_is ctxt ~loc delegate expected_balance =
  let open Lwt_result_wrap_syntax in
  (* Fetch the delegated balance of d. *)
  let*@ delegated_balance = Delegate.For_RPC.delegated_balance ctxt delegate in
  (* Check that the delegated balance of [delegate] is as explected. *)
  Assert.equal_tez ~loc delegated_balance expected_balance

(** Tested scenario:
     1. freeze some bonds for the delegate,
     2. check that the delegated balance is null,
     3. let user contract delegate to 'delegate',
     4. check that the staking balance of 'delegate' has increased as expected,
     5. check that the delegated balance of 'delegate' is equal to the balance of the delegator,
     6. unfreeze the bonds,
     7. check that the staking balance has not changed,
     8. check that the delegated balance of 'delegate' is equal to the balance of the delegator,
     9. remove the delegation,
    10. check that staking balance has decreased as expected,
    11. check that the delegated balance is null,
    12. check that the user's balance is unchanged. *)
let test_delegated_balance () =
  let open Lwt_result_wrap_syntax in
  let* ctxt, user_contract, user_account, delegate =
    init_test ~user_is_delegate:false
  in
  let delegate_contract = Contract.Implicit delegate in
  let delegate_account = `Contract delegate_contract in
  (* Fetch user's initial balance before freeze. *)
  let*@ ctxt, user_balance =
    Token.Internal_for_tests.balance ctxt user_account
  in
  (* Fetch staking balance before freeze. *)
  let*@ staking_balance = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Freeze a sc-rollup deposit for the delegate. *)
  let sc_rollup, _ = mk_sc_rollup () in
  let bond_id = Bond_id.Sc_rollup_bond_id sc_rollup in
  let deposit_amount = small_random_amount () in
  let deposit_account = `Frozen_bonds (delegate_contract, bond_id) in
  let*@ ctxt, _ =
    Token.transfer ctxt delegate_account deposit_account deposit_amount
  in
  (* Check that the delegated balance of [delegate] is null. *)
  let* () = check_delegated_balance_is ctxt ~loc:__LOC__ delegate Tez.zero in
  (* Let user delegate to "delegate". *)
  let*@ ctxt = Contract.Delegate.set ctxt user_contract (Some delegate) in
  (* Fetch staking balance after delegation. *)
  let*@ staking_balance' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* ensure staking balance increased by the user's balance. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      staking_balance'
      (user_balance +! staking_balance)
  in
  (* Check that the delegated balance of [delegate] is equal to [user_balance]. *)
  let* () =
    check_delegated_balance_is ctxt ~loc:__LOC__ delegate user_balance
  in
  (* Unfreeze the deposit. *)
  let*@ ctxt, _ =
    Token.transfer ctxt deposit_account delegate_account deposit_amount
  in
  (* Fetch staking balance after unfreeze. *)
  let*@ staking_balance'' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Ensure that staking balance is unchanged. *)
  let* () = Assert.equal_tez ~loc:__LOC__ staking_balance'' staking_balance' in
  (* Check that the delegated balance of [delegate] is equal to [user_balance]. *)
  let* () =
    check_delegated_balance_is ctxt ~loc:__LOC__ delegate user_balance
  in
  (* Remove delegation. *)
  let*@ ctxt = Contract.Delegate.set ctxt user_contract None in
  (* Fetch staking balance. *)
  let*@ staking_balance''' = Delegate.For_RPC.staking_balance ctxt delegate in
  (* Check that staking balance has decreased by the user's initial balance. *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      staking_balance'''
      (staking_balance'' -! user_balance)
  in
  (* Check that the delegated balance of [delegate] is null. *)
  let* () = check_delegated_balance_is ctxt ~loc:__LOC__ delegate Tez.zero in
  (* Fetch user's balance. *)
  let*@ _, user_balance' = Token.Internal_for_tests.balance ctxt user_account in
  (* Ensure user's balance is unchanged. *)
  Assert.equal_tez ~loc:__LOC__ user_balance' user_balance

(** Tests that the rpcs [contract/pkh/frozen_bonds] and
    [contract/pkh/balance_and_frozen_bonds] can be called successfully.
    These rpcs call the functions [Contract.get_frozen_bonds] and
    [Contract.get_balance_and_frozen_bonds] already tested in previous tests. *)
let test_rpcs () =
  let open Lwt_result_syntax in
  let* blk, contract = Context.init1 () in
  let* frozen_bonds = Context.Contract.frozen_bonds (B blk) contract in
  let* () = Assert.equal_tez ~loc:__LOC__ frozen_bonds Tez.zero in
  let* balance_and_frozen_bonds =
    Context.Contract.balance_and_frozen_bonds (B blk) contract
  in
  let* balance = Context.Contract.balance (B blk) contract in
  Assert.equal_tez ~loc:__LOC__ balance_and_frozen_bonds balance

(** A helper to test a particular delegation/freezing scenario *)
let test_scenario scenario =
  let open Lwt_result_wrap_syntax in
  let* ctxt, user_contract, user_account, delegate1 =
    init_test ~user_is_delegate:false
  in
  let delegate2, delegate_pk2, _ = Signature.generate_key () in
  let delegate_contract2 = Contract.Implicit delegate2 in
  let delegate_account2 = `Contract delegate_contract2 in
  let delegate_balance2 = big_random_amount () in
  let*@ ctxt, _ =
    Token.transfer ctxt `Minted delegate_account2 delegate_balance2
  in
  (* Configure delegate, as a delegate by self-delegation, for which
     revealing its manager key is a prerequisite. *)
  let*@ ctxt = Contract.reveal_manager_key ctxt delegate2 delegate_pk2 in
  let*@ ctxt = Contract.Delegate.set ctxt delegate_contract2 (Some delegate2) in
  let sc_rollup1, nonce = mk_sc_rollup () in
  let sc_rollup2, _ = mk_sc_rollup ~nonce () in
  let bond_id1 = Bond_id.Sc_rollup_bond_id sc_rollup1 in
  let bond_id2 = Bond_id.Sc_rollup_bond_id sc_rollup2 in
  let deposit_amount = Tez.of_mutez_exn 1000L in
  let deposit_account1 = `Frozen_bonds (user_contract, bond_id1) in
  let deposit_account2 = `Frozen_bonds (user_contract, bond_id2) in
  let do_delegate ?(delegate = delegate1) ctxt =
    (* Fetch staking balance before delegation *)
    let*@ staking_balance = Delegate.For_RPC.staking_balance ctxt delegate in
    (* Fetch user's initial balance before delegate. *)
    let*@ user_balance =
      Contract.get_balance_and_frozen_bonds ctxt user_contract
    in
    (* Let user delegate to "delegate". *)
    let*@ ctxt = Contract.Delegate.set ctxt user_contract (Some delegate) in
    (* Fetch staking balance after delegation  *)
    let*@ staking_balance' = Delegate.For_RPC.staking_balance ctxt delegate in
    let+ () =
      Assert.equal_tez
        ~loc:__LOC__
        staking_balance'
        (staking_balance +! user_balance)
    in
    (ctxt, user_balance)
  in
  let do_freeze ?(deposit_account = deposit_account1) ctxt =
    (* Fetch staking balance before freeze *)
    let*@ staking_balance1 = Delegate.For_RPC.staking_balance ctxt delegate1 in
    let*@ staking_balance2 = Delegate.For_RPC.staking_balance ctxt delegate2 in
    (* Freeze a sc-rollup deposit. *)
    let*@ ctxt, _ =
      Token.transfer ctxt user_account deposit_account deposit_amount
    in
    (* Fetch staking balance after freeze. *)
    let*@ staking_balance1' = Delegate.For_RPC.staking_balance ctxt delegate1 in
    let*@ staking_balance2' = Delegate.For_RPC.staking_balance ctxt delegate2 in
    (* Ensure staking balance did not change. *)
    let* () =
      Assert.equal_tez ~loc:__LOC__ staking_balance1' staking_balance1
    in
    let+ () =
      Assert.equal_tez ~loc:__LOC__ staking_balance2' staking_balance2
    in
    ctxt
  in
  let do_unfreeze ?(deposit_account = deposit_account1) ctxt =
    (* Fetch staking balance before unfreeze *)
    let*@ staking_balance1 = Delegate.For_RPC.staking_balance ctxt delegate1 in
    let*@ staking_balance2 = Delegate.For_RPC.staking_balance ctxt delegate2 in
    (* Unfreeze the deposit *)
    let*@ ctxt, _ =
      Token.transfer ctxt deposit_account user_account deposit_amount
    in
    (* Fetch staking balance after unfreeze. *)
    let*@ staking_balance1' = Delegate.For_RPC.staking_balance ctxt delegate1 in
    let*@ staking_balance2' = Delegate.For_RPC.staking_balance ctxt delegate2 in
    (* Ensure staking balance did not change. *)
    let* () =
      Assert.equal_tez ~loc:__LOC__ staking_balance1' staking_balance1
    in
    let+ () =
      Assert.equal_tez ~loc:__LOC__ staking_balance2' staking_balance2
    in
    ctxt
  in
  let do_slash ?(deposit_account = deposit_account1)
      ?(current_delegate = Some delegate1) ctxt =
    (* Fetch staking balance before slash *)
    let*@ staking_balance =
      match current_delegate with
      | None -> return Tez.zero
      | Some current_delegate ->
          Delegate.For_RPC.staking_balance ctxt current_delegate
    in
    (* Slash the deposit *)
    let*@ ctxt, _ =
      Token.transfer
        ctxt
        deposit_account
        `Sc_rollup_refutation_punishments
        deposit_amount
    in
    (* Fetch staking balance after slash. *)
    let+ () =
      match current_delegate with
      | None -> return_unit
      | Some current_delegate ->
          let*@ staking_balance' =
            Delegate.For_RPC.staking_balance ctxt current_delegate
          in
          (* Ensure balance slashed  *)
          Assert.equal_tez
            ~loc:__LOC__
            staking_balance'
            (staking_balance -! deposit_amount)
    in
    ctxt
  in
  let do_undelegate ?(delegate = delegate1) ctxt amount =
    (* Fetch staking balance before undelegate *)
    let*@ staking_balance = Delegate.For_RPC.staking_balance ctxt delegate in
    (* Fetch user's initial balance before undelegate. *)
    let*@ _, user_balance =
      Token.Internal_for_tests.balance ctxt user_account
    in
    (* Remove delegation. *)
    let*@ ctxt = Contract.Delegate.set ctxt user_contract None in
    (* Fetch staking balance after delegation removal. *)
    let*@ staking_balance' = Delegate.For_RPC.staking_balance ctxt delegate in
    (* Ensure staking balance decreased by delegation amount *)
    let* () =
      Assert.equal_tez ~loc:__LOC__ staking_balance' (staking_balance -! amount)
    in
    (* Fetch user's balance again. *)
    let*@ _, user_balance' =
      Token.Internal_for_tests.balance ctxt user_account
    in
    (* Ensure user's balance unchanged. *)
    let+ () = Assert.equal_tez ~loc:__LOC__ user_balance' user_balance in
    ctxt
  in
  let initial_ctxt = ctxt in
  (* delegate-then-freeze *)
  let* ctxt, amount_delegated = do_delegate ctxt in
  let* ctxt = do_freeze ctxt in
  let* () =
    scenario
      ctxt
      ~accounts:(deposit_account1, deposit_account2)
      ~delegates:(delegate1, delegate2)
      amount_delegated
      ~do_delegate
      ~do_undelegate
      ~do_freeze
      ~do_unfreeze
      ~do_slash
  in
  (* freeze-then-delegate *)
  let ctxt = initial_ctxt in
  let* ctxt = do_freeze ctxt in
  let* ctxt, amount_delegated = do_delegate ctxt in
  scenario
    ctxt
    ~accounts:(deposit_account1, deposit_account2)
    ~delegates:(delegate1, delegate2)
    amount_delegated
    ~do_delegate
    ~do_undelegate
    ~do_freeze
    ~do_unfreeze
    ~do_slash

let test_delegate_freeze_unfreeze_undelegate () =
  let open Lwt_result_syntax in
  test_scenario
    (fun
      ctxt
      ~accounts:_
      ~delegates:_
      amount_delegated
      ~do_delegate:_
      ~do_undelegate
      ~do_freeze:_
      ~do_unfreeze
      ~do_slash:_
    ->
      let* ctxt = do_unfreeze ctxt in
      let* (_ : context) = do_undelegate ctxt amount_delegated in
      return_unit)

let test_delegate_freeze_undelegate_unfreeze () =
  let open Lwt_result_syntax in
  test_scenario
    (fun
      ctxt
      ~accounts:_
      ~delegates:_
      amount_delegated
      ~do_delegate:_
      ~do_undelegate
      ~do_freeze:_
      ~do_unfreeze
      ~do_slash:_
    ->
      let* ctxt = do_undelegate ctxt amount_delegated in
      let* (_ : context) = do_unfreeze ctxt in
      return_unit)

let test_delegate_double_freeze_undelegate_unfreeze () =
  let open Lwt_result_syntax in
  test_scenario
    (fun
      ctxt
      ~accounts:(deposit_account1, deposit_account2)
      ~delegates:_
      amount_delegated
      ~do_delegate:_
      ~do_undelegate
      ~do_freeze
      ~do_unfreeze
      ~do_slash:_
    ->
      let* ctxt = do_freeze ~deposit_account:deposit_account2 ctxt in
      let* ctxt = do_undelegate ctxt amount_delegated in
      let* (_ : context) = do_unfreeze ~deposit_account:deposit_account1 ctxt in
      return_unit)

let test_delegate_freeze_redelegate_unfreeze () =
  let open Lwt_result_syntax in
  test_scenario
    (fun
      ctxt
      ~accounts:_
      ~delegates:(_delegate1, delegate2)
      _amount_delegated
      ~do_delegate
      ~do_undelegate
      ~do_freeze:_
      ~do_unfreeze
      ~do_slash:_
    ->
      let* ctxt, amount2 = do_delegate ~delegate:delegate2 ctxt in
      let* ctxt = do_unfreeze ctxt in
      let* (_ : context) = do_undelegate ~delegate:delegate2 ctxt amount2 in
      return_unit)

let test_delegate_freeze_unfreeze_freeze_redelegate () =
  let open Lwt_result_syntax in
  test_scenario
    (fun
      ctxt
      ~accounts:_
      ~delegates:(_delegate1, delegate2)
      _amount_delegated
      ~do_delegate
      ~do_undelegate
      ~do_freeze
      ~do_unfreeze
      ~do_slash:_
    ->
      let* ctxt = do_unfreeze ctxt in
      let* ctxt = do_freeze ctxt in
      let* ctxt, amount2 = do_delegate ~delegate:delegate2 ctxt in
      let* (_ : context) = do_undelegate ~delegate:delegate2 ctxt amount2 in
      return_unit)

let test_delegate_freeze_slash_undelegate () =
  let open Lwt_result_syntax in
  let slash_amount = Tez.of_mutez_exn 1000L in
  test_scenario
    (fun
      ctxt
      ~accounts:_
      ~delegates:_
      amount_delegated
      ~do_delegate:_
      ~do_undelegate
      ~do_freeze:_
      ~do_unfreeze:_
      ~do_slash
    ->
      let* ctxt = do_slash ctxt in
      let* (_ : context) =
        do_undelegate ctxt (amount_delegated -! slash_amount)
      in
      return_unit)

let tests =
  Tztest.
    [
      tztest
        "frozen bonds - delegate then freeze"
        `Quick
        test_delegate_then_freeze_deposit;
      tztest
        "frozen bonds - freeze then delegate"
        `Quick
        test_freeze_deposit_then_delegate;
      tztest
        "frozen bonds - contract remains allocated, user is not a delegate"
        `Quick
        (test_allocated_when_frozen_deposits_exists ~user_is_delegate:false);
      tztest
        "frozen bonds - contract remains allocated, user is a delegate"
        `Quick
        (test_allocated_when_frozen_deposits_exists ~user_is_delegate:true);
      tztest
        "frozen bonds - total stake, user is not a delegate"
        `Quick
        (test_total_stake ~user_is_delegate:false);
      tztest
        "frozen bonds - total stake, user is a delegate"
        `Quick
        (test_total_stake ~user_is_delegate:true);
      tztest "frozen bonds - delegated balance" `Quick test_delegated_balance;
      tztest "frozen bonds - test rpcs" `Quick test_rpcs;
      tztest
        "delegate, freeze, unfreeze, undelegate"
        `Quick
        test_delegate_freeze_unfreeze_undelegate;
      tztest
        "delegate, freeze, undelegate, unfreeze"
        `Quick
        test_delegate_freeze_undelegate_unfreeze;
      tztest
        "delegate, double freeze, undelegate, unfreeze"
        `Quick
        test_delegate_double_freeze_undelegate_unfreeze;
      tztest
        "delegate, freeze, redelegate, unfreeze"
        `Quick
        test_delegate_freeze_redelegate_unfreeze;
      tztest
        "delegate, freeze, unfreeze, freeze, redelegate"
        `Quick
        test_delegate_freeze_unfreeze_freeze_redelegate;
      tztest
        "delegate, freeze, slash, undelegate"
        `Quick
        test_delegate_freeze_slash_undelegate;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("frozen bonds", tests)]
  |> Lwt_main.run
