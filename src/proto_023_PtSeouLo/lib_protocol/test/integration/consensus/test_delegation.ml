(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:  Protocol (delegation)
    Invocation: dune exec src/proto_023_PtSeouLo/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_delegation.ml
    Subject:    - Properties on bootstrap contracts (self-delegation,
                cannot delete/change their delegate (as opposed to contracts
                not-being-delegate which can do these), bootstrap manager
                as delegate during origination).
    - Properties on delegation depending on whether delegate
                keys registration, through origination and delegation.
*)

open Protocol
open Alpha_context
open Tez_helpers

(*****************************************************************************)
(* Bootstrap contracts
   -------------------
   Bootstrap contracts are heavily used in other tests. It is helpful to test
   some properties of these contracts, so we can correctly interpret the other
   tests that use them. *)
(*****************************************************************************)

let expect_error err =
  let open Lwt_result_syntax in
  function
  | err0 :: _ when err = err0 -> return_unit
  | _ -> failwith "Unexpected successful result"

let expect_alpha_error err = expect_error (Environment.Ecoproto_error err)

let expect_no_change_registered_delegate_pkh pkh =
  let open Lwt_result_syntax in
  function
  | Environment.Ecoproto_error (Delegate_storage.Contract.No_deletion pkh0) :: _
    when pkh0 = pkh ->
      return_unit
  | _ -> failwith "Delegate can not be deleted and operation should fail."

let expect_too_low_balance_error i op =
  let open Lwt_result_syntax in
  let*! err = Incremental.add_operation i op in
  Assert.proto_error_with_info ~loc:__LOC__ err "Balance too low"

let expect_delegate_already_active_error i op =
  let open Lwt_result_syntax in
  let*! err = Incremental.add_operation i op in
  Assert.proto_error_with_info ~loc:__LOC__ err "Delegate already active"

(** Bootstrap contracts delegate to themselves. *)
let bootstrap_manager_is_bootstrap_delegate () =
  let open Lwt_result_syntax in
  let* b, bootstrap0 = Context.init1 () in
  let* delegate0 = Context.Contract.delegate (B b) bootstrap0 in
  let* manager0 = Context.Contract.manager (B b) bootstrap0 in
  Assert.equal_pkh ~loc:__LOC__ delegate0 manager0.pkh

(** Bootstrap contracts cannot change their delegate. *)
let bootstrap_delegate_cannot_change ~fee () =
  let open Lwt_result_syntax in
  let* b, (bootstrap0, bootstrap1) = Context.init2 () in
  let pkh1 = Context.Contract.pkh bootstrap0 in
  let* i = Incremental.begin_construction b ~policy:(Block.Excluding [pkh1]) in
  let* manager1 = Context.Contract.manager (I i) bootstrap1 in
  let* balance0 = Context.Contract.balance (I i) bootstrap0 in
  let* delegate0 = Context.Contract.delegate (I i) bootstrap0 in
  (* change delegation to bootstrap1 *)
  let* set_delegate = Op.delegation ~fee (I i) bootstrap0 (Some manager1.pkh) in
  if fee > balance0 then expect_too_low_balance_error i set_delegate
  else
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:
          (expect_no_change_registered_delegate_pkh delegate0)
        i
        set_delegate
    in
    let* b = Incremental.finalize_block i in
    (* bootstrap0 still has same delegate *)
    let* delegate0_after = Context.Contract.delegate (B b) bootstrap0 in
    let* () = Assert.equal_pkh ~loc:__LOC__ delegate0_after delegate0 in
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (B b) bootstrap0 balance0 fee

(** Bootstrap contracts cannot delete their delegation. *)
let bootstrap_delegate_cannot_be_removed ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let* balance = Context.Contract.balance (I i) bootstrap in
  let* delegate = Context.Contract.delegate (I i) bootstrap in
  let* manager = Context.Contract.manager (I i) bootstrap in
  (* remove delegation *)
  let* set_delegate = Op.delegation ~fee (I i) bootstrap None in
  if fee > balance then expect_too_low_balance_error i set_delegate
  else
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:
          (expect_no_change_registered_delegate_pkh manager.pkh)
        i
        set_delegate
    in
    (* delegate has not changed *)
    let* delegate_after = Context.Contract.delegate (I i) bootstrap in
    let* () = Assert.equal_pkh ~loc:__LOC__ delegate delegate_after in
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee

(** Contracts not registered as delegate can change their
    delegation. *)
let delegate_can_be_changed_from_unregistered_contract ~fee () =
  let open Lwt_result_syntax in
  let* b, (bootstrap0, bootstrap1) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let unregistered = Contract.Implicit unregistered_pkh in
  let* manager0 = Context.Contract.manager (B b) bootstrap0 in
  let* manager1 = Context.Contract.manager (B b) bootstrap1 in
  let credit = of_int 10 in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap0 unregistered credit
  in
  let* balance = Context.Contract.balance (B b) bootstrap0 in
  let* b = Block.bake b ~operation:credit_contract in
  (* delegate to bootstrap0 *)
  let* set_delegate =
    Op.delegation
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      unregistered
      (Some manager0.pkh)
  in
  let* b = Block.bake b ~operation:set_delegate in
  let* delegate = Context.Contract.delegate (B b) unregistered in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate manager0.pkh in
  (* change delegation to bootstrap1 *)
  let* change_delegate =
    Op.delegation ~force_reveal:true ~fee (B b) unregistered (Some manager1.pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > balance then expect_too_low_balance_error i change_delegate
  else
    let* i = Incremental.add_operation i change_delegate in
    (* delegate has changed *)
    let* delegate_after = Context.Contract.delegate (I i) unregistered in
    let* () = Assert.equal_pkh ~loc:__LOC__ delegate_after manager1.pkh in
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) unregistered credit fee

(** Contracts not registered as delegate can delete their
    delegation. *)
let delegate_can_be_removed_from_unregistered_contract ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let unregistered = Contract.Implicit unregistered_pkh in
  let* manager = Context.Contract.manager (B b) bootstrap in
  let credit = of_int 10 in
  let* credit_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      unregistered
      credit
  in
  let* balance = Context.Contract.balance (B b) bootstrap in
  let* b = Block.bake b ~operation:credit_contract in
  (* delegate to bootstrap *)
  let* set_delegate =
    Op.delegation
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      unregistered
      (Some manager.pkh)
  in
  let* b = Block.bake b ~operation:set_delegate in
  let* delegate = Context.Contract.delegate (B b) unregistered in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh in
  (* remove delegation *)
  let* delete_delegate = Op.delegation ~fee (B b) unregistered None in
  let* i = Incremental.begin_construction b in
  if fee > balance then expect_too_low_balance_error i delete_delegate
  else
    let* i = Incremental.add_operation i delete_delegate in
    (* the delegate has been removed *)
    let* () =
      let* pkh = Context.Contract.delegate_opt (I i) unregistered in
      match pkh with
      | None -> return_unit
      | Some _ -> failwith "Expected delegate to be removed"
    in
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) unregistered credit fee

(** Bootstrap keys are already registered as delegate keys. *)
let bootstrap_manager_already_registered_delegate ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let* manager = Context.Contract.manager (I i) bootstrap in
  let pkh = manager.pkh in
  let impl_contract = Contract.Implicit pkh in
  let* balance = Context.Contract.balance (I i) impl_contract in
  let* sec_reg = Op.delegation ~fee (I i) impl_contract (Some pkh) in
  if fee > balance then expect_too_low_balance_error i sec_reg
  else
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:(function
          | Environment.Ecoproto_error Delegate_storage.Contract.Active_delegate
            :: _ ->
              return_unit
          | _ ->
              failwith "Delegate is already active and operation should fail.")
        i
        sec_reg
    in
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee

(** Bootstrap manager can be set as delegate of an originated contract
    (through origination operation). *)
let delegate_to_bootstrap_by_origination ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let* manager = Context.Contract.manager (I i) bootstrap in
  let* balance = Context.Contract.balance (I i) bootstrap in
  (* originate a contract with bootstrap's manager as delegate *)
  let* op, orig_contract =
    Op.contract_origination
      ~fee
      ~credit:Tez.zero
      ~delegate:manager.pkh
      (I i)
      bootstrap
      ~script:Op.dummy_script
  in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (I i)
  in
  (* 0.257tz *)
  let*? origination_burn = cost_per_byte *? Int64.of_int origination_size in
  let*? t = fee +? origination_burn in
  let*? total_fee = Op.dummy_script_cost +? t in
  if fee > balance then expect_too_low_balance_error i op
  else if total_fee > balance && balance >= fee then
    (* origination did not proceed; fee has been debited *)
    let expect_apply_failure = function
      | Environment.Ecoproto_error err :: _ ->
          Assert.test_error_encodings err ;
          let error_info =
            Error_monad.find_info_of_error (Environment.wrap_tzerror err)
          in
          if String.equal error_info.title "Balance too low" then return_unit
          else failwith "unexpected failure"
      | _ ->
          failwith
            "Test_delegation.delegate_to_bootstrap_by_origination was expected \
             to fail but has not"
    in
    let* i = Incremental.add_operation i ~expect_apply_failure op in
    (* fee was taken *)
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    in
    (* originated contract has not been created *)
    let*! err = Context.Contract.balance (I i) orig_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)
  else
    (* bootstrap is delegate, fee + origination burn have been debited *)
    let* i = Incremental.add_operation i op in
    let* delegate = Context.Contract.delegate (I i) orig_contract in
    let* () = Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh in
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance total_fee

let undelegated_originated_bootstrap_contract () =
  let open Lwt_result_wrap_syntax in
  let* b, _contract =
    Context.init1
      ~bootstrap_contracts:
        [
          Parameters.
            {
              delegate = None;
              amount = Tez.zero;
              script = Op.dummy_script;
              hash = None;
            };
        ]
      ()
  in
  let* b = Block.bake b in
  (* We know the address of the first originated bootstrap contract because we know the bootstrap origination nonce. This address corresponds to the first TF vesting contract on mainnnet. *)
  let*?@ originated_bootstrap0 =
    Alpha_context.Contract.of_b58check "KT1WPEis2WhAc2FciM2tZVn8qe6pCBe9HkDp"
  in
  let* delegate0 = Context.Contract.delegate_opt (B b) originated_bootstrap0 in
  match delegate0 with
  | None -> return_unit
  | Some _ -> failwith "Bootstrap contract should be undelegated (%s)" __LOC__

let delegated_implicit_bootstrap_contract () =
  let open Lwt_result_wrap_syntax in
  let*? accounts = Account.generate_accounts 2 in
  let to_pkh, from_pkh =
    match accounts with
    | [account1; account2] -> (account1.pkh, account2.pkh)
    | _ -> assert false
  in
  let bootstrap_delegations = [None; Some to_pkh] in
  let bootstrap_accounts =
    Account.make_bootstrap_accounts ~bootstrap_delegations accounts
  in
  let* b = Block.genesis bootstrap_accounts in
  let* () =
    let* pkh = Context.Contract.delegate_opt (B b) (Implicit from_pkh) in
    match pkh with
    | Some pkh when pkh = to_pkh -> return_unit
    | Some _ | None ->
        failwith "Bootstrap contract should be delegated (%s)." __LOC__
  in
  (* Test delegation amount *)
  let* i = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt i in
  let*@ amount = Delegate.For_RPC.delegated_balance ctxt to_pkh in
  Assert.equal_tez ~loc:__LOC__ amount (Tez.of_mutez_exn 4_000_000_000_000L)

let tests_bootstrap_contracts =
  [
    Tztest.tztest
      "bootstrap contracts delegate to themselves"
      `Quick
      bootstrap_manager_is_bootstrap_delegate;
    Tztest.tztest
      "bootstrap contracts can change their delegate (small fee)"
      `Quick
      (bootstrap_delegate_cannot_change ~fee:Tez.one_mutez);
    Tztest.tztest
      "bootstrap contracts can change their delegate (max fee)"
      `Quick
      (bootstrap_delegate_cannot_change ~fee:max_tez);
    Tztest.tztest
      "bootstrap contracts cannot remove their delegation (small fee)"
      `Quick
      (bootstrap_delegate_cannot_be_removed ~fee:Tez.one_mutez);
    Tztest.tztest
      "bootstrap contracts cannot remove their delegation (max fee)"
      `Quick
      (bootstrap_delegate_cannot_be_removed ~fee:max_tez);
    Tztest.tztest
      "contracts not registered as delegate can change their delegation (small \
       fee)"
      `Quick
      (delegate_can_be_changed_from_unregistered_contract ~fee:Tez.one_mutez);
    Tztest.tztest
      "contracts not registered as delegate can change their delegation (max \
       fee)"
      `Quick
      (delegate_can_be_changed_from_unregistered_contract ~fee:max_tez);
    Tztest.tztest
      "contracts not registered as delegate can remove their delegation (small \
       fee)"
      `Quick
      (delegate_can_be_removed_from_unregistered_contract ~fee:Tez.one_mutez);
    Tztest.tztest
      "contracts not registered as delegate can remove their delegation (max \
       fee)"
      `Quick
      (delegate_can_be_removed_from_unregistered_contract ~fee:max_tez);
    Tztest.tztest
      "bootstrap keys are already registered as delegate keys (small fee)"
      `Quick
      (bootstrap_manager_already_registered_delegate ~fee:Tez.one_mutez);
    Tztest.tztest
      "bootstrap keys are already registered as delegate keys (max fee)"
      `Quick
      (bootstrap_manager_already_registered_delegate ~fee:max_tez);
    Tztest.tztest
      "bootstrap manager can be delegate (init origination, small fee)"
      `Quick
      (delegate_to_bootstrap_by_origination ~fee:Tez.one_mutez);
    (* balance enough for fee but not for fee + origination burn + dummy script storage cost *)
    Tztest.tztest
      "bootstrap manager can be delegate (init origination, edge case)"
      `Quick
      (delegate_to_bootstrap_by_origination
         ~fee:(Tez.of_mutez_exn 3_999_999_705_000L));
    (* fee bigger than bootstrap's initial balance*)
    Tztest.tztest
      "bootstrap manager can be delegate (init origination, large fee)"
      `Quick
      (delegate_to_bootstrap_by_origination
         ~fee:(Tez_helpers.of_int 10_000_000));
    Tztest.tztest
      "originated bootstrap contract can be undelegated"
      `Quick
      undelegated_originated_bootstrap_contract;
    Tztest.tztest
      "originated bootstrap contract can be delegated"
      `Quick
      delegated_implicit_bootstrap_contract;
  ]

(*****************************************************************************)
(* Delegate registration
   ---------------------
   A delegate is a pkh. Delegates must be registered. Registration is
   done via the self-delegation of the implicit contract corresponding
   to the pkh. The implicit contract must be credited when the
   self-delegation is done. Furthermore, trying to register an already
   registered key raises an error.

   In this series of tests, we verify that
   1- unregistered delegate keys cannot be delegated to,
   2- registered keys can be delegated to,
   3- registering an already registered key raises an error.

   We consider three scenarios for setting a delegate:
   - through origination,
   - through delegation when the implicit contract has no delegate yet,
   - through delegation when the implicit contract already has a delegate.

   We also test that emptying the implicit contract linked to a
   registered delegate key does not unregister the delegate key.

   Valid registration
   ------------------
   Unregistered key:
   - contract not credited and no self-delegation,
   - contract credited but no self-delegation,
   - contract not credited and self-delegation.

   Not credited:
   - no credit operation
   - credit operation of 1μꜩ and then debit operation of 1μꜩ *)
(*****************************************************************************)

(* Part A.
   Unregistered delegate keys cannot be used for delegation

   Two main series of tests: without self-delegation and with a failed attempt at self-delegation:

   1/ no self-delegation
     a/ no credit
   - no token transfer
   - credit of 1μꜩ and then debit of 1μꜩ
     b/ with credit of 1μꜩ.
       For every scenario, we try three different ways of delegating:
   - through origination (init origination)
   - through delegation when no delegate was assigned (init delegation)
   - through delegation when a delegate was assigned (switch delegation).

   2/ Self-delegation fails if the contract has no credit. We try the
   two possibilities of 1a for non-credited contracts. *)

let expect_unregistered_key pkh =
  let open Lwt_result_syntax in
  function
  | Environment.Ecoproto_error (Delegate_storage.Unregistered_delegate pkh0)
    :: _
    when pkh = pkh0 ->
      return_unit
  | _ -> failwith "Delegate key is not registered: operation should fail."

(* Part A. Section 1.
   No self-delegation. *)

(** No token transfer, no self-delegation.  Originated account. If
    fees are higher than balance, [Balance_too_low] is
    raised. Otherwise, it checks the correct exception is raised
    (unregistered key), and the fees are still debited. Using RPCs, we
    verify the contract has not been originated. *)
let test_unregistered_delegate_key_init_origination ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  (* origination with delegate argument *)
  let* op, orig_contract =
    Op.contract_origination
      ~fee
      ~delegate:unregistered_pkh
      (I i)
      bootstrap
      ~script:Op.dummy_script
  in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (I i)
  in
  let*? origination_burn = cost_per_byte *? Int64.of_int origination_size in
  let*? (_total_fee : Tez.t) = fee +? origination_burn in
  (* FIXME unused variable *)
  let* balance = Context.Contract.balance (I i) bootstrap in
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination did not proceed; fee has been debited *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:(expect_unregistered_key unregistered_pkh)
        i
        op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    in
    (* originated contract has not been created *)
    let*! err = Context.Contract.balance (I i) orig_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)

(** Delegation when delegate key is not assigned. Delegate account is
    initialized. If fees are higher than initial credit (10 tez),
    [Balance_too_low] is raised. Otherwise, fees are still debited. The
    implicit contract has no delegate. *)
let test_unregistered_delegate_key_init_delegation ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  let* credit_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      impl_contract
      credit
  in
  let* b = Block.bake b ~operation:credit_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit in
  (* try to delegate *)
  let* delegate_op =
    Op.delegation
      ~force_reveal:true
      ~fee
      (B b)
      impl_contract
      (Some unregistered_delegate_pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been debited; no delegate *)
    let* i =
      Incremental.add_operation
        i
        ~expect_apply_failure:
          (expect_unregistered_key unregistered_delegate_pkh)
        delegate_op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    in
    (* implicit contract has no delegate *)
    let*! err = Context.Contract.delegate (I i) impl_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)

(** Re-delegation when a delegate key was already assigned. If fees
    are higher than initial credit (10 tez), [Balance_too_low] is
    raised. Otherwise, fees are not debited and the implicit contract
    delegate remains unchanged. *)
let test_unregistered_delegate_key_switch_delegation ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  let* init_credit =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      impl_contract
      credit
  in
  let* b = Block.bake b ~operation:init_credit in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit in
  (* set and check the initial delegate *)
  let* delegate_op =
    Op.delegation
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      impl_contract
      (Some bootstrap_pkh)
  in
  let* b = Block.bake b ~operation:delegate_op in
  let* delegate_pkh = Context.Contract.delegate (B b) bootstrap in
  let* () = Assert.equal_pkh ~loc:__LOC__ bootstrap_pkh delegate_pkh in
  (* try to delegate *)
  let* delegate_op =
    Op.delegation ~fee (B b) impl_contract (Some unregistered_delegate_pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been debited; no delegate *)
    let* i =
      Incremental.add_operation
        i
        ~expect_apply_failure:
          (expect_unregistered_key unregistered_delegate_pkh)
        delegate_op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    in
    (* implicit contract delegate has not changed *)
    let* delegate_pkh_after = Context.Contract.delegate (I i) bootstrap in
    Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate_pkh_after

(** Same as [unregistered_delegate_key_init_origination] and credits
    [amount], no self-delegation. *)
let test_unregistered_delegate_key_init_origination_credit ~fee ~amount () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* credit + check balance *)
  let* create_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake b ~operation:create_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* origination with delegate argument *)
  let* balance = Context.Contract.balance (B b) bootstrap in
  let* op, orig_contract =
    Op.contract_origination
      ~fee
      ~delegate:unregistered_pkh
      (B b)
      bootstrap
      ~script:Op.dummy_script
  in
  let* i = Incremental.begin_construction b in
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination not done, fee taken *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:(expect_unregistered_key unregistered_pkh)
        i
        op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    in
    let*! err = Context.Contract.balance (I i) orig_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)

(** Same as [unregistered_delegate_key_init_delegation] and credits
    the amount [amount] of the implicit contract. *)
let test_unregistered_delegate_key_init_delegation_credit ~fee ~amount () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      impl_contract
      amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  let*? balance = credit +? amount in
  let* init_credit =
    Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  in
  let* b = Block.bake ~operation:init_credit b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract balance in
  (* try to delegate *)
  let* delegate_op =
    Op.delegation
      ~force_reveal:true
      ~fee
      (B b)
      impl_contract
      (Some unregistered_delegate_pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, no delegate for contract *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:
          (expect_unregistered_key unregistered_delegate_pkh)
        i
        delegate_op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    in
    let*! err = Context.Contract.delegate (I i) impl_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)

(** Same as in [unregistered_delegate_key_switch_delegation] and
    credits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_switch_delegation_credit ~fee ~amount () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      impl_contract
      amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  let*? balance = credit +? amount in
  let* init_credit =
    Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  in
  let* b = Block.bake ~operation:init_credit b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract balance in
  (* set and check the initial delegate *)
  let* delegate_op =
    Op.delegation
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      impl_contract
      (Some bootstrap_pkh)
  in
  let* b = Block.bake ~operation:delegate_op b in
  let* delegate_pkh = Context.Contract.delegate (B b) bootstrap in
  let* () = Assert.equal_pkh ~loc:__LOC__ bootstrap_pkh delegate_pkh in
  (* switch delegate through delegation *)
  let* delegate_op =
    Op.delegation ~fee (B b) impl_contract (Some unregistered_delegate_pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, delegate for contract has not changed *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:
          (expect_unregistered_key unregistered_delegate_pkh)
        i
        delegate_op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    in
    let* delegate = Context.Contract.delegate (I i) impl_contract in
    let* () =
      Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_delegate_pkh
    in
    Assert.equal_pkh ~loc:__LOC__ delegate bootstrap_pkh

(** A credit of some amount followed by a debit of the same amount,
    no self-delegation. *)
let test_unregistered_delegate_key_init_origination_credit_debit ~fee ~amount ()
    =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* credit + check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake b ~operation:create_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* debit + check balance *)
  let* debit_contract =
    Op.transaction ~force_reveal:true (B b) impl_contract bootstrap amount
  in
  let* b = Block.bake b ~operation:debit_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* origination with delegate argument *)
  let* balance = Context.Contract.balance (B b) bootstrap in
  let* op, orig_contract =
    Op.contract_origination
      ~fee
      ~delegate:unregistered_pkh
      (B b)
      bootstrap
      ~script:Op.dummy_script
  in
  let* i = Incremental.begin_construction b in
  if fee > balance then expect_too_low_balance_error i op
  else
    (* fee taken, origination not processed *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:(expect_unregistered_key unregistered_pkh)
        i
        op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    in
    let*! err = Context.Contract.balance (I i) orig_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)

(** Same as in [unregistered_delegate_key_init_delegation] but credits
    then debits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_init_delegation_credit_debit ~amount ~fee ()
    =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      impl_contract
      amount
  in
  let* b = Block.bake b ~operation:create_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* debit + check balance *)
  let* debit_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      impl_contract
      bootstrap
      amount
  in
  let* b = Block.bake b ~operation:debit_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  in
  let* b = Block.bake b ~operation:credit_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit in
  (* try to delegate *)
  let* delegate_op =
    Op.delegation
      ~force_reveal:true
      ~fee
      (B b)
      impl_contract
      (Some unregistered_delegate_pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, no delegate for contract *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:
          (expect_unregistered_key unregistered_delegate_pkh)
        i
        delegate_op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    in
    let*! err = Context.Contract.delegate (I i) impl_contract in
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)

(** Same as in [unregistered_delegate_key_switch_delegation] but
    credits then debits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_switch_delegation_credit_debit ~fee ~amount
    () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      bootstrap
      impl_contract
      amount
  in
  let* b = Block.bake b ~operation:create_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* debit + check balance *)
  let* debit_contract =
    Op.transaction ~force_reveal:true (B b) impl_contract bootstrap amount
  in
  let* b = Block.bake b ~operation:debit_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* delegation - initial credit for the delegated contract *)
  let credit = of_int 10 in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  in
  let* b = Block.bake b ~operation:credit_contract in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit in
  (* set and check the initial delegate *)
  let* delegate_op =
    Op.delegation
      ~force_reveal:true
      ~fee:Tez.zero
      (B b)
      impl_contract
      (Some bootstrap_pkh)
  in
  let* b = Block.bake b ~operation:delegate_op in
  let* delegate_pkh = Context.Contract.delegate (B b) bootstrap in
  let* () = Assert.equal_pkh ~loc:__LOC__ bootstrap_pkh delegate_pkh in
  (* switch delegate through delegation *)
  let* delegate_op =
    Op.delegation (B b) ~fee impl_contract (Some unregistered_delegate_pkh)
  in
  let* i = Incremental.begin_construction b in
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, delegate for contract has not changed *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:
          (expect_unregistered_key unregistered_delegate_pkh)
        i
        delegate_op
    in
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    in
    let* delegate = Context.Contract.delegate (I i) impl_contract in
    Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_delegate_pkh

(* Part A. Section 2.
   Self-delegation to an empty contract fails. *)

(** Self-delegation with zero-balance contract should fail. *)
let test_failed_self_delegation_no_transaction () =
  let open Lwt_result_syntax in
  let* b, _contract = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* check balance *)
  let* balance = Context.Contract.balance (I i) impl_contract in
  let* () = Assert.equal_tez ~loc:__LOC__ Tez.zero balance in
  (* self delegation fails *)
  let* self_delegation =
    Op.delegation (I i) impl_contract (Some unregistered_pkh)
  in
  let*! err = Incremental.add_operation i self_delegation in
  Assert.proto_error_with_info ~loc:__LOC__ err "Empty implicit contract"

(** Implicit contract is credited then debited of same amount (i.e.,
    is emptied). Self-delegation fails. *)
let test_failed_self_delegation_emptied_implicit_contract amount () =
  let open Lwt_result_syntax in
  (* create an implicit contract *)
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (*  credit implicit contract and check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* empty implicit contract and check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) impl_contract bootstrap amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* self delegation fails *)
  let* self_delegation =
    Op.delegation (B b) impl_contract (Some unregistered_pkh)
  in
  let* i = Incremental.begin_construction b in
  let*! err = Incremental.add_operation i self_delegation in
  Assert.proto_error_with_info ~loc:__LOC__ err "Empty implicit contract"

(** Implicit contract is credited with a non-zero quantity [amount]
    tz, then it is delegated. The operation of debit of [amount] tz
    should fail as the contract is already delegated. *)
let test_emptying_delegated_implicit_contract_fails amount () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let* bootstrap_manager = Context.Contract.manager (B b) bootstrap in
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* credit unregistered implicit contract and check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* delegate the contract to the bootstrap *)
  let* delegation =
    Op.delegation
      ~force_reveal:true
      (B b)
      impl_contract
      (Some bootstrap_manager.pkh)
  in
  let* b = Block.bake ~operation:delegation b in
  (* empty implicit contract and expect error since the contract is delegated *)
  let* create_contract = Op.transaction (B b) impl_contract bootstrap amount in
  let* i = Incremental.begin_construction b in
  let*! err = Incremental.add_operation i create_contract in
  Assert.proto_error_with_info
    ~loc:__LOC__
    err
    "Empty implicit delegated contract"

(* Part B.
   - Valid registration:
   - Credit implicit contract with some ꜩ + verification of balance
   - Self delegation + verification
   - Empty contract + verification of balance + verification of not being erased / self-delegation
   - Create delegator implicit contract w first implicit contract as delegate + verification of delegation. *)

(** Initialized account is credited of [amount] tz, then
    self-delegated. *)
let test_valid_delegate_registration_init_delegation_credit amount () =
  let open Lwt_result_syntax in
  (* create an implicit contract *)
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* self delegation + verification *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  let* delegate = Context.Contract.delegate (B b) impl_contract in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh in
  (* create an implicit contract with no delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  in
  let* b = Block.bake ~operation:credit_contract b in
  (* check no delegate for delegator contract *)
  let*! err = Context.Contract.delegate (B b) delegator in
  let* () =
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)
  in
  (* delegation to the newly registered key *)
  let* delegation =
    Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  in
  let* b = Block.bake ~operation:delegation b in
  (* check delegation *)
  let* delegator_delegate = Context.Contract.delegate (B b) delegator in
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(** Create an implicit contract, credits with [amount]
    tz. Self-delegates. Create another implicit contract with
    bootstrap as delegate. Re-delegate it to the first implicit
    contract. *)
let test_valid_delegate_registration_switch_delegation_credit amount () =
  let open Lwt_result_syntax in
  (* create an implicit contract *)
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* self delegation + verification *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  let* delegate = Context.Contract.delegate (B b) impl_contract in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh in
  (* create an implicit contract with bootstrap's account as delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  in
  let* b = Block.bake ~operation:credit_contract b in
  let* bootstrap_manager = Context.Contract.manager (B b) bootstrap in
  let* delegation =
    Op.delegation
      ~force_reveal:true
      (B b)
      delegator
      (Some bootstrap_manager.pkh)
  in
  let* b = Block.bake ~operation:delegation b in
  (* test delegate of new contract is bootstrap *)
  let* delegator_delegate = Context.Contract.delegate (B b) delegator in
  let* () =
    Assert.equal_pkh ~loc:__LOC__ delegator_delegate bootstrap_manager.pkh
  in
  (* delegation with newly registered key *)
  let* delegation = Op.delegation (B b) delegator (Some delegate_account.pkh) in
  let* b = Block.bake ~operation:delegation b in
  let* delegator_delegate = Context.Contract.delegate (B b) delegator in
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(** Create an implicit contract. *)
let test_valid_delegate_registration_init_delegation_credit_debit amount () =
  let open Lwt_result_syntax in
  (* create an implicit contract *)
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ+ check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* self delegation + verification *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  let* delegate = Context.Contract.delegate (B b) impl_contract in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate in
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  let* empty_contract = Op.transaction (B b) impl_contract bootstrap amount in
  let* b = Block.bake ~operation:empty_contract b in
  (* impl_contract is empty *)
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* verify self-delegation after contract is emptied *)
  let* delegate = Context.Contract.delegate (B b) impl_contract in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate in
  (* create an implicit contract with no delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  in
  let* b = Block.bake ~operation:credit_contract b in
  (* check no delegate for delegator contract *)
  let*! err = Context.Contract.delegate (B b) delegator in
  let* () =
    Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)
  in
  (* delegation to the newly registered key *)
  let* delegation =
    Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  in
  let* b = Block.bake ~operation:delegation b in
  (* check delegation *)
  let* delegator_delegate = Context.Contract.delegate (B b) delegator in
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(** A created implicit contract is credited with [amount] tz, then is
    self-delegated. It is emptied (fund back into bootstrap), and
    should remain existing (as registered as delegate). Another created
    implicit contract is delegated to bootstrap, then should be able to
    be re-delegated to the latter contract. *)
let test_valid_delegate_registration_switch_delegation_credit_debit amount () =
  let open Lwt_result_syntax in
  (* create an implicit contract *)
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  let* create_contract =
    Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount in
  (* self delegation + verification *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  let* delegate = Context.Contract.delegate (B b) impl_contract in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate in
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  let* empty_contract = Op.transaction (B b) impl_contract bootstrap amount in
  let* b = Block.bake ~operation:empty_contract b in
  (* impl_contract is empty *)
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* create an implicit contract with bootstrap's account as delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  let* credit_contract =
    Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  in
  let* b = Block.bake ~operation:credit_contract b in
  let* bootstrap_manager = Context.Contract.manager (B b) bootstrap in
  let* delegation =
    Op.delegation
      ~force_reveal:true
      (B b)
      delegator
      (Some bootstrap_manager.pkh)
  in
  let* b = Block.bake ~operation:delegation b in
  (* test delegate of new contract is bootstrap *)
  let* delegator_delegate = Context.Contract.delegate (B b) delegator in
  let* () =
    Assert.equal_pkh ~loc:__LOC__ delegator_delegate bootstrap_manager.pkh
  in
  (* delegation with newly registered key *)
  let* delegation =
    Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  in
  let* b = Block.bake ~operation:delegation b in
  let* delegator_delegate = Context.Contract.delegate (B b) delegator in
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(* Part C.
   A second self-delegation should raise an [Active_delegate] error. *)

(** Second self-delegation should fail with implicit contract with
    some credit. *)
let test_double_registration () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit pkh in
  (* credit 1μꜩ+ check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      (B b)
      bootstrap
      impl_contract
      Tez.one_mutez
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez in
  (* self-delegation *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  (* second self-delegation *)
  let* second_registration = Op.delegation (B b) impl_contract (Some pkh) in
  let* i = Incremental.begin_construction b in
  expect_delegate_already_active_error i second_registration

(** Second self-delegation should fail with implicit contract emptied
    after first self-delegation. *)
let test_double_registration_when_empty () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit pkh in
  (* credit 1μꜩ+ check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      (B b)
      bootstrap
      impl_contract
      Tez.one_mutez
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez in
  (* self delegation *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  (* empty the delegate account *)
  let* empty_contract =
    Op.transaction (B b) impl_contract bootstrap Tez.one_mutez
  in
  let* b = Block.bake ~operation:empty_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* second self-delegation *)
  let* second_registration = Op.delegation (B b) impl_contract (Some pkh) in
  let* i = Incremental.begin_construction b in
  expect_delegate_already_active_error i second_registration

(** Second self-delegation should fail with implicit contract emptied
    then credited back after first self-delegation. *)
let test_double_registration_when_recredited () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit pkh in
  (* credit 1μꜩ+ check balance *)
  let* create_contract =
    Op.transaction
      ~force_reveal:true
      (B b)
      bootstrap
      impl_contract
      Tez.one_mutez
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez in
  (* self delegation *)
  let* self_delegation =
    Op.delegation ~force_reveal:true (B b) impl_contract (Some pkh)
  in
  let* b = Block.bake ~operation:self_delegation b in
  (* empty the delegate account *)
  let* empty_contract =
    Op.transaction
      ~force_reveal:true
      (B b)
      impl_contract
      bootstrap
      Tez.one_mutez
  in
  let* b = Block.bake ~operation:empty_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero in
  (* credit 1μꜩ+ check balance *)
  let* create_contract =
    Op.transaction (B b) bootstrap impl_contract Tez.one_mutez
  in
  let* b = Block.bake ~operation:create_contract b in
  let* () = Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez in
  (* second self-delegation *)
  let* second_registration = Op.delegation (B b) impl_contract (Some pkh) in
  let* i = Incremental.begin_construction b in
  expect_delegate_already_active_error i second_registration

(** Self-delegation on unrevealed contract. *)
let test_unregistered_and_unrevealed_self_delegate_key_init_delegation ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let {Account.pkh; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; _} = Account.new_account () in
  let contract = Alpha_context.Contract.Implicit pkh in
  let* operation =
    Op.transaction ~force_reveal:true (B b) bootstrap contract (of_int 10)
  in
  let* b = Block.bake ~operation b in
  let* op =
    Op.delegation ~fee ~force_reveal:true (B b) contract (Some delegate_pkh)
  in
  let* balance = Context.Contract.balance (B b) contract in
  let* i = Incremental.begin_construction b in
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination did not proceed; fee has been debited *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:(expect_unregistered_key delegate_pkh)
        i
        op
    in
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee

(** Self-delegation on revealed but not registered contract. *)
let test_unregistered_and_revealed_self_delegate_key_init_delegation ~fee () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let {Account.pkh; pk; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; _} = Account.new_account () in
  let contract = Alpha_context.Contract.Implicit pkh in
  let* operation = Op.transaction (B b) bootstrap contract (of_int 10) in
  let* b = Block.bake ~operation b in
  let* operation = Op.revelation (B b) pk in
  let* b = Block.bake ~operation b in
  let* op = Op.delegation ~fee (B b) contract (Some delegate_pkh) in
  let* balance = Context.Contract.balance (B b) contract in
  let* i = Incremental.begin_construction b in
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination did not proceed; fee has been debited *)
    let* i =
      Incremental.add_operation
        ~expect_apply_failure:(expect_unregistered_key delegate_pkh)
        i
        op
    in
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee

(** Self-delegation emptying a fresh contract. *)
let test_self_delegation_emptying_contract () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let {Account.pkh; pk; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; _} = Account.new_account () in
  let contract = Alpha_context.Contract.Implicit pkh in
  let amount = of_int 10 in
  let* operation = Op.transaction (B b) bootstrap contract amount in
  let* b = Block.bake ~operation b in
  let* operation = Op.revelation ~fee:Tez.zero (B b) pk in
  let* b = Block.bake ~operation b in
  let* op = Op.delegation ~fee:amount (B b) contract (Some delegate_pkh) in
  let* () =
    let* is_revealed =
      Context.Contract.is_manager_key_revealed (B b) contract
    in
    match is_revealed with
    | false -> failwith "contract should exist"
    | true -> return_unit
  in
  let* i = Incremental.begin_construction b in
  (* The delegation operation should be applied and the fees
     debited but it is expected to fail in the apply-part. *)
  let* i =
    Incremental.add_operation ~expect_apply_failure:(fun _ -> return_unit) i op
  in
  let* is_revealed = Context.Contract.is_manager_key_revealed (I i) contract in
  match is_revealed with
  | false -> return_unit
  | true -> failwith "contract should have been removed"

(** Self-delegation on revealed and registered contract. *)
let test_registered_self_delegate_key_init_delegation () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold_size:0 () in
  let {Account.pkh; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; pk = delegate_pk; _} =
    Account.new_account ()
  in
  let contract = Alpha_context.Contract.Implicit pkh in
  let delegate_contract = Alpha_context.Contract.Implicit delegate_pkh in
  let* operation =
    Op.transaction ~force_reveal:true (B b) bootstrap contract (of_int 10)
  in
  let* b = Block.bake ~operation b in
  let* operation =
    Op.transaction (B b) bootstrap delegate_contract (of_int 1)
  in
  let* b = Block.bake ~operation b in
  let* operation = Op.revelation (B b) delegate_pk in
  let* b = Block.bake ~operation b in
  let* operation = Op.delegation (B b) delegate_contract (Some delegate_pkh) in
  let* b = Block.bake ~operation b in
  let* operation =
    Op.delegation ~force_reveal:true (B b) contract (Some delegate_pkh)
  in
  let* b = Block.bake ~operation b in
  let* delegate = Context.Contract.delegate (B b) contract in
  let* () = Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh in
  return_unit

let test_bls_account_self_delegate ~allow_tz4_delegate_enable () =
  let open Lwt_result_syntax in
  let* b, bootstrap =
    Context.init1 ~consensus_threshold_size:0 ~allow_tz4_delegate_enable ()
  in
  let {Account.pkh = tz4_pkh; pk = tz4_pk; _} =
    Account.new_account ~algo:Bls ()
  in
  let tz4_contract = Alpha_context.Contract.Implicit tz4_pkh in
  let* operation =
    Op.transaction
      ~force_reveal:true
      (B b)
      bootstrap
      tz4_contract
      (of_int 200_000)
  in
  let* b = Block.bake ~operation b in
  let* operation = Op.revelation (B b) tz4_pk in
  let* b = Block.bake ~operation b in
  let* operation = Op.delegation (B b) tz4_contract (Some tz4_pkh) in
  let* inc = Incremental.begin_construction b in
  let tz4_pkh = match tz4_pkh with Bls pkh -> pkh | _ -> assert false in
  if allow_tz4_delegate_enable then
    let* (_i : Incremental.t) = Incremental.validate_operation inc operation in
    return_unit
  else
    let expect_failure = function
      | [
          Environment.Ecoproto_error
            (Contract_delegate_storage.Forbidden_tz4_delegate pkh);
        ]
        when Signature.Bls.Public_key_hash.(pkh = tz4_pkh) ->
          return_unit
      | err ->
          failwith
            "Error trace:@,\
             %a does not match the \
             [Contract_delegate_storage.Forbidden_tz4_delegate] error"
            Error_monad.pp_print_trace
            err
    in
    let* (_i : Incremental.t) =
      Incremental.validate_operation ~expect_failure inc operation
    in
    return_unit

let tests_delegate_registration =
  [
    (*** unregistered delegate key: no self-delegation ***)
    (* no token transfer, no self-delegation *)
    Tztest.tztest
      "unregistered delegate key (origination, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key (origination, edge case fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination ~fee:(of_int 3_999_488));
    Tztest.tztest
      "unregistered delegate key (origination, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination ~fee:(of_int 10_000_000));
    Tztest.tztest
      "unregistered delegate key (init with delegation, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key (init with delegation, max fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation ~fee:max_tez);
    Tztest.tztest
      "unregistered delegate key (switch with delegation, small fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key (switch with delegation, max fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation ~fee:max_tez);
    (* credit/debit 1μꜩ, no self-delegation *)
    Tztest.tztest
      "unregistered delegate key - credit/debit 1μꜩ (origination, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit_debit
         ~fee:Tez.one_mutez
         ~amount:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit/debit 1μꜩ (origination, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit_debit
         ~fee:max_tez
         ~amount:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit/debit 1μꜩ (init with delegation, \
       small fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit/debit 1μꜩ (init with delegation, \
       large fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:max_tez);
    Tztest.tztest
      "unregistered delegate key - credit/debit 1μꜩ (switch with delegation, \
       small fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit/debit 1μꜩ (switch with delegation, \
       large fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:max_tez);
    (* credit 1μꜩ, no self-delegation *)
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (origination, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit
         ~fee:Tez.one_mutez
         ~amount:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (origination, edge case fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit
         ~fee:(of_int 3_999_488)
         ~amount:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (origination, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit
         ~fee:(of_int 10_000_000)
         ~amount:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (init with delegation, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (init with delegation, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:max_tez);
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (switch with delegation, small \
       fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered delegate key - credit 1μꜩ (switch with delegation, large \
       fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:max_tez);
    (* self delegation on unrevealed and unregistered contract *)
    Tztest.tztest
      "unregistered and unrevealed self-delegation (small fee)"
      `Quick
      (test_unregistered_and_unrevealed_self_delegate_key_init_delegation
         ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered and unrevealed self-delegation (large fee)"
      `Quick
      (test_unregistered_and_unrevealed_self_delegate_key_init_delegation
         ~fee:max_tez);
    (* self delegation on unregistered contract *)
    Tztest.tztest
      "unregistered and revealed self-delegation (small fee)"
      `Quick
      (test_unregistered_and_revealed_self_delegate_key_init_delegation
         ~fee:Tez.one_mutez);
    Tztest.tztest
      "unregistered and revealed self-delegation  large fee)"
      `Quick
      (test_unregistered_and_revealed_self_delegate_key_init_delegation
         ~fee:max_tez);
    Tztest.tztest
      "unregistered and revealed self-delegation (fee = balance)"
      `Quick
      test_self_delegation_emptying_contract;
    (* self delegation on registered contract *)
    Tztest.tztest
      "registered and revealed self-delegation"
      `Quick
      test_registered_self_delegate_key_init_delegation;
    (*** unregistered delegate key: failed self-delegation ***)
    (* no token transfer, self-delegation *)
    Tztest.tztest
      "failed self-delegation: no transaction"
      `Quick
      test_failed_self_delegation_no_transaction;
    (* credit 1μtz, debit 1μtz, self-delegation *)
    Tztest.tztest
      "failed self-delegation: credit & debit 1μꜩ"
      `Quick
      (test_failed_self_delegation_emptied_implicit_contract Tez.one_mutez);
    (* credit 1μtz, delegate, debit 1μtz *)
    Tztest.tztest
      "empty delegated contract is not deleted: credit 1μꜩ, delegate & debit \
       1μꜩ"
      `Quick
      (test_emptying_delegated_implicit_contract_fails Tez.one_mutez);
    Tztest.tztest
      "failed BLS self delegation (allow_tz4_delegate_enable:false)"
      `Quick
      (test_bls_account_self_delegate ~allow_tz4_delegate_enable:false);
    (*** valid registration ***)
    (* valid registration: credit 1 μꜩ, self delegation *)
    Tztest.tztest
      "valid delegate registration: credit 1μꜩ, self delegation (init with \
       delegation)"
      `Quick
      (test_valid_delegate_registration_init_delegation_credit Tez.one_mutez);
    Tztest.tztest
      "valid delegate registration: credit 1μꜩ, self delegation (switch with \
       delegation)"
      `Quick
      (test_valid_delegate_registration_switch_delegation_credit Tez.one_mutez);
    (* valid registration: credit 1 μꜩ, self delegation, debit 1μꜩ *)
    Tztest.tztest
      "valid delegate registration: credit 1μꜩ, self delegation, debit 1μꜩ \
       (init with delegation)"
      `Quick
      (test_valid_delegate_registration_init_delegation_credit_debit
         Tez.one_mutez);
    Tztest.tztest
      "valid delegate registration: credit 1μꜩ, self delegation, debit 1μꜩ \
       (switch with delegation)"
      `Quick
      (test_valid_delegate_registration_switch_delegation_credit_debit
         Tez.one_mutez);
    (*** double registration ***)
    Tztest.tztest "double registration" `Quick test_double_registration;
    Tztest.tztest
      "double registration when delegate account is emptied"
      `Quick
      test_double_registration_when_empty;
    Tztest.tztest
      "double registration when delegate account is emptied and then recredited"
      `Quick
      test_double_registration_when_recredited;
    Tztest.tztest
      "valid BLS self delegation (allow_tz4_delegate_enable:true)"
      `Quick
      (test_bls_account_self_delegate ~allow_tz4_delegate_enable:true);
  ]

(******************************************************************************)
(* Main                                                                       *)
(******************************************************************************)

let tests = tests_bootstrap_contracts @ tests_delegate_registration

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("delegation", tests)]
  |> Lwt_main.run
