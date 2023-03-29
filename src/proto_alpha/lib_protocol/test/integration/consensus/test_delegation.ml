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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe
    Subject:    - Properties on bootstrap contracts (self-delegation,
                cannot delete/change their delegate (as opposed to contracts
                not-being-delegate which can do these), bootstrap manager
                as delegate during origination).
    - Properties on delegation depending on whether delegate
                keys registration, through origination and delegation.
*)

open Protocol
open Alpha_context
open Test_tez

(*****************************************************************************)
(* Bootstrap contracts
   -------------------
   Bootstrap contracts are heavily used in other tests. It is helpful to test
   some properties of these contracts, so we can correctly interpret the other
   tests that use them. *)
(*****************************************************************************)

let expect_error err = function
  | err0 :: _ when err = err0 -> return_unit
  | _ -> failwith "Unexpected successful result"

let expect_alpha_error err = expect_error (Environment.Ecoproto_error err)

let expect_no_change_registered_delegate_pkh pkh = function
  | Environment.Ecoproto_error (Delegate_storage.Contract.No_deletion pkh0) :: _
    when pkh0 = pkh ->
      return_unit
  | _ -> failwith "Delegate can not be deleted and operation should fail."

let expect_too_low_balance_error i op =
  Incremental.add_operation i op >>= fun err ->
  Assert.proto_error_with_info ~loc:__LOC__ err "Balance too low"

let expect_delegate_already_active_error i op =
  Incremental.add_operation i op >>= fun err ->
  Assert.proto_error_with_info ~loc:__LOC__ err "Delegate already active"

(** Bootstrap contracts delegate to themselves. *)
let bootstrap_manager_is_bootstrap_delegate () =
  Context.init1 () >>=? fun (b, bootstrap0) ->
  Context.Contract.delegate (B b) bootstrap0 >>=? fun delegate0 ->
  Context.Contract.manager (B b) bootstrap0 >>=? fun manager0 ->
  Assert.equal_pkh ~loc:__LOC__ delegate0 manager0.pkh

(** Bootstrap contracts cannot change their delegate. *)
let bootstrap_delegate_cannot_change ~fee () =
  Context.init2 () >>=? fun (b, (bootstrap0, bootstrap1)) ->
  let pkh1 = Context.Contract.pkh bootstrap0 in
  Incremental.begin_construction b ~policy:(Block.Excluding [pkh1])
  >>=? fun i ->
  Context.Contract.manager (I i) bootstrap1 >>=? fun manager1 ->
  Context.Contract.balance (I i) bootstrap0 >>=? fun balance0 ->
  Context.Contract.delegate (I i) bootstrap0 >>=? fun delegate0 ->
  (* change delegation to bootstrap1 *)
  Op.delegation ~fee (I i) bootstrap0 (Some manager1.pkh)
  >>=? fun set_delegate ->
  if fee > balance0 then expect_too_low_balance_error i set_delegate
  else
    Incremental.add_operation
      ~expect_apply_failure:(expect_no_change_registered_delegate_pkh delegate0)
      i
      set_delegate
    >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* bootstrap0 still has same delegate *)
    Context.Contract.delegate (B b) bootstrap0 >>=? fun delegate0_after ->
    Assert.equal_pkh ~loc:__LOC__ delegate0_after delegate0 >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (B b) bootstrap0 balance0 fee

(** Bootstrap contracts cannot delete their delegation. *)
let bootstrap_delegate_cannot_be_removed ~fee () =
  Context.init1 () >>=? fun (b, bootstrap) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  Context.Contract.delegate (I i) bootstrap >>=? fun delegate ->
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  (* remove delegation *)
  Op.delegation ~fee (I i) bootstrap None >>=? fun set_delegate ->
  if fee > balance then expect_too_low_balance_error i set_delegate
  else
    Incremental.add_operation
      ~expect_apply_failure:
        (expect_no_change_registered_delegate_pkh manager.pkh)
      i
      set_delegate
    >>=? fun i ->
    (* delegate has not changed *)
    Context.Contract.delegate (I i) bootstrap >>=? fun delegate_after ->
    Assert.equal_pkh ~loc:__LOC__ delegate delegate_after >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee

(** Contracts not registered as delegate can change their
    delegation. *)
let delegate_can_be_changed_from_unregistered_contract ~fee () =
  Context.init2 ~consensus_threshold:0 ()
  >>=? fun (b, (bootstrap0, bootstrap1)) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let unregistered = Contract.Implicit unregistered_pkh in
  Context.Contract.manager (B b) bootstrap0 >>=? fun manager0 ->
  Context.Contract.manager (B b) bootstrap1 >>=? fun manager1 ->
  let credit = of_int 10 in
  Op.transaction ~fee:Tez.zero (B b) bootstrap0 unregistered credit
  >>=? fun credit_contract ->
  Context.Contract.balance (B b) bootstrap0 >>=? fun balance ->
  Block.bake b ~operation:credit_contract >>=? fun b ->
  (* delegate to bootstrap0 *)
  Op.delegation
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    unregistered
    (Some manager0.pkh)
  >>=? fun set_delegate ->
  Block.bake b ~operation:set_delegate >>=? fun b ->
  Context.Contract.delegate (B b) unregistered >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate manager0.pkh >>=? fun () ->
  (* change delegation to bootstrap1 *)
  Op.delegation ~force_reveal:true ~fee (B b) unregistered (Some manager1.pkh)
  >>=? fun change_delegate ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > balance then expect_too_low_balance_error i change_delegate
  else
    Incremental.add_operation i change_delegate >>=? fun i ->
    (* delegate has changed *)
    Context.Contract.delegate (I i) unregistered >>=? fun delegate_after ->
    Assert.equal_pkh ~loc:__LOC__ delegate_after manager1.pkh >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) unregistered credit fee

(** Contracts not registered as delegate can delete their
    delegation. *)
let delegate_can_be_removed_from_unregistered_contract ~fee () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let unregistered = Contract.Implicit unregistered_pkh in
  Context.Contract.manager (B b) bootstrap >>=? fun manager ->
  let credit = of_int 10 in
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    unregistered
    credit
  >>=? fun credit_contract ->
  Context.Contract.balance (B b) bootstrap >>=? fun balance ->
  Block.bake b ~operation:credit_contract >>=? fun b ->
  (* delegate to bootstrap *)
  Op.delegation
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    unregistered
    (Some manager.pkh)
  >>=? fun set_delegate ->
  Block.bake b ~operation:set_delegate >>=? fun b ->
  Context.Contract.delegate (B b) unregistered >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun () ->
  (* remove delegation *)
  Op.delegation ~fee (B b) unregistered None >>=? fun delete_delegate ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > balance then expect_too_low_balance_error i delete_delegate
  else
    Incremental.add_operation i delete_delegate >>=? fun i ->
    (* the delegate has been removed *)
    (Context.Contract.delegate_opt (I i) unregistered >>=? function
     | None -> return_unit
     | Some _ -> failwith "Expected delegate to be removed")
    >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) unregistered credit fee

(** Bootstrap keys are already registered as delegate keys. *)
let bootstrap_manager_already_registered_delegate ~fee () =
  Context.init1 () >>=? fun (b, bootstrap) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  let pkh = manager.pkh in
  let impl_contract = Contract.Implicit pkh in
  Context.Contract.balance (I i) impl_contract >>=? fun balance ->
  Op.delegation ~fee (I i) impl_contract (Some pkh) >>=? fun sec_reg ->
  if fee > balance then expect_too_low_balance_error i sec_reg
  else
    Incremental.add_operation
      ~expect_apply_failure:(function
        | Environment.Ecoproto_error Delegate_storage.Contract.Active_delegate
          :: _ ->
            return_unit
        | _ -> failwith "Delegate is already active and operation should fail.")
      i
      sec_reg
    >>=? fun i ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee

(** Bootstrap manager can be set as delegate of an originated contract
    (through origination operation). *)
let delegate_to_bootstrap_by_origination ~fee () =
  Context.init1 () >>=? fun (b, bootstrap) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  (* originate a contract with bootstrap's manager as delegate *)
  Op.contract_origination
    ~fee
    ~credit:Tez.zero
    ~delegate:manager.pkh
    (I i)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  Context.get_constants (I i)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  (* 0.257tz *)
  cost_per_byte *? Int64.of_int origination_size >>?= fun origination_burn ->
  fee +? origination_burn >>? ( +? ) Op.dummy_script_cost >>?= fun total_fee ->
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
    Incremental.add_operation i ~expect_apply_failure op >>=? fun i ->
    (* fee was taken *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    (* originated contract has not been created *)
    Context.Contract.balance (I i) orig_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)
  else
    (* bootstrap is delegate, fee + origination burn have been debited *)
    Incremental.add_operation i op >>=? fun i ->
    Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
    Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun () ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance total_fee

let undelegated_originated_bootstrap_contract () =
  Context.init1
    ~bootstrap_contracts:
      [
        Parameters.{delegate = None; amount = Tez.zero; script = Op.dummy_script};
      ]
    ()
  >>=? fun (b, _contract) ->
  Block.bake b >>=? fun b ->
  (* We know the address of the first originated bootstrap contract because we know the bootstrap origination nonce. This address corresponds to the first TF vesting contract on mainnnet. *)
  Lwt.return @@ Environment.wrap_tzresult
  @@ Alpha_context.Contract.of_b58check "KT1WPEis2WhAc2FciM2tZVn8qe6pCBe9HkDp"
  >>=? fun originated_bootstrap0 ->
  Context.Contract.delegate_opt (B b) originated_bootstrap0
  >>=? fun delegate0 ->
  match delegate0 with
  | None -> return_unit
  | Some _ -> failwith "Bootstrap contract should be undelegated (%s)" __LOC__

let delegated_implicit_bootstrap_contract () =
  Account.generate_accounts 2 >>?= fun accounts ->
  let to_pkh, from_pkh =
    match accounts with
    | [account1; account2] -> (account1.pkh, account2.pkh)
    | _ -> assert false
  in
  let bootstrap_delegations = [None; Some to_pkh] in
  let bootstrap_accounts =
    Account.make_bootstrap_accounts ~bootstrap_delegations accounts
  in
  Block.genesis bootstrap_accounts >>=? fun b ->
  (Context.Contract.delegate_opt (B b) (Implicit from_pkh) >>=? function
   | Some pkh when pkh = to_pkh -> return_unit
   | Some _ | None ->
       failwith "Bootstrap contract should be delegated (%s)." __LOC__)
  >>=? fun () ->
  (* Test delegation amount *)
  Incremental.begin_construction b >>=? fun i ->
  let ctxt = Incremental.alpha_ctxt i in
  Delegate.delegated_balance ctxt to_pkh >|= Environment.wrap_tzresult
  >>=? fun amount ->
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
      (delegate_to_bootstrap_by_origination ~fee:(Test_tez.of_int 10_000_000));
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

let expect_unregistered_key pkh = function
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
  Context.init1 () >>=? fun (b, bootstrap) ->
  Incremental.begin_construction b >>=? fun i ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  (* origination with delegate argument *)
  Op.contract_origination
    ~fee
    ~delegate:unregistered_pkh
    (I i)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  Context.get_constants (I i)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  cost_per_byte *? Int64.of_int origination_size >>?= fun origination_burn ->
  fee +? origination_burn >>?= fun (_total_fee : Tez.t) ->
  (* FIXME unused variable *)
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination did not proceed; fee has been debited *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_pkh)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    (* originated contract has not been created *)
    Context.Contract.balance (I i) orig_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)

(** Delegation when delegate key is not assigned. Delegate account is
    initialized. If fees are higher than initial credit (10 tez),
    [Balance_too_low] is raised. Otherwise, fees are still debited. The
    implicit contract has no delegate. *)
let test_unregistered_delegate_key_init_delegation ~fee () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    impl_contract
    credit
  >>=? fun credit_contract ->
  Block.bake b ~operation:credit_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit >>=? fun () ->
  (* try to delegate *)
  Op.delegation
    ~force_reveal:true
    ~fee
    (B b)
    impl_contract
    (Some unregistered_delegate_pkh)
  >>=? fun delegate_op ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been debited; no delegate *)
    Incremental.add_operation
      i
      ~expect_apply_failure:(expect_unregistered_key unregistered_delegate_pkh)
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    (* implicit contract has no delegate *)
    Context.Contract.delegate (I i) impl_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)

(** Re-delegation when a delegate key was already assigned. If fees
    are higher than initial credit (10 tez), [Balance_too_low] is
    raised. Otherwise, fees are not debited and the implicit contract
    delegate remains unchanged. *)
let test_unregistered_delegate_key_switch_delegation ~fee () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    impl_contract
    credit
  >>=? fun init_credit ->
  Block.bake b ~operation:init_credit >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit >>=? fun () ->
  (* set and check the initial delegate *)
  Op.delegation
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    impl_contract
    (Some bootstrap_pkh)
  >>=? fun delegate_op ->
  Block.bake b ~operation:delegate_op >>=? fun b ->
  Context.Contract.delegate (B b) bootstrap >>=? fun delegate_pkh ->
  Assert.equal_pkh ~loc:__LOC__ bootstrap_pkh delegate_pkh >>=? fun () ->
  (* try to delegate *)
  Op.delegation ~fee (B b) impl_contract (Some unregistered_delegate_pkh)
  >>=? fun delegate_op ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been debited; no delegate *)
    Incremental.add_operation
      i
      ~expect_apply_failure:(expect_unregistered_key unregistered_delegate_pkh)
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    (* implicit contract delegate has not changed *)
    Context.Contract.delegate (I i) bootstrap >>=? fun delegate_pkh_after ->
    Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate_pkh_after

(** Same as [unregistered_delegate_key_init_origination] and credits
    [amount], no self-delegation. *)
let test_unregistered_delegate_key_init_origination_credit ~fee ~amount () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake b ~operation:create_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* origination with delegate argument *)
  Context.Contract.balance (B b) bootstrap >>=? fun balance ->
  Op.contract_origination
    ~fee
    ~delegate:unregistered_pkh
    (B b)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination not done, fee taken *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_pkh)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    Context.Contract.balance (I i) orig_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)

(** Same as [unregistered_delegate_key_init_delegation] and credits
    the amount [amount] of the implicit contract. *)
let test_unregistered_delegate_key_init_delegation_credit ~fee ~amount () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    impl_contract
    amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  credit +? amount >>?= fun balance ->
  Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  >>=? fun init_credit ->
  Block.bake ~operation:init_credit b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract balance >>=? fun () ->
  (* try to delegate *)
  Op.delegation
    ~force_reveal:true
    ~fee
    (B b)
    impl_contract
    (Some unregistered_delegate_pkh)
  >>=? fun delegate_op ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, no delegate for contract *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_delegate_pkh)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)

(** Same as in [unregistered_delegate_key_switch_delegation] and
    credits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_switch_delegation_credit ~fee ~amount () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    impl_contract
    amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  credit +? amount >>?= fun balance ->
  Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  >>=? fun init_credit ->
  Block.bake ~operation:init_credit b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract balance >>=? fun () ->
  (* set and check the initial delegate *)
  Op.delegation
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    impl_contract
    (Some bootstrap_pkh)
  >>=? fun delegate_op ->
  Block.bake ~operation:delegate_op b >>=? fun b ->
  Context.Contract.delegate (B b) bootstrap >>=? fun delegate_pkh ->
  Assert.equal_pkh ~loc:__LOC__ bootstrap_pkh delegate_pkh >>=? fun () ->
  (* switch delegate through delegation *)
  Op.delegation ~fee (B b) impl_contract (Some unregistered_delegate_pkh)
  >>=? fun delegate_op ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, delegate for contract has not changed *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_delegate_pkh)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
    Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_delegate_pkh
    >>=? fun () -> Assert.equal_pkh ~loc:__LOC__ delegate bootstrap_pkh

(** A credit of some amount followed by a debit of the same amount,
    no self-delegation. *)
let test_unregistered_delegate_key_init_origination_credit_debit ~fee ~amount ()
    =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake b ~operation:create_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* debit + check balance *)
  Op.transaction ~force_reveal:true (B b) impl_contract bootstrap amount
  >>=? fun debit_contract ->
  Block.bake b ~operation:debit_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* origination with delegate argument *)
  Context.Contract.balance (B b) bootstrap >>=? fun balance ->
  Op.contract_origination
    ~fee
    ~delegate:unregistered_pkh
    (B b)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > balance then expect_too_low_balance_error i op
  else
    (* fee taken, origination not processed *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_pkh)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    Context.Contract.balance (I i) orig_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)

(** Same as in [unregistered_delegate_key_init_delegation] but credits
    then debits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_init_delegation_credit_debit ~amount ~fee ()
    =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    impl_contract
    amount
  >>=? fun create_contract ->
  Block.bake b ~operation:create_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* debit + check balance *)
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    impl_contract
    bootstrap
    amount
  >>=? fun debit_contract ->
  Block.bake b ~operation:debit_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* initial credit for the delegated contract *)
  let credit = of_int 10 in
  Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  >>=? fun credit_contract ->
  Block.bake b ~operation:credit_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit >>=? fun () ->
  (* try to delegate *)
  Op.delegation
    ~force_reveal:true
    ~fee
    (B b)
    impl_contract
    (Some unregistered_delegate_pkh)
  >>=? fun delegate_op ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, no delegate for contract *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_delegate_pkh)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | Tezos_rpc.Context.Not_found _ -> true
        | _ -> false)

(** Same as in [unregistered_delegate_key_switch_delegation] but
    credits then debits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_switch_delegation_credit_debit ~fee ~amount
    () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let bootstrap_pkh = Context.Contract.pkh bootstrap in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  let unregistered_delegate_account = Account.new_account () in
  let unregistered_delegate_pkh = Account.(unregistered_delegate_account.pkh) in
  (* credit + check balance *)
  Op.transaction
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    bootstrap
    impl_contract
    amount
  >>=? fun create_contract ->
  Block.bake b ~operation:create_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* debit + check balance *)
  Op.transaction ~force_reveal:true (B b) impl_contract bootstrap amount
  >>=? fun debit_contract ->
  Block.bake b ~operation:debit_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* delegation - initial credit for the delegated contract *)
  let credit = of_int 10 in
  Op.transaction ~fee:Tez.zero (B b) bootstrap impl_contract credit
  >>=? fun credit_contract ->
  Block.bake b ~operation:credit_contract >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract credit >>=? fun () ->
  (* set and check the initial delegate *)
  Op.delegation
    ~force_reveal:true
    ~fee:Tez.zero
    (B b)
    impl_contract
    (Some bootstrap_pkh)
  >>=? fun delegate_op ->
  Block.bake b ~operation:delegate_op >>=? fun b ->
  Context.Contract.delegate (B b) bootstrap >>=? fun delegate_pkh ->
  Assert.equal_pkh ~loc:__LOC__ bootstrap_pkh delegate_pkh >>=? fun () ->
  (* switch delegate through delegation *)
  Op.delegation (B b) ~fee impl_contract (Some unregistered_delegate_pkh)
  >>=? fun delegate_op ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > credit then expect_too_low_balance_error i delegate_op
  else
    (* fee has been taken, delegate for contract has not changed *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key unregistered_delegate_pkh)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
    Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_delegate_pkh

(* Part A. Section 2.
   Self-delegation to an empty contract fails. *)

(** Self-delegation with zero-balance contract should fail. *)
let test_failed_self_delegation_no_transaction () =
  Context.init1 () >>=? fun (b, _contract) ->
  Incremental.begin_construction b >>=? fun i ->
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* check balance *)
  Context.Contract.balance (I i) impl_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance >>=? fun () ->
  (* self delegation fails *)
  Op.delegation (I i) impl_contract (Some unregistered_pkh)
  >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>= fun err ->
  Assert.proto_error_with_info ~loc:__LOC__ err "Empty implicit contract"

(** Implicit contract is credited then debited of same amount (i.e.,
    is emptied). Self-delegation fails. *)
let test_failed_self_delegation_emptied_implicit_contract amount () =
  (* create an implicit contract *)
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (*  credit implicit contract and check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* empty implicit contract and check balance *)
  Op.transaction ~force_reveal:true (B b) impl_contract bootstrap amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* self delegation fails *)
  Op.delegation (B b) impl_contract (Some unregistered_pkh)
  >>=? fun self_delegation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i self_delegation >>= fun err ->
  Assert.proto_error_with_info ~loc:__LOC__ err "Empty implicit contract"

(** Implicit contract is credited with a non-zero quantity [amount]
    tz, then it is delegated. The operation of debit of [amount] tz
    should fail as the contract is already delegated. *)
let test_emptying_delegated_implicit_contract_fails amount () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  Context.Contract.manager (B b) bootstrap >>=? fun bootstrap_manager ->
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit unregistered_pkh in
  (* credit unregistered implicit contract and check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* delegate the contract to the bootstrap *)
  Op.delegation
    ~force_reveal:true
    (B b)
    impl_contract
    (Some bootstrap_manager.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  (* empty implicit contract and expect error since the contract is delegated *)
  Op.transaction (B b) impl_contract bootstrap amount
  >>=? fun create_contract ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation i create_contract >>= fun err ->
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
  (* create an implicit contract *)
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* self delegation + verification *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  Context.Contract.delegate (B b) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh >>=? fun () ->
  (* create an implicit contract with no delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  >>=? fun credit_contract ->
  Block.bake ~operation:credit_contract b >>=? fun b ->
  (* check no delegate for delegator contract *)
  Context.Contract.delegate (B b) delegator >>= fun err ->
  Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)
  >>=? fun () ->
  (* delegation to the newly registered key *)
  Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  (* check delegation *)
  Context.Contract.delegate (B b) delegator >>=? fun delegator_delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(** Create an implicit contract, credits with [amount]
    tz. Self-delegates. Create another implicit contract with
    bootstrap as delegate. Re-delegate it to the first implicit
    contract. *)
let test_valid_delegate_registration_switch_delegation_credit amount () =
  (* create an implicit contract *)
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* self delegation + verification *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  Context.Contract.delegate (B b) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh >>=? fun () ->
  (* create an implicit contract with bootstrap's account as delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  >>=? fun credit_contract ->
  Block.bake ~operation:credit_contract b >>=? fun b ->
  Context.Contract.manager (B b) bootstrap >>=? fun bootstrap_manager ->
  Op.delegation ~force_reveal:true (B b) delegator (Some bootstrap_manager.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  (* test delegate of new contract is bootstrap *)
  Context.Contract.delegate (B b) delegator >>=? fun delegator_delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate bootstrap_manager.pkh
  >>=? fun () ->
  (* delegation with newly registered key *)
  Op.delegation (B b) delegator (Some delegate_account.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  Context.Contract.delegate (B b) delegator >>=? fun delegator_delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(** Create an implicit contract. *)
let test_valid_delegate_registration_init_delegation_credit_debit amount () =
  (* create an implicit contract *)
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ+ check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* self delegation + verification *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  Context.Contract.delegate (B b) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun () ->
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  Op.transaction (B b) impl_contract bootstrap amount >>=? fun empty_contract ->
  Block.bake ~operation:empty_contract b >>=? fun b ->
  (* impl_contract is empty *)
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* verify self-delegation after contract is emptied *)
  Context.Contract.delegate (B b) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun () ->
  (* create an implicit contract with no delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  >>=? fun credit_contract ->
  Block.bake ~operation:credit_contract b >>=? fun b ->
  (* check no delegate for delegator contract *)
  Context.Contract.delegate (B b) delegator >>= fun err ->
  Assert.error ~loc:__LOC__ err (function
      | Tezos_rpc.Context.Not_found _ -> true
      | _ -> false)
  >>=? fun () ->
  (* delegation to the newly registered key *)
  Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  (* check delegation *)
  Context.Contract.delegate (B b) delegator >>=? fun delegator_delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(** A created implicit contract is credited with [amount] tz, then is
    self-delegated. It is emptied (fund back into bootstrap), and
    should remain existing (as registered as delegate). Another created
    implicit contract is delegated to bootstrap, then should be able to
    be re-delegated to the latter contract. *)
let test_valid_delegate_registration_switch_delegation_credit_debit amount () =
  (* create an implicit contract *)
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.Implicit delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract amount >>=? fun () ->
  (* self delegation + verification *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some delegate_pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  Context.Contract.delegate (B b) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun () ->
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  Op.transaction (B b) impl_contract bootstrap amount >>=? fun empty_contract ->
  Block.bake ~operation:empty_contract b >>=? fun b ->
  (* impl_contract is empty *)
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* create an implicit contract with bootstrap's account as delegate *)
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let delegator = Contract.Implicit unregistered_pkh in
  Op.transaction ~fee:Tez.zero (B b) bootstrap delegator Tez.one
  >>=? fun credit_contract ->
  Block.bake ~operation:credit_contract b >>=? fun b ->
  Context.Contract.manager (B b) bootstrap >>=? fun bootstrap_manager ->
  Op.delegation ~force_reveal:true (B b) delegator (Some bootstrap_manager.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  (* test delegate of new contract is bootstrap *)
  Context.Contract.delegate (B b) delegator >>=? fun delegator_delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate bootstrap_manager.pkh
  >>=? fun () ->
  (* delegation with newly registered key *)
  Op.delegation ~force_reveal:true (B b) delegator (Some delegate_account.pkh)
  >>=? fun delegation ->
  Block.bake ~operation:delegation b >>=? fun b ->
  Context.Contract.delegate (B b) delegator >>=? fun delegator_delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegator_delegate delegate_pkh

(* Part C.
   A second self-delegation should raise an [Active_delegate] error. *)

(** Second self-delegation should fail with implicit contract with
    some credit. *)
let test_double_registration () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit pkh in
  (* credit 1μꜩ+ check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract Tez.one_mutez
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez
  >>=? fun () ->
  (* self-delegation *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  (* second self-delegation *)
  Op.delegation (B b) impl_contract (Some pkh) >>=? fun second_registration ->
  Incremental.begin_construction b >>=? fun i ->
  expect_delegate_already_active_error i second_registration

(** Second self-delegation should fail with implicit contract emptied
    after first self-delegation. *)
let test_double_registration_when_empty () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit pkh in
  (* credit 1μꜩ+ check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract Tez.one_mutez
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez
  >>=? fun () ->
  (* self delegation *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  (* empty the delegate account *)
  Op.transaction (B b) impl_contract bootstrap Tez.one_mutez
  >>=? fun empty_contract ->
  Block.bake ~operation:empty_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* second self-delegation *)
  Op.delegation (B b) impl_contract (Some pkh) >>=? fun second_registration ->
  Incremental.begin_construction b >>=? fun i ->
  expect_delegate_already_active_error i second_registration

(** Second self-delegation should fail with implicit contract emptied
    then credited back after first self-delegation. *)
let test_double_registration_when_recredited () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.Implicit pkh in
  (* credit 1μꜩ+ check balance *)
  Op.transaction ~force_reveal:true (B b) bootstrap impl_contract Tez.one_mutez
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez
  >>=? fun () ->
  (* self delegation *)
  Op.delegation ~force_reveal:true (B b) impl_contract (Some pkh)
  >>=? fun self_delegation ->
  Block.bake ~operation:self_delegation b >>=? fun b ->
  (* empty the delegate account *)
  Op.transaction ~force_reveal:true (B b) impl_contract bootstrap Tez.one_mutez
  >>=? fun empty_contract ->
  Block.bake ~operation:empty_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.zero >>=? fun () ->
  (* credit 1μꜩ+ check balance *)
  Op.transaction (B b) bootstrap impl_contract Tez.one_mutez
  >>=? fun create_contract ->
  Block.bake ~operation:create_contract b >>=? fun b ->
  Assert.balance_is ~loc:__LOC__ (B b) impl_contract Tez.one_mutez
  >>=? fun () ->
  (* second self-delegation *)
  Op.delegation (B b) impl_contract (Some pkh) >>=? fun second_registration ->
  Incremental.begin_construction b >>=? fun i ->
  expect_delegate_already_active_error i second_registration

(** Self-delegation on unrevealed contract. *)
let test_unregistered_and_unrevealed_self_delegate_key_init_delegation ~fee () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let {Account.pkh; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; _} = Account.new_account () in
  let contract = Alpha_context.Contract.Implicit pkh in
  Op.transaction ~force_reveal:true (B b) bootstrap contract (of_int 10)
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.delegation ~fee ~force_reveal:true (B b) contract (Some delegate_pkh)
  >>=? fun op ->
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination did not proceed; fee has been debited *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key delegate_pkh)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee

(** Self-delegation on revealed but not registered contract. *)
let test_unregistered_and_revealed_self_delegate_key_init_delegation ~fee () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let {Account.pkh; pk; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; _} = Account.new_account () in
  let contract = Alpha_context.Contract.Implicit pkh in
  Op.transaction (B b) bootstrap contract (of_int 10) >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.revelation (B b) pk >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.delegation ~fee (B b) contract (Some delegate_pkh) >>=? fun op ->
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Incremental.begin_construction b >>=? fun i ->
  if fee > balance then expect_too_low_balance_error i op
  else
    (* origination did not proceed; fee has been debited *)
    Incremental.add_operation
      ~expect_apply_failure:(expect_unregistered_key delegate_pkh)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee

(** Self-delegation emptying a fresh contract. *)
let test_self_delegation_emptying_contract () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let {Account.pkh; pk; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; _} = Account.new_account () in
  let contract = Alpha_context.Contract.Implicit pkh in
  let amount = of_int 10 in
  Op.transaction (B b) bootstrap contract amount >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.revelation ~fee:Tez.zero (B b) pk >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.delegation ~fee:amount (B b) contract (Some delegate_pkh) >>=? fun op ->
  (Context.Contract.is_manager_key_revealed (B b) contract >>=? function
   | false -> failwith "contract should exist"
   | true -> return_unit)
  >>=? fun () ->
  Incremental.begin_construction b >>=? fun i ->
  (* The delegation operation should be applied and the fees
     debited but it is expected to fail in the apply-part. *)
  Incremental.add_operation ~expect_apply_failure:(fun _ -> return_unit) i op
  >>=? fun i ->
  Context.Contract.is_manager_key_revealed (I i) contract >>=? function
  | false -> return_unit
  | true -> failwith "contract should have been removed"

(** Self-delegation on revealed and registered contract. *)
let test_registered_self_delegate_key_init_delegation () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (b, bootstrap) ->
  let {Account.pkh; _} = Account.new_account () in
  let {Account.pkh = delegate_pkh; pk = delegate_pk; _} =
    Account.new_account ()
  in
  let contract = Alpha_context.Contract.Implicit pkh in
  let delegate_contract = Alpha_context.Contract.Implicit delegate_pkh in
  Op.transaction ~force_reveal:true (B b) bootstrap contract (of_int 10)
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.transaction (B b) bootstrap delegate_contract (of_int 1)
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.revelation (B b) delegate_pk >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.delegation (B b) delegate_contract (Some delegate_pkh)
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Op.delegation ~force_reveal:true (B b) contract (Some delegate_pkh)
  >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Context.Contract.delegate (B b) contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh >>=? fun () -> return_unit

let test_bls_account_cannot_self_delegate () =
  let open Lwt_result_syntax in
  let* b, bootstrap = Context.init1 ~consensus_threshold:0 () in
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
    Tztest.tztest "TEST" `Quick test_bls_account_cannot_self_delegate;
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
  ]

(******************************************************************************)
(* Main                                                                       *)
(******************************************************************************)

let tests = tests_bootstrap_contracts @ tests_delegate_registration
