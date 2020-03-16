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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^delegation$"
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
  | err0 :: _ when err = err0 ->
      return_unit
  | _ ->
      failwith "Unexpected successful result"

let expect_alpha_error err = expect_error (Environment.Ecoproto_error err)

let expect_no_change_registered_baker pkh = function
  | Environment.Ecoproto_error (Delegation_storage.No_baker_delegation pkh0)
    :: _
    when pkh0 = pkh ->
      return_unit
  | _ ->
      failwith "Baker can not be deleted and operation should fail."

(** Bootstrap bakers delegate to themselves. *)
let test_bootstrap_baker_is_self_delegate () =
  Context.init 1
  >>=? fun (b, _, bootstrap_bakers) ->
  let baker0 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  Context.Contract.delegate (B b) (Contract.baker_contract baker0)
  >>=? fun delegate0 -> Assert.equal_baker ~loc:__LOC__ delegate0 baker0

(** Bootstrap bakers cannot change their delegate. *)
let test_bootstrap_delegate_cannot_change () =
  Context.init 2
  >>=? fun (b, _, bootstrap_bakers) ->
  let baker0 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_bakers 0
  in
  let baker_contract0 = Contract.baker_contract baker0 in
  let baker1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_bakers 1
  in
  Incremental.begin_construction b ~policy:(Block.Excluding [baker0])
  >>=? fun i ->
  Context.Contract.delegate (I i) baker_contract0
  >>=? fun delegate0 ->
  Lwt.catch
    (fun () ->
      (* change delegation to bootstrap1 *)
      Op.delegation (I i) baker_contract0 (Some baker1)
      >>=? fun _op -> return ())
    (function
      | Invalid_argument _ ->
          return ()
      | _ ->
          failwith "Expecting 'Invalid_argument' error")
  >>=? fun _ ->
  (* baker_contract0 still has same delegate *)
  Context.Contract.delegate (B b) baker_contract0
  >>=? fun delegate0_after ->
  Assert.equal_baker ~loc:__LOC__ delegate0_after delegate0

(** bootstrap bakers cannot delete their delegation *)
let test_bootstrap_delegate_cannot_be_removed () =
  Context.init 1
  >>=? fun (b, _, bootstrap_bakers) ->
  let baker_contract0 =
    Contract.baker_contract
    @@ WithExceptions.Option.get ~loc:__LOC__
    @@ List.nth bootstrap_bakers 0
  in
  Incremental.begin_construction b
  >>=? fun i ->
  Context.Contract.delegate (I i) baker_contract0
  >>=? fun delegate ->
  Lwt.catch
    (fun () ->
      (* remove delegation *)
      Op.delegation (I i) baker_contract0 None >>=? fun _op -> return ())
    (function
      | Invalid_argument _ ->
          return ()
      | _ ->
          failwith "Expecting 'Invalid_argument' error")
  >>=? fun _ ->
  (* delegate has not changed *)
  Context.Contract.delegate (I i) baker_contract0
  >>=? fun delegate_after ->
  Assert.equal_baker ~loc:__LOC__ delegate delegate_after

(** contracts not registered as delegate can change their delegation *)
let test_delegate_can_be_changed_from_unregistered_contract ~fee () =
  Context.init 2
  >>=? fun (b, bootstrap_contracts, bakers) ->
  let bootstrap0 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker0 = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  let baker1 = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bakers 1 in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let unregistered = Contract.implicit_contract unregistered_pkh in
  Incremental.begin_construction b
  >>=? fun i ->
  let credit = Tez.of_int 10 in
  Op.transaction ~fee:Tez.zero (I i) bootstrap0 unregistered credit
  >>=? fun credit_contract ->
  Context.Contract.balance (I i) bootstrap0
  >>=? fun balance ->
  Incremental.add_operation i credit_contract
  >>=? fun i ->
  (* delegate to baker0 *)
  Op.delegation ~fee:Tez.zero (I i) unregistered (Some baker0)
  >>=? fun set_delegate ->
  Incremental.add_operation i set_delegate
  >>=? fun i ->
  Context.Contract.delegate (I i) unregistered
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ delegate baker0
  >>=? fun () ->
  (* change delegation to baker1 *)
  Op.delegation ~fee (I i) unregistered (Some baker1)
  >>=? fun change_delegate ->
  if fee > balance then
    Incremental.add_operation i change_delegate
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    Incremental.add_operation i change_delegate
    >>=? fun i ->
    (* delegate has changed *)
    Context.Contract.delegate (I i) unregistered
    >>=? fun delegate_after ->
    Assert.equal_baker ~loc:__LOC__ delegate_after baker1
    >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) unregistered credit fee

(** contracts not registered as delegate can delete their delegation *)
let test_delegate_can_be_removed_from_unregistered_contract ~fee () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bakers) ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let unregistered = Contract.implicit_contract unregistered_pkh in
  Incremental.begin_construction b
  >>=? fun i ->
  let credit = Tez.of_int 10 in
  Op.transaction ~fee:Tez.zero (I i) bootstrap unregistered credit
  >>=? fun credit_contract ->
  Context.Contract.balance (I i) bootstrap
  >>=? fun balance ->
  Incremental.add_operation i credit_contract
  >>=? fun i ->
  (* delegate to bootstrap *)
  Op.delegation ~fee:Tez.zero (I i) unregistered (Some baker)
  >>=? fun set_delegate ->
  Incremental.add_operation i set_delegate
  >>=? fun i ->
  Context.Contract.delegate (I i) unregistered
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ delegate baker
  >>=? fun () ->
  (* remove delegation *)
  Op.delegation ~fee (I i) unregistered None
  >>=? fun delete_delegate ->
  if fee > balance then
    Incremental.add_operation i delete_delegate
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    Incremental.add_operation i delete_delegate
    >>=? fun i ->
    (* the delegate has been removed *)
    Context.Contract.delegate_opt (I i) unregistered
    >>=? (function
           | None ->
               return_unit
           | Some _ ->
               failwith "Expected delegate to be removed")
    >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) unregistered credit fee

(** bootstrap manager can be set as delegate of an originated contract
    (through origination operation) *)
let test_delegate_to_bootstrap_by_origination ~fee () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  Context.Contract.balance (I i) bootstrap
  >>=? fun balance ->
  (* originate a contract with bootstrap's manager as delegate *)
  Op.origination
    ~fee
    ~credit:Tez.zero
    ~delegate:baker
    (I i)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  Context.get_constants (I i)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  (* 0.257tz *)
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  Tez.( +? ) fee origination_burn
  >>? Tez.( +? ) Op.dummy_script_cost
  >>?= fun total_fee ->
  if fee > balance then
    Incremental.add_operation i op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else if total_fee > balance && balance >= fee then
    (* origination did not proceed; fee has been debited *)
    Incremental.add_operation
      i
      ~expect_failure:(function
        | Environment.Ecoproto_error (Contract.Balance_too_low _) :: _ ->
            return_unit
        | _ ->
            failwith
              "Not enough balance for origination burn: operation should fail.")
      op
    >>=? fun i ->
    (* fee was taken *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    (* originated contract has not been created *)
    Context.Contract.balance (I i) orig_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)
  else
    (* bootstrap is delegate, fee + origination burn have been debited *)
    Incremental.add_operation i op
    >>=? fun i ->
    Context.Contract.delegate (I i) orig_contract
    >>=? fun delegate ->
    Assert.equal_baker ~loc:__LOC__ delegate baker
    >>=? fun () ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance total_fee

let tests_bootstrap_contracts =
  [ Test_services.tztest
      "bootstrap bakers delegate to themselves"
      `Quick
      test_bootstrap_baker_is_self_delegate;
    Test_services.tztest
      "bootstrap bakers cannot change their delegate"
      `Quick
      test_bootstrap_delegate_cannot_change;
    Test_services.tztest
      "bootstrap bakers cannot remove their delegation"
      `Quick
      test_bootstrap_delegate_cannot_be_removed;
    Test_services.tztest
      "contracts not registered as delegate can remove their delegation \
       (small fee)"
      `Quick
      (test_delegate_can_be_changed_from_unregistered_contract
         ~fee:Tez.one_mutez);
    Test_services.tztest
      "contracts not registered as delegate can remove their delegation (max \
       fee)"
      `Quick
      (test_delegate_can_be_changed_from_unregistered_contract ~fee:Tez.max_tez);
    Test_services.tztest
      "contracts not registered as delegate can remove their delegation \
       (small fee)"
      `Quick
      (test_delegate_can_be_removed_from_unregistered_contract
         ~fee:Tez.one_mutez);
    Test_services.tztest
      "contracts not registered as delegate can remove their delegation (max \
       fee)"
      `Quick
      (test_delegate_can_be_removed_from_unregistered_contract ~fee:Tez.max_tez);
    Test_services.tztest
      "bootstrap manager can be delegate (init origination, small fee)"
      `Quick
      (test_delegate_to_bootstrap_by_origination ~fee:Tez.one_mutez);
    (* balance enough for fee but not for fee + origination burn + dummy script storage cost *)
    Test_services.tztest
      "bootstrap manager can be delegate (init origination, edge case)"
      `Quick
      (test_delegate_to_bootstrap_by_origination
         ~fee:(Tez.of_mutez_exn 3_999_999_705_000L));
    (* fee bigger than bootstrap's initial balance*)
    Test_services.tztest
      "bootstrap manager can be delegate (init origination, large fee)"
      `Quick
      (test_delegate_to_bootstrap_by_origination ~fee:(Tez.of_int 10_000_000))
  ]

(**************************************************************************)
(* Delegate registration
   ---------------------
   A baker can be a delegate. Bakers must be registered.

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
*)

let expect_unregistered_baker baker = function
  | Environment.Ecoproto_error (Baker_storage.Unregistered_baker baker0) :: _
    when baker = baker0 ->
      return_unit
  | _ ->
      failwith "Delegate key is not registered: operation should fail."

(* Part A. Section 1.
   No self-delegation. *)

(** No token transfer, no self-delegation.  Originated account. If
    fees are higher than balance, [Balance_too_low] is
    raised. Otherwise, it checks the correct exception is raised
    (unregistered key), and the fees are still debited. Using RPCs, we
    verify the contract has not been originated. *)
let test_unregistered_delegate_key_init_origination ~fee () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* origination with delegate argument *)
  Op.origination
    ~fee
    ~delegate:unregistered_baker
    (I i)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  Context.Contract.balance (I i) bootstrap
  >>=? fun balance ->
  if fee > balance then
    Incremental.add_operation i op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* origination did not proceed; fee has been debited *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    (* originated contract has not been created *)
    Context.Contract.balance (I i) orig_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)

(** Delegation when delegate key is not assigned. Delegate account is
    initialized. If fees are higher than initial credit (10 tez),
    [Balance_too_low] is raised. Otherwise, fees are still debited. The
    implicit contract has no delegate. *)
let test_unregistered_delegate_key_init_delegation ~fee () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* initial credit for the delegated contract *)
  let credit = Tez.of_int 10 in
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract credit
  >>=? fun credit_contract ->
  Incremental.add_operation i credit_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract credit
  >>=? fun _ ->
  (* try to delegate *)
  Op.delegation ~fee (I i) impl_contract (Some unregistered_baker)
  >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee has been debited; no delegate *)
    Incremental.add_operation
      i
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    (* implicit contract has no delegate *)
    Context.Contract.delegate (I i) impl_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)

(** Re-delegation when a delegate key was already assigned. If fees
    are higher than initial credit (10 tez), [Balance_too_low] is
    raised. Otherwise, fees are not debited and the implicit contract
    delegate remains unchanged. *)
let test_unregistered_delegate_key_switch_delegation ~fee () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let bootstrap_baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* initial credit for the delegated contract *)
  let credit = Tez.of_int 10 in
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract credit
  >>=? fun init_credit ->
  Incremental.add_operation i init_credit
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract credit
  >>=? fun _ ->
  (* set and check the initial delegate *)
  Op.delegation ~fee:Tez.zero (I i) impl_contract (Some bootstrap_baker)
  >>=? fun delegate_op ->
  Incremental.add_operation i delegate_op
  >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ bootstrap_baker delegate
  >>=? fun () ->
  (* try to delegate *)
  Op.delegation ~fee (I i) impl_contract (Some unregistered_baker)
  >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee has been debited; no delegate *)
    Incremental.add_operation
      i
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    (* implicit contract delegate has not changed *)
    Context.Contract.delegate (I i) impl_contract
    >>=? fun delegate_after ->
    Assert.equal_baker ~loc:__LOC__ delegate delegate_after

(** Same as [unregistered_delegate_key_init_origination] and credits
    [amount], no self-delegation. *)
let test_unregistered_delegate_key_init_origination_credit ~fee ~amount () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* origination with delegate argument *)
  Context.Contract.balance (I i) bootstrap
  >>=? fun balance ->
  Op.origination
    ~fee
    ~delegate:unregistered_baker
    (I i)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  if fee > balance then
    Incremental.add_operation i op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* origination not done, fee taken *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    Context.Contract.balance (I i) orig_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)

(** Same as [unregistered_delegate_key_init_delegation] and credits
    the amount [amount] of the implicit contract. *)
let test_unregistered_delegate_key_init_delegation_credit ~fee ~amount () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* initial credit for the delegated contract *)
  let credit = Tez.of_int 10 in
  Tez.(credit +? amount)
  >>?= fun balance ->
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract credit
  >>=? fun init_credit ->
  Incremental.add_operation i init_credit
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract balance
  >>=? fun _ ->
  (* try to delegate *)
  Op.delegation ~fee (I i) impl_contract (Some unregistered_baker)
  >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee has been taken, no delegate for contract *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)

(** Same as in [test_unregistered_delegate_key_switch_delegation] and
    credits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_switch_delegation_credit ~fee ~amount () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let bootstrap_baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* initial credit for the delegated contract *)
  let credit = Tez.of_int 10 in
  Tez.(credit +? amount)
  >>?= fun balance ->
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract credit
  >>=? fun init_credit ->
  Incremental.add_operation i init_credit
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract balance
  >>=? fun _ ->
  (* set and check the initial delegate *)
  Op.delegation ~fee:Tez.zero (I i) impl_contract (Some bootstrap_baker)
  >>=? fun delegate_op ->
  Incremental.add_operation i delegate_op
  >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ bootstrap_baker delegate
  >>=? fun () ->
  (* switch delegate through delegation *)
  Op.delegation ~fee (I i) impl_contract (Some unregistered_baker)
  >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee has been taken, delegate for contract has not changed *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract
    >>=? fun delegate ->
    Assert.not_equal_baker ~loc:__LOC__ delegate unregistered_baker
    >>=? fun () -> Assert.equal_baker ~loc:__LOC__ delegate bootstrap_baker

(** A credit of some amount followed by a debit of the same amount,
    no self-delegation. *)
let test_unregistered_delegate_key_init_origination_credit_debit ~fee ~amount
    () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* credit + check balance *)
  Op.transaction (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* debit + check balance *)
  Op.transaction (I i) impl_contract bootstrap amount
  >>=? fun debit_contract ->
  Incremental.add_operation i debit_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero
  >>=? fun _ ->
  (* origination with delegate argument *)
  Context.Contract.balance (I i) bootstrap
  >>=? fun balance ->
  Op.origination
    ~fee
    ~delegate:unregistered_baker
    (I i)
    bootstrap
    ~script:Op.dummy_script
  >>=? fun (op, orig_contract) ->
  if fee > balance then
    Incremental.add_operation i op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee taken, origination not processed *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee
    >>=? fun () ->
    Context.Contract.balance (I i) orig_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)

(** Same as in [unregistered_delegate_key_init_delegation] but credits
    then debits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_init_delegation_credit_debit ~amount ~fee ()
    =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* debit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) impl_contract bootstrap amount
  >>=? fun debit_contract ->
  Incremental.add_operation i debit_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero
  >>=? fun _ ->
  (* initial credit for the delegated contract *)
  let credit = Tez.of_int 10 in
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract credit
  >>=? fun credit_contract ->
  Incremental.add_operation i credit_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract credit
  >>=? fun _ ->
  (* try to delegate *)
  Op.delegation ~fee (I i) impl_contract (Some unregistered_baker)
  >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee has been taken, no delegate for contract *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract
    >>= fun err ->
    Assert.error ~loc:__LOC__ err (function
        | RPC_context.Not_found _ ->
            true
        | _ ->
            false)

(** Same as in [test_unregistered_delegate_key_switch_delegation] but
    credits then debits the amount [amount] to the implicit contract. *)
let test_unregistered_delegate_key_switch_delegation_credit_debit ~fee ~amount
    () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let bootstrap_baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* debit + check balance *)
  Op.transaction (I i) impl_contract bootstrap amount
  >>=? fun debit_contract ->
  Incremental.add_operation i debit_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero
  >>=? fun _ ->
  (* delegation - initial credit for the delegated contract *)
  let credit = Tez.of_int 10 in
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract credit
  >>=? fun credit_contract ->
  Incremental.add_operation i credit_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract credit
  >>=? fun _ ->
  (* set and check the initial delegate *)
  Op.delegation ~fee:Tez.zero (I i) impl_contract (Some bootstrap_baker)
  >>=? fun delegate_op ->
  Incremental.add_operation i delegate_op
  >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ bootstrap_baker delegate
  >>=? fun () ->
  (* switch delegate through delegation *)
  Op.delegation (I i) ~fee impl_contract (Some unregistered_baker)
  >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* fee has been taken, delegate for contract has not changed *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      delegate_op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract credit fee
    >>=? fun () ->
    Context.Contract.delegate (I i) impl_contract
    >>=? fun delegate ->
    Assert.not_equal_baker ~loc:__LOC__ delegate unregistered_baker

(** Implicit contract is credited then debited of same amount (i.e.,
    is emptied). A delegation fails. *)
let test_failed_delegation_emptied_implicit_contract amount () =
  (* create an implicit contract *)
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  (*  credit implicit contract and check balance *)
  Op.transaction (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* empty implicit contract and check balance *)
  Op.transaction (I i) impl_contract bootstrap amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero
  >>=? fun _ ->
  (* delegation fails *)
  Op.delegation (I i) impl_contract (Some unregistered_baker)
  >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation
  >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Contract_storage.Empty_implicit_contract pkh ->
          if pkh = unregistered_pkh then true else false
      | _ ->
          false)

(** Implicit contract is credited with a non-zero quantity [amount]
    tz, then it is delegated. The operation of debit of [amount] tz
    should fail as the contract is already delegated. *)
let test_emptying_delegated_implicit_contract_fails amount () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let baker = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit unregistered implicit contract and check balance *)
  Op.transaction (I i) bootstrap impl_contract amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount
  >>=? fun _ ->
  (* delegate the contract to the bootstrap baker *)
  Op.delegation (I i) impl_contract (Some baker)
  >>=? fun delegation ->
  Incremental.add_operation i delegation
  >>=? fun i ->
  (* empty implicit contract and expect error since the contract is delegated *)
  Op.transaction (I i) impl_contract bootstrap amount
  >>=? fun create_contract ->
  Incremental.add_operation i create_contract
  >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Contract_storage.Empty_implicit_delegated_contract _ ->
          true
      | _ ->
          false)

(* Test delegation to an unregistered baker fails *)
let test_cannot_delegate_to_unregistered_baker ~fee () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let unregistered_account = Account.new_account () in
  let unregistered_baker =
    Account.(new_baker ~origination_nonce:b.origination_nonce ()).baker
  in
  let contract = Contract.implicit_contract unregistered_account.pkh in
  Op.transaction (I i) bootstrap contract (Tez.of_int 10)
  >>=? fun op ->
  Incremental.add_operation i op
  >>=? fun i ->
  Op.delegation ~fee (I i) contract (Some unregistered_baker)
  >>=? fun op ->
  Context.Contract.balance (I i) contract
  >>=? fun balance ->
  if fee > balance then
    Incremental.add_operation i op
    >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ ->
            true
        | _ ->
            false)
  else
    (* operation did not proceed; fee has been debited *)
    Incremental.add_operation
      ~expect_failure:(expect_unregistered_baker unregistered_baker)
      i
      op
    >>=? fun i ->
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee

(* delegation on baker that declines delegations *)
let test_cannot_delegate_to_declining_baker () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  (* set the baker to decline delegations *)
  Op.baker_action (I i) ~action:(Toggle_delegations false) bootstrap baker
  >>=? fun deactivation ->
  Incremental.add_operation i deactivation
  >>=? fun i ->
  Op.delegation (I i) bootstrap (Some baker)
  >>=? fun op ->
  Incremental.add_operation i op
  >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Delegation_storage.Baker_declines_delegations baker0
        when baker = baker0 ->
          true
      | _ ->
          false)
  >>=? fun () ->
  (* set the baker to accept delegations again *)
  Op.baker_action (I i) ~action:(Toggle_delegations true) bootstrap baker
  >>=? fun deactivation ->
  Incremental.add_operation i deactivation
  >>=? fun i ->
  Op.delegation (I i) bootstrap (Some baker)
  >>=? fun op ->
  Incremental.add_operation i op
  >>=? fun i ->
  Context.Contract.delegate (I i) bootstrap
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ delegate baker >>=? fun () -> return_unit

(* Test that delegation to a registered baker account works *)
let test_delegate_to_registered_baker () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  Op.delegation (I i) bootstrap (Some baker)
  >>=? fun op ->
  Incremental.add_operation i op
  >>=? fun i ->
  Context.Contract.delegate (I i) bootstrap
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ delegate baker >>=? fun () -> return_unit

let tests_delegate_registration =
  [ (*** unregistered delegate key: no self-delegation ***)
    (* no token transfer, no self-delegation *)
    Test_services.tztest
      "unregistered delegate key (origination, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key (origination, edge case fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination
         ~fee:(Tez.of_int 3_999_488));
    Test_services.tztest
      "unregistered delegate key (origination, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination
         ~fee:(Tez.of_int 10_000_000));
    Test_services.tztest
      "unregistered delegate key (init with delegation, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key (init with delegation, max fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation ~fee:Tez.max_tez);
    Test_services.tztest
      "unregistered delegate key (switch with delegation, small fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key (switch with delegation, max fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation ~fee:Tez.max_tez);
    (* credit/debit 1μꜩ, no self-delegation *)
    Test_services.tztest
      "unregistered delegate key - credit/debit 1μꜩ (origination, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit_debit
         ~fee:Tez.one_mutez
         ~amount:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit/debit 1μꜩ (origination, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit_debit
         ~fee:Tez.max_tez
         ~amount:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit/debit 1μꜩ (init with delegation, \
       small fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit/debit 1μꜩ (init with delegation, \
       large fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:Tez.max_tez);
    Test_services.tztest
      "unregistered delegate key - credit/debit 1μꜩ (switch with \
       delegation, small fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit/debit 1μꜩ (switch with \
       delegation, large fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit_debit
         ~amount:Tez.one_mutez
         ~fee:Tez.max_tez);
    (* credit 1μꜩ, no self-delegation *)
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (origination, small fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit
         ~fee:Tez.one_mutez
         ~amount:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (origination, edge case fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit
         ~fee:(Tez.of_int 3_999_488)
         ~amount:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (origination, large fee)"
      `Quick
      (test_unregistered_delegate_key_init_origination_credit
         ~fee:(Tez.of_int 10_000_000)
         ~amount:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (init with delegation, small \
       fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (init with delegation, large \
       fee)"
      `Quick
      (test_unregistered_delegate_key_init_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:Tez.max_tez);
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (switch with delegation, \
       small fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:Tez.one_mutez);
    Test_services.tztest
      "unregistered delegate key - credit 1μꜩ (switch with delegation, \
       large fee)"
      `Quick
      (test_unregistered_delegate_key_switch_delegation_credit
         ~amount:Tez.one_mutez
         ~fee:Tez.max_tez);
    (* delegation on unregistered baker *)
    Test_services.tztest
      "cannot delegate to unregistered baker (small fee)"
      `Quick
      (test_cannot_delegate_to_unregistered_baker ~fee:Tez.one_mutez);
    Test_services.tztest
      "cannot delegate to unregistered baker (large fee)"
      `Quick
      (test_cannot_delegate_to_unregistered_baker ~fee:Tez.max_tez);
    (* delegation on registered baker that declines delegations *)
    Test_services.tztest
      "cannot delegate to baker that declines delegations"
      `Quick
      test_cannot_delegate_to_declining_baker;
    (* delegation on registered baker *)
    Test_services.tztest
      "delegate to registered baker"
      `Quick
      test_delegate_to_registered_baker;
    Test_services.tztest
      "failed delegation: credit and debit 1μꜩ"
      `Quick
      (test_failed_delegation_emptied_implicit_contract Tez.one_mutez);
    (* credit 1μtz, delegate, debit 1μtz *)
    Test_services.tztest
      "empty delegated contract is not deleted: credit 1μꜩ, delegate & \
       debit 1μꜩ"
      `Quick
      (test_emptying_delegated_implicit_contract_fails Tez.one_mutez) ]

(******************************************************************************)
(* Main                                                                       *)
(******************************************************************************)

let tests = tests_bootstrap_contracts @ tests_delegate_registration
