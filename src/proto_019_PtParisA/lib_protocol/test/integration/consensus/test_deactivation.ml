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
    Component:  Protocol
    Invocation: dune exec src/proto_019_PtParisA/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_deactivation.ml
    Subject:    After a given number of cycles during which a delegate has not
                made use of its baking and attesting rights, its account will
                be deactivated for validator selection. To bake/attest
                again, it will have to re-activate its account.
*)

open Protocol
open Alpha_context
open Test_tez

(** Check that [Delegate.staking_balance] is the same as [Delegate.full_balance]
   (this is not true in general, but in these tests it is because they only deal
   with self-delegation. Also, check that [Delegate.staking_balance] coincides
   with [Stake_storage.Internal_for_tests.get] when the account is active and it
   has the minimal required stake. *)
let check_stake ~loc (b : Block.t) (account : Account.t) =
  let open Lwt_result_wrap_syntax in
  let* staking_balance = Context.Delegate.staking_balance (B b) account.pkh in
  let* full_balance = Context.Delegate.full_balance (B b) account.pkh in
  let* () = Assert.equal_tez ~loc full_balance staking_balance in
  let*@ ctxt =
    Raw_context.prepare
      b.context
      ~level:b.header.shell.level
      ~predecessor_timestamp:b.header.shell.timestamp
      ~timestamp:b.header.shell.timestamp
      ~adaptive_issuance_enable:false
  in
  let*@ stake = Stake_storage.Internal_for_tests.get ctxt account.pkh in
  Assert.equal_int64
    ~loc
    (Tez_repr.to_mutez stake)
    (Tez.to_mutez staking_balance)

(** Check that [Stake_storage.Internal_for_tests.get] returns 0 (following a
   deactivation). Note that in case of deactivation [Delegate.staking_balance]
   does not necessarily coincide with [Stake_storage.Internal_for_tests.get] in
   that [Delegate.staking_balance] may be positive
   (while [Stake_storage.Internal_for_tests.get] returns 0 because the account
   is no longer in [Active_delegate_with_minimal_stake] because of deactivation,
   see [Stake_storage].) *)
let check_no_stake ~loc (b : Block.t) (account : Account.t) =
  let open Lwt_result_wrap_syntax in
  let*@ ctxt =
    Raw_context.prepare
      b.context
      ~level:b.header.shell.level
      ~predecessor_timestamp:b.header.shell.timestamp
      ~timestamp:b.header.shell.timestamp
      ~adaptive_issuance_enable:false
  in
  let*@ stake = Stake_storage.Internal_for_tests.get ctxt account.pkh in
  Assert.equal_int64 ~loc (Tez_repr.to_mutez stake) 0L

(** Create a block with two initialized contracts/accounts. Assert
    that the first account has a staking balance that is equal to its
    own balance, and that its staking rights are consistent
    (check_stake). *)
let test_simple_staking_rights () =
  let open Lwt_result_syntax in
  let* b, (a1, _a2) = Context.init2 () in
  let* balance = Context.Contract.balance (B b) a1 in
  let delegate1 = Context.Contract.pkh a1 in
  let* frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b) delegate1
  in
  let expected_initial_balance =
    Account.default_initial_balance -! frozen_deposits
  in
  let* () = Assert.equal_tez ~loc:__LOC__ balance expected_initial_balance in
  let* m1 = Context.Contract.manager (B b) a1 in
  let* info = Context.Delegate.info (B b) m1.pkh in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      Account.default_initial_balance
      info.staking_balance
  in
  check_stake ~loc:__LOC__ b m1

(** Create a block with two initialized contracts/accounts. Bake
    five blocks. Assert that the staking balance of the first account
    equals to its balance. Then both accounts have consistent staking
    rights. *)
let test_simple_staking_rights_after_baking () =
  let open Lwt_result_syntax in
  let* b, (a1, a2) = Context.init2 ~consensus_threshold:0 () in
  let* m1 = Context.Contract.manager (B b) a1 in
  let* m2 = Context.Contract.manager (B b) a2 in
  let* b = Block.bake_n ~policy:(By_account m2.pkh) 5 b in
  let* balance = Context.Contract.balance (B b) a1 in
  let delegate1 = Context.Contract.pkh a1 in
  let* frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b) delegate1
  in
  let*? full_balance = balance +? frozen_deposits in
  let* info = Context.Delegate.info (B b) m1.pkh in
  let* () = Assert.equal_tez ~loc:__LOC__ full_balance info.staking_balance in
  let* () = check_stake ~loc:__LOC__ b m1 in
  check_stake ~loc:__LOC__ b m2

let check_active_staking_balance ~loc ~deactivated b (m : Account.t) =
  let open Lwt_result_syntax in
  let* info = Context.Delegate.info (B b) m.pkh in
  let* () = Assert.equal_bool ~loc info.deactivated deactivated in
  if deactivated then check_no_stake ~loc b m else check_stake ~loc b m

let run_until_deactivation () =
  let open Lwt_result_syntax in
  let* b, (a1, a2) = Context.init2 ~consensus_threshold:0 () in
  let* balance_start = Context.Contract.balance (B b) a1 in
  let* m1 = Context.Contract.manager (B b) a1 in
  let* m2 = Context.Contract.manager (B b) a2 in
  let* () = check_active_staking_balance ~loc:__LOC__ ~deactivated:false b m1 in
  let* info = Context.Delegate.info (B b) m1.pkh in
  let* b =
    Block.bake_until_cycle ~policy:(By_account m2.pkh) info.grace_period b
  in
  let* () = check_active_staking_balance ~loc:__LOC__ ~deactivated:false b m1 in
  let* b = Block.bake_until_cycle_end ~policy:(By_account m2.pkh) b in
  let+ () = check_active_staking_balance ~loc:__LOC__ ~deactivated:true b m1 in
  (b, ((a1, m1), balance_start), (a2, m2))

(** From an initialized block with two contracts/accounts, the first
    one is active then deactivated. After baking, check that the
    account is active again. Baking rights are ensured. *)
let test_deactivation_then_bake () =
  let open Lwt_result_syntax in
  let* ( b,
         ((_deactivated_contract, deactivated_account), _start_balance),
         (_a2, _m2) ) =
    run_until_deactivation ()
  in
  let* b = Block.bake ~policy:(By_account deactivated_account.pkh) b in
  check_active_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_account

(** check that an account which is deactivated for [consensus_rights_delay] cannot be
   part of a committee *)
let test_a_really_deactivated_account_is_not_in_the_committee () =
  let open Lwt_result_syntax in
  let* ( b,
         ((_deactivated_contract, deactivated_account), _start_balance),
         (_a2, m2) ) =
    run_until_deactivation ()
  in
  (* at this point, the deactivated account can either bake (because it still
     has rights) and become active again, or, in case it is inactive for another
     [consensus_rights_delay], it has no more rights, thus cannot be part of the
     committee. *)
  let constants = Default_parameters.constants_test in
  let* b =
    Block.bake_until_n_cycle_end
      (constants.consensus_rights_delay + 1)
      ~policy:(By_account m2.pkh)
      b
  in
  let* bakers =
    Plugin.RPC.Baking_rights.get
      Block.rpc_ctxt
      ~delegates:[deactivated_account.pkh]
      b
  in
  match List.hd bakers with Some _ -> assert false | None -> return_unit

(** A deactivated account, after baking with self-delegation, is
    active again. Preservation of its balance is tested. Baking rights
    are ensured. *)
let test_deactivation_then_self_delegation () =
  let open Lwt_result_syntax in
  let* ( b,
         ((deactivated_contract, deactivated_account), _start_balance),
         (_a2, m2) ) =
    run_until_deactivation ()
  in
  let* self_delegation =
    Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh)
  in
  let* b =
    Block.bake ~policy:(By_account m2.pkh) b ~operation:self_delegation
  in
  let* () =
    check_active_staking_balance
      ~loc:__LOC__
      ~deactivated:false
      b
      deactivated_account
  in
  check_stake ~loc:__LOC__ b deactivated_account

(** A deactivated account, which is emptied (into a newly created sink
    account), then self-delegated, becomes activated. Its balance is
    zero. Baking rights are ensured. *)
let test_deactivation_then_empty_then_self_delegation () =
  let open Lwt_result_syntax in
  let* ( b,
         ((deactivated_contract, deactivated_account), _start_balance),
         (_a2, m2) ) =
    run_until_deactivation ()
  in
  (* empty the contract *)
  let* balance = Context.Contract.balance (B b) deactivated_contract in
  let sink_account = Account.new_account () in
  let sink_contract = Contract.Implicit sink_account.pkh in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (B b)
  in
  let*? origination_burn = cost_per_byte *? Int64.of_int origination_size in
  let amount =
    match balance -? origination_burn with Ok r -> r | Error _ -> assert false
  in
  let* empty_contract =
    Op.transaction (B b) deactivated_contract sink_contract amount
  in
  let* b1 =
    Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b
  in
  (* the account is deactivated, the stake is 0. *)
  let* () = check_no_stake ~loc:__LOC__ b deactivated_account in
  (* self delegation *)
  let* self_delegation =
    Op.delegation (B b1) deactivated_contract (Some deactivated_account.pkh)
  in
  let* b2 =
    Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b1
  in
  let* () =
    check_active_staking_balance
      ~loc:__LOC__
      ~deactivated:false
      b2
      deactivated_account
  in
  (* the account is activated, the stake is still 0. *)
  let* balance = Context.Contract.balance (B b2) deactivated_contract in
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance

(** A deactivated account, which is emptied, then self-delegated, then
    re-credited of the sunk amount, becomes active again. Staking
    rights remain consistent. *)
let test_deactivation_then_empty_then_self_delegation_then_recredit () =
  let open Lwt_result_syntax in
  let* ( b,
         ((deactivated_contract, deactivated_account), _start_balance),
         (_a2, m2) ) =
    run_until_deactivation ()
  in
  (* empty the contract *)
  let* balance = Context.Contract.balance (B b) deactivated_contract in
  let sink_account = Account.new_account () in
  let sink_contract = Contract.Implicit sink_account.pkh in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (B b)
  in
  let*? origination_burn = cost_per_byte *? Int64.of_int origination_size in
  let amount =
    match balance -? origination_burn with Ok r -> r | Error _ -> assert false
  in
  let* empty_contract =
    Op.transaction
      ~force_reveal:true
      (B b)
      deactivated_contract
      sink_contract
      amount
  in
  let* b0 =
    Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b
  in
  (* the account is deactivated, the stake is 0. *)
  let* () = check_no_stake ~loc:__LOC__ b deactivated_account in
  (**** self delegation *)
  let* self_delegation =
    Op.delegation (B b0) deactivated_contract (Some deactivated_account.pkh)
  in
  let* b1 =
    Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b0
  in
  (* the account is still deactivated *)
  let* () = check_no_stake ~loc:__LOC__ b deactivated_account in
  (**** recredit *)
  let* recredit_contract =
    Op.transaction
      ~force_reveal:true
      (B b1)
      sink_contract
      deactivated_contract
      amount
  in
  let* b2 =
    Block.bake ~policy:(By_account m2.pkh) ~operation:recredit_contract b1
  in
  let* () =
    check_active_staking_balance
      ~loc:__LOC__
      ~deactivated:false
      b2
      deactivated_account
  in
  let* balance2 = Context.Contract.balance (B b2) deactivated_contract in
  let* () = Assert.equal_tez ~loc:__LOC__ amount balance2 in
  check_stake ~loc:__LOC__ b2 deactivated_account

(** Initialize a block with two contracts/accounts. A third new account is also
   created. The first account is self-delegated. First account sends to third
   one minimal_stake tez (so that, once it is active, it can appear in
   [Active_delegate_with_minimal_stake]. The third account has no delegate and is
   consistent for baking rights. Then, it is self-delegated and is supposed to
   be activated. Again, consistency for baking rights are preserved for the
   first and third accounts. *)
let test_delegation () =
  let open Lwt_result_syntax in
  let* b, (a1, a2) = Context.init2 ~consensus_threshold:0 () in
  let m3 = Account.new_account () in
  Account.add_account m3 ;
  let* m1 = Context.Contract.manager (B b) a1 in
  let* m2 = Context.Contract.manager (B b) a2 in
  let a3 = Contract.Implicit m3.pkh in
  let* delegate = Context.Contract.delegate_opt (B b) a1 in
  (match delegate with
  | None -> assert false
  | Some pkh -> assert (Signature.Public_key_hash.equal pkh m1.pkh)) ;
  let constants = Default_parameters.constants_test in
  let minimal_stake = constants.minimal_stake in
  let* transact = Op.transaction ~force_reveal:true (B b) a1 a3 minimal_stake in
  let* b = Block.bake ~policy:(By_account m2.pkh) b ~operation:transact in
  let* delegate = Context.Contract.delegate_opt (B b) a3 in
  (match delegate with None -> () | Some _ -> assert false) ;
  let* () = check_no_stake ~loc:__LOC__ b m3 in
  let* delegation = Op.delegation ~force_reveal:true (B b) a3 (Some m3.pkh) in
  let* b = Block.bake ~policy:(By_account m2.pkh) b ~operation:delegation in
  let* delegate = Context.Contract.delegate_opt (B b) a3 in
  (match delegate with
  | None -> assert false
  | Some pkh -> assert (Signature.Public_key_hash.equal pkh m3.pkh)) ;
  let* () = check_active_staking_balance ~loc:__LOC__ ~deactivated:false b m3 in
  let* () = check_stake ~loc:__LOC__ b m3 in
  check_stake ~loc:__LOC__ b m1

let tests =
  [
    Tztest.tztest "simple staking rights" `Quick test_simple_staking_rights;
    Tztest.tztest
      "simple staking rights after baking"
      `Quick
      test_simple_staking_rights_after_baking;
    Tztest.tztest "deactivation then bake" `Quick test_deactivation_then_bake;
    Tztest.tztest
      "deactivation then self delegation"
      `Quick
      test_deactivation_then_self_delegation;
    Tztest.tztest
      "deactivation then empty then self delegation"
      `Quick
      test_deactivation_then_empty_then_self_delegation;
    Tztest.tztest
      "deactivation then empty then self delegation then recredit"
      `Quick
      test_deactivation_then_empty_then_self_delegation_then_recredit;
    Tztest.tztest "delegate" `Quick test_delegation;
    Tztest.tztest
      "a really deactivated account is not part of the committee"
      `Quick
      test_a_really_deactivated_account_is_not_in_the_committee;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("deactivation", tests)]
  |> Lwt_main.run
