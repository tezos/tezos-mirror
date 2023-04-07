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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_deactivation.ml
    Subject:    After a given number of cycles during which a delegate has not
                made use of its baking and endorsing rights, its account will
                be deactivated for validator selection. To bake/endorse
                again, it will have to re-activate its account.
*)

open Protocol
open Alpha_context
open Test_tez

let wrap e = Lwt.return (Environment.wrap_tzresult e)

(** Check that [Delegate.staking_balance] is the same as [Delegate.full_balance]
   (this is not true in general, but in these tests it is because they only deal
   with self-delegation. Also, check that [Delegate.staking_balance] coincides
   with [Stake_storage.get] when the account is active and it has the minimal
   required stake. *)
let check_stake ~loc (b : Block.t) (account : Account.t) =
  Context.Delegate.staking_balance (B b) account.pkh >>=? fun staking_balance ->
  Context.Delegate.full_balance (B b) account.pkh >>=? fun full_balance ->
  Assert.equal_tez ~loc full_balance staking_balance >>=? fun () ->
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
  >>= wrap
  >>=? fun ctxt ->
  Stake_storage.get ctxt account.pkh >>= wrap >>=? fun stake ->
  Assert.equal_int64
    ~loc
    (Tez_repr.to_mutez stake)
    (Tez.to_mutez staking_balance)

(** Check that [Stake_storage.get] returns 0 (following a deactivation). Note
   that in case of deactivation [Delegate.staking_balance] does not necessarily
   coincide with [Stake_storage.get] in that [Delegate.staking_balance] may be
   positive (while [Stake_storage.get] returns 0 because the account is no
   longer in [Active_delegate_with_minimal_stake] because of deactivation, see
   [Stake_storage].) *)
let check_no_stake ~loc (b : Block.t) (account : Account.t) =
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
  >>= wrap
  >>=? fun ctxt ->
  Stake_storage.get ctxt account.pkh >>= wrap >>=? fun stake ->
  Assert.equal_int64 ~loc (Tez_repr.to_mutez stake) 0L

(** Create a block with two initialized contracts/accounts. Assert
    that the first account has a staking balance that is equal to its
    own balance, and that its staking rights are consistent
    (check_stake). *)
let test_simple_staking_rights () =
  Context.init2 () >>=? fun (b, (a1, _a2)) ->
  Context.Contract.balance (B b) a1 >>=? fun balance ->
  let delegate1 = Context.Contract.pkh a1 in
  Context.Delegate.current_frozen_deposits (B b) delegate1
  >>=? fun frozen_deposits ->
  let expected_initial_balance =
    Account.default_initial_balance -! frozen_deposits
  in
  Assert.equal_tez ~loc:__LOC__ balance expected_initial_balance >>=? fun () ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez
    ~loc:__LOC__
    Account.default_initial_balance
    info.staking_balance
  >>=? fun () -> check_stake ~loc:__LOC__ b m1

(** Create a block with two initialized contracts/accounts. Bake
    five blocks. Assert that the staking balance of the first account
    equals to its balance. Then both accounts have consistent staking
    rights. *)
let test_simple_staking_rights_after_baking () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, (a1, a2)) ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  Block.bake_n ~policy:(By_account m2.pkh) 5 b >>=? fun b ->
  Context.Contract.balance (B b) a1 >>=? fun balance ->
  let delegate1 = Context.Contract.pkh a1 in
  Context.Delegate.current_frozen_deposits (B b) delegate1
  >>=? fun frozen_deposits ->
  balance +? frozen_deposits >>?= fun full_balance ->
  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ full_balance info.staking_balance >>=? fun () ->
  check_stake ~loc:__LOC__ b m1 >>=? fun () -> check_stake ~loc:__LOC__ b m2

let check_active_staking_balance ~loc ~deactivated b (m : Account.t) =
  Context.Delegate.info (B b) m.pkh >>=? fun info ->
  Assert.equal_bool ~loc info.deactivated deactivated >>=? fun () ->
  if deactivated then check_no_stake ~loc b m else check_stake ~loc b m

let run_until_deactivation () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, (a1, a2)) ->
  Context.Contract.balance (B b) a1 >>=? fun balance_start ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  check_active_staking_balance ~loc:__LOC__ ~deactivated:false b m1
  >>=? fun () ->
  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Block.bake_until_cycle ~policy:(By_account m2.pkh) info.grace_period b
  >>=? fun b ->
  check_active_staking_balance ~loc:__LOC__ ~deactivated:false b m1
  >>=? fun () ->
  Block.bake_until_cycle_end ~policy:(By_account m2.pkh) b >>=? fun b ->
  check_active_staking_balance ~loc:__LOC__ ~deactivated:true b m1
  >|=? fun () -> (b, ((a1, m1), balance_start), (a2, m2))

(** From an initialized block with two contracts/accounts, the first
    one is active then deactivated. After baking, check that the
    account is active again. Baking rights are ensured. *)
let test_deactivation_then_bake () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((_deactivated_contract, deactivated_account), _start_balance),
             (_a2, _m2) ) ->
  Block.bake ~policy:(By_account deactivated_account.pkh) b >>=? fun b ->
  check_active_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_account

(** check that an account which is deactivated for [preserved_cycles] cannot be
   part of a committee *)
let test_a_really_deactivated_account_is_not_in_the_committee () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((_deactivated_contract, deactivated_account), _start_balance),
             (_a2, m2) ) ->
  (* at this point, the deactivated account can either bake (because it still
     has rights) and become active again, or, in case it is inactive for another
     [preserved_cycles], it has no more rights, thus cannot be part of the
     committee. *)
  let constants = Default_parameters.constants_test in
  Block.bake_until_n_cycle_end
    (constants.preserved_cycles + 1)
    ~policy:(By_account m2.pkh)
    b
  >>=? fun b ->
  Plugin.RPC.Baking_rights.get
    Block.rpc_ctxt
    ~delegates:[deactivated_account.pkh]
    b
  >>=? fun bakers ->
  match List.hd bakers with Some _ -> assert false | None -> return_unit

(** A deactivated account, after baking with self-delegation, is
    active again. Preservation of its balance is tested. Baking rights
    are ensured. *)
let test_deactivation_then_self_delegation () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((deactivated_contract, deactivated_account), _start_balance),
             (_a2, m2) ) ->
  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh)
  >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:self_delegation
  >>=? fun b ->
  check_active_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_account
  >>=? fun () -> check_stake ~loc:__LOC__ b deactivated_account

(** A deactivated account, which is emptied (into a newly created sink
    account), then self-delegated, becomes activated. Its balance is
    zero. Baking rights are ensured. *)
let test_deactivation_then_empty_then_self_delegation () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((deactivated_contract, deactivated_account), _start_balance),
             (_a2, m2) ) ->
  (* empty the contract *)
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  let sink_account = Account.new_account () in
  let sink_contract = Contract.Implicit sink_account.pkh in
  Context.get_constants (B b)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  cost_per_byte *? Int64.of_int origination_size >>?= fun origination_burn ->
  let amount =
    match balance -? origination_burn with Ok r -> r | Error _ -> assert false
  in
  Op.transaction (B b) deactivated_contract sink_contract amount
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b
  >>=? fun b1 ->
  (* the account is deactivated, the stake is 0. *)
  check_no_stake ~loc:__LOC__ b deactivated_account >>=? fun () ->
  (* self delegation *)
  Op.delegation (B b1) deactivated_contract (Some deactivated_account.pkh)
  >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b1
  >>=? fun b2 ->
  check_active_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b2
    deactivated_account
  >>=? fun () ->
  (* the account is activated, the stake is still 0. *)
  Context.Contract.balance (B b2) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance

(** A deactivated account, which is emptied, then self-delegated, then
    re-credited of the sunk amount, becomes active again. Staking
    rights remain consistent. *)
let test_deactivation_then_empty_then_self_delegation_then_recredit () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((deactivated_contract, deactivated_account), _start_balance),
             (_a2, m2) ) ->
  (* empty the contract *)
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  let sink_account = Account.new_account () in
  let sink_contract = Contract.Implicit sink_account.pkh in
  Context.get_constants (B b)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  cost_per_byte *? Int64.of_int origination_size >>?= fun origination_burn ->
  let amount =
    match balance -? origination_burn with Ok r -> r | Error _ -> assert false
  in
  Op.transaction
    ~force_reveal:true
    (B b)
    deactivated_contract
    sink_contract
    amount
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b
  >>=? fun b0 ->
  (* the account is deactivated, the stake is 0. *)
  check_no_stake ~loc:__LOC__ b deactivated_account >>=? fun () ->
  (**** self delegation *)
  Op.delegation (B b0) deactivated_contract (Some deactivated_account.pkh)
  >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b0
  >>=? fun b1 ->
  (* the account is still deactivated *)
  check_no_stake ~loc:__LOC__ b deactivated_account >>=? fun () ->
  (**** recredit *)
  Op.transaction
    ~force_reveal:true
    (B b1)
    sink_contract
    deactivated_contract
    amount
  >>=? fun recredit_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:recredit_contract b1
  >>=? fun b2 ->
  check_active_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b2
    deactivated_account
  >>=? fun () ->
  Context.Contract.balance (B b2) deactivated_contract >>=? fun balance2 ->
  Assert.equal_tez ~loc:__LOC__ amount balance2 >>=? fun () ->
  check_stake ~loc:__LOC__ b2 deactivated_account

(** Initialize a block with two contracts/accounts. A third new account is also
   created. The first account is self-delegated. First account sends to third
   one minimal_stake tez (so that, once it is active, it can appear in
   [Active_delegate_with_minimal_stake]. The third account has no delegate and is
   consistent for baking rights. Then, it is self-delegated and is supposed to
   be activated. Again, consistency for baking rights are preserved for the
   first and third accounts. *)
let test_delegation () =
  Context.init2 ~consensus_threshold:0 () >>=? fun (b, (a1, a2)) ->
  let m3 = Account.new_account () in
  Account.add_account m3 ;
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  let a3 = Contract.Implicit m3.pkh in
  Context.Contract.delegate_opt (B b) a1 >>=? fun delegate ->
  (match delegate with
  | None -> assert false
  | Some pkh -> assert (Signature.Public_key_hash.equal pkh m1.pkh)) ;
  let constants = Default_parameters.constants_test in
  let minimal_stake = constants.minimal_stake in
  Op.transaction ~force_reveal:true (B b) a1 a3 minimal_stake
  >>=? fun transact ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:transact >>=? fun b ->
  Context.Contract.delegate_opt (B b) a3 >>=? fun delegate ->
  (match delegate with None -> () | Some _ -> assert false) ;
  check_no_stake ~loc:__LOC__ b m3 >>=? fun () ->
  Op.delegation ~force_reveal:true (B b) a3 (Some m3.pkh) >>=? fun delegation ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:delegation >>=? fun b ->
  Context.Contract.delegate_opt (B b) a3 >>=? fun delegate ->
  (match delegate with
  | None -> assert false
  | Some pkh -> assert (Signature.Public_key_hash.equal pkh m3.pkh)) ;
  check_active_staking_balance ~loc:__LOC__ ~deactivated:false b m3
  >>=? fun () ->
  check_stake ~loc:__LOC__ b m3 >>=? fun () -> check_stake ~loc:__LOC__ b m1

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
