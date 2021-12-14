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
    Component:  Protocol (rolls)
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/main.exe -- test "^rolls$"
    Subject:    On rolls and baking rights.
                A delegate has baking rights provided that it has at least
                more than [token_per_rolls] tz of staking balance. This
                balance corresponds to the quantity of tez that have been
                delegated to it for baking rights. After a given number of
                cycles where it has not made use of its baking rights, its
                account will be deactivated for baker selection. To bake
                again, it will have to re-activate its account.
*)

open Protocol
open Alpha_context
open Test_tez

let account_pair = function [a1; a2] -> (a1, a2) | _ -> assert false

let wrap e = Lwt.return (Environment.wrap_tzresult e)

(** Baking rights consistency. Assert that the number of rolls for
    [account]'s pkh - equals to the number of expected rolls, i.e.,
    staking balance of [account] / (token_per_roll). As of protocol
    version 007, token_per_roll = 8000. Note that the consistency is
    verified against the value in the context, i.e. we are testing
    Storage.Roll.Delegate_roll_list. We do not use RPCs here. *)
let check_rolls (b : Block.t) (account : Account.t) =
  Context.get_constants (B b) >>=? fun constants ->
  Context.Delegate.info (B b) account.pkh >>=? fun {staking_balance; _} ->
  let token_per_roll = constants.parametric.tokens_per_roll in
  let expected_rolls =
    Int64.div (Tez.to_mutez staking_balance) (Tez.to_mutez token_per_roll)
  in
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctxt ->
  Roll_storage.count_rolls ctxt account.pkh >>= wrap >>=? fun rolls ->
  Assert.equal_int ~loc:__LOC__ rolls (Int64.to_int expected_rolls)

let check_no_rolls (b : Block.t) (account : Account.t) =
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctxt ->
  Roll_storage.count_rolls ctxt account.pkh >>= wrap >>=? fun rolls ->
  Assert.equal_int ~loc:__LOC__ rolls 0

(** Create a block with two initialized contracts/accounts. Assert
    that the first account has a staking balance that is equal to its
    own balance, and that its staking rights are consistent
    (check_rolls). *)
let test_simple_staking_rights () =
  Context.init 2 >>=? fun (b, accounts) ->
  let (a1, _a2) = account_pair accounts in
  Context.Contract.balance (B b) a1 >>=? fun balance ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance >>=? fun () ->
  check_rolls b m1

(** Create a block with two initialized contracts/accounts. Bake
    five blocks. Assert that the staking balance of the first account
    equals to its balance. Then both accounts have consistent staking
    rights. *)
let test_simple_staking_rights_after_baking () =
  Context.init 2 >>=? fun (b, accounts) ->
  let (a1, a2) = account_pair accounts in
  Context.Contract.balance (B b) a1 >>=? fun balance ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  Block.bake_n ~policy:(By_account m2.pkh) 5 b >>=? fun b ->
  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance >>=? fun () ->
  check_rolls b m1 >>=? fun () -> check_rolls b m2

let frozen_deposit (info : Context.Delegate.info) =
  Cycle.Map.fold
    (fun _ {Delegate.deposit; _} acc -> Test_tez.Tez.(deposit + acc))
    info.frozen_balance_by_cycle
    Tez.zero

let check_activate_staking_balance ~loc ~deactivated b (a, (m : Account.t)) =
  Context.Delegate.info (B b) m.pkh >>=? fun info ->
  Assert.equal_bool ~loc info.deactivated deactivated >>=? fun () ->
  Context.Contract.balance (B b) a >>=? fun balance ->
  let deposit = frozen_deposit info in
  Assert.equal_tez ~loc Test_tez.Tez.(balance + deposit) info.staking_balance

let run_until_deactivation () =
  Context.init 2 >>=? fun (b, accounts) ->
  let (a1, a2) = account_pair accounts in
  Context.Contract.balance (B b) a1 >>=? fun balance_start ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b (a1, m1)
  >>=? fun () ->
  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Block.bake_until_cycle ~policy:(By_account m2.pkh) info.grace_period b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b (a1, m1)
  >>=? fun () ->
  Block.bake_until_cycle_end ~policy:(By_account m2.pkh) b >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:true b (a1, m1)
  >|=? fun () -> (b, ((a1, m1), balance_start), (a2, m2))

(** From an initialized block with two contracts/accounts, the first
    one is active then deactivated. After baking, check that the
    account is active again. Baking rights are ensured. *)
let test_deactivation_then_bake () =
  run_until_deactivation ()
  >>=? fun ( b,
             ( ((_deactivated_contract, deactivated_account) as deactivated),
               _start_balance ),
             (_a2, _m2) ) ->
  Block.bake ~policy:(By_account deactivated_account.pkh) b >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated
  >>=? fun () -> check_rolls b deactivated_account

(** A deactivated account, after baking with self-delegation, is
    active again. Preservation of its balance is tested. Baking rights
    are ensured. *)
let test_deactivation_then_self_delegation () =
  run_until_deactivation ()
  >>=? fun ( b,
             ( ((deactivated_contract, deactivated_account) as deactivated),
               start_balance ),
             (_a2, m2) ) ->
  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh)
  >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:self_delegation
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated
  >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ start_balance balance >>=? fun () ->
  check_rolls b deactivated_account

(** A deactivated account, which is emptied (into a newly created sink
    account), then self-delegated, becomes activated. Its balance is
    zero. Baking rights are ensured. *)
let test_deactivation_then_empty_then_self_delegation () =
  run_until_deactivation ()
  >>=? fun ( b,
             ( ((deactivated_contract, deactivated_account) as deactivated),
               _start_balance ),
             (_a2, m2) ) ->
  (* empty the contract *)
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Context.get_constants (B b)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  let amount =
    match Tez.(balance -? origination_burn) with
    | Ok r -> r
    | Error _ -> assert false
  in
  Op.transaction (B b) deactivated_contract sink_contract amount
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b
  >>=? fun b ->
  (* self delegation *)
  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh)
  >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated
  >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance >>=? fun () ->
  check_rolls b deactivated_account

(** A deactivated account, which is emptied, then self-delegated, then
    re-credited of the sunk amount, becomes active again. Staking
    rights remain consistent. *)
let test_deactivation_then_empty_then_self_delegation_then_recredit () =
  run_until_deactivation ()
  >>=? fun ( b,
             ( ((deactivated_contract, deactivated_account) as deactivated),
               balance ),
             (_a2, m2) ) ->
  (* empty the contract *)
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Context.get_constants (B b)
  >>=? fun {parametric = {origination_size; cost_per_byte; _}; _} ->
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  let amount =
    match Tez.(balance -? origination_burn) with
    | Ok r -> r
    | Error _ -> assert false
  in
  Op.transaction (B b) deactivated_contract sink_contract amount
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b
  >>=? fun b ->
  (* self delegation *)
  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh)
  >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b
  >>=? fun b ->
  (* recredit *)
  Op.transaction (B b) sink_contract deactivated_contract amount
  >>=? fun recredit_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:recredit_contract b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated
  >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ amount balance >>=? fun () ->
  check_rolls b deactivated_account

(** Initialize a block with two contracts/accounts. A third new
    account is also created. The first account is self-delegated. First
    account sends to third one the amount of 0.5 tez. The third account
    has no delegate and is consistent for baking rights. Then, it is
    self-delegated and is supposed to be activated. Again, consistency
    for baking rights are preserved for the first and third accounts. *)
let test_delegation () =
  Context.init 2 >>=? fun (b, accounts) ->
  let (a1, a2) = account_pair accounts in
  let m3 = Account.new_account () in
  Account.add_account m3 ;
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  let a3 = Contract.implicit_contract m3.pkh in
  Context.Contract.delegate_opt (B b) a1 >>=? fun delegate ->
  (match delegate with
  | None -> assert false
  | Some pkh -> assert (Signature.Public_key_hash.equal pkh m1.pkh)) ;
  Op.transaction (B b) a1 a3 Tez.fifty_cents >>=? fun transact ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:transact >>=? fun b ->
  Context.Contract.delegate_opt (B b) a3 >>=? fun delegate ->
  (match delegate with None -> () | Some _ -> assert false) ;
  check_no_rolls b m3 >>=? fun () ->
  Op.delegation (B b) a3 (Some m3.pkh) >>=? fun delegation ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:delegation >>=? fun b ->
  Context.Contract.delegate_opt (B b) a3 >>=? fun delegate ->
  (match delegate with
  | None -> assert false
  | Some pkh -> assert (Signature.Public_key_hash.equal pkh m3.pkh)) ;
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b (a3, m3)
  >>=? fun () ->
  check_rolls b m3 >>=? fun () -> check_rolls b m1

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
    Tztest.tztest "delegation" `Quick test_delegation;
  ]
