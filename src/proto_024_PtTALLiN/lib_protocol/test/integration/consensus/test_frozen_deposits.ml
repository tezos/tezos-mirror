(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (frozen_deposits)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_frozen_deposits.ml
    Subject:    consistency of frozen deposits and the [set_deposits_limit] operation
 *)

open Protocol
open Alpha_context

let constants =
  {
    Default_parameters.constants_test with
    issuance_weights =
      {
        Default_parameters.constants_test.issuance_weights with
        base_total_issued_per_minute = Tez.zero;
      };
    consensus_threshold_size = 0;
    origination_size = 0;
  }

let get_first_2_accounts_contracts (a1, a2) =
  ((a1, Context.Contract.pkh a1), (a2, Context.Contract.pkh a2))

(* Terminology:

   - staking balance = full balance + delegated stake; obtained with
      Delegate.staking_balance

   - active stake = the amount of tez with which a delegate participates in
      consensus; it must be greater than [minimal_stake] and less or equal the staking
      balance; it is computed in [Delegate_sampler.select_distribution_for_cycle]

   - frozen deposits = represents frozen_deposits_percentage of the maximum stake during
      consensus_rights_delay + max_slashing_period cycles; obtained with
      Delegate.current_frozen_deposits

   - spendable balance = full balance - frozen deposits; obtained with Contract.balance

   - full balance = spendable balance + frozen deposits; obtained with Delegate.full_balance
*)
let test_invariants () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (contract1, account1), (contract2, _account2) =
    get_first_2_accounts_contracts contracts
  in
  let* staking_balance =
    Context.Delegate.staking_balance (B genesis) account1
  in
  let* full_balance = Context.Delegate.full_balance (B genesis) account1 in
  let* spendable_balance = Context.Contract.balance (B genesis) contract1 in
  let* frozen_deposits =
    Context.Delegate.current_frozen_deposits (B genesis) account1
  in
  (* before delegation *)
  let* () = Assert.equal_tez ~loc:__LOC__ full_balance staking_balance in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      full_balance
      Tez_helpers.(spendable_balance +! frozen_deposits)
  in
  (* to see how delegation plays a role, let's delegate to account1;
     N.B. account2 represents a delegate so it cannot delegate to account1; this is
     why we go through new_account as an intermediate *)
  let* spendable_balance2 = Context.Contract.balance (B genesis) contract2 in
  let new_account = (Account.new_account ()).pkh in
  let new_contract = Contract.Implicit new_account in
  (* we first put some money in new_account *)
  let* transfer =
    Op.transaction
      ~force_reveal:true
      (B genesis)
      contract2
      new_contract
      spendable_balance2
  in
  let* b = Block.bake ~operation:transfer genesis in
  let* new_account_balance = Context.Contract.balance (B b) new_contract in
  let* () =
    Assert.equal_tez ~loc:__LOC__ new_account_balance spendable_balance2
  in
  let* delegation =
    Op.delegation ~force_reveal:true (B b) new_contract (Some account1)
  in
  let* b1 = Block.bake ~operation:delegation b in
  let* b2 = Block.bake_until_cycle_end b1 in
  let* new_staking_balance = Context.Delegate.staking_balance (B b2) account1 in
  let* new_full_balance = Context.Delegate.full_balance (B b2) account1 in
  let* new_spendable_balance = Context.Contract.balance (B b2) contract1 in
  let* new_frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b2) account1
  in
  (* after delegation, we see the delegated stake reflected in the new staking
     balance of account1 *)
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance
      Tez_helpers.(new_full_balance +! new_account_balance)
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_full_balance
      Tez_helpers.(new_spendable_balance +! new_frozen_deposits)
  in
  Assert.equal_tez ~loc:__LOC__ new_frozen_deposits frozen_deposits

let adjust_staking_towards_limit ~limit ~block ~account ~contract =
  let open Lwt_result_syntax in
  (* Since we do not have the set_deposit_limit operation anymore (nor
     do we have automatic staking) this function adjusts the amount of
     staking towards the given [limit] for the given [account],
     [contract]. It takes a block and returns a new block if a stake
     or unstake operation is necessary, or the same block if the limit
     was already reached. *)
  let* fd = Context.Delegate.current_frozen_deposits (B block) account in
  if limit = fd then return block
  else
    match Tez.sub_opt limit fd with
    | Some diff ->
        let* adjustment_operation =
          Adaptive_issuance_helpers.stake (B block) contract diff
        in
        Block.bake ~operation:adjustment_operation block
    | None -> (
        match Tez.sub_opt fd limit with
        | None -> return block
        | Some diff ->
            let* adjustment_operation =
              Adaptive_issuance_helpers.unstake (B block) contract diff
            in
            Block.bake ~operation:adjustment_operation block)

let test_limit_with_overdelegation () =
  let open Lwt_result_syntax in
  let constants = {constants with limit_of_delegation_over_baking = 9} in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (contract1, account1), (contract2, account2) =
    get_first_2_accounts_contracts contracts
  in
  (* - [account1] and [account2] will give 80% of their balance to
       [new_account]
     - [new_account] will overdelegate to [account1] but [account1] will apply
       a frozen deposits target and limit to 15% of its stake *)
  let* initial_staking_balance =
    Context.Delegate.staking_balance (B genesis) account1
  in
  let* initial_staking_balance' =
    Context.Delegate.staking_balance (B genesis) account2
  in
  let amount = Tez_helpers.(initial_staking_balance *! 8L /! 10L) in
  let amount' = Tez_helpers.(initial_staking_balance' *! 8L /! 10L) in
  let limit = Tez_helpers.(initial_staking_balance *! 15L /! 100L) in
  let new_account = (Account.new_account ()).pkh in
  let new_contract = Contract.Implicit new_account in
  let* transfer1 =
    Op.transaction ~force_reveal:true (B genesis) contract1 new_contract amount
  in
  let* transfer2 =
    Op.transaction ~force_reveal:true (B genesis) contract2 new_contract amount'
  in
  let* b = Block.bake ~operations:[transfer1; transfer2] genesis in
  let expected_new_staking_balance =
    Tez_helpers.(initial_staking_balance -! amount)
  in
  let* new_staking_balance = Context.Delegate.staking_balance (B b) account1 in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance
      expected_new_staking_balance
  in
  let expected_new_staking_balance' =
    Tez_helpers.(initial_staking_balance' -! amount')
  in
  let* new_staking_balance' = Context.Delegate.staking_balance (B b) account2 in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance'
      expected_new_staking_balance'
  in
  let* delegation =
    Op.delegation ~force_reveal:true (B b) new_contract (Some account1)
  in
  let* b = Block.bake ~operation:delegation b in
  (* Overdelegation means that now there isn't enough staking, and the
     baker who wants to have its stake close to its defined limit
     should adjust it. *)
  let* b =
    adjust_staking_towards_limit
      ~block:b
      ~account:account1
      ~contract:contract1
      ~limit
  in
  let expected_new_frozen_deposits = limit in
  let* frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ frozen_deposits expected_new_frozen_deposits
  in
  let cycles_to_bake =
    2 * (constants.consensus_rights_delay + Constants.slashing_delay + 1)
  in
  let rec loop b n =
    if n = 0 then return b
    else
      let* b = Block.bake_until_cycle_end ~policy:(By_account account1) b in
      let* frozen_deposits =
        Context.Delegate.current_frozen_deposits (B b) account1
      in
      let* () =
        Assert.equal_tez
          ~loc:__LOC__
          frozen_deposits
          expected_new_frozen_deposits
      in
      loop b (pred n)
  in
  (* Check that frozen deposits do not change for a sufficient period of
     time *)
  let* (_ : Block.t) = loop b cycles_to_bake in
  return_unit

let test_may_not_bake_again_after_full_deposit_slash () =
  let open Lwt_result_syntax in
  let order_ops op1 op2 =
    let oph1 = Operation.hash op1 in
    let oph2 = Operation.hash op2 in
    let c = Operation_hash.compare oph1 oph2 in
    if c < 0 then (op1, op2) else (op2, op1)
  in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (good_contract, good_account), (slashed_contract, slashed_account) =
    get_first_2_accounts_contracts contracts
  in
  let* operation =
    Op.transaction
      (B genesis)
      good_contract
      slashed_contract
      Alpha_context.Tez.one_cent
  in
  let* blk_a =
    Block.bake ~policy:(By_account slashed_account) ~operation genesis
  in
  let* blk_b = Block.bake ~policy:(By_account slashed_account) genesis in
  let* preattestation1 =
    Op.raw_preattestation ~manager_pkh:slashed_account blk_a
  in
  let* preattestation2 =
    Op.raw_preattestation ~manager_pkh:slashed_account blk_b
  in
  let preattestation1, preattestation2 =
    order_ops preattestation1 preattestation2
  in
  let double_preattestation_op =
    Op.double_preattestation (B blk_a) preattestation1 preattestation2
  in
  let* b =
    Block.bake
      ~policy:(By_account good_account)
      ~operation:double_preattestation_op
      blk_a
  in
  let* fd_before =
    Context.Delegate.current_frozen_deposits (B b) slashed_account
  in
  let* operation =
    Op.transaction
      (B b)
      good_contract
      slashed_contract
      Alpha_context.Tez.one_cent
  in
  let* blk_a = Block.bake ~policy:(By_account slashed_account) ~operation b in
  let* blk_b = Block.bake ~policy:(By_account slashed_account) b in
  let* attestation1 = Op.raw_attestation ~manager_pkh:slashed_account blk_a in
  let* attestation2 = Op.raw_attestation ~manager_pkh:slashed_account blk_b in
  let attestation1, attestation2 = order_ops attestation1 attestation2 in
  let double_attestation_op =
    Op.double_attestation (B blk_a) attestation1 attestation2
  in
  let* b =
    Block.bake
      ~policy:(By_account good_account)
      ~operation:double_attestation_op
      b
  in
  (* The [slashed_account]'s deposits haven't changed yet... *)
  let* fd = Context.Delegate.current_frozen_deposits (B b) slashed_account in
  let* () = Assert.equal_tez ~loc:__LOC__ fd fd_before in
  (* ...though we are immediately not allowed to bake with [slashed_account] *)
  let*! res = Block.bake ~policy:(By_account slashed_account) b in
  let* () =
    Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Forbidden_delegate _ -> true
      | _ -> false)
  in
  let* b, metadata, _ =
    Block.bake_until_cycle_end_with_metadata ~policy:(By_account good_account) b
  in
  (* Assert that the [slashed_account]'s deposits are increased by autostaking. *)
  let metadata = Option.value_f ~default:(fun () -> assert false) metadata in
  let autostaked = Block.autostaked slashed_account metadata in
  let* fd = Context.Delegate.current_frozen_deposits (B b) slashed_account in
  let* () = Assert.equal_tez ~loc:__LOC__ fd autostaked in
  (* Though [slashed_account] is still forbidden for two more cycles. *)
  let*! res = Block.bake ~policy:(By_account slashed_account) b in
  let* () =
    Assert.proto_error ~loc:__LOC__ res (function
      | Validate_errors.Consensus.Forbidden_delegate _ -> true
      | _ -> false)
  in
  let* b = Block.bake_until_n_cycle_end 2 ~policy:(By_account good_account) b in
  (* Check that [slashed_account] can bake since it's a new cycle and
     autostake increased the frozen deposits enough to bake. *)
  let* (_ : Block.t) = Block.bake ~policy:(By_account slashed_account) b in
  return_unit

let test_deposits_after_stake_removal () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (contract1, account1), (contract2, account2) =
    get_first_2_accounts_contracts contracts
  in
  let* initial_frozen_deposits_1 =
    Context.Delegate.current_frozen_deposits (B genesis) account1
  in
  let* initial_frozen_deposits_2 =
    Context.Delegate.current_frozen_deposits (B genesis) account2
  in
  (* Move half the account1's balance to account2 *)
  let* full_balance = Context.Delegate.full_balance (B genesis) account1 in
  let half_balance = Tez_helpers.(full_balance /! 2L) in
  let* operation =
    Op.transaction (B genesis) contract1 contract2 half_balance
  in
  let* b = Block.bake ~operation genesis in
  let* frozen_deposits_1 =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ frozen_deposits_1 initial_frozen_deposits_1
  in
  let* frozen_deposits_2 =
    Context.Delegate.current_frozen_deposits (B b) account2
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ frozen_deposits_2 initial_frozen_deposits_2
  in
  (* Bake a cycle. *)
  let* b = Block.bake_until_cycle_end b in
  let* frozen_deposits_2 =
    Context.Delegate.current_frozen_deposits (B b) account2
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ frozen_deposits_2 initial_frozen_deposits_2
  in
  (* Updating initial_frozen_deposits_x of accountx after autostaking  *)
  let* initial_frozen_deposits_1 =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* initial_frozen_deposits_2 =
    Context.Delegate.current_frozen_deposits (B b) account2
  in
  (* Frozen deposits aren't affected by balance change... *)
  let rec loop b n =
    if n = 0 then return b
    else
      let* frozen_deposits_1 =
        Context.Delegate.current_frozen_deposits (B b) account1
      in
      let* () =
        Assert.equal_tez
          ~loc:__LOC__
          frozen_deposits_1
          initial_frozen_deposits_1
      in
      let* frozen_deposits_2 =
        Context.Delegate.current_frozen_deposits (B b) account2
      in
      let* () =
        Assert.equal_tez
          ~loc:__LOC__
          frozen_deposits_2
          initial_frozen_deposits_2
      in
      let* b, _, _ = Block.bake_until_cycle_end_with_metadata b in
      loop b (pred n)
  in
  (* the frozen deposits for account1 do not change until [preserved cycles +
     max_slashing_period] are baked (-1 because we already baked a cycle) *)
  let* b =
    loop b (constants.consensus_rights_delay + Constants.slashing_delay)
  in
  (* and still after preserved cycles + max_slashing_period, the frozen_deposits
     for account1 won't reflect the decrease in account1's active stake
     without manual staking. *)
  let* frozen_deposits_1 =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ frozen_deposits_1 initial_frozen_deposits_1
  in
  (* similarly account2's frozen deposits aren't increased automatically *)
  let* frozen_deposits_2 =
    Context.Delegate.current_frozen_deposits (B b) account2
  in
  Assert.equal_tez ~loc:__LOC__ frozen_deposits_2 initial_frozen_deposits_2

let test_frozen_deposits_with_deactivation () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (_contract1, account1), (_contract2, account2) =
    get_first_2_accounts_contracts contracts
  in
  let* initial_frozen_deposits =
    Context.Delegate.current_frozen_deposits (B genesis) account1
  in
  (* [account1] will not participate (ie bake/attest); we set the
     expected last cycles at which it is considered active and at
     which it has non-zero deposits *)
  let last_active_cycle =
    constants.tolerated_inactivity_period + constants.consensus_rights_delay
    (* according to [Delegate_activation_storage.set_active] *)
  in
  let last_cycle_with_deposits =
    last_active_cycle + constants.consensus_rights_delay
    + Constants.slashing_delay + 1
    (* according to [Delegate_storage.freeze_deposits] *)
  in
  let cycles_to_bake =
    last_cycle_with_deposits + constants.consensus_rights_delay
  in
  let rec loop b n =
    if n = 0 then return b
    else
      let* b = Block.bake_until_cycle_end ~policy:(By_account account2) b in
      let* is_deactivated = Context.Delegate.deactivated (B b) account1 in
      let* frozen_deposits =
        Context.Delegate.current_frozen_deposits (B b) account1
      in
      let new_cycle = cycles_to_bake - n + 1 in
      let* () =
        Assert.equal_bool
          ~loc:__LOC__
          is_deactivated
          (new_cycle > last_active_cycle)
      in
      let* () =
        Assert.equal_tez ~loc:__LOC__ frozen_deposits initial_frozen_deposits
      in
      loop b (pred n)
  in
  let* (_ : Block.t) = loop genesis cycles_to_bake in
  return_unit

let test_frozen_deposits_with_delegation () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (_contract1, account1), (contract2, account2) =
    get_first_2_accounts_contracts contracts
  in
  let* initial_staking_balance =
    Context.Delegate.staking_balance (B genesis) account1
  in
  let* initial_frozen_deposits =
    Context.Delegate.current_frozen_deposits (B genesis) account1
  in
  let* delegated_amount = Context.Contract.balance (B genesis) contract2 in
  let new_account = Account.new_account () in
  let new_contract = Contract.Implicit new_account.pkh in
  let* transfer =
    Op.transaction
      ~force_reveal:true
      (B genesis)
      contract2
      new_contract
      delegated_amount
  in
  let* b = Block.bake ~operation:transfer genesis in
  let* new_staking_balance = Context.Delegate.staking_balance (B b) account2 in
  let expected_new_staking_balance =
    Tez_helpers.(initial_staking_balance -! delegated_amount)
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance
      expected_new_staking_balance
  in
  let* delegation =
    Op.delegation ~force_reveal:true (B b) new_contract (Some account1)
  in
  let* b = Block.bake ~operation:delegation b in
  let expected_new_staking_balance =
    Tez_helpers.(initial_staking_balance +! delegated_amount)
  in
  let* new_staking_balance = Context.Delegate.staking_balance (B b) account1 in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance
      expected_new_staking_balance
  in
  (* Bake one cycle. *)
  let* b = Block.bake_until_cycle_end b in
  let* new_frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ new_frozen_deposits initial_frozen_deposits
  in
  let cycles_to_bake =
    2 * (constants.consensus_rights_delay + Constants.slashing_delay + 1)
  in
  let rec loop b n =
    if n = 0 then return b
    else
      let* b = Block.bake_until_cycle_end ~policy:(By_account account1) b in
      let* frozen_deposits =
        Context.Delegate.current_frozen_deposits (B b) account1
      in
      let* () =
        Assert.equal_tez ~loc:__LOC__ frozen_deposits initial_frozen_deposits
      in
      loop b (pred n)
  in
  (* Check that frozen deposits do not change for a sufficient period of
     time *)
  let* (_ : Block.t) = loop b cycles_to_bake in
  return_unit

let test_frozen_deposits_with_overdelegation () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (contract1, account1), (contract2, account2) =
    get_first_2_accounts_contracts contracts
  in
  (* - [account1] and [account2] give their spendable balance to [new_account]
     - [new_account] overdelegates to [account1] *)
  let* initial_staking_balance =
    Context.Delegate.staking_balance (B genesis) account1
  in
  let* initial_staking_balance' =
    Context.Delegate.staking_balance (B genesis) account2
  in
  let* initial_frozen_deposits =
    Context.Delegate.current_frozen_deposits (B genesis) account1
  in
  let* amount = Context.Contract.balance (B genesis) contract1 in
  let* amount' = Context.Contract.balance (B genesis) contract2 in
  let new_account = (Account.new_account ()).pkh in
  let new_contract = Contract.Implicit new_account in
  let* transfer1 =
    Op.transaction ~force_reveal:true (B genesis) contract1 new_contract amount
  in
  let* transfer2 =
    Op.transaction ~force_reveal:true (B genesis) contract2 new_contract amount'
  in
  let* b = Block.bake ~operations:[transfer1; transfer2] genesis in
  let expected_new_staking_balance =
    Tez_helpers.(initial_staking_balance -! amount)
  in
  let* new_staking_balance = Context.Delegate.staking_balance (B b) account1 in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance
      expected_new_staking_balance
  in
  let expected_new_staking_balance' =
    Tez_helpers.(initial_staking_balance' -! amount')
  in
  let* new_staking_balance' = Context.Delegate.staking_balance (B b) account2 in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance'
      expected_new_staking_balance'
  in
  let* delegation =
    Op.delegation ~force_reveal:true (B b) new_contract (Some account1)
  in
  let* b = Block.bake ~operation:delegation b in
  let* new_staking_balance = Context.Delegate.staking_balance (B b) account1 in
  let expected_new_staking_balance =
    Tez_helpers.(initial_frozen_deposits +! amount +! amount')
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_staking_balance
      expected_new_staking_balance
  in
  (* Finish the cycle to update the frozen deposits *)
  let* b = Block.bake_until_cycle_end b in
  let* expected_new_frozen_deposits =
    Context.Delegate.full_balance (B b) account1
  in
  (* the equality follows from the definition of active stake in
     [Delegate_sampler.select_distribution_for_cycle]. *)
  assert (initial_frozen_deposits = expected_new_frozen_deposits) ;
  let* new_frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* () =
    Assert.equal_tez
      ~loc:__LOC__
      new_frozen_deposits
      expected_new_frozen_deposits
  in
  let cycles_to_bake =
    2 * (constants.consensus_rights_delay + Constants.slashing_delay + 1)
  in
  let* frozen_deposits =
    Context.Delegate.current_frozen_deposits (B b) account1
  in
  let* () =
    Assert.equal_tez ~loc:__LOC__ frozen_deposits expected_new_frozen_deposits
  in
  let rec loop b n =
    if n = 0 then return b
    else
      let* b = Block.bake_until_cycle_end ~policy:(By_account account1) b in
      let* frozen_deposits =
        Context.Delegate.current_frozen_deposits (B b) account1
      in
      let* () =
        Assert.equal_tez
          ~loc:__LOC__
          frozen_deposits
          expected_new_frozen_deposits
      in
      loop b (pred n)
  in
  (* Check that frozen deposits do not change for a sufficient period of
     time *)
  let* (_ : Block.t) = loop b cycles_to_bake in
  return_unit

(** This test fails when [to_cycle] in [Delegate.freeze_deposits] is smaller than
   [new_cycle + consensus_rights_delay]. *)
let test_error_is_thrown_when_smaller_upper_bound_for_frozen_window () =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let contract1, contract2 = contracts in
  let account1 = Context.Contract.pkh contract1 in
  (* [account2] delegates (through [new_account]) to [account1] its spendable
     balance. The point is to make [account1] have a lot of staking balance so
     that, after [consensus_rights_delay] when the active stake reflects this increase
     in staking balance, its [maximum_stake_to_be_deposited] is bigger than the frozen
     deposit which is computed on a smaller window because [to_cycle] is smaller
     than [new_cycle + consensus_rights_delay]. *)
  let* delegated_amount = Context.Contract.balance (B genesis) contract2 in
  let new_account = Account.new_account () in
  let new_contract = Contract.Implicit new_account.pkh in
  let* transfer =
    Op.transaction
      ~force_reveal:true
      (B genesis)
      contract2
      new_contract
      delegated_amount
  in
  let* b = Block.bake ~operation:transfer genesis in
  let* delegation =
    Op.delegation ~force_reveal:true (B b) new_contract (Some account1)
  in
  let* b = Block.bake ~operation:delegation b in
  let* b = Block.bake_until_cycle_end b in
  (* After 1 cycle, namely, at cycle 2, [account1] transfers all its spendable
     balance. *)
  let* balance1 = Context.Contract.balance (B b) contract1 in
  let* operation =
    Op.transaction ~force_reveal:true (B b) contract1 contract2 balance1
  in
  let* b = Block.bake ~operation b in
  let* (_ : Block.t) =
    Block.bake_until_n_cycle_end constants.consensus_rights_delay b
  in
  (* By this time, after [consensus_rights_delay] passed after [account1] has emptied
        its spendable balance, because [account1] had a big staking balance at
        cycle 0, at this cycle it has a big active stake, and so its
        [maximum_stake_to_be_deposited] too is bigger than [frozen_deposits.current_amount],
        so the variable [to_freeze] in [freeze_deposits] is positive.
     Because the spendable balance of [account1] is 0, an error "Underflowing
        subtraction" is raised at the end of the cycle when updating the balance by
        subtracting [to_freeze] in [freeze_deposits].
     Note that by taking [to_cycle] is [new_cycle + consensus_rights_delay],
        [frozen_deposits.current_amount] can no longer be smaller
        than [maximum_stake_to_be_deposited], that is, the invariant
        maximum_stake_to_be_deposited <= frozen_deposits + balance is preserved.
  *)
  return_unit

let test_set_frozen_deposits_limit_fails () =
  let open Lwt_result_syntax in
  let* b, contract = Context.init_with_constants1 constants in
  let* i = Incremental.begin_construction b in
  let expect_apply_failure =
    Error_helpers.check_error_constructor_name
      ~loc:__LOC__
      ~expected:Protocol.Apply.Set_deposits_limit_when_automated_staking_off
  in
  let check target_limit =
    let* op = Op.set_deposits_limit (I i) contract target_limit in
    let* _i = Incremental.add_operation i op ~expect_apply_failure in
    return_unit
  in
  let* () = check None in
  let* () = check (Some Tez.zero) in
  check (Some (Tez_helpers.of_int 1_000_000_000))

let tests =
  Tztest.
    [
      tztest "invariants" `Quick test_invariants;
      tztest
        "deposits after stake removal"
        `Quick
        test_deposits_after_stake_removal;
      tztest
        "frozen deposits with deactivation"
        `Quick
        test_frozen_deposits_with_deactivation;
      tztest
        "frozen deposits with delegation"
        `Quick
        test_frozen_deposits_with_delegation;
      tztest
        "test simulation of limited staking with overdelegation"
        `Quick
        test_limit_with_overdelegation;
      (* This test has been deactivated following the changes of the
         forbidding mechanism that now forbids delegates right after
         the first denunciation, it should be fixed and reactivated
         https://gitlab.com/tezos/tezos/-/issues/6904 *)
      (* tztest *)
      (*   "test cannot bake again after full deposit slash" *)
      (*   `Quick *)
      (*   test_may_not_bake_again_after_full_deposit_slash; *)
      tztest
        "frozen deposits with overdelegation"
        `Quick
        test_frozen_deposits_with_overdelegation;
      tztest
        "error is thrown when the frozen window is smaller"
        `Quick
        test_error_is_thrown_when_smaller_upper_bound_for_frozen_window;
      tztest
        "set_deposits_limit operation always fails"
        `Quick
        test_set_frozen_deposits_limit_fails;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("frozen deposits", tests)]
  |> Lwt_main.run
