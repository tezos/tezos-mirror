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
    Component:  Protocol (frozen_bonds)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^staking$"
    Subject:    TODO
 *)

open Protocol
open Alpha_context

let constants =
  {
    Tezos_protocol_alpha_parameters.Default_parameters.constants_test with
    endorsing_reward_per_slot = Tez.zero;
    baking_reward_bonus_per_slot = Tez.zero;
    baking_reward_fixed_portion = Tez.zero;
    consensus_threshold = 0;
    origination_size = 0;
  }

let get_first_2_accounts_contracts contracts =
  let ((contract1, account1), (contract2, account2)) =
    match contracts with
    | [a1; a2] ->
        ( ( a1,
            Contract.is_implicit a1 |> function
            | None -> assert false
            | Some pkh -> pkh ),
          ( a2,
            Contract.is_implicit a2 |> function
            | None -> assert false
            | Some pkh -> pkh ) )
    | _ -> assert false
  in
  ((contract1, account1), (contract2, account2))

let test_not_in_active_delegate_with_one_roll () =
  Context.init_with_constants constants 2 >>=? fun (genesis, contracts) ->
  let ((contract1, account1), (_contract2, account2)) =
    match contracts with
    | [a1; a2] ->
        ( ( a1,
            Contract.is_implicit a1 |> function
            | None -> assert false
            | Some pkh -> pkh ),
          ( a2,
            Contract.is_implicit a2 |> function
            | None -> assert false
            | Some pkh -> pkh ) )
    | _ -> assert false
  in
  (* [account1] delegates its spendable balance *)
  Context.Contract.balance (B genesis) contract1 >>=? fun delegated_amount ->
  let new_account = Account.new_account () in
  let new_contract = Contract.implicit_contract new_account.pkh in
  Op.transaction (B genesis) contract1 new_contract delegated_amount
  >>=? fun transfer ->
  Block.bake ~operation:transfer genesis >>=? fun b ->
  (* TODO_TB: justify the calculations *)
  let cycles_to_zero_deposit =
    constants.preserved_cycles
    + (2 * (constants.preserved_cycles + constants.max_slashing_period))
  in
  (* by this time, [account1] has 0 frozen deposits because all the deposits are
     unfrozen. *)
  Block.bake_until_n_cycle_end
    ~policy:(By_account account2)
    cycles_to_zero_deposit
    b
  >>=? fun b ->
  Context.Delegate.deactivated (B b) account1 >>=? fun is_deactivated ->
  Context.Delegate.frozen_deposits (B b) account1 >>=? fun frozen_bonds ->
  Assert.equal_bool ~loc:__LOC__ is_deactivated true >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ frozen_bonds Tez.zero >>=? fun () ->
  Context.Contract.balance (B b) contract1 >>=? fun delegated_amount ->
  (* [account1] delegates its spendable balance. *)
  Op.transaction (B b) contract1 new_contract delegated_amount
  >>=? fun transfer ->
  Block.bake ~operation:transfer b >>=? fun b ->
  (* this triggers the else case in [add_stake] *)
  Op.transaction (B b) new_contract contract1 Tez.one >>=? fun transfer ->
  Block.bake ~operation:transfer b >>=? fun b ->
  Context.Delegate.deactivated (B b) account1 >>=? fun is_deactivated ->
  Context.Contract.balance (B b) contract1 >>=? fun spendable_balance ->
  Context.Delegate.frozen_deposits (B b) account1 >>=? fun frozen_bonds ->
  Assert.equal_bool ~loc:__LOC__ is_deactivated true >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ frozen_bonds Tez.zero >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ spendable_balance Tez.one >>=? fun () ->
  (* this triggers the else case in [remove_stake] *)
  Op.transaction (B b) contract1 new_contract Tez.one >>=? fun transfer ->
  Block.bake ~operation:transfer b >>=? fun b ->
  Context.Delegate.deactivated (B b) account1 >>=? fun is_deactivated ->
  Context.Contract.balance (B b) contract1 >>=? fun spendable_balance ->
  Context.Delegate.frozen_deposits (B b) account1 >>=? fun frozen_bonds ->
  Assert.equal_bool ~loc:__LOC__ is_deactivated true >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ frozen_bonds Tez.zero >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ spendable_balance Tez.zero

let tests =
  Tztest.
    [
      tztest "test not one roll" `Quick test_not_in_active_delegate_with_one_roll;
    ]
