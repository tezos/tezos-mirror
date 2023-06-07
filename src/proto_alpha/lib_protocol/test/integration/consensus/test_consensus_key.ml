(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (delegate_storage)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_consensus_key.ml
    Subject:    consistency of the [Drain_delegate] operation
 *)

open Protocol
open Alpha_context

let constants =
  {
    Default_parameters.constants_test with
    endorsing_reward_per_slot = Tez.zero;
    baking_reward_bonus_per_slot = Tez.zero;
    baking_reward_fixed_portion = Tez.zero;
    consensus_threshold = 0;
    origination_size = 0;
  }

(** Checks that staking balance is sum of delegators' stake. *)
let check_delegate_staking_invariant blk delegate_pkh =
  Context.Delegate.staking_balance (B blk) delegate_pkh
  >>=? fun delegate_staking_balance ->
  Context.Delegate.full_balance (B blk) delegate_pkh
  >>=? fun self_staking_balance ->
  Context.Delegate.info (B blk) delegate_pkh >>=? fun delegate_info ->
  let delegate_contract = Contract.Implicit delegate_pkh in
  let delegated_contracts =
    List.filter
      (fun c -> Contract.(c <> delegate_contract))
      delegate_info.delegated_contracts
  in
  List.fold_left_es
    (fun total pkh ->
      Context.Contract.balance_and_frozen_bonds (B blk) pkh
      >>=? fun staking_balance ->
      Lwt.return Tez.(total +? staking_balance) >|= Environment.wrap_tzresult)
    self_staking_balance
    delegated_contracts
  >>=? fun delegators_stake ->
  Assert.equal_tez ~loc:__LOC__ delegate_staking_balance delegators_stake

let update_consensus_key blk delegate public_key =
  let nb_delay_cycles = constants.preserved_cycles + 1 in
  Op.update_consensus_key (B blk) (Contract.Implicit delegate) public_key
  >>=? fun update_ck ->
  Block.bake ~operation:update_ck blk >>=? fun blk' ->
  Block.bake_until_n_cycle_end nb_delay_cycles blk'

let delegate_stake blk source delegate =
  Op.delegation (B blk) (Contract.Implicit source) (Some delegate)
  >>=? fun delegation -> Block.bake ~operation:delegation blk

let transfer_tokens blk source destination amount =
  Op.transaction
    (B blk)
    (Contract.Implicit source)
    (Contract.Implicit destination)
    amount
  >>=? fun transfer_op -> Block.bake ~operation:transfer_op blk

let may_reveal_manager_key blk (pkh, pk) =
  let open Lwt_result_syntax in
  let* is_revealed =
    Context.Contract.is_manager_key_revealed (B blk) (Contract.Implicit pkh)
  in
  if is_revealed then return blk
  else
    Op.revelation (B blk) pk >>=? fun reveal_op ->
    Block.bake ~operation:reveal_op blk

let drain_delegate ~policy blk consensus_key delegate destination
    expected_final_balance =
  Op.drain_delegate (B blk) ~consensus_key ~delegate ~destination
  >>=? fun drain_del ->
  Block.bake ~policy ~operation:drain_del blk >>=? fun blk' ->
  check_delegate_staking_invariant blk' delegate >>=? fun () ->
  Context.Contract.balance (B blk') (Contract.Implicit delegate)
  >>=? fun final_balance ->
  Assert.equal_tez ~loc:__LOC__ final_balance expected_final_balance

let get_first_2_accounts_contracts (a1, a2) =
  ((a1, Context.Contract.pkh a1), (a2, Context.Contract.pkh a2))

let test_drain_delegate_scenario f =
  Context.init_with_constants2 constants >>=? fun (genesis, contracts) ->
  let (_contract1, account1_pkh), (_contract2, account2_pkh) =
    get_first_2_accounts_contracts contracts
  in
  let consensus_account = Account.new_account () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  transfer_tokens genesis account2_pkh consensus_pkh Tez.one_mutez
  >>=? fun blk' ->
  update_consensus_key blk' delegate consensus_pk >>=? fun blk' ->
  f blk' consensus_pkh consensus_pk delegate

let test_drain_delegate ~low_balance ~exclude_ck ~ck_delegates () =
  test_drain_delegate_scenario (fun blk consensus_pkh consensus_pk delegate ->
      let policy =
        if exclude_ck then Block.Excluding [consensus_pkh]
        else Block.By_account delegate
      in
      (if ck_delegates then
       may_reveal_manager_key blk (consensus_pkh, consensus_pk) >>=? fun blk ->
       delegate_stake blk consensus_pkh delegate
      else return blk)
      >>=? fun blk ->
      Context.Contract.balance (B blk) (Contract.Implicit delegate)
      >>=? fun delegate_balance ->
      (if low_balance then
       transfer_tokens blk delegate consensus_pkh delegate_balance
       >>=? fun blk ->
       may_reveal_manager_key blk (consensus_pkh, consensus_pk) >>=? fun blk ->
       transfer_tokens blk consensus_pkh delegate Tez.(of_mutez_exn 1_000_000L)
      else return blk)
      >>=? fun blk ->
      Context.Contract.balance (B blk) (Contract.Implicit delegate)
      >>=? fun delegate_balance ->
      let expected_final_balance =
        if exclude_ck then Tez.zero
        else Tez.(max one (div_exn delegate_balance 100))
      in
      drain_delegate
        ~policy
        blk
        consensus_pkh
        delegate
        consensus_pkh
        expected_final_balance)

let test_drain_empty_delegate ~exclude_ck () =
  test_drain_delegate_scenario (fun blk consensus_pkh _consensus_pk delegate ->
      let policy =
        if exclude_ck then Block.Excluding [consensus_pkh]
        else Block.By_account delegate
      in
      Context.Contract.balance (B blk) (Contract.Implicit delegate)
      >>=? fun delegate_balance ->
      transfer_tokens blk delegate consensus_pkh delegate_balance
      >>=? fun blk ->
      drain_delegate ~policy blk consensus_pkh delegate consensus_pkh Tez.zero
      >>= fun res ->
      Assert.proto_error_with_info
        ~loc:__LOC__
        res
        "Drain delegate without enough balance for allocation burn or drain \
         fees")

let test_tz4_consensus_key () =
  Context.init_with_constants1 constants >>=? fun (genesis, contracts) ->
  let account1_pkh = Context.Contract.pkh contracts in
  let consensus_account = Account.new_account ~algo:Bls () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  transfer_tokens genesis account1_pkh consensus_pkh Tez.one_mutez
  >>=? fun blk' ->
  Op.update_consensus_key (B blk') (Contract.Implicit delegate) consensus_pk
  >>=? fun operation ->
  let tz4_pk = match consensus_pk with Bls pk -> pk | _ -> assert false in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          (Delegate_consensus_key.Invalid_consensus_key_update_tz4 pk);
      ]
      when Signature.Bls.Public_key.(pk = tz4_pk) ->
        return_unit
    | err ->
        failwith
          "Error trace:@,\
          \ %a does not match the \
           [Delegate_consensus_key.Invalid_consensus_key_update_tz4] error"
          Error_monad.pp_print_trace
          err
  in
  Incremental.begin_construction blk' >>=? fun inc ->
  Incremental.validate_operation ~expect_failure inc operation
  >>=? fun (_i : Incremental.t) -> return_unit

let test_endorsement_with_consensus_key () =
  Context.init_with_constants1 constants >>=? fun (genesis, contracts) ->
  let account1_pkh = Context.Contract.pkh contracts in
  let consensus_account = Account.new_account () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  transfer_tokens genesis account1_pkh consensus_pkh Tez.one_mutez
  >>=? fun blk' ->
  update_consensus_key blk' delegate consensus_pk >>=? fun b_pre ->
  Block.bake b_pre >>=? fun b ->
  let slot = Slot.of_int_do_not_use_except_for_parameters 0 in
  Op.endorsement ~delegate:account1_pkh ~slot b >>=? fun endorsement ->
  Block.bake ~operation:endorsement b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Operation.Invalid_signature -> true
      | _ -> false)
  >>=? fun () ->
  Op.endorsement ~delegate:consensus_pkh ~slot b >>=? fun endorsement ->
  Block.bake ~operation:endorsement b >>=? fun (_good_block : Block.t) ->
  return_unit

let tests =
  Tztest.
    [
      tztest
        "drain delegate high balance, excluding ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:true
           ~ck_delegates:true);
      tztest
        "drain delegate high balance, excluding ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:true
           ~ck_delegates:false);
      tztest
        "drain delegate high balance, with ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:false
           ~ck_delegates:true);
      tztest
        "drain delegate high balance, with ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:false
           ~ck_delegates:false);
      tztest
        "drain delegate low balance, excluding ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:true
           ~ck_delegates:true);
      tztest
        "drain delegate low balance, excluding ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:true
           ~ck_delegates:false);
      tztest
        "drain delegate low balance, with ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:false
           ~ck_delegates:true);
      tztest
        "drain delegate low balance, with ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:false
           ~ck_delegates:false);
      tztest
        "empty drain delegate excluding ck"
        `Quick
        (test_drain_empty_delegate ~exclude_ck:true);
      tztest
        "empty drain delegate with ck"
        `Quick
        (test_drain_empty_delegate ~exclude_ck:false);
      tztest "tz4 consensus key" `Quick test_tz4_consensus_key;
      tztest "endorsement with ck" `Quick test_endorsement_with_consensus_key;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("consensus key", tests)]
  |> Lwt_main.run
