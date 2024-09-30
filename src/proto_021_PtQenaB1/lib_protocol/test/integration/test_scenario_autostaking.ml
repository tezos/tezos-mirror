(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Autostaking
    Invocation:   dune exec src/proto_021_PtQenaB1/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_autostaking.ml
    Subject:      Test autostaking in the protocol.
*)

open State_account
open Tez_helpers.Ez_tez
open Scenario
open Log_helpers

let assert_balance_evolution ~loc ~for_accounts ~part ~name ~old_balance
    ~new_balance compare =
  let open Lwt_result_syntax in
  let old_b, new_b =
    match part with
    | `liquid ->
        ( Q.of_int64 @@ Tez.to_mutez old_balance.liquid_b,
          Q.of_int64 @@ Tez.to_mutez new_balance.liquid_b )
    | `staked -> (old_balance.staked_b, new_balance.staked_b)
    | `unstaked_frozen ->
        ( Q.of_int64 @@ Tez.to_mutez old_balance.unstaked_frozen_b,
          Q.of_int64 @@ Tez.to_mutez new_balance.unstaked_frozen_b )
    | `unstaked_finalizable ->
        ( Q.of_int64 @@ Tez.to_mutez old_balance.unstaked_finalizable_b,
          Q.of_int64 @@ Tez.to_mutez new_balance.unstaked_finalizable_b )
  in
  if List.mem ~equal:String.equal name for_accounts then
    if compare new_b old_b then return_unit
    else (
      Log.debug ~color:warning_color "Balances changes failed:@." ;
      Log.debug "@[<v 2>Old Balance@ %a@]@." balance_pp old_balance ;
      Log.debug "@[<v 2>New Balance@ %a@]@." balance_pp new_balance ;
      Log.debug
        "@[<v 2>Diff between balances@ %a@]@."
        Q.pp_print
        Q.(new_b - old_b) ;
      failwith "%s Unexpected stake evolution for %s" loc name)
  else (
    Log.error
      "Test_scenario_autostaking.assert_balance_evolution: account %s not found"
      name ;
    assert false)

let gt_diff diff new_b old_b =
  let diff = Q.of_int64 @@ Tez.to_mutez diff in
  Q.equal new_b (Q.add old_b diff)

let lt_diff diff new_b old_b =
  let diff = Q.of_int64 @@ Tez.to_mutez diff in
  Q.equal new_b (Q.sub old_b diff)

let delegate = "delegate"

and delegator1 = "delegator1"

and delegator2 = "delegator2"

let setup ~activate_ai =
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable true
  --> Scenario_begin.activate_ai (if activate_ai then `Force else `No)
  --> begin_test [delegate]
  --> add_account_with_funds
        delegator1
        ~funder:"__bootstrap__"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> add_account_with_funds
        delegator2
        ~funder:"__bootstrap__"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> next_cycle
  --> snapshot_balances "before delegation" [delegate]
  --> set_delegate delegator1 (Some delegate)
  --> check_snapshot_balances "before delegation"
  --> next_cycle

let test_autostaking =
  Tag "No Ai" --> setup ~activate_ai:false
  (* Delegate will need to freeze 5% * 2k = 100 *)
  --> check_snapshot_balances
        ~f:
          (assert_balance_evolution
             ~loc:__LOC__
             ~for_accounts:[delegate]
             ~part:`staked
             (gt_diff @@ Tez.of_mutez 100_000_000L))
        "before delegation"
  --> snapshot_balances "before second delegation" [delegate]
  --> (Tag "increase delegation"
       --> set_delegate delegator2 (Some delegate)
       --> next_cycle
       (* Delegate will need to freeze 5% * 2k = 100 *)
       --> check_snapshot_balances
             ~f:
               (assert_balance_evolution
                  ~loc:__LOC__
                  ~for_accounts:[delegate]
                  ~part:`staked
                  (gt_diff @@ Tez.of_mutez 100_000_000L))
             "before second delegation"
      |+ Tag "constant delegation"
         --> snapshot_balances "after stake change" [delegate]
         --> wait_n_cycles 6
         --> check_snapshot_balances "after stake change"
      |+ Tag "decrease delegation"
         --> set_delegate delegator1 None
         --> next_cycle
         (* Delegate will need to unfreeze 5% * 2k = 100 *)
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`staked
                    (lt_diff @@ Tez.of_mutez 100_000_000L))
               "before second delegation"
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`unstaked_frozen
                    (gt_diff @@ Tez.of_mutez 100_000_000L))
               "before second delegation"
         --> snapshot_balances "after unstake" [delegate]
         --> next_cycle
         --> check_snapshot_balances "after unstake"
         --> wait_n_cycles_f Test_scenario_stake.(unstake_wait -- 1)
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`unstaked_frozen
                    (lt_diff @@ Tez.of_mutez 100_000_000L))
               "after unstake"
         (* finalizable are auto-finalize immediately  *)
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`liquid
                    (gt_diff @@ Tez.of_mutez 100_000_000L))
               "after unstake")
  |+ Tag "Yes AI" --> setup ~activate_ai:true
     --> check_snapshot_balances "before delegation"

let test_overdelegation =
  (* This test assumes that all delegate accounts created in [begin_test]
     begin with 4M tz, with 5% staked *)
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable true
  --> set S.consensus_committee_size 7000
  --> activate_ai `No
  --> begin_test
        ~force_attest_all:true
        ["delegate"; "faucet1"; "faucet2"; "faucet3"]
  --> add_account_with_funds
        "delegator_to_fund"
        ~funder:"delegate"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
  (* Delegate has 200k = 5% * 4M staked and 200k liquid *)
  --> check_balance_fields
        "delegate"
        ~liquid:(Tez.of_mutez 200_000_000_000L)
        ~staked:(Tez.of_mutez 200_000_000_000L)
        ()
  --> set_delegate "delegator_to_fund" (Some "delegate")
      (* Delegate stake will not change at the end of cycle: same stake *)
  --> next_cycle
  --> check_balance_fields
        "delegate"
        ~liquid:(Tez.of_mutez 200_000_000_000L)
        ~staked:(Tez.of_mutez 200_000_000_000L)
        ()
  --> transfer
        "faucet1"
        "delegator_to_fund"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
      (* Delegate is not overdelegated, but will need to freeze 180k = 5% * 3.6M *)
  --> next_cycle
  --> check_balance_fields
        "delegate"
        ~liquid:(Tez.of_mutez 20_000_000_000L)
        ~staked:(Tez.of_mutez 380_000_000_000L)
        ()
  --> transfer
        "faucet2"
        "delegator_to_fund"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
      (* Delegate is now overdelegated, it will freeze 100% *)
  --> next_cycle
  --> check_balance_fields
        "delegate"
        ~liquid:Tez.zero
        ~staked:(Tez.of_mutez 400_000_000_000L)
        ()
  --> transfer
        "faucet3"
        "delegator_to_fund"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
  (* Delegate is overmegadelegated *)
  --> next_cycle
  --> check_balance_field
        "delegator_to_fund"
        `Liquid
        (Tez.of_mutez 14_400_000_000_000L)
  --> check_balance_fields
        "delegate"
        ~liquid:Tez.zero
        ~staked:(Tez.of_mutez 400_000_000_000L)
        ()
  --> transfer
        "delegator_to_fund"
        "faucet1"
        (Amount (Tez.of_mutez 7_200_000_000_000L))
  (* Delegate is not overdelegated anymore, it will freeze 380k = 5% * 7.2M
     and unstake 20k *)
  --> next_cycle
  --> check_balance_fields
        "delegate"
        ~liquid:Tez.zero
        ~staked:(Tez.of_mutez 380_000_000_000L)
        ~unstaked_frozen_total:(Tez.of_mutez 20_000_000_000L)
        ()
  (* Unfreezing will be done automatically in
     (consensus_rights_delay + max_slashing_period) cycles *)
  --> wait_n_cycles_f Test_scenario_stake.unstake_wait
  --> check_balance_fields
        "delegate"
        ~liquid:(Tez.of_mutez 20_000_000_000L)
        ~staked:(Tez.of_mutez 380_000_000_000L)
        ()

let tests =
  tests_of_scenarios
    [
      ("Test auto-staking", test_autostaking);
      ("Test auto-staking with overdelegation", test_overdelegation);
    ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "autostaking"] tests
