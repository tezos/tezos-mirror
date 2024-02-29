(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Adaptive Issuance, Autostaking
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
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
      failwith "%s Unexpected stake evolution for %s" loc name)
  else raise Not_found

let delegate = "delegate"

and delegator1 = "delegator1"

and delegator2 = "delegator2"

let setup ~activate_ai =
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable true
  --> Scenario_begin.activate_ai activate_ai
  --> begin_test [delegate]
  --> add_account_with_funds
        delegator1
        "__bootstrap__"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> add_account_with_funds
        delegator2
        "__bootstrap__"
        (Amount (Tez.of_mutez 2_000_000_000L))
  --> next_cycle
  --> (if activate_ai then wait_ai_activation else next_cycle)
  --> snapshot_balances "before delegation" [delegate]
  --> set_delegate delegator1 (Some delegate)
  --> check_snapshot_balances "before delegation"
  --> next_cycle

let test_autostaking =
  Tag "No Ai" --> setup ~activate_ai:false
  --> check_snapshot_balances
        ~f:
          (assert_balance_evolution
             ~loc:__LOC__
             ~for_accounts:[delegate]
             ~part:`staked
             Q.gt)
        "before delegation"
  --> snapshot_balances "before second delegation" [delegate]
  --> (Tag "increase delegation"
       --> set_delegate delegator2 (Some delegate)
       --> next_cycle
       --> check_snapshot_balances
             ~f:
               (assert_balance_evolution
                  ~loc:__LOC__
                  ~for_accounts:[delegate]
                  ~part:`staked
                  Q.gt)
             "before second delegation"
      |+ Tag "constant delegation"
         --> snapshot_balances "after stake change" [delegate]
         --> wait_n_cycles 8
         --> check_snapshot_balances "after stake change"
      |+ Tag "decrease delegation"
         --> set_delegate delegator1 None
         --> next_cycle
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`staked
                    Q.lt)
               "before second delegation"
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`unstaked_frozen
                    Q.gt)
               "before second delegation"
         --> snapshot_balances "after unstake" [delegate]
         --> next_cycle
         --> check_snapshot_balances "after unstake"
         --> wait_n_cycles 4
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`unstaked_frozen
                    Q.lt)
               "after unstake"
         (* finalizable are auto-finalize immediately  *)
         --> check_snapshot_balances
               ~f:
                 (assert_balance_evolution
                    ~loc:__LOC__
                    ~for_accounts:[delegate]
                    ~part:`liquid
                    Q.lt)
               "before finalisation")
  |+ Tag "Yes AI" --> setup ~activate_ai:true
     --> check_snapshot_balances "before delegation"

let test_overdelegation =
  (* This test assumes that all delegate accounts created in [begin_test]
     begin with 4M tz, with 5% staked *)
  init_constants ()
  --> set S.Adaptive_issuance.autostaking_enable true
  --> activate_ai false
  --> begin_test ["delegate"; "faucet1"; "faucet2"; "faucet3"]
  --> add_account_with_funds
        "delegator_to_fund"
        "delegate"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
  (* Delegate has 200k staked and 200k liquid *)
  --> set_delegate "delegator_to_fund" (Some "delegate")
  (* Delegate stake will not change at the end of cycle: same stake *)
  --> next_cycle
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 200_000_000_000L)
  --> transfer
        "faucet1"
        "delegator_to_fund"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
  (* Delegate is not overdelegated, but will need to freeze 180k *)
  --> next_cycle
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 380_000_000_000L)
  --> transfer
        "faucet2"
        "delegator_to_fund"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
  (* Delegate is now overdelegated, it will freeze 100% *)
  --> next_cycle
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 400_000_000_000L)
  --> transfer
        "faucet3"
        "delegator_to_fund"
        (Amount (Tez.of_mutez 3_600_000_000_000L))
  (* Delegate is overmegadelegated *)
  --> next_cycle
  --> check_balance_field "delegate" `Staked (Tez.of_mutez 400_000_000_000L)

let tests =
  tests_of_scenarios
    [
      ("Test auto-staking", test_autostaking);
      ("Test auto-staking with overdelegation", test_overdelegation);
    ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "autostaking"] tests
