(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:    Adaptive Issuance, Slashing
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_slashing.ml
    Subject:      Test slashing scenarios in the protocol.
*)

open Adaptive_issuance_helpers
open State_account
open Tez_helpers.Ez_tez
open Scenario
open Test_scenario_base

let fs = Format.asprintf

let test_simple_slash =
  let constants = init_constants ~autostaking_enable:false () in
  let any_slash delegate =
    Tag "double baking" --> double_bake delegate
    |+ Tag "double attesting"
       --> double_attest ~other_bakers:("bootstrap2", "bootstrap3") delegate
    |+ Tag "double preattesting"
       --> double_preattest ~other_bakers:("bootstrap2", "bootstrap3") delegate
  in
  begin_test
    ~activate_ai:true
    ~ns_enable_fork:true
    ~constants
    ["delegate"; "bootstrap1"; "bootstrap2"; "bootstrap3"]
  --> (Tag "No AI" --> next_cycle
      |+ Tag "Yes AI" --> next_block --> wait_ai_activation)
  --> any_slash "delegate"
  --> snapshot_balances "before slash" ["delegate"]
  --> ((Tag "denounce same cycle"
        --> make_denunciations ()
            (* delegate can be forbidden in this case, so we set another baker *)
        --> exclude_bakers ["delegate"]
       |+ Tag "denounce next cycle" --> next_cycle --> make_denunciations ()
          (* delegate can be forbidden in this case, so we set another baker *)
          --> exclude_bakers ["delegate"])
       --> (Empty
           |+ Tag "another slash" --> any_slash "bootstrap1"
              --> make_denunciations ()
              (* bootstrap1 can be forbidden in this case, so we set another baker *)
              --> exclude_bakers ["delegate"; "bootstrap1"])
       --> check_snapshot_balances "before slash"
       --> exec_unit (check_pending_slashings ~loc:__LOC__)
       --> next_cycle
       --> assert_failure
             (exec_unit (fun (_block, state) ->
                  if state.State.constants.adaptive_issuance.ns_enable then
                    failwith "ns_enable = true: slash not applied yet"
                  else return_unit)
             --> check_snapshot_balances "before slash")
       --> exec_unit (check_pending_slashings ~loc:__LOC__)
       --> next_cycle
      |+ Tag "denounce too late" --> next_cycle --> next_cycle
         --> assert_failure
               ~expected_error:(fun (_block, state) ->
                 let ds = state.State.double_signings in
                 let ds = match ds with [a] -> a | _ -> assert false in
                 let level =
                   Protocol.Alpha_context.Raw_level.Internal_for_tests.from_repr
                     ds.misbehaviour.level
                 in
                 let last_cycle =
                   Cycle.add
                     (Block.current_cycle_of_level
                        ~blocks_per_cycle:state.State.constants.blocks_per_cycle
                        ~current_level:
                          (Protocol.Raw_level_repr.to_int32
                             ds.misbehaviour.level))
                     Protocol.Constants_repr.max_slashing_period
                 in
                 let (kind : Protocol.Alpha_context.Misbehaviour.kind) =
                   (* This conversion would not be needed if
                      Misbehaviour_repr.kind were moved to a
                      separate file that doesn't have under/over
                      Alpha_context versions. *)
                   match ds.misbehaviour.kind with
                   | Double_baking -> Double_baking
                   | Double_attesting -> Double_attesting
                   | Double_preattesting -> Double_preattesting
                 in
                 [
                   Environment.Ecoproto_error
                     (Protocol.Validate_errors.Anonymous.Outdated_denunciation
                        {kind; level; last_cycle});
                 ])
               (make_denunciations ())
         --> check_snapshot_balances "before slash")

let check_is_forbidden baker = assert_failure (next_block_with_baker baker)

let check_is_not_forbidden baker =
  let open Lwt_result_syntax in
  exec (fun ((block, state) as input) ->
      let baker = State.find_account baker state in
      let*! _ = Block.bake ~policy:(By_account baker.pkh) block in
      return input)

let test_delegate_forbidden =
  let constants =
    init_constants ~blocks_per_cycle:30l ~autostaking_enable:false ()
  in
  begin_test
    ~activate_ai:false
    ~ns_enable_fork:true
    ~constants
    ["delegate"; "bootstrap1"; "bootstrap2"]
  --> set_baker "bootstrap1"
  --> (Tag "Many double bakes"
       --> loop_action 14 (double_bake_ "delegate")
       --> (Tag "14 double bakes are not enough to forbid a delegate"
            (*  7*14 = 98 *)
            --> make_denunciations ()
            --> check_is_not_forbidden "delegate"
           |+ Tag "15 double bakes is one too many"
              (*  7*15 = 105 > 100 *)
              --> double_bake "delegate"
              --> make_denunciations ()
              --> check_is_forbidden "delegate")
      |+ Tag "Is forbidden after first denunciation"
         --> double_attest "delegate"
         --> (Tag "very early first denounce" --> make_denunciations ()
             --> (Tag "in same cycle" --> Empty
                 |+ Tag "next cycle" --> next_cycle)
             --> check_is_forbidden "delegate")
      |+ Tag "Is unforbidden after 7 cycles" --> double_attest "delegate"
         --> make_denunciations ()
         --> exclude_bakers ["delegate"]
         --> check_is_forbidden "delegate"
         --> stake "delegate" Half
         --> check_is_not_forbidden "delegate"
      |+ Tag
           "Two double attestations, in consecutive cycles, denounce out of \
            order" --> double_attest "delegate" --> next_cycle
         --> double_attest "delegate"
         --> make_denunciations
               ~filter:(fun {denounced; misbehaviour = {level; _}; _} ->
                 (not denounced) && Protocol.Raw_level_repr.to_int32 level > 10l)
               ()
         --> make_denunciations
               ~filter:(fun {denounced; misbehaviour = {level; _}; _} ->
                 (not denounced)
                 && Protocol.Raw_level_repr.to_int32 level <= 10l)
               ()
         --> check_is_forbidden "delegate")

let test_slash_unstake =
  let constants = init_constants ~autostaking_enable:false () in
  begin_test
    ~activate_ai:false
    ~ns_enable_fork:true
    ~constants
    ["delegate"; "bootstrap1"; "bootstrap2"]
  --> set_baker "bootstrap1" --> next_cycle --> unstake "delegate" Half
  --> next_cycle --> double_bake "delegate" --> make_denunciations ()
  --> (Empty |+ Tag "unstake twice" --> unstake "delegate" Half)
  --> wait_n_cycles 5
  --> finalize_unstake "delegate"

let test_slash_monotonous_stake =
  let scenario ~offending_op ~op ~early_d =
    let constants =
      init_constants ~blocks_per_cycle:16l ~autostaking_enable:false ()
    in
    begin_test
      ~activate_ai:false
      ~ns_enable_fork:true
      ~constants
      ["delegate"; "bootstrap1"]
    --> next_cycle
    --> loop
          6
          (op "delegate" (Amount (Tez.of_mutez 1_000_000_000L)) --> next_cycle)
    --> offending_op "delegate"
    --> (op "delegate" (Amount (Tez.of_mutez 1_000_000_000L))
        --> loop
              2
              (op "delegate" (Amount (Tez.of_mutez 1_000_000_000L))
              -->
              if early_d then
                make_denunciations ()
                --> exclude_bakers ["delegate"]
                --> next_block
              else offending_op "delegate" --> next_block))
  in
  Tag "slashes with increasing stake"
  --> (Tag "denounce early"
       --> (Tag "Double Bake"
            --> scenario ~offending_op:double_bake ~op:stake ~early_d:true
           |+ Tag "Double attest"
              --> scenario ~offending_op:double_attest ~op:stake ~early_d:true)
      |+ Tag "denounce late"
         --> (Tag "Double Bake"
              --> scenario ~offending_op:double_bake ~op:stake ~early_d:false
             |+ Tag "Double attest"
                --> scenario
                      ~offending_op:double_attest
                      ~op:stake
                      ~early_d:false)
         --> make_denunciations ())
  |+ Tag "slashes with decreasing stake"
     --> (Tag "Double Bake"
          --> scenario ~offending_op:double_bake ~op:unstake ~early_d:true
         |+ Tag "Double attest"
            --> scenario ~offending_op:double_attest ~op:unstake ~early_d:true)
  |+ Tag "denounce late"
     --> (Tag "Double Bake"
          --> scenario ~offending_op:double_bake ~op:unstake ~early_d:false
         |+ Tag "Double attest"
            --> scenario ~offending_op:double_attest ~op:unstake ~early_d:false
         )
     --> make_denunciations ()

let test_slash_timing =
  let constants =
    init_constants ~blocks_per_cycle:8l ~autostaking_enable:false ()
  in
  begin_test ~activate_ai:false ~ns_enable_fork:true ~constants ["delegate"]
  --> next_cycle
  --> (Tag "stake" --> stake "delegate" Half
      |+ Tag "unstake" --> unstake "delegate" Half)
  --> (Tag "with a first slash" --> double_bake "delegate"
       --> make_denunciations ()
      |+ Tag "without another slash" --> Empty)
  --> List.fold_left
        (fun acc i ->
          acc |+ Tag (string_of_int i ^ " cycles lag") --> wait_n_cycles i)
        Empty
        [3; 4; 5; 6]
  --> double_bake "delegate" --> make_denunciations () --> next_cycle

let init_scenario_with_delegators delegate_name faucet_name delegators_list =
  let constants = init_constants ~autostaking_enable:false () in
  let rec init_delegators = function
    | [] -> Empty
    | (delegator, amount) :: t ->
        add_account_with_funds
          delegator
          faucet_name
          (Amount (Tez.of_mutez amount))
        --> set_delegate delegator (Some delegate_name)
        --> init_delegators t
  in
  let init_params =
    {limit_of_staking_over_baking = Q.one; edge_of_baking_over_staking = Q.one}
  in
  begin_test
    ~activate_ai:true
    ~ns_enable_fork:true
    ~constants
    [delegate_name; faucet_name]
  --> set_baker faucet_name
  --> set_delegate_params "delegate" init_params
  --> init_delegators delegators_list
  --> next_block --> wait_ai_activation

let test_many_slashes =
  let rec stake_unstake_for = function
    | [] -> Empty
    | staker :: t ->
        stake staker Half --> unstake staker Half --> stake_unstake_for t
  in
  let slash delegate = double_bake delegate --> make_denunciations () in
  Tag "double bake"
  --> (Tag "solo delegate"
      --> init_scenario_with_delegators
            "delegate"
            "faucet"
            [("delegator", 1_234_567_891L)]
      --> loop
            10
            (stake_unstake_for ["delegate"] --> slash "delegate" --> next_cycle)
      )
(* |+ Tag "delegate with one staker"
      --> init_scenario_with_delegators
            "delegate"
            "faucet"
            [("staker", 1_234_356_891L)]
      --> loop
            10
            (stake_unstake_for ["delegate"; "staker"]
            --> slash "delegate" --> next_cycle)
   |+ Tag "delegate with three stakers"
      --> init_scenario_with_delegators
            "delegate"
            "faucet"
            [
              ("staker1", 1_234_356_891L);
              ("staker2", 1_234_356_890L);
              ("staker3", 1_723_333_111L);
            ]
      --> loop
            10
            (stake_unstake_for
               ["delegate"; "staker1"; "staker2"; "staker3"]
            --> slash "delegate" --> next_cycle))*)

let test_no_shortcut_for_cheaters =
  let constants = init_constants ~autostaking_enable:false () in
  let amount = Amount (Tez.of_mutez 333_000_000_000L) in
  let consensus_rights_delay = constants.consensus_rights_delay in
  begin_test
    ~activate_ai:true
    ~ns_enable_fork:false
    ~constants
    ["delegate"; "bootstrap1"]
  --> next_block --> wait_ai_activation
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> next_cycle --> double_bake "delegate" --> make_denunciations ()
  --> set_baker "bootstrap1" (* exclude_bakers ["delegate"] *)
  --> next_cycle
  --> snapshot_balances "init" ["delegate"]
  --> unstake "delegate" amount
  --> (List.fold_left
         (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
         (Tag "wait 0 cycles" --> Empty)
         (Stdlib.List.init (consensus_rights_delay - 1) (fun i -> i + 1))
       --> stake "delegate" amount
       --> assert_failure (check_snapshot_balances "init")
      |+ Tag "wait enough cycles (consensus_rights_delay + 1)"
         --> wait_n_cycles (consensus_rights_delay + 1)
         --> stake "delegate" amount
         --> check_snapshot_balances "init")

let test_slash_correct_amount_after_stake_from_unstake =
  let constants = init_constants ~autostaking_enable:false () in
  let amount_to_unstake = Amount (Tez.of_mutez 200_000_000_000L) in
  let amount_to_restake = Amount (Tez.of_mutez 100_000_000_000L) in
  let amount_expected_in_unstake_after_slash = Tez.of_mutez 50_000_000_000L in
  let consensus_rights_delay = constants.consensus_rights_delay in
  begin_test
    ~activate_ai:true
    ~ns_enable_fork:false
    ~constants
    ["delegate"; "bootstrap1"]
  --> next_block --> wait_ai_activation
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> next_cycle
  --> unstake "delegate" amount_to_unstake
  --> stake "delegate" amount_to_restake
  --> List.fold_left
        (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
        (Tag "wait 0 cycles" --> Empty)
        (Stdlib.List.init (consensus_rights_delay - 2) (fun i -> i + 1))
  --> double_attest "delegate" --> make_denunciations ()
  --> exclude_bakers ["delegate"]
  --> next_cycle
  --> check_balance_field
        "delegate"
        `Unstaked_frozen_total
        amount_expected_in_unstake_after_slash

(* Test a non-zero request finalizes for a non-zero amount if it hasn't been slashed 100% *)
let test_mini_slash =
  let constants = init_constants ~autostaking_enable:false () in
  (Tag "Yes AI"
   --> begin_test
         ~activate_ai:true
         ~ns_enable_fork:false
         ~constants
         ["delegate"; "baker"]
   --> next_block --> wait_ai_activation
  |+ Tag "No AI"
     --> begin_test
           ~activate_ai:false
           ~ns_enable_fork:false
           ~constants
           ["delegate"; "baker"])
  --> unstake "delegate" (Amount Tez.one_mutez)
  --> set_baker "baker" --> next_cycle
  --> (Tag "5% slash" --> double_bake "delegate" --> make_denunciations ()
      |+ Tag "95% slash" --> next_cycle --> double_attest "delegate"
         --> loop 9 (double_bake "delegate")
         --> make_denunciations ())
  (* Wait two cycles because of ns_enable *)
  --> next_cycle
  --> next_cycle
  --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero
  --> wait_n_cycles (constants.consensus_rights_delay + 1)

let test_slash_rounding =
  let constants = init_constants ~autostaking_enable:false () in
  begin_test
    ~activate_ai:true
    ~ns_enable_fork:true
    ~constants
    ["delegate"; "baker"]
  --> set_baker "baker" --> next_block --> wait_ai_activation
  --> unstake "delegate" (Amount (Tez.of_mutez 2L))
  --> next_cycle --> double_bake "delegate" --> double_bake "delegate"
  --> make_denunciations () --> wait_n_cycles 7
  --> finalize_unstake "delegate"

(* TODO #6645: reactivate tests *)
let tests =
  tests_of_scenarios
  @@ [
       ("Test simple slashing", test_simple_slash);
       ("Test slashed is forbidden", test_delegate_forbidden);
       ("Test slash with unstake", test_slash_unstake);
       (* TODO: make sure this test passes with blocks_per_cycle:8l
          https://gitlab.com/tezos/tezos/-/issues/6904 *)
       ("Test slashes with simple varying stake", test_slash_monotonous_stake);
       (* This test has been deactivated following the changes of the
          forbidding mechanism that now forbids delegates right after the
          first denunciation, it should be fixed and reactivated
          https://gitlab.com/tezos/tezos/-/issues/6904 *)
       (* ( "Test multiple slashes with multiple stakes/unstakes", *)
       (*   test_many_slashes ); *)
       (* ("Test slash timing", test_slash_timing); *)
       ( "Test stake from unstake deactivated when slashed",
         test_no_shortcut_for_cheaters );
       ( "Test stake from unstake reduce initial amount",
         test_slash_correct_amount_after_stake_from_unstake );
       ("Test unstake 1 mutez then slash", test_mini_slash);
       ("Test slash rounding", test_slash_rounding);
     ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "slashing"] tests
