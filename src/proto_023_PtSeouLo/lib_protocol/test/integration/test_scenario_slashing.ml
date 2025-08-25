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
    Invocation:   dune exec src/proto_023_PtSeouLo/lib_protocol/test/integration/main.exe \
                   -- --file test_scenario_slashing.ml
    Subject:      Test slashing scenarios in the protocol.
*)

open State_account
open Tez_helpers.Ez_tez
open Scenario
open Scenario_constants

let fs = Format.asprintf

(** Test multiple misbehaviors
    - Test a single delegate misbehaving multiple times
    - Test multiple delegates misbehaving
    - Test multiple delegates misbehaving multiple times
    - Test denunciation at once or in a staggered way
    - Test denunciation in chronological order or reverse order
    - Spread misbehaviors/denunciations over multiple cycles
    *)
let test_multiple_misbehaviors =
  (* Denounce all misbehaviours or 14 one by one in chronological or reverse
     order *)
  let make_denunciations () =
    Tag "denounce chronologically"
    --> log "denounce chronologically"
    --> (Tag "all at once" --> make_denunciations ~rev:false ()
        |+ Tag "one by one"
           --> loop
                 12
                 (make_denunciations
                    ~filter:(fun {denounced; _} -> not denounced)
                    ~single:true
                    ~rev:false
                    ()))
    |+ Tag "denounce reverse" --> log "denounce reverse"
       --> (Tag "all at once" --> make_denunciations ~rev:true ()
           |+ Tag "one by one"
              --> loop
                    12
                    (make_denunciations
                       ~filter:(fun {denounced; _} -> not denounced)
                       ~single:true
                       ~rev:true
                       ()))
  in
  (* Misbehaviors scenarios *)
  let misbehave i delegate1 delegate2 =
    (Tag "single delegate"
     (* A single delegate misbehaves several times before being denunced *)
     --> loop
           i
           (double_attest delegate1 --> double_preattest delegate1
          --> double_bake delegate1 --> double_attest delegate1
          --> double_preattest delegate1)
     --> exclude_bakers [delegate1]
    |+ Tag "multiple delegates"
       (* Two delegates double bake sequentially *)
       --> loop
             i
             (loop
                3
                (double_bake delegate1 --> double_bake delegate2 --> next_block))
       --> exclude_bakers [delegate1; delegate2]
    |+ Tag "double misbehaviors"
       (* Two delegates misbehave in parallel for multiple levels *)
       --> loop
             i
             (double_attest_many [delegate1; delegate2]
             --> double_attest_many [delegate1; delegate2]
             --> double_preattest_many [delegate1; delegate2]
             --> double_bake_many [delegate1; delegate2])
       --> exclude_bakers [delegate1; delegate2])
    --> make_denunciations ()
  in
  init_constants ~blocks_per_cycle:24l ~reward_per_block:0L ()
  --> begin_test ["delegate"; "bootstrap1"; "bootstrap2"; "bootstrap3"]
  --> next_cycle
  -->
  (* various make misbehaviors spread over 1 or two cycles *)
  List.fold_left
    (fun acc i ->
      acc
      |+ Tag (string_of_int i ^ " misbehavior loops")
         --> misbehave i "delegate" "bootstrap1"
         --> next_cycle)
    Empty
    [1; 3]

let wait_for_slashing =
  wait_n_cycles (Protocol.Constants_repr.slashing_delay + 1)

let check_is_forbidden ~loc baker =
  assert_failure
    ~expected_error:(fun (_, state) errs ->
      let baker = State.find_account baker state in
      Error_helpers.expect_forbidden_delegate ~loc ~delegate:baker.contract errs)
    (next_block_with_baker baker)

let check_is_not_forbidden baker =
  let open Lwt_result_syntax in
  exec (fun ((block, state) as input) ->
      let baker = State.find_account baker state in
      let* _ = Block.bake ~policy:(By_account baker.pkh) block in
      return input)

let check_has_no_slots ~loc baker_name =
  let open Lwt_result_syntax in
  exec (fun ((block, state) as input) ->
      let baker = State.find_account baker_name state in
      let* rights =
        Plugin.RPC.Attestation_rights.get
          Block.rpc_ctxt
          ~delegates:[baker.pkh]
          block
      in
      match rights with
      | [] -> return input
      | [{level = _; delegates_rights; estimated_time = _}] -> (
          match delegates_rights with
          | [{delegate; consensus_key = _; first_slot = _; attestation_power}]
            ->
              Test.fail
                ~__LOC__
                "%s: delegate '%s'(%a) expected to have no attestation power, \
                 has %i slots."
                loc
                baker_name
                Signature.Public_key_hash.pp
                delegate
                attestation_power
          | _ ->
              (* Cannot happen: RPC called for only one delegate *) assert false
          )
      | _ ->
          (* Cannot happen: only one level is prompted for the RPC *)
          assert false)

let check_has_slots ~loc baker_name =
  let open Lwt_result_syntax in
  exec (fun ((block, state) as input) ->
      let baker = State.find_account baker_name state in
      let* rights =
        Plugin.RPC.Attestation_rights.get
          Block.rpc_ctxt
          ~delegates:[baker.pkh]
          block
      in
      match rights with
      | [] ->
          Test.fail
            ~__LOC__
            "%s: delegate '%s'(%a) expected to have some attestation power, \
             has none."
            loc
            baker_name
            Signature.Public_key_hash.pp
            baker.pkh
      | _ -> return input)

(** Tests forbidding delegates ensuring:
  - delegates are not forbidden until a denunciation is made (allowing for
    multiple misbehaviours)
  - a single misbehaviour is enough to be denunced and forbidden
  - delegates are unforbidden after a certain amount of time
  - delegates are not forbidden if denounced for an outdated misbehaviour
*)
let test_delegate_forbidden =
  let crd (_, state) = state.State.constants.consensus_rights_delay in
  init_constants ~blocks_per_cycle:32l ()
  --> (Tag "non tz4" --> begin_test ["delegate"; "bootstrap1"; "bootstrap2"]
      |+ Tag "tz4"
         --> begin_test ~algo:Bls ["delegate"; "bootstrap1"; "bootstrap2"])
  --> set_baker "bootstrap1"
  --> (Tag "Is not forbidden until first denunciation"
       --> loop 14 (double_bake "delegate")
       --> exclude_bakers ["delegate"]
       -->
       (* ensure delegate is not forbidden until the denunciations are done *)
       check_is_not_forbidden "delegate"
       --> make_denunciations ()
       -->
       (* delegate is forbidden directly after the first denunciation *)
       check_is_forbidden ~loc:__LOC__ "delegate"
      |+ Tag "Is forbidden after single misbehavior"
         --> double_attest "delegate"
         --> (Tag "very early first denounce"
              --> exclude_bakers ["delegate"]
              --> make_denunciations ()
             |+ Tag "in next cycle" --> next_cycle
                --> exclude_bakers ["delegate"]
                --> make_denunciations ())
         --> check_is_forbidden ~loc:__LOC__ "delegate"
      |+ Tag "Is unforbidden after CONSENSUS_RIGHTS_DELAY after slash cycles"
         --> double_attest "delegate"
         --> exclude_bakers ["delegate"]
         --> make_denunciations ()
         --> check_is_forbidden ~loc:__LOC__ "delegate"
         --> next_cycle (* slash occured *) --> stake "delegate" Half
         --> wait_n_cycles_f crd
         --> check_is_not_forbidden "delegate"
      |+ Tag "Is not forbidden after a denunciation is outdated"
         --> double_attest "delegate" --> wait_for_slashing
         --> assert_failure
               ~expected_error:(fun (_block, state) errs ->
                 Error_helpers.expect_outdated_denunciation_state
                   ~loc:__LOC__
                   ~state
                   errs)
               (make_denunciations ())
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
         --> check_is_forbidden ~loc:__LOC__ "delegate")

let test_slash_unstake =
  init_constants ()
  --> begin_test ["delegate"; "bootstrap1"; "bootstrap2"]
  --> set_baker "bootstrap1" --> next_cycle --> unstake "delegate" Half
  --> next_cycle --> double_bake "delegate" --> make_denunciations ()
  --> (Empty |+ Tag "unstake twice" --> unstake "delegate" Half)
  --> wait_n_cycles 5
  --> finalize_unstake "delegate"

let test_slash_monotonous_stake =
  let scenario ~offending_op ~op ~early_d =
    init_constants ~blocks_per_cycle:16l ()
    --> begin_test ["delegate"; "bootstrap1"]
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
              --> scenario
                    ~offending_op:(fun s -> double_attest s)
                    ~op:stake
                    ~early_d:true)
      |+ Tag "denounce late"
         --> (Tag "Double Bake"
              --> scenario ~offending_op:double_bake ~op:stake ~early_d:false
             |+ Tag "Double attest"
                --> scenario
                      ~offending_op:(fun s -> double_attest s)
                      ~op:stake
                      ~early_d:false)
         --> make_denunciations ())
  |+ Tag "slashes with decreasing stake"
     --> (Tag "Double Bake"
          --> scenario ~offending_op:double_bake ~op:unstake ~early_d:true
         |+ Tag "Double attest"
            --> scenario
                  ~offending_op:(fun s -> double_attest s)
                  ~op:unstake
                  ~early_d:true)
  |+ Tag "denounce late"
     --> (Tag "Double Bake"
          --> scenario ~offending_op:double_bake ~op:unstake ~early_d:false
         |+ Tag "Double attest"
            --> scenario
                  ~offending_op:(fun s -> double_attest s)
                  ~op:unstake
                  ~early_d:false)
     --> make_denunciations ()

let test_slash_timing =
  init_constants ~blocks_per_cycle:8l ()
  --> begin_test ~force_attest_all:true ["delegate"; "bootstrap1"]
  --> next_cycle
  --> (Tag "stake" --> stake "delegate" Half
      |+ Tag "unstake" --> unstake "delegate" Half)
  --> (Tag "with a first slash" --> double_bake "delegate"
       --> exclude_bakers ["delegate"]
       --> make_denunciations ()
      |+ Tag "without another slash" --> Empty)
  --> stake "delegate" Half
  --> List.fold_left
        (fun acc i ->
          acc |+ Tag (string_of_int i ^ " cycles lag") --> wait_n_cycles i)
        wait_for_slashing
        [3; 4; 5; 6]
  --> double_bake "delegate"
  --> exclude_bakers ["delegate"]
  --> make_denunciations () --> next_cycle

let test_no_shortcut_for_cheaters =
  let amount = Amount (Tez.of_mutez 333_000_000_000L) in
  let consensus_rights_delay =
    Default_parameters.constants_test.consensus_rights_delay
  in
  init_constants ()
  --> begin_test ["delegate"; "bootstrap1"]
  --> stake "delegate" (Amount (Tez.of_mutez 1_800_000_000_000L))
  --> next_cycle --> double_bake "delegate" --> make_denunciations ()
  --> set_baker "bootstrap1" (* exclude_bakers ["delegate"] *)
  --> wait_for_slashing
  --> snapshot_balances "init" ["delegate"]
  --> unstake "delegate" amount
  --> (List.fold_left
         (fun acc i -> acc |+ Tag (fs "wait %i cycles" i) --> wait_n_cycles i)
         (Tag "wait 0 cycles" --> Empty)
         (Stdlib.List.init (consensus_rights_delay - 1) (fun i -> i + 1))
       --> stake "delegate" amount
       --> assert_failure_in_check_snapshot_balances ~loc:__LOC__ "init"
      |+ Tag "wait enough cycles (consensus_rights_delay + 1)"
         --> wait_n_cycles (consensus_rights_delay + 1)
         --> stake "delegate" amount
         --> check_snapshot_balances "init")

let test_slash_correct_amount_after_stake_from_unstake =
  let tez_to_unstake = Tez.of_int 150_000 in
  let tez_to_restake = Tez.of_int 120_000 in
  assert (
    Tez.(
      tez_to_restake < tez_to_unstake
      && tez_to_unstake < Account.default_initial_staked_balance)) ;
  let unstaked_before_slash = Tez.(tez_to_unstake -! tez_to_restake) in
  let staked_before_slash =
    Tez.(Account.default_initial_staked_balance -! unstaked_before_slash)
  in
  let slashed_percentage =
    Default_parameters.constants_test
      .percentage_of_frozen_deposits_slashed_per_double_baking
  in
  let expected_unstaked_after_slash =
    Tez.(
      unstaked_before_slash
      -! mul_percentage ~rounding:`Up unstaked_before_slash slashed_percentage)
  in
  let expected_staked_after_slash ~staked_when_computing_misbehaviour_rights =
    Tez.(
      staked_before_slash
      -! mul_percentage
           ~rounding:`Up
           staked_when_computing_misbehaviour_rights
           slashed_percentage)
  in
  let consensus_rights_delay =
    Default_parameters.constants_test.consensus_rights_delay
  in
  let check_balances ~loc ~staked ~unstaked_frozen ~unstaked_finalizable =
    check_balance_fields
      ~loc
      "delegate"
      ~liquid:Account.default_initial_spendable_balance
      ~staked
      ~unstaked_frozen_total:unstaked_frozen
      ~unstaked_finalizable
      ()
  in
  init_constants ()
  --> begin_test ["delegate"; "bootstrap1"]
  --> next_cycle
  --> unstake "delegate" (Amount tez_to_unstake)
  --> stake "delegate" (Amount tez_to_restake)
  --> list_map_branched
        Protocol.Misc.(0 --> (consensus_rights_delay + 1))
        (fun cycles_to_wait ->
          Tag (sf "wait %i cycles" cycles_to_wait)
          --> wait_n_cycles cycles_to_wait
          --> check_balances
                ~loc:__LOC__
                ~staked:staked_before_slash
                ~unstaked_frozen:unstaked_before_slash
                ~unstaked_finalizable:Tez.zero
          --> exclude_bakers ["delegate"]
          --> double_bake "delegate" --> make_denunciations ()
          --> wait_for_slashing
          -->
          if cycles_to_wait > consensus_rights_delay then
            (* The misbehaviour happens at least two full cycles after
               the unstake operation, meaning that
               1) the unstake request is too old to be slashed and
               2) the staked balance at the time rights for the
                  misbehaviour block was computed was
                  [staked_before_slash]. *)
            let staked =
              expected_staked_after_slash
                ~staked_when_computing_misbehaviour_rights:staked_before_slash
            in
            check_balances
              ~loc:__LOC__
              ~staked
              ~unstaked_frozen:Tez.zero
              ~unstaked_finalizable:unstaked_before_slash
          else
            (* 1) The unstake request is slashed and
               2) the staked balance when computing the relevant
                  rights was the initial staked balance. *)
            let staked =
              expected_staked_after_slash
                ~staked_when_computing_misbehaviour_rights:
                  Account.default_initial_staked_balance
            in
            let unstaked_frozen, unstaked_finalizable =
              if cycles_to_wait >= consensus_rights_delay then
                (* The unstake request is finalizable. *)
                (Tez.zero, expected_unstaked_after_slash)
              else (expected_unstaked_after_slash, Tez.zero)
            in
            check_balances
              ~loc:__LOC__
              ~staked
              ~unstaked_frozen
              ~unstaked_finalizable)

(* Test a non-zero request finalizes for a non-zero amount if it hasn't been slashed 100% *)
let test_mini_slash =
  init_constants ()
  --> begin_test ["delegate"; "baker"]
  --> unstake "delegate" (Amount Tez.one_mutez)
  --> set_baker "baker" --> next_cycle
  --> (Tag "5% slash" --> double_bake "delegate" --> make_denunciations ()
      |+ Tag "95% slash" --> next_cycle --> double_attest "delegate"
         --> loop 9 (double_bake "delegate")
         --> make_denunciations ())
  (* Wait two cycles for the slashing to be applied *)
  --> wait_n_cycles 2
  --> check_balance_field "delegate" `Unstaked_frozen_total Tez.zero
  --> wait_n_cycles_f (fun (_, state) ->
          state.constants.consensus_rights_delay + 1)

let test_slash_rounding =
  init_constants ()
  --> begin_test ["delegate"; "baker"]
  --> set_baker "baker"
  --> unstake "delegate" (Amount (Tez.of_mutez 2L))
  --> next_cycle --> double_bake "delegate" --> double_bake "delegate"
  --> make_denunciations () --> wait_n_cycles 7
  --> finalize_unstake "delegate"

let test_mega_slash =
  init_constants ()
  (* Setting constants and flags so that
     - Adaptive Issuance is activated
     - Adaptive Slashing is activated
     - Every double attestation slashes 99% of the stake
     - We have 8 blocks per cycle (faster tests) *)
  --> set
        S.max_slashing_per_block
        (Protocol.Percentage.of_q_bounded ~round:`Down Q.(99 // 100))
  --> set S.max_slashing_threshold {numerator = 0; denominator = 1}
  --> set S.blocks_per_cycle 8l
  -->
  let delegates = ["delegate"; "baker"; "faucet"; "big_spender"] in
  begin_test delegates ~force_attest_all:true
  (* Unstake for all delegates, to have roughly equal rights *)
  --> List.fold_left
        (fun acc name ->
          acc --> unstake name (Amount (Tez.of_mutez 190_000_000_000L)))
        Empty
        delegates
  --> set_baker "baker"
  (* Activate staking for delegate *)
  --> set_delegate_params
        "delegate"
        {
          limit_of_staking_over_baking = Q.one;
          edge_of_baking_over_staking = Q.zero;
        }
  (* Will be useful later... *)
  --> set_delegate_params
        "baker"
        {
          limit_of_staking_over_baking = Q.one;
          edge_of_baking_over_staking = Q.zero;
        }
  --> wait_delegate_parameters_activation
  (* Many (small) stakers for the delegate *)
  -->
  let init_costaking_amount = 500_000_000L in
  let init_funds = Amount (Tez.of_mutez (Int64.mul 2L init_costaking_amount)) in
  let stakers_list = ["staker1"; "staker2"; "staker3"; "staker4"] in
  List.fold_left
    (fun acc name ->
      acc
      --> add_account_with_funds name ~funder:"faucet" init_funds
      --> set_delegate name (Some "delegate")
      --> stake name (Amount (Tez.of_mutez init_costaking_amount)))
    Empty
    stakers_list
  -->
  (* 1-mutez staker *)
  let tiny_staker = "tiny_staker" in
  add_account_with_funds tiny_staker ~funder:"faucet" init_funds
  --> set_delegate tiny_staker (Some "delegate")
  --> stake tiny_staker (Amount Tez.one_mutez)
  (* One big delegator, it will stake for the following amount later *)
  -->
  let big_staking_amount = 3_000_000_000_000L in
  add_account_with_funds
    "big_staker"
    ~funder:"big_spender"
    (Amount (Tez.of_mutez (Int64.succ big_staking_amount)))
  --> set_delegate "big_staker" (Some "delegate")
  --> next_cycle
  -->
  (* The "incident" *)
  let incident =
    double_attest "delegate" --> make_denunciations () --> wait_for_slashing
    (* We check stakers can still unstake and change delegates *)
    --> assert_success (unstake "staker1" Half)
    --> assert_success (unstake "staker1" All)
    --> assert_success
          (unstake "staker1" (Amount (Tez.of_mutez init_costaking_amount)))
    --> assert_success (unstake "staker1" Max_tez)
    --> assert_success
          (set_delegate "staker1" (Some "baker")
          --> assert_failure
                ~expected_error:(fun _ errs ->
                  Error_helpers.check_error_constructor_name
                    ~loc:__LOC__
                    ~expected:
                      Protocol.Staking
                      .Cannot_stake_with_unfinalizable_unstake_requests_to_another_delegate
                    errs)
                (stake "staker1" (Amount (Tez.of_mutez init_costaking_amount)))
          --> wait_n_cycles_f Test_scenario_stake.unstake_wait
          --> stake "staker1" (Amount (Tez.of_mutez init_costaking_amount))
          --> set_delegate "staker1" (Some "delegate"))
    --> assert_success (unstake tiny_staker Half)
    --> assert_success (unstake tiny_staker All)
    --> assert_success (unstake tiny_staker (Amount Tez.one_mutez))
    --> assert_success (unstake tiny_staker Max_tez)
    --> assert_success
          (set_delegate tiny_staker (Some "baker")
           (* [tiny_staker] can immediately stake for its new delegate
              because it had zero staked tez left before changing
              delegates *)
          --> stake tiny_staker (Amount (Tez.of_mutez init_costaking_amount))
          --> set_delegate tiny_staker (Some "delegate"))
    (* We check staking is still possible, with a risk of overflow *)
    --> assert_success
          (stake "big_staker" (Amount (Tez.of_mutez big_staking_amount)))
    (* We wait for the delegate to have rights again *)
    --> stake "delegate" (Amount (Tez.of_mutez 10_000_000_000L))
    (* In consensus_rights_delay + 2 cycles, the delegate has 0 rights
       because of the slash that happens before rights computation, which
       puts it under the minimal frozen threshold required to bake. *)
    --> wait_n_cycles_f (fun (_, state) ->
            state.State.constants.consensus_rights_delay + 2)
    --> check_has_no_slots ~loc:__LOC__ "delegate"
    --> next_cycle
    --> check_has_slots ~loc:__LOC__ "delegate"
    --> check_is_not_forbidden "delegate"
  in
  (* Since Int64.max_int / 3Mtz ~= 3_074_457, and that each slash multiplies by 100 the
     value of the pseudotokens, then 3 slashings won't trigger an overflow... *)
  loop 3 incident
  (*  but 4 will. *)
  --> double_attest "delegate"
  --> make_denunciations () --> wait_for_slashing
  (* We check stakers can still unstake and finalize, despite everything *)
  --> assert_success
        (unstake "staker1" Half
        --> wait_n_cycles_f Test_scenario_stake.unstake_wait
        --> finalize_unstake "staker1")
  (* We check the baker itself can still unstake,... *)
  --> assert_success
        (unstake "delegate" Half
        --> wait_n_cycles_f Test_scenario_stake.unstake_wait
        --> finalize_unstake "delegate")
  (*  ... stake,... *)
  --> assert_success (stake "delegate" Half)
  (* ... change delegate parameters. *)
  --> assert_success
        (set_delegate_params
           "delegate"
           {
             limit_of_staking_over_baking = Q.zero;
             edge_of_baking_over_staking = Q.one;
           }
        --> wait_delegate_parameters_activation
        (* Although it should overflow, the limit=0 check is done first *)
        --> assert_failure
              ~expected_error:(fun _ errs ->
                Error_helpers.check_error_constructor_name
                  ~loc:__LOC__
                  ~expected:
                    Protocol.Apply
                    .Staking_to_delegate_that_refuses_external_staking
                  errs)
              (stake "big_staker" (Amount (Tez.of_mutez big_staking_amount))))
  (* We check stakers can still change delegate, and stake thereafter *)
  --> assert_success
        (set_delegate "staker1" (Some "baker")
        --> wait_n_cycles_f Test_scenario_stake.unstake_wait
        --> finalize_unstake "staker1" --> stake "staker1" Half)
  (* We check for overflow *)
  --> assert_failure
        ~expected_error:(fun _ errs ->
          let init_full_costaking =
            Int64.(succ (mul 4L init_costaking_amount))
            |> Protocol.Tez_repr.of_mutez_exn
          in
          let check_error = function
            | [
                Protocol.Tez_repr.Multiplication_overflow
                  (old_costaking, new_staking);
              ] ->
                Protocol.Tez_repr.equal init_full_costaking old_costaking
                && Z.equal new_staking (Z.of_int64 big_staking_amount)
            | _ -> false
          in
          Assert.expect_error ~loc:__LOC__ errs check_error)
        (stake "big_staker" (Amount (Tez.of_mutez big_staking_amount)))
  (* Check that if everyone unstakes from delegate, then it's back to normal *)
  --> List.fold_left
        (fun acc name -> acc --> unstake name All)
        Empty
        stakers_list
  --> assert_success
        (stake "big_staker" (Amount (Tez.of_mutez big_staking_amount)))
  (* Check that funds can be finalized, just in case *)
  --> wait_n_cycles_f Test_scenario_stake.unstake_wait
  --> List.fold_left
        (fun acc name -> acc --> finalize_unstake name)
        Empty
        stakers_list
  (* One last time, for good measure *)
  --> assert_success
        (stake "big_staker" (Amount (Tez.of_mutez big_staking_amount)))

(* TODO #6645: reactivate tests *)
let tests =
  tests_of_scenarios
  @@ [
       ("Test multiple misbehaviors", test_multiple_misbehaviors);
       ("Test slashed is forbidden", test_delegate_forbidden);
       ("Test slash with unstake", test_slash_unstake);
       (* TODO: make sure this test passes with blocks_per_cycle:8l
          https://gitlab.com/tezos/tezos/-/issues/6904 *)
       ("Test slashes with simple varying stake", test_slash_monotonous_stake);
       ("Test slash timing", test_slash_timing);
       ( "Test stake from unstake deactivated when slashed",
         test_no_shortcut_for_cheaters );
       ( "Test stake from unstake reduce initial amount",
         test_slash_correct_amount_after_stake_from_unstake );
       ("Test unstake 1 mutez then slash", test_mini_slash);
       ("Test slash rounding", test_slash_rounding);
       ("Test slash 99.999999%", test_mega_slash);
     ]

let () =
  register_tests ~__FILE__ ~tags:["protocol"; "scenario"; "slashing"] tests
