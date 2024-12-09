(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open Scenario_dsl
open Scenario_bake
open Scenario_base
open Log_helpers
open Adaptive_issuance_helpers
open Scenario_constants

(** Returns when the number of bootstrap accounts created by [Context.init_n n] is not equal to [n] *)
type error += Inconsistent_number_of_bootstrap_accounts

type starter_constants = Mainnet | Sandbox | Test

let start ~(constants : starter_constants) : (unit, constants) scenarios =
  let constants, name =
    match constants with
    | Mainnet -> (Default_parameters.constants_mainnet, "mainnet")
    | Sandbox -> (Default_parameters.constants_sandbox, "sandbox")
    | Test -> (Default_parameters.constants_test, "test")
  in
  Action
    (fun () ->
      Log.info ~color:begin_end_color "-- Begin test --" ;
      Log.info "Loading constants_%s." name ;
      Lwt_result_syntax.return constants)

let start_with ~(constants : constants) : (unit, constants) scenarios =
  Action
    (fun () ->
      Log.info ~color:begin_end_color "-- Begin test --" ;
      Log.info "Loading custom constants." ;
      Lwt_result_syntax.return constants)

let start_with_list ~(constants : (string * constants) list) :
    (unit, constants) scenarios =
  match constants with
  | [] ->
      Stdlib.failwith
        (Format.asprintf "%s: Cannot build scenarios from empty list" __LOC__)
  | _ -> fold_tag (fun constants -> start_with ~constants) constants

(** Initializes the constants for testing, with well chosen default values.
    Recommended over [start] or [start_with] *)
let init_constants ?(default = Test) ?(reward_per_block = 0L)
    ?(deactivate_dynamic = false) ?blocks_per_cycle
    ?delegate_parameters_activation_delay () =
  let base_total_issued_per_minute = Tez.of_mutez reward_per_block in
  start ~constants:default
  --> (* default for tests: 12 *)
  set_opt S.blocks_per_cycle blocks_per_cycle
  --> set_opt
        S.delegate_parameters_activation_delay
        delegate_parameters_activation_delay
  --> set
        S.issuance_weights
        {
          base_total_issued_per_minute;
          baking_reward_fixed_portion_weight = 1;
          baking_reward_bonus_weight = 0;
          attesting_reward_weight = 0;
          seed_nonce_revelation_tip_weight = 0;
          vdf_revelation_tip_weight = 0;
          dal_rewards_weight = 0;
        }
  --> set S.liquidity_baking_subsidy Tez.zero
  --> set S.minimal_block_delay Protocol.Alpha_context.Period.one_minute
  --> set S.cost_per_byte Tez.zero
  --> set S.consensus_threshold_size 0
  -->
  if deactivate_dynamic then
    set
      S.Adaptive_issuance.Adaptive_rewards_params.max_bonus
      (Protocol.Issuance_bonus_repr.max_bonus_parameter_of_Q_exn Q.zero)
  else Empty

(** Initialize the test, given some initial parameters *)
let begin_test ?(burn_rewards = false) ?(force_attest_all = false)
    delegates_name_list : (constants, t) scenarios =
  exec (fun (constants : constants) ->
      let open Lwt_result_syntax in
      let bootstrap = "__bootstrap__" in
      let delegates_name_list = bootstrap :: delegates_name_list in
      (* Override threshold value if activate *)
      let n = List.length delegates_name_list in
      let* block, delegates = Context.init_with_constants_n constants n in
      let*? init_level = Context.get_level (B block) in
      let*? account_map =
        List.fold_left2
          ~when_different_lengths:[Inconsistent_number_of_bootstrap_accounts]
          (fun account_map name contract ->
            let liquid = Account.default_initial_spendable_balance in
            let init_staked = Account.default_initial_staked_balance in
            let frozen_deposits = Frozen_tez.init init_staked name name in
            let frozen_rights =
              List.fold_left
                (fun map cycle -> CycleMap.add cycle init_staked map)
                CycleMap.empty
                Cycle.(root ---> add root constants.consensus_rights_delay)
            in
            let pkh = Context.Contract.pkh contract in
            let account =
              init_account
                ~name
                ~delegate:name
                ~pkh
                ~contract
                ~parameters:default_params
                ~liquid
                ~frozen_deposits
                ~frozen_rights
                ~last_seen_activity:
                  Cycle.(add root constants.consensus_rights_delay)
                ()
            in
            let account_map = String.Map.add name account account_map in
            let balance, total_balance =
              balance_and_total_balance_of_account name account_map
            in
            Log.debug "Initial balance for %s:\n%a" name balance_pp balance ;
            Log.debug "Initial total balance: %a" Tez.pp total_balance ;
            account_map)
          String.Map.empty
          delegates_name_list
          delegates
      in
      let* total_supply = Context.get_total_supply (B block) in
      let state =
        State.
          {
            account_map;
            total_supply;
            constants;
            param_requests = [];
            force_ai_vote_yes = true;
            baking_policy = None;
            last_level_rewards = init_level;
            snapshot_balances = String.Map.empty;
            saved_rate = None;
            burn_rewards;
            pending_operations = [];
            pending_slashes = [];
            double_signings = [];
            force_attest_all;
          }
      in
      let* () = check_all_balances block state in
      return (block, state))
