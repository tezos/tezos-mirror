(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open Scenario_dsl
open Scenario_base
open Log_helpers
open Adaptive_issuance_helpers

(** Returns when the number of bootstrap accounts created by [Context.init_n n] is not equal to [n] *)
type error += Inconsistent_number_of_bootstrap_accounts

(** Initialize the test, given some initial parameters *)
let begin_test ~activate_ai ?(burn_rewards = false) ?(ns_enable_fork = false)
    ?(constants : Protocol.Alpha_context.Constants.Parametric.t option)
    ?(constants_list :
       (string * Protocol.Alpha_context.Constants.Parametric.t) list option)
    delegates_name_list : (unit, t) scenarios =
  let f ns_enable =
    (match (constants, constants_list) with
    | None, None -> Stdlib.failwith "No constants provided to begin_test"
    | Some _, Some _ ->
        Stdlib.failwith
          "You cannot provide ~constants and ~constants_list to begin_test"
    | None, Some constants_list -> list_to_branch constants_list
    | Some constants, None -> Action (fun () -> return constants))
    --> exec (fun (constants : Protocol.Alpha_context.Constants.Parametric.t) ->
            let open Lwt_result_syntax in
            Log.info ~color:begin_end_color "-- Begin test --" ;
            let bootstrap = "__bootstrap__" in
            let delegates_name_list = bootstrap :: delegates_name_list in
            (* Override threshold value if activate *)
            let constants =
              if activate_ai then (
                Log.info ~color:event_color "Setting ai threshold to 0" ;
                {
                  constants with
                  adaptive_issuance =
                    {
                      constants.adaptive_issuance with
                      launch_ema_threshold = 0l;
                      activation_vote_enable = true;
                      ns_enable;
                    };
                })
              else constants
            in
            let n = List.length delegates_name_list in
            let* block, delegates = Context.init_with_constants_n constants n in
            let*? init_level = Context.get_level (B block) in
            let init_staked = Tez.of_mutez 200_000_000_000L in
            let*? account_map =
              List.fold_left2
                ~when_different_lengths:
                  [Inconsistent_number_of_bootstrap_accounts]
                (fun account_map name contract ->
                  let liquid =
                    Tez.(Account.default_initial_balance -! init_staked)
                  in
                  let frozen_deposits = Frozen_tez.init init_staked name name in
                  let frozen_rights =
                    List.fold_left
                      (fun map cycle -> CycleMap.add cycle init_staked map)
                      CycleMap.empty
                      Cycle.(
                        root ---> add root constants.consensus_rights_delay)
                  in
                  let pkh = Context.Contract.pkh contract in
                  let account =
                    init_account
                      ~delegate:name
                      ~pkh
                      ~contract
                      ~parameters:default_params
                      ~liquid
                      ~frozen_deposits
                      ~frozen_rights
                      ()
                  in
                  let account_map = String.Map.add name account account_map in
                  let balance, total_balance =
                    balance_and_total_balance_of_account name account_map
                  in
                  Log.debug
                    "Initial balance for %s:\n%a"
                    name
                    balance_pp
                    balance ;
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
                  activate_ai;
                  baking_policy = None;
                  last_level_rewards = init_level;
                  snapshot_balances = String.Map.empty;
                  saved_rate = None;
                  burn_rewards;
                  pending_operations = [];
                  pending_slashes = [];
                  double_signings = [];
                }
            in
            let* () = check_all_balances block state in
            return (block, state))
  in
  if ns_enable_fork then
    Tag "ns_enable = true" --> f true |+ Tag "ns_enable = false" --> f false
  else f false
