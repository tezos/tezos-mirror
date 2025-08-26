(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open State

let tolerated_inactivity_period ~block ~state account =
  let open Lwt_result_syntax in
  let tolerance_threshold =
    state.State.constants.tolerated_inactivity_period_threshold
  in
  let tolerance_low = state.State.constants.tolerated_inactivity_period_low in
  let tolerance_high = state.State.constants.tolerated_inactivity_period_high in
  let current_cycle = Block.current_cycle block in
  if Cycle.(current_cycle = root) then return tolerance_low
  else
    let+ total_stake, delegate_stake =
      Context.Delegate.stake_info (B block) ~manager_pkh:account.pkh
    in
    match delegate_stake with
    | None -> tolerance_low
    | Some delegate_stake ->
        let compare_stake_ratio_with_threshold =
          Int64.(
            compare
              (div (mul 1000L delegate_stake) total_stake)
              (of_int tolerance_threshold))
        in
        if Compare.Int.(compare_stake_ratio_with_threshold > 0) then
          tolerance_low
        else tolerance_high

let is_inactive ~block ~state current_activity_cycle account =
  let open Lwt_result_syntax in
  let+ tolerated_inactivity_period =
    tolerated_inactivity_period ~block ~state account
  in
  match account.last_seen_activity with
  | None -> assert false (* delegates have a minimum activity cycle *)
  | Some last_seen_activity_cycle ->
      Cycle.(
        add last_seen_activity_cycle tolerated_inactivity_period
        < current_activity_cycle)

let update_activity_account ~block ~state current_activity_cycle name =
  let open Lwt_result_syntax in
  let consensus_rights_delay =
    state.constants
      .Protocol.Alpha_context.Constants.Parametric.consensus_rights_delay
  in
  let account = State.find_account name state in
  let+ last_seen_activity =
    (* When a delegate is initialized or reactivated (either from
       [set_delegate] or participating in the consensus again), we put
       extra [consensus_rights_delay] cycles in the future to account
       for its extended grace period *)
    match account.last_seen_activity with
    | None -> return @@ Cycle.add current_activity_cycle consensus_rights_delay
    | Some last_seen_activity_cycle ->
        let+ is_inactive =
          is_inactive ~block ~state current_activity_cycle account
        in
        let updated =
          if is_inactive then
            Cycle.add current_activity_cycle consensus_rights_delay
          else current_activity_cycle
        in
        Cycle.max last_seen_activity_cycle updated
  in
  {account with last_seen_activity = Some last_seen_activity}

let update_activity ~block ~state current_activity_cycle name :
    State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let+ baker_acc =
    update_activity_account ~block ~state current_activity_cycle name
  in
  let account_map = String.Map.add name baker_acc state.account_map in
  {state with account_map}
