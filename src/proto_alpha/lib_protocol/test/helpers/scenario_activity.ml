(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open State

let update_activity_account constants current_activity_cycle account =
  let consensus_rights_delay =
    constants.Protocol.Alpha_context.Constants.Parametric.consensus_rights_delay
  in
  let tolerated_inactivity_period = constants.tolerated_inactivity_period in
  let last_seen_activity =
    (* When a delegate is initialized or reactivated (either from
       [set_delegate] or participating in the consensus again), we put
       extra [consensus_rights_delay] cycles in the future to account
       for its extended grace period *)
    match account.last_seen_activity with
    | None -> Cycle.add current_activity_cycle consensus_rights_delay
    | Some last_seen_activity_cycle ->
        let updated =
          if
            Cycle.(
              add last_seen_activity_cycle tolerated_inactivity_period
              < current_activity_cycle)
          then Cycle.add current_activity_cycle consensus_rights_delay
          else current_activity_cycle
        in
        Cycle.max last_seen_activity_cycle updated
  in
  {account with last_seen_activity = Some last_seen_activity}

let update_activity name state current_activity_cycle : State.t =
  State.update_account_f
    name
    (update_activity_account state.constants current_activity_cycle)
    state
