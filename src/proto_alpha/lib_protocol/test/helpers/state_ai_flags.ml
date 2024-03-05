(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the logic of flags for AI/AS in the [State] *)

open State

module AI_Activation = struct
  (** This module is responsible for the field [state.ai_activation_cycle],
      which depends on three protocol parameters:
      [adaptive_issuance.force_activation], [adaptive_issuance.activation_vote_enable], and
      [adaptive_issuance.launch_ema_threshold]. *)

  (** AI can be activated with both flags set to false if the threshold is set to 0 *)
  let enabled state =
    state.constants.adaptive_issuance.force_activation
    || state.constants.adaptive_issuance.activation_vote_enable
    || Compare.Int32.(
         state.constants.adaptive_issuance.launch_ema_threshold = 0l)

  let set_activation_cycle block state block_launch_cycle =
    let current_cycle = Block.current_cycle block in
    let offset =
      if state.constants.adaptive_issuance.force_activation then 0
      else
        1 + state.constants.consensus_rights_delay
        + Protocol.Constants_repr.max_slashing_period
    in
    assert (
      Protocol.Alpha_context.Cycle.(
        add current_cycle offset = block_launch_cycle)) ;
    {state with ai_activation_cycle = Some block_launch_cycle}

  (** Check the activation_cycle is only ever set once.
      Run every block *)
  let check_activation_cycle block state =
    let open Lwt_result_syntax in
    let open Protocol.Alpha_context in
    let* block_launch_cycle =
      Context.get_adaptive_issuance_launch_cycle (B block)
    in
    match (enabled state, state.ai_activation_cycle, block_launch_cycle) with
    | _, None, None -> return state
    | true, Some x, Some y ->
        (* Activation cycle cannot be changed *)
        if Cycle.(x = y) then return state else assert false
    | _, Some _, None -> (* Activation cycle cannot be unset *) assert false
    | false, _, Some _ ->
        (* AI cannot be activated if [enabled] is false *)
        assert false
    | true, None, Some block_launch_cycle ->
        return @@ set_activation_cycle block state block_launch_cycle
end

module AI = struct
  let enabled (block : Block.t) (state : State.t) =
    match state.ai_activation_cycle with
    | None -> false
    | Some activation_cycle ->
        let current_cycle = Block.current_cycle block in
        Protocol.Alpha_context.Cycle.(current_cycle >= activation_cycle)
end
