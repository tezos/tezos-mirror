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

  (** AI can be activated with both flags set to false if the threshold is set to 0.
      If the vote is enabled, but the threshold is above the maximum EMA, then the vote
      cannot trigger the activation. *)
  let enabled state =
    state.constants.adaptive_issuance.force_activation
    || (state.constants.adaptive_issuance.activation_vote_enable
       && Compare.Int32.(
            state.constants.adaptive_issuance.launch_ema_threshold
            <= Protocol.Per_block_votes_repr.Internal_for_tests.ema_max))
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

module Delayed_slashing = struct
  let enabled (state : State.t) = state.constants.adaptive_issuance.ns_enable

  (* Returns a pair, fst is the delayed slashes, snd is the slashes to apply now *)
  let partition_slashes state current_cycle =
    if not (enabled state) then ([], state.pending_slashes)
    else
      List.partition
        (fun (_, Protocol.Denunciations_repr.{misbehaviour; _}) ->
          let cycle =
            Block.current_cycle_of_level
              ~blocks_per_cycle:
                state.constants
                  .Protocol.Alpha_context.Constants.Parametric.blocks_per_cycle
              ~current_level:
                (Protocol.Raw_level_repr.to_int32 misbehaviour.level)
          in
          Protocol.Alpha_context.Cycle.(cycle = current_cycle))
        state.pending_slashes
end

module NS = struct
  let enabled (block : Block.t) (state : State.t) =
    AI.enabled block state && state.constants.adaptive_issuance.ns_enable

  let get_double_attestation_slashing_percentage all_denunciations_to_apply
      block_before_slash state (misbehaviour : Protocol.Misbehaviour_repr.t) =
    let open Lwt_result_wrap_syntax in
    (* We need to get the block before the slash, because after the slash,
       the context gets rid of the required Seed to recompute the rights
       for the misbehaving delegates. *)
    if not (enabled block_before_slash state) then
      return
        state.constants
          .percentage_of_frozen_deposits_slashed_per_double_attestation
    else
      let* alpha_ctxt = Context.(get_alpha_ctxt (B block_before_slash)) in
      let raw_ctxt =
        Protocol.Alpha_context.Internal_for_tests.to_raw alpha_ctxt
      in
      let level =
        Protocol.Level_repr.level_from_raw
          ~cycle_eras:(Protocol.Raw_context.cycle_eras raw_ctxt)
          misbehaviour.level
      in
      let delegates =
        List.filter
          (fun (_, (den : Protocol.Denunciations_repr.item)) ->
            Compare.Int.(
              Protocol.Misbehaviour_repr.compare misbehaviour den.misbehaviour
              = 0))
          all_denunciations_to_apply
        |> List.map fst
        |> List.sort_uniq Signature.Public_key_hash.compare
      in
      let*@ _, pct =
        Protocol.Slash_percentage.get
          raw_ctxt
          ~kind:misbehaviour.kind
          ~level
          delegates
      in
      return pct
end
