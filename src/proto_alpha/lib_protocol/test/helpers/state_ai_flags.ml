(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module handles the logic of flags for AI/AS in the [State] *)

open State
open State_account

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

module Autostake = struct
  let enabled (block : Block.t) (state : State.t) =
    (not (AI.enabled block state))
    && state.constants.adaptive_issuance.autostaking_enable

  let log_model_autostake name pkh old_cycle op ~optimal amount =
    let open Protocol.Alpha_context in
    Log.debug
      "Model Autostaking: at end of cycle %a, %s(%a) to reach optimal stake %a \
       %s %a"
      Cycle.pp
      old_cycle
      name
      Signature.Public_key_hash.pp
      pkh
      Tez.pp
      optimal
      op
      Tez.pp
      (Tez_helpers.of_mutez amount)

  let apply_autostake ~name ~old_cycle
      ({
         pkh;
         contract = _;
         delegate;
         parameters = _;
         liquid;
         bonds = _;
         frozen_deposits;
         unstaked_frozen;
         unstaked_finalizable;
         staking_delegator_numerator = _;
         staking_delegate_denominator = _;
         frozen_rights = _;
         slashed_cycles = _;
       } :
        account_state) state =
    let open Result_syntax in
    if Some name <> delegate then (
      Log.debug
        "Model Autostaking: %s <> %s, noop@."
        name
        (Option.value ~default:"None" delegate) ;
      return state)
    else
      let* current_liquid_delegated = liquid_delegated ~name state in
      let current_frozen = Frozen_tez.total_current frozen_deposits in
      let current_unstaked_frozen_delegated =
        Unstaked_frozen.sum_current unstaked_frozen
      in
      let current_unstaked_final_delegated =
        Unstaked_finalizable.total unstaked_finalizable
      in
      let power =
        Tez.(
          current_liquid_delegated +! current_frozen
          +! current_unstaked_frozen_delegated
          +! current_unstaked_final_delegated
          |> to_mutez |> Z.of_int64)
      in
      let optimal =
        Tez.of_z
          (Z.cdiv
             power
             (Z.of_int (state.constants.limit_of_delegation_over_baking + 1)))
      in
      let autostaked =
        Int64.(sub (Tez.to_mutez optimal) (Tez.to_mutez current_frozen))
      in
      let state = State.apply_unslashable (Cycle.succ old_cycle) name state in
      let state = State.apply_finalize name state in
      (* stake or unstake *)
      let new_state =
        if autostaked > 0L then (
          log_model_autostake ~optimal name pkh old_cycle "stake" autostaked ;
          State.apply_stake
            Tez.(min liquid (of_mutez autostaked))
            (Cycle.succ old_cycle)
            name
            state)
        else if autostaked < 0L then (
          log_model_autostake
            ~optimal
            name
            pkh
            old_cycle
            "unstake"
            (Int64.neg autostaked) ;
          State.apply_unstake
            (Cycle.succ old_cycle)
            (Tez_helpers.of_mutez Int64.(neg autostaked))
            name
            state)
        else (
          log_model_autostake
            ~optimal
            name
            pkh
            old_cycle
            "only finalize"
            autostaked ;
          state)
      in
      return new_state

  let run_at_cycle_end block state =
    let open Result_syntax in
    if enabled block state then
      let current_cycle = Block.current_cycle block in
      String.Map.fold_e
        (fun name account state ->
          apply_autostake ~name ~old_cycle:current_cycle account state)
        state.account_map
        state
    else return state
end
