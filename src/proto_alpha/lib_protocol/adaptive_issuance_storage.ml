(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Default reward coefficient when AI is not in effect, chosen so that
   rewards * coeff = rewards *)
let default_reward = Q.one

(* Default bonus value *)
let default_bonus = Issuance_bonus_repr.zero

(* Note: the reward and the bonus values are computed as rationals ([Q.t]) but
   are stored as fixed-point values (see {!Issuance_bonus_repr}) so that the
   stored representation does not grow unboundedly. *)

type error += Undetermined_issuance_coeff_for_cycle of Cycle_repr.t

let () =
  let open Data_encoding in
  let undetermined_issuance_coeff_for_cycle_description =
    "Issuance coefficient is only determined for the current cycle and the \
     next [preserved_cycles] cycles to come. Requested cycle is not in this \
     window."
  in
  register_error_kind
    `Permanent
    ~id:"undetermined_issuance_coeff_for_cycle"
    ~title:"Undetermined issuance coeff for cycle"
    ~description:undetermined_issuance_coeff_for_cycle_description
    ~pp:(fun ppf cycle ->
      Format.fprintf
        ppf
        "%s (cycle %a)"
        undetermined_issuance_coeff_for_cycle_description
        Cycle_repr.pp
        cycle)
    (obj1 (req "Undetermined_issuance_coeff_for_cycle" Cycle_repr.encoding))
    (function
      | Undetermined_issuance_coeff_for_cycle cycle -> Some cycle | _ -> None)
    (fun cycle -> Undetermined_issuance_coeff_for_cycle cycle)

let launch_cycle ctxt = Storage.Adaptive_issuance.Activation.get ctxt

let check_determined_cycle ctxt cycle =
  let ai_enable = Constants_storage.adaptive_issuance_enable ctxt in
  if ai_enable then
    let ctxt_cycle = (Raw_context.current_level ctxt).cycle in
    let preserved_cycles = Constants_storage.preserved_cycles ctxt in
    fail_unless
      Cycle_repr.(
        ctxt_cycle <= cycle && cycle <= add ctxt_cycle preserved_cycles)
      (Undetermined_issuance_coeff_for_cycle cycle)
  else return_unit

let get_reward_coeff ctxt ~cycle =
  let open Lwt_result_syntax in
  let* () = check_determined_cycle ctxt cycle in
  let ai_enable = Constants_storage.adaptive_issuance_enable ctxt in
  if ai_enable then
    (* Even if AI is enabled, the storage can be empty: this is the case for
       the first 5 cycles after AI is enabled *)
    let* k_opt = Storage.Issuance_coeff.find ctxt cycle in
    return (Option.value ~default:default_reward k_opt)
  else return default_reward

let get_reward_bonus ctxt ~cycle =
  let open Lwt_result_syntax in
  match cycle with
  | None -> return default_bonus
  | Some cycle ->
      let ai_enable = Constants_storage.adaptive_issuance_enable ctxt in
      if ai_enable then
        let* k_opt = Storage.Issuance_bonus.find ctxt cycle in
        return (Option.value ~default:default_bonus k_opt)
      else return default_bonus

let load_reward_coeff ctxt ~cycle =
  let open Lwt_result_syntax in
  let* new_reward = get_reward_coeff ctxt ~cycle in
  let ctxt =
    Raw_context.update_reward_coeff_for_current_cycle ctxt new_reward
  in
  return ctxt

let truncate_reward_coeff ~issuance_ratio_min ~issuance_ratio_max f =
  let f = Q.min f issuance_ratio_max in
  let f = Q.max f issuance_ratio_min in
  f

let compute_extremum ~launch_cycle ~new_cycle ~initial_period ~transition_period
    ~initial ~final =
  match launch_cycle with
  | None ->
      (* This case shouldn't happen, but if it does this value is the most sensible *)
      initial
  | Some launch_cycle ->
      let transition_period = transition_period + 1 in
      assert (Compare.Int.(transition_period > 0)) ;
      let t1 = Cycle_repr.add launch_cycle initial_period in
      let t2 = Cycle_repr.add t1 transition_period in
      if Cycle_repr.(new_cycle <= t1) then initial
      else if Cycle_repr.(new_cycle >= t2) then final
      else
        let t = Cycle_repr.diff new_cycle t1 |> Q.of_int32 in
        Q.(((final - initial) * t / of_int transition_period) + initial)

let compute_min
    ~(reward_params : Constants_parametric_repr.adaptive_rewards_params) =
  let Constants_parametric_repr.
        {
          initial_period;
          transition_period;
          issuance_ratio_initial_min;
          issuance_ratio_final_min;
          _;
        } =
    reward_params
  in
  compute_extremum
    ~initial_period
    ~transition_period
    ~initial:issuance_ratio_initial_min
    ~final:issuance_ratio_final_min

let compute_max
    ~(reward_params : Constants_parametric_repr.adaptive_rewards_params) =
  let Constants_parametric_repr.
        {
          initial_period;
          transition_period;
          issuance_ratio_initial_max;
          issuance_ratio_final_max;
          _;
        } =
    reward_params
  in
  compute_extremum
    ~initial_period
    ~transition_period
    ~initial:issuance_ratio_initial_max
    ~final:issuance_ratio_final_max

let compute_reward_coeff_ratio_without_bonus =
  let q_1600 = Q.of_int 1600 in
  fun ~stake_ratio ~issuance_ratio_max ~issuance_ratio_min ->
    let inv_f = Q.(mul (mul stake_ratio stake_ratio) q_1600) in
    let f = Q.inv inv_f (* f = 1/1600 * (1/x)^2 = yearly issuance rate *) in
    (* f is truncated so that 0.05% <= f <= 5% *)
    truncate_reward_coeff ~issuance_ratio_min ~issuance_ratio_max f

let compute_bonus ~issuance_ratio_max ~seconds_per_cycle ~stake_ratio
    ~base_reward_coeff_ratio ~(previous_bonus : Issuance_bonus_repr.t)
    ~reward_params =
  let Constants_parametric_repr.
        {
          issuance_ratio_final_min = _;
          issuance_ratio_final_max = _;
          issuance_ratio_initial_min = _;
          issuance_ratio_initial_max = _;
          initial_period = _;
          transition_period = _;
          max_bonus;
          growth_rate;
          center_dz;
          radius_dz;
        } =
    reward_params
  in
  let base_reward_coeff_dist_to_max =
    Q.(issuance_ratio_max - base_reward_coeff_ratio)
  in
  (* The bonus reward is truncated between [0] and [max_bonus] *)
  (* It is done in a way that the bonus does not increase if the coeff
     would already be above the [reward_pct_max] *)
  let max_new_bonus =
    Compare.Q.min base_reward_coeff_dist_to_max (max_bonus :> Q.t)
  in
  (* [dist] is the distance from [stake_ratio] to [48%,52%] *)
  let unsigned_dist =
    Q.(max zero (abs (stake_ratio - center_dz) - radius_dz))
  in
  let q_dist =
    if Compare.Q.(stake_ratio >= center_dz) then Q.neg unsigned_dist
    else unsigned_dist
  in
  let q_seconds_per_cycle = Q.of_int64 seconds_per_cycle in
  let q_days_per_cycle = Q.div q_seconds_per_cycle (Q.of_int 86_400) in
  let q_previous_bonus = (previous_bonus :> Q.t) in
  let new_bonus =
    Q.(add q_previous_bonus (mul q_dist (mul growth_rate q_days_per_cycle)))
  in
  let new_bonus = Q.max new_bonus Q.zero in
  let new_bonus = Q.min new_bonus max_new_bonus in
  Issuance_bonus_repr.of_Q ~max_bonus new_bonus

let compute_coeff =
  let q_min_per_year = Q.of_int 525600 in
  fun ~issuance_ratio_max
      ~issuance_ratio_min
      ~base_total_issued_per_minute
      ~base_reward_coeff_ratio
      ~q_total_supply
      ~(bonus : Issuance_bonus_repr.t) ->
    if Tez_repr.(base_total_issued_per_minute = zero) then Q.one
    else
      let q_base_total_issued_per_minute =
        Tez_repr.to_mutez base_total_issued_per_minute |> Q.of_int64
      in
      let f = Q.add base_reward_coeff_ratio (bonus :> Q.t) in
      let f = truncate_reward_coeff ~issuance_ratio_min ~issuance_ratio_max f in
      let f = Q.div f q_min_per_year (* = issuance rate per minute *) in
      let f = Q.mul f q_total_supply (* = issuance per minute *) in
      Q.div f q_base_total_issued_per_minute

let compute_and_store_reward_coeff_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let ai_enable = Constants_storage.adaptive_issuance_enable ctxt in
  if not ai_enable then return ctxt
  else
    let* launch_cycle = launch_cycle ctxt in
    let reward_params =
      Constants_storage.adaptive_issuance_rewards_params ctxt
    in
    let preserved = Constants_storage.preserved_cycles ctxt in
    let for_cycle = Cycle_repr.add new_cycle preserved in
    let before_for_cycle = Cycle_repr.pred for_cycle in
    let* total_supply = Storage.Contract.Total_supply.get ctxt in
    let* total_stake = Stake_storage.get_total_active_stake ctxt for_cycle in
    let base_total_issued_per_minute =
      (Constants_storage.issuance_weights ctxt).base_total_issued_per_minute
    in
    let total_frozen_stake = Stake_repr.get_frozen total_stake in
    let* previous_bonus = get_reward_bonus ctxt ~cycle:before_for_cycle in
    let blocks_per_cycle =
      Constants_storage.blocks_per_cycle ctxt |> Int64.of_int32
    in
    let minimal_block_delay =
      Constants_storage.minimal_block_delay ctxt |> Period_repr.to_seconds
    in
    let seconds_per_cycle = Int64.mul blocks_per_cycle minimal_block_delay in
    let q_total_supply = Tez_repr.to_mutez total_supply |> Q.of_int64 in
    let q_total_frozen_stake =
      Tez_repr.to_mutez total_frozen_stake |> Q.of_int64
    in
    let stake_ratio =
      Q.div q_total_frozen_stake q_total_supply (* = portion of frozen stake *)
    in
    let issuance_ratio_min =
      compute_min ~launch_cycle ~new_cycle ~reward_params
    in
    let issuance_ratio_max =
      compute_max ~launch_cycle ~new_cycle ~reward_params
    in
    let base_reward_coeff_ratio =
      compute_reward_coeff_ratio_without_bonus
        ~stake_ratio
        ~issuance_ratio_max
        ~issuance_ratio_min
    in
    let*? bonus =
      compute_bonus
        ~issuance_ratio_max
        ~seconds_per_cycle
        ~stake_ratio
        ~base_reward_coeff_ratio
        ~previous_bonus
        ~reward_params
    in
    let coeff =
      compute_coeff
        ~issuance_ratio_max
        ~issuance_ratio_min
        ~base_total_issued_per_minute
        ~base_reward_coeff_ratio
        ~q_total_supply
        ~bonus
    in
    let*! ctxt = Storage.Issuance_bonus.add ctxt for_cycle bonus in
    let*! ctxt = Storage.Issuance_coeff.add ctxt for_cycle coeff in
    return ctxt

let clear_outdated_reward_data ctxt ~new_cycle =
  let open Lwt_syntax in
  match Cycle_repr.sub new_cycle 2 with
  | None -> Lwt.return ctxt
  | Some cycle ->
      let* ctxt = Storage.Issuance_coeff.remove ctxt cycle in
      Storage.Issuance_bonus.remove ctxt cycle

let update_stored_rewards_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let* ctxt = compute_and_store_reward_coeff_at_cycle_end ctxt ~new_cycle in
  let*! ctxt = clear_outdated_reward_data ctxt ~new_cycle in
  load_reward_coeff ctxt ~cycle:new_cycle

let load_reward_coeff ctxt =
  load_reward_coeff ctxt ~cycle:(Raw_context.current_level ctxt).cycle

let init ctxt =
  let open Lwt_result_syntax in
  let* ctxt = Storage.Adaptive_issuance.Launch_ema.init ctxt 0l in
  Storage.Adaptive_issuance.Activation.init ctxt None

let activate ctxt ~cycle =
  Storage.Adaptive_issuance.Activation.update ctxt (Some cycle)

let set_adaptive_issuance_enable ctxt =
  let open Lwt_result_syntax in
  let+ enable =
    let+ launch_cycle = launch_cycle ctxt in
    match launch_cycle with
    | None -> false
    | Some launch_cycle ->
        let current_cycle = (Level_storage.current ctxt).cycle in
        Cycle_repr.(current_cycle >= launch_cycle)
  in
  if enable then Raw_context.set_adaptive_issuance_enable ctxt else ctxt

let update_ema ctxt ~vote =
  let open Lwt_result_syntax in
  let* old_ema = Storage.Adaptive_issuance.Launch_ema.get ctxt in
  let* old_ema =
    Per_block_votes_repr.Adaptive_issuance_launch_EMA.of_int32 old_ema
  in
  let new_ema =
    Per_block_votes_repr.compute_new_adaptive_issuance_ema
      ~per_block_vote:vote
      old_ema
  in
  let* ctxt =
    Storage.Adaptive_issuance.Launch_ema.update
      ctxt
      (Per_block_votes_repr.Adaptive_issuance_launch_EMA.to_int32 new_ema)
  in
  let* launch_cycle = launch_cycle ctxt in
  let open Constants_storage in
  let+ ctxt, launch_cycle =
    if
      (not (Constants_storage.adaptive_issuance_activation_vote_enable ctxt))
      || Per_block_votes_repr.Adaptive_issuance_launch_EMA.(
           new_ema < adaptive_issuance_launch_ema_threshold ctxt)
    then return (ctxt, launch_cycle)
    else
      match launch_cycle with
      | Some _ ->
          (* the feature is already set to launch, do nothing to avoid postponing it. *)
          return (ctxt, launch_cycle)
      | None ->
          (* set the feature to activate in a few cycles *)
          let current_cycle = (Level_storage.current ctxt).cycle in
          let delay =
            1 + preserved_cycles ctxt + Constants_repr.max_slashing_period
          in
          let cycle = Cycle_repr.add current_cycle delay in
          let+ ctxt = activate ctxt ~cycle in
          (ctxt, Some cycle)
  in
  (ctxt, launch_cycle, new_ema)

module For_RPC = struct
  let get_reward_coeff = get_reward_coeff

  let get_reward_bonus = get_reward_bonus
end

module Internal_for_tests = struct
  let compute_reward_coeff_ratio_without_bonus =
    compute_reward_coeff_ratio_without_bonus

  let compute_bonus = compute_bonus

  let compute_coeff = compute_coeff
end
