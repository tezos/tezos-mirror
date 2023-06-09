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
let default_bonus = 0L

let reward_ratio_min = Q.(5 // 1000)

let reward_ratio_max = Q.(1 // 10)

(* Order of magnitude of the total supply in mutez
   Approximately 2^50 *)
let bonus_unit = 1_000_000_000_000_000L

let ratio_to_bonus q = Q.(q * of_int64 bonus_unit |> to_int64)

let max_bonus = Int64.div bonus_unit 20L (* = 5% *)

let get_reward_coeff ctxt ~cycle =
  let open Lwt_result_syntax in
  let ai_enable = (Raw_context.constants ctxt).adaptive_inflation.enable in
  if ai_enable then
    (* Even if AI is enabled, the storage can be empty: this is the case for
       the first 5 cycles after AI is enabled *)
    let* k_opt = Storage.Reward_coeff.find ctxt cycle in
    return (Option.value ~default:default_reward k_opt)
  else return default_reward

let get_reward_bonus ctxt ~cycle =
  let open Lwt_result_syntax in
  match cycle with
  | None -> return default_bonus
  | Some cycle ->
      let ai_enable = (Raw_context.constants ctxt).adaptive_inflation.enable in
      if ai_enable then
        let* k_opt = Storage.Reward_bonus.find ctxt cycle in
        return (Option.value ~default:default_bonus k_opt)
      else return default_bonus

let load_reward_coeff ctxt ~cycle =
  let open Lwt_result_syntax in
  let* new_reward = get_reward_coeff ctxt ~cycle in
  let ctxt =
    Raw_context.update_reward_coeff_for_current_cycle ctxt new_reward
  in
  return ctxt

let compute_reward_coeff_ratio =
  let q_400 = Q.of_int 400 in
  fun ~stake_ratio ~bonus ->
    let q_bonus = Q.(div (of_int64 bonus) (of_int64 bonus_unit)) in
    let inv_f = Q.(mul (mul stake_ratio stake_ratio) q_400) in
    let f = Q.inv inv_f (* f = 1/400 * (1/x)^2 = yearly inflation rate *) in
    let f = Q.add f q_bonus in
    (* f is truncated so that 0.5% <= f <= 10% *)
    let f = Q.(min f reward_ratio_max) in
    let f = Q.(max f reward_ratio_min) in
    f

let compute_bonus =
  let growth_rate =
    115_740_740L
    (* = 0.01 * [bonus_unit] / second_per_day
       For each % and each day, grows the bonus by 0.01% *)
  in
  (* Parameters for the dead zone: the bonus should not change in the interval [48%,52%] *)
  let center_dz = Q.(1 // 2) (* = 50 % *) in
  let radius_dz = Q.(1 // 50) (* = 2% *) in
  fun ~seconds_per_cycle ~total_supply ~total_frozen_stake ~previous_bonus ->
    let q_total_supply = Tez_repr.to_mutez total_supply |> Q.of_int64 in
    let q_total_frozen_stake =
      Tez_repr.to_mutez total_frozen_stake |> Q.of_int64
    in
    let stake_ratio =
      Q.div q_total_frozen_stake q_total_supply (* = portion of frozen stake *)
    in
    let base_reward_coeff_ratio =
      compute_reward_coeff_ratio ~stake_ratio ~bonus:0L
    in
    let base_reward_coeff_dist_to_max =
      ratio_to_bonus Q.(reward_ratio_max - base_reward_coeff_ratio)
    in
    (* The bonus reward is truncated between [0] and [max_bonus] *)
    (* It is done in a way that the bonus does not increase if the coeff
       would already be above the [reward_pct_max] *)
    let max_bonus = Compare.Int64.min base_reward_coeff_dist_to_max max_bonus in
    (* [dist] is the distance from [stake_ratio] to [48%,52%] *)
    let unsigned_dist =
      Q.(max zero (abs (stake_ratio - center_dz) - radius_dz))
    in
    let dist_q =
      if Compare.Q.(stake_ratio >= center_dz) then Q.neg unsigned_dist
      else unsigned_dist
    in
    let dist = ratio_to_bonus dist_q in
    let new_bonus =
      Int64.(add previous_bonus (mul dist (mul growth_rate seconds_per_cycle)))
    in
    let new_bonus = Compare.Int64.max new_bonus 0L in
    let new_bonus = Compare.Int64.min new_bonus max_bonus in
    new_bonus

let compute_coeff =
  let q_min_per_year = Q.of_int 525600 in
  fun ~base_total_rewards_per_minute ~total_supply ~total_frozen_stake ~bonus ->
    let q_total_supply = Tez_repr.to_mutez total_supply |> Q.of_int64 in
    let q_total_frozen_stake =
      Tez_repr.to_mutez total_frozen_stake |> Q.of_int64
    in
    let stake_ratio =
      Q.div q_total_frozen_stake q_total_supply (* = portion of frozen stake *)
    in
    let q_base_total_rewards_per_minute =
      Tez_repr.to_mutez base_total_rewards_per_minute |> Q.of_int64
    in
    let f = compute_reward_coeff_ratio ~stake_ratio ~bonus in
    let f = Q.div f q_min_per_year (* = inflation per minute *) in
    let f = Q.mul f q_total_supply (* = rewards per minute *) in
    Q.div f q_base_total_rewards_per_minute

let compute_and_store_reward_coeff_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let ai_enable = (Raw_context.constants ctxt).adaptive_inflation.enable in
  if not ai_enable then return ctxt
  else
    let preserved = Constants_storage.preserved_cycles ctxt in
    let for_cycle = Cycle_repr.add new_cycle preserved in
    let before_for_cycle = Cycle_repr.pred for_cycle in
    let* total_supply = Storage.Contract.Total_supply.get ctxt in
    let* total_stake = Stake_storage.get_total_active_stake ctxt for_cycle in
    let base_total_rewards_per_minute =
      (Constants_storage.reward_weights ctxt).base_total_rewards_per_minute
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
    let bonus =
      compute_bonus
        ~seconds_per_cycle
        ~total_supply
        ~total_frozen_stake
        ~previous_bonus
    in
    let coeff =
      compute_coeff
        ~base_total_rewards_per_minute
        ~total_supply
        ~total_frozen_stake
        ~bonus
    in
    let*! ctxt = Storage.Reward_bonus.add ctxt for_cycle bonus in
    let*! ctxt = Storage.Reward_coeff.add ctxt for_cycle coeff in
    return ctxt

let clear_outdated_reward_data ctxt ~new_cycle =
  let open Lwt_syntax in
  match Cycle_repr.sub new_cycle 1 with
  | None -> Lwt.return ctxt
  | Some cycle ->
      let* ctxt = Storage.Reward_coeff.remove ctxt cycle in
      Storage.Reward_bonus.remove ctxt cycle

let update_stored_rewards_at_cycle_end ctxt ~new_cycle =
  let open Lwt_result_syntax in
  let* ctxt = compute_and_store_reward_coeff_at_cycle_end ctxt ~new_cycle in
  let*! ctxt = clear_outdated_reward_data ctxt ~new_cycle in
  load_reward_coeff ctxt ~cycle:new_cycle

let load_reward_coeff ctxt =
  load_reward_coeff ctxt ~cycle:(Raw_context.current_level ctxt).cycle

let init_ema ctxt = Storage.Adaptive_inflation.Launch_ema.init ctxt 0L

let update_ema ctxt ~vote =
  Storage.Adaptive_inflation.Launch_ema.get ctxt >>=? fun old_ema ->
  Toggle_votes_repr.Adaptive_inflation_launch_EMA.of_int64 old_ema
  >>=? fun old_ema ->
  let new_ema =
    Toggle_votes_repr.compute_new_adaptive_inflation_ema
      ~toggle_vote:vote
      old_ema
  in
  Storage.Adaptive_inflation.Launch_ema.update
    ctxt
    (Toggle_votes_repr.Adaptive_inflation_launch_EMA.to_int64 new_ema)
  >|=? fun ctxt -> (ctxt, new_ema)

let activate ctxt ~cycle = Storage.Adaptive_inflation.Activation.init ctxt cycle

module For_RPC = struct
  let get_reward_coeff = get_reward_coeff
end
