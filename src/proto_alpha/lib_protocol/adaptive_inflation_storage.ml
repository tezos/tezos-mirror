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

let compute_bonus ~total_supply ~total_frozen_stake ~previous_bonus =
  ignore total_supply ;
  ignore total_frozen_stake ;
  ignore previous_bonus ;
  default_bonus

let compute_coeff =
  let q_400 = Q.of_int 400 in
  let q_min_per_year = Q.of_int 525600 in
  fun ~base_total_rewards_per_minute ~total_supply ~total_frozen_stake ->
    let q_total_supply = Tez_repr.to_mutez total_supply |> Q.of_int64 in
    let q_total_frozen_stake =
      Tez_repr.to_mutez total_frozen_stake |> Q.of_int64
    in
    let q_base_total_rewards_per_minute =
      Tez_repr.to_mutez base_total_rewards_per_minute |> Q.of_int64
    in
    let x =
      Q.div q_total_frozen_stake q_total_supply (* = portion of frozen stake *)
    in
    let inv_f = Q.(mul (mul x x) q_400) in
    let f = Q.inv inv_f (* f = 1/400 * (1/x)^2 = yearly inflation rate *) in
    (* f is truncated so that 0.5% <= f <= 10% *)
    let f = Q.(min f (1 // 10)) in
    let f = Q.(max f (5 // 1000)) in
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
    let* total_supply = Storage.Contract.Total_supply.get ctxt in
    let* total_stake = Stake_storage.get_total_active_stake ctxt for_cycle in
    let base_total_rewards_per_minute =
      (Constants_storage.reward_weights ctxt).base_total_rewards_per_minute
    in
    let total_frozen_stake = Stake_repr.get_frozen total_stake in
    let coeff =
      compute_coeff
        ~base_total_rewards_per_minute
        ~total_supply
        ~total_frozen_stake
    in
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
