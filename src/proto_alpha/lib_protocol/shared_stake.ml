(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type shared = {baker_part : Tez_repr.t; stakers_part : Tez_repr.t}

let share ~rounding ctxt delegate amount =
  let open Lwt_result_syntax in
  let* {own_frozen; staked_frozen; delegated = _} =
    Stake_storage.get_full_staking_balance ctxt delegate
  in
  if Tez_repr.(staked_frozen = zero) then
    return {baker_part = amount; stakers_part = Tez_repr.zero}
  else
    let*? total_frozen = Tez_repr.(own_frozen +? staked_frozen) in
    let*? baker_part =
      let rounding =
        match rounding with `Towards_stakers -> `Down | `Towards_baker -> `Up
      in
      Tez_repr.mul_ratio
        ~rounding
        amount
        ~num:(Tez_repr.to_mutez own_frozen)
        ~den:(Tez_repr.to_mutez total_frozen)
    in
    let*? stakers_part = Tez_repr.(amount -? baker_part) in
    return {baker_part; stakers_part}

type reward_distrib = {to_frozen : Tez_repr.t; to_spendable : Tez_repr.t}

(** Compute the reward distribution between frozen and spendable according to:
    - the [stake] of the delegate composed of the [frozen] deposits and the
      [weighted_delegated] tokens.
    - the [edge_of_baking_over_staking_billionth] parameter set by the baker in 1_000_000_000th
    - the [rewards] to be distributed

Preconditions:
 - 0 <= [edge_of_baking_over_staking_billionth]  <= 1_000_000_000
*)
let compute_reward_distrib ~stake ~edge_of_baking_over_staking_billionth
    ~(rewards : Tez_repr.t) =
  let ({frozen; weighted_delegated} : Stake_repr.t) = stake in
  (* convert into Q *)
  let weighted_delegated =
    (* >= 0 *) Q.of_int64 @@ Tez_repr.to_mutez weighted_delegated
  in
  let frozen = (* >= 0 *) Q.of_int64 @@ Tez_repr.to_mutez frozen in
  let baking_over_staking_edge (* 0 <= baking_over_staking_edge <= 1 *) =
    Q.(of_int32 edge_of_baking_over_staking_billionth / of_int 1_000_000_000)
  in
  let rewards_q = Q.of_int64 @@ Tez_repr.to_mutez rewards in
  (* compute in Q *)
  let to_frozen =
    let open Q in
    let total_stake = weighted_delegated + frozen in
    if total_stake <= zero then zero
    else
      let non_delegated_ratio = frozen / total_stake in
      let non_delegated_rewards = rewards_q * non_delegated_ratio in
      non_delegated_rewards * (one - baking_over_staking_edge)
  in
  (* finish computation into mutez *)
  let rewards = Tez_repr.to_mutez rewards in
  let to_frozen = Q.to_int64 to_frozen in
  (* todo: is there any risk to overflow here ? *)
  let to_spendable = Int64.(sub rewards to_frozen) in
  (* convert back to tez *)
  (* Preconditions prevents to_frozen to be negative or greater than
     rewards. Thus we can use to_mutez_exn *)
  let to_frozen = Tez_repr.of_mutez_exn to_frozen in
  let to_spendable = Tez_repr.of_mutez_exn to_spendable in
  Ok {to_frozen; to_spendable}

let compute_reward_distrib ctxt delegate stake rewards =
  let open Lwt_result_syntax in
  let* (delegate_parameter : Staking_parameters_repr.t) =
    Delegate_staking_parameters.of_delegate ctxt delegate
  in
  let edge_of_baking_over_staking_billionth =
    delegate_parameter.edge_of_baking_over_staking_billionth
  in
  Lwt.return
  @@ compute_reward_distrib
       ~stake
       ~edge_of_baking_over_staking_billionth
       ~rewards

let pay_rewards ctxt ?active_stake ~source ~delegate rewards =
  let open Lwt_result_syntax in
  let* active_stake =
    match active_stake with
    | Some active_stake -> return active_stake
    | None ->
        let*? stake_distrib =
          Raw_context.stake_distribution_for_current_cycle ctxt
        in
        return
          (Option.value
             (Signature.Public_key_hash.Map.find delegate stake_distrib)
             ~default:Stake_repr.zero)
  in
  let* {to_frozen; to_spendable} =
    compute_reward_distrib ctxt delegate active_stake rewards
  in
  let* {baker_part; stakers_part} =
    share ~rounding:`Towards_stakers ctxt delegate to_frozen
  in
  let* ctxt, balance_updates_frozen_rewards_baker =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Frozen_staker_repr.baker delegate))
      baker_part
  in
  let* ctxt, balance_updates_frozen_rewards_stakers =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Frozen_staker_repr.shared_between_stakers ~delegate))
      stakers_part
  in
  let+ ctxt, balance_updates_spendable_rewards =
    Token.transfer
      ctxt
      source
      (`Contract (Contract_repr.Implicit delegate))
      to_spendable
  in
  ( ctxt,
    balance_updates_frozen_rewards_baker
    @ balance_updates_frozen_rewards_stakers @ balance_updates_spendable_rewards
  )
