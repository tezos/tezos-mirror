(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type shared = {
  baker_part : Tez_repr.t;
  stakers_part : Tez_repr.t;
  stez_part : Tez_repr.t;
}

let share ~adaptive_issuance_global_limit_of_staking_over_baking
    (delegate_parameters : Staking_parameters_repr.t) ~rounding
    ~full_staking_balance amount =
  let open Result_syntax in
  let Full_staking_balance_repr.
        {baker_over_all_frozen; staker_over_external_frozen} =
    Full_staking_balance_repr.distribution_ratios
      ~adaptive_issuance_global_limit_of_staking_over_baking
      ~delegate_limit_of_staking_over_baking_millionth:
        delegate_parameters.limit_of_staking_over_baking_millionth
      full_staking_balance
  in
  let* baker_part =
    let num, den = baker_over_all_frozen in
    let rounding =
      match rounding with `Towards_stakers -> `Down | `Towards_baker -> `Up
    in
    Tez_repr.mul_ratio ~rounding amount ~num ~den
  in
  let* external_frozen_part = Tez_repr.(amount -? baker_part) in
  let* stakers_part =
    let num, den = staker_over_external_frozen in
    let rounding =
      match rounding with `Towards_stakers -> `Down | `Towards_baker -> `Up
    in
    Tez_repr.mul_ratio ~rounding external_frozen_part ~num ~den
  in
  let* stez_part = Tez_repr.(external_frozen_part -? stakers_part) in
  return {baker_part; stakers_part; stez_part}

type reward_distrib = {
  to_baker_from_staking : Tez_repr.t;
  to_baker_from_edge_over_stakers : Tez_repr.t;
  to_baker_from_edge_over_stez : Tez_repr.t;
  to_stakers : Tez_repr.t;
  to_spendable : Tez_repr.t;
  to_stez : Tez_repr.t;
}

(** Compute the reward distribution between frozen and spendable according to:
    - the [full_staking_balance] of the delegate composed of the [own_frozen]
      and [staked_frozen] parts (the delegated part is ignored).
    - the [stake] of the delegate composed of the [frozen] deposits and the
      [weighted_delegated] tokens.
    - the [edge_of_baking_over_staking_billionth] parameter set by the baker in 1_000_000_000th
    - the [rewards] to be distributed

Preconditions:
 - 0 <= [edge_of_baking_over_staking_billionth]  <= 1_000_000_000

Rounding favors:
  - staking over delegation
  - baking over staking

In case the delegate's stake is zero, everything goes to the spendable balance.
*)
let compute_reward_distrib
    ~adaptive_issuance_global_limit_of_staking_over_baking delegate_parameters
    (stez_parameters : Clst_delegates_parameters_repr.t option)
    ~full_staking_balance ~stake ~(rewards : Tez_repr.t) =
  let open Result_syntax in
  let ({frozen; weighted_delegated} : Stake_repr.t) = stake in
  let* total_stake = Tez_repr.(frozen +? weighted_delegated) in
  if Tez_repr.(total_stake <= zero) then
    return
      {
        to_spendable = rewards;
        to_baker_from_staking = Tez_repr.zero;
        to_baker_from_edge_over_stakers = Tez_repr.zero;
        to_baker_from_edge_over_stez = Tez_repr.zero;
        to_stakers = Tez_repr.zero;
        to_stez = Tez_repr.zero;
      }
  else
    let* to_spendable =
      Tez_repr.mul_ratio
        ~rounding:`Down
        rewards
        ~num:(Tez_repr.to_mutez weighted_delegated)
        ~den:(Tez_repr.to_mutez total_stake)
    in
    let* to_frozen = Tez_repr.(rewards -? to_spendable) in
    let* {baker_part; stakers_part; stez_part} =
      share
        ~adaptive_issuance_global_limit_of_staking_over_baking
        delegate_parameters
        ~rounding:`Towards_baker
        ~full_staking_balance
        to_frozen
    in
    let to_baker_from_staking = baker_part in
    let* to_baker_from_edge_over_stakers =
      Tez_repr.mul_ratio
        ~rounding:`Up
        stakers_part
        ~num:
          (Int64.of_int32
             delegate_parameters.edge_of_baking_over_staking_billionth)
        ~den:1_000_000_000L
    in
    let* to_stakers =
      Tez_repr.(stakers_part -? to_baker_from_edge_over_stakers)
    in
    (* TODO STEZ: This reward repartition with the contract is temporary, it will be different
       once the final allocation model is implemented *)
    let* to_baker_from_edge_over_stez =
      match stez_parameters with
      | None -> return Tez_repr.zero
      (* If the parameters are not set (and sTEZ is enabled), it means the delegate is not registered
         for stez, or has unregistered. By the time it is unregistered, it shouldn't
         have any stez rights anyways. *)
      | Some stez_parameters ->
          Tez_repr.mul_ratio
            ~rounding:`Up
            stez_part
            ~num:
              (Int64.of_int32
                 stez_parameters.edge_of_clst_staking_over_baking_millionth)
            ~den:1_000_000L
    in
    let* to_stez = Tez_repr.(stez_part -? to_baker_from_edge_over_stez) in
    return
      {
        to_baker_from_staking;
        to_baker_from_edge_over_stakers;
        to_baker_from_edge_over_stez;
        to_stakers;
        to_spendable;
        to_stez;
      }

let share ~rounding ctxt delegate amount =
  let open Lwt_result_syntax in
  let* delegate_parameters =
    Delegate_staking_parameters.of_delegate ctxt delegate
  in
  let* full_staking_balance =
    Stake_storage.get_full_staking_balance ctxt delegate
  in
  Lwt.return
    (share
       ~adaptive_issuance_global_limit_of_staking_over_baking:
         (Constants_storage
          .adaptive_issuance_global_limit_of_staking_over_baking
            ctxt)
       delegate_parameters
       ~rounding
       ~full_staking_balance
       amount)

let compute_reward_distrib ctxt delegate stake rewards =
  let open Lwt_result_syntax in
  let* (delegate_parameters : Staking_parameters_repr.t) =
    Delegate_staking_parameters.of_delegate ctxt delegate
  in
  let* (stez_parameters : Clst_delegates_parameters_repr.t option) =
    if Constants_storage.native_contracts_enable ctxt then
      Clst_delegates_storage.get_delegate_parameters ctxt delegate
    else return_none
  in
  let* full_staking_balance =
    Stake_storage.get_full_staking_balance ctxt delegate
  in
  Lwt.return
  @@ compute_reward_distrib
       ~adaptive_issuance_global_limit_of_staking_over_baking:
         (Constants_storage
          .adaptive_issuance_global_limit_of_staking_over_baking
            ctxt)
       delegate_parameters
       stez_parameters
       ~full_staking_balance
       ~stake
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
  let* {
         to_baker_from_staking;
         to_baker_from_edge_over_stakers;
         to_baker_from_edge_over_stez;
         to_stakers;
         to_spendable;
         to_stez;
       } =
    compute_reward_distrib ctxt delegate active_stake rewards
  in
  let* ctxt, balance_updates_frozen_rewards_baker =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Frozen_staker_repr.baker delegate))
      to_baker_from_staking
  in
  let* ctxt, balance_updates_frozen_rewards_baker_edge =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Frozen_staker_repr.baker_edge delegate))
      to_baker_from_edge_over_stakers
  in
  let* ctxt, balance_updates_frozen_rewards_baker_edge_from_stez =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Frozen_staker_repr.baker_edge delegate))
      to_baker_from_edge_over_stez
  in
  let* ctxt, balance_updates_frozen_rewards_stakers =
    Token.transfer
      ctxt
      source
      (`Frozen_deposits (Frozen_staker_repr.shared_between_stakers ~delegate))
      to_stakers
  in
  let* ctxt, balance_updates_spendable_rewards =
    Token.transfer
      ctxt
      source
      (`Contract (Contract_repr.Implicit delegate))
      to_spendable
  in
  let* ctxt, balance_updates_stez_rewards =
    Token.transfer ctxt source `CLST_deposits to_stez
  in

  return
    ( ctxt,
      balance_updates_frozen_rewards_baker
      @ balance_updates_frozen_rewards_baker_edge
      @ balance_updates_frozen_rewards_baker_edge_from_stez
      @ balance_updates_frozen_rewards_stakers
      @ balance_updates_spendable_rewards @ balance_updates_stez_rewards )
